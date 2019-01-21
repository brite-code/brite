-- Manages all the cached information in a Brite project. Brite uses a SQLite database to manage the
-- cache. It’s important to remember why we call this module “Cache” and not “Database”. This isn’t
-- the definitive source of information about the user’s program. The file system is. If the user
-- throws away the cache we’ll just rebuild it. So the file system is the real “database” whereas
-- our SQLite database is merely a cache of the file system.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Brite.Project.Cache
  ( ProjectCache
  , projectDirectory
  , unsafeProjectCacheConnection
  , withCache
  , unsafeWithCustomCache
  , withTransaction
  , withImmediateTransaction
  , CacheSourceFile(..)
  , selectAllSourceFiles
  , selectSourceFiles
  , insertSourceFile
  , updateSourceFile
  , deleteSourceFile
  ) where

import Brite.Exception
import Brite.Project.Files
import Data.Foldable (traverse_)
import Data.Time (UTCTime)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder
import Control.Concurrent (threadDelay)
import Control.Exception (mask, onException, catchJust)
import Database.SQLite.Simple hiding (withTransaction, withImmediateTransaction)
import Database.SQLite.Simple.QQ
import qualified Database.SQLite.Simple as SQLite
import System.FilePath ((</>))

-- Wrapper around a SQLite database connection. This allows us to control what operations the
-- outside world may perform on our cache.
data ProjectCache = ProjectCache
  { projectDirectory :: ProjectDirectory
  , projectCacheConnection :: Connection
  }

-- Unsafely gets the cache’s SQLite connection. You shouldn’t be using the raw SQLite database
-- connection outside of this module! Let this module be responsible for all the SQLite work.
unsafeProjectCacheConnection :: ProjectCache -> Connection
unsafeProjectCacheConnection = projectCacheConnection

-- Opens a cache connection, executes an action using this connection, and closes the connection,
-- even in the presence of exceptions.
withCache :: ProjectDirectory -> (ProjectCache -> IO a) -> IO a
withCache project action = do
  projectCacheDirectory <- findProjectCacheDirectory project
  let projectCacheDatabasePath = projectCacheDirectory </> "project.db"
  withConnection projectCacheDatabasePath (\connection ->
    let cache = ProjectCache project connection in
      setupCache cache *> action cache)

-- Unsafely sets up a cache connection with a custom file path. We only use this in tests!
unsafeWithCustomCache :: ProjectDirectory -> FilePath -> (ProjectCache -> IO a) -> IO a
unsafeWithCustomCache project projectCacheDatabasePath action =
  withConnection projectCacheDatabasePath (\connection ->
    let cache = ProjectCache project connection in
      setupCache cache *> action cache)

-- Setup the Brite SQLite cache database by running appropriate migrations. We determine which
-- migrations need to be run by looking at the [`user_version`][1] pragma which SQLite kindly
-- provides to us.
--
-- [1]: https://sqlite.org/pragma.html#pragma_user_version
setupCache :: ProjectCache -> IO ()
setupCache (ProjectCache _ c) = do
  -- Enable foreign key constraints since SQLite does not enable them by default for backwards
  -- compatibility reasons.
  execute_ c "PRAGMA foreign_keys = ON"
  -- Query the database to get the current user version...
  userVersionRows <- query_ c "PRAGMA user_version" :: IO [Only Int]
  let userVersion = if null userVersionRows then 0 else fromOnly (head userVersionRows)
  -- If the user version is smaller than the latest user version we need to run some migrations!
  if userVersion < latestUserVersion then
    -- Acquire an exclusive transaction on our database. We don’t want anyone else to read or write
    -- to the database while we are running our migrations.
    --
    -- If two processes try to run migrations at the same time then one will acquire the exclusive
    -- lock and the other will fail with SQLITE_BUSY.
    --
    -- IMPORTANT: Don’t retry this transaction without re-checking `PRAGMA user_version` first!!! It
    -- might have changed.
    withExclusiveTransaction c (do
      -- Don’t run migrations that have already been run against this database.
      let migrations = drop userVersion allMigrations
      -- Execute all the migrations which have not yet been run.
      traverse_ (execute_ c) migrations
      -- Update the `user_version` pragma to `latestUserVersion`. Unfortunately, we can’t use query
      -- variables when setting a pragma so we need to manually construct the query. This is safe
      -- from SQL injection because `latestUserVersion` is a constant integer.
      execute_ c ("PRAGMA user_version = " <> Query
        (Text.Lazy.toStrict (Text.Builder.toLazyText (Text.Builder.decimal latestUserVersion)))))

  -- If our `latestUserVersion` is smaller than the `userVersion` then we are using an old version
  -- of the Brite compiler with a new version of the cache.
  else if latestUserVersion < userVersion then
    throw ProjectCacheUnrecognizedVersion

  -- Otherwise `latestUserVersion` is equal to `userVersion` so we have nothing to set up!
  else
    return ()

-- The latest possible `user_version` pragma. If in `setupCache` we find that our `user_version`
-- is smaller than the latest user version then we will run the appropriate migrations.
latestUserVersion :: Int
latestUserVersion = length allMigrations

-- Cache migrations which are run in `setupCache`.
--
-- IMPORTANT: Never change the schema migrations from a released Brite version! Only change the
-- schema migrations for unreleased code. This allows us to upgrade the user’s cache whenever they
-- upgrade Brite.
allMigrations :: [Query]
allMigrations =
  [ [sql|
    -- A source file in our project.
    CREATE TABLE source_file (
      id INTEGER PRIMARY KEY,
      -- The path relative to our project’s source directory. So if our project directory was
      -- `~/projects/my-app` then this path would be relative to `~/projects/my-app/src`.
      path TEXT NOT NULL UNIQUE,
      -- The last time the file was modified in the file system. This value might be earlier than
      -- the actual modification time. If that’s the case we’ll update this file on the next build.
      modification_time TEXT NOT NULL
    );
    |]
  ]

-- Run an IO action inside a “deferred” transaction. A deferred transaction does not acquire any
-- locks until some SQL queries are executed. The first query to read the database will proceed if
-- there are no exclusive locks on the database. The first query to write to the database will
-- proceed if no one else is trying to write to the database.
withTransaction :: ProjectCache -> IO a -> IO a
withTransaction = SQLite.withTransaction . projectCacheConnection

-- Run in IO action inside an “immediate” transaction. An immediate transaction will immediately
-- block other cache connections from _writing_ to the cache but will not prevent other connections
-- from reading from the cache. An immediate transaction will also block any other processes from
-- starting an immediate transaction.
--
-- Immediate transactions are good when we want to _serialize_ cache updates but not reads. Anyone
-- will be able to read from the cache during the transaction. Their reads might be stale though.
--
-- We use this during full project builds. Only one full project build may be executed at once
-- because we take an immediate transaction. However, IDE tools can still read stale data from
-- the cache.
--
-- If we can’t commit because the cache is currently being read by another transaction we will retry
-- the commit again after a delay. The current delay function we use, in seconds, is
-- f(n) = .1s * n ^ 2 where “n” is the number of retries. We only retry four times. This means our
-- delays will look like:
--
-- * f(0) = 0.0s
-- * f(1) = 0.1s
-- * f(2) = 0.4s
-- * f(3) = 0.9s
--
-- So if every commit errors we end up running the following:
-- `COMMIT`, wait 0s, `COMMIT`, wait 0.1s, `COMMIT`, wait 0.4s, `COMMIT`, wait 0.9s, `COMMIT`.
-- That’s a total wait time from delays of 1.4s. We should collect data to tune how frequently we
-- retry and when we retry.
--
-- There aren’t very many cache operations which we retry. We retry `withImmediateTransaction`
-- commits particularly because generally we do a lot of work in immediate transactions. That’s why
-- we want to acquire the write lock at the beginning. So if we’ve done a lot of work but then we
-- get blocked by an IDE tool trying to read some data, that sucks. So we retry.
withImmediateTransaction :: ProjectCache -> IO a -> IO a
withImmediateTransaction (ProjectCache _ c) action =
  mask $ \restore -> do
    execute_ c "BEGIN IMMEDIATE TRANSACTION"
    x <- restore action `onException` execute_ c "ROLLBACK TRANSACTION"
    commit 0
    return x
  where
    commit n | n == 4 = execute_ c "COMMIT TRANSACTION"
    commit n = catchJust
      (\e -> if sqlError e == ErrorBusy then Just () else Nothing)
      (execute_ c "COMMIT TRANSACTION")
      (\() -> do
        threadDelay (100000 * (n ^ (2 :: Int)))
        commit (n + 1))

-- The representation of a source file in our cache.
data CacheSourceFile = CacheSourceFile
  -- The unique identifier for this source file which can be used in foreign key constraints.
  { sourceFileID :: Int
  -- The path to our source file relative to the project’s source directory.
  , sourceFilePath :: SourceFilePath
  -- The last time at which the source file was modified according to our cache. In the file
  -- system the file may have been modified but our cache doesn’t know yet.
  , sourceFileModificationTime :: UTCTime
  }

instance FromRow CacheSourceFile where
  fromRow = CacheSourceFile <$> field <*> (dangerouslyCreateSourceFilePath <$> field) <*> field

-- Selects all of the source files in the cache. Remember that this source file data might not be up
-- to date with the file system!
selectAllSourceFiles :: ProjectCache -> a -> (a -> CacheSourceFile -> IO a) -> IO a
selectAllSourceFiles (ProjectCache _ c) =
  fold_ c "SELECT id, path, modification_time FROM source_file"

-- Selects source the source files with provided file paths. Some of the provided source files might
-- not exist. Remember that this source file data might not be up to date with the file system!
selectSourceFiles :: ProjectCache -> [SourceFilePath] -> a -> (a -> CacheSourceFile -> IO a) -> IO a
selectSourceFiles (ProjectCache _ c) sourceFilePaths =
  let
    (paramCount, params) =
      foldr (\p (n, ps) -> (n + 1, getSourceFileRelativePath p : ps)) (0, []) sourceFilePaths
  in
    fold c
      -- `Data.Text` fusion should make constructing this query fast. `Text.replicate` with a
      -- singleton parameter should be rewritten to `replicateChar` which is subject to fusion.
      (Query (Text.append
        "SELECT id, path, modification_time FROM source_file WHERE path IN ("
        (Text.snoc (Text.intersperse ',' (Text.replicate paramCount "?")) ')')))
      params

-- Inserts a source file into the project’s cache.
insertSourceFile :: ProjectCache -> SourceFilePath -> UTCTime -> IO ()
insertSourceFile (ProjectCache _ c) newSourceFilePath newSourceTime =
  execute c "INSERT INTO source_file (path, modification_time) VALUES (?, ?)"
    (getSourceFileRelativePath newSourceFilePath, newSourceTime)

-- Updates a source file in the project’s cache.
updateSourceFile :: ProjectCache -> CacheSourceFile -> UTCTime -> IO ()
updateSourceFile (ProjectCache _ c) sourceFile newSourceTime =
  execute c "UPDATE source_file SET modification_time = ? WHERE id = ?"
    (newSourceTime, sourceFileID sourceFile)

-- Deletes a source file from the project’s cache.
deleteSourceFile :: ProjectCache -> CacheSourceFile -> IO ()
deleteSourceFile (ProjectCache _ c) sourceFile =
  execute c "DELETE FROM source_file WHERE id = ?" (Only (sourceFileID sourceFile))
