-- Manages all the cached information in a Brite project. Brite uses a SQLite database to manage the
-- cache. It’s important to remember why we call this module “Cache” and not “Database”. This isn’t
-- the definitive source of information about the user’s program. The file system is. If the user
-- throws away the cache we’ll just rebuild it. So the file system is the real “database” whereas
-- our SQLite database is merely a cache of the file system.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Project.Cache
  ( ProjectCache
  , projectDirectory
  , unsafeProjectCacheConnection
  , withCache
  , unsafeWithCustomCache
  , withTransaction
  , withImmediateTransaction
  , SourceFile(..)
  , selectAllSourceFiles
  , selectSourceFiles
  ) where

import Brite.Exception
import Brite.Project.Files
import Control.Exception (throwIO)
import Data.Foldable (traverse_)
import Data.Time (UTCTime)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder
import Database.SQLite.Simple hiding (withTransaction, withImmediateTransaction)
import qualified Database.SQLite.Simple as SQLite
import System.FilePath ((</>))

-- TODO: Error handling for `SQLITE_BUSY`. If we retry after a `SQLITE_BUSY` we need to check
-- `PRAGMA user_version`. For instance, if a newer Brite was updating the cache schema and we tried
-- a request and got `SQLITE_BUSY` then we retry after the newer Brite is done we have an old
-- version of Brite reading against a new schema! So we need to check `PRAGMA user_version` to make
-- sure this doesn’t happen.

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
    throwIO ProjectCacheUnrecognizedVersion

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
  [ "CREATE TABLE source_file (\n\
    \  id INTEGER PRIMARY KEY,\n\
    \  path TEXT NOT NULL UNIQUE,\n\
    \  time TEXT NOT NULL\n\
    \);\n\
    \"
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
withImmediateTransaction :: ProjectCache -> IO a -> IO a
withImmediateTransaction = SQLite.withImmediateTransaction . projectCacheConnection

-- The representation of a source file in our cache.
data SourceFile = SourceFile
  -- The unique identifier for this source file which can be used in foreign key constraints.
  { sourceFileID :: Int
  -- The path to our source file relative to the project’s source directory.
  , sourceFilePath :: SourceFilePath
  -- The last time at which the source file was modified according to our cache. In the file
  -- system the file may have been modified but our cache doesn’t know yet.
  , sourceFileTime :: UTCTime
  }

instance FromRow SourceFile where
  fromRow = SourceFile <$> field <*> (dangerouslyCreateSourceFilePath <$> field) <*> field

-- Selects all of the source files in the cache. Remember that this source file data might not be up
-- to date with the file system!
selectAllSourceFiles :: ProjectCache -> a -> (a -> SourceFile -> IO a) -> IO a
selectAllSourceFiles (ProjectCache _ c) = fold_ c "SELECT id, path, time FROM source_file"

-- Selects source the source files with provided file paths. Some of the provided source files might
-- not exist. Remember that this source file data might not be up to date with the file system!
selectSourceFiles :: ProjectCache -> [SourceFilePath] -> a -> (a -> SourceFile -> IO a) -> IO a
selectSourceFiles (ProjectCache _ c) sourceFilePaths =
  let
    (paramCount, params) =
      foldr (\p (n, ps) -> (n + 1, getSourceFilePath p : ps)) (0, []) sourceFilePaths
  in
    fold c
      -- `Data.Text` fusion should make constructing this query fast. `Text.replicate` with a
      -- singleton parameter should be rewritten to `replicateChar` which is subject to fusion.
      (Query (Text.append
        "SELECT id, path, time FROM source_file WHERE path IN ("
        (Text.snoc (Text.intersperse ',' (Text.replicate paramCount "?")) ')')))
      params
