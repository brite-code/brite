-- Manages all the cached information in a Brite project. Brite uses a SQLite database to manage the
-- cache. It’s important to remember why we call this module “Cache” and not “Database”. This isn’t
-- the definitive source of information about the user’s program. The file system is. If the user
-- throws away the cache we’ll just rebuild it. So the file system is the real “database” whereas
-- our SQLite database is merely a cache of the file system.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Project.Cache
  ( ProjectCache
  , unsafeProjectCacheConnection
  , withCache
  , withTemporaryCache
  , SourceFile(..)
  , selectAllSourceFiles
  ) where

import Brite.Exception
import Brite.Project.FileSystem
import Control.Exception (throwIO)
import Data.Foldable (traverse_)
import Data.Time (UTCTime)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder
import Database.SQLite.Simple
import System.FilePath ((</>))

-- TODO: Error handling for SQLITE_BUSY

-- Wrapper around a SQLite database connection. This allows us to control what operations the
-- outside world may perform on our cache.
newtype ProjectCache = ProjectCache { projectCacheConnection :: Connection }

-- Unsafely gets the cache’s SQLite connection. You shouldn’t be using the raw SQLite database
-- connection outside of this module! Let this module be responsible for all the SQLite work.
unsafeProjectCacheConnection :: ProjectCache -> Connection
unsafeProjectCacheConnection = projectCacheConnection

-- Opens a cache connection, executes an action using this connection, and closes the connection,
-- even in the presence of exceptions.
withCache :: ProjectCacheDirectory -> (ProjectCache -> IO a) -> IO a
withCache projectCacheDirectory action =
  let projectCache = getProjectCacheDirectory projectCacheDirectory </> "project.db" in
    withConnection projectCache (\c ->
      setupCache (ProjectCache c) *> action (ProjectCache c))

-- Opens an in-memory temporary cache connection, executes an action using this connection, and
-- closes the connection, even in the presence of exceptions. The cache will vanish when the
-- action completes.
withTemporaryCache :: (ProjectCache -> IO a) -> IO a
withTemporaryCache action =
  withConnection ":memory:" (\c ->
    setupCache (ProjectCache c) *> action (ProjectCache c))

-- Setup the Brite SQLite cache database by running appropriate migrations. We determine which
-- migrations need to be run by looking at the [`user_version`][1] pragma which SQLite kindly
-- provides to us.
--
-- [1]: https://sqlite.org/pragma.html#pragma_user_version
setupCache :: ProjectCache -> IO ()
setupCache (ProjectCache c) = do
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

-- The representation of a source file in our cache.
data SourceFile = SourceFile
  -- The unique identifier for this source file which can be used in foreign key constraints.
  { sourceFileID :: Int
  -- The path to our source file relative to the project’s `src` directory.
  , sourceFilePath :: SourceFilePath
  -- The last time at which the source file was modified according to our cache. In the file
  -- system the file may have been modified but our cache doesn’t know yet.
  , sourceFileTime :: UTCTime
  }

instance FromRow SourceFile where
  fromRow = SourceFile <$> field <*> (dangerouslyCreateSourceFilePath <$> field) <*> field

-- Selects all of the source files in the cache. Remember that these source files might not be up
-- to date!
selectAllSourceFiles :: ProjectCache -> a -> (a -> SourceFile -> IO a) -> IO a
selectAllSourceFiles (ProjectCache c) = fold_ c "SELECT id, path, time FROM source_file"
