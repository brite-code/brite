{-# LANGUAGE OverloadedStrings #-}

module Brite.Project.Database
  ( Database
  , unsafeDatabaseConnection
  , withDatabase
  , withTemporaryDatabase
  ) where

import Brite.Project.FileSystem (ProjectCacheDirectory, getProjectCacheDirectory)
import Data.Foldable (traverse_)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Int as Text.Builder
import Database.SQLite.Simple
import System.FilePath ((</>))

-- TODO: Error handling for SQLITE_BUSY

-- Wrapper around a SQLite database connection. This allows us to control what operations the
-- outside world may perform on our database.
newtype Database = Database { databaseConnection :: Connection }

-- Unsafely gets the database’s SQLite connection. You shouldn’t be using the raw SQLite database
-- connection outside of this module! Let this module be responsible for all the SQLite work.
unsafeDatabaseConnection :: Database -> Connection
unsafeDatabaseConnection = databaseConnection

-- Opens a database connection, executes an action using this connection, and closes the connection,
-- even in the presence of exceptions.
withDatabase :: ProjectCacheDirectory -> (Database -> IO a) -> IO a
withDatabase projectCacheDirectory action =
  let projectDatabasePath = getProjectCacheDirectory projectCacheDirectory </> "project.db" in
    withConnection projectDatabasePath (\c -> setupDatabase (Database c) *> action (Database c))

-- Opens an in-memory temporary database connection, executes an action using this connection, and
-- closes the connection, even in the presence of exceptions. The database will vanish when the
-- action completes.
withTemporaryDatabase :: (Database -> IO a) -> IO a
withTemporaryDatabase action =
  withConnection ":memory:" (\c -> setupDatabase (Database c) *> action (Database c))

-- Setup the Brite database by running appropriate migrations. We determine which migrations need to
-- be run by looking at the [`user_version`][1] pragma which SQLite kindly provides to us.
--
-- [1]: https://sqlite.org/pragma.html#pragma_user_version
setupDatabase :: Database -> IO ()
setupDatabase (Database c) = do
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
    error "TODO: Using an older version of Brite with a newer cache"

  -- Otherwise `latestUserVersion` is equal to `userVersion` so we have nothing to set up!
  else
    return ()

-- The latest possible `user_version` pragma. If in `setupDatabase` we find that our `user_version`
-- is smaller than the latest user version then we will run the appropriate migrations.
latestUserVersion :: Int
latestUserVersion = length allMigrations

-- Database migrations which get run in `setupDatabase`.
allMigrations :: [Query]
allMigrations =
  [ "SELECT 1 + 1;\n\
    \"
  ]
