-- Something bad happened within Brite and we need to throw an exception. Exceptions are different
-- from diagnostics in that diagnostics are associated with a particular resource whereas exceptions
-- are operational problems associated with the project as a whole.
--
-- If a diagnostic occurs in one resource it often will not block other resources from being
-- processed. However, an exception might block all resources from being processed since Brite can’t
-- proceed past a problem in the project structure. For example, the project cache may be malformed
-- or the project config may have a syntax exception.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Exception
  ( BriteException(..)
  , throw
  , catchEverything
  ) where

import Brite.DiagnosticsMarkup
import Brite.Project.FileNames (configFileName)
import Control.Exception hiding (throw)
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLLite

data BriteException
  -- If we couldn’t find a project directory when looking at the current path or any parent paths
  -- then we throw this exception.
  --
  -- Of course, one could choose to handle not being able to find a project directory, but if you
  -- can’t proceed without out one then throwing is an option. An option that will give you a nice
  -- error message.
  = ProjectDirectoryNotFound FilePath

  -- We expect a source file to live in a particular directory, but instead it is either not a
  -- source file (does not have the `.ite` extension) or it lives outside of the project directory.
  --
  -- The first path is the project directory’s path. The second path is the source file path.
  | InvalidSourceFilePath FilePath

  -- If the project cache was upgraded _past_ what this build of Brite supports then we throw
  -- this exception.
  --
  -- This usually happens when a user runs a newer version of Brite on their project, thus updating
  -- their cache, and then tries to run an older version of Brite which does not support the newer
  -- cache format.
  --
  -- TODO: We should have a nicer error for users which includes version numbers if possible!!!
  | ProjectCacheUnrecognizedVersion

  deriving (Show)

instance Exception BriteException

-- Throws a Brite exception in the IO monad.
throw :: BriteException -> IO a
throw = throwIO

-- Get a human-readable message for our Brite exception. We use the same diagnostic markup data type
-- to build the message.
--
-- Follow the same rules as `Brite.Diagnostics` to write exception messages.
briteExceptionMessage :: BriteException -> Markup

-- Tell the user we couldn’t find a Brite file in their directory. Technically we also looked in
-- the parent directories too, but that’s not the point. Render an abbreviated message if we were
-- searching in the current directory.
briteExceptionMessage (ProjectDirectoryNotFound ".") =
  plain "Could not find a " <> code (Text.pack configFileName) <> plain " file."
briteExceptionMessage (ProjectDirectoryNotFound path) =
  plain "Could not find a " <> code (Text.pack configFileName) <> plain " file \
  \for " <> code (Text.pack path) <> plain "."

-- Tell the user that their source file does not exist in this project. Remember that the source
-- file path might just have the wrong extension. If the project directory is the current directory
-- then use a simpler message.
briteExceptionMessage (InvalidSourceFilePath sourceFilePath) =
  code (Text.pack sourceFilePath) <> plain " is not a source file in this project."

-- Unfortunately, for this message we don’t have nice version numbers because it comes from a
-- low-level check in `Brite.Project.Cache` which uses a version identifier not useful for humans.
-- (The number of database migrations executed against the SQLite cache.)
--
-- If this exception is thrown we’d rather the programmer use the newer version of Brite which
-- upgraded their SQLite cache. So we intentionally use language like “please” to encourage usage of
-- the new version and “mistake” to discourage running `brite reset`.
briteExceptionMessage ProjectCacheUnrecognizedVersion =
  plain "A newer version of Brite built this project. Please use that version instead. If you \
  \think this was a mistake run " <> code "brite reset" <> plain "."

-- TODO: We need an error log file where we log errors in detail. We only want to tell users about
-- the log file in GitHub issues since we don’t want to reveal Brite internal implementation
-- details which a log would contain.
--
-- Ideally, we’d know all of the exceptions Brite might throw ahead of time and we could provide a
-- good error messages for the expected issues.
--
-- Should we automatically upload error logs to the server? The logs might contain
-- sensitive information.

-- We want to be really clear here that this is our fault and not the user’s!
someExceptionErrorMessage :: SomeException -> Markup
someExceptionErrorMessage _ =
  plain "Uh oh. We failed because of an unexpected error. " <> issueTrackerMessage

-- We want to be really clear here that this is our fault and not the user’s! We also don’t want to
-- reveal that we are using SQLite. It’s not a secret, you can look at Brite’s source code, but a
-- Brite user should never have to think about SQLite. If they are trying to run queries against the
-- cache, we haven’t abstracted the cache for enough away from the user.
sqliteGenericErrorMessage :: Markup
sqliteGenericErrorMessage =
  plain "Uh oh. We failed to access your project’s cache. " <> issueTrackerMessage

sqliteSQLErrorMessage :: SQLLite.SQLError -> Markup
sqliteSQLErrorMessage _ = sqliteGenericErrorMessage

sqliteResultErrorMessage :: SQLLite.ResultError -> Markup
sqliteResultErrorMessage _ = sqliteGenericErrorMessage

sqliteFormatErrorMessage :: SQLLite.FormatError -> Markup
sqliteFormatErrorMessage _ = sqliteGenericErrorMessage

-- A message for pushing people to our issue tracker when they encounter an unexpected error.
--
-- We use the word “issue” instead of the word “bug” because the meaning of “bug” is subjective. An
-- error might be expected behavior and we just haven’t created a better error message for it yet.
issueTrackerMessage :: Markup
issueTrackerMessage =
  plain "See if this issue was already reported: https://github.com/brite-code/brite/issues"

-- Catch all errors and print them to a diagnostic markup error message.
catchEverything :: IO a -> IO (Either Markup a)
catchEverything action = (Right <$> action) `catches`
  [ handler briteExceptionMessage
  , handler sqliteSQLErrorMessage
  , handler sqliteResultErrorMessage
  , handler sqliteFormatErrorMessage
  , handler someExceptionErrorMessage
  ]
  where
    handler f = Handler (return . Left . f)
