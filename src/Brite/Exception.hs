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
  , exceptionMessage
  ) where

import Brite.DiagnosticsMarkup
import Control.Exception (Exception)

data BriteException
  -- If the project cache was upgraded _past_ what this build of Brite supports then we throw
  -- this exception.
  --
  -- This usually happens when a user runs a newer version of Brite on their project, thus updating
  -- their cache, and then tries to run an older version of Brite which does not support the newer
  -- cache format.
  --
  -- TODO: We should have a nicer error for users which includes version numbers if possible!!!
  = ProjectCacheUnrecognizedVersion

  deriving (Show)

instance Exception BriteException

-- Get a human-readable message for our Brite exception. We use the same diagnostic markup data type
-- to build the message.
--
-- Follow the same rules as `Brite.Diagnostics` to write exception messages.
exceptionMessage :: BriteException -> Markup

-- Unfortunately, for this message we don’t have nice version numbers because it comes from a
-- low-level check in `Brite.Project.Cache` which uses a version identifier not useful for humans.
-- (The number of database migrations executed against the SQLite cache.)
--
-- If this exception is thrown we’d rather the programmer use the newer version of Brite which
-- upgraded their SQLite cache. So we intentionally use language like “please” to encourage usage of
-- the new version and “mistake” to discourage running `brite reset`.
exceptionMessage ProjectCacheUnrecognizedVersion =
  plain "A newer version of Brite built this project. Please use that version instead. If you \
  \think this was a mistake run " <> code "brite reset" <> plain "."
