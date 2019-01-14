-- Something bad happened within Brite and we need to throw an exception. Exceptions are different
-- from diagnostics in that diagnostics are associated with a particular resource whereas exceptions
-- are operational problems associated with the project as a whole.
--
-- If a diagnostic occurs in one resource it often will not block other resources from being
-- processed. However, an exception might block all resources from being processed since Brite can’t
-- proceed past a problem in the project structure. For example, the project cache may be malformed
-- or the project config may have a syntax exception.

module Brite.Exception
  ( BriteException(..)
  ) where

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
