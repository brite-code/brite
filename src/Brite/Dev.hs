module Brite.Dev (isDev) where

-- Is this build in development mode?
--
-- This will be false if we are building a Brite release.
--
-- TODO: Actually make this false when we are building a Brite release.
isDev :: Bool
isDev = True
