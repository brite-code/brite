{-# LANGUAGE CPP #-}

module Brite.Dev (isDev) where

-- Is this build in development mode?
--
-- This will be false if we are building a Brite release.
isDev :: Bool
#ifdef RELEASE
isDev = False
#else
isDev = True
#endif
