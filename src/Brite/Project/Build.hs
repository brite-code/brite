-- This module is responsible for running the Brite toolchain when the user invokes it. The Brite
-- toolchain can be thought of as a pipeline:
--
-- 1. Discover source files.
-- 2. Parse source files.
-- 3. Type check source files.
-- 4. Pre-evaluate source files.
-- 5. Compile build units.
--
-- It would be incredibly expensive to run this pipeline every time the user changes their project,
-- but we model the world is if that’s the case as it makes writing the individual steps easier.
-- Then we use caching to drastically speed everything up. This idea was popularized in the User
-- Interface world by React.
--
-- The way Brite works is that every time the user invokes the build command we look at all of their
-- source files and only rebuild the ones which changed. The user may also invoke a “check” command
-- which only type checks their project, skipping steps 4 and 5.
--
-- A user may also choose to narrow the files Brite looks at when rebuilding. If the user supplies
-- a set of paths then Brite will only check these paths for changes and will ignore all other
-- source files in the project. If a source file changed, Brite won’t update the cache with those
-- changes until the user for a build that updates those files.
--
-- In this way the Brite toolchain runner is dumb. It moves the problem of cache invalidation onto
-- the user’s shoulders. If the user never tells Brite to invalidate the cache for a specific file
-- then Brite won’t bother.
--
-- While we are building the cache is locked.
--
-- Then there’s the problem of virtual files. Brite supports IDEs creating “virtual files” while the
-- user is editing. Virtual files only live temporarily, but they are still saved to the cache.
-- Virtual files are never pre-evaluated or compiled. They are only type checked. Virtual files may
-- never have dependents. (Brite disallows dependency cycles between files so a virtual file is
-- never a dependent of itself. This is important and one of the reasons we don’t allow cycles
-- between files.)

module Brite.Project.Build () where

import Brite.Project.FileSystem

buildProject :: ProjectDirectory -> IO ()
buildProject = error "unimplemented"

buildProjectFiles :: ProjectDirectory -> [SourceFilePath] -> IO ()
buildProjectFiles = error "unimplemented"

-- TODO: `buildProjectVirtualFiles`
