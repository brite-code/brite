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

module Brite.Project.Build
  ( buildProject
  ) where

import Brite.Project.Files
import Brite.Project.Cache
import qualified Data.HashTable.IO as HashTable

type HashTable k v = HashTable.CuckooHashTable k v

-- Builds _all_ of the source files in a Brite project. If a source file is already up-to-date in
-- the cache then we skip building it.
--
-- If there are any source files in the cache that no longer exist in our project directory then we
-- will delete those source files from the cache. Running full project builds occasionally is
-- important for garbage collection.
buildProject :: ProjectCache -> IO ()
buildProject cache = do
  -- Create a new hash table with which we will store in-memory our source file objects after
  -- fetching them from the cache.
  sourceFiles <- HashTable.new :: IO (HashTable SourceFilePath SourceFile)

  -- Select all the source files from our cache and put them into a hash table keyed by the source
  -- file’s path.
  selectAllSourceFiles cache () $ \() sourceFile ->
    HashTable.insert sourceFiles (sourceFilePath sourceFile) sourceFile

  -- Traverse all the source files in our project. If the source file does not exist in our cache
  -- or the source file has been modified since it was inserted in our cache then we need to process
  -- the source file.
  --
  -- We delete all the source files we see from our `sourceFiles` hash table. This means that at the
  -- very end we’ll be left with only the source files which were deleted since the last time we
  -- updated our cache. These source files need to be removed from the cache.
  traverseProjectSourceFiles (projectDirectory cache) () $ \() localSourceFilePath -> do
    -- Lookup the source file in our hash table.
    sourceFileM <- HashTable.lookup sourceFiles localSourceFilePath
    case sourceFileM of
      -- If the source file does not exist then we need to process the source file and it to
      -- our cache!
      Nothing -> putStrLn ("TODO: Process " ++ getSourceFilePath localSourceFilePath)
      -- If the source file does exist then we check the modification time. If the file was modified
      -- since the last time we built then we need to process the file.
      Just sourceFile -> do
        localSourceFileTime <- getSourceFileTime (projectDirectory cache) localSourceFilePath
        if sourceFileTime sourceFile < localSourceFileTime then
          putStrLn ("TODO: Process " ++ getSourceFilePath localSourceFilePath)
        else return ()
        -- Delete the source file from our hash table. All the source files which remain in our hash
        -- table at the end of our project build will be deleted from the cache.
        HashTable.delete sourceFiles localSourceFilePath

  -- TODO: Delete source files in the cache that still exist in `sourceFiles`.

  return ()

-- Builds a subset of the source files in a project. We only update the source files that we were
-- provided in the cache. We do not touch any other source files. Except possibly the dependents of
-- the source files we are updating.
--
-- If we provide the name of a source file path which no longer exists in the file system but does
-- exist in the cache then we will remove the cache entry. This is how one may perform manual
-- garbage collection. Source file paths which don’t exist in the file system or in the cache
-- are ignored.
buildProjectFiles :: ProjectCache -> [SourceFilePath] -> IO ()
buildProjectFiles = error "TODO: unimplemented"

-- TODO: `buildProjectVirtualFiles`
