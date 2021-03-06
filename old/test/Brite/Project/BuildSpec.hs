{-# LANGUAGE OverloadedStrings #-}

module Brite.Project.BuildSpec (spec) where

import Brite.Project.Build
import Brite.Project.Files
import Brite.Project.Cache
import Control.Exception (bracket)
import Data.List (sort)
import Data.Time.Clock
import Database.SQLite.Simple
import System.Directory
import System.FilePath
import Test.Hspec

-- Creates a temporary directory at for the duration the test and removes the temporary directory
-- with all its contents at the end of the test. Also sets the current directory to the new
-- current directory.
withTemporaryDirectory :: (FilePath -> IO ()) -> IO ()
withTemporaryDirectory action =
  bracket
    (do
      systemTemporaryDirectory <- getTemporaryDirectory
      temporaryDirectory <- canonicalizePath (systemTemporaryDirectory </> "Brite.Project.BuildSpec")
      createDirectory temporaryDirectory
      return temporaryDirectory)
    removeDirectoryRecursive
    (\temporaryDirectory ->
      withCurrentDirectory temporaryDirectory (action temporaryDirectory))

-- Sets up a Brite cache in a temporary directory (created with `withTemporaryDirectory`).
--
-- Also creates a source directory in the temporary directory.
withTestCache :: ((FilePath, ProjectCache) -> IO ()) -> IO ()
withTestCache action = withTemporaryDirectory $ \dir -> do
  createDirectory (dir </> "src")
  unsafeWithCustomCache (dangerouslyCreateProjectDirectory dir) (dir </> "project.db") $ \cache ->
    action (dir, cache)

testSelectAllSourceFiles :: ProjectCache -> IO [FilePath]
testSelectAllSourceFiles cache = sort <$> selectAllSourceFiles cache []
  (\as a -> return (getSourceFileRelativePath (sourceFilePath a) : as))

testBuildProjectFiles :: ProjectCache -> [FilePath] -> IO ()
testBuildProjectFiles cache paths =
  buildProjectFiles cache (map dangerouslyCreateSourceFilePath paths)

spec :: Spec
spec = around withTestCache $ do
  describe "buildProject" $ do
    it "does nothing for an empty source directory" $ \(_, cache) -> do
      testSelectAllSourceFiles cache `shouldReturn` []
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn` []

    it "adds new files to the cache" $ \(dir, cache) -> do
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]

    it "adds new files in a directory to the cache" $ \(dir, cache) -> do
      createDirectory (dir </> "src" </> "foo")
      createDirectory (dir </> "src" </> "bar")
      writeFile (dir </> "src" </> "foo" </> "a.ite") ""
      writeFile (dir </> "src" </> "foo" </> "b.ite") ""
      writeFile (dir </> "src" </> "bar" </> "c.ite") ""
      writeFile (dir </> "src" </> "bar" </> "d.ite") ""
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn`
        [ "bar" </> "c.ite"
        , "bar" </> "d.ite"
        , "foo" </> "a.ite"
        , "foo" </> "b.ite"
        ]

    it "adds new files nested in a directory files to the cache" $ \(dir, cache) -> do
      createDirectory (dir </> "src" </> "foo")
      createDirectory (dir </> "src" </> "foo" </> "qux")
      createDirectory (dir </> "src" </> "bar")
      createDirectory (dir </> "src" </> "bar" </> "lit")
      writeFile (dir </> "src" </> "foo" </> "qux" </> "a.ite") ""
      writeFile (dir </> "src" </> "foo" </> "qux" </> "b.ite") ""
      writeFile (dir </> "src" </> "bar" </> "lit" </> "c.ite") ""
      writeFile (dir </> "src" </> "bar" </> "lit" </> "d.ite") ""
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn`
        [ "bar" </> "lit" </> "c.ite"
        , "bar" </> "lit" </> "d.ite"
        , "foo" </> "qux" </> "a.ite"
        , "foo" </> "qux" </> "b.ite"
        ]

    it "does nothing on a rebuild" $ \(dir, cache) -> do
      let c = unsafeProjectCacheConnection cache
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      buildProject cache
      n <- totalChanges c
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]
      buildProject cache
      totalChanges c `shouldReturn` n
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]
      buildProject cache
      totalChanges c `shouldReturn` n
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]

    it "rebuilds files that were updated" $ \(dir, cache) -> do
      let c = unsafeProjectCacheConnection cache
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      time <- getModificationTime (dir </> "src" </> "b.ite")
      let oldTime = addUTCTime (-nominalDay) time
      buildProject cache
      -- Lie to our cache and say that the last time `b.ite` was modified was a day ago. We don’t
      -- want to delay our tests by actually waiting a couple seconds for the modification time
      -- to change.
      execute c "UPDATE source_file SET modification_time = ? WHERE path = ?" (oldTime, ("b.ite" :: String))
      changes c `shouldReturn` 1
      n <- totalChanges c
      query c "SELECT modification_time FROM source_file WHERE path = ?" (Only ("b.ite" :: String)) `shouldReturn` [Only oldTime]
      buildProject cache
      totalChanges c `shouldNotReturn` n
      query c "SELECT modification_time FROM source_file WHERE path = ?" (Only ("b.ite" :: String)) `shouldReturn` [Only time]

    it "deletes files that were removed" $ \(dir, cache) -> do
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite", "c.ite"]
      removeFile (dir </> "src" </> "b.ite")
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite", "c.ite"]
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]
      removeFile (dir </> "src" </> "c.ite")
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite"]
      removeFile (dir </> "src" </> "a.ite")
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite"]
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn` []

    it "moves files that were moved" $ \(dir, cache) -> do
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]
      renameFile (dir </> "src" </> "a.ite") (dir </> "src" </> "c.ite")
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn` ["b.ite", "c.ite"]
      renameFile (dir </> "src" </> "b.ite") (dir </> "src" </> "d.ite")
      testSelectAllSourceFiles cache `shouldReturn` ["b.ite", "c.ite"]
      buildProject cache
      testSelectAllSourceFiles cache `shouldReturn` ["c.ite", "d.ite"]

  describe "buildProjectFiles" $ do
    it "does nothing on an empty directory with empty input" $ \(_, cache) -> do
      testSelectAllSourceFiles cache `shouldReturn` []
      testBuildProjectFiles cache []
      testSelectAllSourceFiles cache `shouldReturn` []

    it "does nothing on an empty directory with missing input" $ \(_, cache) -> do
      testSelectAllSourceFiles cache `shouldReturn` []
      testBuildProjectFiles cache ["a.ite", "b.ite"]
      testSelectAllSourceFiles cache `shouldReturn` []

    it "does nothing with empty input" $ \(dir, cache) -> do
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      testBuildProjectFiles cache []
      testSelectAllSourceFiles cache `shouldReturn` []

    it "inserts listed new files into the cache" $ \(dir, cache) -> do
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      testBuildProjectFiles cache ["a.ite", "c.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]

    it "inserts listed new files into the cache incrementally" $ \(dir, cache) -> do
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      testBuildProjectFiles cache ["b.ite"]
      writeFile (dir </> "src" </> "d.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` ["b.ite"]
      testBuildProjectFiles cache ["a.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]
      testBuildProjectFiles cache ["d.ite", "c.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite", "c.ite", "d.ite"]

    it "does nothing when rebuilding untouched files" $ \(dir, cache) -> do
      let c = unsafeProjectCacheConnection cache
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      testBuildProjectFiles cache ["a.ite", "c.ite"]
      n <- totalChanges c
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]
      testBuildProjectFiles cache ["a.ite", "c.ite"]
      totalChanges c `shouldReturn` n
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]
      testBuildProjectFiles cache ["a.ite", "c.ite"]
      totalChanges c `shouldReturn` n
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]
      testBuildProjectFiles cache ["a.ite"]
      totalChanges c `shouldReturn` n
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]
      testBuildProjectFiles cache ["c.ite"]
      totalChanges c `shouldReturn` n
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]

    it "updates outdated listed files" $ \(dir, cache) -> do
      let c = unsafeProjectCacheConnection cache
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      aTime <- getModificationTime (dir </> "src" </> "a.ite")
      let aOldTime = addUTCTime (-nominalDay) aTime
      bTime <- getModificationTime (dir </> "src" </> "b.ite")
      let bOldTime = addUTCTime (-nominalDay) bTime
      testBuildProjectFiles cache ["a.ite", "b.ite", "c.ite"]
      -- Lie to our cache and say that the last time `b.ite` was modified was a day ago. We don’t
      -- want to delay our tests by actually waiting a couple seconds for the modification time
      -- to change.
      execute c "UPDATE source_file SET modification_time = ? WHERE path = ?" (aOldTime, ("a.ite" :: String))
      changes c `shouldReturn` 1
      execute c "UPDATE source_file SET modification_time = ? WHERE path = ?" (bOldTime, ("b.ite" :: String))
      changes c `shouldReturn` 1
      n <- totalChanges c
      query c "SELECT modification_time FROM source_file WHERE path = ?" (Only ("a.ite" :: String)) `shouldReturn` [Only aOldTime]
      query c "SELECT modification_time FROM source_file WHERE path = ?" (Only ("b.ite" :: String)) `shouldReturn` [Only bOldTime]
      testBuildProjectFiles cache ["b.ite", "c.ite"]
      totalChanges c `shouldNotReturn` n
      query c "SELECT modification_time FROM source_file WHERE path = ?" (Only ("a.ite" :: String)) `shouldReturn` [Only aOldTime]
      query c "SELECT modification_time FROM source_file WHERE path = ?" (Only ("b.ite" :: String)) `shouldReturn` [Only bTime]

    it "removes listed files from the cache that are deleted" $ \(dir, cache) -> do
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      testBuildProjectFiles cache ["a.ite", "b.ite", "c.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite", "c.ite"]
      removeFile (dir </> "src" </> "b.ite")
      removeFile (dir </> "src" </> "c.ite")
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite", "c.ite"]
      testBuildProjectFiles cache ["a.ite", "b.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]
      removeFile (dir </> "src" </> "a.ite")
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "c.ite"]
      testBuildProjectFiles cache ["c.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite"]
      testBuildProjectFiles cache ["a.ite", "b.ite"]
      testSelectAllSourceFiles cache `shouldReturn` []

    it "moves files that were moved" $ \(dir, cache) -> do
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      testSelectAllSourceFiles cache `shouldReturn` []
      testBuildProjectFiles cache ["a.ite", "b.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]
      renameFile (dir </> "src" </> "a.ite") (dir </> "src" </> "c.ite")
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]
      testBuildProjectFiles cache []
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite"]
      testBuildProjectFiles cache ["c.ite", "b.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite", "c.ite"]
      renameFile (dir </> "src" </> "b.ite") (dir </> "src" </> "d.ite")
      testSelectAllSourceFiles cache `shouldReturn` ["a.ite", "b.ite", "c.ite"]
      testBuildProjectFiles cache ["a.ite", "d.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["b.ite", "c.ite", "d.ite"]
      testBuildProjectFiles cache ["b.ite"]
      testSelectAllSourceFiles cache `shouldReturn` ["c.ite", "d.ite"]
