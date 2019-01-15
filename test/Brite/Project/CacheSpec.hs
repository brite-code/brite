{-# LANGUAGE OverloadedStrings #-}

module Brite.Project.CacheSpec (spec) where

import Brite.Exception
import Brite.Project.Files
import Brite.Project.Cache
import Control.Exception (bracket)
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
      temporaryDirectory <- canonicalizePath (systemTemporaryDirectory </> "Brite.Project.CacheSpec")
      createDirectory temporaryDirectory
      return temporaryDirectory)
    removeDirectoryRecursive
    (\temporaryDirectory ->
      withCurrentDirectory temporaryDirectory (action temporaryDirectory))

testWithCache :: FilePath -> (ProjectCache -> IO a) -> IO a
testWithCache p = unsafeWithCustomCache (dangerouslyCreateProjectDirectory p) (p </> "project.db")

spec :: Spec
spec = around withTemporaryDirectory $ do
  describe "withDatabase" $ do
    it "creates a new SQLite database file when none exists" $ \dir -> do
      doesFileExist (dir </> "project.db") `shouldReturn` False
      testWithCache dir (const (return ()))
      doesFileExist (dir </> "project.db") `shouldReturn` True

    it "runs migrations on a freshly created database" $ \dir -> do
      testWithCache dir $ \db -> do
        let c = unsafeProjectCacheConnection db
        userVersionRows <- query_ c "PRAGMA user_version" :: IO [Only Int]
        length userVersionRows `shouldBe` 1
        fromOnly (head userVersionRows) `shouldNotBe` 0

    it "runs migrations on an empty database" $ \dir -> withConnection "project.db" $ \c -> do
      userVersionRows1 <- query_ c "PRAGMA user_version" :: IO [Only Int]
      length userVersionRows1 `shouldBe` 1
      fromOnly (head userVersionRows1) `shouldBe` 0
      testWithCache dir (const (return ()))
      userVersionRows2 <- query_ c "PRAGMA user_version" :: IO [Only Int]
      length userVersionRows2 `shouldBe` 1
      fromOnly (head userVersionRows2) `shouldNotBe` 0

    it "errors if the user version is larger than expected" $ \dir -> withConnection "project.db" $ \c -> do
      execute_ c "PRAGMA user_version = 9001"
      testWithCache dir (const (return ())) `shouldThrow` \e ->
        case e of
          ProjectCacheUnrecognizedVersion -> True
