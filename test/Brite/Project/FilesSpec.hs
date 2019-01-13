module Brite.Project.FilesSpec (spec) where

import Brite.Project.Files
import Control.Exception.Base (bracket)
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
      temporaryDirectory <- canonicalizePath (systemTemporaryDirectory </> "Brite.Project.FilesSpec")
      createDirectory temporaryDirectory
      return temporaryDirectory)
    removeDirectoryRecursive
    (\temporaryDirectory ->
      withCurrentDirectory temporaryDirectory (action temporaryDirectory))

spec :: Spec
spec = around withTemporaryDirectory $ do
  describe "findProjectConfig" $ do
    it "finds nothing when no config file exists" $ \dir -> do
      findProjectConfig dir `shouldReturn` Nothing
      findProjectConfig "." `shouldReturn` Nothing

    it "finds a config in the same directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectConfig dir `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig "." `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed the exact config path" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectConfig (dir </> "Brite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "Brite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed another file in the same directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      writeFile (dir </> "other.txt") ""
      findProjectConfig (dir </> "other.txt") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "other.txt") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in the same directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectConfig (dir </> "missing.txt") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "missing.txt") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when searching a child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      findProjectConfig (dir </> "src") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when searching a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      findProjectConfig (dir </> "src" </> "a") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig (dir </> "src" </> "a" </> "b") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a" </> "b") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a file in a child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "code.ite") ""
      findProjectConfig (dir </> "src" </> "code.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "code.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a file in a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      writeFile (dir </> "src" </> "a" </> "foo.ite") ""
      writeFile (dir </> "src" </> "a" </> "b" </> "bar.ite") ""
      findProjectConfig (dir </> "src" </> "a" </> "foo.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a" </> "foo.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig (dir </> "src" </> "a" </> "b" </> "bar.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a" </> "b" </> "bar.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in a child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      findProjectConfig (dir </> "src" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      findProjectConfig (dir </> "src" </> "a" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig (dir </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectConfig (dir </> "src") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectConfig (dir </> "src" </> "a") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig (dir </> "src" </> "a" </> "b") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a" </> "b") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in a missing child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectConfig (dir </> "src" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in a missing nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectConfig (dir </> "src" </> "a" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig (dir </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectConfig ("." </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config in a nested directory when using relative paths" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "b" </> "Brite") ""
      findProjectConfig ("." </> "a") `shouldReturn` (Just (dir </> "a" </> "Brite"))
      findProjectConfig ("." </> "b") `shouldReturn` (Just (dir </> "b" </> "Brite"))

    it "does not find a config in a sibling folder" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      findProjectConfig (dir </> "b") `shouldReturn` Nothing
      findProjectConfig ("." </> "b") `shouldReturn` Nothing

    it "does not find a config in a sibling folder when searching for a file" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "other.txt") ""
      writeFile (dir </> "b" </> "other.txt") ""
      findProjectConfig (dir </> "b" </> "other.txt") `shouldReturn` Nothing
      findProjectConfig ("." </> "b" </> "other.txt") `shouldReturn` Nothing

    it "finds the config file for the directory a file was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "src")
      writeFile (dir </> "b" </> "Brite") ""
      createFileLink (dir </> "a" </> "src" </> "test.ite") (dir </> "b" </> "src" </> "test.ite")
      findProjectConfig (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig (dir </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectConfig ("." </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "finds the config file for the directory a nested file was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      createDirectory (dir </> "a" </> "src" </> "other")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "other" </> "test.ite") ""
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "src")
      createDirectory (dir </> "b" </> "src" </> "other")
      writeFile (dir </> "b" </> "Brite") ""
      createFileLink (dir </> "a" </> "src" </> "other" </> "test.ite") (dir </> "b" </> "src" </> "other" </> "test.ite")
      findProjectConfig (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectConfig ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectConfig ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "finds the config file for the directory a directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite") ""
      createDirectoryLink (dir </> "a" </> "src") (dir </> "b" </> "src")
      findProjectConfig (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig (dir </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectConfig ("." </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "finds the config file for the nested directory a directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      createDirectory (dir </> "a" </> "src" </> "other")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite") ""
      createDirectoryLink (dir </> "a" </> "src") (dir </> "b" </> "src")
      findProjectConfig (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectConfig ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectConfig ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "finds the config file for the directory a nested directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      createDirectory (dir </> "a" </> "src" </> "other")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "src")
      writeFile (dir </> "b" </> "Brite") ""
      createDirectoryLink (dir </> "a" </> "src" </> "other") (dir </> "b" </> "src" </> "other")
      findProjectConfig (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectConfig ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectConfig ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectConfig ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "finds the config file even if .. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite") ""
      findProjectConfig (dir </> "a" </> ".." </> "b") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectConfig ("." </> "a" </> ".." </> "b") `shouldReturn` Just (dir </> "b" </> "Brite")
      setCurrentDirectory (dir </> "a")
      findProjectConfig (".." </> "b") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "does not find the config when .. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      findProjectConfig (dir </> "a" </> ".." </> "b") `shouldReturn` Nothing
      findProjectConfig ("." </> "a" </> ".." </> "b") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a")
      findProjectConfig (".." </> "b") `shouldReturn` Nothing

    it "finds the config file even if ../.. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "b" </> "Brite") ""
      writeFile (dir </> "b" </> "d" </> "Brite") ""
      findProjectConfig (dir </> "a" </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      findProjectConfig ("." </> "a" </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      findProjectConfig (dir </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      findProjectConfig ("." </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      setCurrentDirectory (dir </> "a")
      findProjectConfig (".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      setCurrentDirectory (dir </> "a" </> "c")
      findProjectConfig (".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")

    it "does not find the config when ../.. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "c" </> "Brite") ""
      findProjectConfig (dir </> "a" </> ".." </> "b" </> "d") `shouldReturn` Nothing
      findProjectConfig ("." </> "a" </> ".." </> "b" </> "d") `shouldReturn` Nothing
      findProjectConfig (dir </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      findProjectConfig ("." </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a")
      findProjectConfig (".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a" </> "c")
      findProjectConfig (".." </> ".." </> "b" </> "d") `shouldReturn` Nothing

    it "finds the config file even if ../x/../.. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "b" </> "Brite") ""
      writeFile (dir </> "b" </> "d" </> "Brite") ""
      findProjectConfig (dir </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      findProjectConfig ("." </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      setCurrentDirectory (dir </> "a" </> "c")
      findProjectConfig (".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")

    it "does not find the config when ../x/../.. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "c" </> "Brite") ""
      findProjectConfig (dir </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      findProjectConfig ("." </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a" </> "c")
      findProjectConfig (".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
