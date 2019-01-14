module Brite.Project.FilesSpec (spec) where

import Brite.Project.Files
import Control.Exception.Base (bracket)
import Data.List (sort)
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

-- Unwraps `ProjectDirectoryPath`.
findProjectDirectoryPath :: FilePath -> IO (Maybe FilePath)
findProjectDirectoryPath = (fmap projectDirectoryPath <$>) . findProjectDirectory

spec :: Spec
spec = around withTemporaryDirectory $ do
  describe "findProjectDirectory" $ do
    it "finds nothing when no config file exists" $ \dir -> do
      findProjectDirectoryPath dir `shouldReturn` Nothing
      findProjectDirectoryPath "." `shouldReturn` Nothing

    it "finds a config in the same directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectDirectoryPath dir `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath "." `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed the exact config path" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectDirectoryPath (dir </> "Brite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "Brite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed another file in the same directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      writeFile (dir </> "other.txt") ""
      findProjectDirectoryPath (dir </> "other.txt") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "other.txt") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in the same directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectDirectoryPath (dir </> "missing.txt") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "missing.txt") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when searching a child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      findProjectDirectoryPath (dir </> "src") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when searching a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      findProjectDirectoryPath (dir </> "src" </> "a") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath (dir </> "src" </> "a" </> "b") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a" </> "b") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a file in a child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "code.ite") ""
      findProjectDirectoryPath (dir </> "src" </> "code.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "code.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a file in a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      writeFile (dir </> "src" </> "a" </> "foo.ite") ""
      writeFile (dir </> "src" </> "a" </> "b" </> "bar.ite") ""
      findProjectDirectoryPath (dir </> "src" </> "a" </> "foo.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a" </> "foo.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath (dir </> "src" </> "a" </> "b" </> "bar.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a" </> "b" </> "bar.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in a child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      findProjectDirectoryPath (dir </> "src" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      findProjectDirectoryPath (dir </> "src" </> "a" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath (dir </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectDirectoryPath (dir </> "src") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectDirectoryPath (dir </> "src" </> "a") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath (dir </> "src" </> "a" </> "b") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a" </> "b") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in a missing child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectDirectoryPath (dir </> "src" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config when passed a missing file in a missing nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      findProjectDirectoryPath (dir </> "src" </> "a" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath (dir </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))
      findProjectDirectoryPath ("." </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` (Just (dir </> "Brite"))

    it "finds a config in a nested directory when using relative paths" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "b" </> "Brite") ""
      findProjectDirectoryPath ("." </> "a") `shouldReturn` (Just (dir </> "a" </> "Brite"))
      findProjectDirectoryPath ("." </> "b") `shouldReturn` (Just (dir </> "b" </> "Brite"))

    it "does not find a config in a sibling folder" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      findProjectDirectoryPath (dir </> "b") `shouldReturn` Nothing
      findProjectDirectoryPath ("." </> "b") `shouldReturn` Nothing

    it "does not find a config in a sibling folder when searching for a file" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "other.txt") ""
      writeFile (dir </> "b" </> "other.txt") ""
      findProjectDirectoryPath (dir </> "b" </> "other.txt") `shouldReturn` Nothing
      findProjectDirectoryPath ("." </> "b" </> "other.txt") `shouldReturn` Nothing

    it "finds the config file for the directory a file was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "src")
      writeFile (dir </> "b" </> "Brite") ""
      createFileLink (dir </> "a" </> "src" </> "test.ite") (dir </> "b" </> "src" </> "test.ite")
      findProjectDirectoryPath (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath (dir </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

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
      findProjectDirectoryPath (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "finds the config file for the directory a directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite") ""
      createDirectoryLink (dir </> "a" </> "src") (dir </> "b" </> "src")
      findProjectDirectoryPath (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath (dir </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "finds the config file for the nested directory a directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      createDirectory (dir </> "a" </> "src" </> "other")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite") ""
      createDirectoryLink (dir </> "a" </> "src") (dir </> "b" </> "src")
      findProjectDirectoryPath (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

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
      findProjectDirectoryPath (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectDirectoryPath ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "finds the config file even if .. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite") ""
      findProjectDirectoryPath (dir </> "a" </> ".." </> "b") `shouldReturn` Just (dir </> "b" </> "Brite")
      findProjectDirectoryPath ("." </> "a" </> ".." </> "b") `shouldReturn` Just (dir </> "b" </> "Brite")
      setCurrentDirectory (dir </> "a")
      findProjectDirectoryPath (".." </> "b") `shouldReturn` Just (dir </> "b" </> "Brite")

    it "does not find the config when .. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      findProjectDirectoryPath (dir </> "a" </> ".." </> "b") `shouldReturn` Nothing
      findProjectDirectoryPath ("." </> "a" </> ".." </> "b") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a")
      findProjectDirectoryPath (".." </> "b") `shouldReturn` Nothing

    it "finds the config file even if ../.. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "b" </> "Brite") ""
      writeFile (dir </> "b" </> "d" </> "Brite") ""
      findProjectDirectoryPath (dir </> "a" </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      findProjectDirectoryPath ("." </> "a" </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      findProjectDirectoryPath (dir </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      findProjectDirectoryPath ("." </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      setCurrentDirectory (dir </> "a")
      findProjectDirectoryPath (".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      setCurrentDirectory (dir </> "a" </> "c")
      findProjectDirectoryPath (".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")

    it "does not find the config when ../.. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "c" </> "Brite") ""
      findProjectDirectoryPath (dir </> "a" </> ".." </> "b" </> "d") `shouldReturn` Nothing
      findProjectDirectoryPath ("." </> "a" </> ".." </> "b" </> "d") `shouldReturn` Nothing
      findProjectDirectoryPath (dir </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      findProjectDirectoryPath ("." </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a")
      findProjectDirectoryPath (".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a" </> "c")
      findProjectDirectoryPath (".." </> ".." </> "b" </> "d") `shouldReturn` Nothing

    it "finds the config file even if ../x/../.. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "b" </> "Brite") ""
      writeFile (dir </> "b" </> "d" </> "Brite") ""
      findProjectDirectoryPath (dir </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      findProjectDirectoryPath ("." </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")
      setCurrentDirectory (dir </> "a" </> "c")
      findProjectDirectoryPath (".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d" </> "Brite")

    it "does not find the config when ../x/../.. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "c" </> "Brite") ""
      findProjectDirectoryPath (dir </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      findProjectDirectoryPath ("." </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a" </> "c")
      findProjectDirectoryPath (".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing

  describe "findSourceFilePaths" $ do
    it "finds source files immediately in the directory" $ \dir -> do
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.ite") ""
      writeFile (dir </> "c.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [dir </> "a.ite", dir </> "b.ite", dir </> "c.ite"]

    it "finds source files immediately in the directory ignoring non-source files" $ \dir -> do
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.txt") ""
      writeFile (dir </> "c.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [dir </> "a.ite", dir </> "c.ite"]

    it "finds source files when they have an extra leading extension" $ \dir -> do
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.test.ite") ""
      writeFile (dir </> "c.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [dir </> "a.ite", dir </> "b.test.ite", dir </> "c.ite"]

    it "ignores source files when they have an extra trailing extension" $ \dir -> do
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.ite.test") ""
      writeFile (dir </> "c.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [dir </> "a.ite", dir </> "c.ite"]

    it "includes directories with a source file extension same as source files" $ \dir -> do
      createDirectory (dir </> "b.ite")
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "c.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [dir </> "a.ite", dir </> "b.ite", dir </> "c.ite"]

    it "does not include source files in a directory with a source file extension" $ \dir -> do
      createDirectory (dir </> "b.ite")
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "c.ite") ""
      writeFile (dir </> "b.ite" </> "d.ite") ""
      writeFile (dir </> "b.ite" </> "e.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [dir </> "a.ite", dir </> "b.ite", dir </> "c.ite"]

    it "does not include directories with a source file different from as source files" $ \dir -> do
      createDirectory (dir </> "b.txt")
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "c.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [dir </> "a.ite", dir </> "c.ite"]

    it "does include source files in a directory with a different extension" $ \dir -> do
      createDirectory (dir </> "b.txt")
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "c.ite") ""
      writeFile (dir </> "b.txt" </> "d.ite") ""
      writeFile (dir </> "b.txt" </> "e.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [dir </> "a.ite", dir </> "b.txt" </> "d.ite", dir </> "b.txt" </> "e.ite", dir </> "c.ite"]

    it "finds source files in directories" $ \dir -> do
      createDirectory (dir </> "foo")
      createDirectory (dir </> "bar")
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.ite") ""
      writeFile (dir </> "foo" </> "c.ite") ""
      writeFile (dir </> "foo" </> "d.ite") ""
      writeFile (dir </> "bar" </> "e.ite") ""
      writeFile (dir </> "bar" </> "f.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [ dir </> "a.ite"
        , dir </> "b.ite"
        , dir </> "bar" </> "e.ite"
        , dir </> "bar" </> "f.ite"
        , dir </> "foo" </> "c.ite"
        , dir </> "foo" </> "d.ite"
        ]

    it "ignores files with non-source extensions in nested directories" $ \dir -> do
      createDirectory (dir </> "foo")
      createDirectory (dir </> "bar")
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.ite") ""
      writeFile (dir </> "nope.txt") ""
      writeFile (dir </> "foo" </> "c.ite") ""
      writeFile (dir </> "foo" </> "d.ite") ""
      writeFile (dir </> "foo" </> "nope.txt") ""
      writeFile (dir </> "bar" </> "e.ite") ""
      writeFile (dir </> "bar" </> "f.ite") ""
      writeFile (dir </> "bar" </> "nope.txt") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [ dir </> "a.ite"
        , dir </> "b.ite"
        , dir </> "bar" </> "e.ite"
        , dir </> "bar" </> "f.ite"
        , dir </> "foo" </> "c.ite"
        , dir </> "foo" </> "d.ite"
        ]

    it "finds source files in nested directories" $ \dir -> do
      createDirectory (dir </> "foo")
      createDirectory (dir </> "bar")
      createDirectory (dir </> "foo" </> "qux")
      createDirectory (dir </> "bar" </> "lit")
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.ite") ""
      writeFile (dir </> "foo" </> "c.ite") ""
      writeFile (dir </> "foo" </> "d.ite") ""
      writeFile (dir </> "foo" </> "qux" </> "e.ite") ""
      writeFile (dir </> "foo" </> "qux" </> "f.ite") ""
      writeFile (dir </> "bar" </> "g.ite") ""
      writeFile (dir </> "bar" </> "h.ite") ""
      writeFile (dir </> "bar" </> "lit" </> "i.ite") ""
      writeFile (dir </> "bar" </> "lit" </> "j.ite") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [ dir </> "a.ite"
        , dir </> "b.ite"
        , dir </> "bar" </> "g.ite"
        , dir </> "bar" </> "h.ite"
        , dir </> "bar" </> "lit" </> "i.ite"
        , dir </> "bar" </> "lit" </> "j.ite"
        , dir </> "foo" </> "c.ite"
        , dir </> "foo" </> "d.ite"
        , dir </> "foo" </> "qux" </> "e.ite"
        , dir </> "foo" </> "qux" </> "f.ite"
        ]

    it "ignores files with non-source extensions in nested directories" $ \dir -> do
      createDirectory (dir </> "foo")
      createDirectory (dir </> "bar")
      createDirectory (dir </> "foo" </> "qux")
      createDirectory (dir </> "bar" </> "lit")
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.ite") ""
      writeFile (dir </> "nope.txt") ""
      writeFile (dir </> "foo" </> "c.ite") ""
      writeFile (dir </> "foo" </> "d.ite") ""
      writeFile (dir </> "foo" </> "nope.txt") ""
      writeFile (dir </> "foo" </> "qux" </> "e.ite") ""
      writeFile (dir </> "foo" </> "qux" </> "f.ite") ""
      writeFile (dir </> "foo" </> "qux" </> "nope.txt") ""
      writeFile (dir </> "bar" </> "g.ite") ""
      writeFile (dir </> "bar" </> "h.ite") ""
      writeFile (dir </> "bar" </> "nope.txt") ""
      writeFile (dir </> "bar" </> "lit" </> "i.ite") ""
      writeFile (dir </> "bar" </> "lit" </> "j.ite") ""
      writeFile (dir </> "bar" </> "lit" </> "nope.txt") ""
      sort <$> findSourceFilePaths dir `shouldReturn`
        [ dir </> "a.ite"
        , dir </> "b.ite"
        , dir </> "bar" </> "g.ite"
        , dir </> "bar" </> "h.ite"
        , dir </> "bar" </> "lit" </> "i.ite"
        , dir </> "bar" </> "lit" </> "j.ite"
        , dir </> "foo" </> "c.ite"
        , dir </> "foo" </> "d.ite"
        , dir </> "foo" </> "qux" </> "e.ite"
        , dir </> "foo" </> "qux" </> "f.ite"
        ]

    it "finds linked files" $ \dir -> do
      createDirectory (dir </> "foo")
      createDirectory (dir </> "bar")
      writeFile (dir </> "foo" </> "a.ite") ""
      writeFile (dir </> "bar" </> "b.ite") ""
      writeFile (dir </> "foo" </> "c.ite") ""
      writeFile (dir </> "bar" </> "d.ite") ""
      createFileLink (dir </> "bar" </> "b.ite") (dir </> "foo" </> "b.ite")
      createFileLink (dir </> "bar" </> "d.ite") (dir </> "foo" </> "d.ite")
      sort <$> findSourceFilePaths (dir </> "foo") `shouldReturn`
        [ dir </> "foo" </> "a.ite"
        , dir </> "foo" </> "b.ite"
        , dir </> "foo" </> "c.ite"
        , dir </> "foo" </> "d.ite"
        ]
      sort <$> findSourceFilePaths dir `shouldReturn`
        [ dir </> "bar" </> "b.ite"
        , dir </> "bar" </> "d.ite"
        , dir </> "foo" </> "a.ite"
        , dir </> "foo" </> "b.ite"
        , dir </> "foo" </> "c.ite"
        , dir </> "foo" </> "d.ite"
        ]

    it "finds files in linked directories" $ \dir -> do
      createDirectory (dir </> "foo")
      createDirectory (dir </> "bar")
      createDirectory (dir </> "bar" </> "qux")
      writeFile (dir </> "foo" </> "a.ite") ""
      writeFile (dir </> "foo" </> "b.ite") ""
      writeFile (dir </> "bar" </> "qux" </> "c.ite") ""
      writeFile (dir </> "bar" </> "qux" </> "d.ite") ""
      createDirectoryLink (dir </> "bar" </> "qux") (dir </> "foo" </> "qux")
      sort <$> findSourceFilePaths (dir </> "foo") `shouldReturn`
        [ dir </> "foo" </> "a.ite"
        , dir </> "foo" </> "b.ite"
        , dir </> "foo" </> "qux" </> "c.ite"
        , dir </> "foo" </> "qux" </> "d.ite"
        ]
