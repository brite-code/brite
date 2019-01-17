module Brite.Project.FilesSpec (spec) where

import Brite.Project.Files
import Control.Exception (bracket)
import Data.List (sort)
import Data.Maybe (isJust)
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

testFindProjectDirectory :: FilePath -> IO (Maybe FilePath)
testFindProjectDirectory = (fmap getProjectDirectory <$>) . findProjectDirectory

testIntoSourceFilePath :: FilePath -> FilePath -> IO (Maybe FilePath)
testIntoSourceFilePath p1 p2 =
  fmap getSourceFileRelativePath <$> intoSourceFilePath (dangerouslyCreateProjectDirectory p1) p2

testTraverseProjectSourceFiles :: FilePath -> IO [FilePath]
testTraverseProjectSourceFiles =
  (sort <$>)
    . (map getSourceFileRelativePath <$>)
    . (\p -> traverseProjectSourceFiles p [] (\as a -> do
        yay <- isJust <$> intoSourceFilePath p (getSourceFilePath p a)
        if yay then return (a : as)
        else error "Expected intoSourceFilePath to succeed on all paths returned by traverseProjectSourceFiles."))
    . dangerouslyCreateProjectDirectory

spec :: Spec
spec = around withTemporaryDirectory $ do
  describe "findProjectDirectory" $ do
    it "finds nothing when no config file exists" $ \dir -> do
      testFindProjectDirectory dir `shouldReturn` Nothing
      testFindProjectDirectory "." `shouldReturn` Nothing

    it "finds a config in the same directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      testFindProjectDirectory dir `shouldReturn` Just dir
      testFindProjectDirectory "." `shouldReturn` Just dir

    it "finds a config when passed the exact config path" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "Brite.yaml") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "Brite.yaml") `shouldReturn` Just dir

    it "finds a config when passed another file in the same directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      writeFile (dir </> "other.txt") ""
      testFindProjectDirectory (dir </> "other.txt") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "other.txt") `shouldReturn` Just dir

    it "finds a config when passed a missing file in the same directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "missing.txt") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "missing.txt") `shouldReturn` Just dir

    it "finds a config when searching a child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      createDirectory (dir </> "src")
      testFindProjectDirectory (dir </> "src") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src") `shouldReturn` Just dir

    it "finds a config when searching a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      testFindProjectDirectory (dir </> "src" </> "a") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a") `shouldReturn` Just dir
      testFindProjectDirectory (dir </> "src" </> "a" </> "b") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "b") `shouldReturn` Just dir

    it "finds a config when passed a file in a child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "code.ite") ""
      testFindProjectDirectory (dir </> "src" </> "code.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "code.ite") `shouldReturn` Just dir

    it "finds a config when passed a file in a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      writeFile (dir </> "src" </> "a" </> "foo.ite") ""
      writeFile (dir </> "src" </> "a" </> "b" </> "bar.ite") ""
      testFindProjectDirectory (dir </> "src" </> "a" </> "foo.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "foo.ite") `shouldReturn` Just dir
      testFindProjectDirectory (dir </> "src" </> "a" </> "b" </> "bar.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "b" </> "bar.ite") `shouldReturn` Just dir

    it "finds a config when passed a missing file in a child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      createDirectory (dir </> "src")
      testFindProjectDirectory (dir </> "src" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "missing.ite") `shouldReturn` Just dir

    it "finds a config when passed a missing file in a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      testFindProjectDirectory (dir </> "src" </> "a" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory (dir </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` Just dir

    it "finds a config when passed a missing child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "src") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src") `shouldReturn` Just dir

    it "finds a config when passed a missing nested child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "src" </> "a") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a") `shouldReturn` Just dir
      testFindProjectDirectory (dir </> "src" </> "a" </> "b") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "b") `shouldReturn` Just dir

    it "finds a config when passed a missing file in a missing child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "src" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "missing.ite") `shouldReturn` Just dir

    it "finds a config when passed a missing file in a missing nested child directory" $ \dir -> do
      writeFile (dir </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "src" </> "a" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory (dir </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` Just dir

    it "finds a config in a nested directory when using relative paths" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      writeFile (dir </> "b" </> "Brite.yaml") ""
      testFindProjectDirectory ("." </> "a") `shouldReturn` (Just (dir </> "a"))
      testFindProjectDirectory ("." </> "b") `shouldReturn` (Just (dir </> "b"))

    it "does not find a config in a sibling folder" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "b") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "b") `shouldReturn` Nothing

    it "does not find a config in a sibling folder when searching for a file" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      writeFile (dir </> "a" </> "other.txt") ""
      writeFile (dir </> "b" </> "other.txt") ""
      testFindProjectDirectory (dir </> "b" </> "other.txt") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "b" </> "other.txt") `shouldReturn` Nothing

    it "finds the config file for the directory a file was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "src")
      writeFile (dir </> "b" </> "Brite.yaml") ""
      createFileLink (dir </> "a" </> "src" </> "test.ite") (dir </> "b" </> "src" </> "test.ite")
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory (dir </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")

    it "finds the config file for the directory a nested file was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      createDirectory (dir </> "a" </> "src" </> "other")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      writeFile (dir </> "a" </> "src" </> "other" </> "test.ite") ""
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "src")
      createDirectory (dir </> "b" </> "src" </> "other")
      writeFile (dir </> "b" </> "Brite.yaml") ""
      createFileLink (dir </> "a" </> "src" </> "other" </> "test.ite") (dir </> "b" </> "src" </> "other" </> "test.ite")
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory (dir </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory (dir </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")

    it "finds the config file for the directory a directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite.yaml") ""
      createDirectoryLink (dir </> "a" </> "src") (dir </> "b" </> "src")
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")

    it "finds the config file for the nested directory a directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      createDirectory (dir </> "a" </> "src" </> "other")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite.yaml") ""
      createDirectoryLink (dir </> "a" </> "src") (dir </> "b" </> "src")
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")

    it "finds the config file for the directory a nested directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      createDirectory (dir </> "a" </> "src" </> "other")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "src")
      writeFile (dir </> "b" </> "Brite.yaml") ""
      createDirectoryLink (dir </> "a" </> "src" </> "other") (dir </> "b" </> "src" </> "other")
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory (dir </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")

    it "finds the config file even if .. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "a" </> ".." </> "b") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "a" </> ".." </> "b") `shouldReturn` Just (dir </> "b")
      setCurrentDirectory (dir </> "a")
      testFindProjectDirectory (".." </> "b") `shouldReturn` Just (dir </> "b")

    it "does not find the config when .. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "a" </> ".." </> "b") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "a" </> ".." </> "b") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a")
      testFindProjectDirectory (".." </> "b") `shouldReturn` Nothing

    it "finds the config file even if ../.. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "b" </> "Brite.yaml") ""
      writeFile (dir </> "b" </> "d" </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "a" </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")
      testFindProjectDirectory ("." </> "a" </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")
      testFindProjectDirectory (dir </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")
      testFindProjectDirectory ("." </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")
      setCurrentDirectory (dir </> "a")
      testFindProjectDirectory (".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")
      setCurrentDirectory (dir </> "a" </> "c")
      testFindProjectDirectory (".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")

    it "does not find the config when ../.. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      writeFile (dir </> "a" </> "c" </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "a" </> ".." </> "b" </> "d") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "a" </> ".." </> "b" </> "d") `shouldReturn` Nothing
      testFindProjectDirectory (dir </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "a" </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a")
      testFindProjectDirectory (".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a" </> "c")
      testFindProjectDirectory (".." </> ".." </> "b" </> "d") `shouldReturn` Nothing

    it "finds the config file even if ../x/../.. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "b" </> "Brite.yaml") ""
      writeFile (dir </> "b" </> "d" </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")
      testFindProjectDirectory ("." </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")
      setCurrentDirectory (dir </> "a" </> "c")
      testFindProjectDirectory (".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")

    it "does not find the config when ../x/../.. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      writeFile (dir </> "a" </> "c" </> "Brite.yaml") ""
      testFindProjectDirectory (dir </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a" </> "c")
      testFindProjectDirectory (".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing

    it "finds projects in a linked directory" $ \dir -> do
      createDirectory (dir </> "a")
      writeFile (dir </> "a" </> "Brite.yaml") ""
      createDirectoryLink (dir </> "a") (dir </> "b")
      testFindProjectDirectory (dir </> "a") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "a") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "b") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b") `shouldReturn` Just (dir </> "a")

  describe "intoSourceFilePath" $ do
    it "does not find files inside the project directory" $ \dir -> do
      writeFile (dir </> "foo.ite") ""
      testIntoSourceFilePath dir "foo.ite" `shouldReturn` Nothing
      testIntoSourceFilePath dir "bar.ite" `shouldReturn` Nothing

    it "does not find files outside the project directory" $ \dir -> do
      createDirectory (dir </> "test")
      writeFile (dir </> "test" </> "foo.ite") ""
      testIntoSourceFilePath dir ("test" </> "foo.ite") `shouldReturn` Nothing
      testIntoSourceFilePath dir ("test" </> "bar.ite") `shouldReturn` Nothing

    it "finds files inside of the source directory" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "foo.ite") ""
      testIntoSourceFilePath dir ("src" </> "foo.ite") `shouldReturn` Just "foo.ite"
      testIntoSourceFilePath dir ("src" </> "bar.ite") `shouldReturn` Just "bar.ite"

    it "finds files inside of a directory inside the source directory" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      writeFile (dir </> "src" </> "a" </> "foo.ite") ""
      testIntoSourceFilePath dir ("src" </> "a" </> "foo.ite") `shouldReturn` Just ("a" </> "foo.ite")
      testIntoSourceFilePath dir ("src" </> "a" </> "bar.ite") `shouldReturn` Just ("a" </> "bar.ite")

    it "finds files inside of a nested directory inside the source directory" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      writeFile (dir </> "src" </> "a" </> "b" </> "foo.ite") ""
      testIntoSourceFilePath dir ("src" </> "a" </> "b" </> "foo.ite") `shouldReturn` Just ("a" </> "b" </> "foo.ite")
      testIntoSourceFilePath dir ("src" </> "a" </> "b" </> "bar.ite") `shouldReturn` Just ("a" </> "b" </> "bar.ite")

    it "ignores files with the wrong extension" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "foo.txt") ""
      writeFile (dir </> "src" </> "foo.ite.skip") ""
      writeFile (dir </> "src" </> "foo.test.ite") ""
      testIntoSourceFilePath dir ("src" </> "foo.txt") `shouldReturn` Nothing
      testIntoSourceFilePath dir ("src" </> "foo.txt.skip") `shouldReturn` Nothing
      testIntoSourceFilePath dir ("src" </> "foo.test.ite") `shouldReturn` Just "foo.test.ite"
      testIntoSourceFilePath dir ("src" </> "bar.txt") `shouldReturn` Nothing
      testIntoSourceFilePath dir ("src" </> "bar.txt.skip") `shouldReturn` Nothing
      testIntoSourceFilePath dir ("src" </> "bar.test.ite") `shouldReturn` Just "bar.test.ite"

    it "ignores directories" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "foo.ite")
      writeFile (dir </> "src" </> "bar.ite") ""
      testIntoSourceFilePath dir ("src" </> "foo.ite") `shouldReturn` Nothing
      testIntoSourceFilePath dir ("src" </> "bar.ite") `shouldReturn` Just "foo.ite"

  describe "traverseProjectSourceFiles" $ do
    it "finds nothing if a src directory does not exist" $ \dir -> do
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.ite") ""
      writeFile (dir </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn` []

    it "finds nothing if src is a file and not a directory" $ \dir -> do
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.ite") ""
      writeFile (dir </> "c.ite") ""
      writeFile (dir </> "src") ""
      testTraverseProjectSourceFiles dir `shouldReturn` []

    it "finds source files immediately in the directory" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "b.ite"
        , "c.ite"
        ]

    it "ignores files outside the src directory" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "a.ite") ""
      writeFile (dir </> "b.ite") ""
      writeFile (dir </> "c.ite") ""
      writeFile (dir </> "src" </> "d.ite") ""
      writeFile (dir </> "src" </> "e.ite") ""
      writeFile (dir </> "src" </> "f.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "d.ite"
        , "e.ite"
        , "f.ite"
        ]

    it "finds source files immediately in the directory ignoring non-source files" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.txt") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "c.ite"
        ]

    it "finds source files when they have an extra leading extension" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.test.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "b.test.ite"
        , "c.ite"
        ]

    it "ignores source files when they have an extra trailing extension" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite.test") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "c.ite"
        ]

    it "includes directories with a source file extension same as source files" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "b.ite")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "c.ite"
        ]

    it "does not include source files in a directory with a source file extension" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "b.ite")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      writeFile (dir </> "src" </> "b.ite" </> "d.ite") ""
      writeFile (dir </> "src" </> "b.ite" </> "e.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "b.ite/d.ite"
        , "b.ite/e.ite"
        , "c.ite"
        ]

    it "does not include directories with a source file different from as source files" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "b.txt")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "c.ite"
        ]

    it "does include source files in a directory with a different extension" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "b.txt")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      writeFile (dir </> "src" </> "b.txt" </> "d.ite") ""
      writeFile (dir </> "src" </> "b.txt" </> "e.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "b.txt" </> "d.ite"
        , "b.txt" </> "e.ite"
        , "c.ite"
        ]

    it "finds source files in directories" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "foo")
      createDirectory (dir </> "src" </> "bar")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "foo" </> "c.ite") ""
      writeFile (dir </> "src" </> "foo" </> "d.ite") ""
      writeFile (dir </> "src" </> "bar" </> "e.ite") ""
      writeFile (dir </> "src" </> "bar" </> "f.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "b.ite"
        , "bar" </> "e.ite"
        , "bar" </> "f.ite"
        , "foo" </> "c.ite"
        , "foo" </> "d.ite"
        ]

    it "ignores files with non-source extensions in nested directories" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "foo")
      createDirectory (dir </> "src" </> "bar")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "nope.txt") ""
      writeFile (dir </> "src" </> "foo" </> "c.ite") ""
      writeFile (dir </> "src" </> "foo" </> "d.ite") ""
      writeFile (dir </> "src" </> "foo" </> "nope.txt") ""
      writeFile (dir </> "src" </> "bar" </> "e.ite") ""
      writeFile (dir </> "src" </> "bar" </> "f.ite") ""
      writeFile (dir </> "src" </> "bar" </> "nope.txt") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "b.ite"
        , "bar" </> "e.ite"
        , "bar" </> "f.ite"
        , "foo" </> "c.ite"
        , "foo" </> "d.ite"
        ]

    it "finds source files in nested directories" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "foo")
      createDirectory (dir </> "src" </> "bar")
      createDirectory (dir </> "src" </> "foo" </> "qux")
      createDirectory (dir </> "src" </> "bar" </> "lit")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "foo" </> "c.ite") ""
      writeFile (dir </> "src" </> "foo" </> "d.ite") ""
      writeFile (dir </> "src" </> "foo" </> "qux" </> "e.ite") ""
      writeFile (dir </> "src" </> "foo" </> "qux" </> "f.ite") ""
      writeFile (dir </> "src" </> "bar" </> "g.ite") ""
      writeFile (dir </> "src" </> "bar" </> "h.ite") ""
      writeFile (dir </> "src" </> "bar" </> "lit" </> "i.ite") ""
      writeFile (dir </> "src" </> "bar" </> "lit" </> "j.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "b.ite"
        , "bar" </> "g.ite"
        , "bar" </> "h.ite"
        , "bar" </> "lit" </> "i.ite"
        , "bar" </> "lit" </> "j.ite"
        , "foo" </> "c.ite"
        , "foo" </> "d.ite"
        , "foo" </> "qux" </> "e.ite"
        , "foo" </> "qux" </> "f.ite"
        ]

    it "ignores files with non-source extensions in nested directories" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "foo")
      createDirectory (dir </> "src" </> "bar")
      createDirectory (dir </> "src" </> "foo" </> "qux")
      createDirectory (dir </> "src" </> "bar" </> "lit")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite") ""
      writeFile (dir </> "src" </> "nope.txt") ""
      writeFile (dir </> "src" </> "foo" </> "c.ite") ""
      writeFile (dir </> "src" </> "foo" </> "d.ite") ""
      writeFile (dir </> "src" </> "foo" </> "nope.txt") ""
      writeFile (dir </> "src" </> "foo" </> "qux" </> "e.ite") ""
      writeFile (dir </> "src" </> "foo" </> "qux" </> "f.ite") ""
      writeFile (dir </> "src" </> "foo" </> "qux" </> "nope.txt") ""
      writeFile (dir </> "src" </> "bar" </> "g.ite") ""
      writeFile (dir </> "src" </> "bar" </> "h.ite") ""
      writeFile (dir </> "src" </> "bar" </> "nope.txt") ""
      writeFile (dir </> "src" </> "bar" </> "lit" </> "i.ite") ""
      writeFile (dir </> "src" </> "bar" </> "lit" </> "j.ite") ""
      writeFile (dir </> "src" </> "bar" </> "lit" </> "nope.txt") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ "a.ite"
        , "b.ite"
        , "bar" </> "g.ite"
        , "bar" </> "h.ite"
        , "bar" </> "lit" </> "i.ite"
        , "bar" </> "lit" </> "j.ite"
        , "foo" </> "c.ite"
        , "foo" </> "d.ite"
        , "foo" </> "qux" </> "e.ite"
        , "foo" </> "qux" </> "f.ite"
        ]

    it "finds linked files" $ \dir -> do
      createDirectory (dir </> "foo")
      createDirectory (dir </> "bar")
      createDirectory (dir </> "foo" </> "src")
      createDirectory (dir </> "bar" </> "src")
      writeFile (dir </> "foo" </> "src" </> "a.ite") ""
      writeFile (dir </> "bar" </> "src" </> "b.ite") ""
      writeFile (dir </> "foo" </> "src" </> "c.ite") ""
      writeFile (dir </> "bar" </> "src" </> "d.ite") ""
      createFileLink (dir </> "bar" </> "src" </> "b.ite") (dir </> "foo" </> "src" </> "b.ite")
      createFileLink (dir </> "bar" </> "src" </> "d.ite") (dir </> "foo" </> "src" </> "d.ite")
      testTraverseProjectSourceFiles (dir </> "foo") `shouldReturn`
        [ "a.ite"
        , "c.ite"
        ]

    it "finds files in linked directories" $ \dir -> do
      createDirectory (dir </> "foo")
      createDirectory (dir </> "bar")
      createDirectory (dir </> "foo" </> "src")
      createDirectory (dir </> "bar" </> "src")
      createDirectory (dir </> "bar" </> "src" </> "qux")
      writeFile (dir </> "foo" </> "src" </> "a.ite") ""
      writeFile (dir </> "foo" </> "src" </> "b.ite") ""
      writeFile (dir </> "bar" </> "src" </> "qux" </> "c.ite") ""
      writeFile (dir </> "bar" </> "src" </> "qux" </> "d.ite") ""
      createDirectoryLink (dir </> "bar" </> "src" </> "qux") (dir </> "foo" </> "src" </> "qux")
      testTraverseProjectSourceFiles (dir </> "foo") `shouldReturn`
        [ "a.ite"
        , "b.ite"
        ]
