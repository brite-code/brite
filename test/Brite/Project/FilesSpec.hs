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

testFindProjectDirectory :: FilePath -> IO (Maybe FilePath)
testFindProjectDirectory = (fmap getProjectDirectory <$>) . findProjectDirectory

testTraverseProjectSourceFiles :: FilePath -> IO [FilePath]
testTraverseProjectSourceFiles =
  (sort <$>)
    . (map getSourceFilePath <$>)
    . traverseProjectSourceFiles (\as a -> return (a : as)) []
    . dangerouslyCreateProjectDirectory

spec :: Spec
spec = around withTemporaryDirectory $ do
  describe "findProjectDirectory" $ do
    it "finds nothing when no config file exists" $ \dir -> do
      testFindProjectDirectory dir `shouldReturn` Nothing
      testFindProjectDirectory "." `shouldReturn` Nothing

    it "finds a config in the same directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      testFindProjectDirectory dir `shouldReturn` Just dir
      testFindProjectDirectory "." `shouldReturn` Just dir

    it "finds a config when passed the exact config path" $ \dir -> do
      writeFile (dir </> "Brite") ""
      testFindProjectDirectory (dir </> "Brite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "Brite") `shouldReturn` Just dir

    it "finds a config when passed another file in the same directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      writeFile (dir </> "other.txt") ""
      testFindProjectDirectory (dir </> "other.txt") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "other.txt") `shouldReturn` Just dir

    it "finds a config when passed a missing file in the same directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      testFindProjectDirectory (dir </> "missing.txt") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "missing.txt") `shouldReturn` Just dir

    it "finds a config when searching a child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      testFindProjectDirectory (dir </> "src") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src") `shouldReturn` Just dir

    it "finds a config when searching a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      testFindProjectDirectory (dir </> "src" </> "a") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a") `shouldReturn` Just dir
      testFindProjectDirectory (dir </> "src" </> "a" </> "b") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "b") `shouldReturn` Just dir

    it "finds a config when passed a file in a child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "code.ite") ""
      testFindProjectDirectory (dir </> "src" </> "code.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "code.ite") `shouldReturn` Just dir

    it "finds a config when passed a file in a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
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
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      testFindProjectDirectory (dir </> "src" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "missing.ite") `shouldReturn` Just dir

    it "finds a config when passed a missing file in a nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "a")
      createDirectory (dir </> "src" </> "a" </> "b")
      testFindProjectDirectory (dir </> "src" </> "a" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory (dir </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` Just dir

    it "finds a config when passed a missing child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      testFindProjectDirectory (dir </> "src") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src") `shouldReturn` Just dir

    it "finds a config when passed a missing nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      testFindProjectDirectory (dir </> "src" </> "a") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a") `shouldReturn` Just dir
      testFindProjectDirectory (dir </> "src" </> "a" </> "b") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "b") `shouldReturn` Just dir

    it "finds a config when passed a missing file in a missing child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      testFindProjectDirectory (dir </> "src" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "missing.ite") `shouldReturn` Just dir

    it "finds a config when passed a missing file in a missing nested child directory" $ \dir -> do
      writeFile (dir </> "Brite") ""
      testFindProjectDirectory (dir </> "src" </> "a" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory (dir </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` Just dir
      testFindProjectDirectory ("." </> "src" </> "a" </> "b" </> "missing.ite") `shouldReturn` Just dir

    it "finds a config in a nested directory when using relative paths" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "b" </> "Brite") ""
      testFindProjectDirectory ("." </> "a") `shouldReturn` (Just (dir </> "a"))
      testFindProjectDirectory ("." </> "b") `shouldReturn` (Just (dir </> "b"))

    it "does not find a config in a sibling folder" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      testFindProjectDirectory (dir </> "b") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "b") `shouldReturn` Nothing

    it "does not find a config in a sibling folder when searching for a file" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "other.txt") ""
      writeFile (dir </> "b" </> "other.txt") ""
      testFindProjectDirectory (dir </> "b" </> "other.txt") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "b" </> "other.txt") `shouldReturn` Nothing

    it "finds the config file for the directory a file was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "src")
      writeFile (dir </> "b" </> "Brite") ""
      createFileLink (dir </> "a" </> "src" </> "test.ite") (dir </> "b" </> "src" </> "test.ite")
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "b")

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
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "b")

    it "finds the config file for the directory a directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite") ""
      createDirectoryLink (dir </> "a" </> "src") (dir </> "b" </> "src")
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "test.ite") `shouldReturn` Just (dir </> "b")

    it "finds the config file for the nested directory a directory was linked to" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "src")
      createDirectory (dir </> "a" </> "src" </> "other")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "src" </> "test.ite") ""
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite") ""
      createDirectoryLink (dir </> "a" </> "src") (dir </> "b" </> "src")
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "b")

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
      testFindProjectDirectory (dir </> "a" </> "src") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory (dir </> "a" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "a")
      testFindProjectDirectory ("." </> "b" </> "src") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "b" </> "src" </> "other" </> "test.ite") `shouldReturn` Just (dir </> "b")

    it "finds the config file even if .. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "b" </> "Brite") ""
      testFindProjectDirectory (dir </> "a" </> ".." </> "b") `shouldReturn` Just (dir </> "b")
      testFindProjectDirectory ("." </> "a" </> ".." </> "b") `shouldReturn` Just (dir </> "b")
      setCurrentDirectory (dir </> "a")
      testFindProjectDirectory (".." </> "b") `shouldReturn` Just (dir </> "b")

    it "does not find the config when .. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "b")
      writeFile (dir </> "a" </> "Brite") ""
      testFindProjectDirectory (dir </> "a" </> ".." </> "b") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "a" </> ".." </> "b") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a")
      testFindProjectDirectory (".." </> "b") `shouldReturn` Nothing

    it "finds the config file even if ../.. is used" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "b" </> "Brite") ""
      writeFile (dir </> "b" </> "d" </> "Brite") ""
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
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "c" </> "Brite") ""
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
      writeFile (dir </> "b" </> "Brite") ""
      writeFile (dir </> "b" </> "d" </> "Brite") ""
      testFindProjectDirectory (dir </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")
      testFindProjectDirectory ("." </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")
      setCurrentDirectory (dir </> "a" </> "c")
      testFindProjectDirectory (".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Just (dir </> "b" </> "d")

    it "does not find the config when ../x/../.. is used to escape a directory" $ \dir -> do
      createDirectory (dir </> "a")
      createDirectory (dir </> "a" </> "c")
      createDirectory (dir </> "b")
      createDirectory (dir </> "b" </> "d")
      writeFile (dir </> "a" </> "Brite") ""
      writeFile (dir </> "a" </> "c" </> "Brite") ""
      testFindProjectDirectory (dir </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      testFindProjectDirectory ("." </> "a" </> "c" </> ".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing
      setCurrentDirectory (dir </> "a" </> "c")
      testFindProjectDirectory (".." </> "c" </> ".." </> ".." </> "b" </> "d") `shouldReturn` Nothing

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
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "b.ite"
        , dir </> "src" </> "c.ite"
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
        [ dir </> "src" </> "d.ite"
        , dir </> "src" </> "e.ite"
        , dir </> "src" </> "f.ite"
        ]

    it "finds source files immediately in the directory ignoring non-source files" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.txt") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "c.ite"
        ]

    it "finds source files when they have an extra leading extension" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.test.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "b.test.ite"
        , dir </> "src" </> "c.ite"
        ]

    it "ignores source files when they have an extra trailing extension" $ \dir -> do
      createDirectory (dir </> "src")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "b.ite.test") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "c.ite"
        ]

    it "includes directories with a source file extension same as source files" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "b.ite")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "b.ite"
        , dir </> "src" </> "c.ite"
        ]

    it "does not include source files in a directory with a source file extension" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "b.ite")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      writeFile (dir </> "src" </> "b.ite" </> "d.ite") ""
      writeFile (dir </> "src" </> "b.ite" </> "e.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "b.ite"
        , dir </> "src" </> "c.ite"
        ]

    it "does not include directories with a source file different from as source files" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "b.txt")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "c.ite"
        ]

    it "does include source files in a directory with a different extension" $ \dir -> do
      createDirectory (dir </> "src")
      createDirectory (dir </> "src" </> "b.txt")
      writeFile (dir </> "src" </> "a.ite") ""
      writeFile (dir </> "src" </> "c.ite") ""
      writeFile (dir </> "src" </> "b.txt" </> "d.ite") ""
      writeFile (dir </> "src" </> "b.txt" </> "e.ite") ""
      testTraverseProjectSourceFiles dir `shouldReturn`
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "b.txt" </> "d.ite"
        , dir </> "src" </> "b.txt" </> "e.ite"
        , dir </> "src" </> "c.ite"
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
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "b.ite"
        , dir </> "src" </> "bar" </> "e.ite"
        , dir </> "src" </> "bar" </> "f.ite"
        , dir </> "src" </> "foo" </> "c.ite"
        , dir </> "src" </> "foo" </> "d.ite"
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
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "b.ite"
        , dir </> "src" </> "bar" </> "e.ite"
        , dir </> "src" </> "bar" </> "f.ite"
        , dir </> "src" </> "foo" </> "c.ite"
        , dir </> "src" </> "foo" </> "d.ite"
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
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "b.ite"
        , dir </> "src" </> "bar" </> "g.ite"
        , dir </> "src" </> "bar" </> "h.ite"
        , dir </> "src" </> "bar" </> "lit" </> "i.ite"
        , dir </> "src" </> "bar" </> "lit" </> "j.ite"
        , dir </> "src" </> "foo" </> "c.ite"
        , dir </> "src" </> "foo" </> "d.ite"
        , dir </> "src" </> "foo" </> "qux" </> "e.ite"
        , dir </> "src" </> "foo" </> "qux" </> "f.ite"
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
        [ dir </> "src" </> "a.ite"
        , dir </> "src" </> "b.ite"
        , dir </> "src" </> "bar" </> "g.ite"
        , dir </> "src" </> "bar" </> "h.ite"
        , dir </> "src" </> "bar" </> "lit" </> "i.ite"
        , dir </> "src" </> "bar" </> "lit" </> "j.ite"
        , dir </> "src" </> "foo" </> "c.ite"
        , dir </> "src" </> "foo" </> "d.ite"
        , dir </> "src" </> "foo" </> "qux" </> "e.ite"
        , dir </> "src" </> "foo" </> "qux" </> "f.ite"
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
        [ dir </> "foo" </> "src" </> "a.ite"
        , dir </> "foo" </> "src" </> "b.ite"
        , dir </> "foo" </> "src" </> "c.ite"
        , dir </> "foo" </> "src" </> "d.ite"
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
        [ dir </> "foo" </> "src" </> "a.ite"
        , dir </> "foo" </> "src" </> "b.ite"
        , dir </> "foo" </> "src" </> "qux" </> "c.ite"
        , dir </> "foo" </> "src" </> "qux" </> "d.ite"
        ]
