module Brite.Project.Files (findProjectConfig, findSourceFilePaths) where

import Data.Foldable (foldrM)
import System.Directory
import System.FilePath

-- The extension for a Brite source file is `ite`. We imagine there will be many puns which can be
-- created thanks to this file name. Just look at all those [rock types][1] that end in `ite`.
--
-- [1]: https://en.wikipedia.org/wiki/List_of_rock_types
sourceFileExtension :: String
sourceFileExtension = ".ite"

-- The name for a Brite configuration file. Like `package.json` in JavaScript, `Cargo.toml` in Rust,
-- `project.cabal` in Haskell, or `Makefile` in a programming language that uses Make (such as C).
--
-- The Brite configuration file is a YAML file. We choose YAML as it is terser than JSON and
-- supports user comments.
--
-- The file name does not have a file extension. This makes the branding of a Brite project
-- especially clear. There is precedence for this in `Makefile`s and `Dockerfile`s in addition to
-- dot-files like `.gitignore` and `.babelrc` as well as some plain-text files like `LICENSE`
-- and `README`. The Brite configuration file is the first I know of for a project to use their name
-- directly without modification as the configuration file name.
--
-- If we ever choose to change the format of Brite configuration files from YAML to something else
-- it will be very hard to upgrade since we aren’t using file extensions. YAML is a very
-- comprehensive and universal standard, so I (Caleb) don’t imagine us moving away from it at
-- this time.
--
-- Maybe in the future we’ll have Brite configuration written in Brite. After all, a big feature of
-- the language is it’s ability to control the build within it’s source files. However, there will
-- likely always be some project configuration which is preferably static. Like programming language
-- edition and dependencies. Not to mention we need the ability to statically manipulate the
-- configuration. Like adding dependencies when a user chooses to install a new package.
configFileName :: String
configFileName = "Brite"

-- Finds a Brite project configuration file based on the file path provided by the user.
--
-- * If the provided path is a file path we will search the parent directory.
-- * If we don’t find a Brite project configuration file in the directory we try again in the
--   parent directory.
-- * If we reach the file system root and we haven’t found a Brite project configuration file then
--   return `Nothing`.
findProjectConfig :: FilePath -> IO (Maybe FilePath)
findProjectConfig initialFilePath = makeAbsolute initialFilePath >>= loop >>= mapM canonicalizePath
  where
    loop filePath =
      -- If we see `..` then we manually implement going backwards so that we don’t end up searching
      -- in the directory we backed out of.
      if takeFileName filePath == ".." then
        let
          back n nextFilePath =
            if takeFileName nextFilePath == ".." then back (n + 1) (takeDirectory nextFilePath)
            else if n == 0 then nextFilePath
            else back (n - 1) (takeDirectory nextFilePath)
        in
          loop (back (1 :: Int) (takeDirectory filePath))
      else do
        isDirectory <- doesDirectoryExist filePath
        -- If the file is in fact, a directory, then search for our config file in this directory. If
        -- we find it return `Just`. If we don’t find it try the parent directory.
        if isDirectory then do
          let configFilePath = filePath </> configFileName
          isFile <- doesFileExist configFilePath
          if isFile then return (Just configFilePath) else configFileNotFound
        else
          configFileNotFound
      where
        -- If we didn’t find a configuration file then try the parent directory. If
        -- `filePath == takeDirectory filePath` then we know that we’ve reached the root of our file
        -- system so we return `Nothing` as no Brite config was found.
        configFileNotFound =
          let directoryPath = takeDirectory filePath in
            if filePath == directoryPath then return Nothing
            else loop directoryPath

-- Find all the Brite source files in the provided directory. We assume that the provided
-- directory exists.
--
-- IMPORTANT: Remember that this function will take a long time on large directories with many
-- files! After all, it has to traverse every file. We only want to run this over the `src` folder
-- in a Brite project.
findSourceFilePaths :: FilePath -> IO [FilePath]
findSourceFilePaths = loop []
  where
    loop initialSourceFilePaths directoryPath = do
      -- Find all the file names in our current directory.
      fileNames <- listDirectory directoryPath
      -- Iterate through our file names in reverse. This way when we add them to `sourceFilePaths`
      -- they will be in the correct order.
      foldrM
        (\fileName sourceFilePaths -> do
          let filePath = directoryPath </> fileName
          isDirectory <- doesDirectoryExist filePath
          -- If the file path is actually a directory path then recursively search that directory
          -- for source files.
          if isDirectory then
            loop sourceFilePaths filePath
          -- If we have a file extension of `.ite` then we have a source file! The source file might
          -- have a name which is not a valid identifier. If this is the case, we need to warn the
          -- user later on.
          --
          -- We only check the last extension. If the file has a name of `MyFile.test.ite` then we
          -- will add it to our list of source file paths. However, `MyFile.test` is not a valid
          -- identifier so we’ll warn about it in the future.
          --
          -- TODO: Warn if the file name is not a valid identifier.
          else if takeExtension filePath == sourceFileExtension then
            return (filePath : sourceFilePaths)
          -- If this file is not a directory and it’s not a Brite source file, ignore it.
          else
            return sourceFilePaths)
        -- Iterate through file names and start with the initial source file path list.
        initialSourceFilePaths
        fileNames
