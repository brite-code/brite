module Brite.Project.Files
  ( ProjectDirectory
  , getProjectDirectory
  , dangerouslyCreateProjectDirectory
  , SourceFilePath
  , getSourceFilePath
  , findProjectDirectory
  , traverseProjectSourceFiles
  , escapeFilePath
  ) where

import Data.Foldable (foldlM)
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

-- The path to a project directory. We only allow this type to be created through
-- `findProjectDirectory`. With a project directory path we are guaranteed that:
--
-- * The path is canonicalized. It is not a relative path and it does not contain special
--   directories like `..`.
-- * The path is a directory. Not a file.
-- * The directory pointed to by the path contains a Brite configuration file.
newtype ProjectDirectory = ProjectDirectory { getProjectDirectory :: FilePath }

-- Dangerously creates a new project directory. We assume you’ve validated all the assumptions that
-- the `ProjectDirectory` type has made.
dangerouslyCreateProjectDirectory :: FilePath -> ProjectDirectory
dangerouslyCreateProjectDirectory = ProjectDirectory

-- The path to a source file. We only allow this type to be created in this module. With a source
-- file path we are guaranteed that:
--
-- * The path is not a relative path and it does not contain special directories like `..`. The path
--   may contained linked files or directories that are not canonicalized. This is ok.
-- * The path is part of the source code of a Brite project.
-- * The path may point to a directory. The only criteria is that a source path must have the Brite
--   source code extension. A directory can pretend to be a source file by using the extension.
newtype SourceFilePath = SourceFilePath { getSourceFilePath :: FilePath }

-- Finds a Brite project configuration file based on the file path provided by the user.
--
-- * If the provided path is a file path we will search the parent directory.
-- * If we don’t find a Brite project configuration file in the directory we try again in the
--   parent directory.
-- * If we reach the file system root and we haven’t found a Brite project configuration file then
--   return `Nothing`.
findProjectDirectory :: FilePath -> IO (Maybe ProjectDirectory)
findProjectDirectory initialFilePath =
  fmap ProjectDirectory <$> (makeAbsolute initialFilePath >>= loop >>= mapM canonicalizePath)
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

-- Traverse all the Brite source files in the project directory’s `src` folder. The order in which
-- we traverse file paths is consistent across runs, but determined by the underlying file system
-- implementation.
--
-- IMPORTANT: Remember that this function will take a long time on large directories with many
-- files! After all, it has to traverse every file. We only want to run this over the `src` folder
-- in a Brite project.
--
-- NOTE: If the user has a directory that ends in `.ite` then we will consider that a source file
-- path! We do this as it decreases the number of system calls we need to make potentially helping
-- performance. (We have no evidence to prove this actually helps performance.)
traverseProjectSourceFiles :: (a -> SourceFilePath -> IO a) -> a -> ProjectDirectory -> IO a
traverseProjectSourceFiles update initialState (ProjectDirectory projectDirectory) = do
  let sourceDirectory = projectDirectory </> "src"
  doesSourceDirectoryExist <- doesDirectoryExist sourceDirectory
  if doesSourceDirectoryExist then loop initialState sourceDirectory else return initialState
  where
    loop currentState directoryPath = do
      -- Find all the file names in our current directory.
      fileNames <- listDirectory directoryPath
      -- Iterate through our file names and either recurse into the directory or check to see if
      -- this file is a source file.
      foldlM
        (\state fileName ->
          let filePath = directoryPath </> fileName in
          -- If we have a file extension of `.ite` then we have a source file! The source file might
          -- have a name which is not a valid identifier. If this is the case, we need to warn the
          -- user later on.
          --
          -- We only check the last extension. If the file has a name of `MyFile.test.ite` then we
          -- will add it to our list of source file paths. However, `MyFile.test` is not a valid
          -- identifier so we’ll warn about it in the future.
          --
          -- NOTE: We don’t bother to check and see if this is a file or a directory. As long as it
          -- has the `.ite` extension. This reduces the number of system calls we need to make.
          -- (Does this actually help performance? There’s no evidence to back that up.)
          --
          -- TODO: Warn if the file name is not a valid identifier.
          if takeExtension filePath == sourceFileExtension then
            update state (SourceFilePath filePath)
          -- If this file is not a directory and it’s not a Brite source file, ignore it.
          else do
            isDirectory <- doesDirectoryExist filePath
            -- If the file path is actually a directory path then recursively search that directory
            -- for source files.
            --
            -- TODO: Warn if the directory name is not a valid identifier.
            if isDirectory then
              loop state filePath
            else
              return state)
        -- Iterate through file names and start with the current state.
        currentState
        fileNames

-- Escape the slashes in a file path so that the file path may be used as a file name itself. We use
-- this to create directories for Brite caches. Inspired by [Hack/Flow][1].
--
-- [1]: https://github.com/facebook/flow/blob/ef74a645241326a10eba82164feb197eb8f10ac9/hack/utils/sys/path.ml#L55-L66
escapeFilePath :: FilePath -> String
escapeFilePath =
  foldr
    (\c s ->
      case c of
        '\\' -> 'z' : 'B' : s
        ':' -> 'z' : 'C' : s
        '/' -> 'z' : 'S' : s
        '\x00' -> 'z' : '0' : s
        '.' -> 'z' : 'D' : s
        'z' -> 'z' : 'Z' : s
        _ -> c : s)
    []
