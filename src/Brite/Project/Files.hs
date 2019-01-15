{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brite.Project.Files
  ( ProjectDirectory
  , getProjectDirectory
  , dangerouslyCreateProjectDirectory
  , SourceFilePath
  , getSourceFilePath
  , dangerouslyCreateSourceFilePath
  , getSourceFileTime
  , findProjectDirectory
  , intoSourceFilePath
  , findProjectCacheDirectory
  , traverseProjectSourceFiles
  ) where

import Brite.Dev
import Data.Foldable (foldlM)
import Data.Hashable (Hashable)
import Data.Time (UTCTime)
import Network.HTTP.Base (urlEncode)
import System.Directory hiding (isSymbolicLink)
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

-- The `src` convention is really well known among programmers to be the directory that source code
-- lives in. It also plays nicely in the alphabetical ordering of many file explorers and IDEs as
-- `src` comes after most other folder names in the alphabet like `docs` and `app` but
-- before `tests`.
--
-- I (Caleb) briefly considered using the directory `code` for Brite code, but decided against it
-- because the convention around `src` is so strong across many programming disciplines.
sourceDirectoryName :: String
sourceDirectoryName = "src"

-- The relative path we add to an [XDG directory][1] path.
--
-- In development we use a different folder than for releases. This way if a user chooses to run a
-- development build of Brite against their project they’ll get a separate cache so if they choose
-- to switch back to release mode they won’t be affected by the development mode cache.
--
-- [1]: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
xdgRelativePath :: String
xdgRelativePath = if isDev then "brite-dev" else "brite"

-- The path to a project directory. We only allow this type to be created through
-- `findProjectDirectory`. With a project directory path we are guaranteed that:
--
-- * The path is canonicalized. It is not a relative path and it does not contain special
--   directories like `..`.
-- * The path is a directory. Not a file.
-- * The directory pointed to by the path contains a Brite configuration file.
newtype ProjectDirectory = ProjectDirectory { getProjectDirectory :: FilePath }
  deriving (Show)

-- Dangerously creates a new project directory. We assume you’ve validated all the assumptions that
-- the `ProjectDirectory` type has made.
dangerouslyCreateProjectDirectory :: FilePath -> ProjectDirectory
dangerouslyCreateProjectDirectory = ProjectDirectory

-- The path to a source file. We only allow this type to be created in this module. With a source
-- file path we are guaranteed that:
--
-- * The path is relative to the source directory of the project directory the file is a part of.
-- * The path is part of the source code of a Brite project.
-- * The path may point to a directory. The only criteria is that a source path must have the Brite
--   source code extension. A directory can pretend to be a source file by using the extension.
newtype SourceFilePath = SourceFilePath { getSourceFilePath :: FilePath }
  deriving (Eq, Hashable, Show)

-- Gets the last modification time for our source file.
getSourceFileTime :: ProjectDirectory -> SourceFilePath -> IO UTCTime
getSourceFileTime (ProjectDirectory p1) (SourceFilePath p2) = getModificationTime (p1 </> p2)

-- Dangerously creates a new source file path. We assume you’ve validated all the assumptions that
-- the `ProjectCacheDirectory` type has made.
dangerouslyCreateSourceFilePath :: FilePath -> SourceFilePath
dangerouslyCreateSourceFilePath = SourceFilePath

-- Finds a Brite project configuration file based on the file path provided by the user.
--
-- * If the provided path is a file path we will search the parent directory.
-- * If we don’t find a Brite project configuration file in the directory we try again in the
--   parent directory.
-- * If we reach the file system root and we haven’t found a Brite project configuration file then
--   return `Nothing`.
findProjectDirectory :: FilePath -> IO (Maybe ProjectDirectory)
findProjectDirectory initialFilePath =
  fmap ProjectDirectory <$> (canonicalizePath initialFilePath >>= loop)
  where
    loop filePath = do
      isDirectory <- doesDirectoryExist filePath
      -- If the file is in fact, a directory, then search for our config file in this directory.
      -- If we find it return `Just`. If we don’t find it try the parent directory.
      if isDirectory then do
        let configFilePath = filePath </> configFileName
        isFile <- doesFileExist configFilePath
        if isFile then return (Just filePath) else configFileNotFound
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

-- Finds the directory for a project’s cache. If a directory does not yet exist then we create one.
--
-- All of a user’s Brite cache files live in `$XDG_CACHE_HOME/brite`. Where `$XDG_CACHE_HOME` comes
-- from the [XDG Base Directory Specification][1]. On non-Windows systems we default to `~/.cache`.
--
-- The cache files for a specific directory live in the URL encoded relative path from the user’s
-- home directory to the project directory. We assume that `$XDG_CACHE_HOME` is unique for every
-- user. If it is not then we are sad and might experience cache collisions since we take the
-- relative path from the user’s home directory.
--
-- [1]: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
findProjectCacheDirectory :: ProjectDirectory -> IO FilePath
findProjectCacheDirectory (ProjectDirectory projectDirectory) = do
  -- Get the user’s home directory.
  homeDirectory <- getHomeDirectory
  -- Make the path to our project directory relative to the user’s home directory.
  let relativeProjectDirectory = makeRelative homeDirectory projectDirectory
  -- Get the Brite user-specific cache directory. We depend on the assumption that this directory
  -- really is user-specific! If it’s not then taking the relative path between the user’s home
  -- directory and the project directory is unsafe...
  userCacheDirectory <- getXdgDirectory XdgCache xdgRelativePath
  -- Construct the project cache directory by adding the URL encoded relative path directory.
  let projectCacheDirectory = userCacheDirectory </> urlEncode relativeProjectDirectory
  -- Create the project cache directory if it does not already exist. Also create the project cache
  -- directory’s parent directories.
  createDirectoryIfMissing True projectCacheDirectory
  -- Return the project cache directory.
  return projectCacheDirectory

-- Converts a file path into a `SourceFilePath` if that path exists in the source directory of
-- our project.
--
-- Symbolic links will be resolved to their original path.
intoSourceFilePath :: ProjectDirectory -> FilePath -> IO (Maybe SourceFilePath)
intoSourceFilePath (ProjectDirectory projectDirectory) initialSourceFilePath = do
  -- Canonicalize the source file path we were provided. This removes indirections and
  -- resolves links.
  absoluteSourceFilePath <- canonicalizePath initialSourceFilePath
  -- Construct the source directory for our project.
  let sourceDirectory = projectDirectory </> sourceDirectoryName
  -- Construct the relative source file path based on our project’s source directory.
  let sourceFilePath = makeRelative sourceDirectory absoluteSourceFilePath
  -- `makeRelative` will never introduce `..` so if our file path is outside the source directory it
  -- will be left alone. If the file path was changed then we do indeed have a source file path!
  --
  -- If the extension of our source file is something we didn’t expect we also need to
  -- return nothing.
  if sourceFilePath == absoluteSourceFilePath then return Nothing
  else if takeExtension sourceFilePath /= sourceFileExtension then return Nothing
  else return (Just (SourceFilePath sourceFilePath))

-- Traverse all the Brite source files in the project directory’s source folder. The order in which
-- we traverse file paths is consistent across runs, but determined by the underlying file system
-- implementation.
--
-- Ignores all symbolic links.
--
-- IMPORTANT: Remember that this function will take a long time on large directories with many
-- files! After all, it has to traverse every file. We only want to run this over the source folder
-- in a Brite project.
--
-- NOTE: If the user has a directory that ends in `.ite` then we will consider that a source file
-- path! We do this as it decreases the number of system calls we need to make potentially helping
-- performance. (We have no evidence to prove this actually helps performance.)
traverseProjectSourceFiles :: ProjectDirectory -> a -> (a -> SourceFilePath -> IO a) -> IO a
traverseProjectSourceFiles (ProjectDirectory projectDirectory) initialState update = do
  -- Determine if the source directory exists. Only ask if the source directory is a symlink if we
  -- know that the source directory exists.
  doesSourceDirectoryExist <- doesDirectoryExist sourceDirectory
  isSourceDirectorySymbolicLink <-
    if doesSourceDirectoryExist then pathIsSymbolicLink sourceDirectory else return False
  -- The source directory must exist and must not be a symbolic link to begin traversal. Otherwise
  -- we don’t traverse anything.
  if doesSourceDirectoryExist && not isSourceDirectorySymbolicLink then
    loop initialState sourceDirectory
  else
    return initialState
  where
    -- Construct the source directory for our project.
    sourceDirectory = projectDirectory </> sourceDirectoryName

    loop currentState directoryPath = do
      -- Find all the file names in our current directory.
      fileNames <- listDirectory directoryPath
      -- Iterate through our file names and either recurse into the directory or check to see if
      -- this file is a source file.
      foldlM
        (\state fileName -> do
          let filePath = directoryPath </> fileName

          -- Check to see if the file path is a directory and whether or not it is a symbolic link.
          --
          -- NOTE: Does this make one or two system calls?
          isDirectory <- doesDirectoryExist filePath
          isSymbolicLink <- pathIsSymbolicLink filePath

          -- If this file is a symbolic link then ignore it. The symbolic link will either point to
          -- some directory _outside_ of the source directory or it will point to a directory inside
          -- the source directory. If the link does the former we want to ignore the file. If the
          -- link does the latter then we will see the file later during directory traversal.
          --
          -- We must do this for compatibility with `intoSourceFilePath`.
          if isSymbolicLink then
            return state

          -- If the file path is actually a directory path then recursively search that directory
          -- for source files.
          --
          -- TODO: Warn if the directory name is not a valid identifier.
          else if isDirectory then
            loop state filePath

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
            update state (SourceFilePath (makeRelative sourceDirectory filePath))

          -- If this file is not a directory and it’s not a Brite source file, ignore it.
          else
            return state)
        -- Iterate through file names and start with the current state.
        currentState
        fileNames
