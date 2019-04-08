module Brite.CLI (main) where

import Brite.Dev
import Brite.DiagnosticMarkup (toANSIDoc)
import Brite.Exception
import Brite.Project.Build (buildProject, buildProjectFiles)
import Brite.Project.Cache (withCache)
import Brite.Project.Files
import System.Directory (removeDirectoryRecursive)
import System.Environment
import System.Exit
import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

-- The main function for the Brite CLI.
main :: IO ()
main = do
  args <- getArgs
  exitWith =<< case parseArgs args of
    Right command -> do
      result <- catchEverything (execute command)
      case result of
        Left message -> displayDoc (errorMessage (toANSIDoc message)) *> return (ExitFailure 1)
        Right exitCode -> return exitCode

    Left arg -> do
      displayDoc $
        errorMessage
          (Doc.text "Unrecognized argument " <>
            Doc.bold (Doc.text arg) <>
            Doc.text ". See below for the correct usage.") <>
        Doc.hardline <>
        helpMessage
      return (ExitFailure 1)

-- Displays a pretty print ANSI document.
displayDoc :: Doc -> IO ()
displayDoc x = Doc.displayIO stdout (Doc.renderPretty 1 80 x)

{--------------------------------------------------------------------------------------------------}
{- Commands                                                                                       -}
{--------------------------------------------------------------------------------------------------}

-- A Brite command to be executed by the CLI.
data Command
  -- An empty command means the user did not specify any command line arguments. Here we print out
  -- the help message and return a failure exit code.
  = EmptyCommand

  -- If the user ran the help command then they are looking to see the help message. Here we print
  -- out the help message and exit successfully.
  | HelpCommand

  -- If the user ran the “new” command then they want to create a new Brite project in the
  -- provided directory.
  | NewCommand

  -- If the user ran the “build” command they want to build the provided file paths.
  | BuildCommand [String]

  -- If the user ran the “reset” command then we delete their cache and any generated resources.
  | ResetCommand

-- Executes a command and returns an exit code.
execute :: Command -> IO ExitCode

-- An empty command will print out the help text and exit with a failure. We exit with a failure to
-- inform other CLI tools that this is not a correct usage of the Brite CLI.
execute EmptyCommand = do
  displayDoc helpMessage
  return (ExitFailure 1)

-- Print the help message and exit successfully.
execute HelpCommand = do
  displayDoc helpMessage
  return ExitSuccess

-- TODO: Actually implement the `new` command...
execute NewCommand = do
  displayDoc (errorMessage (Doc.text "The `new` command is currently unimplemented."))
  return (ExitFailure 1)

-- Build some Brite code!
--
-- If no source file paths were provided to the command then we will search for the project
-- directory in our current directory. If we find the project directory then we will build the
-- entire project.
--
-- If some source file paths were provided we will search for the project directory in our current
-- directory. Then we will convert all the paths we were provided into source file paths. Finally
-- we will build only those project files.
--
-- NOTE: The build command will accept files that don’t exist in the file system. This way you can
-- delete files from your cache that no longer exist in the file system.
--
-- TODO: Progress indicator showing how much work we’ve done so far in building the project. This
-- way Brite’s not a black box. Also consider telling the user when we’re retrying their
-- transaction? `withImmediateTransaction` has retry logic in case someone is trying to read from
-- the database while we’re trying to commit a large transaction. This could potentially take a long
-- time we should let the user know if it does.
execute (BuildCommand initialSourceFilePaths) =
  if null initialSourceFilePaths then do
    projectDirectory <- findProjectDirectoryOrThrow "."
    withCache projectDirectory buildProject
    return ExitSuccess
  else do
    projectDirectory <- findProjectDirectoryOrThrow "."
    sourceFilePaths <- traverse (intoSourceFilePathOrThrow projectDirectory) initialSourceFilePaths
    withCache projectDirectory (flip buildProjectFiles sourceFilePaths)
    return ExitSuccess

-- Cleans the current project’s cache.
--
-- NOTE: We use the name `reset` instead of the traditional `clean` to imply this is a hard reset of
-- the programmer’s project. We’d prefer that the programmer does not run `reset` if possible since
-- that will delete their cache slowing down future builds. We’d prefer the user run `brite build`
-- to make sure all their files are rebuilt. `clean` implies that user is deleting generated
-- resources so re-generating those resources should not take more time, but in fact the resources
-- we delete when the programmer calls `brite reset` significantly improves the performance of
-- their builds.
execute ResetCommand = do
  projectDirectory <- findProjectDirectoryOrThrow "."
  projectCacheDirectory <- findProjectCacheDirectory projectDirectory
  removeDirectoryRecursive projectCacheDirectory
  return ExitSuccess

-- Parses a list of CLI arguments and returns either a command or an error. An error could be an
-- unrecognized argument, for instance.
parseArgs :: [String] -> Either String Command
parseArgs = loop EmptyCommand
  where
    -- If we see common help flags then the user is asking for the help command.
    loop EmptyCommand ("-h" : args) = loop HelpCommand args
    loop EmptyCommand ("--help" : args) = loop HelpCommand args

    -- Parse the “new” command.
    loop EmptyCommand ("new" : args) = loop NewCommand args

    -- Parse a build command when we see the text `build`. Add every argument to our list of paths
    -- that doesn’t start with `-`. When we reach the end of our arguments reverse the list of paths
    -- since while parsing we added them in reverse.
    loop EmptyCommand ("build" : args) = loop (BuildCommand []) args
    loop (BuildCommand paths) (arg : args) | take 1 arg /= "-" = loop (BuildCommand (arg : paths)) args
    loop (BuildCommand paths) [] = Right (BuildCommand (reverse paths))

    -- Parse the “reset” command.
    loop EmptyCommand ("reset" : args) = loop ResetCommand args

    -- If no one handled this argument then we have an unexpected argument.
    loop _ (arg : _) = Left arg

    -- We successfully parsed a command! Hooray! Return it.
    loop command [] = Right command

{--------------------------------------------------------------------------------------------------}
{- Messages                                                                                       -}
{--------------------------------------------------------------------------------------------------}

-- An operational error message logged by the CLI.
errorMessage :: Doc -> Doc
errorMessage x =
  Doc.bold (Doc.red (Doc.text "Error:")) <> Doc.text " " <> x <> Doc.hardline

-- The help text for Brite. Prints a nice little box which is reminiscent of a postcard. Also allows
-- us to do clever work with alignment since we clearly have a left-hand-side.
helpMessage :: Doc
helpMessage =
  Doc.black (Doc.text "┌" <> Doc.text (replicate 78 '─') <> Doc.text "┐") <> Doc.hardline <>
  boxContent <>
  Doc.black (Doc.text "└" <> Doc.text (replicate 78 '─') <> Doc.text "┘") <> Doc.hardline
  where
    boxContent = mconcat $
      map
        (\a ->
          case a of
            Nothing ->
              Doc.black (Doc.text "│") <>
              Doc.fill 78 mempty <>
              Doc.black (Doc.text "│") <>
              Doc.hardline
            Just b ->
              Doc.black (Doc.text "│") <>
              Doc.fill 78 (Doc.text " " <> b) <>
              Doc.black (Doc.text "│") <> Doc.hardline) $

        [ Just $ Doc.bold (Doc.text "Brite")
        , Just $ Doc.text "A tool for product development."
        , Nothing
        , Just $ Doc.bold (Doc.text "Usage:")
        ] ++
        (map
          (\(a, b) -> Just $
            Doc.black (Doc.text "$") <>
            Doc.text " " <>
            Doc.fill 32 (Doc.text (if isDev then "brite-dev" else "brite") <> Doc.text " " <> Doc.text a) <>
            Doc.black (Doc.text "# " <> Doc.text b))

          [ ("new {name}", "Create a new Brite project.")
          , ("build", "Build the code in your project.")
          , ("build {path...}", "Build the code at these paths.")

          -- TODO: Add format command. Also, combine `build` and `build {path...}` documentation.

          -- NOTE: We intentionally don’t document `brite reset` here. We don’t want programmers
          -- using `brite reset` except in dire circumstances where they absolutely need to reset
          -- their project since `brite reset` throws away their cache which slows down everything.
          -- We would prefer the user to run `brite build`.
          ])
