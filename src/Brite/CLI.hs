module Brite.CLI (main) where

import System.Environment
import System.Exit
import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen

-- The main function for the Brite CLI.
main :: IO ()
main = do
  args <- getArgs
  exitWith =<< case parseArgs args of
    Right command -> execute command
    Left arg -> do
      displayDoc $
        bold (red (text "Error:")) <> text " " <>
        text "Unrecognized argument \"" <>
        bold (text arg) <> text "\". See below for correct usage." <>
        hardline <> hardline <> help
      return (ExitFailure 1)

-- Displays a pretty print ANSI document.
displayDoc :: Doc -> IO ()
displayDoc x = displayIO stdout (renderPretty 0.4 80 x)

-- A Brite command to be executed by the CLI.
data Command
  = EmptyCommand
  | NewCommand
  | BuildCommand [String]

-- Executes a command and returns an exit code.
execute :: Command -> IO ExitCode

-- An empty command will print out the help text and exit with a failure. We exit with a failure to
-- inform other CLI tools that this is not a correct usage of the Brite CLI.
execute EmptyCommand = do
  displayDoc help
  return (ExitFailure 1)

-- TODO: Actually implement the `new` command...
execute NewCommand = do
  displayDoc (bold (red (text "Error:")) <> text " The \"new\" command is currently unimplemented." <> hardline)
  return (ExitFailure 1)

-- TODO: Actually implement the `build` command...
execute (BuildCommand paths) = do
  displayDoc (bold (red (text "Error:")) <> text " The \"build\" command is currently unimplemented." <> hardline)
  return (ExitFailure 1)

-- Parses a list of CLI arguments and returns either a command or an error. An error could be an
-- unrecognized argument, for instance.
parseArgs :: [String] -> Either String Command
parseArgs = loop EmptyCommand
  where
    -- Parse the new command when we see the text `new`.
    loop EmptyCommand ("new" : args) = loop NewCommand args

    -- Parse a build command when we see the text `build`. Add every argument to our list of paths
    -- that doesn’t start with `-`. When we reach the end of our arguments reverse the list of paths
    -- since while parsing we added them in reverse.
    loop EmptyCommand ("build" : args) = loop (BuildCommand []) args
    loop (BuildCommand paths) (arg : args) | take 1 arg /= "-" = loop (BuildCommand (arg : paths)) args
    loop (BuildCommand paths) [] = Right (BuildCommand (reverse paths))

    -- If no one handled this argument then we have an unexpected argument.
    loop _ (arg : _) = Left arg

    -- We successfully parsed a command! Hooray! Return it.
    loop command [] = Right command

-- The help text for Brite. Prints a nice little box which is reminiscent of a postcard. Also allows
-- us to do clever work with alignment since we clearly have a left-hand-side.
help :: Doc
help =
  black (text "┌" <> text (replicate 78 '─') <> text "┐") <> hardline <>
  boxContent <>
  black (text "└" <> text (replicate 78 '─') <> text "┘") <> hardline
  where
    boxContent = mconcat $
      map
        (\a ->
          case a of
            Nothing -> black (text "│") <> fill 78 mempty <> black (text "│") <> hardline
            Just b -> black (text "│") <> fill 78 (text " " <> b) <> black (text "│") <> hardline) $
        [ Just $ bold (text "Brite")
        , Just $ text "A tool for product development."
        , Nothing
        , Just $ bold (text "Usage:")
        ] ++
        (map
          (\(a, b) -> Just $ black (text "$") <> text " " <> fill 32 (text a) <> black (text "# " <> text b))
          [ ("brite new {name}", "Create a new Brite project.")
          , ("brite build", "Build the code in your project.")
          , ("brite build {path...}", "Build the code at these paths.")
          ])
