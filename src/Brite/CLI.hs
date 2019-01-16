module Brite.CLI (cli) where

import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen

cli :: IO ()
cli = do
  displayIO stdout (renderPretty 0.4 80 help)
  return ()

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
