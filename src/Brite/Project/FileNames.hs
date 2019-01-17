module Brite.Project.FileNames
  ( sourceFileExtension
  , configFileName
  , sourceDirectoryName
  , xdgRelativePath
  ) where

import Brite.Dev

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
-- We considered not included a file extension for the project configuration file. However, that
-- makes it difficult to talk about the config file in a sentence. “You need to edit your
--  `Brite.yaml` file.” vs “You need to edit your `Brite` file.” In the latter are we referring to
-- the Brite configuration file or a Brite source code file? Including an extension helps
-- distinguish “Brite” the proper noun from “`Brite.yaml`” the project configuration file.
configFileName :: String
configFileName = "Brite.yaml"

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
