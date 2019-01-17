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
