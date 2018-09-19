# Modules

Brite source code is written in files. All of the files in a folder make up a module. You can think of a module as all of the files in a directory combined together. This means private declarations of a file are accessible to every other file in the folder. The syntax of a single Brite file is represented by {File}.

File : DeclarationList?

DeclarationList :
  - Declaration LineSeparator?
  - Declaration LineSeparator DeclarationList

Languages which treat both files *and* folders as modules like JavaScript, Rust, and Haskell need some way to associate a file as the implementation of a folder module. JavaScript has `folder/index.js`, Rust has `folder/mod.rs`, and Haskell has `folder.hs` adjacent to the actual folder. These systems make folder module implementations difficult to find.

Furthermore, these “index” files are often only used to re-export a subset of exports from each file. Taking care not to export module-private declarations. Brite’s module design greatly simplifies this use case. What you export from your file is available from your module. What you don’t export is available to the other files in your module.
