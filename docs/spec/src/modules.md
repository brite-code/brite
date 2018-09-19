# Modules

File : DeclarationList?

DeclarationList :
  - Declaration LineSeparator?
  - Declaration LineSeparator DeclarationList

Brite programs are organized into files. {File} represents the syntax of a single Brite file. Files are organized into folders. A folder represents a module. All the public declarations of the folder’s files become the module exports. All the private declarations of the folder’s files are accessible within the module’s files and the files of its children, but are not accessible outside the module.

Lets say you had the following file structure.

```
- folder1
  - file1.ite (exports `foo`)
  - file2.ite (exports `bar`)
  - folder2
    - file3.ite (exports `qux`)
    - file4.ite (exports `lit`)
```

`folder2` is a module that exports `qux` and `lit`. `folder1` is a module which exports `foo`, `bar`, `folder2.qux`, and `folder2.lit`. Note that exports from a submodule are qualified. Private declarations in `file1.ite` are accessible in `file2.ite`, `file3.ite`, and `file4.ite`. Private declarations in `file3.ite` are only accessible in `file4.ite`.

This design is based on a couple of understandings of modern day programming styles. Firstly, files are used to group similar logic since putting everything in one file gets to crowded. Conceptually your program is one big file and files are simply concatenated together. Under this design files in the same folder could literally be concatenated and you’d get the same behavior.

Secondly, languages which treat both files *and* folders as modules like JavaScript, Rust, and Haskell need some way to associate a file as the implementation of a folder module. JavaScript has `folder/index.js`, Rust has `folder/mod.rs`, and Haskell has `folder.hs` next to the actual folder which makes the file hard to find in file systems with different groups for files and folders. Furthermore, these “index” files are often only used to re-export a subset of declarations from each file since the file must export everything which is usable within its folder. Brite’s module design greatly simplifies this use case. What you export from your file is available from your module. What you don’t export is available to the other files in your module.
