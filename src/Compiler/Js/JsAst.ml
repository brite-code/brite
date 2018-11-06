type statement = Todo

let reserved_words = StringSet.of_list [
  "null";
  "true";
  "false";
  "if";
  "in";
  "do";
  "var";
  "for";
  "new";
  "try";
  "this";
  "else";
  "case";
  "void";
  "with";
  "enum";
  "while";
  "break";
  "catch";
  "throw";
  "const";
  "yield";
  "class";
  "super";
  "return";
  "typeof";
  "delete";
  "switch";
  "export";
  "import";
  "default";
  "finally";
  "extends";
  "function";
  "continue";
  "debugger";
  "instanceof";
  "implements";
  "interface";
  "package";
  "private";
  "protected";
  "public";
  "static";
  "let";
]

let is_reserved_word name =
  StringSet.mem name reserved_words
