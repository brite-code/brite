# Source Text

The source text section is last because it deals with boring, pedantic, details of the characters that build up Brite programs.

## Identifiers

Identifier : /^[\_A-Za-z][_0-9a-za-z]*[']*$/ but not Keyword

Keyword :
  - `_`

BindingIdentifier : Identifier but not BindingKeyword

BindingKeyword :
  - `if`
  - `then`
  - `else`
  - `match`
  - `with`
  - `return`
  - `loop`
  - `while`
  - `do`
  - `for`
  - `in`
  - `break`
  - `continue`

Identifiers in Brite follow the [Unicode specification](http://www.unicode.org/reports/tr31/). With an extension allowing for `_` in the “start” character and “continue” characters.

It is common to use apostrophes as “primes” to denote new values with small modifications. For instance: `x`, `x'`, and `x''`.

There are some keywords which we do not allow as identifiers. We try to keep the set of keywords *very* small. Only keywords which are absolutely always ambiguous. Not even reserving keywords for the future like other languages do. If we want to add a new keyword we will build codemods to upgrade existing code while adding backwards compatibility for old versions. This is part of our philosophy to make the tooling more complex for a better programmer experience.

We have some pseudo-keywords we don’t reserve like `type`. We can design a non-ambiguous grammar for `type` which means we don’t need to reserve it. An identifier like `type` is also very common for a wide range of programs.

Since there are parse rules which are really difficult to express without reserving some keywords, we have a second category of keyword called {BindingKeyword}. These keywords are excluded from {BindingIdentifier} but not all identifiers in general. Binding keywords may not be used to bind a variable, but they may be used anywhere else. For instance, one cannot write the program `return = 42`, but they could write the program `x = { return = 42 }` since a property label is not a {BindingIdentifier}.

Note: Future specification writers, be careful about reserving common nouns like `type` or `class` in {BindingIdentifier}!

TODO: This regex is clearly not compatible with the Unicode specification. Neither is our implementation. Actually make both compatible with the Unicode specification.

### Identifier Conventions

Type identifiers should use PascalCase. Every other identifier should use camelCase.

Here’s the reasoning why. Some people argue that `snake_case` or `kebab-case` are easier to read. Since the `_` or `-` are closer to spaces which means the identifier looks more like natural language. However, even in languages with this convention (PHP, Clojure, Rust) type identifiers are still PascalCase.

camelCase pairs really well with PascalCase and is by far the predominant style for Brite’s targeted platforms. Web (JavaScript), Android (Java), and iOS (Swift or Objective-C).

TODO: There are a bunch of “soft rules” regarding naming we should consider making spec conventions. [See the Swift style guide for example.](https://github.com/raywenderlich/swift-style-guide)

### Identifier Privacy

Identifiers that start with a single underscore (`_`) are consider private. For example `_name` or `_data`. They may only be accessed within the scope where they were created and child scopes. This is true for class members, object properties, functions, and more.

This decision means we don’t add a bunch of syntactic keywords for privacy. It also means it is easy to tell the privacy of an identifier at its declaration sight _and_ at any call sites.

This decision does limit some identifier expressiveness. However, programming languages of the past tend to use the underscore prefix convention for denoting privacy. In Brite this convention is forced by the compiler.

Brite only applies privacy rules to identifiers with a single underscore. This means programmers can make “protected” identifiers which start with two underscores. For example `__name` or `__data`. The compiler provides no guarantees around privacy for identifiers with more then one underscore. However, it will hide these identifiers from documentation. This way programmers can “cheat” their way out of the privacy system while still hiding documentation and providing syntax vinegar for the wary caller.

```ite example
_add(a, b) -> a + b

twice(x) -> _add(x, x) // Ok: `_add` is defined in this file.
```

```ite counter-example
_add(a, b) -> a + b

// In another file...

twice(x) -> _add(x, x) // Error: `_add` is defined in another file!
```

Note: Some languages/linters overload the underscore prefix convention for marking an identifier as unused *and* for marking an identifier as private. This is confusing. It also means the language/linter can’t warn when the variable goes into use since the prefix might mean something other then the variable was unused. Brite will use annotations for marking a variable as unused. Which are more heavy handed, but also more explicit.

## Whitespace

Whitespace : "Any Unicode code point with the “White_space” property."

Whitespace is used to improve source text legibility. Any number of whitespace code points may be placed between two tokens unless otherwise specified with a negative lookahead. (Like in {CallExpressionArguments}.) As such whitespace is mostly insignificant but sometimes contributes to parsing.

## Line Terminators

LineTerminator :
  - "LF"
  - "CR+LF"

LineSeparator :
  - LineTerminator
  - `;`

Line terminators are used to improve the legibility of a Brite program. Any number of them may be placed between two tokens *unless otherwise specified*.

We only allow line feeds (`LF`, U+000A) and carriage returns plus a line feed (`CR+LF`, U+000D and U+000A) since those are the two most common ways to create a newline.

Note: When reporting errors, Brite will use the number of line terminators to report a line number.

### Line Terminator Ambiguity

Brite sometimes specially uses line terminators to seperate two language grammar productions. For instance, statements.

```ite example
x = 1
y = 2
```

Here we have two statements separated by newlines. They may also be written on the same line with a semicolon between them. This is what {LineSeparator} is for. It provides the flexibility to separate lines with a {LineTerminator} or a semicolon.

```ite example
x = 1; y = 2
```

Since we both use line terminators as statement terminators *and* we allow any number of line terminators between tokens so we sometimes run into cases where the latter rule leads us to interpret two statements as a single expression. Consider:

```ite example
x = f
(a, b)
```

We might interpret this as a function call `x = f(a, b)` or a tuple expression `x = f; (a, b)`. Those two options have significantly different meanings. In cases where we might greedily create a single expression instead of respecting the line terminators as statement separators we turn off the rule allowing line terminators between any two tokens.

Things can get really crazy depending on your grammar rules. Consider this example from {ConditionalExpression}:

```ite example
if(x) then(y) // ConditionalExpression

if(x) // CallExpression
then(y) // CallExpression
```

Note: Welcome to Clowntown.

Why is all of this ambiguity worth it? Why not use semicolons or make `if` a {Keyword}? After all, [JavaScript Automatic Semicolon Injection (ASI)](https://tc39.github.io/ecma262/#sec-automatic-semicolon-insertion) gets a lot of criticism.

Brite takes a strong stance that if there are opportunities to improve the Programmer Experience at the cost of language complexity but not at the cost of User Experience we take that opportunity in a heartbeat. Brite cares much more about the experience of Brite programmers much more then the experience of Brite toolchain programmers.

We avoid the problems of [JS ASI](https://tc39.github.io/ecma262/#sec-automatic-semicolon-insertion) by taking an opinionated stance on how Brite programs should be formatted. In cases where code might be ambiguous we always choose the case that the formatter will output. Supporting other cases is more a formality then anything else.

For instance, ASI chooses to disambiguate `x \n [1, 2]` to `x[1, 2]` instead of `x; [1, 2]`. For the opinionated people who like ASI this is an unfortunate choice which flaws the entire feature.
