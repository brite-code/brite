# TODO

1. Numbers
2. IDE tooling
3. Linting
4. Modules

## Misc Ideas

- Relational expressions form a complete relation. e.g. `a < b < c` is the same
  as `a < b && b < c` or `a == b == c` is the same as `a == b && b == c`. This
  is only possible if these operators follow the appropriate rules.
- Warn if an exported function is not annotated.
- Warn if the programmer uses a type name of `TypeN` asking for a more
  descriptive name. `TypeN` is the default name we give fresh types, so if a
  user copy/pastes we would like them to use a more descriptive name.
- Warn for shadowed variables. We don’t want programmer to be confused about
  where a name was defined. To reuse the same variable name for multiple values
  the programmer may use the update operator.
- Use bottom types to warn about unreachable code.
- Only reserve keywords for binding identifiers. Allow keywords in properties
  like `gen.void`.
