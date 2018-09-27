# Metaprogramming

TODO: Metaprogramming

For building large real-world applications meta-programming is essential. I don’t have a design for this yet.

I think it should be based on partial evaluation like Prepack. Where some functions *must* be executed statically so they can report build time errors. For example, a userland implementation of the equality interface should be able to assert that all fields also implement the equality interface.

Motivating examples:

- Userland implementation of equality or ordering traits.
- Code splitting. For example: `codeSplit(() -> (...))` should put whatever is executed in that function in a separate bundle.
- Extracting CSS at build time. XML syntax should allow for static style properties that are extracted to stylesheets. For example: `<View color=red />` could generate a CSS file with a `color: red` property.
- Parsing static GraphQL queries to generate types and runtime helpers. Like the Relay compiler for JavaScript.
- Building type-safe GraphQL schemas.
- Getting the source text string for an `assert()` message. For example: `assert(x == y)` should be able to throw an error that says `"Assertion failed: x == y"`.
- Pre-computation in constants. For example, `StyleSheet.create()`.
