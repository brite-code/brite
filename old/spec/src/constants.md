# Constants

Constant :
  - VoidConstant
  - BooleanConstant
  - IntegerConstant [lookahead != IdentifierContinue]
  - FloatConstant [lookahead != IdentifierContinue]

Some constant value written by the programmer which never changes during program execution.

{IntegerConstant} and {FloatConstant} may not be immediately followed by an identifier character. For example, `4px` is a syntax error and not `4` next to the identifier `px`. However, `4 px` is ok because of the space between the identifiers.

## Void Constant

VoidConstant : `void`

The void type only has a single value, the void constant. Similar to the “unit” type in other languages.

Brite uses a separate type, void, instead of an empty object (which would be the equivalent of an empty tuple) as the “unit” type to avoid confusing programmers. It is much more natural to write the function `fun() -> void` then to write the function `fun() -> {}`. The latter even looks like a function expression which would have a body!

We use a constant instead of a variable named `void` so that the programmer can match against `void` in a {Pattern}.

## Boolean Constant

BooleanConstant :
  - `true`
  - `false`

`true` and `false` values for the boolean type.

We use a constant instead of variables named `true`/`false` so that the programmer can match against them in a {Pattern}.

## Integer Constant

IntegerConstant :
  - IntegerDecimalConstant
  - IntegerBinaryConstant
  - IntegerHexadecimalConstant

IntegerDecimalConstant : /[0-9]+/

IntegerBinaryConstant : /0[bB][0-1]+/

IntegerHexadecimalConstant : /0[xX][0-9a-fA-F]+/

An integer constant can be written with any number of digits. Once a type has been decided for the integer so will its precision.

An integer, by default, is written in base 10. However, the programmer may also write an integer in base 2 ({IntegerBinaryConstant}) and base 16 ({IntegerHexadecimalConstant}).

## Float Constant

FloatConstant :
  - IntegerDecimalConstant FloatConstantExponent
  - IntegerDecimalConstant `.` IntegerDecimalConstant FloatConstantExponent?
  - `.` IntegerDecimalConstant FloatConstantExponent?
  - IntegerDecimalConstant `.` FloatConstantExponent?

FloatConstantExponent : /[eE][+-]?[0-9]+/

A {FloatConstant} corresponds to a [floating point number](https://en.wikipedia.org/wiki/Floating-point_arithmetic). It follows syntax very similar to other programming languages and data exchange formats (like JSON).

One notable difference is that {FloatConstant} allows a dangling fractional point. So constants like `.4` and `4.` are allowed. They are interpreted as equivalent to `0.4` and `4.0` respectively.
