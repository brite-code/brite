# Source Code

## Lines

LineTerminator :
  - "LF"
  - "CR"
  - "CRLF"

## Identifier

Identifier : IdentifierName but not Keyword

IdentifierName :
  - IdentifierStart
  - IdentifierName IdentifierContinue

IdentifierStart :
  - UnicodeIDStart
  - `_`

IdentifierContinue :
  - UnicodeIDContinue
  - `_`

Keyword : one of
  _     true     false   void
  let   if       else    do
  fun   return   loop    break

UnicodeIDStart : "any Unicode code point with the Unicode property “ID_Start”"

UnicodeIDContinue : "any Unicode code point with the Unicode property “ID_Continue”"

Brite identifier’s use the [Unicode Identifier and Pattern Syntax](http://www.unicode.org/reports/tr31/) specification.

TODO: Maybe create a distinction between binding identifiers and regular identifiers so that keywords may be used in limited contexts like object property names? Or have “special identifiers” with different meanings in various contexts.
