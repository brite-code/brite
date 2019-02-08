# Source Code

## Lines

LineTerminator :
  - "LF"
  - "CR"
  - "CRLF"

## Identifier

IdentifierStart :
  - UnicodeIDStart
  - `_`

IdentifierContinue :
  - UnicodeIDContinue
  - `_`

UnicodeIDStart : "any Unicode code point with the Unicode property “ID_Start”"

UnicodeIDContinue : "any Unicode code point with the Unicode property “ID_Continue”"

Brite identifier’s use the [Unicode Identifier and Pattern Syntax](http://www.unicode.org/reports/tr31/) specification.
