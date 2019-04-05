# Lexer Test: `identifiers`

## Tokens
| Range          | Kind                           | Data                       |
|----------------|--------------------------------|----------------------------|
| 1:1-1:2        | Identifier                     | `x`                        |
| leading        | Trivia::Newlines::LF           | 1                          |
| 2:1-2:4        | Identifier                     | `foo`                      |
| leading        | Trivia::Newlines::LF           | 1                          |
| 3:1-3:4        | Identifier                     | `Bar`                      |
| leading        | Trivia::Newlines::LF           | 1                          |
| 4:1-4:4        | Identifier                     | `_42`                      |
| leading        | Trivia::Newlines::LF           | 1                          |
| 5:1-5:2        | Identifier                     | `Θ`                       |
| leading        | Trivia::Newlines::LF           | 1                          |
| 6:1-6:3        | Identifier                     | `𐐷`                     |
| leading        | Trivia::Newlines::LF           | 1                          |
| 7:1-7:4        | Identifier                     | `u𐐷`                    |
| leading        | Trivia::Newlines::LF           | 1                          |
| 8:1-8:4        | Identifier                     | `𐐷w`                    |
| leading        | Trivia::Newlines::LF           | 1                          |
| 9:1-9:2        | Glyph                          | `_`                        |
| leading        | Trivia::Newlines::LF           | 1                          |
| 10:1-10:5      | Glyph                          | `true`                     |
| leading        | Trivia::Newlines::LF           | 1                          |
| 11:1-11:6      | Glyph                          | `false`                    |
| leading        | Trivia::Newlines::LF           | 1                          |
| 12:1-12:5      | Identifier                     | `void`                     |
| leading        | Trivia::Newlines::LF           | 1                          |
| 13:1-13:4      | Glyph                          | `let`                      |
| leading        | Trivia::Newlines::LF           | 1                          |
| 14:1-14:3      | Glyph                          | `if`                       |
| leading        | Trivia::Newlines::LF           | 1                          |
| 15:1-15:5      | Glyph                          | `else`                     |
| leading        | Trivia::Newlines::LF           | 1                          |
| 16:1-16:3      | Glyph                          | `do`                       |
| leading        | Trivia::Newlines::LF           | 1                          |
| 17:1-17:5      | Glyph                          | `this`                     |
| leading        | Trivia::Newlines::LF           | 1                          |
| 18:1           | End                            |                            |
