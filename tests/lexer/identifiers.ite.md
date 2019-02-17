# Lexer Test: `identifiers`

## Tokens
| Range          | Kind                           | Data                       |
|----------------|--------------------------------|----------------------------|
| 1:1-1:2        | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 2:1-2:4        | Identifier                     | `foo`                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 3:1-3:4        | Identifier                     | `Bar`                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 4:1-4:4        | Identifier                     | `_42`                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 5:1-5:2        | Identifier                     | `Θ`                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 6:1-6:3        | Identifier                     | `𐐷`                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 7:1-7:4        | Identifier                     | `u𐐷`                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 8:1-8:4        | Identifier                     | `𐐷w`                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 9:1-9:2        | Glyph                          | `_`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 10:1-10:5      | Glyph                          | `true`                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 11:1-11:6      | Glyph                          | `false`                    |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 12:1-12:5      | Identifier                     | `void`                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 13:1-13:4      | Identifier                     | `let`                      |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 14:1-14:3      | Identifier                     | `if`                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 15:1-15:5      | Identifier                     | `else`                     |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 16:1-16:3      | Identifier                     | `do`                       |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 17:1           | End                            |                            |
