# Lexer Test: `comments`

## Tokens
| Range          | Kind                           | Data                       |
|----------------|--------------------------------|----------------------------|
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Spaces                 | 1                          |
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Spaces                 | 2                          |
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Spaces                 | 3                          |
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| 5:1-5:2        | Glyph                          | `/`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| 9:1-9:2        | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::CR           | 1                          |
| 11:1-11:2      | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::CRLF         | 1                          |
| 13:1-13:2      | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Line          |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| 15:1-15:2      | Glyph                          | `/`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 36:10-36:11    | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 37:9-37:10     | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 38:13-38:14    | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 39:11-39:12    | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 40:7-40:8      | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 41:8-41:9      | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 43:6-43:7      | Identifier                     | `x`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 44:1           | End                            |                            |
