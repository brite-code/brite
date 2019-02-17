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
| leading        | Trivia::Newlines::LF           | 1                          |
| 45:1-45:2      | Identifier                     | `a`                        |
| trailing       | Trivia::Spaces                 | 1                          |
| trailing       | Trivia::Comment::Block         |                            |
| trailing       | Trivia::Spaces                 | 1                          |
| 45:9-45:10     | Identifier                     | `b`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 46:1-46:2      | Identifier                     | `a`                        |
| trailing       | Trivia::Spaces                 | 1                          |
| trailing       | Trivia::Comment::Block         |                            |
| trailing       | Trivia::Spaces                 | 1                          |
| trailing       | Trivia::Comment::Block         |                            |
| trailing       | Trivia::Spaces                 | 1                          |
| 46:15-46:16    | Identifier                     | `b`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 47:1-47:2      | Identifier                     | `a`                        |
| trailing       | Trivia::Spaces                 | 1                          |
| trailing       | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 48:5-48:6      | Identifier                     | `b`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 49:1-49:2      | Identifier                     | `a`                        |
| trailing       | Trivia::Spaces                 | 1                          |
| trailing       | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 50:5-50:6      | Identifier                     | `b`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 51:1-51:2      | Identifier                     | `a`                        |
| trailing       | Trivia::Spaces                 | 1                          |
| trailing       | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 52:5-52:6      | Identifier                     | `b`                        |
| trailing       | Trivia::Newlines::LF           | 1                          |
| 53:1           | End                            |                            |
