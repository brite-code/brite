# Lexer Test: `block_comment_nested`

## Errors
- (16:1-16:1) We want `*/` but the file ends.

## Tokens
| Range          | Kind                           | Data                       |
|----------------|--------------------------------|----------------------------|
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 3:13-3:14      | Identifier                     | `x`                        |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 7:19-7:20      | Identifier                     | `x`                        |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| leading        | Trivia::Spaces                 | 1                          |
| 9:19-9:20      | Identifier                     | `x`                        |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Comment::Block         |                            |
| 10:6-10:7      | Glyph                          | `*`                        |
| trailing       | Trivia::Spaces                 | 1                          |
| 10:8-10:9      | Glyph                          | `*`                        |
| 10:9-10:10     | Glyph                          | `/`                        |
| trailing       | Trivia::Spaces                 | 1                          |
| 10:11-10:12    | Identifier                     | `x`                        |
| leading        | Trivia::Newlines::LF           | 5                          |
| leading        | Trivia::Comment::Block         |                            |
| 16:1           | End                            |                            |
