# Lexer Test: `tabs`

## Tokens
| Range          | Kind                           | Data                       |
|----------------|--------------------------------|----------------------------|
| leading        | Trivia::Tabs                   | 1                          |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Tabs                   | 2                          |
| leading        | Trivia::Newlines::LF           | 1                          |
| leading        | Trivia::Tabs                   | 3                          |
| leading        | Trivia::Newlines::LF           | 1                          |
| 4:1            | End                            |                            |
