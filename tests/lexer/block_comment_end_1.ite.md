# Lexer Test: `block_comment_end_1`

## Errors
- (2:1-2:1) We want `*/` but the file ends.

## Tokens
| Range          | Kind                           | Data                       |
|----------------|--------------------------------|----------------------------|
| leading        | Trivia::Comment::Block         |                            |
| 2:1            | End                            |                            |
