# Lexer Test: `block_comment_end_2`

## Errors
- (1:14-1:14) We want `*/` but the file ends.

## Tokens
| Range          | Kind                           | Data                       |
|----------------|--------------------------------|----------------------------|
| leading        | Trivia::Comment::Block         |                            |
| 1:14           | End                            |                            |
