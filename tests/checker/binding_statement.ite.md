# Checker Test: `binding_statement`

## Errors
- (2:4-2:5) Can not find `a`.
- (3:4-3:5) Can not find `a`.
- (4:4-4:5) Can not find `b`.
- (5:4-5:5) Can not find `b`.
- (8:16-8:20) Can not set because a boolean cannot be used as an integer.
  - (1:1-1:1) an integer
- (10:4-10:5) Can not change the type because a number cannot be used as an integer.
  - (7:11-7:13) a number
  - (1:1-1:1) an integer
- (11:4-11:5) Can not change the type because a number cannot be used as a boolean.
  - (7:11-7:13) a number
  - (1:1-1:1) a boolean
- (13:4-13:5) Can not change the type because an integer cannot be used as a boolean.
  - (1:1-1:1) an integer
  - (1:1-1:1) a boolean
