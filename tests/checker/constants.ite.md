# Checker Test: `constants`

## Errors
- (9:4-9:8) Can not change the type of `true` because a boolean can not be used as an integer.
  - (9:10-9:13) an integer
- (10:4-10:6) Can not change the type of `42` because a number can not be used as a boolean.
  - (10:8-10:12) a boolean
- (11:4-11:12) Can not change the type of `0xC0FF33` because an integer can not be used as a float.
  - (11:14-11:19) a float
- (12:4-12:10) Can not change the type of `3.1415` because a float can not be used as an integer.
  - (12:12-12:15) an integer
- (14:4-14:8) Can not change the type of `true` because a boolean can not be used as a number.
  - (14:10-14:13) a number
- (15:4-15:6) Can not change the type of `42` because a number can not be used as a boolean.
  - (15:8-15:12) a boolean
- (16:4-16:6) Can not change the type of `42` because a number can not be used as an integer.
  - (16:8-16:11) an integer
- (17:4-17:6) Can not change the type of `42` because a number can not be used as a float.
  - (17:8-17:13) a float
- (18:4-18:12) Can not change the type of `0xC0FF33` because an integer can not be used as a float.
  - (18:14-18:19) a float
- (19:4-19:10) Can not change the type of `3.1415` because a float can not be used as an integer.
  - (19:12-19:15) an integer
- (20:4-20:8) Can not change the type of `true` because a boolean can not be used as never.
  - (20:10-20:15) never
