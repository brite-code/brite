# Checker Test: `constants`

## Errors
- (14:4-14:8) Can not change the type of `true` because `Bool` can not be used as `Num`.
  - (14:10-14:13) `Num`
- (15:4-15:6) Can not change the type of `42` because `Num` can not be used as `Bool`.
  - (15:8-15:12) `Bool`
- (16:4-16:6) Can not change the type of `42` because `Num` can not be used as `Int`.
  - (16:8-16:11) `Int`
- (17:4-17:6) Can not change the type of `42` because `Num` can not be used as `Float`.
  - (17:8-17:13) `Float`
- (18:4-18:12) Can not change the type of `0xC0FF33` because `Int` can not be used as `Float`.
  - (18:14-18:19) `Float`
- (19:4-19:10) Can not change the type of `3.1415` because `Float` can not be used as `Int`.
  - (19:12-19:15) `Int`
- (20:4-20:8) Can not change the type of `true` because `Bool` can not be used as `Never`.
  - (20:10-20:15) `Never`
