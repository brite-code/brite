# Checker Test: `class_extends`

## Errors
- (1:19-1:32) Can not find `DoesNotExist1`.
- (2:24-2:37) Can not find `DoesNotExist2`.
- (4:27-4:35) Can not extend `Function` because it is not a base class.
  - (6:5-6:13) `Function`
- (5:32-5:40) Can not extend `Function` because it is not a base class.
  - (6:5-6:13) `Function`
- (7:27-7:35) Can not extend `Function` because it is not a base class.
  - (6:5-6:13) `Function`
- (8:32-8:40) Can not extend `Function` because it is not a base class.
  - (6:5-6:13) `Function`
- (11:24-11:33) Can not extend `BadParent` because it is not a base class.
  - (10:7-10:16) `BadParent`
- (20:34-20:48) Can not extend `BadGrandparent` because it is not a base class.
  - (19:7-19:21) `BadGrandparent`
