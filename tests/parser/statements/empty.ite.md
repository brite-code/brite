# Parser Test: `empty`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (empty 2:3-2:4)
  (empty 3:3-3:4)
  (empty 3:4-3:5)
  (empty 4:3-4:4)
  (empty 4:4-4:5)
  (empty 4:5-4:6)
  (var 5:3-5:4 x)
  (var 6:3-6:4 x)
  (empty 6:5-6:6)
  (empty 7:3-7:4)
  (var 7:4-7:5 x)))
```
