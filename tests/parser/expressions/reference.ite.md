# Parser Test: `reference`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (var 2:3-2:4 a)
  (var 3:3-3:4 b)
  (var 4:3-4:4 c)
  (var 5:3-5:4 d)
  (var 6:3-6:6 foo)
  (var 7:3-7:6 _42)
  (var 8:3-8:5 𐐷)
  (var 9:3-9:6 u𐐷)
  (var 10:3-10:6 𐐷w)
  (this 11:3-11:7)))
```
