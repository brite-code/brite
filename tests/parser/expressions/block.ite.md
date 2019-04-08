# Parser Test: `block`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (block
   (let 3:5-3:15 (var 3:9-3:10 a) (var 3:13-3:14 b))
   (let 4:5-4:15 (var 4:9-4:10 c) (var 4:13-4:14 d))
   (var 5:5-5:6 c))
  (let
   8:3-11:5
   (var 8:7-8:8 x)
   (block (let 9:5-9:15 (var 9:9-9:10 a) (var 9:13-9:14 b)) (var 10:5-10:6 a)))
  (block (block block))))
```
