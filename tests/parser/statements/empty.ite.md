# Parser Test: `empty`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block (var 5:3-5:4 x) (var 6:3-6:4 x) (var 7:4-7:5 x)))
(fun (name 10:5-10:6 a) (block (var 11:6-11:7 x)))
(fun (name 14:5-14:6 b) (block (var 15:3-15:4 x)))
(fun (name 18:5-18:6 c) (block (var 19:3-19:4 x) (var 19:7-19:8 y)))
```
