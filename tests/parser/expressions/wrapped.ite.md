# Parser Test: `wrapped`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (wrap 2:3-2:6 (var 2:4-2:5 x))
  (wrap 3:3-3:6 (var 3:4-3:5 y))
  (wrap 4:3-4:8 (var 4:4-4:7 foo))
  (wrap 5:3-5:9 (var 5:4-5:5 x) (type (var 5:7-5:8 T)))
  (wrap 6:3-6:9 (var 6:4-6:5 y) (type (var 6:7-6:8 U)))))
```
