# Parser Test: `prefix`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (neg 2:3-2:6 (int 2:4-2:6 42))
  (neg 3:3-3:5 (var 3:4-3:5 x))
  (neg 4:3-4:6 (neg 4:4-4:6 (var 4:5-4:6 x)))
  (neg 5:3-5:8 (pos 5:4-5:8 (neg 5:5-5:8 (pos 5:6-5:8 (var 5:7-5:8 x)))))
  (pos 6:3-6:6 (neg 6:4-6:6 (var 6:5-6:6 x)))
  (not 7:3-7:9 (bool 7:4-7:9 false))
  (not 8:3-8:9 (not 8:4-8:9 (bool 8:5-8:9 true)))
  (not
   9:3-9:9
   (neg 9:4-9:9 (not 9:5-9:9 (pos 9:6-9:9 (not 9:7-9:9 (var 9:8-9:9 x))))))
  (not 10:3-10:7 (prop (var 10:4-10:5 x) (name 10:6-10:7 p)))
  (prop (wrap 11:3-11:7 (not 11:4-11:6 (var 11:5-11:6 x))) (name 11:8-11:9 p))
  (neg 12:3-12:7 (prop (var 12:4-12:5 x) (name 12:6-12:7 p)))
  (prop (wrap 13:3-13:7 (neg 13:4-13:6 (var 13:5-13:6 x))) (name 13:8-13:9 p))))
```
