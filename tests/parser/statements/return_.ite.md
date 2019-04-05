# Parser Test: `return_`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (return 2:3-2:12 (var 2:10-2:11 x))
  (return 3:3-3:12 (int 3:10-3:11 0))
  (return 4:3-4:13 (int 4:10-4:12 42))
  (return 5:3-5:10)
  (return 7:3-7:10)
  (var 7:11-7:12 x)
  (return 8:3-8:10)
  (var 9:3-9:4 x)
  (return 11:3-11:9)
  (var 12:3-12:4 x)
  (return 14:3-14:10)
  (int 14:11-14:13 42)
  (return 15:3-15:10)
  (int 16:3-16:5 42)
  (return 18:3-18:9)
  (int 19:3-19:5 42)
  (return 21:3-21:14 (wrap 21:10-21:13 (var 21:11-21:12 x)))
  (return 22:3-22:13 (wrap 22:9-22:12 (var 22:10-22:11 x)))
  (return 24:3-24:9)
  (wrap 25:3-25:6 (var 25:4-25:5 x))
  (return 27:3-29:5 (wrap 27:10-29:4 (var 28:5-28:6 x)))
  (return 31:3-32:7 (var 32:6-32:7 x))
  (return 33:3-34:9 (wrap 34:6-34:9 (var 34:7-34:8 x)))
  (return 36:3-36:9)
  (return 37:3-37:10)
  (return 38:3-38:11 (var 38:10-38:11 x))
  (return 39:3-39:12 (var 39:10-39:11 x))))
```
