# Parser Test: `return_`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (return (var 2:10-2:11 x))
  (return (int 3:10-3:11 0))
  (return (int 4:10-4:12 42))
  return
  return
  (var 7:11-7:12 x)
  return
  (var 9:3-9:4 x)
  return
  (var 12:3-12:4 x)
  return
  (int 14:11-14:13 42)
  return
  (int 16:3-16:5 42)
  return
  (int 19:3-19:5 42)
  (return (wrap 21:10-21:13 (var 21:11-21:12 x)))
  (return (wrap 22:9-22:12 (var 22:10-22:11 x)))
  return
  (wrap 25:3-25:6 (var 25:4-25:5 x))
  (return (wrap 27:10-29:4 (var 28:5-28:6 x)))))
```
