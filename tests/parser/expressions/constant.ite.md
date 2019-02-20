# Parser Test: `constant`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (bool 2:3-2:7 true)
  (bool 3:3-3:8 false)
  (int 4:3-4:5 42)
  (bin 5:3-5:13 1010101)
  (hex 6:3-6:11 C0FF33)
  (float 7:3-7:6 42)
  (float 8:3-8:6 0.42)
  (float 9:3-9:11 3.141519)
  (float 10:3-10:14 1000000000)
  (float 11:3-11:14 9999999999)
  (float 12:3-12:15 1e10)
  (float 13:3-13:10 1425)
  (float 14:3-14:10 3.22e45)))
```
