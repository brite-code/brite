# Parser Test: `member`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (prop (var 2:3-2:4 o) (name 2:5-2:6 p))
  (prop (prop (var 3:3-3:4 o) (name 3:5-3:6 p)) (name 3:7-3:8 q))
  (prop
   (wrap 4:3-4:8 (prop (var 4:4-4:5 o) (name 4:6-4:7 p)))
   (name 4:9-4:10 q))
  (prop
   (prop
    (prop
     (prop
      (prop (prop (var 5:3-5:4 a) (name 5:5-5:6 b)) (name 5:7-5:8 c))
      (name 5:9-5:10 d))
     (name 5:11-5:12 e))
    (name 5:13-5:14 f))
   (name 5:15-5:16 g))
  (prop
   (prop (prop (var 6:3-6:4 a) (name 7:4-7:5 b)) (name 8:4-8:5 c))
   (name 9:4-9:5 d))
  (prop
   (prop (prop (var 10:3-10:4 a) (name 11:3-11:4 b)) (name 12:3-12:4 c))
   (name 13:3-13:4 d))))
```
