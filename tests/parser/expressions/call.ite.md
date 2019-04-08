# Parser Test: `call`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (call 2:3-2:6 (var 2:3-2:4 f))
  (call 3:3-3:7 (var 3:3-3:4 f) (var 3:5-3:6 a))
  (call 4:3-4:10 (var 4:3-4:4 f) (var 4:5-4:6 a) (var 4:8-4:9 b))
  (call
   5:3-5:13
   (var 5:3-5:4 f)
   (var 5:5-5:6 a)
   (var 5:8-5:9 b)
   (var 5:11-5:12 c))
  (call 6:3-6:8 (var 6:3-6:4 f) (var 6:5-6:6 a))
  (call 7:3-7:11 (var 7:3-7:4 f) (var 7:5-7:6 a) (var 7:8-7:9 b))
  (call
   8:3-8:14
   (var 8:3-8:4 f)
   (var 8:5-8:6 a)
   (var 8:8-8:9 b)
   (var 8:11-8:12 c))
  (call 9:3-9:9 (var 9:3-9:4 f) (call 9:5-9:8 (var 9:5-9:6 g)))
  (call
   10:3-10:14
   (var 10:3-10:4 f)
   (call 10:5-10:8 (var 10:5-10:6 g))
   (call 10:10-10:13 (var 10:10-10:11 h)))
  (call
   11:3-11:16
   (var 11:3-11:4 f)
   (var 11:5-11:6 a)
   (call 11:8-11:12 (var 11:8-11:9 g) (var 11:10-11:11 b))
   (var 11:14-11:15 c))
  (call
   12:3-12:18
   (var 12:3-12:4 f)
   (var 12:5-12:6 a)
   (call 12:8-12:13 (var 12:8-12:9 g) (var 12:10-12:11 b))
   (var 12:15-12:16 c))
  (call 13:3-13:8 (call 13:3-13:6 (var 13:3-13:4 f)))
  (call 14:3-14:9 (call 14:3-14:7 (var 14:3-14:4 f) (var 14:5-14:6 a)))
  (call 15:3-15:9 (call 15:3-15:6 (var 15:3-15:4 f)) (var 15:7-15:8 b))
  (call
   16:3-16:13
   (call 16:3-16:10 (var 16:3-16:4 f) (var 16:5-16:6 a) (var 16:8-16:9 b))
   (var 16:11-16:12 c))
  (call
   17:3-17:13
   (call 17:3-17:7 (var 17:3-17:4 f) (var 17:5-17:6 a))
   (var 17:8-17:9 b)
   (var 17:11-17:12 c))
  (call 18:3-18:10 (call 18:3-18:8 (call 18:3-18:6 (var 18:3-18:4 f))))
  (call 19:3-19:8 (prop (var 19:3-19:4 f) (name 19:5-19:6 x)))
  (call
   20:3-20:10
   (prop (prop (var 20:3-20:4 f) (name 20:5-20:6 x)) (name 20:7-20:8 y)))
  (prop (call 21:3-21:6 (var 21:3-21:4 f)) (name 21:7-21:8 x))
  (prop
   (prop (call 22:3-22:6 (var 22:3-22:4 f)) (name 22:7-22:8 x))
   (name 22:9-22:10 y))
  (prop
   (call 23:3-23:8 (prop (var 23:3-23:4 f) (name 23:5-23:6 x)))
   (name 23:9-23:10 y))
  (call
   24:3-24:12
   (prop
    (call 24:3-24:8 (prop (var 24:3-24:4 f) (name 24:5-24:6 x)))
    (name 24:9-24:10 y)))
  (prop
   (call
    25:3-25:12
    (prop
     (call 25:3-25:8 (prop (var 25:3-25:4 f) (name 25:5-25:6 x)))
     (name 25:9-25:10 y)))
   (name 25:13-25:14 z))
  (call
   26:3-26:16
   (prop
    (call
     26:3-26:12
     (prop
      (call 26:3-26:8 (prop (var 26:3-26:4 f) (name 26:5-26:6 x)))
      (name 26:9-26:10 y)))
    (name 26:13-26:14 z)))
  (var 28:3-28:4 f)
  (wrap 28:5-28:8 (var 28:6-28:7 a))
  (var 30:3-30:4 f)
  (wrap 31:3-31:6 (var 31:4-31:5 a))
  (var 33:3-33:4 f)
  (wrap 34:3-34:6 (var 34:4-34:5 a))
  (call 36:3-37:9 (var 36:3-36:4 f) (var 37:7-37:8 a))))
```
