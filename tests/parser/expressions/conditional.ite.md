# Parser Test: `conditional`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (if (int 2:6-2:8 42) block)
  (if (bool 3:6-3:10 true) block)
  (if (bool 4:6-4:11 false) block)
  (if (var 5:6-5:7 x) block)
  (if (var 6:6-6:10 _foo) block)
  (if
   (var 8:6-8:7 a)
   (block
    (call
     9:5-9:19
     (prop (var 9:5-9:12 console) (name 9:13-9:16 log))
     (int 9:17-9:18 1))))
  (if
   (var 12:6-12:7 a)
   (block
    (call
     13:5-13:19
     (prop (var 13:5-13:12 console) (name 13:13-13:16 log))
     (int 13:17-13:18 1)))
   (block
    (call
     15:5-15:19
     (prop (var 15:5-15:12 console) (name 15:13-15:16 log))
     (int 15:17-15:18 2))))
  (if
   (var 18:6-18:7 a)
   (block
    (call
     19:5-19:19
     (prop (var 19:5-19:12 console) (name 19:13-19:16 log))
     (int 19:17-19:18 1)))
   (if
    (var 20:13-20:14 b)
    (block
     (call
      21:5-21:19
      (prop (var 21:5-21:12 console) (name 21:13-21:16 log))
      (int 21:17-21:18 2)))))
  (if
   (var 24:6-24:7 a)
   (block
    (call
     25:5-25:19
     (prop (var 25:5-25:12 console) (name 25:13-25:16 log))
     (int 25:17-25:18 1)))
   (if
    (var 26:13-26:14 b)
    (block
     (call
      27:5-27:19
      (prop (var 27:5-27:12 console) (name 27:13-27:16 log))
      (int 27:17-27:18 2)))
    (block
     (call
      29:5-29:19
      (prop (var 29:5-29:12 console) (name 29:13-29:16 log))
      (int 29:17-29:18 3)))))
  (if
   (var 32:6-32:7 a)
   (block
    (call
     33:5-33:19
     (prop (var 33:5-33:12 console) (name 33:13-33:16 log))
     (int 33:17-33:18 1)))
   (if
    (var 34:13-34:14 b)
    (block
     (call
      35:5-35:19
      (prop (var 35:5-35:12 console) (name 35:13-35:16 log))
      (int 35:17-35:18 2)))
    (if
     (var 36:13-36:14 c)
     (block
      (call
       37:5-37:19
       (prop (var 37:5-37:12 console) (name 37:13-37:16 log))
       (int 37:17-37:18 3))))))
  (if
   (var 40:6-40:7 a)
   (block
    (call
     41:5-41:19
     (prop (var 41:5-41:12 console) (name 41:13-41:16 log))
     (int 41:17-41:18 1)))
   (if
    (var 42:13-42:14 b)
    (block
     (call
      43:5-43:19
      (prop (var 43:5-43:12 console) (name 43:13-43:16 log))
      (int 43:17-43:18 2)))
    (if
     (var 44:13-44:14 c)
     (block
      (call
       45:5-45:19
       (prop (var 45:5-45:12 console) (name 45:13-45:16 log))
       (int 45:17-45:18 3)))
     (block
      (call
       47:5-47:19
       (prop (var 47:5-47:12 console) (name 47:13-47:16 log))
       (int 47:17-47:18 4))))))))
```
