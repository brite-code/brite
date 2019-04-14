# Parser Test: `binding`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (let 2:3-2:13 (var 2:7-2:8 a) (var 2:11-2:12 b))
  (let 3:3-3:13 (var 3:7-3:8 x) (var 3:11-3:12 y))
  (let 4:3-4:17 (var 4:7-4:10 foo) (var 4:13-4:16 bar))
  (let 5:3-5:14 (hole 5:7-5:8) (int 5:11-5:13 42))
  (let
   6:3-9:5
   (var 6:7-6:8 f)
   (fun
    6:11-9:4
    (block (let 7:5-7:15 (var 7:9-7:10 a) (var 7:13-7:14 b)) (var 8:5-8:6 a))))
  (let
   11:3-11:16
   (var 11:7-11:8 x)
   (type (var 11:10-11:11 T))
   (var 11:14-11:15 y))
  (let
   12:3-12:24
   (var 12:7-12:8 x)
   (type (fun (param (var 12:14-12:15 T)) (var 12:18-12:19 T)))
   (var 12:22-12:23 y))
  (let 14:3-14:12 (var 14:7-14:8 a) (var 14:11-14:12 b))
  (let
   15:3-15:15
   (var 15:7-15:8 x)
   (type (var 15:10-15:11 T))
   (var 15:14-15:15 y))))
```
