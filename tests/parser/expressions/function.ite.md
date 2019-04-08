# Parser Test: `function`

## AST
```
(fun
 (name 1:5-1:9 main)
 (block
  (fun 2:3-2:11 block)
  (fun 3:3-3:12 (param (var 3:7-3:8 a)) block)
  (fun 4:3-4:15 (param (var 4:7-4:8 a)) (param (var 4:10-4:11 b)) block)
  (fun
   5:3-5:18
   (param (var 5:7-5:8 a))
   (param (var 5:10-5:11 b))
   (param (var 5:13-5:14 c))
   block)
  (fun 6:3-6:13 (param (var 6:7-6:8 a)) block)
  (fun 7:3-7:16 (param (var 7:7-7:8 a)) (param (var 7:10-7:11 b)) block)
  (fun
   8:3-8:19
   (param (var 8:7-8:8 a))
   (param (var 8:10-8:11 b))
   (param (var 8:13-8:14 c))
   block)
  (fun 10:3-14:4 (block (var 11:5-11:6 a) (var 12:5-12:6 b) (var 13:5-13:6 c)))
  (fun
   16:3-20:4
   (param (var 16:7-16:8 a))
   (param (var 16:10-16:11 b))
   (param (var 16:13-16:14 c))
   (block (var 17:5-17:6 a) (var 18:5-18:6 b) (var 19:5-19:6 c)))))
```
