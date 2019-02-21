# Parser Test: `class_member_method`

## AST
```
(class
 (name 1:7-1:8 C)
 (fun (name 2:7-2:8 f) block)
 (fun (name 3:7-3:8 f) (param (var 3:9-3:10 a)) block)
 (fun (name 4:7-4:8 f) (param (var 4:9-4:10 a)) (param (var 4:12-4:13 b)) block)
 (fun
  (name 5:7-5:8 f)
  (param (var 5:9-5:10 a))
  (param (var 5:12-5:13 b))
  (param (var 5:15-5:16 c))
  block)
 (fun (name 6:7-6:8 f) (param (var 6:9-6:10 a)) block)
 (fun (name 7:7-7:8 f) (param (var 7:9-7:10 a)) (param (var 7:12-7:13 b)) block)
 (fun
  (name 8:7-8:8 f)
  (param (var 8:9-8:10 a))
  (param (var 8:12-8:13 b))
  (param (var 8:15-8:16 c))
  block)
 (fun (name 10:7-10:11 base) (param (var 10:12-10:13 x)) block)
 (fun (name 11:7-11:12 class) (param (var 11:13-11:14 x)) block)
 (fun
  (name 13:7-13:10 add)
  (param (this 13:11-13:15))
  (type (var 13:20-13:26 String))
  (block
   (add
    (prop (this 14:5-14:9) (name 14:10-14:11 a))
    (prop (this 14:14-14:18) (name 14:19-14:20 b))))))
```
