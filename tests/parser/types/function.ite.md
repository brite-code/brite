# Parser Test: `function`

## AST
```
(fun
 (name 1:5-1:9 main)
 (param (var 2:3-2:4 x) (type (fun (var 2:13-2:14 T))))
 (param
  (var 3:3-3:4 x)
  (type (fun (param (var 3:10-3:11 A)) (var 3:14-3:15 T))))
 (param
  (var 4:3-4:4 x)
  (type
   (fun (param (var 4:10-4:11 A)) (param (var 4:13-4:14 B)) (var 4:17-4:18 T))))
 (param
  (var 5:3-5:4 x)
  (type
   (fun
    (param (var 5:10-5:11 A))
    (param (var 5:13-5:14 B))
    (param (var 5:16-5:17 C))
    (var 5:20-5:21 T))))
 (param
  (var 6:3-6:4 x)
  (type (fun (param (var 6:10-6:11 A)) (var 6:15-6:16 T))))
 (param
  (var 7:3-7:4 x)
  (type
   (fun (param (var 7:10-7:11 A)) (param (var 7:13-7:14 B)) (var 7:18-7:19 T))))
 (param
  (var 8:3-8:4 x)
  (type
   (fun
    (param (var 8:10-8:11 A))
    (param (var 8:13-8:14 B))
    (param (var 8:16-8:17 C))
    (var 8:21-8:22 T))))
 block)
```
