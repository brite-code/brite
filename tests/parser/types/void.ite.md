# Parser Test: `void`

## AST
```
(fun
 (name 1:5-1:6 f)
 (param (var 1:7-1:8 f) (type (fun (void 1:17-1:21))))
 (type (void 1:24-1:28))
 (block (fun 2:3-2:17 (type (void 2:10-2:14)) block)))
```
