# Parser Test: `void`

## AST
```
(fun
 (name 1:5-1:6 f)
 (param (var 1:7-1:8 f) (type (fun (void 1:19-1:23))))
 (type (void 1:28-1:32))
 (block (fun 2:3-2:19 (type (void 2:12-2:16)) block)))
```
