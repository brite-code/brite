# Parser Test: `function`

## AST
```
(fun (name 1:5-1:9 main) block)
(fun
 (name 3:5-3:6 f)
 (param (var 3:7-3:8 a))
 (param (var 3:10-3:11 b))
 (param (var 3:13-3:14 c))
 block)
```
