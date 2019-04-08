# Parser Test: `function`

## AST
```
(fun (name 1:5-1:9 main) block)
(fun (name 3:5-3:6 f) (param (var 3:7-3:8 a)) block)
(fun (name 4:5-4:6 f) (param (var 4:7-4:8 a)) (param (var 4:10-4:11 b)) block)
(fun
 (name 5:5-5:6 f)
 (param (var 5:7-5:8 a))
 (param (var 5:10-5:11 b))
 (param (var 5:13-5:14 c))
 block)
(fun (name 7:5-7:6 f) (param (var 7:7-7:8 a)) block)
(fun (name 8:5-8:6 f) (param (var 8:7-8:8 a)) (param (var 8:10-8:11 b)) block)
(fun
 (name 9:5-9:6 f)
 (param (var 9:7-9:8 a))
 (param (var 9:10-9:11 b))
 (param (var 9:13-9:14 c))
 block)
(fun
 (name 11:5-11:6 f)
 (param (var 11:7-11:8 a) (type (var 11:10-11:11 T)))
 block)
(fun
 (name 12:5-12:6 f)
 (param (var 12:7-12:8 a) (type (var 12:10-12:11 T)))
 (param (var 12:13-12:14 b) (type (var 12:16-12:17 U)))
 block)
(fun
 (name 13:5-13:6 f)
 (param (var 13:7-13:8 a) (type (var 13:10-13:11 T)))
 (param (var 13:13-13:14 b) (type (var 13:16-13:17 U)))
 (param (var 13:19-13:20 c) (type (var 13:22-13:23 V)))
 block)
(fun
 (name 15:5-15:6 f)
 (param (var 15:7-15:8 a) (type (var 15:10-15:11 T)))
 block)
(fun
 (name 16:5-16:6 f)
 (param (var 16:7-16:8 a) (type (var 16:10-16:11 T)))
 (param (var 16:13-16:14 b) (type (var 16:16-16:17 T)))
 block)
(fun
 (name 17:5-17:6 f)
 (param (var 17:7-17:8 a) (type (var 17:10-17:11 T)))
 (param (var 17:13-17:14 b) (type (var 17:16-17:17 U)))
 (param (var 17:19-17:20 c) (type (var 17:22-17:23 V)))
 block)
(fun
 (name 19:5-19:6 f)
 (param (var 19:7-19:8 a) (type (var 19:10-19:11 T)))
 (param (var 19:13-19:14 b))
 (param (var 19:16-19:17 c) (type (var 19:19-19:20 V)))
 block)
(fun
 (name 20:5-20:6 f)
 (param (var 20:7-20:8 a))
 (param (var 20:10-20:11 b) (type (var 20:13-20:14 U)))
 (param (var 20:16-20:17 c))
 block)
(fun (name 22:5-22:6 f) (param (this 22:7-22:11)) block)
(fun
 (name 23:5-23:6 f)
 (param (this 23:7-23:11))
 (param (var 23:13-23:14 x) (type (var 23:16-23:17 T)))
 block)
(fun
 (name 24:5-24:6 f)
 (param (var 24:7-24:8 x) (type (var 24:10-24:11 T)))
 (param (this 24:13-24:17))
 block)
(fun
 (name 25:5-25:6 f)
 (param (this 25:7-25:11))
 (param (var 25:13-25:14 x) (type (var 25:16-25:17 T)))
 (param (var 25:19-25:20 y) (type (var 25:22-25:23 U)))
 block)
(fun
 (name 26:5-26:6 f)
 (param (var 26:7-26:8 x) (type (var 26:10-26:11 T)))
 (param (this 26:13-26:17))
 (param (var 26:19-26:20 y) (type (var 26:22-26:23 U)))
 block)
(fun
 (name 27:5-27:6 f)
 (param (this 27:7-27:11))
 (param (var 27:13-27:14 x))
 (param (var 27:16-27:17 y))
 block)
(fun
 (name 29:5-29:6 f)
 (param (this 29:7-29:11) (type (var 29:13-29:14 T)))
 block)
(fun (name 31:5-31:6 f) (type (var 31:12-31:13 T)) block)
(fun
 (name 32:5-32:6 f)
 (param (var 32:7-32:8 a))
 (type (var 32:13-32:14 T))
 block)
(fun
 (name 33:5-33:6 f)
 (param (var 33:7-33:8 a))
 (param (var 33:10-33:11 b))
 (type (var 33:16-33:17 T))
 block)
(fun
 (name 34:5-34:6 f)
 (param (var 34:7-34:8 a))
 (param (var 34:10-34:11 b))
 (param (var 34:13-34:14 c))
 (type (var 34:19-34:20 T))
 block)
```
