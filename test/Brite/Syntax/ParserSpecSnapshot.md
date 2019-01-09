# ParserSpecSnapshot

--------------------------------------------------------------------------------

### Source
```ite
let x = y;
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let
```

### AST
```
(err (bind (err 0:3-0:3) (err 0:3-0:3)))
```

### Errors
- (0:3-0:3) We wanted a variable name but the file ended.
- (0:3-0:3) We wanted `=` but the file ended.
- (0:3-0:3) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let x
```

### AST
```
(err (bind (var 0:4-0:5 x) (err 0:5-0:5)))
```

### Errors
- (0:5-0:5) We wanted `=` but the file ended.
- (0:5-0:5) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let =
```

### AST
```
(bind (err 0:4-0:5) (err 0:5-0:5))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `=`.
- (0:5-0:5) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let y
```

### AST
```
(err (bind (var 0:4-0:5 y) (err 0:5-0:5)))
```

### Errors
- (0:5-0:5) We wanted `=` but the file ended.
- (0:5-0:5) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let ;
```

### AST
```
(err (bind (err 0:4-0:5) (err 0:4-0:5)))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `;`.
- (0:4-0:5) We wanted `=` but we found `;`.
- (0:4-0:5) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let x =
```

### AST
```
(bind (var 0:4-0:5 x) (err 0:7-0:7))
```

### Errors
- (0:7-0:7) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let x y
```

### AST
```
(err (bind (var 0:4-0:5 x) (var 0:6-0:7 y)))
```

### Errors
- (0:6-0:7) We wanted `=` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let x ;
```

### AST
```
(err (bind (var 0:4-0:5 x) (err 0:6-0:7)))
```

### Errors
- (0:6-0:7) We wanted `=` but we found `;`.
- (0:6-0:7) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let = y
```

### AST
```
(bind (err 0:4-0:5) (var 0:6-0:7 y))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `=`.

--------------------------------------------------------------------------------

### Source
```ite
let = ;
```

### AST
```
(bind (err 0:4-0:5) (err 0:6-0:7))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `=`.
- (0:6-0:7) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
😈 let x = y;
```

### AST
```
(err 0:0-0:2)
(bind (var 0:7-0:8 x) (var 0:11-0:12 y))
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let 😈 x = y;
```

### AST
```
(bind (err (var 0:7-0:8 x)) (var 0:11-0:12 y))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x 😈 = y;
```

### AST
```
(bind (var 0:4-0:5 x) (type (err 0:6-0:8)) (var 0:11-0:12 y))
```

### Errors
- (0:6-0:8) We wanted `:` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = 😈 y;
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:11-0:12 y)))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y 😈;
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:8-0:9 y)))
```

### Errors
- (0:10-0:12) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; 😈
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(err 0:11-0:13)
```

### Errors
- (0:11-0:13) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y 😈
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:8-0:9 y)))
```

### Errors
- (0:10-0:12) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
) let x = y;
```

### AST
```
(err 0:0-0:1)
(bind (var 0:6-0:7 x) (var 0:10-0:11 y))
```

### Errors
- (0:0-0:1) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let ) x = y;
```

### AST
```
(bind (err (var 0:6-0:7 x)) (var 0:10-0:11 y))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x ) = y;
```

### AST
```
(bind (var 0:4-0:5 x) (type (err 0:6-0:7)) (var 0:10-0:11 y))
```

### Errors
- (0:6-0:7) We wanted `:` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = ) y;
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:10-0:11 y)))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y );
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:8-0:9 y)))
```

### Errors
- (0:10-0:11) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; )
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(err 0:11-0:12)
```

### Errors
- (0:11-0:12) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y )
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:8-0:9 y)))
```

### Errors
- (0:10-0:11) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let 😈 = y;
```

### AST
```
(bind (err 0:4-0:6) (var 0:9-0:10 y))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x 😈 y;
```

### AST
```
(err (bind (var 0:4-0:5 x) (type (err 0:6-0:8)) (var 0:9-0:10 y)))
```

### Errors
- (0:6-0:8) We wanted `:` but we found `😈`.
- (0:9-0:10) We wanted `=` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let x = 😈;
```

### AST
```
(bind (var 0:4-0:5 x) (err 0:8-0:10))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let 😈 y;
```

### AST
```
(err (bind (err (var 0:7-0:8 y)) (err 0:8-0:9)))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.
- (0:8-0:9) We wanted `=` but we found `;`.
- (0:8-0:9) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let 😈 =;
```

### AST
```
(bind (err 0:4-0:6) (err 0:8-0:9))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.
- (0:8-0:9) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let x 😈;
```

### AST
```
(err (bind (var 0:4-0:5 x) (type (err 0:6-0:8)) (err 0:8-0:9)))
```

### Errors
- (0:6-0:8) We wanted `:` but we found `😈`.
- (0:8-0:9) We wanted `=` but we found `;`.
- (0:8-0:9) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let = 😈;
```

### AST
```
(bind (err 0:4-0:5) (err 0:6-0:8))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `=`.
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let 😈;
```

### AST
```
(err (bind (err 0:4-0:6) (err 0:6-0:7)))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.
- (0:6-0:7) We wanted `=` but we found `;`.
- (0:6-0:7) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let ) = y;
```

### AST
```
(bind (err 0:4-0:5) (var 0:8-0:9 y))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x ) y;
```

### AST
```
(err (bind (var 0:4-0:5 x) (type (err 0:6-0:7)) (var 0:8-0:9 y)))
```

### Errors
- (0:6-0:7) We wanted `:` but we found `)`.
- (0:8-0:9) We wanted `=` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let x = );
```

### AST
```
(bind (var 0:4-0:5 x) (err 0:8-0:9))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let ) y;
```

### AST
```
(err (bind (err (var 0:6-0:7 y)) (err 0:7-0:8)))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.
- (0:7-0:8) We wanted `=` but we found `;`.
- (0:7-0:8) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let ) =;
```

### AST
```
(bind (err 0:4-0:5) (err 0:7-0:8))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.
- (0:7-0:8) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let x );
```

### AST
```
(err (bind (var 0:4-0:5 x) (type (err 0:6-0:7)) (err 0:7-0:8)))
```

### Errors
- (0:6-0:7) We wanted `:` but we found `)`.
- (0:7-0:8) We wanted `=` but we found `;`.
- (0:7-0:8) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let = );
```

### AST
```
(bind (err 0:4-0:5) (err 0:6-0:7))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `=`.
- (0:6-0:7) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let );
```

### AST
```
(err (bind (err 0:4-0:5) (err 0:5-0:6)))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.
- (0:5-0:6) We wanted `=` but we found `;`.
- (0:5-0:6) We wanted an expression but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
x
```

### AST
```
(var 0:0-0:1 x)
```

--------------------------------------------------------------------------------

### Source
```ite
x;
```

### AST
```
(var 0:0-0:1 x)
```

--------------------------------------------------------------------------------

### Source
```ite
😈 x
```

### AST
```
(err 0:0-0:2)
(var 0:3-0:4 x)
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
x 😈
```

### AST
```
(err (var 0:0-0:1 x))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
😈 x;
```

### AST
```
(err 0:0-0:2)
(var 0:3-0:4 x)
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
x 😈;
```

### AST
```
(err (var 0:0-0:1 x))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
x; 😈
```

### AST
```
(var 0:0-0:1 x)
(err 0:3-0:5)
```

### Errors
- (0:3-0:5) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
=
```

### AST
```
(err 0:0-0:1)
```

### Errors
- (0:0-0:1) We wanted a statement but we found `=`.

--------------------------------------------------------------------------------

### Source
```ite
😈
```

### AST
```
(err 0:0-0:2)
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
)
```

### AST
```
(err 0:0-0:1)
```

### Errors
- (0:0-0:1) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
true
```

### AST
```
(bool 0:0-0:4 true)
```

--------------------------------------------------------------------------------

### Source
```ite
false
```

### AST
```
(bool 0:0-0:5 false)
```

--------------------------------------------------------------------------------

### Source
```ite
true true
```

### AST
```
(bool 0:0-0:4 true)
(bool 0:5-0:9 true)
```

--------------------------------------------------------------------------------

### Source
```ite
(
```

### AST
```
(err (wrap 0:0-0:1 (err 0:1-0:1)))
```

### Errors
- (0:1-0:1) We wanted an expression but the file ended.
- (0:1-0:1) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
(x
```

### AST
```
(err (wrap 0:0-0:2 (var 0:1-0:2 x)))
```

### Errors
- (0:2-0:2) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
()
```

### AST
```
(wrap 0:0-0:2 (err 0:1-0:2))
```

### Errors
- (0:1-0:2) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
(x)
```

### AST
```
(wrap 0:0-0:3 (var 0:1-0:2 x))
```

--------------------------------------------------------------------------------

### Source
```ite
x)
```

### AST
```
(err (var 0:0-0:1 x))
```

### Errors
- (0:1-0:2) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
(x;
```

### AST
```
(err (wrap 0:0-0:2 (var 0:1-0:2 x)))
```

### Errors
- (0:2-0:3) We wanted `)` but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let x = (y);
```

### AST
```
(bind (var 0:4-0:5 x) (wrap 0:8-0:11 (var 0:9-0:10 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = (y;
```

### AST
```
(bind (var 0:4-0:5 x) (err (wrap 0:8-0:10 (var 0:9-0:10 y))))
```

### Errors
- (0:10-0:11) We wanted `)` but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; let x = y;
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:15-0:16 x) (var 0:19-0:20 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y; let x = y; let x = y;
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:15-0:16 x) (var 0:19-0:20 y))
(bind (var 0:26-0:27 x) (var 0:30-0:31 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y; let x = y; let x = y; let x = y;
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:15-0:16 x) (var 0:19-0:20 y))
(bind (var 0:26-0:27 x) (var 0:30-0:31 y))
(bind (var 0:37-0:38 x) (var 0:41-0:42 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:14-0:15 x) (var 0:18-0:19 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:14-0:15 x) (var 0:18-0:19 y))
(bind (var 0:24-0:25 x) (var 0:28-0:29 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y let x = y let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:14-0:15 x) (var 0:18-0:19 y))
(bind (var 0:24-0:25 x) (var 0:28-0:29 y))
(bind (var 0:34-0:35 x) (var 0:38-0:39 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y
let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 1:4-1:5 x) (var 1:8-1:9 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y
let x = y
let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 1:4-1:5 x) (var 1:8-1:9 y))
(bind (var 2:4-2:5 x) (var 2:8-2:9 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y
let x = y
let x = y
let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 1:4-1:5 x) (var 1:8-1:9 y))
(bind (var 2:4-2:5 x) (var 2:8-2:9 y))
(bind (var 3:4-3:5 x) (var 3:8-3:9 y))
```

--------------------------------------------------------------------------------

### Source
```ite
😈 let x = y; let x = y; let x = y;
```

### AST
```
(err 0:0-0:2)
(bind (var 0:7-0:8 x) (var 0:11-0:12 y))
(bind (var 0:18-0:19 x) (var 0:22-0:23 y))
(bind (var 0:29-0:30 x) (var 0:33-0:34 y))
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; 😈 let x = y; let x = y;
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(err 0:11-0:13)
(bind (var 0:18-0:19 x) (var 0:22-0:23 y))
(bind (var 0:29-0:30 x) (var 0:33-0:34 y))
```

### Errors
- (0:11-0:13) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; let x = y; 😈 let x = y;
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:15-0:16 x) (var 0:19-0:20 y))
(err 0:22-0:24)
(bind (var 0:29-0:30 x) (var 0:33-0:34 y))
```

### Errors
- (0:22-0:24) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; let x = y; let x = y; 😈
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:15-0:16 x) (var 0:19-0:20 y))
(bind (var 0:26-0:27 x) (var 0:30-0:31 y))
(err 0:33-0:35)
```

### Errors
- (0:33-0:35) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
😈 let x = y let x = y let x = y
```

### AST
```
(err 0:0-0:2)
(bind (var 0:7-0:8 x) (var 0:11-0:12 y))
(bind (var 0:17-0:18 x) (var 0:21-0:22 y))
(bind (var 0:27-0:28 x) (var 0:31-0:32 y))
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y 😈 let x = y let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:8-0:9 y)))
(bind (var 0:17-0:18 x) (var 0:21-0:22 y))
(bind (var 0:27-0:28 x) (var 0:31-0:32 y))
```

### Errors
- (0:10-0:12) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y 😈 let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:14-0:15 x) (err (var 0:18-0:19 y)))
(bind (var 0:27-0:28 x) (var 0:31-0:32 y))
```

### Errors
- (0:20-0:22) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y let x = y 😈
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:14-0:15 x) (var 0:18-0:19 y))
(bind (var 0:24-0:25 x) (err (var 0:28-0:29 y)))
```

### Errors
- (0:30-0:32) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
) let x = y; let x = y; let x = y;
```

### AST
```
(err 0:0-0:1)
(bind (var 0:6-0:7 x) (var 0:10-0:11 y))
(bind (var 0:17-0:18 x) (var 0:21-0:22 y))
(bind (var 0:28-0:29 x) (var 0:32-0:33 y))
```

### Errors
- (0:0-0:1) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; ) let x = y; let x = y;
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(err 0:11-0:12)
(bind (var 0:17-0:18 x) (var 0:21-0:22 y))
(bind (var 0:28-0:29 x) (var 0:32-0:33 y))
```

### Errors
- (0:11-0:12) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; let x = y; ) let x = y;
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:15-0:16 x) (var 0:19-0:20 y))
(err 0:22-0:23)
(bind (var 0:28-0:29 x) (var 0:32-0:33 y))
```

### Errors
- (0:22-0:23) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; let x = y; let x = y; )
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:15-0:16 x) (var 0:19-0:20 y))
(bind (var 0:26-0:27 x) (var 0:30-0:31 y))
(err 0:33-0:34)
```

### Errors
- (0:33-0:34) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
) let x = y let x = y let x = y
```

### AST
```
(err 0:0-0:1)
(bind (var 0:6-0:7 x) (var 0:10-0:11 y))
(bind (var 0:16-0:17 x) (var 0:20-0:21 y))
(bind (var 0:26-0:27 x) (var 0:30-0:31 y))
```

### Errors
- (0:0-0:1) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y ) let x = y let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:8-0:9 y)))
(bind (var 0:16-0:17 x) (var 0:20-0:21 y))
(bind (var 0:26-0:27 x) (var 0:30-0:31 y))
```

### Errors
- (0:10-0:11) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y ) let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:14-0:15 x) (err (var 0:18-0:19 y)))
(bind (var 0:26-0:27 x) (var 0:30-0:31 y))
```

### Errors
- (0:20-0:21) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y let x = y )
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(bind (var 0:14-0:15 x) (var 0:18-0:19 y))
(bind (var 0:24-0:25 x) (err (var 0:28-0:29 y)))
```

### Errors
- (0:30-0:31) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do {}
```

### AST
```
(block 0:0-0:5)
```

--------------------------------------------------------------------------------

### Source
```ite
do { 
```

### AST
```
(err (block 0:0-0:4))
```

### Errors
- (0:5-0:5) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
do }
```

### AST
```
(err (block 0:0-0:4))
```

### Errors
- (0:3-0:4) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
do } do }
```

### AST
```
(err (block 0:0-0:4))
(err (block 0:5-0:9))
```

### Errors
- (0:3-0:4) We wanted `{` but we found `}`.
- (0:8-0:9) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
do
```

### AST
```
(err (block 0:0-0:2))
```

### Errors
- (0:2-0:2) We wanted `{` but the file ended.
- (0:2-0:2) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
do do
```

### AST
```
(err (block 0:0-0:5 (err (block 0:3-0:5))))
```

### Errors
- (0:3-0:5) We wanted `{` but we found `do`.
- (0:5-0:5) We wanted `{` but the file ended.
- (0:5-0:5) We wanted `}` but the file ended.
- (0:5-0:5) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; }
```

### AST
```
(block 0:0-0:17 (bind (var 0:9-0:10 x) (var 0:13-0:14 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; 
```

### AST
```
(err (block 0:0-0:15 (bind (var 0:9-0:10 x) (var 0:13-0:14 y))))
```

### Errors
- (0:16-0:16) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
do let x = y; }
```

### AST
```
(err (block 0:0-0:15 (bind (var 0:7-0:8 x) (var 0:11-0:12 y))))
```

### Errors
- (0:3-0:6) We wanted `{` but we found `let`.

--------------------------------------------------------------------------------

### Source
```ite
do let x = y }
```

### AST
```
(err (block 0:0-0:14 (bind (var 0:7-0:8 x) (var 0:11-0:12 y))))
```

### Errors
- (0:3-0:6) We wanted `{` but we found `let`.

--------------------------------------------------------------------------------

### Source
```ite
do let x = y;
```

### AST
```
(err (block 0:0-0:13 (bind (var 0:7-0:8 x) (var 0:11-0:12 y))))
```

### Errors
- (0:3-0:6) We wanted `{` but we found `let`.
- (0:13-0:13) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
do let x = y
```

### AST
```
(err (block 0:0-0:12 (bind (var 0:7-0:8 x) (var 0:11-0:12 y))))
```

### Errors
- (0:3-0:6) We wanted `{` but we found `let`.
- (0:12-0:12) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let x = (do {);
```

### AST
```
(bind (var 0:4-0:5 x) (wrap 0:8-0:14 (err (block 0:9-0:13))))
```

### Errors
- (0:13-0:14) We wanted `}` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = (do { let y = z; );
```

### AST
```
(bind
  (var 0:4-0:5 x)
  (wrap 0:8-0:26
    (err (block 0:9-0:24 (bind (var 0:18-0:19 y) (var 0:22-0:23 z))))))
```

### Errors
- (0:25-0:26) We wanted `}` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = (do);
```

### AST
```
(bind (var 0:4-0:5 x) (wrap 0:8-0:12 (err (block 0:9-0:11))))
```

### Errors
- (0:11-0:12) We wanted `{` but we found `)`.
- (0:11-0:12) We wanted `}` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = (do let y = z; );
```

### AST
```
(bind
  (var 0:4-0:5 x)
  (wrap 0:8-0:24
    (err (block 0:9-0:22 (bind (var 0:16-0:17 y) (var 0:20-0:21 z))))))
```

### Errors
- (0:12-0:15) We wanted `{` but we found `let`.
- (0:23-0:24) We wanted `}` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = (do { let y = z );
```

### AST
```
(bind
  (var 0:4-0:5 x)
  (wrap 0:8-0:25
    (err (block 0:9-0:23 (bind (var 0:18-0:19 y) (var 0:22-0:23 z))))))
```

### Errors
- (0:24-0:25) We wanted `}` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = (do { let y = );
```

### AST
```
(bind
  (var 0:4-0:5 x)
  (wrap 0:8-0:23
    (err (block 0:9-0:23 (bind (var 0:18-0:19 y) (err 0:22-0:23))))))
```

### Errors
- (0:22-0:23) We wanted an expression but we found `)`.
- (0:22-0:23) We wanted `}` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; }
```

### AST
```
(block 0:0-0:17 (bind (var 0:9-0:10 x) (var 0:13-0:14 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y }
```

### AST
```
(block 0:0-0:16 (bind (var 0:9-0:10 x) (var 0:13-0:14 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; }
```

### AST
```
(block 0:0-0:28
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:20-0:21 x) (var 0:24-0:25 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; let x = y; }
```

### AST
```
(block 0:0-0:39
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:20-0:21 x) (var 0:24-0:25 y))
  (bind (var 0:31-0:32 x) (var 0:35-0:36 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; let x = y; let x = y; }
```

### AST
```
(block 0:0-0:50
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:20-0:21 x) (var 0:24-0:25 y))
  (bind (var 0:31-0:32 x) (var 0:35-0:36 y))
  (bind (var 0:42-0:43 x) (var 0:46-0:47 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y }
```

### AST
```
(block 0:0-0:26
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:19-0:20 x) (var 0:23-0:24 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y let x = y }
```

### AST
```
(block 0:0-0:36
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:19-0:20 x) (var 0:23-0:24 y))
  (bind (var 0:29-0:30 x) (var 0:33-0:34 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y let x = y let x = y }
```

### AST
```
(block 0:0-0:46
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:19-0:20 x) (var 0:23-0:24 y))
  (bind (var 0:29-0:30 x) (var 0:33-0:34 y))
  (bind (var 0:39-0:40 x) (var 0:43-0:44 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y
let x = y
 }
```

### AST
```
(block 0:0-2:2
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 1:4-1:5 x) (var 1:8-1:9 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y
let x = y
let x = y
 }
```

### AST
```
(block 0:0-3:2
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 1:4-1:5 x) (var 1:8-1:9 y))
  (bind (var 2:4-2:5 x) (var 2:8-2:9 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y
let x = y
let x = y
let x = y
 }
```

### AST
```
(block 0:0-4:2
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 1:4-1:5 x) (var 1:8-1:9 y))
  (bind (var 2:4-2:5 x) (var 2:8-2:9 y))
  (bind (var 3:4-3:5 x) (var 3:8-3:9 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
do { 😈 let x = y; let x = y; let x = y; }
```

### AST
```
(block 0:0-0:42
  (err 0:5-0:7)
  (bind (var 0:12-0:13 x) (var 0:16-0:17 y))
  (bind (var 0:23-0:24 x) (var 0:27-0:28 y))
  (bind (var 0:34-0:35 x) (var 0:38-0:39 y)))
```

### Errors
- (0:5-0:7) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; 😈 let x = y; let x = y; }
```

### AST
```
(block 0:0-0:42
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (err 0:16-0:18)
  (bind (var 0:23-0:24 x) (var 0:27-0:28 y))
  (bind (var 0:34-0:35 x) (var 0:38-0:39 y)))
```

### Errors
- (0:16-0:18) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; 😈 let x = y; }
```

### AST
```
(block 0:0-0:42
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:20-0:21 x) (var 0:24-0:25 y))
  (err 0:27-0:29)
  (bind (var 0:34-0:35 x) (var 0:38-0:39 y)))
```

### Errors
- (0:27-0:29) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; let x = y; 😈 }
```

### AST
```
(block 0:0-0:42
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:20-0:21 x) (var 0:24-0:25 y))
  (bind (var 0:31-0:32 x) (var 0:35-0:36 y))
  (err 0:38-0:40))
```

### Errors
- (0:38-0:40) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
do { 😈 let x = y let x = y let x = y }
```

### AST
```
(block 0:0-0:39
  (err 0:5-0:7)
  (bind (var 0:12-0:13 x) (var 0:16-0:17 y))
  (bind (var 0:22-0:23 x) (var 0:26-0:27 y))
  (bind (var 0:32-0:33 x) (var 0:36-0:37 y)))
```

### Errors
- (0:5-0:7) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y 😈 let x = y let x = y }
```

### AST
```
(block 0:0-0:39
  (bind (var 0:9-0:10 x) (err (var 0:13-0:14 y)))
  (bind (var 0:22-0:23 x) (var 0:26-0:27 y))
  (bind (var 0:32-0:33 x) (var 0:36-0:37 y)))
```

### Errors
- (0:15-0:17) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y 😈 let x = y }
```

### AST
```
(block 0:0-0:39
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:19-0:20 x) (err (var 0:23-0:24 y)))
  (bind (var 0:32-0:33 x) (var 0:36-0:37 y)))
```

### Errors
- (0:25-0:27) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y let x = y 😈 }
```

### AST
```
(block 0:0-0:39
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:19-0:20 x) (var 0:23-0:24 y))
  (bind (var 0:29-0:30 x) (err (var 0:33-0:34 y))))
```

### Errors
- (0:35-0:37) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
do { ) let x = y; let x = y; let x = y; }
```

### AST
```
(block 0:0-0:41
  (err 0:5-0:6)
  (bind (var 0:11-0:12 x) (var 0:15-0:16 y))
  (bind (var 0:22-0:23 x) (var 0:26-0:27 y))
  (bind (var 0:33-0:34 x) (var 0:37-0:38 y)))
```

### Errors
- (0:5-0:6) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; ) let x = y; let x = y; }
```

### AST
```
(block 0:0-0:41
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (err 0:16-0:17)
  (bind (var 0:22-0:23 x) (var 0:26-0:27 y))
  (bind (var 0:33-0:34 x) (var 0:37-0:38 y)))
```

### Errors
- (0:16-0:17) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; ) let x = y; }
```

### AST
```
(block 0:0-0:41
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:20-0:21 x) (var 0:24-0:25 y))
  (err 0:27-0:28)
  (bind (var 0:33-0:34 x) (var 0:37-0:38 y)))
```

### Errors
- (0:27-0:28) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; let x = y; ) }
```

### AST
```
(block 0:0-0:41
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:20-0:21 x) (var 0:24-0:25 y))
  (bind (var 0:31-0:32 x) (var 0:35-0:36 y))
  (err 0:38-0:39))
```

### Errors
- (0:38-0:39) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do { ) let x = y let x = y let x = y }
```

### AST
```
(block 0:0-0:38
  (err 0:5-0:6)
  (bind (var 0:11-0:12 x) (var 0:15-0:16 y))
  (bind (var 0:21-0:22 x) (var 0:25-0:26 y))
  (bind (var 0:31-0:32 x) (var 0:35-0:36 y)))
```

### Errors
- (0:5-0:6) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y ) let x = y let x = y }
```

### AST
```
(block 0:0-0:38
  (bind (var 0:9-0:10 x) (err (var 0:13-0:14 y)))
  (bind (var 0:21-0:22 x) (var 0:25-0:26 y))
  (bind (var 0:31-0:32 x) (var 0:35-0:36 y)))
```

### Errors
- (0:15-0:16) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y ) let x = y }
```

### AST
```
(block 0:0-0:38
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:19-0:20 x) (err (var 0:23-0:24 y)))
  (bind (var 0:31-0:32 x) (var 0:35-0:36 y)))
```

### Errors
- (0:25-0:26) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y let x = y ) }
```

### AST
```
(block 0:0-0:38
  (bind (var 0:9-0:10 x) (var 0:13-0:14 y))
  (bind (var 0:19-0:20 x) (var 0:23-0:24 y))
  (bind (var 0:29-0:30 x) (err (var 0:33-0:34 y))))
```

### Errors
- (0:35-0:36) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = ) let x = )
```

### AST
```
(bind (var 0:4-0:5 x) (err 0:8-0:9))
(bind (var 0:14-0:15 x) (err 0:18-0:19))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `)`.
- (0:18-0:19) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = ) )
```

### AST
```
(bind (var 0:4-0:5 x) (err 0:8-0:11))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `)`.
- (0:10-0:11) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
) let x = )
```

### AST
```
(err 0:0-0:1)
(bind (var 0:6-0:7 x) (err 0:10-0:11))
```

### Errors
- (0:0-0:1) We wanted a statement but we found `)`.
- (0:10-0:11) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = ) ) let x = )
```

### AST
```
(bind (var 0:4-0:5 x) (err 0:8-0:11))
(bind (var 0:16-0:17 x) (err 0:20-0:21))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `)`.
- (0:10-0:11) We wanted an expression but we found `)`.
- (0:20-0:21) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do do 😈
```

### AST
```
(err (block 0:0-0:8 (err (block 0:3-0:8))))
```

### Errors
- (0:3-0:5) We wanted `{` but we found `do`.
- (0:6-0:8) We wanted `{` but we found `😈`.
- (0:8-0:8) We wanted `}` but the file ended.
- (0:8-0:8) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
do do )
```

### AST
```
(err (block 0:0-0:7 (err (block 0:3-0:7))))
```

### Errors
- (0:3-0:5) We wanted `{` but we found `do`.
- (0:6-0:7) We wanted `{` but we found `)`.
- (0:7-0:7) We wanted `}` but the file ended.
- (0:7-0:7) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x {}
```

### AST
```
(if 0:0-0:7 (var 0:3-0:4 x) block)
```

--------------------------------------------------------------------------------

### Source
```ite
if x { y }
```

### AST
```
(if 0:0-0:10 (var 0:3-0:4 x) (block (var 0:7-0:8 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else {}
```

### AST
```
(if 0:0-0:15 (var 0:3-0:4 x) block block)
```

--------------------------------------------------------------------------------

### Source
```ite
if x { y } else {}
```

### AST
```
(if 0:0-0:18 (var 0:3-0:4 x) (block (var 0:7-0:8 y)) block)
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else { y }
```

### AST
```
(if 0:0-0:18 (var 0:3-0:4 x) block (block (var 0:15-0:16 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
if x { y } else { z }
```

### AST
```
(if 0:0-0:21 (var 0:3-0:4 x) (block (var 0:7-0:8 y)) (block (var 0:18-0:19 z)))
```

--------------------------------------------------------------------------------

### Source
```ite
if { let x = y }
```

### AST
```
(if 0:0-0:16 (err 0:3-0:4) (block (bind (var 0:9-0:10 x) (var 0:13-0:14 y))))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.

--------------------------------------------------------------------------------

### Source
```ite
if {}
```

### AST
```
(if 0:0-0:5 (err 0:3-0:4) block)
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.

--------------------------------------------------------------------------------

### Source
```ite
if x }
```

### AST
```
(err (if 0:0-0:6 (var 0:3-0:4 x) block))
```

### Errors
- (0:5-0:6) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if x {
```

### AST
```
(err (if 0:0-0:6 (var 0:3-0:4 x) block))
```

### Errors
- (0:6-0:6) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x
```

### AST
```
(err (if 0:0-0:2 (var 0:3-0:4 x) block))
```

### Errors
- (0:4-0:4) We wanted `{` but the file ended.
- (0:4-0:4) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if {
```

### AST
```
(err (if 0:0-0:4 (err 0:3-0:4) block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:4-0:4) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if }
```

### AST
```
(err (if 0:0-0:4 (err 0:3-0:4) block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:3-0:4) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if {} else {}
```

### AST
```
(if 0:0-0:13 (err 0:3-0:4) block block)
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.

--------------------------------------------------------------------------------

### Source
```ite
if x } else {}
```

### AST
```
(err (if 0:0-0:14 (var 0:3-0:4 x) block block))
```

### Errors
- (0:5-0:6) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if x { else {}
```

### AST
```
(err (if 0:0-0:14 (var 0:3-0:4 x) block block))
```

### Errors
- (0:7-0:11) We wanted `}` but we found `else`.

--------------------------------------------------------------------------------

### Source
```ite
if x else {}
```

### AST
```
(err (if 0:0-0:12 (var 0:3-0:4 x) block block))
```

### Errors
- (0:5-0:9) We wanted `{` but we found `else`.
- (0:5-0:9) We wanted `}` but we found `else`.

--------------------------------------------------------------------------------

### Source
```ite
if { else {}
```

### AST
```
(err (if 0:0-0:12 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:5-0:9) We wanted `}` but we found `else`.

--------------------------------------------------------------------------------

### Source
```ite
if } else {}
```

### AST
```
(err (if 0:0-0:12 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:3-0:4) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if {} {}
```

### AST
```
(if 0:0-0:5 (err 0:3-0:4) block)
(object 0:6-0:8)
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.

--------------------------------------------------------------------------------

### Source
```ite
if x } {}
```

### AST
```
(err (if 0:0-0:6 (var 0:3-0:4 x) block))
(object 0:7-0:9)
```

### Errors
- (0:5-0:6) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if x { {}
```

### AST
```
(err (if 0:0-0:9 (var 0:3-0:4 x) (block (object 0:7-0:9))))
```

### Errors
- (0:9-0:9) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if { {}
```

### AST
```
(err (if 0:0-0:7 (err 0:3-0:4) (block (object 0:5-0:7))))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:7-0:7) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if } {}
```

### AST
```
(err (if 0:0-0:4 (err 0:3-0:4) block))
(object 0:5-0:7)
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:3-0:4) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if {} else }
```

### AST
```
(err (if 0:0-0:12 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:11-0:12) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if x } else }
```

### AST
```
(err (if 0:0-0:13 (var 0:3-0:4 x) block block))
```

### Errors
- (0:5-0:6) We wanted `{` but we found `}`.
- (0:12-0:13) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if x { else }
```

### AST
```
(err (if 0:0-0:13 (var 0:3-0:4 x) block block))
```

### Errors
- (0:7-0:11) We wanted `}` but we found `else`.
- (0:12-0:13) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if x else }
```

### AST
```
(err (if 0:0-0:11 (var 0:3-0:4 x) block block))
```

### Errors
- (0:5-0:9) We wanted `{` but we found `else`.
- (0:5-0:9) We wanted `}` but we found `else`.
- (0:10-0:11) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if { else }
```

### AST
```
(err (if 0:0-0:11 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:5-0:9) We wanted `}` but we found `else`.
- (0:10-0:11) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if } else }
```

### AST
```
(err (if 0:0-0:11 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:3-0:4) We wanted `{` but we found `}`.
- (0:10-0:11) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if {} else {
```

### AST
```
(err (if 0:0-0:12 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:12-0:12) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x } else {
```

### AST
```
(err (if 0:0-0:13 (var 0:3-0:4 x) block block))
```

### Errors
- (0:5-0:6) We wanted `{` but we found `}`.
- (0:13-0:13) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x { else {
```

### AST
```
(err (if 0:0-0:13 (var 0:3-0:4 x) block block))
```

### Errors
- (0:7-0:11) We wanted `}` but we found `else`.
- (0:13-0:13) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x else {
```

### AST
```
(err (if 0:0-0:11 (var 0:3-0:4 x) block block))
```

### Errors
- (0:5-0:9) We wanted `{` but we found `else`.
- (0:5-0:9) We wanted `}` but we found `else`.
- (0:11-0:11) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if { else {
```

### AST
```
(err (if 0:0-0:11 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:5-0:9) We wanted `}` but we found `else`.
- (0:11-0:11) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if } else {
```

### AST
```
(err (if 0:0-0:11 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:3-0:4) We wanted `{` but we found `}`.
- (0:11-0:11) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if {} else
```

### AST
```
(err (if 0:0-0:10 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:10-0:10) We wanted `{` but the file ended.
- (0:10-0:10) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x } else
```

### AST
```
(err (if 0:0-0:11 (var 0:3-0:4 x) block block))
```

### Errors
- (0:5-0:6) We wanted `{` but we found `}`.
- (0:11-0:11) We wanted `{` but the file ended.
- (0:11-0:11) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x { else
```

### AST
```
(err (if 0:0-0:11 (var 0:3-0:4 x) block block))
```

### Errors
- (0:7-0:11) We wanted `}` but we found `else`.
- (0:11-0:11) We wanted `{` but the file ended.
- (0:11-0:11) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x else
```

### AST
```
(err (if 0:0-0:9 (var 0:3-0:4 x) block block))
```

### Errors
- (0:5-0:9) We wanted `{` but we found `else`.
- (0:5-0:9) We wanted `}` but we found `else`.
- (0:9-0:9) We wanted `{` but the file ended.
- (0:9-0:9) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if { else
```

### AST
```
(err (if 0:0-0:9 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:5-0:9) We wanted `}` but we found `else`.
- (0:9-0:9) We wanted `{` but the file ended.
- (0:9-0:9) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if } else
```

### AST
```
(err (if 0:0-0:9 (err 0:3-0:4) block block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:3-0:4) We wanted `{` but we found `}`.
- (0:9-0:9) We wanted `{` but the file ended.
- (0:9-0:9) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if {} {
```

### AST
```
(if 0:0-0:5 (err 0:3-0:4) block)
(err (object 0:6-0:7))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:7-0:7) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x } {
```

### AST
```
(err (if 0:0-0:6 (var 0:3-0:4 x) block))
(err (object 0:7-0:8))
```

### Errors
- (0:5-0:6) We wanted `{` but we found `}`.
- (0:8-0:8) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x { {
```

### AST
```
(err (if 0:0-0:8 (var 0:3-0:4 x) (block (err (object 0:7-0:8)))))
```

### Errors
- (0:8-0:8) We wanted `}` but the file ended.
- (0:8-0:8) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x {
```

### AST
```
(err (if 0:0-0:6 (var 0:3-0:4 x) block))
```

### Errors
- (0:6-0:6) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if { {
```

### AST
```
(err (if 0:0-0:6 (err 0:3-0:4) (block (err (object 0:5-0:6)))))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:6-0:6) We wanted `}` but the file ended.
- (0:6-0:6) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if } {
```

### AST
```
(err (if 0:0-0:4 (err 0:3-0:4) block))
(err (object 0:5-0:6))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:3-0:4) We wanted `{` but we found `}`.
- (0:6-0:6) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if {} }
```

### AST
```
(err (if 0:0-0:5 (err 0:3-0:4) block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:6-0:7) We wanted `else` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if x } }
```

### AST
```
(err (if 0:0-0:6 (var 0:3-0:4 x) block))
```

### Errors
- (0:5-0:6) We wanted `{` but we found `}`.
- (0:7-0:8) We wanted `else` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if x { }
```

### AST
```
(if 0:0-0:8 (var 0:3-0:4 x) block)
```

--------------------------------------------------------------------------------

### Source
```ite
if x }
```

### AST
```
(err (if 0:0-0:6 (var 0:3-0:4 x) block))
```

### Errors
- (0:5-0:6) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
if { }
```

### AST
```
(if 0:0-0:6 (err 0:3-0:4) block)
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.

--------------------------------------------------------------------------------

### Source
```ite
if } }
```

### AST
```
(err (if 0:0-0:4 (err 0:3-0:4) block))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:3-0:4) We wanted `{` but we found `}`.
- (0:5-0:6) We wanted `else` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
x 😈 😈 ;
```

### AST
```
(err (var 0:0-0:1 x))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.
- (0:5-0:7) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
x ) ) ;
```

### AST
```
(err (var 0:0-0:1 x))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `)`.
- (0:4-0:5) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let 😈 😈 x = y;
```

### AST
```
(bind (err (var 0:10-0:11 x)) (var 0:14-0:15 y))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.
- (0:7-0:9) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let ) ) x = y;
```

### AST
```
(bind (err (var 0:8-0:9 x)) (var 0:12-0:13 y))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.
- (0:6-0:7) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
😈 😈
```

### AST
```
(err 0:0-0:5)
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.
- (0:3-0:5) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
) )
```

### AST
```
(err 0:0-0:3)
```

### Errors
- (0:0-0:1) We wanted a statement but we found `)`.
- (0:2-0:3) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
if x {} { let y = z }
```

### AST
```
(if 0:0-0:7 (var 0:3-0:4 x) block)
(err (object 0:8-0:9))
(bind (var 0:14-0:15 y) (err (var 0:18-0:19 z)))
```

### Errors
- (0:10-0:13) We wanted `}` but we found `let`.
- (0:20-0:21) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
o.p
```

### AST
```
(prop 0:0-0:3 (var 0:0-0:1 o) (name 0:2-0:3 p))
```

--------------------------------------------------------------------------------

### Source
```ite
o.p.q
```

### AST
```
(prop 0:0-0:5 (prop 0:0-0:3 (var 0:0-0:1 o) (name 0:2-0:3 p)) (name 0:4-0:5 q))
```

--------------------------------------------------------------------------------

### Source
```ite
o.
```

### AST
```
(err (var 0:0-0:1 o))
```

### Errors
- (0:2-0:2) We wanted a variable name but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
o.p.
```

### AST
```
(err (prop 0:0-0:3 (var 0:0-0:1 o) (name 0:2-0:3 p)))
```

### Errors
- (0:4-0:4) We wanted a variable name but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
o..p
```

### AST
```
(prop 0:0-0:4 (err (var 0:0-0:1 o)) (name 0:3-0:4 p))
```

### Errors
- (0:2-0:3) We wanted a variable name but we found `.`.

--------------------------------------------------------------------------------

### Source
```ite
o p
```

### AST
```
(var 0:0-0:1 o)
(var 0:2-0:3 p)
```

--------------------------------------------------------------------------------

### Source
```ite
o😈.p
```

### AST
```
(err (prop 0:0-0:5 (var 0:0-0:1 o) (name 0:4-0:5 p)))
```

### Errors
- (0:1-0:3) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
o.😈p
```

### AST
```
(err (prop 0:0-0:5 (var 0:0-0:1 o) (name 0:4-0:5 p)))
```

### Errors
- (0:2-0:4) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
o.😈p.q
```

### AST
```
(prop 0:0-0:7
  (err (prop 0:0-0:5 (var 0:0-0:1 o) (name 0:4-0:5 p)))
  (name 0:6-0:7 q))
```

### Errors
- (0:2-0:4) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
o.p😈.q
```

### AST
```
(err
  (prop 0:0-0:7
    (prop 0:0-0:3 (var 0:0-0:1 o) (name 0:2-0:3 p))
    (name 0:6-0:7 q)))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
o.p.😈q
```

### AST
```
(err
  (prop 0:0-0:7
    (prop 0:0-0:3 (var 0:0-0:1 o) (name 0:2-0:3 p))
    (name 0:6-0:7 q)))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
o).p
```

### AST
```
(err (prop 0:0-0:4 (var 0:0-0:1 o) (name 0:3-0:4 p)))
```

### Errors
- (0:1-0:2) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
o.)p
```

### AST
```
(err (prop 0:0-0:4 (var 0:0-0:1 o) (name 0:3-0:4 p)))
```

### Errors
- (0:2-0:3) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
o.)p.q
```

### AST
```
(prop 0:0-0:6
  (err (prop 0:0-0:4 (var 0:0-0:1 o) (name 0:3-0:4 p)))
  (name 0:5-0:6 q))
```

### Errors
- (0:2-0:3) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
o.p).q
```

### AST
```
(err
  (prop 0:0-0:6
    (prop 0:0-0:3 (var 0:0-0:1 o) (name 0:2-0:3 p))
    (name 0:5-0:6 q)))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
o.p.)q
```

### AST
```
(err
  (prop 0:0-0:6
    (prop 0:0-0:3 (var 0:0-0:1 o) (name 0:2-0:3 p))
    (name 0:5-0:6 q)))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
if x {} 😈
```

### AST
```
(err (if 0:0-0:7 (var 0:3-0:4 x) block))
```

### Errors
- (0:8-0:10) We wanted `else` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
if x {} 😈 else {}
```

### AST
```
(err (if 0:0-0:18 (var 0:3-0:4 x) block block))
```

### Errors
- (0:8-0:10) We wanted `else` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
if x {} )
```

### AST
```
(err (if 0:0-0:7 (var 0:3-0:4 x) block))
```

### Errors
- (0:8-0:9) We wanted `else` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
if x {} ) else {}
```

### AST
```
(err (if 0:0-0:17 (var 0:3-0:4 x) block block))
```

### Errors
- (0:8-0:9) We wanted `else` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
if 😈 x {}
```

### AST
```
(if 0:0-0:10 (err (var 0:6-0:7 x)) block)
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
if x 😈 {}
```

### AST
```
(if 0:0-0:10 (err (var 0:3-0:4 x)) block)
```

### Errors
- (0:5-0:7) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
if ) x {}
```

### AST
```
(if 0:0-0:9 (err (var 0:5-0:6 x)) block)
```

### Errors
- (0:3-0:4) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
if x ) {}
```

### AST
```
(if 0:0-0:9 (err (var 0:3-0:4 x)) block)
```

### Errors
- (0:5-0:6) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
do {}.p
```

### AST
```
(prop 0:0-0:7 (block 0:0-0:5) (name 0:6-0:7 p))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {}.p
```

### AST
```
(prop 0:0-0:9 (if 0:0-0:7 (var 0:3-0:4 x) block) (name 0:8-0:9 p))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else {}.p
```

### AST
```
(prop 0:0-0:17 (if 0:0-0:15 (var 0:3-0:4 x) block block) (name 0:16-0:17 p))
```

--------------------------------------------------------------------------------

### Source
```ite
f()
```

### AST
```
(call 0:0-0:3 (var 0:0-0:1 f))
```

--------------------------------------------------------------------------------

### Source
```ite
f ()
```

### AST
```
(call 0:0-0:4 (var 0:0-0:1 f))
```

--------------------------------------------------------------------------------

### Source
```ite
f😈()
```

### AST
```
(err (call 0:0-0:5 (var 0:0-0:1 f)))
```

### Errors
- (0:1-0:3) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f)()
```

### AST
```
(err (call 0:0-0:4 (var 0:0-0:1 f)))
```

### Errors
- (0:1-0:2) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f
()
```

### AST
```
(var 0:0-0:1 f)
(wrap 1:0-1:2 (err 1:1-1:2))
```

### Errors
- (1:1-1:2) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f;()
```

### AST
```
(var 0:0-0:1 f)
(wrap 0:2-0:4 (err 0:3-0:4))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f
😈()
```

### AST
```
(err (var 0:0-0:1 f))
(wrap 1:2-1:4 (err 1:3-1:4))
```

### Errors
- (1:0-1:2) We wanted an expression but we found `😈`.
- (1:3-1:4) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f😈
()
```

### AST
```
(err (var 0:0-0:1 f))
(wrap 1:0-1:2 (err 1:1-1:2))
```

### Errors
- (0:1-0:3) We wanted an expression but we found `😈`.
- (1:1-1:2) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f
)()
```

### AST
```
(err (var 0:0-0:1 f))
(wrap 1:1-1:3 (err 1:2-1:3))
```

### Errors
- (1:0-1:1) We wanted an expression but we found `)`.
- (1:2-1:3) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f)
()
```

### AST
```
(err (var 0:0-0:1 f))
(wrap 1:0-1:2 (err 1:1-1:2))
```

### Errors
- (0:1-0:2) We wanted an expression but we found `)`.
- (1:1-1:2) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(😈)
```

### AST
```
(call 0:0-0:5 (var 0:0-0:1 f) (err 0:2-0:4))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(
```

### AST
```
(err (call 0:0-0:2 (var 0:0-0:1 f)))
```

### Errors
- (0:2-0:2) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
😈.p
```

### AST
```
(err 0:0-0:3)
(var 0:3-0:4 p)
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.
- (0:2-0:3) We wanted a statement but we found `.`.

--------------------------------------------------------------------------------

### Source
```ite
(😈.p)
```

### AST
```
(err (prop 0:0-0:5 (err (wrap 0:0-0:3 (err 0:1-0:3))) (name 0:4-0:5 p)))
```

### Errors
- (0:1-0:3) We wanted an expression but we found `😈`.
- (0:3-0:4) We wanted `)` but we found `.`.
- (0:5-0:6) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let 😈) = y;
```

### AST
```
(bind (err 0:4-0:7) (var 0:10-0:11 y))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.
- (0:6-0:7) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let )😈 = y;
```

### AST
```
(bind (err 0:4-0:7) (var 0:10-0:11 y))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.
- (0:5-0:7) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let 😈)x = y;
```

### AST
```
(bind (err (var 0:7-0:8 x)) (var 0:11-0:12 y))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.
- (0:6-0:7) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let )😈x = y;
```

### AST
```
(bind (err (var 0:7-0:8 x)) (var 0:11-0:12 y))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.
- (0:5-0:7) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y 😈);
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:8-0:9 y)))
```

### Errors
- (0:10-0:12) We wanted an expression but we found `😈`.
- (0:12-0:13) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y )😈;
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:8-0:9 y)))
```

### Errors
- (0:10-0:11) We wanted an expression but we found `)`.
- (0:11-0:13) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
)😈 let x = y;
```

### AST
```
(err 0:0-0:3)
(bind (var 0:8-0:9 x) (var 0:12-0:13 y))
```

### Errors
- (0:0-0:1) We wanted a statement but we found `)`.
- (0:1-0:3) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
😈) let x = y;
```

### AST
```
(err 0:0-0:3)
(bind (var 0:8-0:9 x) (var 0:12-0:13 y))
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.
- (0:2-0:3) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; )😈
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(err 0:11-0:14)
```

### Errors
- (0:11-0:12) We wanted a statement but we found `)`.
- (0:12-0:14) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; 😈)
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(err 0:11-0:14)
```

### Errors
- (0:11-0:13) We wanted a statement but we found `😈`.
- (0:13-0:14) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
)😈
```

### AST
```
(err 0:0-0:3)
```

### Errors
- (0:0-0:1) We wanted a statement but we found `)`.
- (0:1-0:3) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
😈)
```

### AST
```
(err 0:0-0:3)
```

### Errors
- (0:0-0:2) We wanted a statement but we found `😈`.
- (0:2-0:3) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
let x = 🐶🐱 y;
```

### AST
```
(bind (var 0:4-0:5 x) (err (var 0:13-0:14 y)))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `🐶`.
- (0:10-0:12) We wanted an expression but we found `🐱`.

--------------------------------------------------------------------------------

### Source
```ite
if x {} 🐶🐱 else {}
```

### AST
```
(err (if 0:0-0:20 (var 0:3-0:4 x) block block))
```

### Errors
- (0:8-0:10) We wanted `else` but we found `🐶`.
- (0:10-0:12) We wanted `else` but we found `🐱`.

--------------------------------------------------------------------------------

### Source
```ite
if x {} 🐶🐱 else {
```

### AST
```
(err (if 0:0-0:19 (var 0:3-0:4 x) block block))
```

### Errors
- (0:8-0:10) We wanted `else` but we found `🐶`.
- (0:10-0:12) We wanted `else` but we found `🐱`.
- (0:19-0:19) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let x = y; do { let x = y; 😈 let x = y; } let x = y;
```

### AST
```
(bind (var 0:4-0:5 x) (var 0:8-0:9 y))
(block 0:11-0:42
  (bind (var 0:20-0:21 x) (var 0:24-0:25 y))
  (err 0:27-0:29)
  (bind (var 0:34-0:35 x) (var 0:38-0:39 y)))
(bind (var 0:47-0:48 x) (var 0:51-0:52 y))
```

### Errors
- (0:27-0:29) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let x = 😈 let x = y
```

### AST
```
(bind (var 0:4-0:5 x) (err 0:8-0:10))
(bind (var 0:15-0:16 x) (var 0:19-0:20 y))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(x)
```

### AST
```
(call 0:0-0:4 (var 0:0-0:1 f) (var 0:2-0:3 x))
```

--------------------------------------------------------------------------------

### Source
```ite
f
(x)
```

### AST
```
(var 0:0-0:1 f)
(wrap 1:0-1:3 (var 1:1-1:2 x))
```

--------------------------------------------------------------------------------

### Source
```ite
f;(x)
```

### AST
```
(var 0:0-0:1 f)
(wrap 0:2-0:5 (var 0:3-0:4 x))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a)
```

### AST
```
(call 0:0-0:4 (var 0:0-0:1 f) (var 0:2-0:3 a))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a😈)
```

### AST
```
(call 0:0-0:6 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a})
```

### AST
```
(call 0:0-0:5 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈})
```

### AST
```
(call 0:0-0:7 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:5-0:6) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a}😈)
```

### AST
```
(call 0:0-0:7 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:4-0:6) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈,)
```

### AST
```
(call 0:0-0:7 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a},)
```

### AST
```
(call 0:0-0:6 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈},)
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:5-0:6) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a}😈,)
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:4-0:6) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈, b)
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (var 0:7-0:8 b))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a}, b)
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (var 0:6-0:7 b))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈}, b)
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (var 0:8-0:9 b))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:5-0:6) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a}😈, b)
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (var 0:8-0:9 b))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:4-0:6) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈, b.)
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (err (var 0:7-0:8 b)))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:9-0:10) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a}, b.)
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (err (var 0:6-0:7 b)))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:8-0:9) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈}, b.)
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (err (var 0:8-0:9 b)))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:5-0:6) We wanted an expression but we found `}`.
- (0:10-0:11) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a}😈, b.)
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (err (var 0:8-0:9 b)))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:4-0:6) We wanted an expression but we found `😈`.
- (0:10-0:11) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈 b)
```

### AST
```
(err (call 0:0-0:8 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (var 0:6-0:7 b)))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:6-0:7) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a} b)
```

### AST
```
(err (call 0:0-0:7 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (var 0:5-0:6 b)))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:5-0:6) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈} b)
```

### AST
```
(err (call 0:0-0:9 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (var 0:7-0:8 b)))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:5-0:6) We wanted an expression but we found `}`.
- (0:7-0:8) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a}😈 b)
```

### AST
```
(err (call 0:0-0:9 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (var 0:7-0:8 b)))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:4-0:6) We wanted an expression but we found `😈`.
- (0:7-0:8) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈 b.)
```

### AST
```
(err (call 0:0-0:9 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (err (var 0:6-0:7 b))))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:6-0:7) We wanted `,` but we found a variable name.
- (0:8-0:9) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a} b.)
```

### AST
```
(err (call 0:0-0:8 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (err (var 0:5-0:6 b))))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:5-0:6) We wanted `,` but we found a variable name.
- (0:7-0:8) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈} b.)
```

### AST
```
(err
  (call 0:0-0:10 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (err (var 0:7-0:8 b))))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:5-0:6) We wanted an expression but we found `}`.
- (0:7-0:8) We wanted `,` but we found a variable name.
- (0:9-0:10) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a}😈 b.)
```

### AST
```
(err
  (call 0:0-0:10 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (err (var 0:7-0:8 b))))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:4-0:6) We wanted an expression but we found `😈`.
- (0:7-0:8) We wanted `,` but we found a variable name.
- (0:9-0:10) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b)
```

### AST
```
(call 0:0-0:7 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈)
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b})
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}😈)
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:7-0:9) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈})
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:8-0:9) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈,)
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b},)
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}😈,)
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:7-0:9) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈},)
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:8-0:9) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈, c)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (var 0:10-0:11 c))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}, c)
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (var 0:9-0:10 c))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}😈, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (var 0:11-0:12 c))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:7-0:9) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈}, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (var 0:11-0:12 c))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:8-0:9) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈, c.)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (err (var 0:10-0:11 c)))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:12-0:13) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}, c.)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (err (var 0:9-0:10 c)))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:11-0:12) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}😈, c.)
```

### AST
```
(call 0:0-0:14
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (err (var 0:11-0:12 c)))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:7-0:9) We wanted an expression but we found `😈`.
- (0:13-0:14) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈}, c.)
```

### AST
```
(call 0:0-0:14
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (err (var 0:11-0:12 c)))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:8-0:9) We wanted an expression but we found `}`.
- (0:13-0:14) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈 c)
```

### AST
```
(err
  (call 0:0-0:11
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (err (var 0:5-0:6 b))
    (var 0:9-0:10 c)))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:9-0:10) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b} c)
```

### AST
```
(err
  (call 0:0-0:10
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (err (var 0:5-0:6 b))
    (var 0:8-0:9 c)))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:8-0:9) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}😈 c)
```

### AST
```
(err
  (call 0:0-0:12
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (err (var 0:5-0:6 b))
    (var 0:10-0:11 c)))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:7-0:9) We wanted an expression but we found `😈`.
- (0:10-0:11) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈} c)
```

### AST
```
(err
  (call 0:0-0:12
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (err (var 0:5-0:6 b))
    (var 0:10-0:11 c)))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:8-0:9) We wanted an expression but we found `}`.
- (0:10-0:11) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈 c.)
```

### AST
```
(err
  (call 0:0-0:12
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (err (var 0:5-0:6 b))
    (err (var 0:9-0:10 c))))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:9-0:10) We wanted `,` but we found a variable name.
- (0:11-0:12) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b} c.)
```

### AST
```
(err
  (call 0:0-0:11
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (err (var 0:5-0:6 b))
    (err (var 0:8-0:9 c))))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:8-0:9) We wanted `,` but we found a variable name.
- (0:10-0:11) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}😈 c.)
```

### AST
```
(err
  (call 0:0-0:13
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (err (var 0:5-0:6 b))
    (err (var 0:10-0:11 c))))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:7-0:9) We wanted an expression but we found `😈`.
- (0:10-0:11) We wanted `,` but we found a variable name.
- (0:12-0:13) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈} c.)
```

### AST
```
(err
  (call 0:0-0:13
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (err (var 0:5-0:6 b))
    (err (var 0:10-0:11 c))))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:8-0:9) We wanted an expression but we found `}`.
- (0:10-0:11) We wanted `,` but we found a variable name.
- (0:12-0:13) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a.)
```

### AST
```
(call 0:0-0:5 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a.,)
```

### AST
```
(call 0:0-0:6 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a., b)
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (err (var 0:2-0:3 a)) (var 0:6-0:7 b))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a. (b))
```

### AST
```
(call 0:0-0:9
  (var 0:0-0:1 f)
  (call 0:2-0:8 (err (var 0:2-0:3 a)) (var 0:6-0:7 b)))
```

### Errors
- (0:5-0:6) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
f(a. (b).)
```

### AST
```
(call 0:0-0:10
  (var 0:0-0:1 f)
  (err (call 0:2-0:8 (err (var 0:2-0:3 a)) (var 0:6-0:7 b))))
```

### Errors
- (0:5-0:6) We wanted a variable name but we found `(`.
- (0:9-0:10) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a.😈)
```

### AST
```
(call 0:0-0:7 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a.})
```

### AST
```
(call 0:0-0:6 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a.😈})
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.
- (0:6-0:7) We wanted a variable name but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a.}😈)
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (err (var 0:2-0:3 a)))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `}`.
- (0:5-0:7) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b.)
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:7-0:8) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b.,)
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:7-0:8) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b., c)
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (var 0:9-0:10 c))
```

### Errors
- (0:7-0:8) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b. (c))
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (call 0:5-0:11 (err (var 0:5-0:6 b)) (var 0:9-0:10 c)))
```

### Errors
- (0:8-0:9) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b. (c).)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (call 0:5-0:11 (err (var 0:5-0:6 b)) (var 0:9-0:10 c))))
```

### Errors
- (0:8-0:9) We wanted a variable name but we found `(`.
- (0:12-0:13) We wanted a variable name but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b.😈)
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:7-0:9) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b.})
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:7-0:8) We wanted a variable name but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b.😈})
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:7-0:9) We wanted a variable name but we found `😈`.
- (0:9-0:10) We wanted a variable name but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b.}😈)
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (var 0:2-0:3 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:7-0:8) We wanted a variable name but we found `}`.
- (0:8-0:10) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(😈)
```

### AST
```
(call 0:0-0:5 (var 0:0-0:1 f) (err 0:2-0:4))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(})
```

### AST
```
(call 0:0-0:4 (var 0:0-0:1 f) (err 0:2-0:3))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(😈})
```

### AST
```
(call 0:0-0:6 (var 0:0-0:1 f) (err 0:2-0:5))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.
- (0:4-0:5) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(}😈)
```

### AST
```
(call 0:0-0:6 (var 0:0-0:1 f) (err 0:2-0:5))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `}`.
- (0:3-0:5) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, 😈)
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:5-0:7))
```

### Errors
- (0:5-0:7) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, })
```

### AST
```
(call 0:0-0:7 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:5-0:6))
```

### Errors
- (0:5-0:6) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, 😈})
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:5-0:8))
```

### Errors
- (0:5-0:7) We wanted an expression but we found `😈`.
- (0:7-0:8) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, }😈)
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:5-0:8))
```

### Errors
- (0:5-0:6) We wanted an expression but we found `}`.
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a b)
```

### AST
```
(err (call 0:0-0:6 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:4-0:5 b)))
```

### Errors
- (0:4-0:5) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b c)
```

### AST
```
(err
  (call 0:0-0:9
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:5-0:6 b)
    (var 0:7-0:8 c)))
```

### Errors
- (0:7-0:8) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a b, c)
```

### AST
```
(err
  (call 0:0-0:9
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:4-0:5 b)
    (var 0:7-0:8 c)))
```

### Errors
- (0:4-0:5) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a b c)
```

### AST
```
(err
  (call 0:0-0:8
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:4-0:5 b)
    (var 0:6-0:7 c)))
```

### Errors
- (0:4-0:5) We wanted `,` but we found a variable name.
- (0:6-0:7) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a b, c, d)
```

### AST
```
(err
  (call 0:0-0:12
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:4-0:5 b)
    (var 0:7-0:8 c)
    (var 0:10-0:11 d)))
```

### Errors
- (0:4-0:5) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b c, d)
```

### AST
```
(err
  (call 0:0-0:12
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:5-0:6 b)
    (var 0:7-0:8 c)
    (var 0:10-0:11 d)))
```

### Errors
- (0:7-0:8) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c d)
```

### AST
```
(err
  (call 0:0-0:12
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:5-0:6 b)
    (var 0:8-0:9 c)
    (var 0:10-0:11 d)))
```

### Errors
- (0:10-0:11) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a b c, d)
```

### AST
```
(err
  (call 0:0-0:11
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:4-0:5 b)
    (var 0:6-0:7 c)
    (var 0:9-0:10 d)))
```

### Errors
- (0:4-0:5) We wanted `,` but we found a variable name.
- (0:6-0:7) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b c d)
```

### AST
```
(err
  (call 0:0-0:11
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:5-0:6 b)
    (var 0:7-0:8 c)
    (var 0:9-0:10 d)))
```

### Errors
- (0:7-0:8) We wanted `,` but we found a variable name.
- (0:9-0:10) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a b, c d)
```

### AST
```
(err
  (call 0:0-0:11
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:4-0:5 b)
    (var 0:7-0:8 c)
    (var 0:9-0:10 d)))
```

### Errors
- (0:4-0:5) We wanted `,` but we found a variable name.
- (0:9-0:10) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a b c d)
```

### AST
```
(err
  (call 0:0-0:10
    (var 0:0-0:1 f)
    (var 0:2-0:3 a)
    (var 0:4-0:5 b)
    (var 0:6-0:7 c)
    (var 0:8-0:9 d)))
```

### Errors
- (0:4-0:5) We wanted `,` but we found a variable name.
- (0:6-0:7) We wanted `,` but we found a variable name.
- (0:8-0:9) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
f(a,, b)
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:4-0:5) (var 0:6-0:7 b))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a,, b, c)
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err 0:4-0:5)
  (var 0:6-0:7 b)
  (var 0:9-0:10 c))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b,, c)
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err 0:7-0:8)
  (var 0:9-0:10 c))
```

### Errors
- (0:7-0:8) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a,, b,, c)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err 0:4-0:5)
  (var 0:6-0:7 b)
  (err 0:8-0:9)
  (var 0:10-0:11 c))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `,`.
- (0:8-0:9) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a,, b, c, d)
```

### AST
```
(call 0:0-0:14
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err 0:4-0:5)
  (var 0:6-0:7 b)
  (var 0:9-0:10 c)
  (var 0:12-0:13 d))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b,, c, d)
```

### AST
```
(call 0:0-0:14
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err 0:7-0:8)
  (var 0:9-0:10 c)
  (var 0:12-0:13 d))
```

### Errors
- (0:7-0:8) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c,, d)
```

### AST
```
(call 0:0-0:14
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (var 0:8-0:9 c)
  (err 0:10-0:11)
  (var 0:12-0:13 d))
```

### Errors
- (0:10-0:11) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b,, c,, d)
```

### AST
```
(call 0:0-0:15
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err 0:7-0:8)
  (var 0:9-0:10 c)
  (err 0:11-0:12)
  (var 0:13-0:14 d))
```

### Errors
- (0:7-0:8) We wanted an expression but we found `,`.
- (0:11-0:12) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a,, b, c,, d)
```

### AST
```
(call 0:0-0:15
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err 0:4-0:5)
  (var 0:6-0:7 b)
  (var 0:9-0:10 c)
  (err 0:11-0:12)
  (var 0:13-0:14 d))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `,`.
- (0:11-0:12) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a,, b,, c, d)
```

### AST
```
(call 0:0-0:15
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err 0:4-0:5)
  (var 0:6-0:7 b)
  (err 0:8-0:9)
  (var 0:10-0:11 c)
  (var 0:13-0:14 d))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `,`.
- (0:8-0:9) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a,, b,, c,, d)
```

### AST
```
(call 0:0-0:16
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err 0:4-0:5)
  (var 0:6-0:7 b)
  (err 0:8-0:9)
  (var 0:10-0:11 c)
  (err 0:12-0:13)
  (var 0:14-0:15 d))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `,`.
- (0:8-0:9) We wanted an expression but we found `,`.
- (0:12-0:13) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c, d,)
```

### AST
```
(call 0:0-0:14
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (var 0:8-0:9 c)
  (var 0:11-0:12 d))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c, d,,)
```

### AST
```
(call 0:0-0:15
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (var 0:8-0:9 c)
  (var 0:11-0:12 d)
  (err 0:13-0:14))
```

### Errors
- (0:13-0:14) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c,,)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (var 0:8-0:9 c)
  (err 0:10-0:11))
```

### Errors
- (0:10-0:11) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b,,)
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b) (err 0:7-0:8))
```

### Errors
- (0:7-0:8) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a,,)
```

### AST
```
(call 0:0-0:6 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:4-0:5))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, 😈, c)
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:5-0:7) (var 0:9-0:10 c))
```

### Errors
- (0:5-0:7) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(😈, b, c)
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (err 0:2-0:4) (var 0:6-0:7 b) (var 0:9-0:10 c))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, 😈)
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b) (err 0:8-0:10))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, }, c)
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:5-0:6) (var 0:8-0:9 c))
```

### Errors
- (0:5-0:6) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(}, b, c)
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (err 0:2-0:3) (var 0:5-0:6 b) (var 0:8-0:9 c))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, })
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b) (err 0:8-0:9))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, 😈}, c)
```

### AST
```
(call 0:0-0:12 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:5-0:8) (var 0:10-0:11 c))
```

### Errors
- (0:5-0:7) We wanted an expression but we found `😈`.
- (0:7-0:8) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(😈}, b, c)
```

### AST
```
(call 0:0-0:12 (var 0:0-0:1 f) (err 0:2-0:5) (var 0:7-0:8 b) (var 0:10-0:11 c))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.
- (0:4-0:5) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, 😈})
```

### AST
```
(call 0:0-0:12 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b) (err 0:8-0:11))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.
- (0:10-0:11) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, }😈, c)
```

### AST
```
(call 0:0-0:12 (var 0:0-0:1 f) (var 0:2-0:3 a) (err 0:5-0:8) (var 0:10-0:11 c))
```

### Errors
- (0:5-0:6) We wanted an expression but we found `}`.
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(}😈, b, c)
```

### AST
```
(call 0:0-0:12 (var 0:0-0:1 f) (err 0:2-0:5) (var 0:7-0:8 b) (var 0:10-0:11 c))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `}`.
- (0:3-0:5) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, }😈)
```

### AST
```
(call 0:0-0:12 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b) (err 0:8-0:11))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `}`.
- (0:9-0:11) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈, c)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (var 0:10-0:11 c))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈, b, c)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (err (var 0:2-0:3 a))
  (var 0:7-0:8 b)
  (var 0:10-0:11 c))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c😈)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:8-0:9 c)))
```

### Errors
- (0:9-0:11) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}, c)
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (var 0:9-0:10 c))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a}, b, c)
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (err (var 0:2-0:3 a))
  (var 0:6-0:7 b)
  (var 0:9-0:10 c))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c})
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:8-0:9 c)))
```

### Errors
- (0:9-0:10) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈}, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (var 0:11-0:12 c))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.
- (0:8-0:9) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a😈}, b, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (err (var 0:2-0:3 a))
  (var 0:8-0:9 b)
  (var 0:11-0:12 c))
```

### Errors
- (0:3-0:5) We wanted an expression but we found `😈`.
- (0:5-0:6) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c😈})
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:8-0:9 c)))
```

### Errors
- (0:9-0:11) We wanted an expression but we found `😈`.
- (0:11-0:12) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b}😈, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:5-0:6 b))
  (var 0:11-0:12 c))
```

### Errors
- (0:6-0:7) We wanted an expression but we found `}`.
- (0:7-0:9) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a}😈, b, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (err (var 0:2-0:3 a))
  (var 0:8-0:9 b)
  (var 0:11-0:12 c))
```

### Errors
- (0:3-0:4) We wanted an expression but we found `}`.
- (0:4-0:6) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c}😈)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:8-0:9 c)))
```

### Errors
- (0:9-0:10) We wanted an expression but we found `}`.
- (0:10-0:12) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, 😈b, c)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:7-0:8 b))
  (var 0:10-0:11 c))
```

### Errors
- (0:5-0:7) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(😈a, b, c)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (err (var 0:4-0:5 a))
  (var 0:7-0:8 b)
  (var 0:10-0:11 c))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, 😈c)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:10-0:11 c)))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, }b, c)
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:6-0:7 b))
  (var 0:9-0:10 c))
```

### Errors
- (0:5-0:6) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(}a, b, c)
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (err (var 0:3-0:4 a))
  (var 0:6-0:7 b)
  (var 0:9-0:10 c))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, }c)
```

### AST
```
(call 0:0-0:11
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:9-0:10 c)))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, 😈}b, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:8-0:9 b))
  (var 0:11-0:12 c))
```

### Errors
- (0:5-0:7) We wanted an expression but we found `😈`.
- (0:7-0:8) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(😈}a, b, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (err (var 0:5-0:6 a))
  (var 0:8-0:9 b)
  (var 0:11-0:12 c))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.
- (0:4-0:5) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, 😈}c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:11-0:12 c)))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.
- (0:10-0:11) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, }😈b, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:8-0:9 b))
  (var 0:11-0:12 c))
```

### Errors
- (0:5-0:6) We wanted an expression but we found `}`.
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(}😈a, b, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (err (var 0:5-0:6 a))
  (var 0:8-0:9 b)
  (var 0:11-0:12 c))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `}`.
- (0:3-0:5) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, }😈c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:11-0:12 c)))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `}`.
- (0:9-0:11) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, 😈b}, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:7-0:8 b))
  (var 0:11-0:12 c))
```

### Errors
- (0:5-0:7) We wanted an expression but we found `😈`.
- (0:8-0:9) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(😈a}, b, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (err (var 0:4-0:5 a))
  (var 0:8-0:9 b)
  (var 0:11-0:12 c))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.
- (0:5-0:6) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, 😈c})
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:10-0:11 c)))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.
- (0:11-0:12) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, }b😈, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (err (var 0:6-0:7 b))
  (var 0:11-0:12 c))
```

### Errors
- (0:5-0:6) We wanted an expression but we found `}`.
- (0:7-0:9) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(}a😈, b, c)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (err (var 0:3-0:4 a))
  (var 0:8-0:9 b)
  (var 0:11-0:12 c))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `}`.
- (0:4-0:6) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, }c😈)
```

### AST
```
(call 0:0-0:13
  (var 0:0-0:1 f)
  (var 0:2-0:3 a)
  (var 0:5-0:6 b)
  (err (var 0:9-0:10 c)))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `}`.
- (0:10-0:12) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
f(, a)
```

### AST
```
(call 0:0-0:6 (var 0:0-0:1 f) (err 0:2-0:3) (var 0:4-0:5 a))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(, a, b)
```

### AST
```
(call 0:0-0:9 (var 0:0-0:1 f) (err 0:2-0:3) (var 0:4-0:5 a) (var 0:7-0:8 b))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(, a, b, c)
```

### AST
```
(call 0:0-0:12
  (var 0:0-0:1 f)
  (err 0:2-0:3)
  (var 0:4-0:5 a)
  (var 0:7-0:8 b)
  (var 0:10-0:11 c))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f(a,)
```

### AST
```
(call 0:0-0:5 (var 0:0-0:1 f) (var 0:2-0:3 a))
```

--------------------------------------------------------------------------------

### Source
```ite
f(,)
```

### AST
```
(call 0:0-0:4 (var 0:0-0:1 f) (err 0:2-0:3))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
f()
```

### AST
```
(call 0:0-0:3 (var 0:0-0:1 f))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a)
```

### AST
```
(call 0:0-0:4 (var 0:0-0:1 f) (var 0:2-0:3 a))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a,)
```

### AST
```
(call 0:0-0:5 (var 0:0-0:1 f) (var 0:2-0:3 a))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b)
```

### AST
```
(call 0:0-0:7 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b,)
```

### AST
```
(call 0:0-0:8 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c)
```

### AST
```
(call 0:0-0:10 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c,)
```

### AST
```
(call 0:0-0:11 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
let _ = x;
```

### AST
```
(bind (hole 0:4-0:5) (var 0:8-0:9 x))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = _;
```

### AST
```
(bind (var 0:4-0:5 x) (err 0:8-0:9))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `_`.

--------------------------------------------------------------------------------

### Source
```ite
let _ = _;
```

### AST
```
(bind (hole 0:4-0:5) (err 0:8-0:9))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `_`.

--------------------------------------------------------------------------------

### Source
```ite
fun() {}
```

### AST
```
(fun 0:0-0:8 err block)
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
(fun() {})
```

### AST
```
(wrap 0:0-0:10 (fun 0:1-0:9 block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() {}
```

### AST
```
(fun 0:0-0:10 (name 0:4-0:5 f) block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun(a) {}
```

### AST
```
(fun 0:0-0:9 err (param (var 0:4-0:5 a)) block)
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
(fun(a) {})
```

### AST
```
(wrap 0:0-0:11 (fun 0:1-0:10 (param (var 0:5-0:6 a)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun(a, b) {}
```

### AST
```
(fun 0:0-0:12 err (param (var 0:4-0:5 a)) (param (var 0:7-0:8 b)) block)
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
fun(a, b, c) {}
```

### AST
```
(fun 0:0-0:15
  err
  (param (var 0:4-0:5 a))
  (param (var 0:7-0:8 b))
  (param (var 0:10-0:11 c))
  block)
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
(fun(a, b) {})
```

### AST
```
(wrap 0:0-0:14
  (fun 0:1-0:13 (param (var 0:5-0:6 a)) (param (var 0:8-0:9 b)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
(fun(a, b, c) {})
```

### AST
```
(wrap 0:0-0:17
  (fun 0:1-0:16
    (param (var 0:5-0:6 a))
    (param (var 0:8-0:9 b))
    (param (var 0:11-0:12 c))
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a) {}
```

### AST
```
(fun 0:0-0:11 (name 0:4-0:5 f) (param (var 0:6-0:7 a)) block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b) {}
```

### AST
```
(fun 0:0-0:14
  (name 0:4-0:5 f)
  (param (var 0:6-0:7 a))
  (param (var 0:9-0:10 b))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b, c) {}
```

### AST
```
(fun 0:0-0:17
  (name 0:4-0:5 f)
  (param (var 0:6-0:7 a))
  (param (var 0:9-0:10 b))
  (param (var 0:12-0:13 c))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun(a,) {}
```

### AST
```
(fun 0:0-0:10 err (param (var 0:4-0:5 a)) block)
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
fun(a, b,) {}
```

### AST
```
(fun 0:0-0:13 err (param (var 0:4-0:5 a)) (param (var 0:7-0:8 b)) block)
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
fun(a, b, c,) {}
```

### AST
```
(fun 0:0-0:16
  err
  (param (var 0:4-0:5 a))
  (param (var 0:7-0:8 b))
  (param (var 0:10-0:11 c))
  block)
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
(fun(a,) {})
```

### AST
```
(wrap 0:0-0:12 (fun 0:1-0:11 (param (var 0:5-0:6 a)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
(fun(a, b,) {})
```

### AST
```
(wrap 0:0-0:15
  (fun 0:1-0:14 (param (var 0:5-0:6 a)) (param (var 0:8-0:9 b)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
(fun(a, b, c,) {})
```

### AST
```
(wrap 0:0-0:18
  (fun 0:1-0:17
    (param (var 0:5-0:6 a))
    (param (var 0:8-0:9 b))
    (param (var 0:11-0:12 c))
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a,) {}
```

### AST
```
(fun 0:0-0:12 (name 0:4-0:5 f) (param (var 0:6-0:7 a)) block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b,) {}
```

### AST
```
(fun 0:0-0:15
  (name 0:4-0:5 f)
  (param (var 0:6-0:7 a))
  (param (var 0:9-0:10 b))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b, c,) {}
```

### AST
```
(fun 0:0-0:18
  (name 0:4-0:5 f)
  (param (var 0:6-0:7 a))
  (param (var 0:9-0:10 b))
  (param (var 0:12-0:13 c))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun() { let x = y; }
```

### AST
```
(fun 0:0-0:20 err (block (bind (var 0:12-0:13 x) (var 0:16-0:17 y))))
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
(fun() { let x = y; })
```

### AST
```
(wrap 0:0-0:22
  (fun 0:1-0:21 (block (bind (var 0:13-0:14 x) (var 0:17-0:18 y)))))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() { let x = y; }
```

### AST
```
(fun 0:0-0:22
  (name 0:4-0:5 f)
  (block (bind (var 0:14-0:15 x) (var 0:18-0:19 y))))
```

--------------------------------------------------------------------------------

### Source
```ite
fun() {}()
```

### AST
```
(fun 0:0-0:8 err block)
(wrap 0:8-0:10 (err 0:9-0:10))
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.
- (0:9-0:10) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
(fun() {}())
```

### AST
```
(wrap 0:0-0:12 (call 0:1-0:11 (fun 0:1-0:9 block)))
```

--------------------------------------------------------------------------------

### Source
```ite
(fun() {})()
```

### AST
```
(call 0:0-0:12 (wrap 0:0-0:10 (fun 0:1-0:9 block)))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun() {};
```

### AST
```
(bind (var 0:4-0:5 f) (fun 0:8-0:16 block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f() {};
```

### AST
```
(bind (var 0:4-0:5 f) (err (fun 0:8-0:18 block)))
```

### Errors
- (0:12-0:13) We wanted `(` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a) {};
```

### AST
```
(bind (var 0:4-0:5 f) (fun 0:8-0:17 (param (var 0:12-0:13 a)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:20 (param (var 0:12-0:13 a)) (param (var 0:15-0:16 b)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b, c) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:23
    (param (var 0:12-0:13 a))
    (param (var 0:15-0:16 b))
    (param (var 0:18-0:19 c))
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f(a) {};
```

### AST
```
(bind (var 0:4-0:5 f) (err (fun 0:8-0:19 (param (var 0:14-0:15 a)) block)))
```

### Errors
- (0:12-0:13) We wanted `(` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f(a, b) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (err
    (fun 0:8-0:22 (param (var 0:14-0:15 a)) (param (var 0:17-0:18 b)) block)))
```

### Errors
- (0:12-0:13) We wanted `(` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f(a, b, c) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (err
    (fun 0:8-0:25
      (param (var 0:14-0:15 a))
      (param (var 0:17-0:18 b))
      (param (var 0:20-0:21 c))
      block)))
```

### Errors
- (0:12-0:13) We wanted `(` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a,) {};
```

### AST
```
(bind (var 0:4-0:5 f) (fun 0:8-0:18 (param (var 0:12-0:13 a)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b,) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:21 (param (var 0:12-0:13 a)) (param (var 0:15-0:16 b)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b, c,) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:24
    (param (var 0:12-0:13 a))
    (param (var 0:15-0:16 b))
    (param (var 0:18-0:19 c))
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f(a,) {};
```

### AST
```
(bind (var 0:4-0:5 f) (err (fun 0:8-0:20 (param (var 0:14-0:15 a)) block)))
```

### Errors
- (0:12-0:13) We wanted `(` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f(a, b,) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (err
    (fun 0:8-0:23 (param (var 0:14-0:15 a)) (param (var 0:17-0:18 b)) block)))
```

### Errors
- (0:12-0:13) We wanted `(` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f(a, b, c,) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (err
    (fun 0:8-0:26
      (param (var 0:14-0:15 a))
      (param (var 0:17-0:18 b))
      (param (var 0:20-0:21 c))
      block)))
```

### Errors
- (0:12-0:13) We wanted `(` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let f = fun() { let x = y; };
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:28 (block (bind (var 0:20-0:21 x) (var 0:24-0:25 y)))))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f() { let x = y; };
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (err (fun 0:8-0:30 (block (bind (var 0:22-0:23 x) (var 0:26-0:27 y))))))
```

### Errors
- (0:12-0:13) We wanted `(` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
let f = fun() {}();
```

### AST
```
(bind (var 0:4-0:5 f) (call 0:8-0:18 (fun 0:8-0:16 block)))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = (fun() {})();
```

### AST
```
(bind (var 0:4-0:5 f) (call 0:8-0:20 (wrap 0:8-0:18 (fun 0:9-0:17 block))))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() {}
```

### AST
```
(fun 0:0-0:10 (name 0:4-0:5 f) block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f) {}
```

### AST
```
(err (fun 0:0-0:9 (name 0:4-0:5 f) block))
```

### Errors
- (0:5-0:6) We wanted `(` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
fun f( {}
```

### AST
```
(err (fun 0:0-0:9 (name 0:4-0:5 f) (param (object 0:7-0:9)) block))
```

### Errors
- (0:9-0:9) We wanted `)` but the file ended.
- (0:9-0:9) We wanted `{` but the file ended.
- (0:9-0:9) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
fun f( { let x = y }
```

### AST
```
(err
  (fun 0:0-0:20
    (name 0:4-0:5 f)
    (param (err (object 0:7-0:8)))
    (block (bind (var 0:13-0:14 x) (var 0:17-0:18 y)))))
```

### Errors
- (0:9-0:12) We wanted `}` but we found `let`.
- (0:9-0:12) We wanted `)` but we found `let`.
- (0:9-0:12) We wanted `{` but we found `let`.

--------------------------------------------------------------------------------

### Source
```ite
fun f() }
```

### AST
```
(err (fun 0:0-0:9 (name 0:4-0:5 f) block))
```

### Errors
- (0:8-0:9) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
fun f() {
```

### AST
```
(err (fun 0:0-0:9 (name 0:4-0:5 f) block))
```

### Errors
- (0:9-0:9) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
fun) {}
```

### AST
```
(err (fun 0:0-0:7 err block))
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `)`.
- (0:3-0:4) We wanted `(` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
fun( {}
```

### AST
```
(err (fun 0:0-0:7 err (param (object 0:5-0:7)) block))
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.
- (0:7-0:7) We wanted `)` but the file ended.
- (0:7-0:7) We wanted `{` but the file ended.
- (0:7-0:7) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
fun() }
```

### AST
```
(err (fun 0:0-0:7 err block))
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.
- (0:6-0:7) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
fun() {
```

### AST
```
(err (fun 0:0-0:7 err block))
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.
- (0:7-0:7) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
fun(a, b let x = y; }
```

### AST
```
(err
  (fun 0:0-0:21
    err
    (param (var 0:4-0:5 a))
    (param (var 0:7-0:8 b))
    (block (bind (var 0:13-0:14 x) (var 0:17-0:18 y)))))
```

### Errors
- (0:3-0:4) We wanted a variable name but we found `(`.
- (0:9-0:12) We wanted `)` but we found `let`.
- (0:9-0:12) We wanted `{` but we found `let`.

--------------------------------------------------------------------------------

### Source
```ite
(fun) {})
```

### AST
```
(wrap 0:0-0:9 (err (fun 0:1-0:8 block)))
```

### Errors
- (0:4-0:5) We wanted `(` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
(fun( {})
```

### AST
```
(err (wrap 0:0-0:9 (err (fun 0:1-0:9 (param (object 0:6-0:8)) block))))
```

### Errors
- (0:9-0:9) We wanted `{` but the file ended.
- (0:9-0:9) We wanted `}` but the file ended.
- (0:9-0:9) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
(fun() })
```

### AST
```
(wrap 0:0-0:9 (err (fun 0:1-0:8 block)))
```

### Errors
- (0:7-0:8) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
(fun() {)
```

### AST
```
(wrap 0:0-0:9 (err (fun 0:1-0:8 block)))
```

### Errors
- (0:8-0:9) We wanted `}` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
(fun(a, b let x = y; })
```

### AST
```
(wrap 0:0-0:23
  (err
    (fun 0:1-0:22
      (param (var 0:5-0:6 a))
      (param (var 0:8-0:9 b))
      (block (bind (var 0:14-0:15 x) (var 0:18-0:19 y))))))
```

### Errors
- (0:10-0:13) We wanted `)` but we found `let`.
- (0:10-0:13) We wanted `{` but we found `let`.

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b let x = y; }
```

### AST
```
(err
  (fun 0:0-0:23
    (name 0:4-0:5 f)
    (param (var 0:6-0:7 a))
    (param (var 0:9-0:10 b))
    (block (bind (var 0:15-0:16 x) (var 0:19-0:20 y)))))
```

### Errors
- (0:11-0:14) We wanted `)` but we found `let`.
- (0:11-0:14) We wanted `{` but we found `let`.

--------------------------------------------------------------------------------

### Source
```ite
fun 😈 f() {}
```

### AST
```
(err (fun 0:0-0:13 (name 0:7-0:8 f) block))
```

### Errors
- (0:4-0:6) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
fun f😈() {}
```

### AST
```
(err (fun 0:0-0:12 (name 0:4-0:5 f) block))
```

### Errors
- (0:5-0:7) We wanted `(` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
fun f(😈) {}
```

### AST
```
(fun 0:0-0:12 (name 0:4-0:5 f) (param (err 0:6-0:8)) block)
```

### Errors
- (0:6-0:8) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
fun f() 😈 {}
```

### AST
```
(fun 0:0-0:13 (name 0:4-0:5 f) (type (err 0:8-0:10)) block)
```

### Errors
- (0:8-0:10) We wanted `->` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
fun f() {😈}
```

### AST
```
(fun 0:0-0:12 (name 0:4-0:5 f) (block (err 0:9-0:11)))
```

### Errors
- (0:9-0:11) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
fun ] f() {}
```

### AST
```
(err (fun 0:0-0:12 (name 0:6-0:7 f) block))
```

### Errors
- (0:4-0:5) We wanted a variable name but we found `]`.

--------------------------------------------------------------------------------

### Source
```ite
fun f]() {}
```

### AST
```
(err (fun 0:0-0:11 (name 0:4-0:5 f) block))
```

### Errors
- (0:5-0:6) We wanted `(` but we found `]`.

--------------------------------------------------------------------------------

### Source
```ite
fun f(]) {}
```

### AST
```
(fun 0:0-0:11 (name 0:4-0:5 f) (param (err 0:6-0:7)) block)
```

### Errors
- (0:6-0:7) We wanted a variable name but we found `]`.

--------------------------------------------------------------------------------

### Source
```ite
fun f() ] {}
```

### AST
```
(fun 0:0-0:12 (name 0:4-0:5 f) (type (err 0:8-0:9)) block)
```

### Errors
- (0:8-0:9) We wanted `->` but we found `]`.

--------------------------------------------------------------------------------

### Source
```ite
fun f() {]}
```

### AST
```
(fun 0:0-0:11 (name 0:4-0:5 f) (block (err 0:9-0:10)))
```

### Errors
- (0:9-0:10) We wanted a statement but we found `]`.

--------------------------------------------------------------------------------

### Source
```ite
fun f(,) {}
```

### AST
```
(fun 0:0-0:11 (name 0:4-0:5 f) (param (err 0:6-0:7)) block)
```

### Errors
- (0:6-0:7) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
return
```

### AST
```
(return 0:0-0:6)
```

--------------------------------------------------------------------------------

### Source
```ite
return x
```

### AST
```
(return 0:0-0:8 (var 0:7-0:8 x))
```

--------------------------------------------------------------------------------

### Source
```ite
return
x
```

### AST
```
(return 0:0-0:6)
(var 1:0-1:1 x)
```

--------------------------------------------------------------------------------

### Source
```ite
return;
```

### AST
```
(return 0:0-0:6)
```

--------------------------------------------------------------------------------

### Source
```ite
return;x
```

### AST
```
(return 0:0-0:6)
(var 0:7-0:8 x)
```

--------------------------------------------------------------------------------

### Source
```ite
return x;
```

### AST
```
(return 0:0-0:8 (var 0:7-0:8 x))
```

--------------------------------------------------------------------------------

### Source
```ite
return 😈 x;
```

### AST
```
(return 0:0-0:11 (err (var 0:10-0:11 x)))
```

### Errors
- (0:7-0:9) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
return
😈 x;
```

### AST
```
(err (return 0:0-0:6))
(var 1:3-1:4 x)
```

### Errors
- (1:0-1:2) We wanted `;` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
return 😈
x;
```

### AST
```
(return 0:0-0:9 (err 0:7-0:9))
(var 1:0-1:1 x)
```

### Errors
- (0:7-0:9) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
return ) x;
```

### AST
```
(return 0:0-0:10 (err (var 0:9-0:10 x)))
```

### Errors
- (0:7-0:8) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
return
) x;
```

### AST
```
(err (return 0:0-0:6))
(var 1:2-1:3 x)
```

### Errors
- (1:0-1:1) We wanted `;` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
return )
x;
```

### AST
```
(return 0:0-0:8 (err 0:7-0:8))
(var 1:0-1:1 x)
```

### Errors
- (0:7-0:8) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
return 😈) x;
```

### AST
```
(return 0:0-0:12 (err (var 0:11-0:12 x)))
```

### Errors
- (0:7-0:9) We wanted an expression but we found `😈`.
- (0:9-0:10) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
return
😈) x;
```

### AST
```
(err (return 0:0-0:6))
(var 1:4-1:5 x)
```

### Errors
- (1:0-1:2) We wanted `;` but we found `😈`.
- (1:2-1:3) We wanted `;` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
return 😈)
x;
```

### AST
```
(return 0:0-0:10 (err 0:7-0:10))
(var 1:0-1:1 x)
```

### Errors
- (0:7-0:9) We wanted an expression but we found `😈`.
- (0:9-0:10) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
return )😈 x;
```

### AST
```
(return 0:0-0:12 (err (var 0:11-0:12 x)))
```

### Errors
- (0:7-0:8) We wanted an expression but we found `)`.
- (0:8-0:10) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
return
)😈 x;
```

### AST
```
(err (return 0:0-0:6))
(var 1:4-1:5 x)
```

### Errors
- (1:0-1:1) We wanted `;` but we found `)`.
- (1:1-1:3) We wanted `;` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
return )😈
x;
```

### AST
```
(return 0:0-0:10 (err 0:7-0:10))
(var 1:0-1:1 x)
```

### Errors
- (0:7-0:8) We wanted an expression but we found `)`.
- (0:8-0:10) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
break
```

### AST
```
(break 0:0-0:5)
```

--------------------------------------------------------------------------------

### Source
```ite
break x
```

### AST
```
(break 0:0-0:7 (var 0:6-0:7 x))
```

--------------------------------------------------------------------------------

### Source
```ite
break
x
```

### AST
```
(break 0:0-0:5)
(var 1:0-1:1 x)
```

--------------------------------------------------------------------------------

### Source
```ite
break;
```

### AST
```
(break 0:0-0:5)
```

--------------------------------------------------------------------------------

### Source
```ite
break;x
```

### AST
```
(break 0:0-0:5)
(var 0:6-0:7 x)
```

--------------------------------------------------------------------------------

### Source
```ite
break x;
```

### AST
```
(break 0:0-0:7 (var 0:6-0:7 x))
```

--------------------------------------------------------------------------------

### Source
```ite
loop {}
```

### AST
```
(loop 0:0-0:7)
```

--------------------------------------------------------------------------------

### Source
```ite
loop { let x = y; }
```

### AST
```
(loop 0:0-0:19 (bind (var 0:11-0:12 x) (var 0:15-0:16 y)))
```

--------------------------------------------------------------------------------

### Source
```ite
!x
```

### AST
```
(not 0:0-0:2 (var 0:1-0:2 x))
```

--------------------------------------------------------------------------------

### Source
```ite
+x
```

### AST
```
(pos 0:0-0:2 (var 0:1-0:2 x))
```

--------------------------------------------------------------------------------

### Source
```ite
-x
```

### AST
```
(neg 0:0-0:2 (var 0:1-0:2 x))
```

--------------------------------------------------------------------------------

### Source
```ite
!x.p
```

### AST
```
(not 0:0-0:4 (prop 0:1-0:4 (var 0:1-0:2 x) (name 0:3-0:4 p)))
```

--------------------------------------------------------------------------------

### Source
```ite
!x()
```

### AST
```
(not 0:0-0:4 (call 0:1-0:4 (var 0:1-0:2 x)))
```

--------------------------------------------------------------------------------

### Source
```ite
!
```

### AST
```
(not 0:0-0:1 (err 0:1-0:1))
```

### Errors
- (0:1-0:1) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
!😈x
```

### AST
```
(not 0:0-0:4 (err (var 0:3-0:4 x)))
```

### Errors
- (0:1-0:3) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
!)x
```

### AST
```
(not 0:0-0:3 (err (var 0:2-0:3 x)))
```

### Errors
- (0:1-0:2) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
!😈)x
```

### AST
```
(not 0:0-0:5 (err (var 0:4-0:5 x)))
```

### Errors
- (0:1-0:3) We wanted an expression but we found `😈`.
- (0:3-0:4) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
!)😈x
```

### AST
```
(not 0:0-0:5 (err (var 0:4-0:5 x)))
```

### Errors
- (0:1-0:2) We wanted an expression but we found `)`.
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
!!x
```

### AST
```
(not 0:0-0:3 (not 0:1-0:3 (var 0:2-0:3 x)))
```

--------------------------------------------------------------------------------

### Source
```ite
++x
```

### AST
```
(pos 0:0-0:3 (pos 0:1-0:3 (var 0:2-0:3 x)))
```

--------------------------------------------------------------------------------

### Source
```ite
--x
```

### AST
```
(neg 0:0-0:3 (neg 0:1-0:3 (var 0:2-0:3 x)))
```

--------------------------------------------------------------------------------

### Source
```ite
+-x
```

### AST
```
(pos 0:0-0:3 (neg 0:1-0:3 (var 0:2-0:3 x)))
```

--------------------------------------------------------------------------------

### Source
```ite
-+x
```

### AST
```
(neg 0:0-0:3 (pos 0:1-0:3 (var 0:2-0:3 x)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b
```

### AST
```
(add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b + c
```

### AST
```
(add 0:0-0:9 (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a - b
```

### AST
```
(sub 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a - b - c
```

### AST
```
(sub 0:0-0:9 (sub 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b
```

### AST
```
(mul 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b * c
```

### AST
```
(mul 0:0-0:9 (mul 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a / b
```

### AST
```
(div 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a / b / c
```

### AST
```
(div 0:0-0:9 (div 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a % b
```

### AST
```
(rem 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a % b % c
```

### AST
```
(rem 0:0-0:9 (rem 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a == b
```

### AST
```
(eq 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a == b == c
```

### AST
```
(eq 0:0-0:11 (eq 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a != b
```

### AST
```
(ne 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a != b != c
```

### AST
```
(ne 0:0-0:11 (ne 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a < b
```

### AST
```
(lt 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a < b < c
```

### AST
```
(lt 0:0-0:9 (lt 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a <= b
```

### AST
```
(lte 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a <= b <= c
```

### AST
```
(lte 0:0-0:11 (lte 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a > b
```

### AST
```
(gt 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a > b > c
```

### AST
```
(gt 0:0-0:9 (gt 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a >= b
```

### AST
```
(gte 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a >= b >= c
```

### AST
```
(gte 0:0-0:11 (gte 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b - c
```

### AST
```
(sub 0:0-0:9 (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a - b + c
```

### AST
```
(add 0:0-0:9 (sub 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b * c
```

### AST
```
(add 0:0-0:9 (var 0:0-0:1 a) (mul 0:4-0:9 (var 0:4-0:5 b) (var 0:8-0:9 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b + c
```

### AST
```
(add 0:0-0:9 (mul 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b / c
```

### AST
```
(add 0:0-0:9 (var 0:0-0:1 a) (div 0:4-0:9 (var 0:4-0:5 b) (var 0:8-0:9 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a / b + c
```

### AST
```
(add 0:0-0:9 (div 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b / c
```

### AST
```
(div 0:0-0:9 (mul 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a / b * c
```

### AST
```
(mul 0:0-0:9 (div 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b * c + d
```

### AST
```
(add 0:0-0:13
  (var 0:0-0:1 a)
  (add 0:4-0:13
    (mul 0:4-0:9 (var 0:4-0:5 b) (var 0:8-0:9 c))
    (var 0:12-0:13 d)))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b + c * d
```

### AST
```
(add 0:0-0:13
  (mul 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
  (mul 0:8-0:13 (var 0:8-0:9 c) (var 0:12-0:13 d)))
```

--------------------------------------------------------------------------------

### Source
```ite
a ^ b + c
```

### AST
```
(add 0:0-0:9 (exp 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b ^ c
```

### AST
```
(add 0:0-0:9 (var 0:0-0:1 a) (exp 0:4-0:9 (var 0:4-0:5 b) (var 0:8-0:9 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a ^ b * c
```

### AST
```
(mul 0:0-0:9 (exp 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b ^ c
```

### AST
```
(mul 0:0-0:9 (var 0:0-0:1 a) (exp 0:4-0:9 (var 0:4-0:5 b) (var 0:8-0:9 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a > b + c
```

### AST
```
(gt 0:0-0:9 (var 0:0-0:1 a) (add 0:4-0:9 (var 0:4-0:5 b) (var 0:8-0:9 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b > c
```

### AST
```
(gt 0:0-0:9 (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a < b + c
```

### AST
```
(lt 0:0-0:9 (var 0:0-0:1 a) (add 0:4-0:9 (var 0:4-0:5 b) (var 0:8-0:9 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b < c
```

### AST
```
(lt 0:0-0:9 (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a >= b + c
```

### AST
```
(gte 0:0-0:10 (var 0:0-0:1 a) (add 0:5-0:10 (var 0:5-0:6 b) (var 0:9-0:10 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b >= c
```

### AST
```
(gte 0:0-0:10 (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:9-0:10 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a <= b + c
```

### AST
```
(lte 0:0-0:10 (var 0:0-0:1 a) (add 0:5-0:10 (var 0:5-0:6 b) (var 0:9-0:10 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b <= c
```

### AST
```
(lte 0:0-0:10 (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:9-0:10 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b == c
```

### AST
```
(eq 0:0-0:10 (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:9-0:10 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a == b + c
```

### AST
```
(eq 0:0-0:10 (var 0:0-0:1 a) (add 0:5-0:10 (var 0:5-0:6 b) (var 0:9-0:10 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b != c
```

### AST
```
(ne 0:0-0:10 (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:9-0:10 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a != b + c
```

### AST
```
(ne 0:0-0:10 (var 0:0-0:1 a) (add 0:5-0:10 (var 0:5-0:6 b) (var 0:9-0:10 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a ==
```

### AST
```
(eq 0:0-0:4 (var 0:0-0:1 a) (err 0:4-0:4))
```

### Errors
- (0:4-0:4) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
== b
```

### AST
```
(err 0:0-0:2)
(var 0:3-0:4 b)
```

### Errors
- (0:0-0:2) We wanted a statement but we found `==`.

--------------------------------------------------------------------------------

### Source
```ite
a 😈 == b
```

### AST
```
(eq 0:0-0:9 (err (var 0:0-0:1 a)) (var 0:8-0:9 b))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a == 😈 b
```

### AST
```
(eq 0:0-0:9 (var 0:0-0:1 a) (err (var 0:8-0:9 b)))
```

### Errors
- (0:5-0:7) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a == b 😈
```

### AST
```
(eq 0:0-0:6 (var 0:0-0:1 a) (err (var 0:5-0:6 b)))
```

### Errors
- (0:7-0:9) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a ) == b
```

### AST
```
(eq 0:0-0:8 (err (var 0:0-0:1 a)) (var 0:7-0:8 b))
```

### Errors
- (0:2-0:3) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
a == ) b
```

### AST
```
(eq 0:0-0:8 (var 0:0-0:1 a) (err (var 0:7-0:8 b)))
```

### Errors
- (0:5-0:6) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
a.p + b.q
```

### AST
```
(add 0:0-0:9
  (prop 0:0-0:3 (var 0:0-0:1 a) (name 0:2-0:3 p))
  (prop 0:6-0:9 (var 0:6-0:7 b) (name 0:8-0:9 q)))
```

--------------------------------------------------------------------------------

### Source
```ite
!a + !b
```

### AST
```
(add 0:0-0:7 (not 0:0-0:2 (var 0:1-0:2 a)) (not 0:5-0:7 (var 0:6-0:7 b)))
```

--------------------------------------------------------------------------------

### Source
```ite
a() + b()
```

### AST
```
(add 0:0-0:9 (call 0:0-0:3 (var 0:0-0:1 a)) (call 0:6-0:9 (var 0:6-0:7 b)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b +
```

### AST
```
(add 0:0-0:7 (var 0:0-0:1 a) (add 0:4-0:7 (var 0:4-0:5 b) (err 0:7-0:7)))
```

### Errors
- (0:7-0:7) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
a + b + c +
```

### AST
```
(add 0:0-0:11
  (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
  (add 0:8-0:11 (var 0:8-0:9 c) (err 0:11-0:11)))
```

### Errors
- (0:11-0:11) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
a 😈 + b + c
```

### AST
```
(add 0:0-0:12
  (add 0:0-0:8 (err (var 0:0-0:1 a)) (var 0:7-0:8 b))
  (var 0:11-0:12 c))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a + 😈 b + c
```

### AST
```
(add 0:0-0:12
  (add 0:0-0:8 (var 0:0-0:1 a) (err (var 0:7-0:8 b)))
  (var 0:11-0:12 c))
```

### Errors
- (0:4-0:6) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a + b 😈 + c
```

### AST
```
(add 0:0-0:12
  (add 0:0-0:5 (var 0:0-0:1 a) (err (var 0:4-0:5 b)))
  (var 0:11-0:12 c))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a + b + 😈 c
```

### AST
```
(add 0:0-0:12
  (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
  (err (var 0:11-0:12 c)))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a + b + c 😈
```

### AST
```
(add 0:0-0:9
  (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
  (err (var 0:8-0:9 c)))
```

### Errors
- (0:10-0:12) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
^ b * c ^ d
```

### AST
```
(err 0:0-0:1)
(mul 0:2-0:11 (var 0:2-0:3 b) (exp 0:6-0:11 (var 0:6-0:7 c) (var 0:10-0:11 d)))
```

### Errors
- (0:0-0:1) We wanted a statement but we found `^`.

--------------------------------------------------------------------------------

### Source
```ite
a ^ * c ^ d
```

### AST
```
(mul 0:0-0:11
  (exp 0:0-0:5 (var 0:0-0:1 a) (err 0:4-0:5))
  (exp 0:6-0:11 (var 0:6-0:7 c) (var 0:10-0:11 d)))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `*`.

--------------------------------------------------------------------------------

### Source
```ite
a ^ b * ^ d
```

### AST
```
(exp 0:0-0:11
  (var 0:0-0:1 a)
  (exp 0:4-0:11 (mul 0:4-0:9 (var 0:4-0:5 b) (err 0:8-0:9)) (var 0:10-0:11 d)))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `^`.

--------------------------------------------------------------------------------

### Source
```ite
a * ^ c * d
```

### AST
```
(mul 0:0-0:11
  (exp 0:0-0:7 (mul 0:0-0:5 (var 0:0-0:1 a) (err 0:4-0:5)) (var 0:6-0:7 c))
  (var 0:10-0:11 d))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `^`.

--------------------------------------------------------------------------------

### Source
```ite
a * b ^ * d
```

### AST
```
(mul 0:0-0:11
  (var 0:0-0:1 a)
  (mul 0:4-0:11 (exp 0:4-0:9 (var 0:4-0:5 b) (err 0:8-0:9)) (var 0:10-0:11 d)))
```

### Errors
- (0:8-0:9) We wanted an expression but we found `*`.

--------------------------------------------------------------------------------

### Source
```ite
a ^ b * c ^
```

### AST
```
(mul 0:0-0:11
  (exp 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
  (exp 0:8-0:11 (var 0:8-0:9 c) (err 0:11-0:11)))
```

### Errors
- (0:11-0:11) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
a 😈 * b + c * d
```

### AST
```
(add 0:0-0:16
  (mul 0:0-0:8 (err (var 0:0-0:1 a)) (var 0:7-0:8 b))
  (mul 0:11-0:16 (var 0:11-0:12 c) (var 0:15-0:16 d)))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a * 😈 b + c * d
```

### AST
```
(add 0:0-0:16
  (mul 0:0-0:8 (var 0:0-0:1 a) (err (var 0:7-0:8 b)))
  (mul 0:11-0:16 (var 0:11-0:12 c) (var 0:15-0:16 d)))
```

### Errors
- (0:4-0:6) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a * b 😈 + c * d
```

### AST
```
(add 0:0-0:16
  (mul 0:0-0:5 (var 0:0-0:1 a) (err (var 0:4-0:5 b)))
  (mul 0:11-0:16 (var 0:11-0:12 c) (var 0:15-0:16 d)))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a * b + 😈 c * d
```

### AST
```
(add 0:0-0:16
  (mul 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
  (err (mul 0:11-0:16 (var 0:11-0:12 c) (var 0:15-0:16 d))))
```

### Errors
- (0:8-0:10) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a * b + c 😈 * d
```

### AST
```
(add 0:0-0:16
  (mul 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
  (mul 0:8-0:16 (err (var 0:8-0:9 c)) (var 0:15-0:16 d)))
```

### Errors
- (0:10-0:12) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a * b + c * 😈 d
```

### AST
```
(add 0:0-0:16
  (mul 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
  (mul 0:8-0:16 (var 0:8-0:9 c) (err (var 0:15-0:16 d))))
```

### Errors
- (0:12-0:14) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a * b + c * d 😈
```

### AST
```
(add 0:0-0:13
  (mul 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b))
  (mul 0:8-0:13 (var 0:8-0:9 c) (err (var 0:12-0:13 d))))
```

### Errors
- (0:14-0:16) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a - b + c
```

### AST
```
(add 0:0-0:9 (sub 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a + -b + c
```

### AST
```
(add 0:0-0:10
  (add 0:0-0:6 (var 0:0-0:1 a) (neg 0:4-0:6 (var 0:5-0:6 b)))
  (var 0:9-0:10 c))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if y {}
```

### AST
```
(if 0:0-0:20 (var 0:3-0:4 x) block (if (var 0:16-0:17 y) block))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if y {} else {}
```

### AST
```
(if 0:0-0:28 (var 0:3-0:4 x) block (if (var 0:16-0:17 y) block block))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if y {} else if z {}
```

### AST
```
(if 0:0-0:33
  (var 0:3-0:4 x)
  block
  (if (var 0:16-0:17 y) block (if (var 0:29-0:30 z) block)))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if y {} else if z {} else {}
```

### AST
```
(if 0:0-0:41
  (var 0:3-0:4 x)
  block
  (if (var 0:16-0:17 y) block (if (var 0:29-0:30 z) block block)))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if {}
```

### AST
```
(if 0:0-0:18 (var 0:3-0:4 x) block (if (err 0:16-0:17) block))
```

### Errors
- (0:16-0:17) We wanted an expression but we found `{`.

--------------------------------------------------------------------------------

### Source
```ite
if x {} else 😈 if y {}
```

### AST
```
(err
  (if 0:0-0:23
    (var 0:3-0:4 x)
    block
    (block (if 0:16-0:23 (var 0:19-0:20 y) block))))
```

### Errors
- (0:13-0:15) We wanted `{` but we found `😈`.
- (0:23-0:23) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x {} else 😈 if y + z {}
```

### AST
```
(err
  (if 0:0-0:27
    (var 0:3-0:4 x)
    block
    (block
      (if 0:16-0:27
        (add 0:19-0:24 (var 0:19-0:20 y) (var 0:23-0:24 z))
        block))))
```

### Errors
- (0:13-0:15) We wanted `{` but we found `😈`.
- (0:27-0:27) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
if x {} else { if y {} }
```

### AST
```
(if 0:0-0:24
  (var 0:3-0:4 x)
  block
  (block (if 0:15-0:22 (var 0:18-0:19 y) block)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b * c ^ d
```

### AST
```
(add 0:0-0:13
  (var 0:0-0:1 a)
  (mul 0:4-0:13
    (var 0:4-0:5 b)
    (exp 0:8-0:13 (var 0:8-0:9 c) (var 0:12-0:13 d))))
```

--------------------------------------------------------------------------------

### Source
```ite
a * ^ b
```

### AST
```
(exp 0:0-0:7 (mul 0:0-0:5 (var 0:0-0:1 a) (err 0:4-0:5)) (var 0:6-0:7 b))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `^`.

--------------------------------------------------------------------------------

### Source
```ite
a ^ * b
```

### AST
```
(mul 0:0-0:7 (exp 0:0-0:5 (var 0:0-0:1 a) (err 0:4-0:5)) (var 0:6-0:7 b))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `*`.

--------------------------------------------------------------------------------

### Source
```ite
a * * b
```

### AST
```
(mul 0:0-0:7 (mul 0:0-0:5 (var 0:0-0:1 a) (err 0:4-0:5)) (var 0:6-0:7 b))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `*`.

--------------------------------------------------------------------------------

### Source
```ite
a 😈 * ^ b
```

### AST
```
(exp 0:0-0:10
  (mul 0:0-0:8 (err (var 0:0-0:1 a)) (err 0:7-0:8))
  (var 0:9-0:10 b))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.
- (0:7-0:8) We wanted an expression but we found `^`.

--------------------------------------------------------------------------------

### Source
```ite
a 😈 ^ * b
```

### AST
```
(mul 0:0-0:10
  (exp 0:0-0:8 (err (var 0:0-0:1 a)) (err 0:7-0:8))
  (var 0:9-0:10 b))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.
- (0:7-0:8) We wanted an expression but we found `*`.

--------------------------------------------------------------------------------

### Source
```ite
a 😈 * * b
```

### AST
```
(mul 0:0-0:10
  (mul 0:0-0:8 (err (var 0:0-0:1 a)) (err 0:7-0:8))
  (var 0:9-0:10 b))
```

### Errors
- (0:2-0:4) We wanted an expression but we found `😈`.
- (0:7-0:8) We wanted an expression but we found `*`.

--------------------------------------------------------------------------------

### Source
```ite
{}
```

### AST
```
(object 0:0-0:2)
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a}
```

### AST
```
(object 0:0-0:6 (prop (name 0:1-0:2 p) (var 0:4-0:5 a)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, q: b}
```

### AST
```
(object 0:0-0:12
  (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
  (prop (name 0:7-0:8 q) (var 0:10-0:11 b)))
```

--------------------------------------------------------------------------------

### Source
```ite
{,}
```

### AST
```
(err (object 0:0-0:3))
```

### Errors
- (0:1-0:2) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
{p: a,}
```

### AST
```
(object 0:0-0:7 (prop (name 0:1-0:2 p) (var 0:4-0:5 a)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, q: b,}
```

### AST
```
(object 0:0-0:13
  (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
  (prop (name 0:7-0:8 q) (var 0:10-0:11 b)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a q: b}
```

### AST
```
(err
  (object 0:0-0:11
    (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
    (prop (name 0:6-0:7 q) (var 0:9-0:10 b))))
```

### Errors
- (0:6-0:7) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
{p: a,, q: b}
```

### AST
```
(err
  (object 0:0-0:13
    (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
    (prop (name 0:8-0:9 q) (var 0:11-0:12 b))))
```

### Errors
- (0:6-0:7) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
{p: a, q: b,,}
```

### AST
```
(err
  (object 0:0-0:14
    (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
    (prop (name 0:7-0:8 q) (var 0:10-0:11 b))))
```

### Errors
- (0:12-0:13) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
{p: a q: b,}
```

### AST
```
(err
  (object 0:0-0:12
    (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
    (prop (name 0:6-0:7 q) (var 0:9-0:10 b))))
```

### Errors
- (0:6-0:7) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
{| o}
```

### AST
```
(object 0:0-0:5 (var 0:3-0:4 o))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a | o}
```

### AST
```
(object 0:0-0:10 (prop (name 0:1-0:2 p) (var 0:4-0:5 a)) (var 0:8-0:9 o))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, | o}
```

### AST
```
(object 0:0-0:11 (prop (name 0:1-0:2 p) (var 0:4-0:5 a)) (var 0:9-0:10 o))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, q: b | o}
```

### AST
```
(object 0:0-0:16
  (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
  (prop (name 0:7-0:8 q) (var 0:10-0:11 b))
  (var 0:14-0:15 o))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a | {q: b}}
```

### AST
```
(object 0:0-0:15
  (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
  (object 0:8-0:14 (prop (name 0:9-0:10 q) (var 0:12-0:13 b))))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a | {q: b | {}}}
```

### AST
```
(object 0:0-0:20
  (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
  (object 0:8-0:19
    (prop (name 0:9-0:10 q) (var 0:12-0:13 b))
    (object 0:16-0:18)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a | {q: b | o}}
```

### AST
```
(object 0:0-0:19
  (prop (name 0:1-0:2 p) (var 0:4-0:5 a))
  (object 0:8-0:18
    (prop (name 0:9-0:10 q) (var 0:12-0:13 b))
    (var 0:16-0:17 o)))
```

--------------------------------------------------------------------------------

### Source
```ite
{: a}
```

### AST
```
(err (object 0:0-0:5 (prop (name 0:3-0:4 a))))
```

### Errors
- (0:1-0:2) We wanted a variable name but we found `:`.

--------------------------------------------------------------------------------

### Source
```ite
{p a}
```

### AST
```
(err (object 0:0-0:5 (prop (name 0:1-0:2 p)) (prop (name 0:3-0:4 a))))
```

### Errors
- (0:3-0:4) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
{p: }
```

### AST
```
(object 0:0-0:5 (prop (name 0:1-0:2 p) (err 0:4-0:5)))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
{: a, q: b}
```

### AST
```
(err
  (object 0:0-0:11
    (prop (name 0:3-0:4 a))
    (prop (name 0:6-0:7 q) (var 0:9-0:10 b))))
```

### Errors
- (0:1-0:2) We wanted a variable name but we found `:`.

--------------------------------------------------------------------------------

### Source
```ite
{p a, q: b}
```

### AST
```
(err
  (object 0:0-0:11
    (prop (name 0:1-0:2 p))
    (prop (name 0:3-0:4 a))
    (prop (name 0:6-0:7 q) (var 0:9-0:10 b))))
```

### Errors
- (0:3-0:4) We wanted `,` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
{p: , q: b}
```

### AST
```
(object 0:0-0:11
  (prop (name 0:1-0:2 p) (err 0:4-0:5))
  (prop (name 0:6-0:7 q) (var 0:9-0:10 b)))
```

### Errors
- (0:4-0:5) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
{p}
```

### AST
```
(object 0:0-0:3 (prop (name 0:1-0:2 p)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p, q}
```

### AST
```
(object 0:0-0:6 (prop (name 0:1-0:2 p)) (prop (name 0:4-0:5 q)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, q}
```

### AST
```
(object 0:0-0:9 (prop (name 0:1-0:2 p) (var 0:4-0:5 a)) (prop (name 0:7-0:8 q)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p, q: b}
```

### AST
```
(object 0:0-0:9 (prop (name 0:1-0:2 p)) (prop (name 0:4-0:5 q) (var 0:7-0:8 b)))
```

--------------------------------------------------------------------------------

### Source
```ite
if {} {}
```

### AST
```
(if 0:0-0:5 (err 0:3-0:4) block)
(object 0:6-0:8)
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.

--------------------------------------------------------------------------------

### Source
```ite
if {p: a}.p {}
```

### AST
```
(prop 0:0-0:11
  (if 0:0-0:9 (err 0:3-0:4) (block (err (var 0:4-0:5 p)) (var 0:7-0:8 a)))
  (name 0:10-0:11 p))
(object 0:12-0:14)
```

### Errors
- (0:3-0:4) We wanted an expression but we found `{`.
- (0:5-0:6) We wanted an expression but we found `:`.

--------------------------------------------------------------------------------

### Source
```ite
{p: a}.p
```

### AST
```
(prop 0:0-0:8
  (object 0:0-0:6 (prop (name 0:1-0:2 p) (var 0:4-0:5 a)))
  (name 0:7-0:8 p))
```

--------------------------------------------------------------------------------

### Source
```ite
{😈 p: a}
```

### AST
```
(err (object 0:0-0:9 (prop (name 0:4-0:5 p) (var 0:7-0:8 a))))
```

### Errors
- (0:1-0:3) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
{p 😈 : a}
```

### AST
```
(object 0:0-0:10 (prop (name 0:1-0:2 p) (err (var 0:8-0:9 a))))
```

### Errors
- (0:3-0:5) We wanted `:` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
{p: 😈 a}
```

### AST
```
(object 0:0-0:9 (prop (name 0:1-0:2 p) (err (var 0:7-0:8 a))))
```

### Errors
- (0:4-0:6) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
{p: a 😈}
```

### AST
```
(object 0:0-0:9 (prop (name 0:1-0:2 p) (err (var 0:4-0:5 a))))
```

### Errors
- (0:6-0:8) We wanted an expression but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
{😈}
```

### AST
```
(err (object 0:0-0:4))
```

### Errors
- (0:1-0:3) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
a && b
```

### AST
```
(and 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b && c
```

### AST
```
(and 0:0-0:11 (and 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b || c
```

### AST
```
(or 0:0-0:11 (or 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b || c
```

### AST
```
(or 0:0-0:11 (and 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b && c
```

### AST
```
(or 0:0-0:11 (var 0:0-0:1 a) (and 0:5-0:11 (var 0:5-0:6 b) (var 0:10-0:11 c)))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b && c && d
```

### AST
```
(and 0:0-0:16
  (and 0:0-0:11 (and 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
  (var 0:15-0:16 d))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b && c && d
```

### AST
```
(or 0:0-0:16
  (var 0:0-0:1 a)
  (and 0:5-0:16
    (and 0:5-0:11 (var 0:5-0:6 b) (var 0:10-0:11 c))
    (var 0:15-0:16 d)))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b || c && d
```

### AST
```
(or 0:0-0:16
  (and 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b))
  (and 0:10-0:16 (var 0:10-0:11 c) (var 0:15-0:16 d)))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b && c || d
```

### AST
```
(or 0:0-0:16
  (and 0:0-0:11 (and 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
  (var 0:15-0:16 d))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b || c || d
```

### AST
```
(or 0:0-0:16
  (or 0:0-0:11 (and 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
  (var 0:15-0:16 d))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b && c || d
```

### AST
```
(or 0:0-0:16
  (var 0:0-0:1 a)
  (or 0:5-0:16
    (and 0:5-0:11 (var 0:5-0:6 b) (var 0:10-0:11 c))
    (var 0:15-0:16 d)))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b || c && d
```

### AST
```
(or 0:0-0:16
  (or 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b))
  (and 0:10-0:16 (var 0:10-0:11 c) (var 0:15-0:16 d)))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b || c || d
```

### AST
```
(or 0:0-0:16
  (or 0:0-0:11 (or 0:0-0:6 (var 0:0-0:1 a) (var 0:5-0:6 b)) (var 0:10-0:11 c))
  (var 0:15-0:16 d))
```

--------------------------------------------------------------------------------

### Source
```ite
let true = x
```

### AST
```
(bind (bool 0:4-0:8 true) (var 0:11-0:12 x))
```

--------------------------------------------------------------------------------

### Source
```ite
let {} = o
```

### AST
```
(bind (object 0:4-0:6) (var 0:9-0:10 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a} = o
```

### AST
```
(bind (object 0:4-0:7 (prop (name 0:5-0:6 a))) (var 0:10-0:11 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b} = o
```

### AST
```
(bind
  (object 0:4-0:10 (prop (name 0:5-0:6 a)) (prop (name 0:8-0:9 b)))
  (var 0:13-0:14 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b, c} = o
```

### AST
```
(bind
  (object 0:4-0:13
    (prop (name 0:5-0:6 a))
    (prop (name 0:8-0:9 b))
    (prop (name 0:11-0:12 c)))
  (var 0:16-0:17 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {,} = o
```

### AST
```
(bind (err (object 0:4-0:7)) (var 0:10-0:11 o))
```

### Errors
- (0:5-0:6) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
let {a,} = o
```

### AST
```
(bind (object 0:4-0:8 (prop (name 0:5-0:6 a))) (var 0:11-0:12 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b,} = o
```

### AST
```
(bind
  (object 0:4-0:11 (prop (name 0:5-0:6 a)) (prop (name 0:8-0:9 b)))
  (var 0:14-0:15 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b, c,} = o
```

### AST
```
(bind
  (object 0:4-0:14
    (prop (name 0:5-0:6 a))
    (prop (name 0:8-0:9 b))
    (prop (name 0:11-0:12 c)))
  (var 0:17-0:18 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a: a2} = o
```

### AST
```
(bind
  (object 0:4-0:11 (prop (name 0:5-0:6 a) (var 0:8-0:10 a2)))
  (var 0:14-0:15 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a: a2, b: b2} = o
```

### AST
```
(bind
  (object 0:4-0:18
    (prop (name 0:5-0:6 a) (var 0:8-0:10 a2))
    (prop (name 0:12-0:13 b) (var 0:15-0:17 b2)))
  (var 0:21-0:22 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a: a2, b: b2, c: c2} = o
```

### AST
```
(bind
  (object 0:4-0:25
    (prop (name 0:5-0:6 a) (var 0:8-0:10 a2))
    (prop (name 0:12-0:13 b) (var 0:15-0:17 b2))
    (prop (name 0:19-0:20 c) (var 0:22-0:24 c2)))
  (var 0:28-0:29 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a: a2, b} = o
```

### AST
```
(bind
  (object 0:4-0:14
    (prop (name 0:5-0:6 a) (var 0:8-0:10 a2))
    (prop (name 0:12-0:13 b)))
  (var 0:17-0:18 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b: b2} = o
```

### AST
```
(bind
  (object 0:4-0:14
    (prop (name 0:5-0:6 a))
    (prop (name 0:8-0:9 b) (var 0:11-0:13 b2)))
  (var 0:17-0:18 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {| o} = o
```

### AST
```
(bind (object 0:4-0:9 (var 0:7-0:8 o)) (var 0:12-0:13 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a | o} = o
```

### AST
```
(bind
  (object 0:4-0:11 (prop (name 0:5-0:6 a)) (var 0:9-0:10 o))
  (var 0:14-0:15 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, | o} = o
```

### AST
```
(bind
  (object 0:4-0:12 (prop (name 0:5-0:6 a)) (var 0:10-0:11 o))
  (var 0:15-0:16 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b | o} = o
```

### AST
```
(bind
  (object 0:4-0:14
    (prop (name 0:5-0:6 a))
    (prop (name 0:8-0:9 b))
    (var 0:12-0:13 o))
  (var 0:17-0:18 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b, c | o} = o
```

### AST
```
(bind
  (object 0:4-0:17
    (prop (name 0:5-0:6 a))
    (prop (name 0:8-0:9 b))
    (prop (name 0:11-0:12 c))
    (var 0:15-0:16 o))
  (var 0:20-0:21 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a | {b | o}} = o
```

### AST
```
(bind
  (object 0:4-0:17
    (prop (name 0:5-0:6 a))
    (object 0:9-0:16 (prop (name 0:10-0:11 b)) (var 0:14-0:15 o)))
  (var 0:20-0:21 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a | {b | {c | o}}} = o
```

### AST
```
(bind
  (object 0:4-0:23
    (prop (name 0:5-0:6 a))
    (object 0:9-0:22
      (prop (name 0:10-0:11 b))
      (object 0:14-0:21 (prop (name 0:15-0:16 c)) (var 0:19-0:20 o))))
  (var 0:26-0:27 o))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a | {b | {c | {}}}} = o
```

### AST
```
(bind
  (object 0:4-0:24
    (prop (name 0:5-0:6 a))
    (object 0:9-0:23
      (prop (name 0:10-0:11 b))
      (object 0:14-0:22 (prop (name 0:15-0:16 c)) (object 0:19-0:21))))
  (var 0:27-0:28 o))
```

--------------------------------------------------------------------------------

### Source
```ite
{a: {b: c}}
```

### AST
```
(object 0:0-0:11
  (prop
    (name 0:1-0:2 a)
    (object 0:4-0:10 (prop (name 0:5-0:6 b) (var 0:8-0:9 c)))))
```

--------------------------------------------------------------------------------

### Source
```ite
{a {}}
```

### AST
```
(err (object 0:0-0:2 (prop (name 0:1-0:2 a))))
(err (object 0:3-0:5))
```

### Errors
- (0:3-0:4) We wanted `}` but we found `{`.
- (0:5-0:6) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
{a true}
```

### AST
```
(err (object 0:0-0:2 (prop (name 0:1-0:2 a))))
(err (bool 0:3-0:7 true))
```

### Errors
- (0:3-0:7) We wanted `}` but we found `true`.
- (0:7-0:8) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
let x: T = y;
```

### AST
```
(bind (var 0:4-0:5 x) (type (var 0:7-0:8 T)) (var 0:11-0:12 y))
```

--------------------------------------------------------------------------------

### Source
```ite
let x: = y;
```

### AST
```
(bind (var 0:4-0:5 x) (type (err 0:7-0:8)) (var 0:9-0:10 y))
```

### Errors
- (0:7-0:8) We wanted a type but we found `=`.

--------------------------------------------------------------------------------

### Source
```ite
let x T = y;
```

### AST
```
(err (bind (var 0:4-0:5 x) (err (var 0:6-0:7 T))))
(var 0:10-0:11 y)
```

### Errors
- (0:6-0:7) We wanted `=` but we found a variable name.
- (0:8-0:9) We wanted an expression but we found `=`.

--------------------------------------------------------------------------------

### Source
```ite
(x: T)
```

### AST
```
(wrap 0:0-0:6 (var 0:1-0:2 x) (type (var 0:4-0:5 T)))
```

--------------------------------------------------------------------------------

### Source
```ite
(x:)
```

### AST
```
(wrap 0:0-0:4 (var 0:1-0:2 x) (type (err 0:3-0:4)))
```

### Errors
- (0:3-0:4) We wanted a type but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
(x T)
```

### AST
```
(err (wrap 0:0-0:2 (var 0:1-0:2 x)))
(err (var 0:3-0:4 T))
```

### Errors
- (0:3-0:4) We wanted `)` but we found a variable name.
- (0:4-0:5) We wanted an expression but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
fun f g() {}
```

### AST
```
(err (fun 0:0-0:12 (name 0:4-0:5 f) block))
```

### Errors
- (0:6-0:7) We wanted `(` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
fun f(a: T) {}
```

### AST
```
(fun 0:0-0:14
  (name 0:4-0:5 f)
  (param (var 0:6-0:7 a) (type (var 0:9-0:10 T)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a: T, b: U) {}
```

### AST
```
(fun 0:0-0:20
  (name 0:4-0:5 f)
  (param (var 0:6-0:7 a) (type (var 0:9-0:10 T)))
  (param (var 0:12-0:13 b) (type (var 0:15-0:16 U)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b: U) {}
```

### AST
```
(fun 0:0-0:17
  (name 0:4-0:5 f)
  (param (var 0:6-0:7 a))
  (param (var 0:9-0:10 b) (type (var 0:12-0:13 U)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a: T, b) {}
```

### AST
```
(fun 0:0-0:17
  (name 0:4-0:5 f)
  (param (var 0:6-0:7 a) (type (var 0:9-0:10 T)))
  (param (var 0:12-0:13 b))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a: T, b: U, c: V) {}
```

### AST
```
(fun 0:0-0:26
  (name 0:4-0:5 f)
  (param (var 0:6-0:7 a) (type (var 0:9-0:10 T)))
  (param (var 0:12-0:13 b) (type (var 0:15-0:16 U)))
  (param (var 0:18-0:19 c) (type (var 0:21-0:22 V)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a: T) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:20 (param (var 0:12-0:13 a) (type (var 0:15-0:16 T))) block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a: T, b: U) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:26
    (param (var 0:12-0:13 a) (type (var 0:15-0:16 T)))
    (param (var 0:18-0:19 b) (type (var 0:21-0:22 U)))
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b: U) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:23
    (param (var 0:12-0:13 a))
    (param (var 0:15-0:16 b) (type (var 0:18-0:19 U)))
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a: T, b) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:23
    (param (var 0:12-0:13 a) (type (var 0:15-0:16 T)))
    (param (var 0:18-0:19 b))
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a: T, b: U, c: V) {};
```

### AST
```
(bind
  (var 0:4-0:5 f)
  (fun 0:8-0:32
    (param (var 0:12-0:13 a) (type (var 0:15-0:16 T)))
    (param (var 0:18-0:19 b) (type (var 0:21-0:22 U)))
    (param (var 0:24-0:25 c) (type (var 0:27-0:28 V)))
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() -> T {}
```

### AST
```
(fun 0:0-0:15 (name 0:4-0:5 f) (type (var 0:11-0:12 T)) block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() -> {}
```

### AST
```
(err (fun 0:0-0:13 (name 0:4-0:5 f) (type (object 0:11-0:13)) block))
```

### Errors
- (0:13-0:13) We wanted `{` but the file ended.
- (0:13-0:13) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
fun f() T {}
```

### AST
```
(err (fun 0:0-0:12 (name 0:4-0:5 f) (block (var 0:8-0:9 T) (object 0:10-0:12))))
```

### Errors
- (0:8-0:9) We wanted `{` but we found a variable name.
- (0:12-0:12) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
fun f() -> {} {}
```

### AST
```
(fun 0:0-0:16 (name 0:4-0:5 f) (type (object 0:11-0:13)) block)
```

--------------------------------------------------------------------------------

### Source
```ite
(x: !)
```

### AST
```
(wrap 0:0-0:6 (var 0:1-0:2 x) (type (bottom 0:4-0:5)))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <> X)
```

### AST
```
(wrap 0:0-0:9 (var 0:1-0:2 x) (type (quantify 0:4-0:8 (var 0:7-0:8 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T> X)
```

### AST
```
(wrap 0:0-0:10
  (var 0:1-0:2 x)
  (type (quantify 0:4-0:9 (forall (name 0:5-0:6 T)) (var 0:8-0:9 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T: A> X)
```

### AST
```
(wrap 0:0-0:13
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:12
      (forall (name 0:5-0:6 T) flex (var 0:8-0:9 A))
      (var 0:11-0:12 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T = A> X)
```

### AST
```
(wrap 0:0-0:14
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:13
      (forall (name 0:5-0:6 T) rigid (var 0:9-0:10 A))
      (var 0:12-0:13 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T, U> X)
```

### AST
```
(wrap 0:0-0:13
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:12
      (forall (name 0:5-0:6 T))
      (forall (name 0:8-0:9 U))
      (var 0:11-0:12 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T: A, U: B> X)
```

### AST
```
(wrap 0:0-0:19
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:18
      (forall (name 0:5-0:6 T) flex (var 0:8-0:9 A))
      (forall (name 0:11-0:12 U) flex (var 0:14-0:15 B))
      (var 0:17-0:18 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T = A, U = B> X)
```

### AST
```
(wrap 0:0-0:21
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:20
      (forall (name 0:5-0:6 T) rigid (var 0:9-0:10 A))
      (forall (name 0:12-0:13 U) rigid (var 0:16-0:17 B))
      (var 0:19-0:20 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T = A, U: B> X)
```

### AST
```
(wrap 0:0-0:20
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:19
      (forall (name 0:5-0:6 T) rigid (var 0:9-0:10 A))
      (forall (name 0:12-0:13 U) flex (var 0:15-0:16 B))
      (var 0:18-0:19 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T: A, U = B> X)
```

### AST
```
(wrap 0:0-0:20
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:19
      (forall (name 0:5-0:6 T) flex (var 0:8-0:9 A))
      (forall (name 0:11-0:12 U) rigid (var 0:15-0:16 B))
      (var 0:18-0:19 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T, U: B> X)
```

### AST
```
(wrap 0:0-0:16
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:15
      (forall (name 0:5-0:6 T))
      (forall (name 0:8-0:9 U) flex (var 0:11-0:12 B))
      (var 0:14-0:15 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T: A, U> X)
```

### AST
```
(wrap 0:0-0:16
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:15
      (forall (name 0:5-0:6 T) flex (var 0:8-0:9 A))
      (forall (name 0:11-0:12 U))
      (var 0:14-0:15 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T, U = B> X)
```

### AST
```
(wrap 0:0-0:17
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:16
      (forall (name 0:5-0:6 T))
      (forall (name 0:8-0:9 U) rigid (var 0:12-0:13 B))
      (var 0:15-0:16 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T = A, U> X)
```

### AST
```
(wrap 0:0-0:17
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:16
      (forall (name 0:5-0:6 T) rigid (var 0:9-0:10 A))
      (forall (name 0:12-0:13 U))
      (var 0:15-0:16 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <,> X)
```

### AST
```
(wrap 0:0-0:10 (var 0:1-0:2 x) (type (err (quantify 0:4-0:9 (var 0:8-0:9 X)))))
```

### Errors
- (0:5-0:6) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
(x: <T,> X)
```

### AST
```
(wrap 0:0-0:11
  (var 0:1-0:2 x)
  (type (quantify 0:4-0:10 (forall (name 0:5-0:6 T)) (var 0:9-0:10 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T, U,> X)
```

### AST
```
(wrap 0:0-0:14
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:13
      (forall (name 0:5-0:6 T))
      (forall (name 0:8-0:9 U))
      (var 0:12-0:13 X))))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<>() {}
```

### AST
```
(fun 0:0-0:12 (name 0:4-0:5 f) block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T>() {}
```

### AST
```
(fun 0:0-0:13 (name 0:4-0:5 f) (forall (name 0:6-0:7 T)) block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T: A>() {}
```

### AST
```
(fun 0:0-0:16
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T) flex (var 0:9-0:10 A))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T = A>() {}
```

### AST
```
(fun 0:0-0:17
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T) rigid (var 0:10-0:11 A))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T, U>() {}
```

### AST
```
(fun 0:0-0:16
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T))
  (forall (name 0:9-0:10 U))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T: A, U: B>() {}
```

### AST
```
(fun 0:0-0:22
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T) flex (var 0:9-0:10 A))
  (forall (name 0:12-0:13 U) flex (var 0:15-0:16 B))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T = A, U = B>() {}
```

### AST
```
(fun 0:0-0:24
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T) rigid (var 0:10-0:11 A))
  (forall (name 0:13-0:14 U) rigid (var 0:17-0:18 B))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T = A, U: B>() {}
```

### AST
```
(fun 0:0-0:23
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T) rigid (var 0:10-0:11 A))
  (forall (name 0:13-0:14 U) flex (var 0:16-0:17 B))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T: A, U = B>() {}
```

### AST
```
(fun 0:0-0:23
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T) flex (var 0:9-0:10 A))
  (forall (name 0:12-0:13 U) rigid (var 0:16-0:17 B))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T, U: B>() {}
```

### AST
```
(fun 0:0-0:19
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T))
  (forall (name 0:9-0:10 U) flex (var 0:12-0:13 B))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T: A, U>() {}
```

### AST
```
(fun 0:0-0:19
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T) flex (var 0:9-0:10 A))
  (forall (name 0:12-0:13 U))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T, U = B>() {}
```

### AST
```
(fun 0:0-0:20
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T))
  (forall (name 0:9-0:10 U) rigid (var 0:13-0:14 B))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T = A, U>() {}
```

### AST
```
(fun 0:0-0:20
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T) rigid (var 0:10-0:11 A))
  (forall (name 0:13-0:14 U))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<,>() {}
```

### AST
```
(err (fun 0:0-0:13 (name 0:4-0:5 f) block))
```

### Errors
- (0:6-0:7) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
fun f<T,>() {}
```

### AST
```
(fun 0:0-0:14 (name 0:4-0:5 f) (forall (name 0:6-0:7 T)) block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T, U,>() {}
```

### AST
```
(fun 0:0-0:17
  (name 0:4-0:5 f)
  (forall (name 0:6-0:7 T))
  (forall (name 0:9-0:10 U))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {})
```

### AST
```
(wrap 0:0-0:7 (var 0:1-0:2 x) (type (object 0:4-0:6)))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T})
```

### AST
```
(wrap 0:0-0:11
  (var 0:1-0:2 x)
  (type (object 0:4-0:10 (prop (name 0:5-0:6 a) (var 0:8-0:9 T)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, b: U})
```

### AST
```
(wrap 0:0-0:17
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:16
      (prop (name 0:5-0:6 a) (var 0:8-0:9 T))
      (prop (name 0:11-0:12 b) (var 0:14-0:15 U)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {,})
```

### AST
```
(wrap 0:0-0:8 (var 0:1-0:2 x) (type (err (object 0:4-0:7))))
```

### Errors
- (0:5-0:6) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T,})
```

### AST
```
(wrap 0:0-0:12
  (var 0:1-0:2 x)
  (type (object 0:4-0:11 (prop (name 0:5-0:6 a) (var 0:8-0:9 T)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, b: U,})
```

### AST
```
(wrap 0:0-0:18
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:17
      (prop (name 0:5-0:6 a) (var 0:8-0:9 T))
      (prop (name 0:11-0:12 b) (var 0:14-0:15 U)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a})
```

### AST
```
(wrap 0:0-0:8
  (var 0:1-0:2 x)
  (type (object 0:4-0:7 (prop (name 0:5-0:6 a) (err 0:6-0:7)))))
```

### Errors
- (0:6-0:7) We wanted `:` but we found `}`.
- (0:6-0:7) We wanted a type but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
(x: {a, b})
```

### AST
```
(wrap 0:0-0:11
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:10
      (prop (name 0:5-0:6 a) (err 0:6-0:7))
      (prop (name 0:8-0:9 b) (err 0:9-0:10)))))
```

### Errors
- (0:6-0:7) We wanted `:` but we found `,`.
- (0:6-0:7) We wanted a type but we found `,`.
- (0:9-0:10) We wanted `:` but we found `}`.
- (0:9-0:10) We wanted a type but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, b})
```

### AST
```
(wrap 0:0-0:14
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:13
      (prop (name 0:5-0:6 a) (var 0:8-0:9 T))
      (prop (name 0:11-0:12 b) (err 0:12-0:13)))))
```

### Errors
- (0:12-0:13) We wanted `:` but we found `}`.
- (0:12-0:13) We wanted a type but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
(x: {a, b: U})
```

### AST
```
(wrap 0:0-0:14
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:13
      (prop (name 0:5-0:6 a) (err 0:6-0:7))
      (prop (name 0:8-0:9 b) (var 0:11-0:12 U)))))
```

### Errors
- (0:6-0:7) We wanted `:` but we found `,`.
- (0:6-0:7) We wanted a type but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
(x: {| O})
```

### AST
```
(wrap 0:0-0:10 (var 0:1-0:2 x) (type (object 0:4-0:9 (var 0:7-0:8 O))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T | O})
```

### AST
```
(wrap 0:0-0:15
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:14
      (prop (name 0:5-0:6 a) (var 0:8-0:9 T))
      (var 0:12-0:13 O))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, | O})
```

### AST
```
(wrap 0:0-0:16
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:15
      (prop (name 0:5-0:6 a) (var 0:8-0:9 T))
      (var 0:13-0:14 O))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, b: U | O})
```

### AST
```
(wrap 0:0-0:21
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:20
      (prop (name 0:5-0:6 a) (var 0:8-0:9 T))
      (prop (name 0:11-0:12 b) (var 0:14-0:15 U))
      (var 0:18-0:19 O))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, b: U, | O})
```

### AST
```
(wrap 0:0-0:22
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:21
      (prop (name 0:5-0:6 a) (var 0:8-0:9 T))
      (prop (name 0:11-0:12 b) (var 0:14-0:15 U))
      (var 0:19-0:20 O))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T | {b: U | O}})
```

### AST
```
(wrap 0:0-0:24
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:23
      (prop (name 0:5-0:6 a) (var 0:8-0:9 T))
      (object 0:12-0:22
        (prop (name 0:13-0:14 b) (var 0:16-0:17 U))
        (var 0:20-0:21 O)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T | {b: U | {}}})
```

### AST
```
(wrap 0:0-0:25
  (var 0:1-0:2 x)
  (type
    (object 0:4-0:24
      (prop (name 0:5-0:6 a) (var 0:8-0:9 T))
      (object 0:12-0:23
        (prop (name 0:13-0:14 b) (var 0:16-0:17 U))
        (object 0:20-0:22)))))
```

--------------------------------------------------------------------------------

### Source
```ite
x
.Foo
```

### AST
```
(prop 0:0-1:4 (var 0:0-0:1 x) (name 1:1-1:4 Foo))
```

--------------------------------------------------------------------------------

### Source
```ite
x;
.Foo;
```

### AST
```
(var 0:0-0:1 x)
(err 1:0-1:1)
(var 1:1-1:4 Foo)
```

### Errors
- (1:0-1:1) We wanted a statement but we found `.`.

--------------------------------------------------------------------------------

### Source
```ite
(x: fun)
```

### AST
```
(err (wrap 0:0-0:2 (var 0:1-0:2 x) (type (err (fun 0:4-0:8 (err 0:8-0:8))))))
```

### Errors
- (0:7-0:8) We wanted `(` but we found `)`.
- (0:8-0:8) We wanted `->` but the file ended.
- (0:8-0:8) We wanted a type but the file ended.
- (0:8-0:8) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
(x: fun😈)
```

### AST
```
(err (wrap 0:0-0:2 (var 0:1-0:2 x) (type (err (fun 0:4-0:10 (err 0:10-0:10))))))
```

### Errors
- (0:7-0:9) We wanted `(` but we found `😈`.
- (0:9-0:10) We wanted `(` but we found `)`.
- (0:10-0:10) We wanted `->` but the file ended.
- (0:10-0:10) We wanted a type but the file ended.
- (0:10-0:10) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
(x: fun())
```

### AST
```
(wrap 0:0-0:10 (var 0:1-0:2 x) (type (err (fun 0:4-0:10 (err 0:9-0:10)))))
```

### Errors
- (0:9-0:10) We wanted `->` but we found `)`.
- (0:9-0:10) We wanted a type but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
(x: fun() ->)
```

### AST
```
(wrap 0:0-0:13 (var 0:1-0:2 x) (type (fun 0:4-0:13 (err 0:12-0:13))))
```

### Errors
- (0:12-0:13) We wanted a type but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
(x: fun() -> A)
```

### AST
```
(wrap 0:0-0:15 (var 0:1-0:2 x) (type (fun 0:4-0:14 (var 0:13-0:14 A))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A) -> B)
```

### AST
```
(wrap 0:0-0:16
  (var 0:1-0:2 x)
  (type (fun 0:4-0:15 (param (var 0:8-0:9 A)) (var 0:14-0:15 B))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A, B) -> C)
```

### AST
```
(wrap 0:0-0:19
  (var 0:1-0:2 x)
  (type
    (fun 0:4-0:18
      (param (var 0:8-0:9 A))
      (param (var 0:11-0:12 B))
      (var 0:17-0:18 C))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A, B, C) -> D)
```

### AST
```
(wrap 0:0-0:22
  (var 0:1-0:2 x)
  (type
    (fun 0:4-0:21
      (param (var 0:8-0:9 A))
      (param (var 0:11-0:12 B))
      (param (var 0:14-0:15 C))
      (var 0:20-0:21 D))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(,) -> A)
```

### AST
```
(wrap 0:0-0:16
  (var 0:1-0:2 x)
  (type (fun 0:4-0:15 (param (err 0:8-0:9)) (var 0:14-0:15 A))))
```

### Errors
- (0:8-0:9) We wanted a type but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A,) -> B)
```

### AST
```
(wrap 0:0-0:17
  (var 0:1-0:2 x)
  (type (fun 0:4-0:16 (param (var 0:8-0:9 A)) (var 0:15-0:16 B))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A, B,) -> C)
```

### AST
```
(wrap 0:0-0:20
  (var 0:1-0:2 x)
  (type
    (fun 0:4-0:19
      (param (var 0:8-0:9 A))
      (param (var 0:11-0:12 B))
      (var 0:18-0:19 C))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A, B, C,) -> D)
```

### AST
```
(wrap 0:0-0:23
  (var 0:1-0:2 x)
  (type
    (fun 0:4-0:22
      (param (var 0:8-0:9 A))
      (param (var 0:11-0:12 B))
      (param (var 0:14-0:15 C))
      (var 0:21-0:22 D))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<>() -> A)
```

### AST
```
(wrap 0:0-0:17 (var 0:1-0:2 x) (type (fun 0:4-0:16 (var 0:15-0:16 A))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A>() -> B)
```

### AST
```
(wrap 0:0-0:18
  (var 0:1-0:2 x)
  (type (fun 0:4-0:17 (forall (name 0:8-0:9 A)) (var 0:16-0:17 B))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B>() -> C)
```

### AST
```
(wrap 0:0-0:21
  (var 0:1-0:2 x)
  (type
    (fun 0:4-0:20
      (forall (name 0:8-0:9 A))
      (forall (name 0:11-0:12 B))
      (var 0:19-0:20 C))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B, C>() -> D)
```

### AST
```
(wrap 0:0-0:24
  (var 0:1-0:2 x)
  (type
    (fun 0:4-0:23
      (forall (name 0:8-0:9 A))
      (forall (name 0:11-0:12 B))
      (forall (name 0:14-0:15 C))
      (var 0:22-0:23 D))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<,>() -> A)
```

### AST
```
(wrap 0:0-0:18 (var 0:1-0:2 x) (type (err (fun 0:4-0:17 (var 0:16-0:17 A)))))
```

### Errors
- (0:8-0:9) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A,>() -> B)
```

### AST
```
(wrap 0:0-0:19
  (var 0:1-0:2 x)
  (type (fun 0:4-0:18 (forall (name 0:8-0:9 A)) (var 0:17-0:18 B))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B,>() -> C)
```

### AST
```
(wrap 0:0-0:22
  (var 0:1-0:2 x)
  (type
    (fun 0:4-0:21
      (forall (name 0:8-0:9 A))
      (forall (name 0:11-0:12 B))
      (var 0:20-0:21 C))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B, C,>() -> D)
```

### AST
```
(wrap 0:0-0:25
  (var 0:1-0:2 x)
  (type
    (fun 0:4-0:24
      (forall (name 0:8-0:9 A))
      (forall (name 0:11-0:12 B))
      (forall (name 0:14-0:15 C))
      (var 0:23-0:24 D))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B>(C, D) -> E)
```

### AST
```
(wrap 0:0-0:25
  (var 0:1-0:2 x)
  (type
    (fun 0:4-0:24
      (forall (name 0:8-0:9 A))
      (forall (name 0:11-0:12 B))
      (param (var 0:14-0:15 C))
      (param (var 0:17-0:18 D))
      (var 0:23-0:24 E))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <A, B> fun<C, D>(E, F) -> G)
```

### AST
```
(wrap 0:0-0:32
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:31
      (forall (name 0:5-0:6 A))
      (forall (name 0:8-0:9 B))
      (fun 0:11-0:31
        (forall (name 0:15-0:16 C))
        (forall (name 0:18-0:19 D))
        (param (var 0:21-0:22 E))
        (param (var 0:24-0:25 F))
        (var 0:30-0:31 G)))))
```

--------------------------------------------------------------------------------

### Source
```ite
let (x) = y;
```

### AST
```
(bind (wrap 0:4-0:7 (var 0:5-0:6 x)) (var 0:10-0:11 y))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: (T));
```

### AST
```
(wrap 0:0-0:8 (var 0:1-0:2 x) (type (wrap 0:4-0:7 (var 0:5-0:6 T))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <A> (<B> T));
```

### AST
```
(wrap 0:0-0:16
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:15
      (forall (name 0:5-0:6 A))
      (wrap 0:8-0:15
        (quantify 0:9-0:14 (forall (name 0:10-0:11 B)) (var 0:13-0:14 T))))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <A> <B> T);
```

### AST
```
(wrap 0:0-0:14
  (var 0:1-0:2 x)
  (type
    (quantify 0:4-0:13
      (forall (name 0:5-0:6 A))
      (quantify 0:8-0:13 (forall (name 0:9-0:10 B)) (var 0:12-0:13 T)))))
```

--------------------------------------------------------------------------------

### Source
```ite
/*
```

### AST
```
```

### Errors
- (0:2-0:2) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/*/
```

### AST
```
```

### Errors
- (0:3-0:3) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/**/
```

### AST
```
```

--------------------------------------------------------------------------------

### Source
```ite
/* *
```

### AST
```
```

### Errors
- (0:4-0:4) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/* **
```

### AST
```
```

### Errors
- (0:5-0:5) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/* * /
```

### AST
```
```

### Errors
- (0:6-0:6) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/* */
```

### AST
```
```

--------------------------------------------------------------------------------

### Source
```ite
/* **/
```

### AST
```
```

--------------------------------------------------------------------------------

### Source
```ite
/* x
```

### AST
```
```

### Errors
- (0:4-0:4) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/*/ x
```

### AST
```
```

### Errors
- (0:5-0:5) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/**/ x
```

### AST
```
(var 0:5-0:6 x)
```

--------------------------------------------------------------------------------

### Source
```ite
/* * x
```

### AST
```
```

### Errors
- (0:6-0:6) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/* ** x
```

### AST
```
```

### Errors
- (0:7-0:7) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/* * / x
```

### AST
```
```

### Errors
- (0:8-0:8) We wanted `*/` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
/* */ x
```

### AST
```
(var 0:6-0:7 x)
```

--------------------------------------------------------------------------------

### Source
```ite
/* **/ x
```

### AST
```
(var 0:7-0:8 x)
```

--------------------------------------------------------------------------------

### Source
```ite
a + b + c + (d * e)
```

### AST
```
(add 0:0-0:19
  (add 0:0-0:9 (add 0:0-0:5 (var 0:0-0:1 a) (var 0:4-0:5 b)) (var 0:8-0:9 c))
  (wrap 0:12-0:19 (mul 0:13-0:18 (var 0:13-0:14 d) (var 0:17-0:18 e))))
```

--------------------------------------------------------------------------------

### Source
```ite
f(
```

### AST
```
(err (call 0:0-0:2 (var 0:0-0:1 f)))
```

### Errors
- (0:2-0:2) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
f(a
```

### AST
```
(err (call 0:0-0:3 (var 0:0-0:1 f) (var 0:2-0:3 a)))
```

### Errors
- (0:3-0:3) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
f(a,
```

### AST
```
(err (call 0:0-0:3 (var 0:0-0:1 f) (var 0:2-0:3 a)))
```

### Errors
- (0:4-0:4) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b
```

### AST
```
(err (call 0:0-0:6 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b)))
```

### Errors
- (0:6-0:6) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
f(a, b,
```

### AST
```
(err (call 0:0-0:6 (var 0:0-0:1 f) (var 0:2-0:3 a) (var 0:5-0:6 b)))
```

### Errors
- (0:7-0:7) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
{
```

### AST
```
(err (object 0:0-0:1))
```

### Errors
- (0:1-0:1) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
{|o
```

### AST
```
(err (object 0:0-0:3 (var 0:2-0:3 o)))
```

### Errors
- (0:3-0:3) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
{a: b
```

### AST
```
(err (object 0:0-0:5 (prop (name 0:1-0:2 a) (var 0:4-0:5 b))))
```

### Errors
- (0:5-0:5) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
{a
```

### AST
```
(err (object 0:0-0:2 (prop (name 0:1-0:2 a))))
```

### Errors
- (0:2-0:2) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
{a: b,
```

### AST
```
(err (object 0:0-0:5 (prop (name 0:1-0:2 a) (var 0:4-0:5 b))))
```

### Errors
- (0:6-0:6) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
{a,
```

### AST
```
(err (object 0:0-0:2 (prop (name 0:1-0:2 a))))
```

### Errors
- (0:3-0:3) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let {
```

### AST
```
(err (bind (err (object 0:4-0:5)) (err 0:5-0:5)))
```

### Errors
- (0:5-0:5) We wanted `}` but the file ended.
- (0:5-0:5) We wanted `=` but the file ended.
- (0:5-0:5) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let {|o
```

### AST
```
(err (bind (err (object 0:4-0:7 (var 0:6-0:7 o))) (err 0:7-0:7)))
```

### Errors
- (0:7-0:7) We wanted `}` but the file ended.
- (0:7-0:7) We wanted `=` but the file ended.
- (0:7-0:7) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let {a: b
```

### AST
```
(err
  (bind
    (err (object 0:4-0:9 (prop (name 0:5-0:6 a) (var 0:8-0:9 b))))
    (err 0:9-0:9)))
```

### Errors
- (0:9-0:9) We wanted `}` but the file ended.
- (0:9-0:9) We wanted `=` but the file ended.
- (0:9-0:9) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let {a
```

### AST
```
(err (bind (err (object 0:4-0:6 (prop (name 0:5-0:6 a)))) (err 0:6-0:6)))
```

### Errors
- (0:6-0:6) We wanted `}` but the file ended.
- (0:6-0:6) We wanted `=` but the file ended.
- (0:6-0:6) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let {a: b,
```

### AST
```
(err
  (bind
    (err (object 0:4-0:9 (prop (name 0:5-0:6 a) (var 0:8-0:9 b))))
    (err 0:10-0:10)))
```

### Errors
- (0:10-0:10) We wanted `}` but the file ended.
- (0:10-0:10) We wanted `=` but the file ended.
- (0:10-0:10) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let {a,
```

### AST
```
(err (bind (err (object 0:4-0:6 (prop (name 0:5-0:6 a)))) (err 0:7-0:7)))
```

### Errors
- (0:7-0:7) We wanted `}` but the file ended.
- (0:7-0:7) We wanted `=` but the file ended.
- (0:7-0:7) We wanted an expression but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
let a = b; 😈 let c = d;
```

### AST
```
(bind (var 0:4-0:5 a) (var 0:8-0:9 b))
(err 0:11-0:13)
(bind (var 0:18-0:19 c) (var 0:22-0:23 d))
```

### Errors
- (0:11-0:13) We wanted a statement but we found `😈`.

--------------------------------------------------------------------------------
