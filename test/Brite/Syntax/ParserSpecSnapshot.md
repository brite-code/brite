# ParserSpecSnapshot

--------------------------------------------------------------------------------

### Source
```ite
let x = y;
```

### AST
```
(bind (var `x`) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y
```

### AST
```
(bind (var `x`) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
let
```

### AST
```
(bind err err)
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
(bind (var `x`) err)
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
(bind err err)
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
(bind (var `y`) err)
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
(bind err err)
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
(bind (var `x`) err)
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) err)
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
(bind err (var `y`))
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
(bind err err)
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (type err) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
err
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (type err) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
err
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
(bind (var `x`) (var `y`))
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
(bind err (var `y`))
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
(bind (var `x`) (type err) (var `y`))
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
(bind (var `x`) err)
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
(bind (var `y`) err)
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
(bind err err)
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
(bind (var `x`) (type err) err)
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
(bind err err)
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
(bind err err)
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
(bind err (var `y`))
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
(bind (var `x`) (type err) (var `y`))
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
(bind (var `x`) err)
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
(bind (var `y`) err)
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
(bind err err)
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
(bind (var `x`) (type err) err)
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
(bind err err)
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
(bind err err)
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
(var `x`)
```

--------------------------------------------------------------------------------

### Source
```ite
x;
```

### AST
```
(var `x`)
```

--------------------------------------------------------------------------------

### Source
```ite
😈 x
```

### AST
```
(var `x`)
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
(var `x`)
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
(var `x`)
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
(var `x`)
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
(var `x`)
err
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
err
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
err
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
err
```

### Errors
- (0:0-0:1) We wanted a statement but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
;
```

### AST
```
err
```

### Errors
- (0:0-0:1) We wanted a statement but we found `;`.

--------------------------------------------------------------------------------

### Source
```ite
true
```

### AST
```
(bool true)
```

--------------------------------------------------------------------------------

### Source
```ite
false
```

### AST
```
(bool false)
```

--------------------------------------------------------------------------------

### Source
```ite
(
```

### AST
```
(wrap err)
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
(wrap (var `x`))
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
(wrap err)
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
(wrap (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
x)
```

### AST
```
(var `x`)
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
(wrap (var `x`))
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
(bind (var `x`) (wrap (var `y`)))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = (y;
```

### AST
```
(bind (var `x`) (wrap (var `y`)))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y; let x = y; let x = y;
```

### AST
```
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y; let x = y; let x = y; let x = y;
```

### AST
```
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y
```

### AST
```
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y let x = y
```

### AST
```
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y let x = y let x = y let x = y
```

### AST
```
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = y
let x = y
```

### AST
```
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
😈 let x = y; let x = y; let x = y;
```

### AST
```
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
err
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
err
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
(bind (var `x`) (var `y`))
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
(do block)
```

--------------------------------------------------------------------------------

### Source
```ite
do { 
```

### AST
```
(do block)
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
(do block)
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
(do block)
(do block)
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
(do block)
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
(do (block
  (do block)))
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
(do (block
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; 
```

### AST
```
(do (block
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))))
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
(bind (var `x`) (wrap (do block)))
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
(bind (var `x`) (wrap (do (block
  (bind (var `y`) (var `z`))))))
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
(bind (var `x`) (wrap (do block)))
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
(bind (var `x`) (wrap (do (block
  (bind (var `y`) (var `z`))))))
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
(bind (var `x`) (wrap (do (block
  (bind (var `y`) (var `z`))))))
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
(bind (var `x`) (wrap (do (block
  (bind (var `y`) err)))))
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
(do (block
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y }
```

### AST
```
(do (block
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; }
```

### AST
```
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; let x = y; }
```

### AST
```
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y; let x = y; let x = y; let x = y; }
```

### AST
```
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y }
```

### AST
```
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y let x = y }
```

### AST
```
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
do { let x = y let x = y let x = y let x = y }
```

### AST
```
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
do { 😈 let x = y; let x = y; let x = y; }
```

### AST
```
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  err))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  err))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
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
(bind (var `x`) err)
(bind (var `x`) err)
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
(bind (var `x`) err)
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
(bind (var `x`) err)
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
(bind (var `x`) err)
(bind (var `x`) err)
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
(do (block
  (do block)))
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
(do (block
  (do block)))
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
(if
  (var `x`)
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
if x { y }
```

### AST
```
(if
  (var `x`)
  (block
    (var `y`)))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else {}
```

### AST
```
(if
  (var `x`)
  block
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
if x { y } else {}
```

### AST
```
(if
  (var `x`)
  (block
    (var `y`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else { y }
```

### AST
```
(if
  (var `x`)
  block
  (block
    (var `y`)))
```

--------------------------------------------------------------------------------

### Source
```ite
if x { y } else { z }
```

### AST
```
(if
  (var `x`)
  (block
    (var `y`))
  (block
    (var `z`)))
```

--------------------------------------------------------------------------------

### Source
```ite
if { let x = y }
```

### AST
```
(if
  err
  (block
    (bind (var `x`) (var `y`))))
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
(if
  err
  block)
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
(if
  (var `x`)
  block)
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
(if
  (var `x`)
  block)
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
(if
  (var `x`)
  block)
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
(if
  err
  block)
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
(if
  err
  block)
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
(if
  err
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  err
  block
  block)
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
(if
  err
  block
  block)
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
(if
  err
  block)
object
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
(if
  (var `x`)
  block)
object
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
(if
  (var `x`)
  (block
    object))
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
(if
  err
  (block
    object))
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
(if
  err
  block)
object
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
(if
  err
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  err
  block
  block)
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
(if
  err
  block
  block)
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
(if
  err
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  err
  block
  block)
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
(if
  err
  block
  block)
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
(if
  err
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(if
  err
  block
  block)
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
(if
  err
  block
  block)
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
(if
  err
  block)
object
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
(if
  (var `x`)
  block)
object
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
(if
  (var `x`)
  (block
    object))
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
(if
  (var `x`)
  block)
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
(if
  err
  (block
    object))
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
(if
  err
  block)
object
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
(if
  err
  block
  err)
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
(if
  (var `x`)
  block
  err)
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
(if
  (var `x`)
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
if x }
```

### AST
```
(if
  (var `x`)
  block)
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
(if
  err
  block)
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
(if
  err
  block
  err)
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
(var `x`)
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
(var `x`)
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
err
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
err
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
(if
  (var `x`)
  block)
object
(bind (var `y`) (var `z`))
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
(prop (var `o`) (name `p`))
```

--------------------------------------------------------------------------------

### Source
```ite
o.p.q
```

### AST
```
(prop (prop (var `o`) (name `p`)) (name `q`))
```

--------------------------------------------------------------------------------

### Source
```ite
o.
```

### AST
```
(prop (var `o`) err)
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
(prop (prop (var `o`) (name `p`)) err)
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
(prop (prop (var `o`) err) (name `p`))
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
(var `o`)
(var `p`)
```

--------------------------------------------------------------------------------

### Source
```ite
o😈.p
```

### AST
```
(prop (var `o`) (name `p`))
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
(prop (var `o`) (name `p`))
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
(prop (prop (var `o`) (name `p`)) (name `q`))
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
(prop (prop (var `o`) (name `p`)) (name `q`))
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
(prop (prop (var `o`) (name `p`)) (name `q`))
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
(prop (var `o`) (name `p`))
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
(prop (var `o`) (name `p`))
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
(prop (prop (var `o`) (name `p`)) (name `q`))
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
(prop (prop (var `o`) (name `p`)) (name `q`))
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
(prop (prop (var `o`) (name `p`)) (name `q`))
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
(if
  (var `x`)
  block
  err)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  err)
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block)
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
(if
  (var `x`)
  block)
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
(if
  (var `x`)
  block)
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
(if
  (var `x`)
  block)
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
(prop (do block) (name `p`))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {}.p
```

### AST
```
(prop (if
  (var `x`)
  block) (name `p`))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else {}.p
```

### AST
```
(prop (if
  (var `x`)
  block
  block) (name `p`))
```

--------------------------------------------------------------------------------

### Source
```ite
f()
```

### AST
```
(call (var `f`))
```

--------------------------------------------------------------------------------

### Source
```ite
f ()
```

### AST
```
(call (var `f`))
```

--------------------------------------------------------------------------------

### Source
```ite
f😈()
```

### AST
```
(call (var `f`))
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
(call (var `f`))
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
(var `f`)
(wrap err)
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
(var `f`)
(wrap err)
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
(var `f`)
(wrap err)
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
(var `f`)
(wrap err)
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
(var `f`)
(wrap err)
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
(var `f`)
(wrap err)
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
(call
  (var `f`)
  err)
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
(call (var `f`))
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
(var `p`)
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
(prop (wrap err) (name `p`))
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
(bind err (var `y`))
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
(bind err (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
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
(bind (var `x`) (var `y`))
err
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
(bind (var `x`) (var `y`))
err
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
err
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
err
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
(bind (var `x`) (var `y`))
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
(if
  (var `x`)
  block
  block)
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
(if
  (var `x`)
  block
  block)
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
(bind (var `x`) (var `y`))
(do (block
  (bind (var `x`) (var `y`))
  (bind (var `x`) (var `y`))))
(bind (var `x`) (var `y`))
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
(bind (var `x`) err)
(bind (var `x`) (var `y`))
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
(call
  (var `f`)
  (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
f
(x)
```

### AST
```
(var `f`)
(wrap (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
f;(x)
```

### AST
```
(var `f`)
(wrap (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a)
```

### AST
```
(call
  (var `f`)
  (var `a`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a😈)
```

### AST
```
(call
  (var `f`)
  (var `a`))
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
(call
  (var `f`)
  (var `a`))
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
(call
  (var `f`)
  (var `a`))
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
(call
  (var `f`)
  (var `a`))
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
(call
  (var `f`)
  (var `a`))
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
(call
  (var `f`)
  (var `a`))
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
(call
  (var `f`)
  (var `a`))
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
(call
  (var `f`)
  (var `a`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b😈)
```

### AST
```
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (prop (var `c`) err))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (prop (var `c`) err))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (prop (var `c`) err))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (prop (var `c`) err))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (prop (var `c`) err))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (prop (var `c`) err))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (prop (var `c`) err))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (prop (var `c`) err))
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
(call
  (var `f`)
  (prop (var `a`) err))
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
(call
  (var `f`)
  (prop (var `a`) err))
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
(call
  (var `f`)
  (prop (var `a`) err)
  (var `b`))
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
(call
  (var `f`)
  (call
    (prop (var `a`) err)
    (var `b`)))
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
(call
  (var `f`)
  (prop (call
    (prop (var `a`) err)
    (var `b`)) err))
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
(call
  (var `f`)
  (prop (var `a`) err))
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
(call
  (var `f`)
  (prop (var `a`) err))
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
(call
  (var `f`)
  (prop (var `a`) err))
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
(call
  (var `f`)
  (prop (var `a`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (call
    (prop (var `b`) err)
    (var `c`)))
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
(call
  (var `f`)
  (var `a`)
  (prop (call
    (prop (var `b`) err)
    (var `c`)) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  (var `a`)
  (prop (var `b`) err))
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
(call
  (var `f`)
  err)
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
(call
  (var `f`)
  err)
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
(call
  (var `f`)
  err)
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
(call
  (var `f`)
  err)
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
(call
  (var `f`)
  (var `a`)
  err)
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
(call
  (var `f`)
  (var `a`)
  err)
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
(call
  (var `f`)
  (var `a`)
  err)
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
(call
  (var `f`)
  (var `a`)
  err)
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
(call
  (var `f`)
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  err
  (var `b`))
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
(call
  (var `f`)
  (var `a`)
  err
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  err
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  err
  (var `b`)
  err
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  err
  (var `b`)
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  err
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  err
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  err
  (var `c`)
  err
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  err
  (var `b`)
  (var `c`)
  err
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  err
  (var `b`)
  err
  (var `c`)
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  err
  (var `b`)
  err
  (var `c`)
  err
  (var `d`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  (var `d`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c, d,,)
```

### AST
```
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  (var `d`)
  err)
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`)
  err)
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  err)
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
(call
  (var `f`)
  (var `a`)
  err)
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
(call
  (var `f`)
  (var `a`)
  err
  (var `c`))
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
(call
  (var `f`)
  err
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  err)
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
(call
  (var `f`)
  (var `a`)
  err
  (var `c`))
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
(call
  (var `f`)
  err
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  err)
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
(call
  (var `f`)
  (var `a`)
  err
  (var `c`))
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
(call
  (var `f`)
  err
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  err)
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
(call
  (var `f`)
  (var `a`)
  err
  (var `c`))
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
(call
  (var `f`)
  err
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  err)
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  err
  (var `a`))
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
(call
  (var `f`)
  err
  (var `a`)
  (var `b`))
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
(call
  (var `f`)
  err
  (var `a`)
  (var `b`)
  (var `c`))
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
(call
  (var `f`)
  (var `a`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(,)
```

### AST
```
(call
  (var `f`)
  err)
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
(call (var `f`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a)
```

### AST
```
(call
  (var `f`)
  (var `a`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a,)
```

### AST
```
(call
  (var `f`)
  (var `a`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b)
```

### AST
```
(call
  (var `f`)
  (var `a`)
  (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b,)
```

### AST
```
(call
  (var `f`)
  (var `a`)
  (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c)
```

### AST
```
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
f(a, b, c,)
```

### AST
```
(call
  (var `f`)
  (var `a`)
  (var `b`)
  (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
let _ = x;
```

### AST
```
(bind hole (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let x = _;
```

### AST
```
(bind (var `x`) err)
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
(bind hole err)
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
(fun
  err
  block)
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
(wrap (fun
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() {}
```

### AST
```
(fun
  (name `f`)
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun(a) {}
```

### AST
```
(fun
  err
  (param (var `a`))
  block)
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
(wrap (fun
  (param (var `a`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun(a, b) {}
```

### AST
```
(fun
  err
  (param (var `a`))
  (param (var `b`))
  block)
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
(fun
  err
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
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
(wrap (fun
  (param (var `a`))
  (param (var `b`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
(fun(a, b, c) {})
```

### AST
```
(wrap (fun
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`))
  (param (var `b`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b, c) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun(a,) {}
```

### AST
```
(fun
  err
  (param (var `a`))
  block)
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
(fun
  err
  (param (var `a`))
  (param (var `b`))
  block)
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
(fun
  err
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
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
(wrap (fun
  (param (var `a`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
(fun(a, b,) {})
```

### AST
```
(wrap (fun
  (param (var `a`))
  (param (var `b`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
(fun(a, b, c,) {})
```

### AST
```
(wrap (fun
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a,) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b,) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`))
  (param (var `b`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b, c,) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun() { let x = y; }
```

### AST
```
(fun
  err
  (block
    (bind (var `x`) (var `y`))))
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
(wrap (fun
  (block
    (bind (var `x`) (var `y`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() { let x = y; }
```

### AST
```
(fun
  (name `f`)
  (block
    (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
fun() {}()
```

### AST
```
(fun
  err
  block)
(wrap err)
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
(wrap (call (fun
  block)))
```

--------------------------------------------------------------------------------

### Source
```ite
(fun() {})()
```

### AST
```
(call (wrap (fun
  block)))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun() {};
```

### AST
```
(bind (var `f`) (fun
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f() {};
```

### AST
```
(bind (var `f`) (fun
  block))
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
(bind (var `f`) (fun
  (param (var `a`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`))
  (param (var `b`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b, c) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f(a) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`))
  block))
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
(bind (var `f`) (fun
  (param (var `a`))
  (param (var `b`))
  block))
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
(bind (var `f`) (fun
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
  block))
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
(bind (var `f`) (fun
  (param (var `a`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b,) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`))
  (param (var `b`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b, c,) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f(a,) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`))
  block))
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
(bind (var `f`) (fun
  (param (var `a`))
  (param (var `b`))
  block))
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
(bind (var `f`) (fun
  (param (var `a`))
  (param (var `b`))
  (param (var `c`))
  block))
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
(bind (var `f`) (fun
  (block
    (bind (var `x`) (var `y`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun f() { let x = y; };
```

### AST
```
(bind (var `f`) (fun
  (block
    (bind (var `x`) (var `y`)))))
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
(bind (var `f`) (call (fun
  block)))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = (fun() {})();
```

### AST
```
(bind (var `f`) (call (wrap (fun
  block))))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() {}
```

### AST
```
(fun
  (name `f`)
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f) {}
```

### AST
```
(fun
  (name `f`)
  block)
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
(fun
  (name `f`)
  (param object)
  block)
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
(fun
  (name `f`)
  (param object)
  (block
    (bind (var `x`) (var `y`))))
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
(fun
  (name `f`)
  block)
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
(fun
  (name `f`)
  block)
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
(fun
  err
  block)
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
(fun
  err
  (param object)
  block)
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
(fun
  err
  block)
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
(fun
  err
  block)
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
(fun
  err
  (param (var `a`))
  (param (var `b`))
  (block
    (bind (var `x`) (var `y`))))
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
(wrap (fun
  block))
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
(wrap (fun
  (param object)
  block))
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
(wrap (fun
  block))
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
(wrap (fun
  block))
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
(wrap (fun
  (param (var `a`))
  (param (var `b`))
  (block
    (bind (var `x`) (var `y`)))))
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
(fun
  (name `f`)
  (param (var `a`))
  (param (var `b`))
  (block
    (bind (var `x`) (var `y`))))
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
(fun
  (name `f`)
  block)
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
(fun
  (name `f`)
  block)
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
(fun
  (name `f`)
  (param err)
  block)
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
(fun
  (name `f`)
  (ret err)
  block)
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
(fun
  (name `f`)
  (block
    err))
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
(fun
  (name `f`)
  block)
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
(fun
  (name `f`)
  block)
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
(fun
  (name `f`)
  (param err)
  block)
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
(fun
  (name `f`)
  (ret err)
  block)
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
(fun
  (name `f`)
  (block
    err))
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
(fun
  (name `f`)
  (param err)
  block)
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
return
```

--------------------------------------------------------------------------------

### Source
```ite
return x
```

### AST
```
(return (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
return
x
```

### AST
```
return
(var `x`)
```

--------------------------------------------------------------------------------

### Source
```ite
return;
```

### AST
```
return
```

--------------------------------------------------------------------------------

### Source
```ite
return;x
```

### AST
```
return
(var `x`)
```

--------------------------------------------------------------------------------

### Source
```ite
return x;
```

### AST
```
(return (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
return 😈 x;
```

### AST
```
(return (var `x`))
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
return
(var `x`)
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
(return err)
(var `x`)
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
(return (var `x`))
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
return
(var `x`)
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
(return err)
(var `x`)
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
(return (var `x`))
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
return
(var `x`)
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
(return err)
(var `x`)
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
(return (var `x`))
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
return
(var `x`)
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
(return err)
(var `x`)
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
break
```

--------------------------------------------------------------------------------

### Source
```ite
break x
```

### AST
```
(break (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
break
x
```

### AST
```
break
(var `x`)
```

--------------------------------------------------------------------------------

### Source
```ite
break;
```

### AST
```
break
```

--------------------------------------------------------------------------------

### Source
```ite
break;x
```

### AST
```
break
(var `x`)
```

--------------------------------------------------------------------------------

### Source
```ite
break x;
```

### AST
```
(break (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
loop {}
```

### AST
```
(loop block)
```

--------------------------------------------------------------------------------

### Source
```ite
loop { let x = y; }
```

### AST
```
(loop (block
  (bind (var `x`) (var `y`))))
```

--------------------------------------------------------------------------------

### Source
```ite
!x
```

### AST
```
(not (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
+x
```

### AST
```
(pos (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
-x
```

### AST
```
(neg (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
!x.p
```

### AST
```
(not (prop (var `x`) (name `p`)))
```

--------------------------------------------------------------------------------

### Source
```ite
!x()
```

### AST
```
(not (call (var `x`)))
```

--------------------------------------------------------------------------------

### Source
```ite
!
```

### AST
```
(not err)
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
(not (var `x`))
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
(not (var `x`))
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
(not (var `x`))
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
(not (var `x`))
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
(not (not (var `x`)))
```

--------------------------------------------------------------------------------

### Source
```ite
++x
```

### AST
```
(pos (pos (var `x`)))
```

--------------------------------------------------------------------------------

### Source
```ite
--x
```

### AST
```
(neg (neg (var `x`)))
```

--------------------------------------------------------------------------------

### Source
```ite
+-x
```

### AST
```
(pos (neg (var `x`)))
```

--------------------------------------------------------------------------------

### Source
```ite
-+x
```

### AST
```
(neg (pos (var `x`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b
```

### AST
```
(add (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b + c
```

### AST
```
(add (add (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a - b
```

### AST
```
(sub (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a - b - c
```

### AST
```
(sub (sub (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b
```

### AST
```
(mul (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b * c
```

### AST
```
(mul (mul (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a / b
```

### AST
```
(div (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a / b / c
```

### AST
```
(div (div (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a % b
```

### AST
```
(rem (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a % b % c
```

### AST
```
(rem (rem (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a == b
```

### AST
```
(eq (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a == b == c
```

### AST
```
(eq (eq (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a != b
```

### AST
```
(neq (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a != b != c
```

### AST
```
(neq (neq (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a < b
```

### AST
```
(lt (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a < b < c
```

### AST
```
(lt (lt (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a <= b
```

### AST
```
(lte (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a <= b <= c
```

### AST
```
(lte (lte (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a > b
```

### AST
```
(gt (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a > b > c
```

### AST
```
(gt (gt (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a >= b
```

### AST
```
(gte (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a >= b >= c
```

### AST
```
(gte (gte (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b - c
```

### AST
```
(sub (add (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a - b + c
```

### AST
```
(add (sub (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b * c
```

### AST
```
(add (var `a`) (mul (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b + c
```

### AST
```
(add (mul (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b / c
```

### AST
```
(add (var `a`) (div (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a / b + c
```

### AST
```
(add (div (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b / c
```

### AST
```
(div (mul (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a / b * c
```

### AST
```
(mul (div (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b * c + d
```

### AST
```
(add (add (var `a`) (mul (var `b`) (var `c`))) (var `d`))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b + c * d
```

### AST
```
(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a ^ b + c
```

### AST
```
(add (pow (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b ^ c
```

### AST
```
(add (var `a`) (pow (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a ^ b * c
```

### AST
```
(mul (pow (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a * b ^ c
```

### AST
```
(mul (var `a`) (pow (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a > b + c
```

### AST
```
(gt (var `a`) (add (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b > c
```

### AST
```
(gt (add (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a < b + c
```

### AST
```
(lt (var `a`) (add (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b < c
```

### AST
```
(lt (add (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a >= b + c
```

### AST
```
(gte (var `a`) (add (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b >= c
```

### AST
```
(gte (add (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a <= b + c
```

### AST
```
(lte (var `a`) (add (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b <= c
```

### AST
```
(lte (add (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b == c
```

### AST
```
(eq (add (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a == b + c
```

### AST
```
(eq (var `a`) (add (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b != c
```

### AST
```
(neq (add (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a != b + c
```

### AST
```
(neq (var `a`) (add (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a ==
```

### AST
```
(eq (var `a`) err)
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
(var `b`)
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
(eq (var `a`) (var `b`))
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
(eq (var `a`) (var `b`))
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
(eq (var `a`) (var `b`))
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
(eq (var `a`) (var `b`))
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
(eq (var `a`) (var `b`))
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
(add (prop (var `a`) (name `p`)) (prop (var `b`) (name `q`)))
```

--------------------------------------------------------------------------------

### Source
```ite
!a + !b
```

### AST
```
(add (not (var `a`)) (not (var `b`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a() + b()
```

### AST
```
(add (call (var `a`)) (call (var `b`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b +
```

### AST
```
(add (add (var `a`) (var `b`)) err)
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
(add (add (add (var `a`) (var `b`)) (var `c`)) err)
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
(add (add (var `a`) (var `b`)) (var `c`))
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
(add (add (var `a`) (var `b`)) (var `c`))
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
(add (add (var `a`) (var `b`)) (var `c`))
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
(add (add (var `a`) (var `b`)) (var `c`))
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
(add (add (var `a`) (var `b`)) (var `c`))
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
(mul (var `b`) (pow (var `c`) (var `d`)))
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
(mul (pow (var `a`) err) (pow (var `c`) (var `d`)))
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
(mul (pow (var `a`) (var `b`)) (pow err (var `d`)))
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
(mul (mul (var `a`) (pow err (var `c`))) (var `d`))
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
(mul (mul (var `a`) (pow (var `b`) err)) (var `d`))
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
(mul (pow (var `a`) (var `b`)) (pow (var `c`) err))
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
(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))
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
(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))
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
(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))
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
(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))
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
(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))
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
(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))
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
(add (mul (var `a`) (var `b`)) (mul (var `c`) (var `d`)))
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
(add (sub (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a + -b + c
```

### AST
```
(add (add (var `a`) (neg (var `b`))) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if y {}
```

### AST
```
(if
  (var `x`)
  block
  (if
    (var `y`)
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if y {} else {}
```

### AST
```
(if
  (var `x`)
  block
  (if
    (var `y`)
    block
    block))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if y {} else if z {}
```

### AST
```
(if
  (var `x`)
  block
  (if
    (var `y`)
    block
    (if
      (var `z`)
      block)))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if y {} else if z {} else {}
```

### AST
```
(if
  (var `x`)
  block
  (if
    (var `y`)
    block
    (if
      (var `z`)
      block
      block)))
```

--------------------------------------------------------------------------------

### Source
```ite
if x {} else if {}
```

### AST
```
(if
  (var `x`)
  block
  (if
    err
    block))
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
(if
  (var `x`)
  block
  (block
    (if
      (var `y`)
      block)))
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
(if
  (var `x`)
  block
  (block
    (if
      (add (var `y`) (var `z`))
      block)))
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
(if
  (var `x`)
  block
  (block
    (if
      (var `y`)
      block)))
```

--------------------------------------------------------------------------------

### Source
```ite
a + b * c ^ d
```

### AST
```
(add (var `a`) (mul (var `b`) (pow (var `c`) (var `d`))))
```

--------------------------------------------------------------------------------

### Source
```ite
a 😈 + * b
```

### AST
```
(add (var `a`) (mul err (var `b`)))
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
object
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a}
```

### AST
```
(object
  (prop (name `p`) (var `a`)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, q: b}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (prop (name `q`) (var `b`)))
```

--------------------------------------------------------------------------------

### Source
```ite
{,}
```

### AST
```
(object
  (prop err))
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
(object
  (prop (name `p`) (var `a`)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, q: b,}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (prop (name `q`) (var `b`)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a q: b}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (prop (name `q`) (var `b`)))
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
(object
  (prop (name `p`) (var `a`))
  (prop err)
  (prop (name `q`) (var `b`)))
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
(object
  (prop (name `p`) (var `a`))
  (prop (name `q`) (var `b`))
  (prop err))
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
(object
  (prop (name `p`) (var `a`))
  (prop (name `q`) (var `b`)))
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
(object
  (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a | o}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, | o}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, q: b | o}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (prop (name `q`) (var `b`))
  (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a | {q: b}}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (object
    (prop (name `q`) (var `b`))))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a | {q: b | {}}}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (object
    (prop (name `q`) (var `b`))
    object))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a | {q: b | o}}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (object
    (prop (name `q`) (var `b`))
    (var `o`)))
```

--------------------------------------------------------------------------------

### Source
```ite
{: a}
```

### AST
```
(object
  (prop (name `a`)))
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
(object
  (prop (name `p`))
  (prop (name `a`)))
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
(object
  (prop (name `p`) err))
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
(object
  (prop (name `a`))
  (prop (name `q`) (var `b`)))
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
(object
  (prop (name `p`))
  (prop (name `a`))
  (prop (name `q`) (var `b`)))
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
(object
  (prop (name `p`) err)
  (prop (name `q`) (var `b`)))
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
(object
  (prop (name `p`)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p, q}
```

### AST
```
(object
  (prop (name `p`))
  (prop (name `q`)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p: a, q}
```

### AST
```
(object
  (prop (name `p`) (var `a`))
  (prop (name `q`)))
```

--------------------------------------------------------------------------------

### Source
```ite
{p, q: b}
```

### AST
```
(object
  (prop (name `p`))
  (prop (name `q`) (var `b`)))
```

--------------------------------------------------------------------------------

### Source
```ite
if {} {}
```

### AST
```
(if
  err
  block)
object
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
(prop (if
  err
  (block
    (var `p`)
    (var `a`))) (name `p`))
object
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
(prop (object
  (prop (name `p`) (var `a`))) (name `p`))
```

--------------------------------------------------------------------------------

### Source
```ite
{😈 p: a}
```

### AST
```
(object
  (prop (name `p`) (var `a`)))
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
(object
  (prop (name `p`) (var `a`)))
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
(object
  (prop (name `p`) (var `a`)))
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
(object
  (prop (name `p`) (var `a`)))
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
(object
  (prop err))
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
(and (var `a`) (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b && c
```

### AST
```
(and (and (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b || c
```

### AST
```
(or (or (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b || c
```

### AST
```
(or (and (var `a`) (var `b`)) (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b && c
```

### AST
```
(or (var `a`) (and (var `b`) (var `c`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b && c && d
```

### AST
```
(and (and (and (var `a`) (var `b`)) (var `c`)) (var `d`))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b && c && d
```

### AST
```
(or (var `a`) (and (and (var `b`) (var `c`)) (var `d`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b || c && d
```

### AST
```
(or (and (var `a`) (var `b`)) (and (var `c`) (var `d`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b && c || d
```

### AST
```
(or (and (and (var `a`) (var `b`)) (var `c`)) (var `d`))
```

--------------------------------------------------------------------------------

### Source
```ite
a && b || c || d
```

### AST
```
(or (or (and (var `a`) (var `b`)) (var `c`)) (var `d`))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b && c || d
```

### AST
```
(or (or (var `a`) (and (var `b`) (var `c`))) (var `d`))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b || c && d
```

### AST
```
(or (or (var `a`) (var `b`)) (and (var `c`) (var `d`)))
```

--------------------------------------------------------------------------------

### Source
```ite
a || b || c || d
```

### AST
```
(or (or (or (var `a`) (var `b`)) (var `c`)) (var `d`))
```

--------------------------------------------------------------------------------

### Source
```ite
case V
```

### AST
```
(variant (name `V`))
```

--------------------------------------------------------------------------------

### Source
```ite
case V()
```

### AST
```
(variant (name `V`))
```

--------------------------------------------------------------------------------

### Source
```ite
case V(a)
```

### AST
```
(variant (name `V`)
  (var `a`))
```

--------------------------------------------------------------------------------

### Source
```ite
case V(a, b)
```

### AST
```
(variant (name `V`)
  (var `a`)
  (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
case V(a, b, c)
```

### AST
```
(variant (name `V`)
  (var `a`)
  (var `b`)
  (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
case V(,)
```

### AST
```
(variant (name `V`)
  err)
```

### Errors
- (0:7-0:8) We wanted an expression but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
case V(a,)
```

### AST
```
(variant (name `V`)
  (var `a`))
```

--------------------------------------------------------------------------------

### Source
```ite
case V(a, b,)
```

### AST
```
(variant (name `V`)
  (var `a`)
  (var `b`))
```

--------------------------------------------------------------------------------

### Source
```ite
case V(a, b, c,)
```

### AST
```
(variant (name `V`)
  (var `a`)
  (var `b`)
  (var `c`))
```

--------------------------------------------------------------------------------

### Source
```ite
case
```

### AST
```
(variant err)
```

### Errors
- (0:4-0:4) We wanted a variable name but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
case V
```

### AST
```
(variant (name `V`))
```

--------------------------------------------------------------------------------

### Source
```ite
case V(
```

### AST
```
(variant (name `V`))
```

### Errors
- (0:7-0:7) We wanted `)` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
case V)
```

### AST
```
(variant (name `V`)err)
```

### Errors
- (0:6-0:7) We wanted `(` but we found `)`.

--------------------------------------------------------------------------------

### Source
```ite
case ()
```

### AST
```
(variant err)
```

### Errors
- (0:5-0:6) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
case (a)
```

### AST
```
(variant err
  (var `a`))
```

### Errors
- (0:5-0:6) We wanted a variable name but we found `(`.

--------------------------------------------------------------------------------

### Source
```ite
case 😈(a)
```

### AST
```
(variant err
  (var `a`))
```

### Errors
- (0:5-0:7) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
let true = x
```

### AST
```
(bind (bool true) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {} = o
```

### AST
```
(bind object (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a} = o
```

### AST
```
(bind (object
  (prop (name `a`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (prop (name `b`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b, c} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (prop (name `b`))
  (prop (name `c`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {,} = o
```

### AST
```
(bind (object
  (prop err)) (var `o`))
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
(bind (object
  (prop (name `a`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b,} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (prop (name `b`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b, c,} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (prop (name `b`))
  (prop (name `c`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a: a2} = o
```

### AST
```
(bind (object
  (prop (name `a`) (var `a2`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a: a2, b: b2} = o
```

### AST
```
(bind (object
  (prop (name `a`) (var `a2`))
  (prop (name `b`) (var `b2`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a: a2, b: b2, c: c2} = o
```

### AST
```
(bind (object
  (prop (name `a`) (var `a2`))
  (prop (name `b`) (var `b2`))
  (prop (name `c`) (var `c2`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a: a2, b} = o
```

### AST
```
(bind (object
  (prop (name `a`) (var `a2`))
  (prop (name `b`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b: b2} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (prop (name `b`) (var `b2`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {| o} = o
```

### AST
```
(bind (object
  (var `o`)) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a | o} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (var `o`)) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, | o} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (var `o`)) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b | o} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (prop (name `b`))
  (var `o`)) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a, b, c | o} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (prop (name `b`))
  (prop (name `c`))
  (var `o`)) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a | {b | o}} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (object
    (prop (name `b`))
    (var `o`))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a | {b | {c | o}}} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (object
    (prop (name `b`))
    (object
      (prop (name `c`))
      (var `o`)))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
let {a | {b | {c | {}}}} = o
```

### AST
```
(bind (object
  (prop (name `a`))
  (object
    (prop (name `b`))
    (object
      (prop (name `c`))
      object))) (var `o`))
```

--------------------------------------------------------------------------------

### Source
```ite
{a: {b: c}}
```

### AST
```
(object
  (prop (name `a`) (object
    (prop (name `b`) (var `c`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
{a {}}
```

### AST
```
(object
  (prop (name `a`)))
object
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
(object
  (prop (name `a`)))
(bool true)
```

### Errors
- (0:3-0:7) We wanted `}` but we found `true`.
- (0:7-0:8) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
let case V = x
```

### AST
```
(bind (variant (name `V`)) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let case V() = x
```

### AST
```
(bind (variant (name `V`)) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let case V(a) = x
```

### AST
```
(bind (variant (name `V`)
  (var `a`)) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let case V(a, b) = x
```

### AST
```
(bind (variant (name `V`)
  (var `a`)
  (var `b`)) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let case V(a, b, c) = x
```

### AST
```
(bind (variant (name `V`)
  (var `a`)
  (var `b`)
  (var `c`)) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let case V(,) = x
```

### AST
```
(bind (variant (name `V`)
  err) (var `x`))
```

### Errors
- (0:11-0:12) We wanted a variable name but we found `,`.

--------------------------------------------------------------------------------

### Source
```ite
let case V(a,) = x
```

### AST
```
(bind (variant (name `V`)
  (var `a`)) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let case V(a, b,) = x
```

### AST
```
(bind (variant (name `V`)
  (var `a`)
  (var `b`)) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let case V(a, b, c,) = x
```

### AST
```
(bind (variant (name `V`)
  (var `a`)
  (var `b`)
  (var `c`)) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { y -> {} }
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { y -> {} z -> {} }
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block)
  (case (var `z`) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { case Red -> {} case Green -> {} case Blue -> {} }
```

### AST
```
(match
  (var `x`)
  (case (variant (name `Red`)) block)
  (case (variant (name `Green`)) block)
  (case (variant (name `Blue`)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x {
  y -> {}
  z -> {}
}
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block)
  (case (var `z`) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x {
  case Red -> {}
  case Green -> {}
  case Blue -> {}
}
```

### AST
```
(match
  (var `x`)
  (case (variant (name `Red`)) block)
  (case (variant (name `Green`)) block)
  (case (variant (name `Blue`)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch {} {}
```

### AST
```
(match
  object)
```

--------------------------------------------------------------------------------

### Source
```ite
switch {}
```

### AST
```
(match
  object)
```

### Errors
- (0:9-0:9) We wanted `{` but the file ended.
- (0:9-0:9) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
switch { y -> {} }
```

### AST
```
(match
  (object
    (prop (name `y`) err)))
```

### Errors
- (0:11-0:13) We wanted `:` but we found `->`.
- (0:14-0:15) We wanted `}` but we found `{`.
- (0:17-0:18) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
switch x y -> {} }
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block))
```

### Errors
- (0:9-0:10) We wanted `{` but we found a variable name.

--------------------------------------------------------------------------------

### Source
```ite
switch x { y -> {}
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block))
```

### Errors
- (0:18-0:18) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
switch x { -> {} }
```

### AST
```
(match
  (var `x`)
  (case object block))
```

### Errors
- (0:11-0:13) We wanted a variable name but we found `->`.
- (0:17-0:18) We wanted `->` but we found `}`.
- (0:17-0:18) We wanted `{` but we found `}`.
- (0:18-0:18) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
switch x { -> { let a = b } }
```

### AST
```
(match
  (var `x`)
  (case object (block
    (bind (var `a`) (var `b`)))))
```

### Errors
- (0:11-0:13) We wanted a variable name but we found `->`.
- (0:16-0:19) We wanted `}` but we found `let`.
- (0:16-0:19) We wanted `->` but we found `let`.
- (0:16-0:19) We wanted `{` but we found `let`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { y {} }
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block))
```

### Errors
- (0:13-0:14) We wanted `->` but we found `{`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { y -> } }
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block))
```

### Errors
- (0:16-0:17) We wanted `{` but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { y -> { }
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block))
```

### Errors
- (0:19-0:19) We wanted `}` but the file ended.

--------------------------------------------------------------------------------

### Source
```ite
switch x { y -> {} let a = b }
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block))
(bind (var `a`) (var `b`))
```

### Errors
- (0:19-0:22) We wanted `}` but we found `let`.
- (0:29-0:30) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { y -> {} let a = b z -> {} }
```

### AST
```
(match
  (var `x`)
  (case (var `y`) block))
(bind (var `a`) (var `b`))
(var `z`)
object
```

### Errors
- (0:19-0:22) We wanted `}` but we found `let`.
- (0:31-0:33) We wanted an expression but we found `->`.
- (0:37-0:38) We wanted an expression but we found `}`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { y -> { let a = b } }
```

### AST
```
(match
  (var `x`)
  (case (var `y`) (block
    (bind (var `a`) (var `b`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
let x: T = y;
```

### AST
```
(bind (var `x`) (type (var `T`)) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
let x: = y;
```

### AST
```
(bind (var `x`) (type err) (var `y`))
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
(bind (var `x`) (var `T`))
(var `y`)
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
(wrap (var `x`) (type (var `T`)))
```

--------------------------------------------------------------------------------

### Source
```ite
(x:)
```

### AST
```
(wrap (var `x`) (type err))
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
(wrap (var `x`))
(var `T`)
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
(fun
  (name `f`)
  block)
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
(fun
  (name `f`)
  (param (var `a`) (type (var `T`)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a: T, b: U) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`) (type (var `T`)))
  (param (var `b`) (type (var `U`)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a, b: U) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`))
  (param (var `b`) (type (var `U`)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a: T, b) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`) (type (var `T`)))
  (param (var `b`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f(a: T, b: U, c: V) {}
```

### AST
```
(fun
  (name `f`)
  (param (var `a`) (type (var `T`)))
  (param (var `b`) (type (var `U`)))
  (param (var `c`) (type (var `V`)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a: T) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`) (type (var `T`)))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a: T, b: U) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`) (type (var `T`)))
  (param (var `b`) (type (var `U`)))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a, b: U) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`))
  (param (var `b`) (type (var `U`)))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a: T, b) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`) (type (var `T`)))
  (param (var `b`))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
let f = fun(a: T, b: U, c: V) {};
```

### AST
```
(bind (var `f`) (fun
  (param (var `a`) (type (var `T`)))
  (param (var `b`) (type (var `U`)))
  (param (var `c`) (type (var `V`)))
  block))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() -> T {}
```

### AST
```
(fun
  (name `f`)
  (ret (type (var `T`)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f() -> {}
```

### AST
```
(fun
  (name `f`)
  (ret (type (object)))
  block)
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
(fun
  (name `f`)
  (block
    (var `T`)
    object))
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
(fun
  (name `f`)
  (ret (type (object)))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
(x: !)
```

### AST
```
(wrap (var `x`) (type bottom))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T: A> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`) flex (var `A`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T = A> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`) rigid (var `A`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T, U> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`))
  (forall (name `U`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T: A, U: B> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`) flex (var `A`))
  (forall (name `U`) flex (var `B`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T = A, U = B> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`) rigid (var `A`))
  (forall (name `U`) rigid (var `B`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T = A, U: B> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`) rigid (var `A`))
  (forall (name `U`) flex (var `B`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T: A, U = B> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`) flex (var `A`))
  (forall (name `U`) rigid (var `B`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T, U: B> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`))
  (forall (name `U`) flex (var `B`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T: A, U> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`) flex (var `A`))
  (forall (name `U`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T, U = B> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`))
  (forall (name `U`) rigid (var `B`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T = A, U> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`) rigid (var `A`))
  (forall (name `U`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <,> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall err)
  (var `X`))))
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
(wrap (var `x`) (type (quantify
  (forall (name `T`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <T, U,> X)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `T`))
  (forall (name `U`))
  (var `X`))))
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<>() {}
```

### AST
```
(fun
  (name `f`)
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T: A>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`) flex (var `A`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T = A>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`) rigid (var `A`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T, U>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`))
  (forall (name `U`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T: A, U: B>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`) flex (var `A`))
  (forall (name `U`) flex (var `B`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T = A, U = B>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`) rigid (var `A`))
  (forall (name `U`) rigid (var `B`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T = A, U: B>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`) rigid (var `A`))
  (forall (name `U`) flex (var `B`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T: A, U = B>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`) flex (var `A`))
  (forall (name `U`) rigid (var `B`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T, U: B>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`))
  (forall (name `U`) flex (var `B`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T: A, U>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`) flex (var `A`))
  (forall (name `U`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T, U = B>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`))
  (forall (name `U`) rigid (var `B`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T = A, U>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`) rigid (var `A`))
  (forall (name `U`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<,>() {}
```

### AST
```
(fun
  (name `f`)
  (forall err)
  block)
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
(fun
  (name `f`)
  (forall (name `T`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
fun f<T, U,>() {}
```

### AST
```
(fun
  (name `f`)
  (forall (name `T`))
  (forall (name `U`))
  block)
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {})
```

### AST
```
(wrap (var `x`) (type (object)))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, b: U})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`))
  (prop (name `b`) (var `U`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {,})
```

### AST
```
(wrap (var `x`) (type (object
  (prop err))))
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
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, b: U,})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`))
  (prop (name `b`) (var `U`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) err))))
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
(wrap (var `x`) (type (object
  (prop (name `a`) err)
  (prop (name `b`) err))))
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
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`))
  (prop (name `b`) err))))
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
(wrap (var `x`) (type (object
  (prop (name `a`) err)
  (prop (name `b`) (var `U`)))))
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
(wrap (var `x`) (type (object
  (var `O`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T | O})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`))
  (var `O`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, | O})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`))
  (var `O`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, b: U | O})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`))
  (prop (name `b`) (var `U`))
  (var `O`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T, b: U, | O})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`))
  (prop (name `b`) (var `U`))
  (var `O`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T | {b: U | O}})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`))
  (object
    (prop (name `b`) (var `U`))
    (var `O`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: {a: T | {b: U | {}}})
```

### AST
```
(wrap (var `x`) (type (object
  (prop (name `a`) (var `T`))
  (object
    (prop (name `b`) (var `U`))
    (object)))))
```

--------------------------------------------------------------------------------

### Source
```ite
x
.Foo
```

### AST
```
(prop (var `x`) (name `Foo`))
```

--------------------------------------------------------------------------------

### Source
```ite
x;
.Foo;
```

### AST
```
(var `x`)
(var `Foo`)
```

### Errors
- (1:0-1:1) We wanted a statement but we found `.`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { case V😈 -> {} }
```

### AST
```
(match
  (var `x`)
  (case (variant (name `V`)err) block))
```

### Errors
- (0:17-0:19) We wanted `(` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { | case V -> {} }
```

### AST
```
(match
  (var `x`)
  (case (variant (name `V`)) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { |😈 -> {} }
```

### AST
```
(match
  (var `x`)
  (case (variant err) block))
```

### Errors
- (0:12-0:14) We wanted `case` but we found `😈`.
- (0:15-0:17) We wanted a variable name but we found `->`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { |😈 case V -> {} }
```

### AST
```
(match
  (var `x`)
  (case (variant (name `V`)) block))
```

### Errors
- (0:12-0:14) We wanted `case` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { case V | 😈 -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`))
    (variant err)) block))
```

### Errors
- (0:20-0:22) We wanted `case` but we found `😈`.
- (0:23-0:25) We wanted a variable name but we found `->`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { case V | case 😈 -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`))
    (variant err)) block))
```

### Errors
- (0:25-0:27) We wanted a variable name but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
switch x { case V | case W -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`))
    (variant (name `W`))) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { case V | case W | case X -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`))
    (variant (name `W`))
    (variant (name `X`))) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { | case V | case W -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`))
    (variant (name `W`))) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { | case V | case W | case X -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`))
    (variant (name `W`))
    (variant (name `X`))) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { case V(y) | case W(y) -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`)
      (var `y`))
    (variant (name `W`)
      (var `y`))) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { case V(y) | case W(y) | case X(y) -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`)
      (var `y`))
    (variant (name `W`)
      (var `y`))
    (variant (name `X`)
      (var `y`))) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { | case V(y) | case W(y) -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`)
      (var `y`))
    (variant (name `W`)
      (var `y`))) block))
```

--------------------------------------------------------------------------------

### Source
```ite
switch x { | case V(y) | case W(y) | case X(y) -> {} }
```

### AST
```
(match
  (var `x`)
  (case (union
    (variant (name `V`)
      (var `y`))
    (variant (name `W`)
      (var `y`))
    (variant (name `X`)
      (var `y`))) block))
```

--------------------------------------------------------------------------------

### Source
```ite
let case V | case W = x;
```

### AST
```
(bind (union
  (variant (name `V`))
  (variant (name `W`))) (var `x`))
```

--------------------------------------------------------------------------------

### Source
```ite
let case V | | case W = x;
```

### AST
```
(bind (union
  (variant (name `V`))
  (variant err)
  (variant (name `W`))) (var `x`))
```

### Errors
- (0:13-0:14) We wanted `case` but we found `|`.
- (0:13-0:14) We wanted a variable name but we found `|`.

--------------------------------------------------------------------------------

### Source
```ite
(x: case V)
```

### AST
```
(wrap (var `x`) (type (variant (name `V`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: case V | case W)
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`))
  (variant (name `W`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: case V | case W | case X)
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`))
  (variant (name `W`))
  (variant (name `X`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: | case V)
```

### AST
```
(wrap (var `x`) (type (variant (name `V`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: | case V | case W)
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`))
  (variant (name `W`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: | case V | case W | case X)
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`))
  (variant (name `W`))
  (variant (name `X`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: case V(A))
```

### AST
```
(wrap (var `x`) (type (variant (name `V`)
  (var `A`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: case V(A) | case W(B))
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`)
    (var `A`))
  (variant (name `W`)
    (var `B`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: case V(A) | case W(B) | case X(C))
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`)
    (var `A`))
  (variant (name `W`)
    (var `B`))
  (variant (name `X`)
    (var `C`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: | case V(A))
```

### AST
```
(wrap (var `x`) (type (variant (name `V`)
  (var `A`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: | case V(A) | case W(B))
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`)
    (var `A`))
  (variant (name `W`)
    (var `B`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: | case V(A) | case W(B) | case X(C))
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`)
    (var `A`))
  (variant (name `W`)
    (var `B`))
  (variant (name `X`)
    (var `C`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: case V | | case W)
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`))
  (variant err)
  (variant (name `W`)))))
```

### Errors
- (0:13-0:14) We wanted `case` but we found `|`.
- (0:13-0:14) We wanted a variable name but we found `|`.

--------------------------------------------------------------------------------

### Source
```ite
(x: 😈 case V | case W)
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`))
  (variant (name `W`)))))
```

### Errors
- (0:4-0:6) We wanted a type but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
(x: | 😈 case V | case W)
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`))
  (variant (name `W`)))))
```

### Errors
- (0:6-0:8) We wanted `case` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
(x: | case V 😈 | case W)
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`)err)
  (variant (name `W`)))))
```

### Errors
- (0:13-0:15) We wanted `(` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
(x: | case V | 😈 case W)
```

### AST
```
(wrap (var `x`) (type (union
  (variant (name `V`))
  (variant (name `W`)))))
```

### Errors
- (0:15-0:17) We wanted `case` but we found `😈`.

--------------------------------------------------------------------------------

### Source
```ite
(x: fun)
```

### AST
```
(wrap (var `x`) (type (fun
  err)))
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
(wrap (var `x`) (type (fun
  err)))
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
(wrap (var `x`) (type (fun
  err)))
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
(wrap (var `x`) (type (fun
  err)))
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
(wrap (var `x`) (type (fun
  (var `A`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A) -> B)
```

### AST
```
(wrap (var `x`) (type (fun
  (param (var `A`))
  (var `B`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A, B) -> C)
```

### AST
```
(wrap (var `x`) (type (fun
  (param (var `A`))
  (param (var `B`))
  (var `C`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A, B, C) -> D)
```

### AST
```
(wrap (var `x`) (type (fun
  (param (var `A`))
  (param (var `B`))
  (param (var `C`))
  (var `D`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(,) -> A)
```

### AST
```
(wrap (var `x`) (type (fun
  (param err)
  (var `A`))))
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
(wrap (var `x`) (type (fun
  (param (var `A`))
  (var `B`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A, B,) -> C)
```

### AST
```
(wrap (var `x`) (type (fun
  (param (var `A`))
  (param (var `B`))
  (var `C`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun(A, B, C,) -> D)
```

### AST
```
(wrap (var `x`) (type (fun
  (param (var `A`))
  (param (var `B`))
  (param (var `C`))
  (var `D`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<>() -> A)
```

### AST
```
(wrap (var `x`) (type (fun
  (var `A`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A>() -> B)
```

### AST
```
(wrap (var `x`) (type (fun
  (forall (name `A`))
  (var `B`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B>() -> C)
```

### AST
```
(wrap (var `x`) (type (fun
  (forall (name `A`))
  (forall (name `B`))
  (var `C`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B, C>() -> D)
```

### AST
```
(wrap (var `x`) (type (fun
  (forall (name `A`))
  (forall (name `B`))
  (forall (name `C`))
  (var `D`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<,>() -> A)
```

### AST
```
(wrap (var `x`) (type (fun
  (forall err)
  (var `A`))))
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
(wrap (var `x`) (type (fun
  (forall (name `A`))
  (var `B`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B,>() -> C)
```

### AST
```
(wrap (var `x`) (type (fun
  (forall (name `A`))
  (forall (name `B`))
  (var `C`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B, C,>() -> D)
```

### AST
```
(wrap (var `x`) (type (fun
  (forall (name `A`))
  (forall (name `B`))
  (forall (name `C`))
  (var `D`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: fun<A, B>(C, D) -> E)
```

### AST
```
(wrap (var `x`) (type (fun
  (forall (name `A`))
  (forall (name `B`))
  (param (var `C`))
  (param (var `D`))
  (var `E`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <A, B> fun<C, D>(E, F) -> G)
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `A`))
  (forall (name `B`))
  (fun
    (forall (name `C`))
    (forall (name `D`))
    (param (var `E`))
    (param (var `F`))
    (var `G`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
let (x) = y;
```

### AST
```
(bind (wrap (var `x`)) (var `y`))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: (T));
```

### AST
```
(wrap (var `x`) (type (wrap (var `T`))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <A> (<B> T));
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `A`))
  (wrap (quantify
    (forall (name `B`))
    (var `T`))))))
```

--------------------------------------------------------------------------------

### Source
```ite
(x: <A> <B> T);
```

### AST
```
(wrap (var `x`) (type (quantify
  (forall (name `A`))
  (quantify
    (forall (name `B`))
    (var `T`)))))
```

--------------------------------------------------------------------------------

### Source
```ite
/*
```

### AST
```
empty
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
empty
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
empty
```

--------------------------------------------------------------------------------

### Source
```ite
/* *
```

### AST
```
empty
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
empty
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
empty
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
empty
```

--------------------------------------------------------------------------------

### Source
```ite
/* **/
```

### AST
```
empty
```

--------------------------------------------------------------------------------

### Source
```ite
/* x
```

### AST
```
empty
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
empty
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
(var `x`)
```

--------------------------------------------------------------------------------

### Source
```ite
/* * x
```

### AST
```
empty
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
empty
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
empty
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
(var `x`)
```

--------------------------------------------------------------------------------

### Source
```ite
/* **/ x
```

### AST
```
(var `x`)
```

--------------------------------------------------------------------------------

### Source
```ite
a + b + c + (d * e)
```

### AST
```
(add (add (add (var `a`) (var `b`)) (var `c`)) (wrap (mul (var `d`) (var `e`))))
```

--------------------------------------------------------------------------------
