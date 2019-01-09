# ParserSpecSnapshot

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
loop {}
```

### AST
```
(loop 0:0-0:7)
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
