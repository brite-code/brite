# InferSpec

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (x: Bool), x)
```

### Output
```
(<>, Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (x: Int), x)
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (x: fun<A>(A) -> A), x)
```

### Output
```
(<>, fun<A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (x: <A> A), x)
```

### Output
```
(<>, <A> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (x: <A> Int), x)
```

### Output
```
(<>, <A> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(x) { x })
```

### Output
```
(<>, fun<Type1>(Type1) -> Type1)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), add1(42))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), add1(true))
```

### Output
```
(<>, Int)
```

### Errors
- (0:40-0:44) Can not call `add1` because `Bool` is not an `Int`.
  - (0:21-0:24): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { x }))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { (x: Int) }))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { (x: Bool) }))
```

### Output
```
(<>, Int)
```

### Errors
- (0:59-0:63) Can not call `f` because `Bool` is not an `Int`.
  - (0:22-0:25): `Int`
- (0:59-0:63) Can not call `f` because `Bool` is not an `Int`.
  - (0:30-0:33): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { (x: Bool); 42 }))
```

### Output
```
(<>, Int)
```

### Errors
- (0:59-0:63) Can not call `f` because `Bool` is not an `Int`.
  - (0:22-0:25): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun(fun(Int) -> Int) -> Int), f(fun(x) { true }))
```

### Output
```
(<>, Int)
```

### Errors
- (0:55-0:59) Can not call `f` because `Bool` is not an `Int`.
  - (0:30-0:33): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun(fun(fun(Int) -> Int) -> Int) -> Int), f(fun(g) { g(42) }))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun(fun(fun(Int) -> Int) -> Int) -> Int), f(fun(g) { g(true) }))
```

### Output
```
(<>, Int)
```

### Errors
- (0:69-0:73) Can not call `f` because `Bool` is not an `Int`.
  - (0:26-0:29): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<A>, (add1: fun(Int) -> Int, x: A), add1(x))
```

### Output
```
(<A = Int>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int, x: !), add1(x))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let id = fun(x) { x }; fun(x) { x } })
```

### Output
```
(<>, fun<Type1>(Type1) -> Type1)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), fun(x) { add1(x) })
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(x) { 42 })
```

### Output
```
(<>, fun<Type1>(Type1) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(z) { fun(x) { x } })
```

### Output
```
(<>, fun<Type1, Type2: fun<Type2>(Type2) -> Type2>(Type1) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<A>(A) -> A), id(42))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<A>(A) -> A), id(id))
```

### Output
```
(<>, fun<A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<A>(A) -> A), do { let x = id(42); id })
```

### Output
```
(<>, fun<A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<A>, (x: A), x(x))
```

### Output
```
(<A>, !)
```

### Errors
- (0:19-0:23) Can not call `x` because the type checker infers an infinite type.

--------------------------------------------------------------------------------

### Input
```ite
infer(<A>, (x: A), do { let x = (x: fun<A>(A) -> A); x(x) })
```

### Output
```
(<A = fun<A>(A) -> A>, fun<A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(x) { x(x) })
```

### Output
```
(<>, fun<Type1, Type2>(Type1) -> Type2)
```

### Errors
- (0:23-0:27) Can not call `x` because the type checker infers an infinite type.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(x) { let x = (x: fun<A>(A) -> A); x(x) })
```

### Output
```
(<>, fun<Type1 = fun<A>(A) -> A, Type2: fun<A>(A) -> A>(Type1) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) }: fun<A = fun<A>(A) -> A>(A) -> A))
```

### Output
```
(<>, fun<A = fun<A>(A) -> A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(add1))
```

### Output
```
(<>, fun<A>(A) -> A)
```

### Errors
- (0:82-0:86) Can not call `fun(x) {}` because `fun<A>(A) -> A` is more general than `fun(Int) -> Int`.
  - (0:57-0:71): `fun<A>(A) -> A`
  - (0:17-0:32): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(42))
```

### Output
```
(<>, fun<A>(A) -> A)
```

### Errors
- (0:82-0:84) Can not call `fun(x) {}` because `Int` is not a function.
  - (0:57-0:71): function

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(fun(x) { x }))
```

### Output
```
(<>, fun<A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(fun(x) { x })(42))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), nope)
```

### Output
```
(<>, !)
```

### Errors
- (0:14-0:18) Can not find `nope`.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), true)
```

### Output
```
(<>, Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let x = true; x })
```

### Output
```
(<>, Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let x = true; let y = x; y })
```

### Output
```
(<>, Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let x = true; let y = x; x })
```

### Output
```
(<>, Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(x) { true })
```

### Output
```
(<>, fun<Type1>(Type1) -> Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(x) { let y = x; y })
```

### Output
```
(<>, fun<Type1>(Type1) -> Type1)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), true(42))
```

### Output
```
(<>, !)
```

### Errors
- (0:14-0:18) Can not call `true` because `Bool` is not a function.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), 42(true))
```

### Output
```
(<>, !)
```

### Errors
- (0:14-0:16) Can not call `42` because `Int` is not a function.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), 0b101(true))
```

### Output
```
(<>, !)
```

### Errors
- (0:14-0:19) Can not call `0b101` because `Int` is not a function.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), 0xFFF(true))
```

### Output
```
(<>, !)
```

### Errors
- (0:14-0:19) Can not call `0xFFF` because `Int` is not a function.

--------------------------------------------------------------------------------

### Input
```ite
infer(<B>, (choose: fun<A>(A) -> fun(A) -> A, x: B), choose(x)(42))
```

### Output
```
(<B = Int>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<B>, (choose: fun<A>(A) -> fun(A) -> A, x: B), choose(42)(x))
```

### Output
```
(<B = Int>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { choose(x)(42) })
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { choose(42)(x) })
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { let y = choose(x)(42); x })
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { let y = choose(42)(x); x })
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<B>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, x: B), choose(x)(id))
```

### Output
```
(<B: fun<A>(A) -> A>, B)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { choose(x)(id) })
```

### Output
```
(<>, fun<Type1: fun<A>(A) -> A>(Type1) -> Type1)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { choose(id)(x) })
```

### Output
```
(<>, fun<Type1: fun<A>(A) -> A>(Type1) -> Type1)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { let y = choose(x)(id); x })
```

### Output
```
(<>, fun<Type1: fun<A>(A) -> A>(Type1) -> Type1)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A), fun(x) { let y = choose(id)(x); x })
```

### Output
```
(<>, fun<Type1: fun<A>(A) -> A>(Type1) -> Type1)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id)(add1))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1)(id))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (fun(x) { choose(x)(id) })(add1))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (fun(x) { choose(id)(x) })(add1))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), (fun(x) { choose(x)(true) })(42))
```

### Output
```
(<>, Bool)
```

### Errors
- (0:75-0:77) Can not call `fun(x) {}` because `Int` is not a `Bool`.
  - (0:66-0:70): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), (fun(x) { choose(true)(x) })(42))
```

### Output
```
(<>, Bool)
```

### Errors
- (0:75-0:77) Can not call `fun(x) {}` because `Int` is not a `Bool`.
  - (0:63-0:67): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(f) { fun(x) { choose(f(x))(x) } })
```

### Output
```
(<>, fun<Type2>(fun(Type2) -> Type2) -> fun(Type2) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), (fun(f) { fun(x) { choose(f(x))(x) } })(fun(x) { x }))
```

### Output
```
(<>, fun<Type4>(Type4) -> Type4)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, undefined: !), (fun(f) { fun(x) { choose(f(x))(x) } })(fun(x) { undefined }))
```

### Output
```
(<>, fun<Type4>(Type4) -> Type4)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, undefined: !), fun(f) { fun(x) { choose(f(x))(undefined) } })
```

### Output
```
(<>, fun<Type2, Type3>(fun(Type2) -> Type3) -> fun(Type2) -> Type3)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (42: Bool))
```

### Output
```
(<>, Bool)
```

### Errors
- (0:15-0:17) Can not change type of `42` because `Int` is not a `Bool`.
  - (0:19-0:23): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Int) -> Bool) })
```

### Output
```
(<>, fun(Int) -> Bool)
```

### Errors
- (0:63-0:64) Can not change type of `f` because `Bool` is not an `Int`.
  - (0:47-0:51): `Bool`
  - (0:70-0:73): `Int`
- (0:63-0:64) Can not change type of `f` because `Int` is not a `Bool`.
  - (0:56-0:59): `Int`
  - (0:78-0:82): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<T>(T) -> T), do { let x = true; (id(x): Int) })
```

### Output
```
(<>, Int)
```

### Errors
- (0:52-0:57) Can not change type of `id(x)` because `Bool` is not an `Int`.
  - (0:45-0:49): `Bool`
  - (0:59-0:62): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Bool) -> Int) })
```

### Output
```
(<>, fun(Bool) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Int) -> Int) })
```

### Output
```
(<>, fun(Int) -> Int)
```

### Errors
- (0:63-0:64) Can not change type of `f` because `Bool` is not an `Int`.
  - (0:47-0:51): `Bool`
  - (0:70-0:73): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let f = (fun(x) { 42 }: fun(Bool) -> Int); (f: fun(Bool) -> Bool) })
```

### Output
```
(<>, fun(Bool) -> Bool)
```

### Errors
- (0:63-0:64) Can not change type of `f` because `Int` is not a `Bool`.
  - (0:56-0:59): `Int`
  - (0:79-0:83): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { 42 }: fun(Bool) -> Int))
```

### Output
```
(<>, fun(Bool) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { x }: fun(Bool) -> Int))
```

### Output
```
(<>, fun(Bool) -> Int)
```

### Errors
- (0:24-0:25) Can not change type of `fun(x) {}` because `Bool` is not an `Int`.
  - (0:33-0:37): `Bool`
  - (0:42-0:45): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { x }: fun(Bool) -> Bool))
```

### Output
```
(<>, fun(Bool) -> Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ((fun(x) { x }: fun(Bool) -> Bool): fun<A>(A) -> A))
```

### Output
```
(<>, fun<A>(A) -> A)
```

### Errors
- (0:30-0:47) Can not change type of `fun(x) {}` because `fun<A>(A) -> A` is more general than `fun(Bool) -> Bool`.
  - (0:50-0:64): `fun<A>(A) -> A`

--------------------------------------------------------------------------------

### Input
```ite
infer(<A, B>, (f: A, x: B), f(x))
```

### Output
```
(<B, Type1, A = fun(B) -> Type1>, Type1)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(f) { fun(x) { f(x) } })
```

### Output
```
(<>, fun<Type2, Type3>(fun(Type2) -> Type3) -> fun(Type2) -> Type3)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B, add1: fun(Int) -> Int), app(add1))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B, add1: fun(Int) -> Int), app(add1)(0))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B), app(fun(x) { x }))
```

### Output
```
(<>, fun<A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B), app(fun(x) { x })(42))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), choose(fun(x) { x }))
```

### Output
```
(<>, fun<Type2: fun<Type1>(Type1) -> Type1>(Type2) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), choose(fun(x) { x })(42))
```

### Output
```
(<>, fun<Type1>(Type1) -> Type1)
```

### Errors
- (0:67-0:69) Can not call `choose(fun(x) {})` because `Int` is not a function.
  - (0:53-0:65): function

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), choose(42)(fun(x) { x }))
```

### Output
```
(<>, Int)
```

### Errors
- (0:57-0:69) Can not call `choose(42)` because a function is not an `Int`.
  - (0:53-0:55): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(x) { x(x) })
```

### Output
```
(<>, fun<Type1, Type2>(Type1) -> Type2)
```

### Errors
- (0:23-0:27) Can not call `x` because the type checker infers an infinite type.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(x)(y); x(y) } })
```

### Output
```
(<>, fun<Type1, Type2: fun<Type3>(Type1) -> Type3>(Type1) -> Type2)
```

### Errors
- (0:86-0:90) Can not call `x` because the type checker infers an infinite type.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(y)(x); x(y) } })
```

### Output
```
(<>, fun<Type1, Type2: fun<Type3>(Type1) -> Type3>(Type1) -> Type2)
```

### Errors
- (0:86-0:90) Can not call `x` because the type checker infers an infinite type.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(x)(y); y(x) } })
```

### Output
```
(<>, fun<Type1, Type2: fun<Type3>(Type1) -> Type3>(Type1) -> Type2)
```

### Errors
- (0:86-0:90) Can not call `y` because the type checker infers an infinite type.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A), fun(x) { fun(y) { let z = choose(y)(x); y(x) } })
```

### Output
```
(<>, fun<Type1, Type2: fun<Type3>(Type1) -> Type3>(Type1) -> Type2)
```

### Errors
- (0:86-0:90) Can not call `y` because the type checker infers an infinite type.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(x) { let x = (x: fun<A>(A) -> A); x(x) })
```

### Output
```
(<>, fun<Type1 = fun<A>(A) -> A, Type2: fun<A>(A) -> A>(Type1) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(add1))
```

### Output
```
(<>, fun<A>(A) -> A)
```

### Errors
- (0:82-0:86) Can not call `fun(x) {}` because `fun<A>(A) -> A` is more general than `fun(Int) -> Int`.
  - (0:57-0:71): `fun<A>(A) -> A`
  - (0:17-0:32): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), (fun(x) { let x = (x: fun<A>(A) -> A); x(x) }: fun<A = fun<A>(A) -> A>(A) -> A)(add1))
```

### Output
```
(<>, fun<A>(A) -> A)
```

### Errors
- (0:115-0:119) Can not call `fun(x) {}` because `fun<A>(A) -> A` is more general than `fun(Int) -> Int`.
  - (0:90-0:104): `fun<A>(A) -> A`
  - (0:17-0:32): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), auto(add1))
```

### Output
```
(<>, fun<A>(A) -> A)
```

### Errors
- (0:79-0:83) Can not call `auto` because `fun<A>(A) -> A` is more general than `fun(Int) -> Int`.
  - (0:25-0:39): `fun<A>(A) -> A`
  - (0:56-0:71): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id)(id))
```

### Output
```
(<>, fun<A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1)(add1))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id))
```

### Output
```
(<>, fun<Type2: fun<A>(A) -> A>(Type2) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1))
```

### Output
```
(<>, fun(fun(Int) -> Int) -> fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(id)(add1))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), choose(add1)(id))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), choose(id)(auto))
```

### Output
```
(<>, fun<A = fun<A>(A) -> A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), choose(auto)(id))
```

### Output
```
(<>, fun<A = fun<A>(A) -> A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), id(auto))
```

### Output
```
(<>, fun<A = fun<A>(A) -> A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), auto(id))
```

### Output
```
(<>, fun<A2>(A2) -> A2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), (fun(x) { x(id) }))
```

### Output
```
(<>, fun<Type2: fun<A>(A) -> A, Type3>(fun(Type2) -> Type3) -> Type3)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<A>(A) -> A, auto: fun<A = fun<A>(A) -> A>(A) -> A), (fun(x) { x(id) })(auto))
```

### Output
```
(<>, fun<A2>(A2) -> A2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { x(fun(x) { x }) })(fun(x) { let x = (x: fun<A>(A) -> A); x(x) }))
```

### Output
```
(<>, fun<A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (app: fun<A, B>(fun(A) -> B) -> fun(A) -> B, auto: fun<A = fun<A>(A) -> A>(A) -> A, id: fun<A>(A) -> A), app(auto)(id))
```

### Output
```
(<>, fun<A2>(A2) -> A2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(f) { fun(x) { f(x) } })(fun(x) { let x = (x: fun<A>(A) -> A); x(x) })(fun(x) { x }))
```

### Output
```
(<>, fun<A>(A) -> A)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (undefined: !), fun(x) { undefined })
```

### Output
```
(<>, fun<Type1, Type2>(Type1) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let id = fun(x) { x }; (id: fun<X>(X) -> Int) })
```

### Output
```
(<>, fun<X>(X) -> Int)
```

### Errors
- (0:43-0:45) Can not change type of `id` because `fun<X>(X) -> Int` is more general than `fun(Int) -> Int`.
  - (0:47-0:63): `fun<X>(X) -> Int`
  - (0:28-0:40): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let id = fun(x) { x }; let id = (id: fun<X>(X) -> Int); (id: fun<X>(X) -> X) })
```

### Output
```
(<>, fun<X>(X) -> X)
```

### Errors
- (0:52-0:54) Can not change type of `id` because `fun<X>(X) -> Int` is more general than `fun(Int) -> Int`.
  - (0:56-0:72): `fun<X>(X) -> Int`
  - (0:28-0:40): `fun(Int) -> Int`
- (0:76-0:78) Can not change type of `id` because `fun<X>(X) -> X` is more general than `fun(Int) -> Int`.
  - (0:80-0:94): `fun<X>(X) -> X`
  - (0:56-0:72): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let f = fun(x) { 42 }; (f: fun<X>(X) -> Bool) })
```

### Output
```
(<>, fun<X>(X) -> Bool)
```

### Errors
- (0:43-0:44) Can not change type of `f` because `Int` is not a `Bool`.
  - (0:36-0:38): `Int`
  - (0:59-0:63): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), auto(add1))
```

### Output
```
(<>, fun<A>(A) -> A)
```

### Errors
- (0:79-0:83) Can not call `auto` because `fun<A>(A) -> A` is more general than `fun(Int) -> Int`.
  - (0:25-0:39): `fun<A>(A) -> A`
  - (0:56-0:71): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), do { let id = fun(x) { x }; (id: fun<X>(X) -> Int); id })
```

### Output
```
(<>, fun<Type1>(Type1) -> Type1)
```

### Errors
- (0:64-0:66) Can not change type of `id` because `fun<X>(X) -> Int` is more general than `fun(Int) -> Int`.
  - (0:68-0:84): `fun<X>(X) -> Int`
  - (0:49-0:61): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), do { let id = (fun(x) { x }: fun<X>(X) -> Int); add1(id(true)) })
```

### Output
```
(<>, Int)
```

### Errors
- (0:50-0:62) Can not change type of `fun(x) {}` because `fun<X>(X) -> Int` is more general than `fun(Int) -> Int`.
  - (0:64-0:80): `fun<X>(X) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), do { let id = (fun(x) { true }: fun<X>(X) -> X); add1(id(42)) })
```

### Output
```
(<>, Int)
```

### Errors
- (0:50-0:65) Can not change type of `fun(x) {}` because `fun<X>(X) -> X` is more general than `fun(Bool) -> Bool`.
  - (0:67-0:81): `fun<X>(X) -> X`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), do { let id = fun(x) { x }; add1(id(true)) })
```

### Output
```
(<>, Int)
```

### Errors
- (0:71-0:75) Can not call `add1` because `Bool` is not an `Int`.
  - (0:21-0:24): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), do { let id = fun(x) { x }; let add1 = fun(x) { add1(id(x)) }; add1(true) })
```

### Output
```
(<>, Int)
```

### Errors
- (0:103-0:107) Can not call `add1` because `Bool` is not an `Int`.
  - (0:21-0:24): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (undefined: !, choose: fun<A>(A) -> fun(A) -> A), choose((undefined: fun<X: fun<A: !, B = !>(A) -> B>(X) -> X))((undefined: fun<X: fun<A = !, B: !>(A) -> B>(X) -> X)))
```

### Output
```
(<>, fun<X: fun<A = !, B = !>(A) -> B>(X) -> X)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), (auto: fun<X: fun<A>(A) -> A>(X) -> X))
```

### Output
```
(<>, fun<X: fun<A>(A) -> A>(X) -> X)
```

### Errors
- (0:75-0:79) Can not change type of `auto` because `fun<X: fun<A>(A) -> A>(X) -> X` is more general than `fun<A = fun<A2>(A2) -> A2>(A) -> A`.
  - (0:81-0:111): `fun<X: fun<A>(A) -> A>(X) -> X`
  - (0:17-0:48): `fun<A = fun<A2>(A2) -> A2>(A) -> A`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (auto: fun<A = fun<A>(A) -> A>(A) -> A, add1: fun(Int) -> Int), (auto: fun<X: fun<A>(A) -> A>(X) -> X)(add1))
```

### Output
```
(<>, fun(Int) -> Int)
```

### Errors
- (0:75-0:79) Can not change type of `auto` because `fun<X: fun<A>(A) -> A>(X) -> X` is more general than `fun<A = fun<A2>(A2) -> A2>(A) -> A`.
  - (0:81-0:111): `fun<X: fun<A>(A) -> A>(X) -> X`
  - (0:17-0:48): `fun<A = fun<A2>(A2) -> A2>(A) -> A`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (choose(id): fun<X = fun<A>(A) -> A>(X) -> X))
```

### Output
```
(<>, fun<X = fun<A>(A) -> A>(X) -> X)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (choose(id): fun<X = fun<A>(A) -> A>(X) -> X)(add1))
```

### Output
```
(<>, fun<A>(A) -> A)
```

### Errors
- (0:135-0:139) Can not call `choose(id)` because `fun<A>(A) -> A` is more general than `fun(Int) -> Int`.
  - (0:110-0:124): `fun<A>(A) -> A`
  - (0:71-0:86): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (choose: fun<A>(A) -> fun(A) -> A, id: fun<A>(A) -> A, add1: fun(Int) -> Int), (choose(id): fun<X: fun<A>(A) -> A>(X) -> X)(add1))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (add1: fun(Int) -> Int), (add1: fun<A>(A) -> A))
```

### Output
```
(<>, fun<A>(A) -> A)
```

### Errors
- (0:36-0:40) Can not change type of `add1` because `fun<A>(A) -> A` is more general than `fun(Int) -> Int`.
  - (0:42-0:56): `fun<A>(A) -> A`
  - (0:17-0:32): `fun(Int) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { x }: fun(Int) -> Int))
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { x }: fun<A>(A) -> A)(42))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<A, B, C>, (A: A, B: B, C: C), if A { B } else { C })
```

### Output
```
(<A = Bool, C, B = C>, B)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(x) { fun(y) { if true { x } else { y } } })
```

### Output
```
(<>, fun<Type1>(Type1) -> fun(Type1) -> Type1)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), if 42 { true } else { false })
```

### Output
```
(<>, Bool)
```

### Errors
- (0:17-0:19) Can not test `42` because `Int` is not a `Bool`.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), if true { true } else { false })
```

### Output
```
(<>, Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), if true { 1 } else { 0 })
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), if true { 1 } else { false })
```

### Output
```
(<>, Int)
```

### Errors
- (0:35-0:40) Can not test `true` because `Bool` is not an `Int`.
  - (0:24-0:25): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), if true { true } else { 0 })
```

### Output
```
(<>, Bool)
```

### Errors
- (0:38-0:39) Can not test `true` because `Int` is not a `Bool`.
  - (0:24-0:28): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), if true { 1 } else {})
```

### Output
```
(<>, Int)
```

### Errors
- (0:33-0:35) Can not test `true` because void is not an `Int`.
  - (0:24-0:25): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), if true {} else { 0 })
```

### Output
```
(<>, void)
```

### Errors
- (0:32-0:33) Can not test `true` because `Int` is not void.
  - (0:22-0:24): void

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<A>(A) -> A, add1: fun(Int) -> Int), if true { id } else { add1 })
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (id: fun<A>(A) -> A, add1: fun(Int) -> Int), if true { add1 } else { id })
```

### Output
```
(<>, fun(Int) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (42: nope))
```

### Output
```
(<>, !)
```

### Errors
- (0:19-0:23) Can not find `nope`.
- (0:15-0:17) Can not change type of `42` because `!` is more general than `Int`.
  - (0:19-0:23): `!`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (x: nope), x)
```

### Output
```
(<>, !)
```

### Errors
- (0:14-0:18) Can not find `nope`.

--------------------------------------------------------------------------------

### Input
```ite
infer(<T = nope>, (x: T, y: T, choose: fun<A>(A) -> fun(A) -> A), choose(x)(y))
```

### Output
```
(<T = !>, T)
```

### Errors
- (0:11-0:15) Can not find `nope`.

--------------------------------------------------------------------------------

### Input
```ite
infer(<T1 = nope, T2 = nope>, (x: T1, y: T2, choose: fun<A>(A) -> fun(A) -> A), choose(x)(y))
```

### Output
```
(<T2 = !, T1 = T2>, T2)
```

### Errors
- (0:12-0:16) Can not find `nope`.
- (0:23-0:27) Can not find `nope`.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (42: !))
```

### Output
```
(<>, !)
```

### Errors
- (0:15-0:17) Can not change type of `42` because `!` is more general than `Int`.
  - (0:19-0:20): `!`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(id) { let id = (id: fun<A>(A) -> A); id })
```

### Output
```
(<>, fun<Type1 = fun<A>(A) -> A, Type2: fun<A>(A) -> A>(Type1) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun<A = fun<B>(B) -> B>(Int) -> A), f(0))
```

### Output
```
(<>, fun<B>(B) -> B)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun<A = fun<B>(B) -> B>(Int) -> A), f(0)(1))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(x) { x }: fun<X>(X) -> Int))
```

### Output
```
(<>, fun<X>(X) -> Int)
```

### Errors
- (0:15-0:27) Can not change type of `fun(x) {}` because `fun<X>(X) -> Int` is more general than `fun(Int) -> Int`.
  - (0:29-0:45): `fun<X>(X) -> Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {})
```

### Output
```
(<>, {})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42})
```

### Output
```
(<>, {a: Int})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: true})
```

### Output
```
(<>, {a: Bool})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, b: true})
```

### Output
```
(<>, {a: Int, b: Bool})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, b: true, c: void})
```

### Output
```
(<>, {a: Int, b: Bool, c: void})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, a: true})
```

### Output
```
(<>, {a: Int, a: Bool})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, a: true, a: void})
```

### Output
```
(<>, {a: Int, a: Bool, a: void})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: (42: Bool), b: (true: Int)})
```

### Output
```
(<>, {a: Bool, b: Int})
```

### Errors
- (0:19-0:21) Can not change type of `42` because `Int` is not a `Bool`.
  - (0:23-0:27): `Bool`
- (0:34-0:38) Can not change type of `true` because `Bool` is not an `Int`.
  - (0:40-0:43): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: (42: Bool), a: (true: Bool)})
```

### Output
```
(<>, {a: Bool, a: Bool})
```

### Errors
- (0:19-0:21) Can not change type of `42` because `Int` is not a `Bool`.
  - (0:23-0:27): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:17) Can not change type of `{}` because an object is not an `Int`.
  - (0:19-0:22): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({a: 42}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:22) Can not change type of `{a}` because an object is not an `Int`.
  - (0:24-0:27): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({a: 42, b: true}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:31) Can not change type of `{a, b}` because an object is not an `Int`.
  - (0:33-0:36): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({a: 42, b: true, c: void}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:40) Can not change type of `{a, b, c}` because an object is not an `Int`.
  - (0:42-0:45): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({c: void, a: 42, b: true}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:40) Can not change type of `{c, a, b}` because an object is not an `Int`.
  - (0:42-0:45): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({a: 42, b: true, c: void, d: 42}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:47) Can not change type of `{a, b, c}` because an object is not an `Int`.
  - (0:49-0:52): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({| {}}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:21) Can not change type of `{ | {}}` because an object is not an `Int`.
  - (0:23-0:26): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({a: 42 | {}}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:27) Can not change type of `{a | {}}` because an object is not an `Int`.
  - (0:29-0:32): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({a: 42, b: true | {}}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:36) Can not change type of `{a, b | {}}` because an object is not an `Int`.
  - (0:38-0:41): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({a: 42, b: true, c: void | {}}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:45) Can not change type of `{a, b, c | {}}` because an object is not an `Int`.
  - (0:47-0:50): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({c: void, a: 42, b: true | {}}: Int))
```

### Output
```
(<>, Int)
```

### Errors
- (0:15-0:45) Can not change type of `{c, a, b | {}}` because an object is not an `Int`.
  - (0:47-0:50): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {| {}})
```

### Output
```
(<>, {})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {| ({}: Int)})
```

### Output
```
(<>, { | Int})
```

### Errors
- (0:18-0:20) Can not change type of `{}` because an object is not an `Int`.
  - (0:22-0:25): `Int`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42 | {}})
```

### Output
```
(<>, {a: Int})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, b: true | {}})
```

### Output
```
(<>, {a: Int, b: Bool})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42 | {b: true}})
```

### Output
```
(<>, {a: Int, b: Bool})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, b: true, c: void | {}})
```

### Output
```
(<>, {a: Int, b: Bool, c: void})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, b: true | {c: void}})
```

### Output
```
(<>, {a: Int, b: Bool, c: void})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42 | {b: true, c: void}})
```

### Output
```
(<>, {a: Int, b: Bool, c: void})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, a: true, a: void | {}})
```

### Output
```
(<>, {a: Int, a: Bool, a: void})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, a: true | {a: void}})
```

### Output
```
(<>, {a: Int, a: Bool, a: void})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42 | {a: true, a: void}})
```

### Output
```
(<>, {a: Int, a: Bool, a: void})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {id: fun(x) { x }})
```

### Output
```
(<>, <Type1: fun<Type1>(Type1) -> Type1> {id: Type1})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({id: fun(x) { x }}: {id: fun<T>(T) -> T}))
```

### Output
```
(<>, <Type1: fun<T>(T) -> T> {id: Type1})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({id: fun(x) { (x: Int) }}: {id: fun<T>(T) -> T}))
```

### Output
```
(<>, <Type1: fun<T>(T) -> T> {id: Type1})
```

### Errors
- (0:15-0:40) Can not change type of `{id}` because `{id: fun<T>(T) -> T}` is more general than `{id: fun(Int) -> Int}`.
  - (0:42-0:62): `{id: fun<T>(T) -> T}`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(r) { if true { { x: 2 | r } } else { { y: 2 | r } } })
```

### Output
```
(<>, fun<Type1>(Type1) -> {x: Int | Type1})
```

### Errors
- (0:55-0:67) Can not test `true` because `x:` is missing.
  - (0:35-0:36): `x:`
- (0:57-0:58) Can not test `true` because `y:` is not needed.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), do { let o = {id: fun(x) { x }}; o.id(42); o })
```

### Output
```
(<>, <Type1: fun<Type1>(Type1) -> Type1> {id: Type1})
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { o.a })
```

### Output
```
(<>, fun<Type2, Type3>({a: Type2 | Type3}) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { o.a; o.b })
```

### Output
```
(<>, fun<Type2, Type4, Type6>({a: Type2, b: Type4 | Type6}) -> Type4)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { o.a; o.b; o.c })
```

### Output
```
(<>, fun<Type2, Type4, Type5, Type9>({a: Type2, b: Type4, c: Type5 | Type9}) -> Type5)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { o.b; o.a })
```

### Output
```
(<>, fun<Type2, Type4, Type6>({b: Type2, a: Type4 | Type6}) -> Type4)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { (o.a: Int) })
```

### Output
```
(<>, fun<Type3>({a: Int | Type3}) -> Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { (o.a: Int); (o.b: Bool) })
```

### Output
```
(<>, fun<Type6>({a: Int, b: Bool | Type6}) -> Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { (o.a: Int); (o.b: Bool); (o.c: void) })
```

### Output
```
(<>, fun<Type9>({a: Int, b: Bool, c: void | Type9}) -> void)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { o.a; o.a })
```

### Output
```
(<>, fun<Type2, Type3>({a: Type2 | Type3}) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { (o.a: Int); (o.a: Bool) })
```

### Output
```
(<>, fun<Type3>({a: Int | Type3}) -> Bool)
```

### Errors
- (0:36-0:39) Can not change type of `o.a` because `Int` is not a `Bool`.
  - (0:29-0:32): `Int`
  - (0:41-0:45): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(o) { o.a })({a: 42}))
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(o) { (o.a: Bool) })({a: 42}))
```

### Output
```
(<>, Bool)
```

### Errors
- (0:43-0:45) Can not call `fun(o) {}` because `Int` is not a `Bool`.
  - (0:30-0:34): `Bool`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), (fun(o) { o.a; o.b })({a: 42}))
```

### Output
```
(<>, !)
```

### Errors
- (0:36-0:43) Can not call `fun(o) {}` because `b:` is missing.
  - (0:31-0:32): `b:`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42}.a)
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, b: true}.a)
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, b: true, c: void}.a)
```

### Output
```
(<>, Int)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, b: true, c: void}.b)
```

### Output
```
(<>, Bool)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42, b: true, c: void}.c)
```

### Output
```
(<>, void)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), {a: 42}.b)
```

### Output
```
(<>, !)
```

### Errors
- (0:22-0:23) Can not get `b:` on `{a}` because `b:` is missing.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({hidden: 42}: <T> {hidden: T}))
```

### Output
```
(<>, <T> {hidden: T})
```

### Errors
- (0:15-0:27) Can not change type of `{hidden}` because `{hidden: !}` is more general than `{hidden: Int}`.
  - (0:33-0:44): `{hidden: !}`

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun<T: {}>(T) -> void), f({}))
```

### Output
```
(<>, void)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun<T: {}>(T) -> void), f({a: 42}))
```

### Output
```
(<>, void)
```

### Errors
- (0:41-0:42) Can not call `f` because `a:` is not needed.

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun<T: {}>(T) -> void), f(42))
```

### Output
```
(<>, void)
```

### Errors
- (0:40-0:42) Can not call `f` because `Int` is not an object.
  - (0:21-0:23): object

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun<T: { | ! }>(T) -> void), f({}))
```

### Output
```
(<>, void)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun<T: { | ! }>(T) -> void), f({a: 42}))
```

### Output
```
(<>, void)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (f: fun<T: { | ! }>(T) -> void), f(42))
```

### Output
```
(<>, void)
```

### Errors
- (0:45-0:47) Can not call `f` because `Int` is not an object.
  - (0:21-0:28): object

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), fun(o) { o.p })
```

### Output
```
(<>, fun<Type2, Type3>({p: Type2 | Type3}) -> Type2)
```

--------------------------------------------------------------------------------

### Input
```ite
infer(<>, (), ({a: 42, b: false}: {a: Int | !}))
```

### Output
```
(<>, <Type1> {a: Int | Type1})
```

### Errors
- (0:15-0:32) Can not change type of `{a, b}` because `{a: Int | !}` is more general than `{a: Int, b: Bool}`.
  - (0:34-0:46): `{a: Int | !}`

--------------------------------------------------------------------------------

### Input
```ite
infer(<T>, (o: T), o.p)
```

### Output
```
(<Type1, Type2, T = {p: Type1 | Type2}>, Type1)
```

--------------------------------------------------------------------------------
