open TestFramework

let run () = suite "JsSerialize" (fun () -> (
  let cases = [
    (
"λx.x",
"\
x => x;
"
    );
    (
"λx.λy.x y",
"\
x => y => x(y);
"
    );
    (
"λf.(λx.λv.f (x x) v) (λx.λv.f (x x) v)",
"\
f => (x => v => f(x(x))(v))(x => v => f(x(x))(v));
"
    );
    (
"λf.let x = λx.λv.f (x x) v in λv.f (x x) v",
"\
f => {
  let x = x => v => f(x(x))(v);
  return v => f(x(x))(v);
};
"
    );
    (
"(λx.x) (λy.y)",
"\
(x => x)(y => y);
"
    );
    (
"let x = λy.y in x",
"\
let x = y => y;
x;
"
    );
    (
"(λx.(λx.x) x) (λy.y)",
"\
(x => (x => x)(x))(y => y);
"
    );
    (
"let x = λy.y in let x = x in x",
"\
let x = y => y;
let x$2 = x;
x$2;
"
    );
    (
"(λx.(λx.x) x) (λx.x)",
"\
(x => (x => x)(x))(x => x);
"
    );
    (
"let x = λx.x in let x = x in x",
"\
let x = x => x;
let x$2 = x;
x$2;
"
    );
    (
"let instanceof = 42 in instanceof",
"\
let instanceof$1 = 42.;
instanceof$1;
"
    );
    (
"let instanceof = 42 in let instanceof = instanceof in instanceof",
"\
let instanceof$1 = 42.;
let instanceof$2 = instanceof$1;
instanceof$2;
"
    );
    (
"let x = (λx.x) in let x = (λx.x) in (λx.x) x",
"\
let x = x => x;
let x$2 = x => x;
(x => x)(x$2);
"
    );
    (
"let x = (λx.let x = x in x) in let x = (λx.let x = x in x) in x",
"\
let x = x => {
  let x$2 = x;
  return x$2;
};
let x$2 = x => {
  let x$2 = x;
  return x$2;
};
x$2;
"
    );
    (
"let x = (λy.y) in let x = λy.x in λy.y",
"\
let x = y => y;
let x$2 = y => x;
y => y;
"
    );
    (
"let x = (λx.x) in let x = (λx.x) in x",
"\
let x = x => x;
let x$2 = x => x;
x$2;
"
    );
    (
"let f = λx.x in f (let y = f f f in y) (f f)",
"\
let f = x => x;
let y = f(f)(f);
f(y)(f(f));
"
    );
    (
"let f = λx.x in f (f f) (let y = f f f in y)",
"\
let f = x => x;
let $tmp = f(f(f));
let y = f(f)(f);
$tmp(y);
"
    );
    (
"let f = λx.x in f (let y = f f f in y) (f f) (f f)",
"\
let f = x => x;
let y = f(f)(f);
f(y)(f(f))(f(f));
"
    );
    (
"let f = λx.x in f (f f) (let y = f f f in y) (f f)",
"\
let f = x => x;
let $tmp = f(f(f));
let y = f(f)(f);
$tmp(y)(f(f));
"
    );
    (
"let f = λx.x in f (f f) (f f) (let y = f f f in y)",
"\
let f = x => x;
let $tmp = f(f(f))(f(f));
let y = f(f)(f);
$tmp(y);
"
    );
    (
"let f = λx.x in f (f f) (let y = f f f in y) (f f) (let y = f f f in y)",
"\
let f = x => x;
let $tmp = f(f(f));
let y = f(f)(f);
let $tmp$2 = $tmp(y)(f(f));
let y$2 = f(f)(f);
$tmp$2(y$2);
"
    );
    (
"let f = λx.x in f (let y = f f f in y) (λz.z)",
"\
let f = x => x;
let y = f(f)(f);
f(y)(z => z);
"
    );
    (
"let f = λx.x in f (λz.z) (let y = f f f in y)",
"\
let f = x => x;
let $tmp = f(z => z);
let y = f(f)(f);
$tmp(y);
"
    );
    (
"let f = λx.x in f ((λz.z) (let y = f f f in y))",
"\
let f = x => x;
let y = f(f)(f);
f((z => z)(y));
"
    );
    (
"let x = true in if x then x else x",
"\
let x = true;
x ? x : x;
"
    );
    (
"let x = true in if if x then x else x then x else x",
"\
let x = true;
(x ? x : x) ? x : x;
"
    );
    (
"let x = true in if x then if x then x else x else x",
"\
let x = true;
x ? x ? x : x : x;
"
    );
    (
"let x = true in if x then x else if x then x else x",
"\
let x = true;
x ? x : x ? x : x;
"
    );
    (
"let x = true in if if x then x else x then if x then x else x else if x then x else x",
"\
let x = true;
(x ? x : x) ? x ? x : x : x ? x : x;
"
    );
    (
"let a = true in if let b = a in b then a else a",
"\
let a = true;
let b = a;
b ? a : a;
"
    );
    (
"let a = true in if a then let b = a in b else a",
"\
let a = true;
let $phi;

if (a) {
  let b = a;
  $phi = b;
} else {
  $phi = a;
}

$phi;
"
    );
    (
"let a = true in if a then a else let b = a in b",
"\
let a = true;
let $phi;

if (a) {
  $phi = a;
} else {
  let b = a;
  $phi = b;
}

$phi;
"
    );
    (
"let a = true in if a then let b = a in b else let b = a in b",
"\
let a = true;
let $phi;

if (a) {
  let b = a;
  $phi = b;
} else {
  let b = a;
  $phi = b;
}

$phi;
"
    );
    (
"let a = true in if let b = a in b then let b = a in b else let b = a in b",
"\
let a = true;
let b = a;
let $phi;

if (b) {
  let b = a;
  $phi = b;
} else {
  let b = a;
  $phi = b;
}

$phi;
"
    );
    (
"let a = true in if a then if a then a else a else if a then a else a",
"\
let a = true;
a ? a ? a : a : a ? a : a;
"
    );
    (
"λx.if x then false else true",
"\
x => x ? false : true;
"
    );
    (
"let a = true in if a then if a then let b = a in b else a else if a then a else a",
"\
let a = true;
let $phi;

if (a) {
  if (a) {
    let b = a;
    $phi = b;
  } else {
    $phi = a;
  }
} else {
  $phi = a ? a : a;
}

$phi;
"
    );
    (
"let a = true in if a then if a then a else let b = a in b else if a then a else a",
"\
let a = true;
let $phi;

if (a) {
  if (a) {
    $phi = a;
  } else {
    let b = a;
    $phi = b;
  }
} else {
  $phi = a ? a : a;
}

$phi;
"
    );
    (
"let a = true in if a then if a then a else a else if a then let b = a in b else a",
"\
let a = true;
let $phi;

if (a) {
  $phi = a ? a : a;
} else {
  if (a) {
    let b = a;
    $phi = b;
  } else {
    $phi = a;
  }
}

$phi;
"
    );
    (
"let a = true in if a then if a then a else a else if a then a else let b = a in b",
"\
let a = true;
let $phi;

if (a) {
  $phi = a ? a : a;
} else {
  if (a) {
    $phi = a;
  } else {
    let b = a;
    $phi = b;
  }
}

$phi;
"
    );
    (
"λx.if x then x else x",
"x => x ? x : x;\n"
    );
    (
"λa.if let b = a in b then a else a",
"\
a => {
  let b = a;
  return b ? a : a;
};
"
    );
    (
"λa.if a then let b = a in b else a",
"\
a => {
  if (a) {
    let b = a;
    return b;
  } else {
    return a;
  }
};
"
    );
    (
"λa.if a then a else let b = a in b",
"\
a => {
  if (a) {
    return a;
  } else {
    let b = a;
    return b;
  }
};
"
    );
    (
"λa.if a then let b = a in b else let b = a in b",
"\
a => {
  if (a) {
    let b = a;
    return b;
  } else {
    let b = a;
    return b;
  }
};
"
    );
    (
"λa.if let b = a in b then let b = a in b else let b = a in b",
"\
a => {
  let b = a;

  if (b) {
    let b = a;
    return b;
  } else {
    let b = a;
    return b;
  }
};
"
    );
    (
"λa.if a then if a then a else a else if a then a else a",
"a => a ? a ? a : a : a ? a : a;\n"
    );
    (
"λa.if a then if a then let b = a in b else a else if a then a else a",
"\
a => {
  if (a) {
    if (a) {
      let b = a;
      return b;
    } else {
      return a;
    }
  } else {
    return a ? a : a;
  }
};
"
    );
    (
"λa.if a then if a then a else let b = a in b else if a then a else a",
"\
a => {
  if (a) {
    if (a) {
      return a;
    } else {
      let b = a;
      return b;
    }
  } else {
    return a ? a : a;
  }
};
"
    );
    (
"λa.if a then if a then a else a else if a then let b = a in b else a",
"\
a => {
  if (a) {
    return a ? a : a;
  } else {
    if (a) {
      let b = a;
      return b;
    } else {
      return a;
    }
  }
};
"
    );
    (
"λa.if a then if a then a else a else if a then a else let b = a in b",
"\
a => {
  if (a) {
    return a ? a : a;
  } else {
    if (a) {
      return a;
    } else {
      let b = a;
      return b;
    }
  }
};
"
    );
  ] in

  cases |> List.iter (fun (input, output) -> (
    test input (fun () -> (
      let tokens = Parser.tokenize (Stream.of_string input) in
      let expression = Parser.parse_expression tokens in
      Stream.empty tokens;
      let program = JsSerialize.serialize expression in
      let program = JsAst.print_program program in
      assert_equal (Printf.sprintf "%S" program) (Printf.sprintf "%S" output)
    ))
  ))
))
