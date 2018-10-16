import generate from '@babel/generator';

import {parse} from '../parse';

import {serialize} from './serialize';

[
  ['λx.x', 'x => x;'],
  ['λx.λy.x y', 'x => y => x(y);'],

  // fix-point combinator
  [
    'λf.(λx.λv.f (x x) v) (λx.λv.f (x x) v)',
    `f => {
  const x = x => v => f(x(x))(v);

  return v => f(x(x))(v);
};`,
  ],

  [
    '(λx.x) (λy.y)',
    `const x = y => y;

x;`,
  ],
  [
    'let x = (λy.y) in x',
    `const x = y => y;

x;`,
  ],
  [
    '(λx.(λx.x) x) (λy.y)',
    `const x = y => y;

const x$2 = x;
x$2;`,
  ],
  [
    '(λx.(λx.x) x) (λx.x)',
    `const x = x => x;

const x$2 = x;
x$2;`,
  ],

  [
    'let x = (λx.x) in let x = (λx.x) in (λx.x) x',
    `const x = x => x;

const x$2 = x$2 => x$2;

const x$3 = x$2;
x$3;`,
  ],

  [
    'let x = (λx.let x = x in x) in let x = (λx.let x = x in x) in x',
    `const x = x => {
  const x$2 = x;
  return x$2;
};

const x$2 = x$2 => {
  const x$3 = x$2;
  return x$3;
};

x$2;`,
  ],

  [
    'let x = (λy.y) in let x = λy.x in (λy.y)',
    `const x = y => y;

const x$2 = y => x;

y => y;`,
  ],

  [
    'let x = (λx.x) in let x = (λx.x) in x',
    `const x = x => x;

const x$2 = x$2 => x$2;

x$2;`,
  ],

  [
    'let x = (λx.x) in let x = (λx.^2) in x',
    `const x = x => x;

const x$2 = x$2 => x;

x$2;`,
  ],

  [
    'let x = (λy.y) in let x = (λy.y) in ^1',
    `const x = y => y;

const x$2 = y => y;

x$2;`,
  ],

  [
    'let x = (λy.y) in let x = (λy.y) in ^2',
    `const x = y => y;

const x$2 = y => y;

x;`,
  ],

  [
    'let f = λx.x in f (let y = f f f in y) (f f)',
    `const f = x => x;

const y = f(f)(f);
f(y)(f(f));`,
  ],

  [
    'let f = λx.x in f (f f) (let y = f f f in y)',
    `const f = x => x;

const $1 = f(f(f));
const y = f(f)(f);
$1(y);`,
  ],

  [
    'let f = λx.x in f (f f) (let y = f f f in y) (f f)',
    `const f = x => x;

const $1 = f(f(f));
const y = f(f)(f);
$1(y)(f(f));`,
  ],

  [
    'let f = λx.x in f (let y = f f f in y) (λz.z)',
    `const f = x => x;

const y = f(f)(f);
f(y)(z => z);`,
  ],

  [
    'let f = λx.x in f (λz.z) (let y = f f f in y)',
    `const f = x => x;

const $1 = f(z => z);
const y = f(f)(f);
$1(y);`,
  ],

  [
    'let f = λx.x in f ((λz.z) (let y = f f f in y))',
    `const f = x => x;

const y = f(f)(f);
const z = y;
f(z);`,
  ],

  [
    'let x = λy.y in if x then x else x',
    `const x = y => y;

x ? x : x;`,
  ],

  [
    'let a = λz.z in if let b = a in b then a else a',
    `const a = z => z;

const b = a;
b ? a : a;`,
  ],

  [
    'let a = λz.z in if a then let b = a in b else a',
    `const a = z => z;

let $phi;

if (a) {
  const b = a;
  $phi = b;
} else {
  $phi = a;
}

$phi;`,
  ],

  [
    'let a = λz.z in if a then a else let b = a in b',
    `const a = z => z;

let $phi;

if (a) {
  $phi = a;
} else {
  const b = a;
  $phi = b;
}

$phi;`,
  ],

  [
    'let a = λz.z in if a then let b = a in b else let b = a in b',
    `const a = z => z;

let $phi;

if (a) {
  const b = a;
  $phi = b;
} else {
  const b = a;
  $phi = b;
}

$phi;`,
  ],

  [
    'let a = λz.z in if let b = a in b then let b = a in b else let b = a in b',
    `const a = z => z;

const b = a;
let $phi;

if (b) {
  const b$2 = a;
  $phi = b$2;
} else {
  const b$2 = a;
  $phi = b$2;
}

$phi;`,
  ],

  [
    'let a = λz.z in if a then if a then a else a else if a then a else a',
    `const a = z => z;

a ? a ? a : a : a ? a : a;`,
  ],

  [
    'let a = λz.z in if a then if a then let b = a in b else a else if a then a else a',
    `const a = z => z;

let $phi;

if (a) {
  if (a) {
    const b = a;
    $phi = b;
  } else {
    $phi = a;
  }
} else {
  $phi = a ? a : a;
}

$phi;`,
  ],

  [
    'let a = λz.z in if a then if a then a else let b = a in b else if a then a else a',
    `const a = z => z;

let $phi;

if (a) {
  if (a) {
    $phi = a;
  } else {
    const b = a;
    $phi = b;
  }
} else {
  $phi = a ? a : a;
}

$phi;`,
  ],

  [
    'let a = λz.z in if a then if a then a else a else if a then let b = a in b else a',
    `const a = z => z;

let $phi;

if (a) {
  $phi = a ? a : a;
} else {
  if (a) {
    const b = a;
    $phi = b;
  } else {
    $phi = a;
  }
}

$phi;`,
  ],

  [
    'let a = λz.z in if a then if a then a else a else if a then a else let b = a in b',
    `const a = z => z;

let $phi;

if (a) {
  $phi = a ? a : a;
} else {
  if (a) {
    $phi = a;
  } else {
    const b = a;
    $phi = b;
  }
}

$phi;`,
  ],

  ['λx.if x then x else x', 'x => x ? x : x;'],

  [
    'λa.if let b = a in b then a else a',
    `a => {
  const b = a;
  return b ? a : a;
};`,
  ],

  [
    'λa.if a then let b = a in b else a',
    `a => {
  if (a) {
    const b = a;
    return b;
  } else {
    return a;
  }
};`,
  ],

  [
    'λa.if a then a else let b = a in b',
    `a => {
  if (a) {
    return a;
  } else {
    const b = a;
    return b;
  }
};`,
  ],

  [
    'λa.if a then let b = a in b else let b = a in b',
    `a => {
  if (a) {
    const b = a;
    return b;
  } else {
    const b = a;
    return b;
  }
};`,
  ],

  [
    'λa.if let b = a in b then let b = a in b else let b = a in b',
    `a => {
  const b = a;

  if (b) {
    const b$2 = a;
    return b$2;
  } else {
    const b$2 = a;
    return b$2;
  }
};`,
  ],

  [
    'λa.if a then if a then a else a else if a then a else a',
    `a => a ? a ? a : a : a ? a : a;`,
  ],

  [
    'λa.if a then if a then let b = a in b else a else if a then a else a',
    `a => {
  if (a) {
    if (a) {
      const b = a;
      return b;
    } else {
      return a;
    }
  } else {
    return a ? a : a;
  }
};`,
  ],

  [
    'λa.if a then if a then a else let b = a in b else if a then a else a',
    `a => {
  if (a) {
    if (a) {
      return a;
    } else {
      const b = a;
      return b;
    }
  } else {
    return a ? a : a;
  }
};`,
  ],

  [
    'λa.if a then if a then a else a else if a then let b = a in b else a',
    `a => {
  if (a) {
    return a ? a : a;
  } else {
    if (a) {
      const b = a;
      return b;
    } else {
      return a;
    }
  }
};`,
  ],

  [
    'λa.if a then if a then a else a else if a then a else let b = a in b',
    `a => {
  if (a) {
    return a ? a : a;
  } else {
    if (a) {
      return a;
    } else {
      const b = a;
      return b;
    }
  }
};`,
  ],
].forEach(([input, output]) => {
  test(input, () => {
    expect(generate(serialize(parse(input))).code).toEqual(output);
  });
});
