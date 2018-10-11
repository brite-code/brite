import generate from '@babel/generator';

import {parse} from '../parse';

import {serialize} from './serialize';

[
  ['λx.x', 'x => x;'],
  ['λx.λy.x y', 'x => y => x(y);'],

  // fix-point combinator
  [
    'λf.(λx.λv.f (x x) v) (λx.λv.f (x x) v)',
    `f => {\n  const x = x => v => f(x(x))(v);\n\n  return v => f(x(x))(v);\n};`,
  ],

  ['(λx.x) (λy.y)', 'const x = y => y;\n\nx;'],
  ['let x = (λy.y) in x', 'const x = y => y;\n\nx;'],
  ['(λx.(λx.x) x) (λy.y)', 'const x = y => y;\n\nconst x$2 = x;\nx$2;'],
  ['(λx.(λx.x) x) (λx.x)', 'const x = x => x;\n\nconst x$2 = x;\nx$2;'],

  [
    'let x = (λx.x) in let x = (λx.x) in (λx.x) x',
    'const x = x => x;\n\nconst x$2 = x$2 => x$2;\n\nconst x$3 = x$2;\nx$3;',
  ],

  [
    'let x = (λx.let x = x in x) in let x = (λx.let x = x in x) in x',
    'const x = x => {\n  const x$2 = x;\n  return x$2;\n};\n\nconst x$2 = x$2 => {\n  const x$3 = x$2;\n  return x$3;\n};\n\nx$2;',
  ],

  [
    'let x = (λy.y) in let x = λy.x in (λy.y)',
    'const x = y => y;\n\nconst x$2 = y => x;\n\ny => y;',
  ],

  [
    'let x = (λx.x) in let x = (λx.x) in x',
    'const x = x => x;\n\nconst x$2 = x$2 => x$2;\n\nx$2;',
  ],

  [
    'let x = (λx.x) in let x = (λx.^2) in x',
    'const x = x => x;\n\nconst x$2 = x$2 => x;\n\nx$2;',
  ],

  [
    'let x = (λy.y) in let x = (λy.y) in ^1',
    'const x = y => y;\n\nconst x$2 = y => y;\n\nx$2;',
  ],

  [
    'let x = (λy.y) in let x = (λy.y) in ^2',
    'const x = y => y;\n\nconst x$2 = y => y;\n\nx;',
  ],

  [
    'let f = λx.x in f (let y = f f f in y) (f f)',
    'const f = x => x;\n\nconst y = f(f)(f);\nf(y)(f(f));',
  ],

  [
    'let f = λx.x in f (f f) (let y = f f f in y)',
    'const f = x => x;\n\nconst $1 = f(f(f));\nconst y = f(f)(f);\n$1(y);',
  ],

  [
    'let f = λx.x in f (f f) (let y = f f f in y) (f f)',
    'const f = x => x;\n\nconst $1 = f(f(f));\nconst y = f(f)(f);\n$1(y)(f(f));',
  ],

  [
    'let f = λx.x in f (let y = f f f in y) (λz.z)',
    'const f = x => x;\n\nconst y = f(f)(f);\nf(y)(z => z);',
  ],

  [
    'let f = λx.x in f (λz.z) (let y = f f f in y)',
    'const f = x => x;\n\nconst $1 = f(z => z);\nconst y = f(f)(f);\n$1(y);',
  ],

  [
    'let f = λx.x in f ((λz.z) (let y = f f f in y))',
    'const f = x => x;\n\nconst y = f(f)(f);\nconst z = y;\nf(z);',
  ],
].forEach(([input, output]) => {
  test(input, () => {
    expect(generate(serialize(parse(input))).code).toEqual(output);
  });
});
