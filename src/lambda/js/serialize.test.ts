import generate from '@babel/generator';
import {parse} from '../parse';
import {serialize} from './serialize';

[
  ['x', 'x;'],
  ['λx.x', 'x => x;'],
  ['x y', 'x(y);'],

  // fix-point combinator
  [
    'λf.(λx.λv.((f (x x)) v)) (λx.λv.((f (x x)) v))',
    `f => {\n  const x = x => v => f(x(x))(v);\n\n  return v => f(x(x))(v);\n};`,
  ],

  ['(λx.f x) y', 'const x = y;\nf(x);'],
  ['let x = y in f x', 'const x = y;\nf(x);'],
].forEach(([input, output]) => {
  test(`${input} → ${output}`, () => {
    expect(generate(serialize(parse(input))).code).toEqual(output);
  });
});
