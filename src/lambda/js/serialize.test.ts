import generate from '@babel/generator';
import {parse} from '../parse';
import {serialize} from './serialize';

[
  ['x', 'x'],
  ['λx.x', 'x => x'],
  ['x y', 'x(y)'],

  // fix-point combinator
  [
    'λf.(λx.λv.((f (x x)) v)) (λx.λv.((f (x x)) v))',
    'f => (x => v => f(x(x))(v))(x => v => f(x(x))(v))',
  ],
].forEach(([input, output]) => {
  test(`${input} → ${output}`, () => {
    expect(generate(serialize(parse(input))).code).toEqual(output);
  });
});
