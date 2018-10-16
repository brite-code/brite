// tslint:disable no-eval

import generate from '@babel/generator';

import {serialize} from './js/serialize';
import {parseWithPrelude} from './prelude';

[
  {source: 'id', input: 42, output: 42},
  {source: 'zero', output: 0},
  {source: 'one', output: 1},
  {source: 'add one one', output: 2},
  {source: 'add one', input: 3, output: 4},
  {source: 'sub one one', output: 0},
  {source: 'λx.sub x one', input: 3, output: 2},
  {source: 'mul (add one one) (add one one)', output: 4},
  {source: 'mul (add one one)', input: 3, output: 6},
  {source: 'div one (add one one)', output: 0.5},
  {source: 'div (add (add one one) (add one one))', input: 2, output: 2},
  {source: 'eq one one', output: 1},
  {source: 'eq one zero', output: 0},
  {source: 'eq one', input: 1, output: 1},
  {source: 'eq one', input: 0, output: 0},
  {source: 'if (eq one one) then one else zero', output: 1},
  {source: 'λx.if x then one else zero', input: 1, output: 1},
  {source: 'λx.if x then one else zero', input: 0, output: 0},
].forEach(({source, input, output}) => {
  if (input !== undefined) {
    test(`${source} with ${input} == ${output}`, () => {
      const f = eval(generate(serialize(parseWithPrelude(source))).code);
      expect(f(input)).toEqual(output);
    });
  } else {
    test(`${source} == ${output}`, () => {
      const input = eval(generate(serialize(parseWithPrelude(source))).code);
      expect(input).toEqual(output);
    });
  }
});
