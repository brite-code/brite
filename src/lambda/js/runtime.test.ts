// tslint:disable no-eval

import generate from '@babel/generator';

import {parse} from '../parse';

import {addRuntime} from './runtime';
import {serialize} from './serialize';

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
  {source: 'eq one one', output: true},
  {source: 'eq one zero', output: false},
  {source: 'eq one', input: 1, output: true},
  {source: 'eq one', input: 0, output: false},
  {source: 'if (eq one one) (const one) (const zero)', output: 1},
  {source: 'λx.if x (const one) (const zero)', input: true, output: 1},
  {source: 'λx.if x (const one) (const zero)', input: false, output: 0},
].forEach(({source, input, output}) => {
  if (input !== undefined) {
    test(`(${source}) ${input} == ${output}`, () => {
      const f = eval(generate(serialize(addRuntime(parse(source)))).code);
      expect(f(input)).toEqual(output);
    });
  } else {
    test(`${source} == ${output}`, () => {
      const input = eval(generate(serialize(addRuntime(parse(source)))).code);
      expect(input).toEqual(output);
    });
  }
});

test('only adds referenced variables', () => {
  expect(addRuntime(parse('id (λv.v)'))).toEqual(
    parse('(λid.id (λv.v)) (λx.x)'),
  );
});
