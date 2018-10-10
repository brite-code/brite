// tslint:disable no-eval

import generate from '@babel/generator';

import {parse} from '../parse';

import {addRuntime} from './runtime';
import {serialize} from './serialize';

[
  {
    source: 'fix (λf.λn.if (eq n zero) (const one) (λz.mul n (f (sub n one))))',
    cases: [[0, 1], [1, 1], [2, 2], [3, 6], [4, 24], [5, 120]],
  },
  {
    source:
      'fix (λf.λn.if (eq n zero) (const zero) (λz.if (eq n one) (const one) (λz.add (f (sub n one)) (f (sub (sub n one) one)))))',
    cases: [[0, 0], [1, 1], [2, 1], [3, 2], [4, 3], [5, 5], [6, 8], [7, 13]],
  },
  {
    source:
      'let fix = λf.(λx.f (λv.x x v)) (λx.f (λv.x x v)) in fix (λf.λn.if (eq n zero) (const one) (λz.mul n (f (sub n one))))',
    cases: [[0, 1], [1, 1], [2, 2], [3, 6], [4, 24], [5, 120]],
  },
].forEach(({source, cases}) => {
  test(source, () => {
    const {code} = generate(serialize(addRuntime(parse(source))));
    expect(code).toMatchSnapshot();
    const f = eval(code);
    cases.forEach(([input, output]) => {
      expect(f(input)).toEqual(output);
    });
  });
});
