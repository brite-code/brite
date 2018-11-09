// tslint:disable no-eval

import generate from '@babel/generator';

import {parseWithPrelude} from '../prelude';

import {serialize} from './serialize';

[
  {
    source: 'fix (λf.λn.if (eq n zero) then one else mul n (f (sub n one)))',
    cases: [[0, 1], [1, 1], [2, 2], [3, 6], [4, 24], [5, 120]],
  },
  {
    source:
      'fix (λf.λn.if (eq n zero) then zero else if (eq n one) then one else add (f (sub n one)) (f (sub (sub n one) one)))',
    cases: [[0, 0], [1, 1], [2, 1], [3, 2], [4, 3], [5, 5], [6, 8], [7, 13]],
  },
  {
    source:
      'let fix = λf.(λx.f (λv.x x v)) (λx.f (λv.x x v)) in fix (λf.λn.if (eq n zero) then one else mul n (f (sub n one)))',
    cases: [[0, 1], [1, 1], [2, 2], [3, 6], [4, 24], [5, 120]],
  },
].forEach(({source, cases}) => {
  test(source, () => {
    const {code} = generate(serialize(parseWithPrelude(source)));
    expect(code).toMatchSnapshot();
    const f = eval(code);
    cases.forEach(([input, output]) => {
      expect(f(input)).toEqual(output);
    });
  });
});
