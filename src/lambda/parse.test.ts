import {variable, abstraction, application} from './term';
import {parse} from './parse';

const v = variable;
const f = abstraction;
const c = application;

[
  ['x', v('x')],
  ['λx.x', f('x', v('x'))],
  ['x y', c(v('x'), v('y'))],
  ['(x)', v('x')],
  ['(λx.x)', f('x', v('x'))],
  ['(x y)', c(v('x'), v('y'))],
  ['λy.λx.x', f('y', f('x', v('x')))],
  ['λx.x y', f('x', c(v('x'), v('y')))],
  ['(λx.x) y', c(f('x', v('x')), v('y'))],
  ['y (λx.x)', c(v('y'), f('x', v('x')))],
  ['a b c', c(c(v('a'), v('b')), v('c'))],
  ['(a b) c', c(c(v('a'), v('b')), v('c'))],
  ['a (b c)', c(v('a'), c(v('b'), v('c')))],
].forEach(([input, output]: any) => {
  test(input, () => {
    expect(parse(input)).toEqual(output);
  });
});
