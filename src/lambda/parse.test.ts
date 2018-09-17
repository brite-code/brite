import {variable, abstraction, application, binding} from './term';
import {parse} from './parse';

const v = variable;
const f = abstraction;
const c = application;
const b = binding;

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
  ['let a = b in c', b('a', v('b'), v('c'))],
  ['let a = b in c d', b('a', v('b'), c(v('c'), v('d')))],
  ['let a = b in λc.c', b('a', v('b'), f('c', v('c')))],
  ['let a = b c in d', b('a', c(v('b'), v('c')), v('d'))],
  ['let a = λb.b in c', b('a', f('b', v('b')), v('c'))],
  ['(let a = b in c) d', c(b('a', v('b'), v('c')), v('d'))],
  ['λb.let a = b in c', f('b', b('a', v('b'), v('c')))],
  ['let a = let b = c in d in e', b('a', b('b', v('c'), v('d')), v('e'))],
  ['let a = b in let c = d in e', b('a', v('b'), b('c', v('d'), v('e')))],
].forEach(([input, output]: any) => {
  test(input, () => {
    expect(parse(input)).toEqual(output);
  });
});
