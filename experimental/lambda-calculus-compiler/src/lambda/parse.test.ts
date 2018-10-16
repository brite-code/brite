import {parse} from './parse';
import {
  Term,
  abstraction,
  application,
  binding,
  conditional,
  variable,
} from './term';

const v = variable;
const f = abstraction;
const c = application;
const b = binding;
const t = conditional;

const cases: ReadonlyArray<[string, Term]> = [
  ['x', v(1)],
  ['λx.x', f('x', v(1))],
  ['x y', c(v(1), v(2))],
  ['(x)', v(1)],
  ['(λx.x)', f('x', v(1))],
  ['(x y)', c(v(1), v(2))],
  ['λy.λx.x', f('y', f('x', v(1)))],
  ['λx.x y', f('x', c(v(1), v(3)))],
  ['(λx.x) y', c(f('x', v(1)), v(2))],
  ['y (λx.x)', c(v(2), f('x', v(1)))],
  ['a b c', c(c(v(3), v(4)), v(5))],
  ['(a b) c', c(c(v(3), v(4)), v(5))],
  ['a (b c)', c(v(3), c(v(4), v(5)))],
  ['let a = b in c', b('a', v(4), v(6))],
  ['let a = b in c d', b('a', v(4), c(v(6), v(7)))],
  ['let a = b in λc.c', b('a', v(4), f('c', v(1)))],
  ['let a = b c in d', b('a', c(v(4), v(5)), v(7))],
  ['let a = λb.b in c', b('a', f('b', v(1)), v(6))],
  ['(let a = b in c) d', c(b('a', v(4), v(6)), v(6))],
  ['λb.let a = b in c', f('b', b('a', v(1), v(7)))],
  ['let a = let b = c in d in e', b('a', b('b', v(5), v(7)), v(8))],
  ['let a = b in let c = d in e', b('a', v(4), b('c', v(7), v(9)))],
  ['λx.^1', f('x', v(1))],
  ['λx.λx.^1', f('x', f('x', v(1)))],
  ['λx.λx.^2', f('x', f('x', v(2)))],
  ['λx y.x', f('x', f('y', v(2)))],
  ['λx y.y', f('x', f('y', v(1)))],
  ['if a then b else c', t(v(3), v(4), v(5))],
];

cases.forEach(([input, output]) => {
  test(input, () => {
    const scope = ['x', 'y', 'a', 'b', 'c', 'd', 'e'];
    expect(parse(input, scope)).toEqual(output);
  });
});
