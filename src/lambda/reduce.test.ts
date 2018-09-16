import {parse} from './parse';
import {reduce} from './reduce';
import {native, variable, abstraction, application} from './term';

[
  ['x', 'x'],
  ['λx.x', 'λx.x'],
  ['x y', 'x y'],
  ['(λx.x) y', 'y'],
  ['(λx.x) (λy.y)', 'λy.y'],
  ['((λx.x) (λy.y)) z', 'z'],
  ['(λx.x x) y', 'y y'],
  ['(λx.(z x) x) (λy.y)', '(z (λy.y)) (λy.y)'],
  ['((λx.λx.x) y) z', 'z'],
  ['(λx.(λx.x) y)', '(λx.y)'],
  ['(λx.(λx.x) y) z', 'y'],
  ['(λx.((λx.λx.x) y) z)', '(λx.z)'],
  ['(λx.((λx.λx.x) y) z) x', 'z'],

  // fix-point combinator (Y-combinator)
  ['λg.(λx.g (x x)) (λx.g (x x))', 'λg.g ((λx.g (x x)) (λx.g (x x)))'],

  // fix-point combinator (Z-combinator)
  [
    'λf.(λx.λv.((f (x x)) v)) (λx.λv.((f (x x)) v))',
    'λf.λv.(f ((λx.λv.((f (x x)) v)) (λx.λv.((f (x x)) v)))) v',
  ],

  // factorial function
  [
    `λn.(if (equals n zero)) one (times n (f (minus n one)))`,
    `λn.(if (equals n zero)) one (times n (f (minus n one)))`,
  ],

  // fix-point combinator (Y-combinator) with factorial function
  [
    `(λg.(λx.g (x x)) (λx.g (x x))) (λf.λn.(if (equals n zero)) one (times n (f (minus n one))))`,
    `λn.if (equals n zero) one (times n (if (equals (minus n one) zero) one (times (minus n one) (((λx.λn.if (equals n zero) one (times n ((x x) (minus n one)))) (λx.λn.if (equals n zero) one ((times n) ((x x) (minus n one))))) (minus (minus n one) one)))))`,
  ],
].forEach(([input, output]) => {
  test(`${input} → ${output}`, () => {
    expect(reduce(parse(input))).toEqual(parse(output));
  });
});

test('native terms do not reduce', () => {
  const term = native(['a', 'b'], vs => vs);
  expect(reduce(term)).toBe(term);
});

test('variable not reduced is referentially equal', () => {
  const term = variable('x');
  expect(reduce(term)).toBe(term);
});

test('abstraction not reduced is referentially equal', () => {
  const term = abstraction('x', variable('x'));
  expect(reduce(term)).toBe(term);
});

test('application not reduced is referentially equal', () => {
  const term = application(variable('x'), variable('y'));
  expect(reduce(term)).toBe(term);
});
