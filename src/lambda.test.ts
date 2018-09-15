import {parse, reduce} from './lambda';

describe('reduce', () => {
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

    // fix-point combinator
    ['λg.(λx.g (x x)) (λx.g (x x))', 'λg.g ((λx.g (x x)) (λx.g (x x)))'],

    // factorial function
    [
      `λn.(if (equals n zero))
        one
        (times n (f (minus n one)))`,
      `λn.(if (equals n zero))
        one
        (times n (f (minus n one)))`,
    ],

    // fix-point combinator with factorial function
    [
      `(λg.(λx.g (x x)) (λx.g (x x)))
        (λf.λn.(if (equals n zero))
          one
          (times n (f (minus n one))))`,
      `λn.if (equals n zero)
        one
        (times n
          (if (equals (minus n one) zero)
            one
            (times (minus n one)
              (((λx.λn.if (equals n zero)
                  one
                  (times n ((x x) (minus n one))))
                (λx.λn.if (equals n zero)
                  one
                  ((times n) ((x x) (minus n one)))))
                (minus (minus n one) one)))))`,
    ],
  ].forEach(([input, output]) => {
    test(`${input} → ${output}`, () => {
      expect(reduce(parse(input))).toEqual(parse(output));
    });
  });
});
