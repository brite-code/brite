import {parse} from './parse';
import {abstraction, getFreeVariables, native, variable} from './term';

describe('getFreeVariables', () => {
  const cases: ReadonlyArray<[string, ReadonlyArray<string>]> = [
    ['x', ['x']],
    ['λx.x', []],
    ['x y', ['x', 'y']],
    ['(λx.x) x', ['x']],
    ['x (λx.x)', ['x']],
  ];

  cases.forEach(([input, outputArray]) => {
    const output = new Set(outputArray);
    test(`${input} → {${[...output].join(', ')}}`, () => {
      expect(getFreeVariables(parse(input))).toEqual(output);
    });
  });

  test('native variables are free if not bound', () => {
    const term1 = abstraction(
      'x',
      native([variable('x'), variable('y')], () => {
        throw new Error('unreachable');
      }),
    );
    const term2 = abstraction(
      'y',
      native([variable('x'), variable('y')], () => {
        throw new Error('unreachable');
      }),
    );
    expect(getFreeVariables(term1)).toEqual(new Set(['y']));
    expect(getFreeVariables(term2)).toEqual(new Set(['x']));
  });
});
