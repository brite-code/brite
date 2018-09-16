import {getFreeVariables, abstraction, native} from './term';
import {parse} from './parse';

describe('getFreeVariables', () => {
  [
    ['x', ['x']],
    ['λx.x', []],
    ['x y', ['x', 'y']],
    ['(λx.x) x', ['x']],
    ['x (λx.x)', ['x']],
  ].forEach(([input, outputArray]: any) => {
    const output = new Set(outputArray);
    test(`${input} → {${[...output].join(', ')}}`, () => {
      expect(getFreeVariables(parse(input))).toEqual(output);
    });
  });

  test('native variables are free if not bound', () => {
    const term1 = abstraction(
      'x',
      native(['x', 'y'], () => {
        throw new Error('unreachable');
      }),
    );
    const term2 = abstraction(
      'y',
      native(['x', 'y'], () => {
        throw new Error('unreachable');
      }),
    );
    expect(getFreeVariables(term1)).toEqual(new Set(['y']));
    expect(getFreeVariables(term2)).toEqual(new Set(['x']));
  });
});
