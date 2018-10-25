// tslint:disable no-any
// tslint:disable no-non-null-assertion

import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {InferError, infer} from './infer';
import {Type} from './type';

const app = Expression.function_(
  'f',
  Expression.function_(
    'x',
    Expression.call(Expression.variable('f'), Expression.variable('x'))
  )
);

const appTyped = Expression.Typed.function_(
  Type.quantifyUnbounded(
    'b',
    Type.quantifyUnbounded(
      'c',
      Type.quantify(
        'a',
        Type.rigidBound(Type.function_(Type.variable('b'), Type.variable('c'))),
        Type.function_(
          Type.variable('a'),
          Type.function_(Type.variable('b'), Type.variable('c'))
        )
      )
    )
  ),
  'f',
  Expression.Typed.function_(
    Type.function_(Type.variable('b'), Type.variable('c')),
    'x',
    Expression.Typed.call(
      Type.variable('c'),
      Expression.Typed.variable(Type.variable('a'), 'f'),
      Expression.Typed.variable(Type.variable('b'), 'x')
    )
  )
);

const prelude = new Map<string, Type>([
  ['neg', Type.function_(Type.number, Type.number)],
  [
    'add',
    Type.function_(Type.number, Type.function_(Type.number, Type.number)),
  ],
  ['app', appTyped.type],
]);

test('negative function call', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(Expression.variable('neg'), Expression.number(42))
  );
  const allDiagnostics = [...diagnostics];
  expect(expression).toEqual(
    Expression.Typed.call(
      Type.quantify('a', Type.rigidBound(Type.number), Type.variable('a')),
      Expression.Typed.variable(prelude.get('neg')!, 'neg'),
      Expression.Typed.number(42)
    )
  );
  expect(allDiagnostics).toEqual([]);
});

test('negative function call with wrong argument', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(Expression.variable('neg'), Expression.boolean(true))
  );
  const allDiagnostics = [...diagnostics];
  expect(expression).toEqual(
    Expression.Typed.error(
      Type.quantify('a', Type.rigidBound(Type.number), Type.variable('a')),
      allDiagnostics[0] as any
    )
  );
  expect(allDiagnostics).toEqual([
    {
      kind: 'IncompatibleTypes',
      actual: Type.boolean,
      expected: Type.number,
    },
  ]);
});

test('application function', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  expect(infer(diagnostics, prelude, app)).toEqual(appTyped);
  expect([...diagnostics]).toEqual([]);
});

test('application function call', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(Expression.variable('app'), Expression.variable('neg'))
  );
  const allDiagnostics = [...diagnostics];
  expect(expression).toEqual(
    Expression.Typed.call(
      Type.quantify(
        'b',
        Type.rigidBound(Type.number),
        Type.quantify(
          'c',
          Type.rigidBound(Type.number),
          Type.quantify(
            'b',
            Type.rigidBound(
              Type.function_(Type.variable('b'), Type.variable('c'))
            ),
            Type.quantify(
              'a',
              Type.rigidBound(
                Type.function_(
                  Type.function_(Type.number, Type.number),
                  Type.variable('b')
                )
              ),
              Type.variable('b')
            )
          )
        )
      ),
      Expression.Typed.variable(prelude.get('app')!, 'app'),
      Expression.Typed.variable(prelude.get('neg')!, 'neg')
    )
  );
  expect(allDiagnostics).toEqual([]);
});

test.only('application function call with second argument', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    // t.callExpression(
    Expression.call(Expression.variable('app'), Expression.variable('neg'))
    // t.numberExpression(42)
    // )
  );
  const allDiagnostics = [...diagnostics];
  // const callType = Type.quantify(
  //   'b',
  //   Type.rigidBound(Type.number),
  //   Type.quantify(
  //     'c',
  //     Type.rigidBound(Type.number),
  //     Type.quantify(
  //       'b',
  //       Type.rigidBound(Type.function_(Type.variable('b'), Type.variable('c'))),
  //       Type.quantify(
  //         'a',
  //         Type.rigidBound(
  //           Type.function_(
  //             Type.function_(Type.number, Type.number),
  //             Type.variable('b')
  //           )
  //         ),
  //         Type.variable('b')
  //       )
  //     )
  //   )
  // );
  console.log(Type.toDisplayString(expression.type));
  expect(allDiagnostics).toEqual([]);
  // expect(expression).toEqual(
  //   t.callExpressionTyped(
  //     t.quantifiedType(
  //       'a',
  //       t.flexibleBound(callType),
  //       t.quantifiedType('b', t.rigidBound(t.numberType), t.variableType('b'))
  //     ),
  //     t.callExpressionTyped(
  //       callType,
  //       t.variableExpressionTyped(appTyped.type, 'app'),
  //       t.variableExpressionTyped(prelude.get('neg')!, 'neg')
  //     ),
  //     t.numberExpressionTyped(42)
  //   )
  // );
});

// test('call with wrong argument', () => {
//   const diagnostics = new Diagnostics<InferError<never>>();
//   const expression = infer(
//     diagnostics,
//     prelude,
//     t.callExpression(t.variableExpression('app'), t.numberExpression(42))
//   );
//   const allDiagnostics = [...diagnostics];
//   expect(expression).toEqual(
//     t.errorExpressionTyped(
//       t.quantifiedType(
//         'a',
//         t.rigidBound(t.functionType(t.numberType, t.variableType('b'))),
//         t.quantifiedType(
//           'b',
//           t.rigidBound(
//             t.functionType(t.variableType('b'), t.variableType('c'))
//           ),
//           t.variableType('b')
//         )
//       ),
//       allDiagnostics[0] as any
//     )
//   );
//   expect(allDiagnostics).toEqual([
//     {
//       kind: 'IncompatibleTypes',
//       actual: t.numberType,
//       expected: t.functionType(t.variableType('b'), t.variableType('c')),
//     },
//   ]);
// });
