// tslint:disable no-any

import {BindingMap} from './bindings';
import * as t from './builder';
import {Diagnostics} from './diagnostics';
import {InferError, infer} from './infer';
import {Type} from './type';

const app = t.functionExpression(
  'f',
  t.functionExpression(
    'x',
    t.callExpression(t.variableExpression('f'), t.variableExpression('x'))
  )
);

const appTyped = t.functionExpressionTyped(
  t.quantifiedType(
    'b',
    t.flexibleBound(t.bottomType),
    t.quantifiedType(
      'c',
      t.flexibleBound(t.bottomType),
      t.quantifiedType(
        'a',
        t.rigidBound(t.functionType(t.variableType('b'), t.variableType('c'))),
        t.functionType(
          t.variableType('a'),
          t.functionType(t.variableType('b'), t.variableType('c'))
        )
      )
    )
  ),
  'f',
  t.functionExpressionTyped(
    t.functionType(t.variableType('b'), t.variableType('c')),
    'x',
    t.callExpressionTyped(
      t.variableType('c'),
      t.variableExpressionTyped(t.variableType('a'), 'f'),
      t.variableExpressionTyped(t.variableType('b'), 'x')
    )
  )
);

const prelude = new BindingMap<string, Type>([
  ['neg', t.functionType(t.numberType, t.numberType)],
  [
    'add',
    t.functionType(t.numberType, t.functionType(t.numberType, t.numberType)),
  ],
  ['app', appTyped.type],
]);

test.skip('application function', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  expect(infer(diagnostics, prelude, app)).toEqual(appTyped);
  expect([...diagnostics]).toEqual([]);
});

test('application function call', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    t.callExpression(t.variableExpression('app'), t.variableExpression('neg'))
  );
  console.log(Type.toDisplayString(infer(diagnostics, prelude, app).type));
  console.log(Type.toDisplayString(expression.type));
  // const allDiagnostics = [...diagnostics];
  // expect(expression).toEqual(
  //   t.callExpressionTyped(
  //     undefined,
  //     t.variableExpressionTyped(appTyped.type, 'app'),
  //     t.numberExpressionTyped(42)
  //   )
  // );
  // expect(allDiagnostics).toEqual([]);
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
