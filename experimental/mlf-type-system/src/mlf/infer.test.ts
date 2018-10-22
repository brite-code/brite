// tslint:disable no-any
// tslint:disable no-non-null-assertion

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

test('negative function call', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    t.callExpression(t.variableExpression('neg'), t.numberExpression(42))
  );
  const allDiagnostics = [...diagnostics];
  expect(expression).toEqual(
    t.callExpressionTyped(
      t.quantifiedType('a', t.rigidBound(t.numberType), t.variableType('a')),
      t.variableExpressionTyped(prelude.get('neg')!, 'neg'),
      t.numberExpressionTyped(42)
    )
  );
  expect(allDiagnostics).toEqual([]);
});

test('negative function call with wrong argument', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    t.callExpression(t.variableExpression('neg'), t.booleanExpression(true))
  );
  const allDiagnostics = [...diagnostics];
  expect(expression).toEqual(
    t.errorExpressionTyped(
      t.quantifiedType('a', t.rigidBound(t.numberType), t.variableType('a')),
      allDiagnostics[0] as any
    )
  );
  expect(allDiagnostics).toEqual([
    {
      kind: 'IncompatibleTypes',
      actual: t.booleanType,
      expected: t.numberType,
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
    t.callExpression(t.variableExpression('app'), t.variableExpression('neg'))
  );
  const allDiagnostics = [...diagnostics];
  expect(expression).toEqual(
    t.callExpressionTyped(
      t.quantifiedType(
        'b',
        t.rigidBound(t.numberType),
        t.quantifiedType(
          'c',
          t.rigidBound(t.numberType),
          t.quantifiedType(
            'b',
            t.rigidBound(
              t.functionType(t.variableType('b'), t.variableType('c'))
            ),
            t.quantifiedType(
              'a',
              t.rigidBound(
                t.functionType(
                  t.functionType(t.numberType, t.numberType),
                  t.variableType('b')
                )
              ),
              t.variableType('b')
            )
          )
        )
      ),
      t.variableExpressionTyped(appTyped.type, 'app'),
      t.variableExpressionTyped(prelude.get('neg')!, 'neg')
    )
  );
  expect(allDiagnostics).toEqual([]);
});

test.only('application function call with second argument', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    t.callExpression(
      t.callExpression(
        t.variableExpression('app'),
        t.variableExpression('neg')
      ),
      t.numberExpression(42)
    )
  );
  const allDiagnostics = [...diagnostics];
  const callType = t.quantifiedType(
    'b',
    t.rigidBound(t.numberType),
    t.quantifiedType(
      'c',
      t.rigidBound(t.numberType),
      t.quantifiedType(
        'b',
        t.rigidBound(t.functionType(t.variableType('b'), t.variableType('c'))),
        t.quantifiedType(
          'a',
          t.rigidBound(
            t.functionType(
              t.functionType(t.numberType, t.numberType),
              t.variableType('b')
            )
          ),
          t.variableType('b')
        )
      )
    )
  );
  console.log(Type.toDisplayString(expression.type));
  expect(allDiagnostics).toEqual([]);
  expect(expression).toEqual(
    t.callExpressionTyped(
      t.quantifiedType(
        'a',
        t.flexibleBound(callType),
        t.quantifiedType('b', t.rigidBound(t.numberType), t.variableType('b'))
      ),
      t.callExpressionTyped(
        callType,
        t.variableExpressionTyped(appTyped.type, 'app'),
        t.variableExpressionTyped(prelude.get('neg')!, 'neg')
      ),
      t.numberExpressionTyped(42)
    )
  );
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
