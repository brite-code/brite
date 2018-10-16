import * as Immutable from 'immutable';

import * as t from './builder';
import {Diagnostics} from './diagnostics';
import {InferError, infer} from './infer';
import {Type} from './type';

const prelude = Immutable.Map<string, Type>([
  [
    'add',
    t.functionType(t.numberType, t.functionType(t.numberType, t.numberType)),
  ],
]);

const app = t.functionExpression(
  'f',
  t.functionExpression(
    'x',
    t.callExpression(t.variableExpression('f'), t.variableExpression('x'))
  )
);

test('application function', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  expect(infer(diagnostics, prelude, app).type).toEqual(
    t.quantifiedType(
      'b',
      t.flexibleBound(t.bottomType),
      t.quantifiedType(
        'c',
        t.flexibleBound(t.bottomType),
        t.quantifiedType(
          'a',
          t.rigidBound(
            t.functionType(t.variableType('b'), t.variableType('c'))
          ),
          t.functionType(
            t.variableType('a'),
            t.functionType(t.variableType('b'), t.variableType('c'))
          )
        )
      )
    )
  );
  expect([...diagnostics].length).toEqual(0);
});
