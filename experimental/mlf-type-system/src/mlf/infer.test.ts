// tslint:disable no-any
// tslint:disable no-non-null-assertion

import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {InferError, infer} from './infer';
import {Type} from './type';

const prelude = new Map<string, Type>([
  ['neg', Type.function_(Type.number, Type.number)],
  ['succ', Type.function_(Type.number, Type.number)],
  [
    'id',
    Type.quantifyUnbounded(
      'x',
      Type.function_(Type.variable('x'), Type.variable('x'))
    ),
  ],
  [
    'app',
    Type.quantifyUnbounded(
      'a',
      Type.quantifyUnbounded(
        'b',
        Type.function_(
          Type.function_(Type.variable('a'), Type.variable('b')),
          Type.function_(Type.variable('a'), Type.variable('b'))
        )
      )
    ),
  ],
  [
    'choose',
    Type.quantifyUnbounded(
      'a',
      Type.function_(
        Type.variable('a'),
        Type.function_(Type.variable('a'), Type.variable('a'))
      )
    ),
  ],
  [
    'auto',
    Type.quantify(
      'a',
      Type.rigidBound(
        Type.quantifyUnbounded(
          'a',
          Type.function_(Type.variable('a'), Type.variable('a'))
        )
      ),
      Type.function_(Type.variable('a'), Type.variable('a'))
    ),
  ],
]);

test('λ(f, x).f x', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.function_(
      'f',
      Expression.function_(
        'x',
        Expression.call(Expression.variable('f'), Expression.variable('x'))
      )
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('app neg', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(Expression.variable('app'), Expression.variable('neg'))
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('app neg 42', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.call(Expression.variable('app'), Expression.variable('neg')),
      Expression.number(42)
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('app id', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(Expression.variable('app'), Expression.variable('id'))
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('app id 42', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.call(Expression.variable('app'), Expression.variable('id')),
      Expression.number(42)
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('choose id', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(Expression.variable('choose'), Expression.variable('id'))
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('choose id 42', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.call(Expression.variable('choose'), Expression.variable('id')),
      Expression.number(42)
    )
  );
  const allDiagnostics = [...diagnostics];
  // expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('λx.x x', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.function_(
      'x',
      Expression.call(Expression.variable('x'), Expression.variable('x'))
    )
  );
  const allDiagnostics = [...diagnostics];
  // expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('λx.λy.let z = choose x y in x y', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.function_(
      'x',
      Expression.function_(
        'y',
        Expression.binding(
          'z',
          Expression.call(
            Expression.call(
              Expression.variable('choose'),
              Expression.variable('x')
            ),
            Expression.variable('y')
          ),
          Expression.call(Expression.variable('x'), Expression.variable('y'))
        )
      )
    )
  );
  const allDiagnostics = [...diagnostics];
  // expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('λx.λy.let z = choose y x in x y', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.function_(
      'x',
      Expression.function_(
        'y',
        Expression.binding(
          'z',
          Expression.call(
            Expression.call(
              Expression.variable('choose'),
              Expression.variable('y')
            ),
            Expression.variable('x')
          ),
          Expression.call(Expression.variable('x'), Expression.variable('y'))
        )
      )
    )
  );
  const allDiagnostics = [...diagnostics];
  // expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('λx.λy.let z = choose x y in y x', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.function_(
      'x',
      Expression.function_(
        'y',
        Expression.binding(
          'z',
          Expression.call(
            Expression.call(
              Expression.variable('choose'),
              Expression.variable('x')
            ),
            Expression.variable('y')
          ),
          Expression.call(Expression.variable('y'), Expression.variable('x'))
        )
      )
    )
  );
  const allDiagnostics = [...diagnostics];
  // expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('λx.λy.let z = choose y x in y x', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.function_(
      'x',
      Expression.function_(
        'y',
        Expression.binding(
          'z',
          Expression.call(
            Expression.call(
              Expression.variable('choose'),
              Expression.variable('y')
            ),
            Expression.variable('x')
          ),
          Expression.call(Expression.variable('y'), Expression.variable('x'))
        )
      )
    )
  );
  const allDiagnostics = [...diagnostics];
  // expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('λx.let x = (x: ∀a.a → a) in x x', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.function_(
      'x',
      Expression.binding(
        'x',
        Expression.annotation(
          Expression.variable('x'),
          Type.quantifyUnbounded(
            'a',
            Type.function_(Type.variable('a'), Type.variable('a'))
          )
        ),
        Expression.call(Expression.variable('x'), Expression.variable('x'))
      )
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('choose id id', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.call(Expression.variable('choose'), Expression.variable('id')),
      Expression.variable('id')
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('choose id succ', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.call(Expression.variable('choose'), Expression.variable('id')),
      Expression.variable('succ')
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('choose succ id', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.call(
        Expression.variable('choose'),
        Expression.variable('succ')
      ),
      Expression.variable('id')
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('choose id auto', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.call(Expression.variable('choose'), Expression.variable('id')),
      Expression.variable('auto')
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('choose auto id', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.call(
        Expression.variable('choose'),
        Expression.variable('auto')
      ),
      Expression.variable('id')
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('id auto', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(Expression.variable('id'), Expression.variable('auto'))
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

// NOTE: This should be ok!!!
test('(λx.x (λy.y)) auto', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.function_(
        'x',
        Expression.call(
          Expression.variable('x'),
          Expression.function_('y', Expression.variable('y'))
        )
      ),
      Expression.variable('auto')
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});

test('app auto id', () => {
  const diagnostics = new Diagnostics<InferError<never>>();
  const expression = infer(
    diagnostics,
    prelude,
    Expression.call(
      Expression.call(Expression.variable('app'), Expression.variable('auto')),
      Expression.variable('id')
    )
  );
  const allDiagnostics = [...diagnostics];
  expect(allDiagnostics).toEqual([]);
  console.log(Type.toDisplayString(expression.type));
});
