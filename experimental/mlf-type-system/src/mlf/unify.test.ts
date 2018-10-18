import * as t from './builder';
import {Diagnostics} from './diagnostics';
import {Prefix} from './prefix';
import {unify} from './unify';

const constantTypes = [t.booleanType, t.numberType, t.stringType];

test('constant types unify with each other', () => {
  constantTypes.forEach(type => {
    const diagnostics = new Diagnostics();
    const prefix = new Prefix();
    expect(unify(diagnostics, prefix, type, type)).toEqual(undefined);
    expect([...diagnostics].length).toEqual(0);
  });
});

test('constant types do not unify with different constant types', () => {
  constantTypes.forEach((actual, i) => {
    constantTypes.forEach((expected, j) => {
      if (i !== j) {
        const diagnostics = new Diagnostics();
        const prefix = new Prefix();
        expect(unify(diagnostics, prefix, actual, expected)).toEqual({
          kind: 'IncompatibleTypes',
          actual,
          expected,
        });
        expect([...diagnostics].length).toEqual(1);
      }
    });
  });
});

test('functions are ok if parameter and body are the same', () => {
  constantTypes.forEach(parameter => {
    constantTypes.forEach(body => {
      const diagnostics = new Diagnostics();
      const prefix = new Prefix();
      expect(
        unify(
          diagnostics,
          prefix,
          t.functionType(parameter, body),
          t.functionType(parameter, body)
        )
      ).toEqual(undefined);
      expect([...diagnostics].length).toEqual(0);
    });
  });
});

test('functions are not ok if parameter and body are not the same', () => {
  constantTypes.forEach((parameter1, i1) => {
    constantTypes.forEach((body1, j1) => {
      constantTypes.forEach((parameter2, i2) => {
        constantTypes.forEach((body2, j2) => {
          if (i1 !== i2 && j1 !== j2) {
            const diagnostics = new Diagnostics();
            const prefix = new Prefix();
            expect(
              unify(
                diagnostics,
                prefix,
                t.functionType(parameter1, body1),
                t.functionType(parameter2, body2)
              )
            ).toEqual({
              kind: 'IncompatibleTypes',
              actual: parameter2,
              expected: parameter1,
            });
            expect([...diagnostics]).toEqual([
              {
                kind: 'IncompatibleTypes',
                actual: parameter2,
                expected: parameter1,
              },
              {
                kind: 'IncompatibleTypes',
                actual: body1,
                expected: body2,
              },
            ]);
          }
        });
      });
    });
  });
});

test('monomorphic rigid actual variable unifies with same type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push('x', t.rigidBound(t.booleanType));
  expect(
    unify(diagnostics, prefix, t.variableType('x'), t.booleanType)
  ).toEqual(undefined);
  expect(prefix.get('x')).toEqual(t.rigidBound(t.booleanType));
  expect([...diagnostics].length).toEqual(0);
});

test('monomorphic flexible actual variable unifies with same type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push('x', t.flexibleBound(t.booleanType));
  expect(
    unify(diagnostics, prefix, t.variableType('x'), t.booleanType)
  ).toEqual(undefined);
  expect(prefix.get('x')).toEqual(t.flexibleBound(t.booleanType));
  expect([...diagnostics].length).toEqual(0);
});

test('monomorphic rigid actual variable does not unify with different type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push('x', t.rigidBound(t.booleanType));
  expect(unify(diagnostics, prefix, t.variableType('x'), t.numberType)).toEqual(
    {
      kind: 'IncompatibleTypes',
      actual: t.booleanType,
      expected: t.numberType,
    }
  );
  expect(prefix.get('x')).toEqual(t.rigidBound(t.booleanType));
  expect([...diagnostics].length).toEqual(1);
});

test('monomorphic flexible actual variable does not unify with different type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push('x', t.flexibleBound(t.booleanType));
  expect(unify(diagnostics, prefix, t.variableType('x'), t.numberType)).toEqual(
    {
      kind: 'IncompatibleTypes',
      actual: t.booleanType,
      expected: t.numberType,
    }
  );
  expect(prefix.get('x')).toEqual(t.flexibleBound(t.booleanType));
  expect([...diagnostics].length).toEqual(1);
});

test('monomorphic rigid expected variable unifies with same type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push('x', t.rigidBound(t.booleanType));
  expect(
    unify(diagnostics, prefix, t.booleanType, t.variableType('x'))
  ).toEqual(undefined);
  expect(prefix.get('x')).toEqual(t.rigidBound(t.booleanType));
  expect([...diagnostics].length).toEqual(0);
});

test('monomorphic flexible expected variable unifies with same type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push('x', t.flexibleBound(t.booleanType));
  expect(
    unify(diagnostics, prefix, t.booleanType, t.variableType('x'))
  ).toEqual(undefined);
  expect(prefix.get('x')).toEqual(t.flexibleBound(t.booleanType));
  expect([...diagnostics].length).toEqual(0);
});

test('monomorphic rigid expected variable does not unify with different type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push('x', t.rigidBound(t.booleanType));
  expect(unify(diagnostics, prefix, t.numberType, t.variableType('x'))).toEqual(
    {
      kind: 'IncompatibleTypes',
      actual: t.numberType,
      expected: t.booleanType,
    }
  );
  expect(prefix.get('x')).toEqual(t.rigidBound(t.booleanType));
  expect([...diagnostics].length).toEqual(1);
});

test('monomorphic flexible expected variable does not unify with different type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push('x', t.flexibleBound(t.booleanType));
  expect(unify(diagnostics, prefix, t.numberType, t.variableType('x'))).toEqual(
    {
      kind: 'IncompatibleTypes',
      actual: t.numberType,
      expected: t.booleanType,
    }
  );
  expect(prefix.get('x')).toEqual(t.flexibleBound(t.booleanType));
  expect([...diagnostics].length).toEqual(1);
});

test('quantified monomorphic rigid actual variable unifies with same type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push(
    'x',
    t.rigidBound(
      t.quantifiedType('y', t.rigidBound(t.booleanType), t.variableType('y'))
    )
  );
  expect(
    unify(diagnostics, prefix, t.variableType('x'), t.booleanType)
  ).toEqual(undefined);
  expect(prefix.get('x')).toEqual(t.rigidBound(t.booleanType));
  expect([...diagnostics].length).toEqual(0);
});

test('quantified monomorphic flexible actual variable unifies with same type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push(
    'x',
    t.rigidBound(
      t.quantifiedType('y', t.flexibleBound(t.booleanType), t.variableType('y'))
    )
  );
  expect(
    unify(diagnostics, prefix, t.variableType('x'), t.booleanType)
  ).toEqual(undefined);
  expect(prefix.get('x')).toEqual(t.rigidBound(t.booleanType));
  expect([...diagnostics].length).toEqual(0);
});

test('quantified monomorphic rigid actual variable does not unify with different type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  const bound = t.rigidBound(
    t.quantifiedType('y', t.rigidBound(t.booleanType), t.variableType('y'))
  );
  prefix.push('x', bound);
  expect(unify(diagnostics, prefix, t.variableType('x'), t.numberType)).toEqual(
    {
      kind: 'IncompatibleTypes',
      actual: t.booleanType,
      expected: t.numberType,
    }
  );
  expect(prefix.get('x')).toEqual(bound);
  expect([...diagnostics].length).toEqual(1);
});

test('quantified monomorphic flexible actual variable does not unify with different type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  const bound = t.rigidBound(
    t.quantifiedType('y', t.flexibleBound(t.booleanType), t.variableType('y'))
  );
  prefix.push('x', bound);
  expect(unify(diagnostics, prefix, t.variableType('x'), t.numberType)).toEqual(
    {
      kind: 'IncompatibleTypes',
      actual: t.booleanType,
      expected: t.numberType,
    }
  );
  expect(prefix.get('x')).toEqual(bound);
  expect([...diagnostics].length).toEqual(1);
});

test('quantified monomorphic rigid expected variable unifies with same type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push(
    'x',
    t.rigidBound(
      t.quantifiedType('y', t.rigidBound(t.booleanType), t.variableType('y'))
    )
  );
  expect(
    unify(diagnostics, prefix, t.booleanType, t.variableType('x'))
  ).toEqual(undefined);
  expect(prefix.get('x')).toEqual(t.rigidBound(t.booleanType));
  expect([...diagnostics].length).toEqual(0);
});

test('quantified monomorphic flexible expected variable unifies with same type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  prefix.push(
    'x',
    t.rigidBound(
      t.quantifiedType('y', t.flexibleBound(t.booleanType), t.variableType('y'))
    )
  );
  expect(
    unify(diagnostics, prefix, t.booleanType, t.variableType('x'))
  ).toEqual(undefined);
  expect(prefix.get('x')).toEqual(t.rigidBound(t.booleanType));
  expect([...diagnostics].length).toEqual(0);
});

test('quantified monomorphic rigid expected variable does not unify with different type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  const bound = t.rigidBound(
    t.quantifiedType('y', t.rigidBound(t.booleanType), t.variableType('y'))
  );
  prefix.push('x', bound);
  expect(unify(diagnostics, prefix, t.numberType, t.variableType('x'))).toEqual(
    {
      kind: 'IncompatibleTypes',
      actual: t.numberType,
      expected: t.booleanType,
    }
  );
  expect(prefix.get('x')).toEqual(bound);
  expect([...diagnostics].length).toEqual(1);
});

test('quantified monomorphic flexible expected variable does not unify with different type', () => {
  const diagnostics = new Diagnostics();
  const prefix = new Prefix();
  const bound = t.rigidBound(
    t.quantifiedType('y', t.flexibleBound(t.booleanType), t.variableType('y'))
  );
  prefix.push('x', bound);
  expect(unify(diagnostics, prefix, t.numberType, t.variableType('x'))).toEqual(
    {
      kind: 'IncompatibleTypes',
      actual: t.numberType,
      expected: t.booleanType,
    }
  );
  expect(prefix.get('x')).toEqual(bound);
  expect([...diagnostics].length).toEqual(1);
});
