import {gen} from 'testcheck';

import {Diagnostics} from './diagnostics';
import {genMonomorphicType, testCheck} from './gen';
import {Prefix} from './prefix';
import {BooleanType, BottomType, MonomorphicType, Type} from './type';
import {unify} from './unify';

testCheck(
  'type unifies with self',
  genMonomorphicType.then(createType => {
    const prefix = Prefix.create();
    const type = createType(prefix);
    return gen.return({prefix, type});
  }),
  ({prefix, type}) => {
    const diagnostics = new Diagnostics();
    const {error} = unify(diagnostics, prefix, type, type);
    return error === undefined;
  }
);

testCheck(
  'type unifies with structurally identical self',
  genMonomorphicType.then(createType => {
    const prefix = Prefix.create();
    const actual = createType(prefix);
    const expected = createType(prefix);
    return gen.return({prefix, actual, expected});
  }),
  ({prefix, actual, expected}) => {
    const diagnostics = new Diagnostics();
    const {error} = unify(diagnostics, prefix, actual, expected);
    return error === undefined;
  }
);

testCheck(
  'unify solves type variables',
  genMonomorphicType.then(createType => {
    const prefix = Prefix.create();
    const type = createType(prefix);
    return gen.return({prefix, type});
  }),
  ({prefix, type: actual}) => {
    const diagnostics = new Diagnostics();
    const identifier = prefix.add({
      kind: 'flexible',
      type: BottomType,
    });
    const expected: Type = {kind: 'Variable', identifier};
    const {error} = unify(diagnostics, prefix, actual, expected);
    const bound = prefix.find(identifier);
    if (
      error !== undefined ||
      (bound.kind === 'flexible' && bound.type.kind === 'Bottom')
    ) {
      return false;
    }
    return true;
  }
);

// Example 4 from the [MLF paper][1] we implement.
//
// [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
test('example 4', () => {
  const diagnostics = new Diagnostics();
  const prefix = Prefix.create();
  const identifier1 = prefix.add({kind: 'flexible', type: BottomType});
  const type1: MonomorphicType = BooleanType;
  const type2: MonomorphicType = {kind: 'Variable', identifier: identifier1};
  const identifier2 = prefix.add({
    kind: 'flexible',
    type: {
      kind: 'Quantified',
      prefix: new Map([[identifier1, prefix.find(identifier1)]]),
      body: type2,
    },
  });
  const actual: MonomorphicType = {
    kind: 'Function',
    parameter: type1,
    body: {kind: 'Variable', identifier: identifier2},
  };
  const expected: MonomorphicType = {
    kind: 'Function',
    parameter: type1,
    body: type2,
  };
  const {error} = unify(diagnostics, prefix, actual, expected);
  expect(error).toBeUndefined();
});
