import {gen} from 'testcheck';

import {Diagnostics} from './diagnostics';
import {State, genMonomorphicType, testCheck} from './gen';
import {BottomType, Type} from './type';
import {unify} from './unify';

testCheck(
  'type unifies with self',
  genMonomorphicType.then(m => gen.return(m.run())),
  ({value: type, prefix}) => {
    const diagnostics = Diagnostics.create();
    const {error} = unify(diagnostics, prefix, type, type);
    return error === undefined;
  }
);

testCheck(
  'type unifies with structurally identical self',
  genMonomorphicType.then(m =>
    State.then2(m, m, (actual, expected) =>
      State.return({actual, expected})
    ).run()
  ),
  ({value: {actual, expected}, prefix}) => {
    const diagnostics = Diagnostics.create();
    const {error} = unify(diagnostics, prefix, actual, expected);
    return error === undefined;
  }
);

testCheck(
  'unify solves type variables',
  genMonomorphicType.then(m =>
    gen.return({actual: m.run(), expected: m.run()})
  ),
  ({actual, expected}) => {
    {
      const diagnostics = Diagnostics.create();
      const {prefix: prefix1, identifier} = actual.prefix.add({
        kind: 'flexible',
        type: BottomType,
      });
      const expected: Type = {kind: 'Variable', identifier};
      const {prefix: prefix2, error} = unify(
        diagnostics,
        prefix1,
        actual.value,
        expected
      );
      const bound = prefix2.find(identifier);
      if (
        error !== undefined ||
        (bound.kind === 'flexible' && bound.type.kind === 'Bottom')
      ) {
        return false;
      }
    }
    {
      const diagnostics = Diagnostics.create();
      const {prefix: prefix1, identifier} = expected.prefix.add({
        kind: 'flexible',
        type: BottomType,
      });
      const actual: Type = {kind: 'Variable', identifier};
      const {prefix: prefix2, error} = unify(
        diagnostics,
        prefix1,
        actual,
        expected.value
      );
      const bound = prefix2.find(identifier);
      if (
        error !== undefined ||
        (bound.kind === 'flexible' && bound.type.kind === 'Bottom')
      ) {
        return false;
      }
    }
    return true;
  }
);
