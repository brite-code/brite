import {Diagnostics} from './diagnostics';
import {State, genMonomorphicType, testCheck} from './gen';
import {unify} from './unify';

testCheck(
  'type unifies with self',
  genMonomorphicType.then(m => m.run()),
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
