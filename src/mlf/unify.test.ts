import {Diagnostics} from './diagnostics';
import {genMonomorphicType, testCheck} from './gen';
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
