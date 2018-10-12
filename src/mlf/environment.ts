import * as Immutable from 'immutable';

import {Identifier, TypeIdentifier} from './identifier';
import {PolymorphicType} from './type';

export type Prefix = {
  readonly bindings: Immutable.Map<TypeIdentifier, PolymorphicType>;
};

export type Context = {
  readonly bindings: Immutable.Map<Identifier, PolymorphicType>;
};
