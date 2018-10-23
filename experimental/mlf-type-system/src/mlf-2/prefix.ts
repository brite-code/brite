import * as Immutable from 'immutable';

import {Bound} from './type';

export type PrefixBound = {
  readonly prefix: Prefix;
  readonly bound: Bound;
};

export class Prefix {
  static empty = new Prefix(Immutable.Map());

  private readonly bindings: Immutable.Map<string, PrefixBound>;

  private constructor(bindings: Immutable.Map<string, PrefixBound>) {
    this.bindings = bindings;
  }

  add(name: string, bound: Bound): Prefix {
    return new Prefix(this.bindings.set(name, {prefix: this, bound}));
  }

  find(name: string): PrefixBound {
    const prefixBound = this.bindings.get(name);
    if (prefixBound === undefined) {
      throw new Error(`Unbound type variable "${name}".`);
    }
    return prefixBound;
  }
}
