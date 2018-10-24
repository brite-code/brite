import * as Immutable from 'immutable';

import {Bound, Polytype} from './type';

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

  find(name: string): PrefixBound | undefined {
    return this.bindings.get(name);
  }

  findOrPanic(name: string): PrefixBound {
    const prefixBound = this.bindings.get(name);
    if (prefixBound === undefined) {
      throw new Error(`Unbound type variable "${name}".`);
    }
    return prefixBound;
  }

  toDisplayString() {
    if (this.bindings.isEmpty()) return `(∅)`;
    const bounds = [];
    for (const [name, {bound}] of this.bindings) {
      if (bound.kind === 'flexible' && bound.type.kind === 'Bottom') {
        bounds.push(name);
      } else {
        const boundKind = bound.kind === 'flexible' ? '≥' : '=';
        const boundType = Polytype.toDisplayString(bound.type);
        bounds.push(`${name} ${boundKind} ${boundType}`);
      }
    }
    return `(${bounds.join(', ')})`;
  }
}
