import * as Immutable from 'immutable';

import {Bound, Polytype} from './type';

/**
 * A prefix is a sequence of bindings where each successive binding only has
 * access to the previous bindings. The term “prefix” comes from the
 * [MLF paper][1]. The name makes sense as we “prefix” polytypes with
 * quantified bindings.
 *
 * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
 */
export class Prefix {
  /**
   * An empty prefix with no bindings.
   */
  static empty = new Prefix(Immutable.Map());

  // Every binding in our prefix holds a reference to the prefix when it was
  // added. This way we may properly resolve the variables in the bound.
  // Otherwise consider an equivalence check like ∀(x = unit, x = x).x ≡ unit.
  // If the bound x = x does not resolve the right-hand-side x to x = unit then
  // we are at risk of recursing infinitely.
  private readonly bindings: Immutable.Map<string, PrefixBound>;

  private constructor(bindings: Immutable.Map<string, PrefixBound>) {
    this.bindings = bindings;
  }

  /**
   * Adds a bound to the prefix.
   */
  add(name: string, bound: Bound): Prefix {
    return new Prefix(this.bindings.set(name, {prefix: this, bound}));
  }

  /**
   * Finds a bound in the prefix. Returns nothing if the bound could not
   * be found.
   */
  find(name: string): PrefixBound | undefined {
    return this.bindings.get(name);
  }

  /**
   * Finds a bound in the prefix. Panics if the bound could not be found.
   */
  findOrPanic(name: string): PrefixBound {
    const prefixBound = this.bindings.get(name);
    if (prefixBound === undefined) {
      throw new Error(`Unbound type variable "${name}".`);
    }
    return prefixBound;
  }

  /**
   * Converts a prefix to a string for debugging purposes. The format of the
   * string is chosen based on the [MLF paper][1] and so is quite academic.
   *
   * Brite programmers will not be familiar with this syntax. It is for
   * debugging purposes only.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
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

export type PrefixBound = {
  readonly prefix: Prefix;
  readonly bound: Bound;
};
