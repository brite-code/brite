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
  static empty = new Prefix(0, 0, Immutable.Map());

  // The number of bindings in the prefix.
  private readonly level: number;

  // The level at which bindings may be accessed.
  private readonly accessLevel: number;

  // Every binding in our prefix includes its level. This way when we fetch a
  // bound from the prefix we can return a prefix which can only access bindings
  // above the level of the binding we just found.
  private readonly bindings: Immutable.Map<string, Immutable.List<PrefixBound>>;

  private constructor(
    level: number,
    accessLevel: number,
    bindings: Immutable.Map<string, Immutable.List<PrefixBound>>
  ) {
    this.level = level;
    this.accessLevel = accessLevel;
    this.bindings = bindings;
  }

  /**
   * Adds a bound to the end of the prefix. Moves the access level in the
   * returned prefix to the end.
   */
  add(name: string, bound: Bound): Prefix {
    return new Prefix(
      this.level + 1,
      this.level,
      this.bindings.update(name, list =>
        (list || Immutable.List()).push({level: this.level, bound})
      )
    );
  }

  /**
   * Finds a bound in the prefix. Returns nothing if the bound could not
   * be found.
   */
  find(name: string): {prefix: Prefix; bound: Bound} | undefined {
    const bindings = this.bindings.get(name);
    if (bindings === undefined) return undefined;
    const binding = bindings.findLast(
      binding => binding.level <= this.accessLevel
    );
    if (binding === undefined) return undefined;
    const prefix = new Prefix(this.level, binding.level - 1);
    return {
      bound: binding.bound,
    };
  }

  /**
   * Finds a bound in the prefix. Panics if the bound could not be found.
   */
  findOrPanic(name: string): {prefix: Prefix; bound: Bound} {
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

type PrefixBound = {
  readonly level: number;
  readonly bound: Bound;
};
