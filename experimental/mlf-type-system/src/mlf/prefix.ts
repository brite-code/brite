import {Bound, PolymorphicType} from './type';

type PrefixBound = {
  level: number;
  order: number;
  bound: Bound;
};

export class Prefix {
  private counter = 0;
  private readonly bindings: Map<string, PrefixBound>;
  private readonly levels: Array<Set<string>> = [];

  constructor(bindings?: ReadonlyArray<[string, Bound]>) {
    this.bindings = new Map(
      (bindings || []).map(
        ([binding, bound]): [string, PrefixBound] => [
          binding,
          {level: 0, order: 0, bound},
        ]
      )
    );
  }

  /**
   * Gets the bound for this type identifier. If it exists.
   */
  get(identifier: string): Bound | undefined {
    const bound = this.bindings.get(identifier);
    return bound !== undefined ? bound.bound : undefined;
  }

  /**
   * Creates a unique type identifier in this prefix and sets the bound to
   * that identifier. Returns the identifier so that it may be retrieved from
   * the prefix later.
   */
  add(bound: Bound): string {
    // Find an identifier in our bindings map that has not already been taken.
    let counter = this.counter;
    let identifier = typeVariableName(counter);
    while (this.bindings.has(identifier)) {
      counter = counter + 1;
      identifier = typeVariableName(counter);
    }
    // Add the fresh identifier with its bound to our prefix and return.
    this.bindings.set(identifier, {
      level: this.levels.length,
      order: counter,
      bound,
    });
    // If we are tracking newly created type variables then add this one.
    if (this.levels.length > 0) {
      this.levels[this.levels.length - 1].add(identifier);
    }
    return identifier;
  }

  /**
   * Tracks all the new bindings created in the execution of `f()`. At the end
   * of execution we remove all the new bindings from our prefix and move them
   * to a new bindings map.
   *
   * The bindings are in reverse order for easy conversion to a quantified type.
   */
  quantify<T>(
    f: () => T
  ): {
    readonly result: T;
    readonly bindings: ReadonlyArray<{
      readonly binding: string;
      readonly bound: Bound;
    }>;
  } {
    const oldCounter = this.counter;
    const level = new Set<string>();
    this.levels.push(level);
    const result = f();
    this.levels.pop();
    // Collect all the bindings created at this level into a map. Make sure they
    // are in their proper order.
    const bindings = Array.from(level)
      .map(binding => {
        const bound = this.bindings.get(binding)!; // tslint:disable-line no-non-null-assertion
        this.bindings.delete(binding);
        return {binding, bound};
      })
      .sort((a, b) => a.bound.order - b.bound.order)
      .map(({binding, bound: {bound}}) => ({binding, bound}))
      .reverse();
    // Restore the old counter variable after we delete all the type variables
    // created by `f()` since there will be no collisions with those type
    // variables now. This way we can reuse type variable names.
    this.counter = oldCounter;
    return {result, bindings};
  }

  /**
   * Updates an existing binding. Throws an error if the binding does not exist
   * in the prefix. If the type has bindings at a higher levels then the binding
   * we are updating then those bounds will be moved to a lower level.
   */
  update(identifier: string, bound: Bound) {
    const currentBound = this.bindings.get(identifier);
    if (currentBound === undefined) {
      throw new Error('Can only update an existing binding.');
    }
    // If the bound we are updating is at a lower level than our current level
    // then we need to move up bindings referenced by our new bound which are
    // not accessible at our level.
    if (currentBound.level < this.levels.length) {
      this.levelUp(
        currentBound.level,
        currentBound.order - 1,
        new Set(),
        bound.type
      );
    }
    // Actually update the bound.
    currentBound.bound = bound;
  }

  /**
   * Moves type variables bound in our prefix at a higher level then the one
   * specified to that level and to the specified order.
   *
   * In other words when we unify `f` to `a → b` in
   * `∀(f = a → b).∀(g ≥ ∀a.∀b.a → b)` we need to “level up” `a` and `b` so that
   * the type variables are accessible in `f`. So we convert that previous
   * prefix to `∀a.∀b.∀(f = a → b).∀(g ≥ a → b)`.
   *
   * Oh, and regarding the name of this function, pun totally intended.
   */
  private levelUp(
    level: number,
    order: number,
    scope: Set<string>,
    type: PolymorphicType
  ) {
    // NOTE: This is kind of inefficient since we may end up recursively
    // checking deep types multiple times. We may want some cache to improve
    // performance of this step. Measure first before assuming this is slowing
    // us down, though!

    switch (type.kind) {
      case 'Variable': {
        // If this variable is defined in scope then we don’t need to level up.
        if (scope.has(type.identifier)) break;
        // Get the bound for this type. If no bound exists in our prefix then
        // ignore this type variable.
        const bound = this.bindings.get(type.identifier);
        if (bound === undefined) break;
        // If the bound’s level is larger than our own we need to level up
        // the bound!
        if (level < bound.level) {
          // TODO: What if by moving the type variable’s level we are overriding
          // some other type variable???

          // Remove this bound from its current level and add it to its
          // new level.
          this.levels[bound.level - 1].delete(type.identifier);
          if (level > 0) this.levels[level - 1].add(type.identifier);
          // Set the new level and order on the bound.
          bound.level = level;
          bound.order = order;
        }
        break;
      }
      case 'Quantified': {
        this.levelUp(level, order, scope, type.bound.type);
        const hasBinding = scope.has(type.binding);
        if (!hasBinding) scope.add(type.binding);
        this.levelUp(level, order, scope, type.body);
        if (!hasBinding) scope.delete(type.binding);
        break;
      }
      case 'Constant':
        break;
      case 'Function':
        this.levelUp(level, order, scope, type.parameter);
        this.levelUp(level, order, scope, type.body);
        break;
      case 'Bottom':
        break;
      default:
        const never: never = type;
        throw never;
    }
  }
}

/**
 * Get a type variable name for the current counter value.
 */
export function typeVariableName(counter: number): string {
  if (counter >= letters.length) {
    return `t${counter - letters.length + 1}`;
  } else {
    return letters[counter];
  }
}

/**
 * Tries to decode a type variable created by `typeVariableName()` back into
 * a number.
 */
export function typeVariableCounter(name: string): number | undefined {
  let count = lettersToIndex.get(name);
  if (count !== undefined) return count;
  if (!/^t[1-9][0-9]*$/.test(name)) return undefined;
  count = parseInt(name.slice(1), 10);
  if (isNaN(count) || count < 1) return undefined;
  return count + letters.length - 1;
}

// All the ASCII lowercase letters. We use these for creating type
// variable names.
const letters: ReadonlyArray<string> = 'abcdefghijklmnopqrstuvwxyz'.split('');

// A map of ASCII lowercase letters to its integer index in the letters array.
const lettersToIndex = new Map<string, number>(
  letters.map((letter, i): [string, number] => [letter, i])
);
