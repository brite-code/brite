import * as Immutable from 'immutable';

import {TypeIdentifier} from './identifier';
import {Bound} from './type';

export class Prefix {
  public static empty = new Prefix(0, Immutable.List([Immutable.Map()]));

  private readonly nextTypeIdentifier: number;

  private readonly bindingStack: Immutable.List<
    Immutable.Map<TypeIdentifier, Bound>
  >;

  private constructor(
    nextTypeIdentifier: number,
    bindingStack: Immutable.List<Immutable.Map<TypeIdentifier, Bound>>
  ) {
    this.nextTypeIdentifier = nextTypeIdentifier;
    this.bindingStack = bindingStack;
  }

  /**
   * Creates a new type variable with the provided bound referenced with the
   * returned identifier.
   */
  add(
    bound: Bound
  ): {
    readonly prefix: Prefix;
    readonly identifier: TypeIdentifier;
  } {
    const identifier = TypeIdentifier.create(this.nextTypeIdentifier);
    const newBindingStack = this.bindingStack.update(0, bindings =>
      bindings.set(identifier, bound)
    );
    return {
      prefix: new Prefix(this.nextTypeIdentifier + 1, newBindingStack),
      identifier,
    };
  }

  /**
   * Finds the bound of the provided type variable in the prefix. Expects that
   * the type variable exists in this prefix. Throws an error if it does not.
   */
  find(identifier: TypeIdentifier): Bound {
    for (const bindings of this.bindingStack) {
      const bound = bindings.get(identifier);
      if (bound !== undefined) return bound;
    }
    throw new Error('Could not find type variable.');
  }

  /**
   * Updates the type variable with this identifier in the prefix with the
   * new bound.
   */
  update(identifier: TypeIdentifier, bound: Bound): Prefix {
    const bindingStack = this.bindingStack.withMutations(bindingStack => {
      let index = 0;
      for (const bindings of this.bindingStack) {
        if (bindings.has(identifier)) {
          bindingStack.set(index, bindings.set(identifier, bound));
          return;
        }
        index++;
      }
      throw new Error('Could not find type variable.');
    });
    return new Prefix(this.nextTypeIdentifier, bindingStack);
  }

  /**
   * Pushes a scope of bindings onto the prefix. This scope may later be popped
   * off. Which enables us to capture fresh bindings in a quantified type.
   */
  pushScope(bindings: Immutable.Map<TypeIdentifier, Bound>): Prefix {
    return new Prefix(
      this.nextTypeIdentifier,
      this.bindingStack.unshift(bindings) // Assumes `unshift()` is O(1)
    );
  }

  /**
   * Pops a scope of bindings we most recently previously pushed off the prefix.
   * If no scope was pushed then we return an empty bindings map.
   */
  popScope(): {
    readonly prefix: Prefix;
    readonly bindings: Immutable.Map<TypeIdentifier, Bound>;
  } {
    const bindings = this.bindingStack.first() || Immutable.Map();
    const prefix = new Prefix(
      this.nextTypeIdentifier,
      this.bindingStack.shift() // Assumes `shift()` is O(1)
    );
    return {bindings, prefix};
  }
}
