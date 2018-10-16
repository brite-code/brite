import {Bound} from './type';

export class Prefix {
  private counter = 0;
  private readonly bindings: Map<string, Bound>;
  private readonly newBindings: Array<Array<string>> = [];

  constructor(bindings?: ReadonlyArray<[string, Bound]>) {
    this.bindings = new Map(bindings);
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
    this.bindings.set(identifier, bound);
    // If we are tracking newly created type variables then add this one.
    if (this.newBindings.length > 0) {
      this.newBindings[this.newBindings.length - 1].push(identifier);
    }
    return identifier;
  }

  /**
   * Gets the bound for this type identifier. If it exists.
   */
  get(identifier: string): Bound | undefined {
    return this.bindings.get(identifier);
  }

  /**
   * Updates an existing binding. Throws an error if the binding does not exist
   * in the prefix.
   */
  update(identifier: string, bound: Bound) {
    if (!this.bindings.has(identifier)) {
      throw new Error('Can only update an existing binding.');
    }
    this.bindings.set(identifier, bound);
  }

  /**
   * Tracks all the new bindings created in the execution of `f()`. At the end
   * of execution we remove all the new bindings from our prefix and move them
   * to a new bindings map.
   */
  quantify<T>(
    f: () => T
  ): {
    readonly result: T;
    readonly bindings: ReadonlyMap<string, Bound>;
  } {
    const oldCounter = this.counter;
    const newBindings: Array<string> = [];
    this.newBindings.push(newBindings);
    const result = f();
    this.newBindings.pop();
    const bindings = new Map(
      newBindings.map(
        (binding): [string, Bound] => {
          const bound = this.bindings.get(binding)!; // tslint:disable-line no-non-null-assertion
          this.bindings.delete(binding);
          return [binding, bound];
        }
      )
    );
    // Restore the old counter variable after we delete all the type variables
    // created by `f()` since there will be no collisions with those type
    // variables now. This way we can reuse type variable names.
    this.counter = oldCounter;
    return {result, bindings};
  }
}

/**
 * Get a type variable name for the current counter value.
 */
function typeVariableName(counter: number): string {
  if (counter >= LETTERS.length) {
    return `t${counter}`;
  } else {
    return LETTERS[counter];
  }
}

// All the ASCII lowercase letters. We use these for creating type
// variable names.
const LETTERS: ReadonlyArray<string> = 'abcdefghijklmnopqrstuvwxyz'.split('');
