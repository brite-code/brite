import {Bound, TypeIdentifier} from './type';

export class Prefix {
  private counter = 0;
  private readonly bindings = new Map<string, Bound>();
  private readonly newBindings: Array<Array<TypeIdentifier>> = [];

  /**
   * Creates a unique type identifier in this prefix and sets the bound to
   * that identifier. Returns the identifier so that it may be retrieved from
   * the prefix later.
   */
  add(bound: Bound): TypeIdentifier {
    // Find an identifier in our bindings map that has not already been taken.
    let counter = this.counter + 1;
    while (this.bindings.has(`t${counter}`)) {
      counter = counter + 1;
    }
    // Add the fresh identifier with its bound to our prefix and return.
    const identifier = TypeIdentifier.create(`t${counter}`);
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
  get(identifier: TypeIdentifier): Bound | undefined {
    return this.bindings.get(identifier);
  }

  /**
   * Updates an existing binding. Throws an error if the binding does not exist
   * in the prefix.
   */
  update(identifier: TypeIdentifier, bound: Bound) {
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
    readonly bindings: ReadonlyMap<TypeIdentifier, Bound>;
  } {
    const oldCounter = this.counter;
    const newBindings: Array<TypeIdentifier> = [];
    this.newBindings.push(newBindings);
    const result = f();
    this.newBindings.pop();
    const bindings = new Map(
      newBindings.map(
        (binding): [TypeIdentifier, Bound] => {
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
