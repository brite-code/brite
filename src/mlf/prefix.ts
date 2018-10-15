import {Bound, TypeIdentifier} from './type';

export class Prefix {
  public static create(): Prefix {
    return new Prefix();
  }

  private readonly bindings: Array<Bound> = [];
  private readonly newBindings: Array<Set<TypeIdentifier>> = [];

  private constructor() {}

  /**
   * Creates a new type variable with the provided bound referenced with the
   * returned identifier.
   */
  add(bound: Bound): TypeIdentifier {
    const identifier = TypeIdentifier.create(this.bindings.length);
    this.bindings.push(bound);
    if (this.newBindings.length > 0) {
      const newBindings = this.newBindings[this.newBindings.length - 1];
      newBindings.add(identifier);
    }
    return identifier;
  }

  /**
   * Finds the bound of the provided type variable in the prefix. Expects that
   * the type variable exists in this prefix. Throws an error if it does not.
   */
  find(identifier: TypeIdentifier): Bound {
    const bound = this.bindings[identifier];
    if (bound === undefined) throw new Error('Type variable not found.');
    return bound;
  }

  /**
   * Updates the type variable with this identifier in the prefix with the
   * new bound.
   */
  update(identifier: TypeIdentifier, bound: Bound) {
    if ((identifier as number) >= this.bindings.length) {
      throw new Error('Type variable not found.');
    }
    this.bindings[identifier] = bound;
  }

  /**
   * Starts a capture of all new bindings.
   */
  captureStart() {
    this.newBindings.push(new Set());
  }

  /**
   * Stops a capture of all new bindings. Returns the bindings which
   * were captured.
   */
  captureStop(): Map<TypeIdentifier, Bound> {
    const newBindings = this.newBindings.pop();
    const bindings = new Map();
    if (newBindings !== undefined) {
      for (const identifier of newBindings) {
        bindings.set(identifier, this.bindings[identifier]);
      }
    }
    return bindings;
  }
}
