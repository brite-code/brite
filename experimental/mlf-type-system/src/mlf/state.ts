import {Bound, Monotype, Type} from './type';

/**
 * State required to type-check a Brite application. Holds things like all
 * allocated type variables.
 */
export class State {
  // The id for the type variable in our program.
  private nextType = 0;

  // All the allocated type variables in our program.
  private readonly typeVariables = new Map<
    string,
    {readonly level: number; readonly bound: Bound}
  >();

  // A stack of all our levels and the types allocated in each level.
  private readonly levels: Array<Set<string>> = [];

  /**
   * Gets the current level for our program. Levels are used to determine which
   * type variables need to be generalized.
   *
   * Think of it a bit like garbage collection. We allocate a type variable at a
   * given level. Then when we exit that level all type variables need to be
   * “collected” or in our case “quantified”.
   */
  getLevel(): number {
    return this.levels.length;
  }

  /**
   * Increments the level.
   */
  incrementLevel() {
    this.levels.push(new Set());
  }

  /**
   * Decrements the level. Removes all variables allocated in this level from
   * our state. Make sure you don’t need any of the variables before they
   * are removed!
   */
  decrementLevel() {
    const variables = this.levels.pop();
    if (variables !== undefined) {
      for (const variable of variables) {
        this.typeVariables.delete(variable);
      }
    }
  }

  /**
   * Allocates a fresh type variable in our state with no bound. When
   * `State.decrementLevel()` is called this type will be deallocated.
   *
   * The created type is guaranteed to have a unique name in our entire program
   * since the generated name starts with `$` which is not allowed in
   * user identifiers.
   */
  newType(): Monotype {
    return this.newTypeWithBound(Type.unbounded);
  }

  /**
   * Allocates a type variable in our state with the provided bound. When
   * `State.decrementLevel()` is called this type will be deallocated.
   *
   * The created type is guaranteed to have a unique name in our entire program
   * since the generated name starts with `$` which is not allowed in
   * user identifiers.
   */
  newTypeWithBound(bound: Bound): Monotype {
    const name = `$${this.nextType++}`;
    this.typeVariables.set(name, {level: this.levels.length, bound});
    if (this.levels.length > 1) this.levels[this.levels.length - 1].add(name);
    return Type.variable(name);
  }

  /**
   * Looks up a type variable in our state. The type variable must have come
   * from `State.newType()` or `State.newTypeWithBound()` as to assure no
   * scoping shenanigans are necessary since all type names are guaranteed to be
   * unique in the program.
   */
  lookupType(name: string): {readonly level: number; readonly bound: Bound} {
    const entry = this.typeVariables.get(name);
    if (entry === undefined) {
      throw new Error(`Type variable not found: "${name}"`);
    }
    return entry;
  }

  /**
   * Updates a type variable in our state. The type variable must have come
   * from `State.newType()` or `State.newTypeWithBound()` as to assure no
   * scoping shenanigans are necessary since all type names are guaranteed to be
   * unique in the program.
   *
   * TODO: Deallocation saving implementation and documentation.
   */
  updateType(name: string, bound: Bound) {
    const {level: oldLevel} = this.lookupType(name);
    this.typeVariables.set(name, {level: oldLevel, bound});
  }
}
