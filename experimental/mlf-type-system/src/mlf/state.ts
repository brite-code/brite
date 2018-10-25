import {Bound, Monotype, Polytype, Type} from './type';

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
    {level: number; bound: Bound}
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
    return {level: entry.level, bound: entry.bound};
  }

  /**
   * Updates a type variable in our state. The type variable must have come
   * from `State.newType()` or `State.newTypeWithBound()` as to assure no
   * scoping shenanigans are necessary since all type names are guaranteed to be
   * unique in the program.
   *
   * Also, if a free type variable in the provided bound has a higher level then
   * the type variable we are updating then we “level up” the type variable in
   * the bound so that it won’t be deallocated before the type variable we are
   * updating.
   */
  updateType(name: string, bound: Bound) {
    const entry = this.typeVariables.get(name);
    if (entry === undefined) {
      throw new Error(`Type variable not found: "${name}"`);
    }
    // Update the entry’s bound to the new bound.
    entry.bound = bound;
    // Level up all the free type variables in our bound’s type.
    if (bound.type !== undefined) this.levelUp(entry.level, bound.type);
  }

  /**
   * Moves the free variables of the provided type at a level higher then the
   * provided level to the provided level. Also updates the bounds of these free
   * variables recursively.
   *
   * This operation saves type variables that are needed at the provided level
   * from deallocation when `State.decrementLevel()` is called.
   *
   * And yes, pun intended.
   */
  private levelUp(newLevel: number, type: Polytype) {
    // For all the free variables in our type...
    for (const name of Type.getFreeVariables(type)) {
      const entry = this.typeVariables.get(name);
      if (entry === undefined) continue;
      // If the entry’s level is larger than the new level then that means this
      // type variable will be deallocated before we need it at `newLevel`. So
      // we need to update its level.
      if (entry.level > newLevel) {
        // Update the level.
        const oldLevel = entry.level;
        entry.level = newLevel;
        // Move the type variable from its old level to its new level so it
        // won’t be deallocated with the old level.
        if (oldLevel > 0) this.levels[oldLevel - 1].delete(name);
        if (newLevel > 0) this.levels[newLevel - 1].add(name);
        // Recursively update the free type variables in our bound since they
        // too might be deallocated too soon if we don’t.
        if (entry.bound.type !== undefined) {
          this.levelUp(newLevel, entry.bound.type);
        }
      }
    }
  }
}
