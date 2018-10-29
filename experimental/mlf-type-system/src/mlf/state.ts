import {Bound, Monotype, Polytype, Type} from './type';

/**
 * State required to type-check a Brite application. Holds things like allocated
 * type variables.
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
    if (this.levels.length > 0) this.levels[this.levels.length - 1].add(name);
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
   * IMPORTANT: We really shouldn’t be calling this function outside of the
   * `unify()` function. We need to maintain an instance relation between the
   * old bound and the new bound (`old ⊑ new`) whenever this function is called.
   * We take care to maintain that relation in `unify()`. We don’t trust other
   * callers to take the same care.
   *
   * Updates a type variable in our state. The type variable must have come
   * from `State.newType()` or `State.newTypeWithBound()` as to assure no
   * scoping shenanigans are necessary since all type names are guaranteed to be
   * unique in the program.
   *
   * Returns true if the type was successfully updated. Returns false if
   * performing this update would result in an infinite type. We add
   * `withOccursCheck` to the name so that the programmer is reminded to handle
   * failed occurrence checks.
   *
   * Also, if a free type variable in the provided bound has a higher level then
   * the type variable we are updating then we “level up” the type variable in
   * the bound so that it won’t be deallocated before the type variable we are
   * updating.
   *
   * NOTE: We don’t enforce this, but according to the [MLF thesis][1] the
   * current value of the bound we are updating (`old`) and the new value of the
   * bound we are updating to (`new`) should be in an instance relation
   * (in notation: `old ⊑ new`). See the documentation comment above `unify()` for
   * more information.
   *
   * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
   */
  updateTypeWithOccursCheck(name: string, bound: Bound): boolean {
    const entry = this.typeVariables.get(name);
    if (entry === undefined) {
      throw new Error(`Type variable not found: "${name}"`);
    }
    // Checks to see if the type variable we are updating exists in the type we
    // are updating to. If so then we have an infinite type. Fail early!
    if (bound.type !== undefined && this.occurs(name, bound.type)) {
      return false;
    }
    // Update the entry’s bound to the new bound.
    entry.bound = bound;
    // Level up all the free type variables in our bound’s type.
    this.levelUp(entry.level, bound.type);
    return true;
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
        this.levelUp(newLevel, entry.bound.type);
      }
    }
  }

  /**
   * Recursively tests to see if the provided type or any of the type’s bounds
   * in state contain the provided name. If so then it is not safe to update the
   * provided name with the provided type.
   */
  private occurs(testName: string, type: Polytype): boolean {
    // For all free type variables...
    for (const name of Type.getFreeVariables(type)) {
      // If this name is the one we are testing for then the test name does
      // occur in our type! Uh oh...
      if (name === testName) return true;
      // Get the bound for this type variable.
      const entry = this.typeVariables.get(name);
      if (entry === undefined) continue;
      // Recursively check to see if our name occurs in the type bound.
      if (this.occurs(testName, entry.bound.type)) return true;
    }
    return false;
  }

  /**
   * Prints the state  type variable prefix to a display string using the
   * standard syntax for types in academic literature. Particularly the [MLF][1]
   * paper we implement.
   *
   * Brite programmers will not be familiar with this syntax. It is for
   * debugging purposes only.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
  toDisplayString() {
    if (this.typeVariables.size === 0) {
      return '(∅)';
    }
    const bounds: Array<string> = [];
    for (const [name, {bound}] of this.typeVariables) {
      if (bound.kind === 'flexible' && bound.type === undefined) {
        bounds.push(name);
      } else {
        const kind = bound.kind === 'flexible' ? '≥' : '=';
        const type =
          bound.type !== undefined ? Type.toDisplayString(bound.type) : '⊥';
        bounds.push(`${name} ${kind} ${type}`);
      }
    }
    return `(${bounds.join(', ')})`;
  }

  /**
   * Are there no type variables in our unification state?
   */
  isEmpty(): boolean {
    return this.typeVariables.size === 0;
  }
}
