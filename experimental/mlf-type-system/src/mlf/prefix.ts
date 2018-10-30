import {BindingMap} from '../utils/bindings';
import {Err, Ok, Result} from '../utils/result';

import {Bound, Polytype, Type} from './type';

export type PrefixError =
  | {
      readonly kind: 'UnboundTypeVariable';
      readonly name: string;
    }
  | {
      readonly kind: 'InfiniteType';
    }
  | {
      readonly kind: 'InvalidTypeUpdate';
      readonly old: Type;
      readonly new: Type;
    };

/**
 * State required to type-check a Brite application. Holds things like allocated
 * type variables.
 */
export class Prefix {
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
   * our prefix. Make sure you don’t need any of the variables before they
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
   * Are there no type variables in our prefix?
   */
  isEmpty(): boolean {
    return this.typeVariables.size === 0;
  }

  /**
   * Allocates a fresh type variable in our prefix with no bound. When
   * `Prefix.decrementLevel()` is called this type will be deallocated.
   *
   * The created type is guaranteed to have a unique name in our entire program
   * since the generated name starts with `$` which is not allowed in
   * user identifiers.
   */
  fresh(): string {
    return this.freshWithBound(Type.unbounded);
  }

  /**
   * Allocates a type variable in our prefix with the provided bound. When
   * `Prefix.decrementLevel()` is called this type will be deallocated.
   *
   * The created type is guaranteed to have a unique name in our entire program
   * since the generated name starts with `$` which is not allowed in
   * user identifiers.
   */
  freshWithBound(bound: Bound): string {
    const name = `$${this.nextType++}`;
    this.addToLevel(name, bound);
    return name;
  }

  /**
   * Adds a named type variable to the prefix. If a type variable with this name
   * already exists in the prefix then a new name is generated. The name used to
   * allocate the type variable is returned. When `Prefix.decrementLevel()` is
   * called this type variable will be deallocated.
   */
  add(name: string, bound: Bound): string {
    if (this.typeVariables.has(name)) {
      return this.freshWithBound(bound);
    } else {
      this.addToLevel(name, bound);
      return name;
    }
  }

  /**
   * Internal function for adding a type variable to our prefix.
   */
  private addToLevel(name: string, bound: Bound): void {
    this.typeVariables.set(name, {level: this.levels.length, bound});
    if (this.levels.length > 0) this.levels[this.levels.length - 1].add(name);
  }

  /**
   * Looks up a type variable in our prefix. The type variable must have come
   * from `Prefix.fresh()` or `Prefix.freshWithBound()` as to assure no
   * scoping shenanigans are necessary since all type names are guaranteed to be
   * unique in the program.
   */
  lookup(
    name: string
  ): Result<{readonly level: number; readonly bound: Bound}, PrefixError> {
    const entry = this.typeVariables.get(name);
    if (entry === undefined) {
      return Err<PrefixError>({kind: 'UnboundTypeVariable', name});
    } else {
      return Ok({level: entry.level, bound: entry.bound});
    }
  }

  /**
   * Updates a type variable in our prefix. Assumes that the old type and the
   * new provided type are in the instance relation (`(Q) old ⊑ new`). Please
   * don’t call this function outside of the unification algorithm. Since the
   * unification algorithm takes care to maintain the relation `old ⊑ new`.
   *
   * Possibly returns an error if the occurs check or abstraction check fails.
   *
   * Also, if a free type variable in the provided bound has a higher level then
   * the type variable we are updating then we “level up” the type variable in
   * the bound so that it won’t be deallocated before the type variable we are
   * updating.
   */
  update(name: string, bound: Bound): PrefixError | undefined {
    const entry = this.typeVariables.get(name);
    if (entry === undefined) {
      throw new Error(`Type variable not found: "${name}"`);
    }
    // Checks to see if the type variable we are updating exists in the type we
    // are updating to. If so then we have an infinite type. Fail early!
    if (this.occursCheck(name, bound.type)) return {kind: 'InfiniteType'};
    // TODO: Documentation comment
    if (
      entry.bound.kind === 'rigid' &&
      !this.abstractionCheck(entry.bound.type, bound.type)
    ) {
      return {
        kind: 'InvalidTypeUpdate',
        old: entry.bound.type,
        new: bound.type,
      };
    }
    // Update the entry’s bound to the new bound.
    entry.bound = bound;
    // Level up all the free type variables in our bound’s type.
    this.levelUp(entry.level, bound.type);
    return undefined;
  }

  /**
   * Moves the free variables of the provided type at a level higher then the
   * provided level to the provided level. Also updates the bounds of these free
   * variables recursively.
   *
   * This operation saves type variables that are needed at the provided level
   * from deallocation when `Prefix.decrementLevel()` is called.
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
   * in prefix contain the provided name. If so then it is not safe to update
   * the provided name with the provided type.
   */
  private occursCheck(testName: string, type: Polytype): boolean {
    // For all free type variables...
    for (const name of Type.getFreeVariables(type)) {
      // If this name is the one we are testing for then the test name does
      // occur in our type! Uh oh...
      if (name === testName) return true;
      // Get the bound for this type variable.
      const entry = this.typeVariables.get(name);
      if (entry === undefined) continue;
      // Recursively check to see if our name occurs in the type bound.
      if (this.occursCheck(testName, entry.bound.type)) return true;
    }
    return false;
  }

  private abstractionCheck(oldType: Polytype, newType: Polytype): boolean {
    const prefix = this;
    const scope1 = new BindingMap<string, Polytype>();
    const scope2 = new BindingMap<string, Polytype>();

    if (!projectionEquals(oldType, newType)) return false;
    if (Type.isMonotype(oldType)) return true;

    return true;
    // if (newType.description.kind === 'Variable') {
    // } else {
    //   return true;
    // }

    /**
     * Counts the number of bottom types which appear in a flexible context.
     *
     * For illustration consider the following type. For readability we write
     * the type on multiple lines:
     *
     * ```
     * ∀(a ≥ ⊥).
     * ∀(b ≥ ∀(b1 ≥ ⊥, b2 = ⊥).b1 → b2).
     * ∀(c = ∀(c1 ≥ ⊥, c2 = ⊥).c1 → c2).
     * a → b → c
     * ```
     *
     * In this type we have five bottom types (⊥). Each bottom type has a “path”
     * corresponding to:
     *
     * 1. `a`: ≥
     * 2. `b1`: ≥≥
     * 3. `b2`: ≥=
     * 4. `c1`: =≥
     * 5. `c2`: ==
     *
     * We consider a bottom type to appear in a flexible context if the path
     * starts with one or more flexible bounds (`≥`). So `a`, `b1`, and `b2`
     * appear in a flexible context since they begin with one or more
     * flexible bounds.
     *
     * For more information about the theory of “flexible paths” see
     * [MLF thesis][1] section 2.7.2 entitled “Weights”. For the theory behind
     * paths. See definition 2.7.3 and its accompanying definition of w(t) for
     * an application of weights. See figure 4.1 and lemma 2.7.8 for how the
     * theory of weights applies to our specific abstraction check algorithm.
     *
     * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
     */
    function flexiblePaths(type: Polytype): number {
      let paths = 0;

      while (type.description.kind === 'Quantify') {
        if (type.description.bound.kind === 'flexible') {
          paths += allPaths(type.description.bound.type);
        }
        type = type.description.body;
      }

      return paths;

      function allPaths(type: Polytype): number {
        let paths = 0;

        while (type.description.kind === 'Quantify') {
          paths += allPaths(type.description.bound.type);
          type = type.description.body;
        }

        if (type.description.kind === 'Bottom') {
          paths += 1;
        }

        return paths;
      }
    }

    /**
     * Determine if the projection of two types under our prefix are equal. The
     * projection is a simple inlining of all bounds no matter their kind.
     */
    function projectionEquals(t1: Polytype, t2: Polytype): boolean {
      // Substitute the type for `t1` if it is in scope.
      if (t1.description.kind === 'Variable') {
        const type = scope1.get(t1.description.name);
        if (type !== undefined) return projectionEquals(type, t2);
      }

      // Substitute the type for `t2` if it is in scope.
      if (t2.description.kind === 'Variable') {
        const type = scope2.get(t2.description.name);
        if (type !== undefined) return projectionEquals(t1, type);
      }

      // If both `t1` and `t2` are variables and they share the same name then
      // we know their bounds to be equal.
      if (
        t1.description.kind === 'Variable' &&
        t2.description.kind === 'Variable' &&
        t1.description.name === t2.description.name
      ) {
        return true;
      }

      // Substitute the type for `t1` from our prefix. Return false if the type
      // is not found in our prefix.
      if (t1.description.kind === 'Variable') {
        const entry = prefix.typeVariables.get(t1.description.name);
        if (entry === undefined) return false;
        return projectionEquals(entry.bound.type, t2);
      }

      // Substitute the type for `t2` from our prefix. Return false if the type
      // is not found in our prefix.
      if (t2.description.kind === 'Variable') {
        const entry = prefix.typeVariables.get(t2.description.name);
        if (entry === undefined) return false;
        return projectionEquals(t1, entry.bound.type);
      }

      // Unwrap `t1` quantifications into scope.
      if (t1.description.kind === 'Quantify') {
        let pops = 0;
        while (t1.description.kind === 'Quantify') {
          pops++;
          scope1.push(t1.description.name, t1.description.bound.type);
          t1 = t1.description.body;
        }
        const equals = projectionEquals(t1, t2);
        for (let i = 0; i < pops; i++) scope1.pop();
        return equals;
      }

      // Unwrap `t2` quantifications into scope.
      if (t2.description.kind === 'Quantify') {
        let pops = 0;
        while (t2.description.kind === 'Quantify') {
          pops++;
          scope2.push(t2.description.name, t2.description.bound.type);
          t2 = t2.description.body;
        }
        const equals = projectionEquals(t1, t2);
        for (let i = 0; i < pops; i++) scope2.pop();
        return equals;
      }

      // Check equality of scalar types.
      if (t1.description.kind === 'Bottom') return t2.description.kind === 'Bottom'; // prettier-ignore
      if (t2.description.kind === 'Bottom') return false;
      if (t1.description.kind === 'Boolean') return t2.description.kind === 'Boolean'; // prettier-ignore
      if (t2.description.kind === 'Boolean') return false;
      if (t1.description.kind === 'Number') return t2.description.kind === 'Number'; // prettier-ignore
      if (t2.description.kind === 'Number') return false;
      if (t1.description.kind === 'String') return t2.description.kind === 'String'; // prettier-ignore
      if (t2.description.kind === 'String') return false;

      // Check equality of composite types.
      if (t1.description.kind === 'Function') {
        if (t2.description.kind === 'Function') {
          return (
            projectionEquals(t1.description.parameter, t2.description.parameter) && // prettier-ignore
            projectionEquals(t1.description.body, t2.description.body)
          );
        } else {
          return false;
        }
      } else if (t2.description.kind === 'Function') {
        return false;
      }

      const never1: never = t1.description;
      const never2: never = t2.description;
      return never1 && never2;
    }
  }

  /**
   * Prints the prefix to a display string using the standard syntax for types
   * in academic literature. Particularly the [MLF][1] paper we implement.
   *
   * Brite programmers will not be familiar with this syntax. It is for
   * debugging purposes only.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
  toDisplayString() {
    if (this.typeVariables.size === 0) return '(∅)';
    // All the type variables in our prefix in dependency order.
    const seen = new Set<string>();
    // Visits a type variable with the provided name. Adds the dependencies of
    // the visited type variable before adding the type variable name itself.
    const visit = (name: string) => {
      if (seen.has(name)) return;
      const entry = this.typeVariables.get(name);
      if (entry === undefined) return;
      for (const name of Type.getFreeVariables(entry.bound.type)) visit(name);
      seen.add(name);
    };
    // Visit all the type variables in our prefix. This will populate the `seen`
    // set in dependency order. That is dependencies will appear before their
    // dependents in the set.
    for (const name of this.typeVariables.keys()) visit(name);
    // Iterate through the type variables in dependency order and produce the
    // display string.
    const bounds: Array<string> = [];
    for (const name of seen) {
      const entry = this.typeVariables.get(name);
      if (entry === undefined) continue;
      const bound = entry.bound;
      if (bound.kind === 'flexible' && bound.type === undefined) {
        bounds.push(name);
      } else {
        const kind = bound.kind === 'flexible' ? '≥' : '=';
        const type =
          bound.type !== undefined ? Type.toDisplayString(bound.type) : '⊥';
        bounds.push(`${name} ${kind} ${type}`);
      }
    }
    // Return the prefix in parentheses.
    return `(${bounds.join(', ')})`;
  }
}
