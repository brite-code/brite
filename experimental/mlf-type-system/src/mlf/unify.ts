import * as Immutable from 'immutable';

import {Ok, Result} from '../utils/result';

import {BindingMap} from './bindings';
import {Diagnostics, Reported} from './diagnostics';
import {Bound, Monotype, Polytype, Type} from './type';

/**
 * Implements the unification algorithm from the [MLF thesis][1].
 *
 * In theory, the paper defines unification as:
 *
 * > Definition 4.1.1 (Unification): A prefix `Q'` unifies `t1` and `t2` under
 * > `Q` if and only if `Q ⊑ Q'` and `(Q') t1 ≡ t2` hold.
 *
 * In our implementation, one can think of “state” as the prefix. So every
 * mutation to type variable state must maintain the invariant `Q ⊑ Q'`. In
 * practice, we mostly only mutate unbounded type variables in state. In our
 * implementation an unbounded `a` type variable represent the theory for the
 * prefix entry `a ≥ ⊥`. According to the instance relation every polymorphic
 * type is an instance of bottom (`⊥`).
 *
 * As an optimization we also mutate type variables in state to an equivalent
 * type when unification proves it is safe.
 *
 * So `Q ⊑ Q'` holds, but what about `(Q') t1 ≡ t2`? This one is interesting
 * since for an improved developer experience we don’t want to stop type
 * checking if any component of `t1` is not equivalent to the same component
 * in `t2`. For example, `unify((a), number → string, boolean → a)`. Here
 * `number ≢ boolean`. The result in the [MLF thesis][1] would be to fail type
 * checking. In our implementation, however, this unification will emit an error
 * when it sees `number ≢ boolean` but will also continue and update `a` to
 * `string` resulting in the prefix `Q'` as `(a = string)`. Here `Q ⊑ Q'` holds,
 * but `(Q') t1 ≡ t2` does not!
 *
 * How do we maintain soundness? Our implementation of unify will return an
 * error. It is then expected that the caller of unify handle that error! So if
 * the caller expects `(Q') t1 ≡ t2` to hold and it does not then the caller
 * should insert a runtime error in the program where the assertion failed.
 *
 * Consider the program:
 *
 * ```
 * let x = 42 in
 * let y = (x: string) in
 * ...
 * ```
 *
 * Here `number ≢ string`, so we convert this code to:
 *
 * ```
 * let x = 42 in
 * let y = error "number ≢ string" in
 * ...
 * ```
 *
 * At runtime this will halt execution of our program. It is yet to be shown in
 * theory that this strategy is type sound.
 *
 * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
 */
export function unify<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: UnifyState,
  actual: Monotype,
  expected: Monotype
): Reported<UnifyError<Diagnostic>> | undefined {
  return unifyMonotype(diagnostics, state, actual, expected);
}

export type UnifyError<T> =
  | T
  | {
      readonly kind: 'IncompatibleTypes';
      readonly actual: Monotype;
      readonly expected: Monotype;
    }
  | {
      readonly kind: 'InfiniteType';
    };

/**
 * Unifies two monotypes.
 */
function unifyMonotype<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: UnifyState,
  actual: Monotype,
  expected: Monotype
): Reported<UnifyError<Diagnostic>> | undefined {
  // If the actual and expected type variable names match then we know the two
  // types are identical and we don’t need to recursively unify. This only
  // works because we rename variables during instantiation so names
  // don’t conflict!
  if (
    actual.description.kind === 'Variable' &&
    expected.description.kind === 'Variable' &&
    actual.description.name === expected.description.name
  ) {
    return undefined;
  }

  // If the bound of actual is a monomorphic then let us recursively call
  // unify with that type.
  let actualBound: Bound | undefined;
  if (actual.description.kind === 'Variable') {
    actualBound = state.lookupType(actual.description.name).bound;
    if (actualBound.type.description.kind === 'Variable') {
      return unify(diagnostics, state, actualBound.type as Monotype, expected);
    }
  }

  // If the bound of expected is a monomorphic then let us recursively call
  // unify with that type.
  let expectedBound: Bound | undefined;
  if (expected.description.kind === 'Variable') {
    expectedBound = state.lookupType(expected.description.name).bound;
    if (expectedBound.type.description.kind === 'Variable') {
      return unify(diagnostics, state, actual, expectedBound.type as Monotype);
    }
  }

  if (actualBound !== undefined && expectedBound !== undefined) {
  }

  // if (
  //   actual.description.kind === 'Variable' &&
  //   expected.description.kind === 'Variable'
  // ) {
  //   return unifyVariable(
  //     diagnostics,
  //     state,
  //     actual.description.name,
  //     expected.description.name
  //   );
  // } else if (actual.description.kind === 'Variable') {
  //   const variable = actual.description.name;
  //   return unifyVariableWithType(diagnostics, state, expected, variable, true);
  // } else if (expected.description.kind === 'Variable') {
  //   const variable = expected.description.name;
  //   return unifyVariableWithType(diagnostics, state, actual, variable, false);
  // }

  // Matching constants unify.
  if (
    (actual.description.kind === 'Boolean' &&
      expected.description.kind === 'Boolean') ||
    (actual.description.kind === 'Number' &&
      expected.description.kind === 'Number') ||
    (actual.description.kind === 'String' &&
      expected.description.kind === 'String')
  ) {
    return undefined;
  }

  // Functions unify their parameter and body types.
  if (
    actual.description.kind === 'Function' &&
    expected.description.kind === 'Function'
  ) {
    // NOTE: We switch `expected` and `actual` when we unify the function
    // parameter since the function parameter is contravariant.
    const diagnostic1 = unifyMonotype(
      diagnostics,
      state,
      expected.description.parameter,
      actual.description.parameter
    );
    const diagnostic2 = unifyMonotype(
      diagnostics,
      state,
      actual.description.body,
      expected.description.body
    );
    return diagnostic1 || diagnostic2;
  }

  // If two types are not compatible then report a diagnostic and return.
  return diagnostics.report({
    kind: 'IncompatibleTypes',
    actual,
    expected,
  });
}

/**
 * Unifies two polytypes. Returns a type which both should be made equivalent to
 * if not already.
 */
function unifyPolytype<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: UnifyState,
  actual: Polytype,
  expected: Polytype
): Result<Polytype, Reported<UnifyError<Diagnostic>>> {
  // If either is bottom then return the other.
  if (actual.description.kind === 'Bottom') return Ok(expected);
  if (expected.description.kind === 'Bottom') return Ok(actual);

  // Increment the level. We are about to create some type variables which will
  // need to be generalized.
  state.incrementLevel();

  // Decrement the level. We are done creating type variables.
  state.decrementLevel();
}

/**
 * Merges the prefix of a polytype into state. Returns the unwrapped monotype or
 * bottom if the provided polytype was the bottom type.
 */
function mergePrefix(state: UnifyState, type: Polytype): Monotype | undefined {
  while (type.description.kind === 'Quantify') {
    type = type.description.body;
  }

  if (type.description.kind === 'Bottom') return undefined;
  return type as Monotype;
}

/**
 * Unifies a type variable with a monomorphic type.
 */
function unifyVariableWithType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: UnifyState,
  type: Monotype,
  variable: string,
  isVariableActual: boolean
): Reported<UnifyError<Diagnostic>> | undefined {
  // Lookup the bound for our variable.
  const {bound} = state.lookupType(variable);

  // If our type variable is the bottom type then update our variable to the
  // type we are unifying to. Remember to take our quantifications out of
  // scope as well!
  if (bound.type.description.kind === 'Bottom') {
    // Maintains `Q ⊑ Q'` because every type is an instance of bottom.
    return updateVariable(diagnostics, state, variable, type);
  }

  // If our type variable has a monomorphic bound then we unify the variable
  // bound and the type without updating our variable.
  if (Type.isMonotype(bound.type)) {
    return unifyMonotype(
      diagnostics,
      state,
      isVariableActual ? bound.type : type,
      isVariableActual ? type : bound.type
    );
  } else {
    // Instantiate the variable type.
    const variableType = instantiate(state, bound.type);

    // Unify the variable’s monomorphic bound with our type.
    const error = unifyMonotype(
      diagnostics,
      state,
      isVariableActual ? variableType : type,
      isVariableActual ? type : variableType
    );

    // If there was no error then update our variable to the monomorphic type
    // we unified with since our variable and the type are equivalent.
    if (error === undefined) {
      // Maintains `Q ⊑ Q'` because if unify did not error then the two types
      // are equivalent (`≡`) and equivalence is part of the instance relation.
      return updateVariable(diagnostics, state, variable, type);
    } else {
      return error;
    }
  }
}

/**
 * Unifies two type variables.
 */
function unifyVariable<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: UnifyState,
  actualVariable: string,
  expectedVariable: string
): Reported<UnifyError<Diagnostic>> | undefined {
  // If the actual and expected type variable names match then we know the two
  // types are identical and we don’t need to recursively unify. This only
  // works because we rename variables in actual and expected instantiation so
  // they don’t conflict!
  if (actualVariable === expectedVariable) return undefined;

  // Lookup the bounds for our types.
  const {bound: actualBound} = state.lookupType(actualVariable);
  const {bound: expectedBound} = state.lookupType(expectedVariable);

  // If the actual bound is a monotype then let’s try unification again.
  if (Type.isMonotype(actualBound.type)) {
    const expectedType = Type.variable(expectedVariable);
    return unifyMonotype(diagnostics, state, actualBound.type, expectedType);
  }

  // If the expected bound is a monotype then let’s try unification again.
  if (Type.isMonotype(expectedBound.type)) {
    const actualType = Type.variable(actualVariable);
    return unifyMonotype(diagnostics, state, actualType, expectedBound.type);
  }

  // If actual is the bottom type then unify to expected.
  if (actualBound.type.description.kind === 'Bottom') {
    const expectedType = Type.variable(expectedVariable);
    // Maintains `Q ⊑ Q'` because every type is an instance of bottom.
    return updateVariable(diagnostics, state, actualVariable, expectedType);
  }

  // If expected is the bottom type then unify to actual.
  if (expectedBound.type.description.kind === 'Bottom') {
    const actualType = Type.variable(actualVariable);
    // Maintains `Q ⊑ Q'` because every type is an instance of bottom.
    return updateVariable(diagnostics, state, expectedVariable, actualType);
  }

  // Instantiate our type bounds.
  const actualType = instantiate(state, actualBound.type);
  const expectedType = instantiate(state, expectedBound.type);

  // Unify the actual and expected types.
  const error = unifyMonotype(diagnostics, state, actualType, expectedType);

  // If unification was successful then unify both actual and expected to the
  // actual type. Since unification shows that the actual and expected types
  // are equivalent.
  if (error === undefined) {
    const actualType = Type.variable(actualVariable);
    // Maintains `Q ⊑ Q'` because if unify did not error then the two types
    // are equivalent (`≡`) and equivalence is part of the instance relation.
    return updateVariable(diagnostics, state, expectedVariable, actualType);
  } else {
    return error;
  }
}

/**
 * Updates a type in our state while also handling failures in the occurs check.
 *
 * See that we may only update types to a monotype. This is based on the
 * core constraint of type inference in MLF. We do not infer types for function
 * arguments that are used polymorphically.
 *
 * NOTE: We don’t enforce this, but according to the [MLF thesis][1] the
 * current value of the bound we are updating (`old`) and the new value of the
 * bound we are updating to (`new`) should be in an instance relation
 * (in notation: `old ⊑ new`). See the documentation comment above `unify()` for
 * more information.
 *
 * [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
 */
function updateVariable<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: UnifyState,
  name: string,
  type: Monotype
): Reported<UnifyError<Diagnostic>> | undefined {
  const ok = state.updateTypeWithOccursCheck(name, {kind: 'rigid', type});
  if (ok === true) {
    return undefined;
  } else {
    return diagnostics.report({kind: 'InfiniteType'});
  }
}

/**
 * Removes all the quantifications on the provided type and replaces them with
 * fresh type variables in state.
 *
 * This function will never return a quantified polytype.
 */
function instantiate(state: UnifyState, type: Polytype): Polytype {
  if (Type.isMonotype(type)) return type;
  const substitutions = new BindingMap<string, Monotype>();
  return instantiatePolytype(type);

  // Decompose any quantified polytypes and add their bounds to our
  // `substitutions` map. Then recurse into the  resulting monotype to apply
  // the substitutions.
  function instantiatePolytype(type: Polytype): Polytype {
    // If the type is a monotype then we instantiate and return.
    if (Type.isMonotype(type)) {
      return instantiateMonotype(type);
    }

    // Decompose the quantified type by instantiating the bounds and creating
    // fresh types for the instantiated bounds.
    if (type.description.kind === 'Quantify') {
      let pops = 0;
      while (type.description.kind === 'Quantify') {
        const {name, bound} = type.description;
        const newType = state.newTypeWithBound({
          kind: bound.kind,
          type: instantiatePolytype(bound.type),
        });
        substitutions.push(name, newType);
        pops++;
        type = type.description.body;
      }

      // If we have some substitutions then instantiate the monotype before
      // returning it.
      const instantiatedType = instantiateMonotype(type.description.body);

      // Take all our quantified types out of the substitutions map.
      for (let i = 0; i < pops; i++) substitutions.pop();

      return instantiatedType;
    }

    return type;
  }

  // Apply our `substitutions` map to a monotype. We aid our search with
  // `Type.getFreeVariables()` to know whether or not we need to traverse.
  function instantiateMonotype(type: Monotype): Monotype {
    // If there is no overlap between this type’s free type variables and the
    // type variables we want to substitute then we don’t need to recurse.
    if (!overlapsWithSubstitutions(Type.getFreeVariables(type))) return type;

    switch (type.description.kind) {
      case 'Variable': {
        const newType = substitutions.get(type.description.name);
        return newType || type;
      }

      case 'Function': {
        return Type.function_(
          instantiateMonotype(type.description.parameter),
          instantiateMonotype(type.description.body)
        );
      }

      case 'Boolean':
      case 'Number':
      case 'String':
        return type;

      default:
        const never: never = type.description;
        return never;
    }
  }

  // Check to see if the free type variables set we were provided overlaps with
  // the `substitutions` type variable set. If they overlap return true.
  // Otherwise return false.
  function overlapsWithSubstitutions(
    freeVariables: Immutable.Set<string>
  ): boolean {
    // Iterate over the smaller set.
    if (freeVariables.size <= substitutions.distinctKeysCount()) {
      for (const variable of freeVariables) {
        if (substitutions.has(variable)) return true;
      }
    } else {
      for (const variable of substitutions.distinctKeys()) {
        if (freeVariables.has(variable)) return true;
      }
    }
    return false;
  }
}

/**
 * State required to type-check a Brite application. Holds things like allocated
 * type variables.
 */
export class UnifyState {
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
