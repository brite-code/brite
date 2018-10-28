import * as Immutable from 'immutable';

import {BindingMap} from './bindings';
import {Diagnostics, Reported} from './diagnostics';
import {UnifyState} from './state';
import {Monotype, Polytype, Type} from './type';

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
  return unifyType(diagnostics, state, actual, expected);
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
 * Unifies two monomorphic types.
 */
function unifyType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: UnifyState,
  actual: Monotype,
  expected: Monotype
): Reported<UnifyError<Diagnostic>> | undefined {
  if (
    actual.description.kind === 'Variable' &&
    expected.description.kind === 'Variable'
  ) {
    return unifyVariable(
      diagnostics,
      state,
      actual.description.name,
      expected.description.name
    );
  } else if (actual.description.kind === 'Variable') {
    const variable = actual.description.name;
    return unifyVariableWithType(diagnostics, state, expected, variable, true);
  } else if (expected.description.kind === 'Variable') {
    const variable = expected.description.name;
    return unifyVariableWithType(diagnostics, state, actual, variable, false);
  } else if (
    // Matching constants unify.
    (actual.description.kind === 'Boolean' &&
      expected.description.kind === 'Boolean') ||
    (actual.description.kind === 'Number' &&
      expected.description.kind === 'Number') ||
    (actual.description.kind === 'String' &&
      expected.description.kind === 'String')
  ) {
    return undefined;
  } else if (
    // Functions unify their parameter and body types.
    actual.description.kind === 'Function' &&
    expected.description.kind === 'Function'
  ) {
    // NOTE: We switch `expected` and `actual` when we unify the function
    // parameter since the function parameter is contravariant.
    const diagnostic1 = unifyType(
      diagnostics,
      state,
      expected.description.parameter,
      actual.description.parameter
    );
    const diagnostic2 = unifyType(
      diagnostics,
      state,
      actual.description.body,
      expected.description.body
    );
    return diagnostic1 || diagnostic2;
  } else {
    // If two types are not compatible then report a diagnostic and return.
    return diagnostics.report({
      kind: 'IncompatibleTypes',
      actual,
      expected,
    });
  }
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
  if (bound.type === undefined) {
    // Maintains `Q ⊑ Q'` because every type is an instance of bottom.
    return updateVariable(diagnostics, state, variable, type);
  }

  // If our type variable has a monomorphic bound then we unify the variable
  // bound and the type without updating our variable.
  if (Type.isMonotype(bound.type)) {
    return unifyType(
      diagnostics,
      state,
      isVariableActual ? bound.type : type,
      isVariableActual ? type : bound.type
    );
  } else {
    // Instantiate the variable type.
    const variableType = instantiate(state, bound.type);

    // Unify the variable’s monomorphic bound with our type.
    const error = unifyType(
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
  if (actualBound.type !== undefined && Type.isMonotype(actualBound.type)) {
    const expectedType = Type.variable(expectedVariable);
    return unifyType(diagnostics, state, actualBound.type, expectedType);
  }

  // If the expected bound is a monotype then let’s try unification again.
  if (expectedBound.type !== undefined && Type.isMonotype(expectedBound.type)) {
    const actualType = Type.variable(actualVariable);
    return unifyType(diagnostics, state, actualType, expectedBound.type);
  }

  // If actual is the bottom type then unify to expected.
  if (actualBound.type === undefined) {
    const expectedType = Type.variable(expectedVariable);
    // Maintains `Q ⊑ Q'` because every type is an instance of bottom.
    return updateVariable(diagnostics, state, actualVariable, expectedType);
  }

  // If expected is the bottom type then unify to actual.
  if (expectedBound.type === undefined) {
    const actualType = Type.variable(actualVariable);
    // Maintains `Q ⊑ Q'` because every type is an instance of bottom.
    return updateVariable(diagnostics, state, expectedVariable, actualType);
  }

  // Instantiate our type bounds.
  const actualType = instantiate(state, actualBound.type);
  const expectedType = instantiate(state, expectedBound.type);

  // Unify the actual and expected types.
  const error = unifyType(diagnostics, state, actualType, expectedType);

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
function instantiate(state: UnifyState, type: Polytype): Monotype {
  if (type.description.kind !== 'Quantify') return type as Monotype; // Shortcut
  const substitutions = new BindingMap<string, Monotype>();
  return instantiatePolytype(type);

  // Decompose any quantified polytypes and add their bounds to our
  // `substitutions` map. Then recurse into the  resulting monotype to apply
  // the substitutions.
  function instantiatePolytype(type: Polytype): Monotype {
    // Decompose the quantified type by instantiating the bounds and creating
    // fresh types for the instantiated bounds.
    let pops = 0;
    while (type.description.kind === 'Quantify') {
      const {name, bound} = type.description;
      const newType = state.newTypeWithBound({
        kind: bound.kind,
        type:
          bound.type !== undefined
            ? instantiatePolytype(bound.type)
            : undefined,
      });
      substitutions.push(name, newType);
      pops++;
      type = type.description.body;
    }

    // If we have some substitutions then instantiate the monotype before
    // returning it.
    const instantiatedType = instantiateMonotype(type as Monotype);

    // Take all our quantified types out of the substitutions map.
    for (let i = 0; i < pops; i++) substitutions.pop();

    return instantiatedType;
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
