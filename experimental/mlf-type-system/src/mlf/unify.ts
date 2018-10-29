import * as Immutable from 'immutable';

import {BindingMap} from '../utils/bindings';
import {Ok, Result} from '../utils/result';

import {Diagnostics, Reported} from './diagnostics';
import {State} from './state';
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
  state: State,
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
  state: State,
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
  state: State,
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
function mergePrefix(state: State, type: Polytype): Monotype | undefined {
  while (type.description.kind === 'Quantify') {
    type = type.description.body;
  }

  if (type.description.kind === 'Bottom') return undefined;
  return type as Monotype;
}
