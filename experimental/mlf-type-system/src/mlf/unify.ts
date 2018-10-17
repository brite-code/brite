import * as t from './builder';
import {Diagnostics, Reported} from './diagnostics';
import {ForkedPrefix, Prefix} from './prefix';
import {Bound, MonomorphicType, PolymorphicType, Type} from './type';

/**
 * Implements the unification algorithm from Appendix A in the MLF paper
 * [“Raising ML to the Power of System F”][1].
 *
 * Modified to support our custom types and to support error recovery.
 *
 * When unification encounters incompatible types we don’t fatal. Instead we
 * continue type-checking. The unification caller should is responsible for
 * ensuring that failed unifications crash the program at runtime.
 *
 * We don’t yet know if our error-recovery behavior here is sound. However,
 * we are comfortable giving up soundness guarantees in the presence of a type
 * error. No program should be shipped to production if it has a type error.
 * Programs executed with type-errors are purely a development time convenience.
 *
 * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
 */
export function unify<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  prefix: Prefix,
  actual: MonomorphicType,
  expected: MonomorphicType
): {
  readonly error: Reported<UnifyError<Diagnostic>> | undefined;
} {
  const actualPrefix = new ForkedPrefix(prefix);
  const expectedPrefix = new ForkedPrefix(prefix);
  const result = unifyMonomorphicType(
    diagnostics,
    actualPrefix,
    expectedPrefix,
    actual,
    expected
  );
  if (!actualPrefix.isLocallyEmpty() || !expectedPrefix.isLocallyEmpty()) {
    throw new Error('Must cleanup all local type variables.');
  }
  return result;
}

/**
 * Unifies two monomorphic types.
 */
function unifyMonomorphicType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  actualPrefix: ForkedPrefix,
  expectedPrefix: ForkedPrefix,
  actual: MonomorphicType,
  expected: MonomorphicType
): {
  readonly error: Reported<UnifyError<Diagnostic>> | undefined;
} {
  if (actual.kind === 'Variable' && expected.kind === 'Variable') {
    // If both the actual variable and expected variable are in the parent of
    // their respective prefix then they are identical since the actual prefix
    // and expected prefix share a common parent. We don’t need to do any
    // unification and may instead immediately return.
    if (
      actualPrefix.isInParent(actual.identifier) &&
      expectedPrefix.isInParent(expected.identifier)
    ) {
      return {error: undefined};
    }

    // Find the actual variable’s bound.
    const actualBound = actualPrefix.get(actual.identifier);
    if (actualBound === undefined) throw new Error('Could not find type.');

    // Find the expected variable’s bound.
    const expectedBound = expectedPrefix.get(expected.identifier);
    if (expectedBound === undefined) throw new Error('Could not find type.');

    // Unify the two polymorphic types.
    const {type, error} = unifyPolymorphicType(
      diagnostics,
      actualPrefix,
      expectedPrefix,
      actualBound.type,
      expectedBound.type
    );

    // Create the new bound for the type variables. It is a flexible bound if
    // both the actual and expected bounds are also flexible.
    const bound: Bound = {
      kind:
        actualBound.kind === 'flexible' && expectedBound.kind === 'flexible'
          ? 'flexible'
          : 'rigid',
      type,
    };

    // Update the actual and expected type variables in their
    // respective prefixes.
    //
    // If both the actual and expected type variables are defined in the common
    // parent prefix then we won’t reach this point since we’ll instead be
    // returning early above.
    actualPrefix.update(actual.identifier, bound);
    expectedPrefix.update(expected.identifier, bound);

    return {error};
  } else if (actual.kind === 'Variable' || expected.kind === 'Variable') {
    // Get all the shared variable information from actual and expected. In
    // another language we might select all this in our pattern match.
    let isActual: boolean;
    let identifier: string;
    if (actual.kind === 'Variable') {
      isActual = true;
      identifier = actual.identifier;
    } else if (expected.kind === 'Variable') {
      isActual = false;
      identifier = expected.identifier;
    } else {
      throw new Error('Unreachable');
    }

    // Find the variable’s bound.
    const bound = (isActual ? actualPrefix : expectedPrefix).get(identifier);
    if (bound === undefined) throw new Error('Could not find type.');

    // If the bound type is monomorphic then we do a monomorphic unification
    // with our current type. If it is polymorphic then we do a polymorphic
    // unification and update the bound.
    if (Type.isMonomorphic(bound.type)) {
      return unifyMonomorphicType(
        diagnostics,
        actualPrefix,
        expectedPrefix,
        isActual ? bound.type : actual,
        isActual ? expected : bound.type
      );
    } else {
      const {error} = unifyPolymorphicType(
        diagnostics,
        actualPrefix,
        expectedPrefix,
        isActual ? bound.type : actual,
        isActual ? expected : bound.type
      );
      // Update the type variable’s bound in the respective prefix.
      (isActual ? actualPrefix : expectedPrefix).update(identifier, {
        kind: 'rigid',
        type: isActual ? expected : actual,
      });
      return {error};
    }
  } else if (
    // Matching constants unify.
    actual.kind === 'Constant' &&
    expected.kind === 'Constant' &&
    ((actual.constant.kind === 'Boolean' &&
      expected.constant.kind === 'Boolean') ||
      (actual.constant.kind === 'Number' &&
        expected.constant.kind === 'Number') ||
      (actual.constant.kind === 'String' &&
        expected.constant.kind === 'String'))
  ) {
    return {error: undefined};
  } else if (
    // Functions unify their parameter and body types.
    actual.kind === 'Function' &&
    expected.kind === 'Function'
  ) {
    // NOTE: We switch `expected` and `actual` when we unify the function
    // parameter since the function parameter is contravariant.
    const {error: diagnostic1} = unifyMonomorphicType(
      diagnostics,
      expectedPrefix,
      actualPrefix,
      expected.parameter,
      actual.parameter
    );
    const {error: diagnostic2} = unifyMonomorphicType(
      diagnostics,
      actualPrefix,
      expectedPrefix,
      actual.body,
      expected.body
    );
    return {error: diagnostic1 || diagnostic2};
  } else {
    // If two types are not compatible then report a diagnostic and return.
    return {
      error: diagnostics.report({
        kind: 'IncompatibleTypes',
        actual,
        expected,
      }),
    };
  }
}

/**
 * Unifies two polymorphic types.
 */
function unifyPolymorphicType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  actualPrefix: ForkedPrefix,
  expectedPrefix: ForkedPrefix,
  actual: PolymorphicType,
  expected: PolymorphicType
): {
  readonly type: Type;
  readonly error: Reported<UnifyError<Diagnostic>> | undefined;
} {
  // Bottom unifies with everything.
  if (actual.kind === 'Bottom') {
    return {type: expected, error: undefined};
  } else if (expected.kind === 'Bottom') {
    return {type: actual, error: undefined};
  }

  // Push all the actual bindings into our actual prefix.
  const actualBindings: Array<string> = [];
  while (actual.kind === 'Quantified') {
    const {binding, bound} = actual;
    actualBindings.push(binding);
    actualPrefix.push(binding, bound);
    actual = actual.body;
  }
  // Reverse actual bindings since we’ll need to call `Prefix.pop()` in the
  // opposite order we called `Prefix.push()`.
  actualBindings.reverse();

  // Push all the expected bindings into our expected prefix.
  const expectedBindings: Array<string> = [];
  while (expected.kind === 'Quantified') {
    const {binding, bound} = expected;
    expectedBindings.push(binding);
    expectedPrefix.push(binding, bound);
    expected = expected.body;
  }
  // Reverse expected bindings since we’ll need to call `Prefix.pop()` in the
  // opposite order we called `Prefix.push()`.
  expectedBindings.reverse();

  // Bottom unifies with everything. While we have this check at the top of our
  // function we need to check again here. We also need to make sure and clean
  // up our prefixes
  if (actual.kind === 'Bottom') {
    for (const binding of actualBindings) actualPrefix.pop(binding);
    for (const binding of expectedBindings) expectedPrefix.pop(binding);
    return {type: expected, error: undefined};
  } else if (expected.kind === 'Bottom') {
    for (const binding of actualBindings) actualPrefix.pop(binding);
    for (const binding of expectedBindings) expectedPrefix.pop(binding);
    return {type: actual, error: undefined};
  }

  // Unify the actual quantified type body and the expected quantified
  // type body.
  const {error} = unifyMonomorphicType(
    diagnostics,
    actualPrefix,
    expectedPrefix,
    actual,
    expected
  );

  // We return the unified type from this function. If there was no error then
  // the types are equivalent so we pick the “actual” type to return. However,
  // if there was an error we can’t just pick a winner. So instead we return the
  // bottom type.
  let type: Type = error !== undefined ? t.bottomType : actual;

  // Cleanup the actual type bindings and also quantify our return type.
  for (const binding of actualBindings) {
    if (error === undefined) {
      type = actualPrefix.quantify(binding, type);
    } else {
      actualPrefix.pop(binding);
    }
  }

  // Cleanup the expected type bindings.
  for (const binding of expectedBindings) expectedPrefix.pop(binding);

  // Return the unified type and our error.
  return {type, error};
}

export type UnifyError<T> =
  | T
  | {
      readonly kind: 'IncompatibleTypes';
      readonly actual: MonomorphicType;
      readonly expected: MonomorphicType;
    };
