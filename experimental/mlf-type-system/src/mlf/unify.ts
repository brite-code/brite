import * as t from './builder';
import {Diagnostics, Reported} from './diagnostics';
import {Prefix} from './prefix';
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
  return unifyMonomorphicType(
    diagnostics,
    prefix,
    new Map(),
    new Map(),
    actual,
    expected
  );
}

/**
 * Unifies two monomorphic types.
 */
function unifyMonomorphicType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  commonPrefix: Prefix,
  actualPrefix: Map<string, Bound>,
  expectedPrefix: Map<string, Bound>,
  actual: MonomorphicType,
  expected: MonomorphicType
): {
  readonly error: Reported<UnifyError<Diagnostic>> | undefined;
} {
  if (actual.kind === 'Variable' && expected.kind === 'Variable') {
    // Find the actual variable’s bound.
    let isActualCommon = false;
    let actualBound = actualPrefix.get(actual.identifier);
    if (actualBound === undefined) {
      isActualCommon = true;
      actualBound = commonPrefix.get(actual.identifier);
      if (actualBound === undefined) throw new Error('Could not find type.');
    }

    // Find the expected variable’s bound.
    let isExpectedCommon = false;
    let expectedBound = expectedPrefix.get(expected.identifier);
    if (expectedBound === undefined) {
      isExpectedCommon = true;
      expectedBound = commonPrefix.get(expected.identifier);
      if (expectedBound === undefined) throw new Error('Could not find type.');
    }

    // If both the actual variable and expected variable are in the common
    // prefix then they are identical. We don’t need to do any unification and
    // may instead immediately return.
    if (isActualCommon && isExpectedCommon) {
      return {error: undefined};
    }

    // Unify the two polymorphic types.
    const {type, error} = unifyPolymorphicType(
      diagnostics,
      commonPrefix,
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
    if (isActualCommon) {
      commonPrefix.update(actual.identifier, bound);
    } else {
      actualPrefix.set(actual.identifier, bound);
    }
    if (isExpectedCommon) {
      commonPrefix.update(expected.identifier, bound);
    } else {
      expectedPrefix.set(expected.identifier, bound);
    }

    return {error};
  } else if (actual.kind === 'Variable' || expected.kind === 'Variable') {
    // Get all the shared variable information from actual and expected. In
    // another language we might select all this in our pattern match.
    let isActual: boolean;
    let identifier: string;
    let variablePrefix: Map<string, Bound>;
    if (actual.kind === 'Variable') {
      isActual = true;
      identifier = actual.identifier;
      variablePrefix = actualPrefix;
    } else if (expected.kind === 'Variable') {
      isActual = false;
      identifier = expected.identifier;
      variablePrefix = expectedPrefix;
    } else {
      throw new Error('Unreachable');
    }

    // Resolve the bound. First we check the specific prefix for our
    // actual/expected variable. If it does not exist in there then we check the
    // common prefix for both types. If it does not exist in there then
    // we panic.
    let bound: Bound | undefined = variablePrefix.get(identifier);
    let isCommon = false;
    if (bound === undefined) {
      bound = commonPrefix.get(identifier);
      isCommon = true;
      // TODO: If, as a programmer I write `∀(y)x` where `x` is not defined
      // anywhere then I expect to get a diagnostic, not a panic. Also, we need
      // to make sure that temporary variables allocated by `Prefix.all()` are
      // not accessible by the programmer’s type annotations.
      if (bound === undefined) throw new Error('Could not find type.');
    }

    // If the bound type is monomorphic then we do a monomorphic unification
    // with our current type. If it is polymorphic then we do a polymorphic
    // unification and update the bound.
    if (Type.isMonomorphic(bound.type)) {
      return unifyMonomorphicType(
        diagnostics,
        commonPrefix,
        actualPrefix,
        expectedPrefix,
        isActual ? bound.type : actual,
        isActual ? expected : bound.type
      );
    } else {
      const {error} = unifyPolymorphicType(
        diagnostics,
        commonPrefix,
        actualPrefix,
        expectedPrefix,
        isActual ? bound.type : actual,
        isActual ? expected : bound.type
      );
      // Update the type variable’s bound in the respective prefix.
      const newBound: Bound = {
        kind: 'rigid',
        type: isActual ? expected : actual,
      };
      if (isCommon) {
        commonPrefix.update(identifier, newBound);
      } else {
        variablePrefix.set(identifier, newBound);
      }
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
      commonPrefix,
      expectedPrefix,
      actualPrefix,
      expected.parameter,
      actual.parameter
    );
    const {error: diagnostic2} = unifyMonomorphicType(
      diagnostics,
      commonPrefix,
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
  commonPrefix: Prefix,
  actualPrefix: Map<string, Bound>,
  expectedPrefix: Map<string, Bound>,
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

  // Get all the new bindings and save all the old bindings from our quantified
  // “actual” type.
  const actualBindingsOld = new Map<string, Bound>();
  const actualBindings: Array<string> = [];
  while (actual.kind === 'Quantified') {
    const {binding, bound} = actual;
    actualBindings.push(binding);
    const oldBound = actualPrefix.get(binding);
    if (oldBound !== undefined) actualBindingsOld.set(binding, oldBound);
    actualPrefix.set(binding, bound);
    actual = actual.body;
  }

  // Get all the new bindings and save all the old bindings from our quantified
  // “expected” type.
  const expectedBindingsOld = new Map<string, Bound>();
  const expectedBindings: Array<string> = [];
  while (expected.kind === 'Quantified') {
    const {binding, bound} = expected;
    expectedBindings.push(binding);
    const oldBound = expectedPrefix.get(binding);
    if (oldBound !== undefined) expectedBindingsOld.set(binding, oldBound);
    expectedPrefix.set(binding, bound);
    expected = expected.body;
  }

  // Bottom unifies with everything.
  if (actual.kind === 'Bottom') {
    return {type: expected, error: undefined};
  } else if (expected.kind === 'Bottom') {
    return {type: actual, error: undefined};
  }

  // Unify the actual quantified type body and the expected quantified
  // type body.
  const {error} = unifyMonomorphicType(
    diagnostics,
    commonPrefix,
    actualPrefix,
    expectedPrefix,
    actual,
    expected
  );

  // If we did not have an error then actual and expected are equivalent. So we
  // pick actual and return it. Which type we pick doesn’t matter since they
  // are equivalent.
  //
  // IMPORTANT: However, If there was an error then the types are not
  // equivalent! Instead we return the bottom type with the error in that case.
  const bindings = new Map<string, Bound>();

  // Reverses `actualBindings` before adding them to `bindings`. This way they
  // will be added in reverse order so we may easily construct a quantified
  // type from them.
  actualBindings.reverse();

  // Restore our “actual” prefix to its old state. Before we added
  // new bindings.
  for (const binding of actualBindings) {
    const newBound = actualPrefix.get(binding)!; // tslint:disable-line no-non-null-assertion
    bindings.set(binding, newBound);

    const oldBound = actualBindingsOld.get(binding);
    if (oldBound === undefined) {
      actualPrefix.delete(binding);
    } else {
      actualPrefix.set(binding, oldBound);
    }
  }

  // Restore our “expected” prefix to its old state. Before we added
  // new bindings.
  for (const binding of expectedBindings) {
    const oldBound = expectedBindingsOld.get(binding);
    if (oldBound === undefined) {
      expectedPrefix.delete(binding);
    } else {
      expectedPrefix.set(binding, oldBound);
    }
  }

  // If we did not have an error then actual and expected are equivalent. So we
  // pick actual and return it. Although it shouldn’t matter since both types
  // are equivalent.
  //
  // If there was an error then we return the bottom type with the error.
  if (error === undefined) {
    let type: Type = actual;
    for (const [binding, bound] of bindings) {
      type = t.quantifiedType(binding, bound, type);
    }
    return {type, error: undefined};
  } else {
    return {type: t.bottomType, error};
  }
}

export type UnifyError<T> =
  | T
  | {
      readonly kind: 'IncompatibleTypes';
      readonly actual: MonomorphicType;
      readonly expected: MonomorphicType;
    };
