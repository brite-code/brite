import {Diagnostics, Reported} from './diagnostics';
import {Prefix} from './prefix';
import {
  BottomType,
  Bound,
  MonomorphicType,
  PolymorphicType,
  Type,
  TypeIdentifier,
} from './type';

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
  return unifyMonomorphicType(diagnostics, prefix, actual, expected);
}

function unifyMonomorphicType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  prefix: Prefix,
  actual: MonomorphicType,
  expected: MonomorphicType
): {
  readonly error: Reported<UnifyError<Diagnostic>> | undefined;
} {
  if (
    actual.kind === 'Variable' &&
    expected.kind === 'Variable' &&
    actual.identifier === expected.identifier
  ) {
    return {error: undefined};
  } else if (actual.kind === 'Variable' && expected.kind === 'Variable') {
    const actualBound = prefix.find(actual.identifier);
    const expectedBound = prefix.find(expected.identifier);
    const {type, error} = unifyPolymorphicType(
      diagnostics,
      prefix,
      actualBound.type,
      expectedBound.type
    );
    const kind =
      actualBound.kind === 'flexible' && expectedBound.kind === 'flexible'
        ? 'flexible'
        : 'rigid';
    const bound: Bound = {kind, type};
    prefix.update(actual.identifier, bound);
    prefix.update(expected.identifier, bound);
    return {error};
  } else if (actual.kind === 'Variable') {
    const bound = prefix.find(actual.identifier);
    if (bound.type.kind !== 'Quantified' && bound.type.kind !== 'Bottom') {
      return unifyMonomorphicType(diagnostics, prefix, bound.type, expected);
    } else {
      const {error} = unifyPolymorphicType(
        diagnostics,
        prefix,
        bound.type,
        expected
      );
      prefix.update(actual.identifier, {kind: 'rigid', type: expected});
      return {error};
    }
  } else if (expected.kind === 'Variable') {
    const bound = prefix.find(expected.identifier);
    if (bound.type.kind !== 'Quantified' && bound.type.kind !== 'Bottom') {
      return unifyMonomorphicType(diagnostics, prefix, actual, bound.type);
    } else {
      const {error} = unifyPolymorphicType(
        diagnostics,
        prefix,
        actual,
        bound.type
      );
      prefix.update(expected.identifier, {kind: 'rigid', type: actual});
      return {error};
    }
  } else if (
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
  } else if (actual.kind === 'Function' && expected.kind === 'Function') {
    const {error: diagnostic1} = unifyMonomorphicType(
      diagnostics,
      prefix,
      actual.parameter,
      expected.parameter
    );
    const {error: diagnostic2} = unifyMonomorphicType(
      diagnostics,
      prefix,
      actual.body,
      expected.body
    );
    return {error: diagnostic1 || diagnostic2};
  } else {
    return {
      error: diagnostics.report({
        type: 'IncompatibleTypes',
        expected,
        actual,
      }),
    };
  }
}

function unifyPolymorphicType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  prefix: Prefix,
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

  // If the types are monomorphic then convert them to quantified types with
  // an empty prefix.
  const {prefix: actualPrefix, body: actualBody} =
    actual.kind !== 'Quantified'
      ? {prefix: new Map<TypeIdentifier, Bound>(), body: actual}
      : {prefix: actual.prefix, body: actual.body};
  const expectedBody =
    expected.kind !== 'Quantified' ? expected : expected.body;

  // Bottom unifies with everything.
  if (actualBody.kind === 'Bottom') {
    return {type: expected, error: undefined};
  } else if (expectedBody.kind === 'Bottom') {
    return {type: actual, error: undefined};
  }

  // Unify the actual quantified type body and the expected quantified
  // type body.
  const {error} = unifyMonomorphicType(
    diagnostics,
    prefix,
    actualBody,
    expectedBody
  );

  // If we did not have an error then actual and expected are equivalent. So we
  // pick actual and return it. Although it shouldn’t matter since both types
  // are equivalent.
  //
  // If there was an error then we return the bottom type with the error.
  if (error !== undefined) {
    const bindings = new Map<TypeIdentifier, Bound>();
    for (const identifier of actualPrefix.keys()) {
      bindings.set(identifier, prefix.find(identifier));
    }
    const type: Type = {
      kind: 'Quantified',
      prefix: bindings,
      body: error !== undefined ? actualBody : BottomType,
    };
    return {type, error: undefined};
  } else {
    return {type: BottomType, error};
  }
}

export type UnifyError<T> =
  | T
  | {
      readonly type: 'IncompatibleTypes';
      readonly expected: MonomorphicType;
      readonly actual: MonomorphicType;
    };
