import * as Immutable from 'immutable';

import {Diagnostics, Reported} from './diagnostics';
import {Prefix} from './prefix';
import {
  BottomType,
  Bound,
  MonomorphicType,
  PolymorphicType,
  Type,
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
  readonly prefix: Prefix;
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
  readonly prefix: Prefix;
  readonly error: Reported<UnifyError<Diagnostic>> | undefined;
} {
  if (
    actual.kind === 'Variable' &&
    expected.kind === 'Variable' &&
    actual.identifier === expected.identifier
  ) {
    return {prefix, error: undefined};
  } else if (actual.kind === 'Variable' && expected.kind === 'Variable') {
    const actualBound = prefix.find(actual.identifier);
    const expectedBound = prefix.find(expected.identifier);
    const {prefix: newPrefix, type, error} = unifyPolymorphicType(
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
    return {
      prefix: newPrefix
        .update(actual.identifier, bound)
        .update(expected.identifier, bound),
      error,
    };
  } else if (actual.kind === 'Variable') {
    const bound = prefix.find(actual.identifier);
    if (bound.type.kind !== 'Quantified' && bound.type.kind !== 'Bottom') {
      return unifyMonomorphicType(diagnostics, prefix, bound.type, expected);
    } else {
      const {prefix: newPrefix, error} = unifyPolymorphicType(
        diagnostics,
        prefix,
        bound.type,
        expected
      );
      const newBound: Bound = {kind: 'rigid', type: expected};
      return {
        prefix: newPrefix.update(actual.identifier, newBound),
        error,
      };
    }
  } else if (expected.kind === 'Variable') {
    const bound = prefix.find(expected.identifier);
    if (bound.type.kind !== 'Quantified' && bound.type.kind !== 'Bottom') {
      return unifyMonomorphicType(diagnostics, prefix, actual, bound.type);
    } else {
      const {prefix: newPrefix, error} = unifyPolymorphicType(
        diagnostics,
        prefix,
        actual,
        bound.type
      );
      const newBound: Bound = {kind: 'rigid', type: actual};
      return {
        prefix: newPrefix.update(expected.identifier, newBound),
        error,
      };
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
    return {prefix, error: undefined};
  } else if (actual.kind === 'Function' && expected.kind === 'Function') {
    const {prefix: prefix1, error: diagnostic1} = unifyMonomorphicType(
      diagnostics,
      prefix,
      actual.parameter,
      expected.parameter
    );
    const {prefix: prefix2, error: diagnostic2} = unifyMonomorphicType(
      diagnostics,
      prefix1,
      actual.body,
      expected.body
    );
    return {
      prefix: prefix2,
      error: diagnostic1 || diagnostic2,
    };
  } else {
    return {
      prefix,
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
  readonly prefix: Prefix;
  readonly type: Type;
  readonly error: Reported<UnifyError<Diagnostic>> | undefined;
} {
  // Bottom unifies with everything.
  if (actual.kind === 'Bottom') {
    return {prefix, type: expected, error: undefined};
  } else if (expected.kind === 'Bottom') {
    return {prefix, type: actual, error: undefined};
  }

  // If the types are monomorphic then convert them to quantified types with
  // an empty prefix.
  actual =
    actual.kind !== 'Quantified'
      ? {kind: 'Quantified', prefix: Immutable.Map(), body: actual}
      : actual;
  expected =
    expected.kind !== 'Quantified'
      ? {kind: 'Quantified', prefix: Immutable.Map(), body: expected}
      : expected;

  // Bottom unifies with everything.
  if (actual.body.kind === 'Bottom') {
    return {prefix, type: expected, error: undefined};
  } else if (expected.body.kind === 'Bottom') {
    return {prefix, type: actual, error: undefined};
  }

  const {prefix: prefix1, error} = unifyMonomorphicType(
    diagnostics,
    prefix.pushScope(actual.prefix.merge(expected.prefix)), // TODO: Domain collisions?
    actual.body,
    expected.body
  );

  const {prefix: prefix2, bindings} = prefix1.popScope();

  const type: Type = {
    kind: 'Quantified',
    prefix: bindings,
    // If we had an error then `actual` and `expected` aren’t equivalent. We can
    // only get away with returning `actual` if we believe it to be equivalent
    // with `expected`. Instead return the bottom type.
    body: error !== undefined ? actual.body : BottomType,
  };

  return {prefix: prefix2, type, error};
}

export type UnifyError<T> =
  | T
  | {
      readonly type: 'IncompatibleTypes';
      readonly expected: MonomorphicType;
      readonly actual: MonomorphicType;
    };
