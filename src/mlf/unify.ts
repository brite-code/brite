import * as Immutable from 'immutable';

import {Err, Ok, Result} from '../utils/result';

import {Diagnostics, Reported} from './diagnostics';
import {TypeIdentifier} from './identifier';
import {
  BottomType,
  Bound,
  MonomorphicType,
  PolymorphicType,
  Prefix,
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
function unifyMonomorphicType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  prefixes: Immutable.List<Prefix>,
  actual: MonomorphicType,
  expected: MonomorphicType
): {
  readonly prefixes: Immutable.List<Prefix>;
  readonly error: Reported<UnifyError<Diagnostic>> | undefined;
} {
  if (
    actual.kind === 'Variable' &&
    expected.kind === 'Variable' &&
    actual.identifier === expected.identifier
  ) {
    return {prefixes, error: undefined};
  } else if (actual.kind === 'Variable' && expected.kind === 'Variable') {
    const {bound: actualBound, index: actualIndex} = resolveTypeVariable(
      prefixes,
      actual.identifier
    );
    const {bound: expectedBound, index: expectedIndex} = resolveTypeVariable(
      prefixes,
      expected.identifier
    );
    if (actualBound === undefined || expectedBound === undefined) {
      throw new Error('Could not find type variable.');
    }
    const {prefixes: newPrefixes, type} = unifyPolymorphicType(
      diagnostics,
      prefixes,
      actualBound.type,
      expectedBound.type
    );
    if (type.kind === 'ok') {
      return {
        prefixes: newPrefixes
          .update(actualIndex, prefix =>
            prefix.set(actual.identifier, {
              kind: actualBound.kind,
              type: type.value,
            })
          )
          .update(expectedIndex, prefix =>
            prefix.set(expected.identifier, {
              kind: expectedBound.kind,
              type: type.value,
            })
          ),
        error: undefined,
      };
    } else {
      return {
        prefixes: newPrefixes
          .update(actualIndex, prefix =>
            prefix.set(actual.identifier, {
              kind: actualBound.kind,
              type: BottomType,
            })
          )
          .update(expectedIndex, prefix =>
            prefix.set(expected.identifier, {
              kind: expectedBound.kind,
              type: BottomType,
            })
          ),
        error: type.value,
      };
    }
  } else if (actual.kind === 'Variable') {
    const {bound, index} = resolveTypeVariable(prefixes, actual.identifier);
    if (bound === undefined) throw new Error('Could not find type variable.');
    if (bound.type.kind !== 'Quantified' && bound.type.kind !== 'Bottom') {
      return unifyMonomorphicType(diagnostics, prefixes, bound.type, expected);
    } else {
      const {prefixes: newPrefixes, type} = unifyPolymorphicType(
        diagnostics,
        prefixes,
        bound.type,
        expected
      );
      return {
        prefixes: newPrefixes.update(index, prefix =>
          prefix.set(actual.identifier, {kind: 'rigid', type: expected})
        ),
        error: type.kind === 'err' ? type.value : undefined,
      };
    }
  } else if (expected.kind === 'Variable') {
    const {bound, index} = resolveTypeVariable(prefixes, expected.identifier);
    if (bound === undefined) throw new Error('Could not find type variable.');
    if (bound.type.kind !== 'Quantified' && bound.type.kind !== 'Bottom') {
      return unifyMonomorphicType(diagnostics, prefixes, actual, bound.type);
    } else {
      const {prefixes: newPrefixes} = unifyPolymorphicType(
        diagnostics,
        prefixes,
        actual,
        bound.type
      );
      return {
        prefixes: newPrefixes.update(index, prefix =>
          prefix.set(expected.identifier, {kind: 'rigid', type: actual})
        ),
        error: undefined,
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
    return {prefixes, error: undefined};
  } else if (actual.kind === 'Function' && expected.kind === 'Function') {
    const {prefixes: prefixes1, error: diagnostic1} = unifyMonomorphicType(
      diagnostics,
      prefixes,
      actual.parameter,
      expected.parameter
    );
    const {prefixes: prefixes2, error: diagnostic2} = unifyMonomorphicType(
      diagnostics,
      prefixes1,
      actual.body,
      expected.body
    );
    return {
      prefixes: prefixes2,
      error: diagnostic1 || diagnostic2,
    };
  } else {
    return {
      prefixes,
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
  prefixes: Immutable.List<Prefix>,
  actual: PolymorphicType,
  expected: PolymorphicType
): {
  readonly prefixes: Immutable.List<Prefix>;
  readonly type: Result<Type, Reported<UnifyError<Diagnostic>>>;
} {
  // Bottom unifies with everything.
  if (actual.kind === 'Bottom') {
    return {prefixes, type: Ok(expected)};
  } else if (expected.kind === 'Bottom') {
    return {prefixes, type: Ok(actual)};
  }

  // If the types are monomorphic then convert them to quantified types with
  // an empty prefix.
  actual =
    actual.kind !== 'Quantified'
      ? {kind: 'Quantified', prefix: Prefix.empty, body: actual}
      : actual;
  expected =
    expected.kind !== 'Quantified'
      ? {kind: 'Quantified', prefix: Prefix.empty, body: expected}
      : expected;

  // Bottom unifies with everything.
  if (actual.body.kind === 'Bottom') return {prefixes, type: Ok(expected)};
  if (expected.body.kind === 'Bottom') return {prefixes, type: Ok(expected)};

  const {prefixes: newPrefixes, error} = unifyMonomorphicType(
    diagnostics,
    prefixes.unshift(actual.prefix.merge(expected.prefix)), // Assumes `List.unshift()` is O(1).
    actual.body,
    expected.body
  );

  if (!error) {
    const prefix = newPrefixes.first()!; // tslint:disable-line no-non-null-assertion
    const type: Type = {kind: 'Quantified', prefix, body: actual.body};
    return {
      prefixes: newPrefixes.shift(), // Assumes `List.shift()` is O(1).
      type: Ok(type),
    };
  } else {
    return {
      prefixes,
      type: Err(error),
    };
  }
}

function resolveTypeVariable(
  prefixes: Immutable.List<Prefix>,
  identifier: TypeIdentifier
): {
  readonly bound: Bound | undefined;
  readonly index: number;
} {
  let index = 0;
  for (const prefix of prefixes) {
    const bound = prefix.get(identifier);
    if (bound !== undefined) return {bound, index};
    index += 1;
  }
  return {bound: undefined, index: -1};
}

export type UnifyError<T> =
  | T
  | {
      readonly type: 'IncompatibleTypes';
      readonly expected: MonomorphicType;
      readonly actual: MonomorphicType;
    };
