import * as t from './builder';
import {Diagnostics, Reported} from './diagnostics';
import {Prefix} from './prefix';
import {Bound, MonomorphicType, Type} from './type';

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
): Reported<UnifyError<Diagnostic>> | undefined {
  const actualPrefix = new Prefix(prefix);
  const expectedPrefix = new Prefix(prefix);
  return unifyType(diagnostics, actualPrefix, expectedPrefix, actual, expected);
}

/**
 * Unifies two monomorphic types.
 */
function unifyType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  actualPrefix: Prefix,
  expectedPrefix: Prefix,
  actual: MonomorphicType,
  expected: MonomorphicType
): Reported<UnifyError<Diagnostic>> | undefined {
  if (actual.kind === 'Variable' && expected.kind === 'Variable') {
    return unifyVariable(
      diagnostics,
      actualPrefix,
      expectedPrefix,
      actual.identifier,
      expected.identifier
    );
  } else if (actual.kind === 'Variable') {
    return unifyVariableWithType(
      diagnostics,
      actualPrefix,
      expectedPrefix,
      actual.identifier,
      expected,
      true
    );
  } else if (expected.kind === 'Variable') {
    return unifyVariableWithType(
      diagnostics,
      expectedPrefix,
      actualPrefix,
      expected.identifier,
      actual,
      false
    );
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
    return undefined;
  } else if (
    // Functions unify their parameter and body types.
    actual.kind === 'Function' &&
    expected.kind === 'Function'
  ) {
    // NOTE: We switch `expected` and `actual` when we unify the function
    // parameter since the function parameter is contravariant.
    const diagnostic1 = unifyType(
      diagnostics,
      expectedPrefix,
      actualPrefix,
      expected.parameter,
      actual.parameter
    );
    const diagnostic2 = unifyType(
      diagnostics,
      actualPrefix,
      expectedPrefix,
      actual.body,
      expected.body
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
  variablePrefix: Prefix,
  typePrefix: Prefix,
  variable: string,
  type: MonomorphicType,
  isVariableActual: boolean
): Reported<UnifyError<Diagnostic>> | undefined {
  const bound = variablePrefix.get(variable);
  if (bound === undefined) throw new Error('Could not find type.');
  let variableType: Type = bound.type;

  // If our type variable has a monomorphic bound then we unify the variable
  // bound and the type without updating our variable.
  if (Type.isMonomorphic(variableType)) {
    return unifyType(
      diagnostics,
      isVariableActual ? variablePrefix : typePrefix,
      isVariableActual ? typePrefix : variablePrefix,
      isVariableActual ? variableType : type,
      isVariableActual ? type : variableType
    );
  } else {
    // Push all of our quantifications into scope.
    let bindings = 0;
    while (variableType.kind === 'Quantified') {
      bindings++;
      variablePrefix.push(variableType.binding, variableType.bound);
      variableType = variableType.body;
    }

    // If our type variable is the bottom type then update our variable to the
    // type we are unifying to. Remember to take our quantifications out of
    // scope as well!
    if (variableType.kind === 'Bottom') {
      for (let i = 0; i < bindings; i++) variablePrefix.pop(t.bottomType);
      const bound: Bound = {kind: 'rigid', type};
      Prefix.update(variablePrefix, typePrefix, variable, bound);
      return undefined;
    }

    // Unify the variable’s monomorphic bound with our type.
    const error = unifyType(
      diagnostics,
      isVariableActual ? variablePrefix : typePrefix,
      isVariableActual ? typePrefix : variablePrefix,
      isVariableActual ? variableType : type,
      isVariableActual ? type : variableType
    );

    // Take all of our quantifications out of scope.
    for (let i = 0; i < bindings; i++) variablePrefix.pop(t.bottomType);

    // If there was no error then update our variable to the monomorphic type
    // we unified with since our variable and the type are equivalent.
    if (error === undefined) {
      const bound: Bound = {kind: 'rigid', type};
      Prefix.update(variablePrefix, typePrefix, variable, bound);
    }

    return error;
  }
}

/**
 * Unifies two type variables.
 */
function unifyVariable<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  actualPrefix: Prefix,
  expectedPrefix: Prefix,
  actualVariable: string,
  expectedVariable: string
): Reported<UnifyError<Diagnostic>> | undefined {
  // If both the actual variable and expected variable are in the parent of
  // their respective prefix then they are identical since the actual prefix
  // and expected prefix share a common parent. We don’t need to do any
  // unification and may instead immediately return.
  if (
    actualPrefix.isInParent(actualVariable) &&
    expectedPrefix.isInParent(expectedVariable)
  ) {
    return undefined;
  }

  // Find the actual variable’s bound.
  const actualBound = actualPrefix.get(actualVariable);
  if (actualBound === undefined) throw new Error('Could not find type.');

  // Find the expected variable’s bound.
  const expectedBound = expectedPrefix.get(expectedVariable);
  if (expectedBound === undefined) throw new Error('Could not find type.');

  // Push all of our actual quantifications into scope.
  let actual = actualBound.type;
  let actualBindings = 0;
  while (actual.kind === 'Quantified') {
    actualBindings++;
    actualPrefix.push(actual.binding, actual.bound);
    actual = actual.body;
  }

  // Push all of our expected quantifications into scope.
  let expected = expectedBound.type;
  let expectedBindings = 0;
  while (expected.kind === 'Quantified') {
    expectedBindings++;
    expectedPrefix.push(expected.binding, expected.bound);
    expected = expected.body;
  }

  // Get the bound kind for the bound we will create between our two type
  // variables. The bound is rigid unless both bounds are flexible.
  const boundKind: 'flexible' | 'rigid' =
    actualBound.kind === 'flexible' && expectedBound.kind === 'flexible'
      ? 'flexible'
      : 'rigid';

  // If actual is the bottom type then unify to expected. Make sure to take all
  // the type variables out of scope.
  if (actual.kind === 'Bottom') {
    for (let i = 0; i < expectedBindings; i++) {
      expected = expectedPrefix.pop(expected);
    }
    for (let i = 0; i < actualBindings; i++) actualPrefix.pop(t.bottomType);
    const bound = {kind: boundKind, type: expected};
    Prefix.update(actualPrefix, actualPrefix, actualVariable, bound);
    Prefix.update(expectedPrefix, actualPrefix, expectedVariable, bound);
    return undefined;
  }

  // If expected is the bottom type then unify to actual. Make sure to take all
  // the type variables out of scope.
  if (expected.kind === 'Bottom') {
    for (let i = 0; i < actualBindings; i++) {
      actual = actualPrefix.pop(actual);
    }
    for (let i = 0; i < expectedBindings; i++) expectedPrefix.pop(t.bottomType);
    const bound = {kind: boundKind, type: actual};
    Prefix.update(actualPrefix, actualPrefix, actualVariable, bound);
    Prefix.update(expectedPrefix, actualPrefix, expectedVariable, bound);
    return undefined;
  }

  // Unify the actual and expected types.
  const error = unifyType(
    diagnostics,
    actualPrefix,
    expectedPrefix,
    actual,
    expected
  );

  // If unification was successful then unify both actual and expected to the
  // actual type. Since unification shows that the actual and expected types
  // are equivalent.
  if (error === undefined) {
    // Quantify the actual type.
    for (let i = 0; i < actualBindings; i++) {
      actual = actualPrefix.pop(actual);
    }
    // Update both the actual and expected variable to the actual type. Since
    // unification was successful we can assume the actual and expected types
    // are the same.
    const bound = {kind: boundKind, type: actual};
    Prefix.update(actualPrefix, actualPrefix, actualVariable, bound);
    Prefix.update(expectedPrefix, actualPrefix, expectedVariable, bound);
  } else {
    // If there was an error then take the actual bindings out of scope and
    // don’t do anything with them.
    for (let i = 0; i < actualBindings; i++) actualPrefix.pop(t.bottomType);
  }

  // Take all the expected bindings out of scope. We definitely won’t need them
  // but we might need the actual bindings.
  for (let i = 0; i < expectedBindings; i++) expectedPrefix.pop(t.bottomType);

  return error;
}

export type UnifyError<T> =
  | T
  | {
      readonly kind: 'IncompatibleTypes';
      readonly actual: MonomorphicType;
      readonly expected: MonomorphicType;
    };
