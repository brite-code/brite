import * as Immutable from 'immutable';

import {DerivableValue} from '../utils/derive';

import {Diagnostics, Reported} from './diagnostics';
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
  prefix: Immutable.Map<string, TypeVariable>,
  level: number,
  actual: MonomorphicType,
  expected: MonomorphicType
): Reported<UnifyError<Diagnostic>> | undefined {
  return unifyType(diagnostics, level, prefix, prefix, actual, expected);
}

/**
 * Unifies two monomorphic types.
 */
function unifyType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  level: number,
  actualPrefix: Immutable.Map<string, TypeVariable>,
  expectedPrefix: Immutable.Map<string, TypeVariable>,
  actual: MonomorphicType,
  expected: MonomorphicType
): Reported<UnifyError<Diagnostic>> | undefined {
  if (
    actual.description.kind === 'Variable' &&
    expected.description.kind === 'Variable'
  ) {
    // Find the actual variable.
    const actualIdentifier = actual.description.identifier;
    const actualVariable = actualPrefix.get(actualIdentifier);
    if (actualVariable === undefined) throw new Error('Could not find type.');

    // Find the expected variable.
    const expectedIdentifier = expected.description.identifier;
    const expectedVariable = expectedPrefix.get(expectedIdentifier);
    if (expectedVariable === undefined) throw new Error('Could not find type.');

    // If the two variables are referentially identical then we know them to be
    // equal so we don’t need to unify recursively.
    if (actualVariable.equals(expectedVariable)) return undefined;

    // Unify the two variables.
    return unifyVariable(diagnostics, level, actualVariable, expectedVariable);
  } else if (actual.description.kind === 'Variable') {
    // Find the variable.
    const variable = actualPrefix.get(actual.description.identifier);
    if (variable === undefined) throw new Error('Could not find type.');

    return unifyVariableWithType(
      diagnostics,
      level,
      expectedPrefix,
      expected,
      variable,
      true
    );
  } else if (expected.description.kind === 'Variable') {
    // Find the variable.
    const variable = expectedPrefix.get(expected.description.identifier);
    if (variable === undefined) throw new Error('Could not find type.');

    return unifyVariableWithType(
      diagnostics,
      level,
      actualPrefix,
      actual,
      variable,
      false
    );
  } else if (
    // Matching constants unify.
    actual.description.kind === 'Constant' &&
    expected.description.kind === 'Constant' &&
    ((actual.description.constant.kind === 'Boolean' &&
      expected.description.constant.kind === 'Boolean') ||
      (actual.description.constant.kind === 'Number' &&
        expected.description.constant.kind === 'Number') ||
      (actual.description.constant.kind === 'String' &&
        expected.description.constant.kind === 'String'))
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
      level,
      expectedPrefix,
      actualPrefix,
      expected.description.parameter,
      actual.description.parameter
    );
    const diagnostic2 = unifyType(
      diagnostics,
      level,
      actualPrefix,
      expectedPrefix,
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
  level: number,
  typePrefix: Immutable.Map<string, TypeVariable>,
  type: MonomorphicType,
  variable: TypeVariable,
  isVariableActual: boolean
): Reported<UnifyError<Diagnostic>> | undefined {
  let variablePrefix = variable.getPrefix();
  let variableType = variable.getBound().type;

  // If our type variable has a monomorphic bound then we unify the variable
  // bound and the type without updating our variable.
  if (Type.isMonomorphic(variableType)) {
    return unifyType(
      diagnostics,
      level,
      isVariableActual ? variablePrefix : typePrefix,
      isVariableActual ? typePrefix : variablePrefix,
      isVariableActual ? variableType : type,
      isVariableActual ? type : variableType
    );
  } else {
    // Add all of our quantifications into scope.
    while (variableType.description.kind === 'Quantified') {
      variablePrefix = variablePrefix.set(
        variableType.description.binding,
        new TypeVariable(
          new DerivableValue(level),
          variablePrefix,
          variableType.description.bound
        )
      );
      variableType = variableType.description.body;
    }

    // If our type variable is the bottom type then update our variable to the
    // type we are unifying to. Remember to take our quantifications out of
    // scope as well!
    if (variableType.description.kind === 'Bottom') {
      variable.update(typePrefix, {kind: 'rigid', type});
      return undefined;
    }

    // Unify the variable’s monomorphic bound with our type.
    const error = unifyType(
      diagnostics,
      level,
      isVariableActual ? variablePrefix : typePrefix,
      isVariableActual ? typePrefix : variablePrefix,
      isVariableActual ? (variableType as MonomorphicType) : type,
      isVariableActual ? type : (variableType as MonomorphicType)
    );

    // If there was no error then update our variable to the monomorphic type
    // we unified with since our variable and the type are equivalent.
    if (error === undefined) {
      variable.update(typePrefix, {kind: 'rigid', type});
    }

    return error;
  }
}

/**
 * Unifies two type variables.
 */
function unifyVariable<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  level: number,
  actualVariable: TypeVariable,
  expectedVariable: TypeVariable
): Reported<UnifyError<Diagnostic>> | undefined {
  // Add all of our actual quantifications into scope.
  let actualPrefix = actualVariable.getPrefix();
  let actualType = actualVariable.getBound().type;
  while (actualType.description.kind === 'Quantified') {
    actualPrefix = actualPrefix.set(
      actualType.description.binding,
      new TypeVariable(
        new DerivableValue(level),
        actualPrefix,
        actualType.description.bound
      )
    );
    actualType = actualType.description.body;
  }

  // Add all of our expected quantifications into scope.
  let expectedPrefix = expectedVariable.getPrefix();
  let expectedType = expectedVariable.getBound().type;
  while (expectedType.description.kind === 'Quantified') {
    expectedPrefix = expectedPrefix.set(
      expectedType.description.binding,
      new TypeVariable(
        new DerivableValue(level),
        expectedPrefix,
        expectedType.description.bound
      )
    );
    expectedType = expectedType.description.body;
  }

  // Get the bound kind for the bound we will create between our two type
  // variables. The bound is rigid unless both bounds are flexible.
  const boundKind: 'flexible' | 'rigid' =
    actualVariable.getBound().kind === 'flexible' &&
    expectedVariable.getBound().kind === 'flexible'
      ? 'flexible'
      : 'rigid';

  // If actual is the bottom type then unify to expected. Make sure to take all
  // the type variables out of scope.
  if (actualType.description.kind === 'Bottom') {
    const bound = {kind: boundKind, type: expectedType};
    actualVariable.update(expectedPrefix, bound);
    expectedVariable.update(expectedPrefix, bound);
    return undefined;
  }

  // If expected is the bottom type then unify to actual. Make sure to take all
  // the type variables out of scope.
  if (expectedType.description.kind === 'Bottom') {
    const bound = {kind: boundKind, type: actualType};
    actualVariable.update(actualPrefix, bound);
    expectedVariable.update(actualPrefix, bound);
    return undefined;
  }

  // Unify the actual and expected types.
  const error = unifyType(
    diagnostics,
    level,
    actualPrefix,
    expectedPrefix,
    actualType as MonomorphicType,
    expectedType as MonomorphicType
  );

  // If unification was successful then unify both actual and expected to the
  // actual type. Since unification shows that the actual and expected types
  // are equivalent.
  if (error === undefined) {
    const bound = {kind: boundKind, type: actualType};
    actualVariable.update(actualPrefix, bound);
    expectedVariable.update(actualPrefix, bound);
  }

  return error;
}

export class TypeVariable {
  private readonly level: DerivableValue<number>;
  private prefix: Immutable.Map<string, TypeVariable>;
  private bound: Bound;

  constructor(
    level: DerivableValue<number>,
    prefix: Immutable.Map<string, TypeVariable>,
    bound: Bound
  ) {
    this.level = level;
    this.prefix = prefix;
    this.bound = bound;
  }

  getPrefix(): Immutable.Map<string, TypeVariable> {
    return this.prefix;
  }

  getBound(): Bound {
    return this.bound;
  }

  equals(other: TypeVariable) {
    return this === other;
  }

  update(prefix: Immutable.Map<string, TypeVariable>, bound: Bound) {
    this.prefix = prefix;
    this.bound = bound;
    this.level.set(Math.max(this.level.get(), bound.type.level.get()));
  }
}

export type UnifyError<T> =
  | T
  | {
      readonly kind: 'IncompatibleTypes';
      readonly actual: MonomorphicType;
      readonly expected: MonomorphicType;
    };
