import {Diagnostics, Reported} from './diagnostics';
import {State} from './state';
import {Monotype, Polytype, Type} from './type';

export type UnifyError<T> =
  | T
  | {
      readonly kind: 'IncompatibleTypes';
      readonly actual: Monotype;
      readonly expected: Monotype;
    };

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
  state: State,
  actual: Monotype,
  expected: Monotype
): Reported<UnifyError<Diagnostic>> | undefined {
  return unifyType(diagnostics, state, actual, expected);
}

/**
 * Unifies two monomorphic types.
 */
function unifyType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: State,
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
  state: State,
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
    state.updateType(variable, {kind: 'rigid', type});
    return undefined;
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
      state.updateType(variable, {kind: 'rigid', type});
    }

    return error;
  }
}

/**
 * Unifies two type variables.
 */
function unifyVariable<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: State,
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

  // Get the bound kind for the bound we will create between our two type
  // variables. The bound is rigid unless both bounds are flexible.
  const boundKind: 'flexible' | 'rigid' =
    actualBound.kind === 'flexible' && expectedBound.kind === 'flexible'
      ? 'flexible'
      : 'rigid';

  // If actual is the bottom type then unify to expected.
  if (actualBound.type === undefined) {
    const bound = {kind: boundKind, type: expectedBound.type};
    state.updateType(actualVariable, bound);
    return undefined;
  }

  // If expected is the bottom type then unify to actual.
  if (expectedBound.type === undefined) {
    const bound = {kind: boundKind, type: actualBound.type};
    state.updateType(expectedVariable, bound);
    return undefined;
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
    const bound = {kind: boundKind, type: actualType};
    state.updateType(actualVariable, bound);
    state.updateType(expectedVariable, bound);
  }

  return error;
}

/**
 * Removes all the quantifications on the provided type and replaces them with
 * fresh type variables in state.
 *
 * This function will never return a quantified polytype.
 */
function instantiate(state: State, type: Polytype): Monotype {
  const substitutions = new Map<string, Monotype>();

  // If we have some substitutions for free type variables then this function
  // will apply them to the provided type.
  function substitute(type: Polytype): Polytype {
    if (substitutions.size < 1) return type;
    return Type.transformFreeVariables(type, name => substitutions.get(name));
  }

  // Decompose the quantified type. Create fresh types for all the bounds and
  // substitute the bound types if applicable.
  while (type.description.kind === 'Quantify') {
    const {name, bound} = type.description;
    const newType = state.newTypeWithBound({
      kind: bound.kind,
      type: bound.type !== undefined ? substitute(bound.type) : undefined,
    });
    substitutions.set(name, newType);
    type = type.description.body;
  }

  // Substitute the body type before returning it.
  const newType = substitute(type);

  // We decomposed all the quantifications. We should never see another one.
  if (newType.description.kind === 'Quantify') throw new Error('Unreachable');

  return newType as Monotype;
}
