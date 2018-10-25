import * as Immutable from 'immutable';

import {BindingMap} from './bindings';
import {Diagnostics, Reported} from './diagnostics';
import {State} from './state';
import {Monotype, Polytype, Type} from './type';

export type UnifyError<T> =
  | T
  | {
      readonly kind: 'IncompatibleTypes';
      readonly actual: Monotype;
      readonly expected: Monotype;
    }
  | {
      readonly kind: 'InfiniteType';
      readonly name: string;
      readonly type: Polytype;
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
    return updateType(diagnostics, state, variable, type);
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
      return updateType(diagnostics, state, variable, type);
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

  // If actual is the bottom type then unify to expected.
  if (actualBound.type === undefined) {
    const expectedType = Type.variable(expectedVariable);
    return updateType(diagnostics, state, actualVariable, expectedType);
  }

  // If expected is the bottom type then unify to actual.
  if (expectedBound.type === undefined) {
    const actualType = Type.variable(actualVariable);
    return updateType(diagnostics, state, expectedVariable, actualType);
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
    return updateType(diagnostics, state, expectedVariable, actualType);
  } else {
    return error;
  }
}

/**
 * Updates a type in our state while also handling failures in the occurs check.
 */
function updateType<Diagnostic>(
  diagnostics: Diagnostics<UnifyError<Diagnostic>>,
  state: State,
  name: string,
  type: Polytype
): Reported<UnifyError<Diagnostic>> | undefined {
  const ok = state.updateTypeWithOccursCheck(name, {kind: 'rigid', type});
  if (ok === true) {
    return undefined;
  } else {
    return diagnostics.report({kind: 'InfiniteType', name, type});
  }
}

/**
 * Removes all the quantifications on the provided type and replaces them with
 * fresh type variables in state.
 *
 * This function will never return a quantified polytype.
 */
function instantiate(state: State, type: Polytype): Monotype {
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
