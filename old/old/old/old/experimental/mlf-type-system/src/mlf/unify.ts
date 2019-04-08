import {BindingMap} from '../utils/bindings';
import {Err, Ok, Result} from '../utils/result';

import {Diagnostics, Reported} from './diagnostics';
import {generalize} from './generalize';
import {Prefix, PrefixError} from './prefix';
import {Bound, Monotype, Polytype, Type} from './type';

export type UnifyError =
  | PrefixError
  | {
      readonly kind: 'IncompatibleTypes';
      readonly actual: Monotype;
      readonly expected: Monotype;
    };

/**
 * Implements the unification algorithm from the [MLF thesis][1].
 *
 * In theory, the paper defines unification as:
 *
 * > Definition 4.1.1 (Unification): A prefix `Q'` unifies `t1` and `t2` under
 * > `Q` if and only if `Q ⊑ Q'` and `(Q') t1 ≡ t2` hold.
 *
 * So every mutation to the prefix must maintain the invariant `Q ⊑ Q'`. In our
 * implementation an unbounded `a` type variable represent the theory for the
 * prefix entry `a ≥ ⊥`. According to the instance relation every polymorphic
 * type is an instance of bottom (`⊥`).
 *
 * As an optimization we also mutate type variables in prefix to an equivalent
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
  diagnostics: Diagnostics<UnifyError | Diagnostic>,
  prefix: Prefix,
  actual: Monotype,
  expected: Monotype
): Reported<UnifyError | Diagnostic> | undefined {
  return unifyMonotype(diagnostics, prefix, actual, expected);
}

/**
 * Unifies two monotypes.
 */
function unifyMonotype<Diagnostic>(
  diagnostics: Diagnostics<UnifyError | Diagnostic>,
  prefix: Prefix,
  actual: Monotype,
  expected: Monotype
): Reported<UnifyError | Diagnostic> | undefined {
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
  let actualVariable:
    | {readonly name: string; readonly bound: Bound}
    | undefined;
  if (actual.description.kind === 'Variable') {
    const entry = prefix.lookup(actual.description.name);
    // If no type variable exists with this name then report an error.
    if (entry.kind === 'Err') {
      return diagnostics.report(entry.value);
    }
    // Return the bound from this block and check for shortcuts.
    actualVariable = {name: actual.description.name, bound: entry.value.bound};
    if (actualVariable.bound.type.description.kind === 'Variable') {
      const actualType = actualVariable.bound.type as Monotype;
      return unifyMonotype(diagnostics, prefix, actualType, expected);
    }
  }

  // If the bound of expected is a monomorphic then let us recursively call
  // unify with that type.
  let expectedVariable:
    | {readonly name: string; readonly bound: Bound}
    | undefined;
  if (expected.description.kind === 'Variable') {
    const entry = prefix.lookup(expected.description.name);
    // If no type variable exists with this name then report an error.
    if (entry.kind === 'Err') {
      return diagnostics.report(entry.value);
    }
    // Return the bound from this block and check for shortcuts.
    expectedVariable = {
      name: expected.description.name,
      bound: entry.value.bound,
    };
    if (expectedVariable.bound.type.description.kind === 'Variable') {
      const expectedType = expectedVariable.bound.type as Monotype;
      return unifyMonotype(diagnostics, prefix, actual, expectedType);
    }
  }

  // If both actual and expected are variables then unify their bounds. Also
  // unify the two types together at the very end.
  if (actualVariable !== undefined && expectedVariable !== undefined) {
    const result = unifyPolytype(
      diagnostics,
      prefix,
      actualVariable.bound.type,
      expectedVariable.bound.type
    );
    // If there was an error then don’t update our prefix. Only return the error
    // and carry on.
    if (result.kind === 'Err') {
      return result.value;
    } else {
      // Update actual to the new type and update expected to point at actual.
      // If we get an error in the process then return it.
      const type = result.value;
      const error1 = prefix.update(actualVariable.name, Type.rigidBound(type));
      if (error1 !== undefined) return diagnostics.report(error1);
      const error2 = prefix.update(
        expectedVariable.name,
        Type.rigidBound(Type.variable(actualVariable.name))
      );
      if (error2 !== undefined) return diagnostics.report(error2);
      return undefined;
    }
  }

  // If only actual was a variable then unify its type bound. If unification
  // succeeds then update the actual variable in the prefix.
  if (actualVariable !== undefined) {
    const actualType = actualVariable.bound.type;
    const result = unifyPolytype(diagnostics, prefix, actualType, expected);
    if (result.kind === 'Err') {
      return result.value;
    } else {
      const type = result.value;
      const error = prefix.update(actualVariable.name, Type.rigidBound(type));
      if (error !== undefined) return diagnostics.report(error);
      return undefined;
    }
  }

  // If only expected was a variable then unify its type bound. If unification
  // succeeds then update the expected variable in the prefix.
  if (expectedVariable !== undefined) {
    const expectedType = expectedVariable.bound.type;
    const result = unifyPolytype(diagnostics, prefix, actual, expectedType);
    if (result.kind === 'Err') {
      return result.value;
    } else {
      const type = result.value;
      const error = prefix.update(expectedVariable.name, Type.rigidBound(type));
      if (error !== undefined) return diagnostics.report(error);
      return undefined;
    }
  }

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
      prefix,
      expected.description.parameter,
      actual.description.parameter
    );
    const diagnostic2 = unifyMonotype(
      diagnostics,
      prefix,
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
  diagnostics: Diagnostics<UnifyError | Diagnostic>,
  prefix: Prefix,
  actual: Polytype,
  expected: Polytype
): Result<Polytype, Reported<UnifyError | Diagnostic>> {
  // If either is bottom then return the other.
  if (actual.description.kind === 'Bottom') return Ok(expected);
  if (expected.description.kind === 'Bottom') return Ok(actual);

  // Increment the level. We are about to create some type variables which will
  // need to be generalized.
  prefix.incrementLevel();

  // Merge the prefixes of actual and expected into our global prefix. We also
  // rename type variables when necessary to avoid collisions.
  const actualMonotype = mergePrefix(prefix, actual);
  const expectedMonotype = mergePrefix(prefix, expected);

  // If our type is of the form `∀a.⊥` then we won’t have handled the bottom
  // type above. Instead the bottom type will have been hiding out under
  // quantifications. We don’t have a sensible way to handle bottom types in
  // this case, so panic. We assume that the types being unified are in normal
  // form anyway so panicking is fine.
  if (actualMonotype === undefined || expectedMonotype === undefined) {
    throw new Error('Unsupported quantified bottom type.');
  }

  // Unify the underlying monotypes.
  const error = unifyMonotype(
    diagnostics,
    prefix,
    actualMonotype,
    expectedMonotype
  );

  if (error === undefined) {
    // Generalize the actual type in the current level. Because unification did
    // not produce an error we know that actual and expected are equivalent. So
    // we arbitrarily pick actual but it doesn’t matter which one we pick.
    const type = generalize(prefix, actualMonotype);
    // Decrement the level. We are done creating type variables.
    prefix.decrementLevel();
    // Return the type we generalized.
    return Ok(type);
  } else {
    // Decrement the level. We are done creating type variables. We don’t end up
    // using any of the type variables at this level though because we erred!
    prefix.decrementLevel();
    return Err(error);
  }
}

/**
 * Merges the prefix of a polytype into the global prefix. Returns the unwrapped
 * monotype or nothing if the provided polytype was the bottom type.
 */
function mergePrefix(prefix: Prefix, type: Polytype): Monotype | undefined {
  const substitutions = new BindingMap<string, Monotype | undefined>();

  // For all quantification bounds...
  while (type.description.kind === 'Quantify') {
    const {name, bound} = type.description;
    let newBound = bound;
    // If we have some substitutions apply them in our bound’s type.
    if (!substitutions.isEmpty()) {
      const newType = substitutePolytype(bound.type);
      newBound = {kind: newBound.kind, type: newType};
    }
    // Add our bound to the prefix. It is possible that a type variable with
    // this name already exists in the prefix.
    const newName = prefix.add(name, newBound);
    // If the type variable added to the prefix has a different name then let us
    // add a substitution for our type variable.
    //
    // Consider `∀a.∀(a = a).∀(a = a). ...` If we later see the same name then
    // our prefix will generate a new name and we will override the previous
    // substitution in our bindings map.
    //
    // Also note since we shallowly look through quantification bounds we don’t
    // need to pop substitutions.
    if (newName !== name) {
      substitutions.push(name, Type.variable(newName));
    }
    type = type.description.body;
  }

  // If we have the bottom type then return undefined.
  if (type.description.kind === 'Bottom') return undefined;

  // Substitute the monotype and return it.
  return substituteMonotype(type as Monotype);

  /**
   * Apply our substitutions to the provided polytype. Locally shadowed
   * variables will not be substituted. Will return the type if substitutions
   * are not needed.
   */
  function substitutePolytype(type: Polytype): Polytype {
    if (!needsSubstitution(type)) return type;

    switch (type.description.kind) {
      case 'Quantify': {
        const name = type.description.name;
        const bound = {
          kind: type.description.bound.kind,
          type: substitutePolytype(type.description.bound.type),
        };
        substitutions.push(name, undefined);
        const body = substitutePolytype(type.description.body);
        substitutions.pop();
        return Type.quantify(name, bound, body);
      }

      case 'Bottom':
        return type;

      default:
        return substituteMonotype(type as Monotype);
    }
  }

  /**
   * Apply our substitutions to the provided monotype. Will return the type if
   * substitutions are not needed.
   */
  function substituteMonotype(type: Monotype): Monotype {
    if (!needsSubstitution(type)) return type;

    switch (type.description.kind) {
      case 'Variable': {
        const newType = substitutions.get(type.description.name);
        return newType || type;
      }

      case 'Function': {
        return Type.function_(
          substituteMonotype(type.description.parameter),
          substituteMonotype(type.description.body)
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

  /**
   * Given the provided substitutions map does our type need a substitution?
   */
  function needsSubstitution(type: Polytype): boolean {
    const freeVariables = Type.getFreeVariables(type);
    // Iterate over the smaller set.
    if (freeVariables.size <= substitutions.distinctKeysCount()) {
      for (const variable of freeVariables) {
        const newType = substitutions.get(variable);
        if (newType !== undefined) return true;
      }
    } else {
      for (const [variable, newType] of substitutions.distinctEntries()) {
        if (freeVariables.has(variable) && newType !== undefined) {
          return true;
        }
      }
    }
    return false;
  }
}
