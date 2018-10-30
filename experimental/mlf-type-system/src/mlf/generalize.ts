import {Prefix} from './prefix';
import {Bound, Polytype, Type} from './type';

/**
 * Turns type variables at a level larger then the current level into generic
 * quantified type bounds.
 */
export function generalize(prefix: Prefix, type: Polytype): Polytype {
  const quantify = new Map<string, Bound>();
  generalize(quantify, prefix, type);
  // Quantify our type for every variable in the `quantify` set. The order of
  // the `quantify` set does matter! Since some type variables may have a
  // dependency on others.
  const quantifyReverse = Array<[string, Bound]>(quantify.size);
  let i = 0;
  for (const entry of quantify) {
    quantifyReverse[quantify.size - i++ - 1] = entry;
  }
  for (const [name, bound] of quantifyReverse) {
    type = Type.quantify(name, bound, type);
  }
  return type;

  // Iterate over every free type variable and determine if we need to
  // quantify it.
  function generalize(
    quantify: Map<string, Bound>,
    prefix: Prefix,
    type: Polytype
  ) {
    for (const name of Type.getFreeVariables(type)) {
      const result = prefix.lookup(name);
      if (result.kind === 'Err') throw new Error('Unexpected error.');
      const {level, bound} = result.value;
      // If we have a type variable with a level greater than our current
      // level then we need to quantify that type variable.
      if (level >= prefix.getLevel()) {
        // If `quantify` already contains this type variable then we don’t
        // need to add it again.
        if (!quantify.has(name)) {
          // Generalize the dead type variables in our bound as well.
          generalize(quantify, prefix, bound.type);
          // It is important that we add the name to `quantify` _after_ we
          // generalize the bound type. The order of variables in `quantify`
          // does matter.
          quantify.set(name, bound);
        }
      }
    }
  }
}
