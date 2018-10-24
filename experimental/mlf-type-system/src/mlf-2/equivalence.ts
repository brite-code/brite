import {Prefix} from './prefix';
import {Monotype, Polytype} from './type';

/**
 * Returns true if the provided types are equivalent under the provided prefix.
 * We abbreviate this relation in writing with a triple equals sign (≡ and the
 * inverse ≢).
 *
 * The equivalence relation captures the fact that structural equality of
 * artifacts like quantified types does not always matter. For instance, while
 * not structurally equal the following holds: ∀(a, b).a → b ≡ ∀(b, a).a → b.
 */
export function equivalent(
  prefix: Prefix,
  type1: Polytype,
  type2: Polytype
): boolean {
  return equivalentPolytype(prefix, Prefix.empty, Prefix.empty, type1, type2);

  /**
   * Tests the equivalence of two polytypes.
   *
   * Takes three prefixes. A shared prefix, `prefix0`, and a local prefix for
   * each type being tested. We separate the shared prefix from the local
   * prefixes so that we may short-circuit the testing of two type variables
   * defined in the shared prefix.
   */
  function equivalentPolytype(
    prefix0: Prefix,
    prefix1: Prefix,
    prefix2: Prefix,
    type1: Polytype,
    type2: Polytype
  ): boolean {
    // Unwrap quantifications for our first type.
    while (type1.kind === 'Quantify') {
      prefix1 = prefix1.add(type1.name, type1.bound);
      type1 = type1.body;
    }

    // Unwrap quantifications for our second type.
    while (type2.kind === 'Quantify') {
      prefix2 = prefix2.add(type2.name, type2.bound);
      type2 = type2.body;
    }

    // Variables directly under a polytype are substituted. Note that this rule
    // is intentionally different when testing monotype equivalence.
    if (type1.kind === 'Variable') {
      const {prefix: boundPrefix1, bound: bound1} =
        prefix1.find(type1.name) || prefix0.findOrPanic(type1.name);
      return equivalentPolytype(
        prefix0,
        boundPrefix1,
        prefix2,
        bound1.type,
        type2
      );
    }

    // Variables directly under a polytype are substituted. Note that this rule
    // is intentionally different when testing monotype equivalence.
    if (type2.kind === 'Variable') {
      const {prefix: boundPrefix2, bound: bound2} =
        prefix2.find(type2.name) || prefix0.findOrPanic(type2.name);
      return equivalentPolytype(
        prefix0,
        prefix1,
        boundPrefix2,
        type1,
        bound2.type
      );
    }

    // Bottom is only equivalent to itself.
    if (type1.kind === 'Bottom') return type2.kind === 'Bottom';
    if (type2.kind === 'Bottom') return false;

    // Create a new cache every time we call `equivalentPolytype()` because
    // the prefixes changed.
    const cache = new Map();

    return equivalentMonotype(cache, prefix0, prefix1, prefix2, type1, type2);
  }

  /**
   * Tests the equivalence of two monotypes.
   */
  function equivalentMonotype(
    cache: Map<string, boolean>,
    prefix0: Prefix,
    prefix1: Prefix,
    prefix2: Prefix,
    type1: Monotype,
    type2: Monotype
  ): boolean {
    // Type variables which have a monotype bound may be substituted directly.
    if (type1.kind === 'Variable') {
      const {prefix: boundPrefix1, bound: bound1} =
        prefix1.find(type1.name) || prefix0.findOrPanic(type1.name);
      if (isEquivalentToSomeMonotype(bound1.type)) {
        return equivalentPolytype(
          prefix0,
          boundPrefix1,
          prefix2,
          bound1.type,
          type2
        );
      }
    }

    // Type variables which have a monotype bound may be substituted directly.
    if (type2.kind === 'Variable') {
      const {prefix: boundPrefix2, bound: bound2} =
        prefix2.find(type2.name) || prefix0.findOrPanic(type2.name);
      if (isEquivalentToSomeMonotype(bound2.type)) {
        return equivalentPolytype(
          prefix0,
          prefix1,
          boundPrefix2,
          type1,
          bound2.type
        );
      }
    }

    // Otherwise, type variables are only equivalent if they have the same name
    // and equivalent bounds.
    if (
      type1.kind === 'Variable' &&
      type2.kind === 'Variable' &&
      type1.name === type2.name
    ) {
      // Look for the type variable in our local prefix.
      const name = type1.name;
      const result1 = prefix1.find(name);
      const result2 = prefix2.find(name);
      // If we didn’t find a type variable with this name in either local prefix
      // then it must be in the shared prefix. If this variable is shared then
      // we know that the variable is equivalent with itself.
      if (result1 === undefined && result2 === undefined) {
        prefix0.findOrPanic(name);
        return true;
      }
      // If we’ve already tested the equivalence of these two type variables
      // then return the cached result.
      let equivalent = cache.get(name);
      if (equivalent !== undefined) return equivalent;
      // Otherwise check to see if the bounds on the two type variables
      // are equivalent.
      const {prefix: boundPrefix1, bound: bound1} =
        result1 || prefix0.findOrPanic(name);
      const {prefix: boundPrefix2, bound: bound2} =
        result2 || prefix0.findOrPanic(name);
      equivalent =
        bound1.kind === bound2.kind &&
        equivalentPolytype(
          prefix0,
          boundPrefix1,
          boundPrefix2,
          bound1.type,
          bound2.type
        );
      // Cache the result in case this type variable is used again.
      cache.set(name, equivalent);
      return equivalent;
    }

    // All other variables are not equivalent to anything else.
    if (type1.kind === 'Variable' || type2.kind === 'Variable') return false;

    // Unit is only equivalent to unit.
    if (type1.kind === 'Unit') return type2.kind === 'Unit';
    if (type2.kind === 'Unit') return false;

    // Boolean is only equivalent to boolean.
    if (type1.kind === 'Boolean') return type2.kind === 'Boolean';
    if (type2.kind === 'Boolean') return false;

    // Number is only equivalent to number.
    if (type1.kind === 'Number') return type2.kind === 'Number';
    if (type2.kind === 'Number') return false;

    // String is only equivalent to string.
    if (type1.kind === 'String') return type2.kind === 'String';
    if (type2.kind === 'String') return false;

    // Functions are only equivalent to functions with equivalent parameter and
    // body types.
    if (type1.kind === 'Function') {
      if (type2.kind !== 'Function') return false;
      const param = equivalentMonotype(
        cache,
        prefix0,
        prefix1,
        prefix2,
        type1.param,
        type2.param
      );
      const body = equivalentMonotype(
        cache,
        prefix0,
        prefix1,
        prefix2,
        type1.body,
        type2.body
      );
      return param && body;
    } else if (type2.kind === 'Function') {
      return false;
    }

    const never1: never = type1;
    const never2: never = type2;
    return never1 && never2;
  }

  /**
   * Is there some monotype that this type is equivalent to? True if the type is
   * a monotype or a quantified type with all unused type bounds. Since a
   * quantified type with unused type bounds may be considered a monotype after
   * applying the [Eq-Free][1] rule.
   *
   * In the worst case this function traverses the entire type, but more
   * realistically we stop traversing the moment we see a quantified type
   * variable used.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
  function isEquivalentToSomeMonotype(type: Polytype): boolean {
    if (Polytype.isMonotype(type)) return true;

    const locals = new Set();
    while (type.kind === 'Quantify') {
      locals.add(type.name);
      type = type.body;
    }

    if (type.kind === 'Bottom') return false;

    const stack: Array<Monotype> = [type];
    while (stack.length > 0) {
      const type = stack.pop()!; // tslint:disable-line no-non-null-assertion
      switch (type.kind) {
        case 'Unit':
        case 'Boolean':
        case 'Number':
        case 'String':
          break;
        case 'Function':
          stack.push(type.param);
          stack.push(type.body);
          break;
        case 'Variable':
          if (locals.has(type.name)) return false;
          break;
        default:
          const never: never = type;
          stack.push(never);
          break;
      }
    }

    return true;
  }
}
