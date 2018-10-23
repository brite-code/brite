import {Prefix} from './prefix';
import {Monotype, Polytype} from './type';

export function equivalent(
  prefix: Prefix,
  type1: Polytype,
  type2: Polytype
): boolean {
  return equivalentPolytype(prefix, prefix, type1, type2);

  function equivalentPolytype(
    prefix1: Prefix,
    prefix2: Prefix,
    type1: Polytype,
    type2: Polytype
  ): boolean {
    const unused1 = new Set();
    const unused2 = new Set();

    while (type1.kind === 'Quantify') {
      unused1.add(type1.name);
      prefix1 = prefix1.add(type1.name, type1.bound);
      type1 = type1.body;
    }

    while (type2.kind === 'Quantify') {
      unused2.add(type2.name);
      prefix2 = prefix2.add(type2.name, type2.bound);
      type2 = type2.body;
    }

    if (type1.kind === 'Bottom') return type2.kind === 'Bottom';
    if (type2.kind === 'Bottom') return false;

    return equivalentMonotype(prefix1, prefix2, type1, type2);
  }

  function equivalentMonotype(
    prefix1: Prefix,
    prefix2: Prefix,
    type1: Monotype,
    type2: Monotype
  ): boolean {
    if (
      type1.kind === 'Variable' &&
      type2.kind === 'Variable' &&
      type1.name === type2.name
    ) {
      const name = type1.name;
      const {prefix: boundPrefix1, bound: bound1} = prefix1.find(name);
      const {prefix: boundPrefix2, bound: bound2} = prefix2.find(name);
      if (
        isEquivalentToSomeMonotype(bound1.type) &&
        isEquivalentToSomeMonotype(bound2.type)
      ) {
        return equivalentPolytype(
          boundPrefix1,
          boundPrefix2,
          bound1.type,
          bound2.type
        );
      } else {
        return (
          bound1.kind === bound2.kind &&
          equivalentPolytype(
            boundPrefix1,
            boundPrefix2,
            bound1.type,
            bound2.type
          )
        );
      }
    }

    if (type1.kind === 'Variable') {
      const {prefix: boundPrefix1, bound: bound1} = prefix1.find(type1.name);
      if (isEquivalentToSomeMonotype(bound1.type)) {
        return equivalentPolytype(boundPrefix1, prefix2, bound1.type, type2);
      } else {
        return false;
      }
    }

    if (type2.kind === 'Variable') {
      const {prefix: boundPrefix2, bound: bound2} = prefix2.find(type2.name);
      if (isEquivalentToSomeMonotype(bound2.type)) {
        return equivalentPolytype(prefix1, boundPrefix2, type1, bound2.type);
      } else {
        return false;
      }
    }

    if (type1.kind === 'Unit') return type2.kind === 'Unit';
    if (type2.kind === 'Unit') return false;

    if (type1.kind === 'Boolean') return type2.kind === 'Boolean';
    if (type2.kind === 'Boolean') return false;

    if (type1.kind === 'Number') return type2.kind === 'Number';
    if (type2.kind === 'Number') return false;

    if (type1.kind === 'String') return type2.kind === 'String';
    if (type2.kind === 'String') return false;

    if (type1.kind === 'Function') {
      if (type2.kind !== 'Function') return false;
      return (
        equivalentMonotype(prefix1, prefix2, type1.param, type2.param) &&
        equivalentMonotype(prefix1, prefix2, type1.body, type2.body)
      );
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
          break;
        case 'Boolean':
          break;
        case 'Number':
          break;
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

export function abstracted(
  prefix: Prefix,
  type1: Polytype,
  type2: Polytype
): boolean {}

export function instance(
  prefix: Prefix,
  type1: Polytype,
  type2: Polytype
): boolean {}
