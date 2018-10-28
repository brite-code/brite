import * as Immutable from 'immutable';

/**
 * A type in our type system. Actually an alias for `Polytype`.
 */
export type Type = Polytype;

/**
 * Monotypes are types which do not contain quantifiers.
 */
export type Monotype = {
  _freeVariables: Immutable.Set<string> | undefined;
  readonly description: MonotypeDescription;
};

/**
 * Polytypes are types which do contain quantifiers. All monotypes are also
 * polytypes. In general when we say “type” we are referring to a polytype.
 */
export type Polytype = {
  _freeVariables: Immutable.Set<string> | undefined;
  readonly description: PolytypeDescription;
};

export type MonotypeDescription =
  | {readonly kind: 'Boolean'}
  | {readonly kind: 'Number'}
  | {readonly kind: 'String'}
  | {
      readonly kind: 'Function';
      readonly parameter: Monotype;
      readonly body: Monotype;
    }
  | {
      readonly kind: 'Variable';
      readonly name: string;
    };

export type PolytypeDescription =
  | MonotypeDescription
  | {
      readonly kind: 'Quantify';
      readonly name: string;
      readonly bound: Bound;
      readonly body: Polytype;
    };

/**
 * Bound of a polytype quantification. An undefined type bound should be treated
 * as the bottom type.
 */
export type Bound = {
  readonly kind: 'flexible' | 'rigid';
  readonly type: Type | undefined;
};

export namespace Type {
  /**
   * Returns true if the provided type is monomorphic.
   */
  export function isMonotype(type: Type): type is Monotype {
    return type.description.kind !== 'Quantify';
  }

  /**
   * Prints a type to a display string using the standard syntax for types in
   * academic literature. Particularly the [MLF][1] paper we implement.
   *
   * Brite programmers will not be familiar with this syntax. It is for
   * debugging purposes only.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
  export function toDisplayString(type: Type): string {
    switch (type.description.kind) {
      case 'Variable':
        return type.description.name;

      case 'Boolean':
        return 'boolean';
      case 'Number':
        return 'number';
      case 'String':
        return 'string';

      case 'Function': {
        let param = toDisplayString(type.description.parameter);
        const body = toDisplayString(type.description.body);
        if (type.description.parameter.description.kind === 'Function') {
          param = `(${param})`;
        }
        return `${param} → ${body}`;
      }

      case 'Quantify': {
        if (
          type.description.body.description.kind !== 'Quantify' &&
          type.description.bound.kind === 'flexible' &&
          type.description.bound.type === undefined
        ) {
          const name = type.description.name;
          const body = Type.toDisplayString(type.description.body);
          return `∀${name}.${body}`;
        }
        const bounds = [];
        while (type.description.kind === 'Quantify') {
          if (
            type.description.bound.kind === 'flexible' &&
            type.description.bound.type === undefined
          ) {
            bounds.push(type.description.name);
          } else {
            const name = type.description.name;
            const kind = type.description.bound.kind === 'flexible' ? '≥' : '=';
            const bound =
              type.description.bound.type !== undefined
                ? Type.toDisplayString(type.description.bound.type)
                : '⊥';
            bounds.push(`${name} ${kind} ${bound}`);
          }
          type = type.description.body;
        }
        const body = Type.toDisplayString(type);
        return `∀(${bounds.join(', ')}).${body}`;
      }

      default:
        const never: never = type.description;
        return never;
    }
  }

  /**
   * Gets all the free type variables in the provided type. This function caches
   * its results so that subsequent accesses are fast.
   */
  export function getFreeVariables(type: Polytype): Immutable.Set<string> {
    // If the result is already cached on the type then return it. Otherwise we
    // need to compute the free type variables.
    if (type._freeVariables !== undefined) return type._freeVariables;

    let freeVariables: Immutable.Set<string>;
    switch (type.description.kind) {
      case 'Quantify': {
        const body = getFreeVariables(type.description.body);
        if (body.has(type.description.name)) {
          // Only add the free variables from the quantified type bound if the
          // bound is actually used. Otherwise it doesn’t matter what’s free and
          // what’s not in the bound.
          const bound =
            type.description.bound.type !== undefined
              ? getFreeVariables(type.description.bound.type)
              : Immutable.Set<string>();
          freeVariables = body.delete(type.description.name).union(bound);
        } else {
          freeVariables = body;
        }
        break;
      }

      case 'Function': {
        const parameter = getFreeVariables(type.description.parameter);
        const body = getFreeVariables(type.description.body);
        freeVariables = parameter.union(body);
        break;
      }

      case 'Variable':
        freeVariables = Immutable.Set([type.description.name]);
        break;

      case 'Boolean':
      case 'Number':
      case 'String':
        freeVariables = Immutable.Set();
        break;

      default:
        const never: never = type.description;
        freeVariables = never;
        break;
    }

    // Cache the free variables we computed on our type so that if
    // `getFreeVariables()` is ever called again we will have them ready.
    //
    // TODO: Measure if caching the result of this function actually causes a
    // significant performance improvement.
    type._freeVariables = freeVariables;
    return freeVariables;
  }

  export function variable(name: string): Monotype {
    return {
      _freeVariables: Immutable.Set([name]),
      description: {kind: 'Variable', name},
    };
  }

  export const boolean: Monotype = {
    _freeVariables: Immutable.Set(),
    description: {kind: 'Boolean'},
  };

  export const number: Monotype = {
    _freeVariables: Immutable.Set(),
    description: {kind: 'Number'},
  };

  export const string: Monotype = {
    _freeVariables: Immutable.Set(),
    description: {kind: 'String'},
  };

  export function function_(parameter: Monotype, body: Monotype): Monotype {
    return {
      _freeVariables: undefined,
      description: {kind: 'Function', parameter, body},
    };
  }

  export function quantify(
    name: string,
    bound: Bound,
    body: Polytype
  ): Polytype {
    return {
      _freeVariables: undefined,
      description: {kind: 'Quantify', name, bound, body},
    };
  }

  export function quantifyUnbounded(name: string, body: Polytype): Polytype {
    return {
      _freeVariables: undefined,
      description: {kind: 'Quantify', name, bound: unbounded, body},
    };
  }

  export function rigidBound(type: Polytype): Bound {
    return {kind: 'rigid', type};
  }

  export function flexibleBound(type: Polytype): Bound {
    return {kind: 'flexible', type};
  }

  export const unbounded: Bound = {kind: 'flexible', type: undefined};

  export const bottom: Polytype = Type.quantifyUnbounded(
    'x',
    Type.variable('x')
  );
}
