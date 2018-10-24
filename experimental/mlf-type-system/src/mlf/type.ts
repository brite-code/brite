export type Type = Polytype;

export type Monotype = {
  readonly description: MonotypeDescription;
};

export type Polytype = {
  readonly description: PolytypeDescription;
};

export type MonotypeDescription =
  | {readonly kind: 'Boolean'}
  | {readonly kind: 'Number'}
  | {readonly kind: 'String'}
  | {
      readonly kind: 'Function';
      readonly param: Monotype;
      readonly body: Monotype;
    }
  | {
      readonly kind: 'Variable';
      readonly name: string;
    };

export type PolytypeDescription =
  | MonotypeDescription
  | {
      readonly kind: 'Bottom';
    }
  | {
      readonly kind: 'Quantify';
      readonly name: string;
      readonly bound: Bound;
      readonly body: Polytype;
    };

export type Bound = {
  readonly kind: 'flexible' | 'rigid';
  readonly type: Type;
};

export namespace Type {
  /**
   * Returns true if the provided type is monomorphic.
   */
  export function isMonotype(type: Type): type is Monotype {
    return (
      type.description.kind !== 'Quantify' && type.description.kind !== 'Bottom'
    );
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
        let param = toDisplayString(type.description.param);
        const body = toDisplayString(type.description.body);
        if (type.description.param.description.kind === 'Function') {
          param = `(${param})`;
        }
        return `${param} → ${body}`;
      }

      case 'Bottom':
        return '⊥';

      case 'Quantify': {
        if (
          type.description.body.description.kind !== 'Quantify' &&
          type.description.bound.kind === 'flexible' &&
          type.description.bound.type.description.kind === 'Bottom'
        ) {
          const name = type.description.name;
          const body = Type.toDisplayString(type.description.body);
          return `∀${name}.${body}`;
        }
        const bounds = [];
        while (type.description.kind === 'Quantify') {
          if (
            type.description.bound.kind === 'flexible' &&
            type.description.bound.type.description.kind === 'Bottom'
          ) {
            bounds.push(type.description.name);
          } else {
            const name = type.description.name;
            const kind = type.description.bound.kind === 'flexible' ? '≥' : '=';
            const bound = Type.toDisplayString(type.description.bound.type);
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

  export function variable(name: string): Monotype {
    return {
      description: {kind: 'Variable', name},
    };
  }

  export const boolean: Monotype = {
    description: {kind: 'Boolean'},
  };

  export const number: Monotype = {
    description: {kind: 'Number'},
  };

  export const string: Monotype = {
    description: {kind: 'String'},
  };

  export function function_(param: Monotype, body: Monotype): Monotype {
    return {
      description: {kind: 'Function', param, body},
    };
  }

  export function quantify(
    name: string,
    bound: Bound,
    body: Polytype
  ): Polytype {
    return {
      description: {kind: 'Quantify', name, bound, body},
    };
  }

  export const bottom: Polytype = {
    description: {kind: 'Bottom'},
  };

  export function rigidBound(type: Polytype): Bound {
    return {kind: 'rigid', type};
  }

  export function flexibleBound(type: Polytype): Bound {
    return {kind: 'flexible', type};
  }
}
