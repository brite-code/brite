export type Monotype =
  | {readonly kind: 'Unit'}
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

export type Polytype =
  | Monotype
  | {readonly kind: 'Bottom'}
  | {
      readonly kind: 'Quantify';
      readonly name: string;
      readonly bound: Bound;
      readonly body: Polytype;
    };

export type Bound = {
  readonly kind: 'flexible' | 'rigid';
  readonly type: Polytype;
};

export namespace Monotype {
  export function toDisplayString(type: Monotype): string {
    switch (type.kind) {
      case 'Unit':
        return 'unit';
      case 'Boolean':
        return 'boolean';
      case 'Number':
        return 'number';
      case 'String':
        return 'string';
      case 'Variable':
        return type.name;

      case 'Function': {
        let param = toDisplayString(type.param);
        const body = toDisplayString(type.body);
        if (type.param.kind === 'Function') param = `(${param})`;
        return `${param} → ${body}`;
      }

      default:
        const never: never = type;
        return never;
    }
  }
}

export namespace Polytype {
  /**
   * Returns true if the provided polytype is a monotype.
   *
   * IMPORTANT: Some polytypes may still be semantically monotypes. For example
   * `∀x.boolean` is structurally a polytype, but according to the rules of our
   * equivalence relation it can be considered a monotype after applying the
   * [Eq-Free][1] rule.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
  export function isMonotype(type: Polytype): type is Monotype {
    return type.kind !== 'Bottom' && type.kind !== 'Quantify';
  }

  export function toDisplayString(type: Polytype): string {
    switch (type.kind) {
      case 'Bottom':
        return '⊥';

      case 'Quantify': {
        if (
          type.body.kind !== 'Quantify' &&
          type.bound.kind === 'flexible' &&
          type.bound.type.kind === 'Bottom'
        ) {
          return `∀${type.name}.${Polytype.toDisplayString(type.body)}`;
        }
        const bounds = [];
        while (type.kind === 'Quantify') {
          if (
            type.bound.kind === 'flexible' &&
            type.bound.type.kind === 'Bottom'
          ) {
            bounds.push(type.name);
          } else {
            const boundKind = type.bound.kind === 'flexible' ? '≥' : '=';
            const boundType = Polytype.toDisplayString(type.bound.type);
            bounds.push(`${type.name} ${boundKind} ${boundType}`);
          }
          type = type.body;
        }
        return `∀(${bounds.join(', ')}).${Polytype.toDisplayString(type)}`;
      }

      default:
        return Monotype.toDisplayString(type);
    }
  }
}

export namespace Type {
  export const unit: Monotype = {kind: 'Unit'};
  export const boolean: Monotype = {kind: 'Boolean'};
  export const number: Monotype = {kind: 'Number'};
  export const string: Monotype = {kind: 'String'};

  export function function_(param: Monotype, body: Monotype): Monotype {
    return {kind: 'Function', param, body};
  }

  export function variable(name: string): Monotype {
    return {kind: 'Variable', name};
  }

  export const bottom: Polytype = {kind: 'Bottom'};

  export function quantify(
    name: string,
    bound: Bound,
    body: Polytype
  ): Polytype {
    return {kind: 'Quantify', name, bound, body};
  }

  export function quantifyUnbounded(name: string, body: Polytype): Polytype {
    return quantify(name, flexibleBound(bottom), body);
  }

  export function flexibleBound(type: Polytype): Bound {
    return {kind: 'flexible', type};
  }

  export function rigidBound(type: Polytype): Bound {
    return {kind: 'rigid', type};
  }
}
