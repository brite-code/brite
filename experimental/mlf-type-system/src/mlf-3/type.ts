export type Type = PolymorphicType;

export type ConstructedType<T = never> =
  | T
  | {readonly kind: 'Boolean'}
  | {readonly kind: 'Number'}
  | {readonly kind: 'String'}
  | {
      readonly kind: 'Function';
      readonly param: ConstructedType<T>;
      readonly body: ConstructedType<T>;
    }
  | {
      readonly kind: 'Constructed';
      readonly type: ConstructedType<never>;
    };

export type MonomorphicType = ConstructedType<{
  readonly kind: 'Variable';
  readonly name: string;
}>;

export type PolymorphicType =
  | MonomorphicType
  | {readonly kind: 'Bottom'}
  | {
      readonly kind: 'Quantify';
      readonly name: string;
      readonly bound: PolymorphicTypeBound;
      readonly body: PolymorphicType;
    };

export type PolymorphicTypeBound = {
  readonly kind: 'flexible' | 'rigid';
  readonly type: PolymorphicType;
};

export namespace MonomorphicType {
  /**
   * Converts a monotype to a string for debugging purposes. The format of the
   * string is chosen based on the [MLF paper][1] and so is quite academic.
   *
   * Brite programmers will not be familiar with this syntax. It is for
   * debugging purposes only.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
  export function toDisplayString(type: MonomorphicType): string {
    switch (type.kind) {
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

      case 'Constructed':
        return toDisplayString(type.type);

      default:
        const never: never = type;
        return never;
    }
  }
}

export namespace PolymorphicType {
  /**
   * Converts a polytype to a string for debugging purposes. The format of the
   * string is chosen based on the [MLF paper][1] and so is quite academic.
   *
   * Brite programmers will not be familiar with this syntax. It is for
   * debugging purposes only.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
  export function toDisplayString(type: PolymorphicType): string {
    switch (type.kind) {
      case 'Bottom':
        return '⊥';

      case 'Quantify': {
        if (
          type.body.kind !== 'Quantify' &&
          type.bound.kind === 'flexible' &&
          type.bound.type.kind === 'Bottom'
        ) {
          return `∀${type.name}.${PolymorphicType.toDisplayString(type.body)}`;
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
            const boundType = PolymorphicType.toDisplayString(type.bound.type);
            bounds.push(`${type.name} ${boundKind} ${boundType}`);
          }
          type = type.body;
        }
        const body = PolymorphicType.toDisplayString(type);
        return `∀(${bounds.join(', ')}).${body}`;
      }

      default:
        return MonomorphicType.toDisplayString(type);
    }
  }
}

export namespace Type {
  /**
   * Returns true if the provided polytype is a monotype.
   *
   * NOTE: Some polytypes may still be semantically monotypes. For example
   * `∀x.boolean` is structurally a polytype, but according to the rules of our
   * equivalence relation it can be considered a monotype after applying the
   * [Eq-Free][1] rule.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
  export function isMonomorphic(
    type: PolymorphicType
  ): type is MonomorphicType {
    return type.kind !== 'Bottom' && type.kind !== 'Quantify';
  }

  /**
   * Converts a type to a string for debugging purposes. The format of the
   * string is chosen based on the [MLF paper][1] and so is quite academic.
   *
   * Brite programmers will not be familiar with this syntax. It is for
   * debugging purposes only.
   *
   * [1]: http://pauillac.inria.fr/~remy/work/mlf/icfp.pdf
   */
  export function toDisplayString(type: PolymorphicType): string {
    return PolymorphicType.toDisplayString(type);
  }

  function isEntirelyConstructed(
    type: MonomorphicType
  ): type is ConstructedType {
    return (
      type.kind === 'Boolean' ||
      type.kind === 'Number' ||
      type.kind === 'String' ||
      type.kind === 'Constructed'
    );
  }

  export const boolean: MonomorphicType = {kind: 'Boolean'};
  export const number: MonomorphicType = {kind: 'Number'};
  export const string: MonomorphicType = {kind: 'String'};

  export function function_(
    param: MonomorphicType,
    body: MonomorphicType
  ): MonomorphicType {
    if (isEntirelyConstructed(param) && isEntirelyConstructed(body)) {
      const param2: ConstructedType =
        param.kind === 'Constructed' ? param.type : param;
      const body2: ConstructedType =
        body.kind === 'Constructed' ? body.type : body;
      return {
        kind: 'Constructed',
        type: {kind: 'Function', param: param2, body: body2},
      };
    } else {
      return {kind: 'Function', param, body};
    }
  }

  export function variable(name: string): MonomorphicType {
    return {kind: 'Variable', name};
  }

  export const bottom: PolymorphicType = {kind: 'Bottom'};

  export function quantify(
    name: string,
    bound: PolymorphicTypeBound,
    body: PolymorphicType
  ): PolymorphicType {
    return {kind: 'Quantify', name, bound, body};
  }

  export function quantifyUnbounded(
    name: string,
    body: PolymorphicType
  ): PolymorphicType {
    return quantify(name, flexibleBound(bottom), body);
  }

  export function flexibleBound(type: PolymorphicType): PolymorphicTypeBound {
    return {kind: 'flexible', type};
  }

  export function rigidBound(type: PolymorphicType): PolymorphicTypeBound {
    return {kind: 'rigid', type};
  }
}
