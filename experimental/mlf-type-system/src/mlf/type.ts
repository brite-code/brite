import {Derivable, DerivableConstant} from '../utils/derive';

export type Type = PolymorphicType;

export type MonomorphicType = {
  readonly level: Derivable<number>;
  readonly description: MonomorphicTypeDescription;
};

export type PolymorphicType = {
  readonly level: Derivable<number>;
  readonly description: PolymorphicTypeDescription;
};

export type MonomorphicTypeDescription =
  | {
      readonly kind: 'Variable';
      readonly identifier: string;
    }
  | {
      readonly kind: 'Constant';
      readonly constant: ConstantType;
    }
  | {
      readonly kind: 'Function';
      readonly parameter: MonomorphicType;
      readonly body: MonomorphicType;
    };

export type ConstantType =
  | {readonly kind: 'Boolean'}
  | {readonly kind: 'Number'}
  | {readonly kind: 'String'};

export type PolymorphicTypeDescription =
  | MonomorphicTypeDescription
  | {
      readonly kind: 'Bottom';
    }
  | {
      readonly kind: 'Quantified';
      readonly binding: string;
      readonly bound: Bound;
      readonly body: PolymorphicType;
    };

export type Bound = {
  readonly kind: 'flexible' | 'rigid';
  readonly type: Type;
};

export namespace Type {
  /**
   * Returns true if the provided type is monomorphic.
   */
  export function isMonomorphic(type: Type): type is MonomorphicType {
    return (
      type.description.kind !== 'Quantified' &&
      type.description.kind !== 'Bottom'
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
        return type.description.identifier;

      case 'Constant': {
        switch (type.description.constant.kind) {
          case 'Boolean':
            return 'boolean';
          case 'Number':
            return 'number';
          case 'String':
            return 'string';
          default:
            const never: never = type.description.constant;
            return never;
        }
      }

      case 'Function': {
        let parameter = toDisplayString(type.description.parameter);
        parameter =
          type.description.parameter.description.kind === 'Variable' ||
          type.description.parameter.description.kind === 'Constant'
            ? parameter
            : `(${parameter})`;
        return `${parameter} → ${toDisplayString(type.description.body)}`;
      }

      case 'Bottom':
        return '⊥';

      case 'Quantified': {
        const quantified = type.description;
        const body = toDisplayString(quantified.body);
        if (
          quantified.bound.kind === 'flexible' &&
          quantified.bound.type.description.kind === 'Bottom'
        ) {
          return `∀${quantified.binding}.${body}`;
        } else {
          const boundKind = quantified.bound.kind === 'flexible' ? '≥' : '=';
          const boundType = toDisplayString(quantified.bound.type);
          const binding = `${quantified.binding} ${boundKind} ${boundType}`;
          return `∀(${binding}).${body}`;
        }
      }

      default:
        const never: never = type.description;
        return never;
    }
  }

  export function variable(identifier: string): MonomorphicType {
    return {
      level: new DerivableConstant(0),
      description: {kind: 'Variable', identifier},
    };
  }

  export function variableWithLevel(
    identifier: string,
    level: Derivable<number>
  ): MonomorphicType {
    return {
      level,
      description: {kind: 'Variable', identifier},
    };
  }

  export const boolean: MonomorphicType = {
    level: new DerivableConstant(0),
    description: {
      kind: 'Constant',
      constant: {kind: 'Boolean'},
    },
  };

  export const number: MonomorphicType = {
    level: new DerivableConstant(0),
    description: {
      kind: 'Constant',
      constant: {kind: 'Number'},
    },
  };

  export const string: MonomorphicType = {
    level: new DerivableConstant(0),
    description: {
      kind: 'Constant',
      constant: {kind: 'String'},
    },
  };

  export function function_(
    parameter: MonomorphicType,
    body: MonomorphicType
  ): MonomorphicType {
    return {
      level: Derivable.then2(parameter.level, body.level, (a, b) =>
        Math.max(a, b)
      ),
      description: {kind: 'Function', parameter, body},
    };
  }

  export function quantified(
    binding: string,
    bound: Bound,
    body: PolymorphicType
  ): PolymorphicType {
    return {
      level: Derivable.then2(bound.type.level, body.level, (a, b) =>
        Math.max(a, b)
      ),
      description: {kind: 'Quantified', binding, bound, body},
    };
  }

  export const bottom: PolymorphicType = {
    level: new DerivableConstant(0),
    description: {kind: 'Bottom'},
  };

  export function rigidBound(type: PolymorphicType): Bound {
    return {kind: 'rigid', type};
  }

  export function flexibleBound(type: PolymorphicType): Bound {
    return {kind: 'flexible', type};
  }
}
