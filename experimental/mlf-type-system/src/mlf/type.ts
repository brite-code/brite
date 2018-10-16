export type Type = PolymorphicType;

export type MonomorphicType =
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

export type PolymorphicType =
  | MonomorphicType
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
    return type.kind !== 'Quantified' && type.kind !== 'Bottom';
  }

  /**
   * Gets all the free type variables in the provided type.
   *
   * Does not add the type variables of an unused quantified bound.
   */
  export function getFreeVariables(type: Type): ReadonlySet<string> {
    // NOTE: This function may get called a lot on some large types. It could be
    // a good idea to memoize this function based on the structure of types.

    function getFreeVariables(set: Set<string>, type: Type): void {
      switch (type.kind) {
        case 'Variable':
          set.add(type.identifier);
          break;
        case 'Constant':
          break;
        case 'Function':
          getFreeVariables(set, type.parameter);
          getFreeVariables(set, type.body);
          break;
        case 'Bottom':
          break;
        case 'Quantified': {
          getFreeVariables(set, type.body);
          // NOTE: If the quantified type variable is unused then we don’t add
          // its type variables to the free variables set. We will never use it
          // so what’s the point?
          if (set.has(type.binding)) {
            set.delete(type.binding);
            return getFreeVariables(set, type.bound.type); // Tail recursion
          }
          break;
        }
        default:
          const never: never = type;
          set.add(never);
          break;
      }
    }

    const set = new Set();
    getFreeVariables(set, type);
    return set;
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
    switch (type.kind) {
      case 'Variable':
        return type.identifier;

      case 'Constant': {
        switch (type.constant.kind) {
          case 'Boolean':
            return 'boolean';
          case 'Number':
            return 'number';
          case 'String':
            return 'string';
          default:
            const never: never = type.constant;
            return never;
        }
      }

      case 'Function': {
        let parameter = toDisplayString(type.parameter);
        parameter =
          type.parameter.kind === 'Variable' ||
          type.parameter.kind === 'Constant'
            ? parameter
            : `(${parameter})`;
        return `${parameter} → ${toDisplayString(type.body)}`;
      }

      case 'Bottom':
        return '⊥';

      case 'Quantified': {
        const body = toDisplayString(type.body);
        if (
          type.bound.kind === 'flexible' &&
          type.bound.type.kind === 'Bottom'
        ) {
          return `∀${type.binding}.${body}`;
        } else {
          const boundKind = type.bound.kind === 'flexible' ? '≥' : '=';
          const boundType = toDisplayString(type.bound.type);
          const binding = `${type.binding} ${boundKind} ${boundType}`;
          return `∀(${binding}).${body}`;
        }
      }

      default:
        const never: never = type;
        return never;
    }
  }
}
