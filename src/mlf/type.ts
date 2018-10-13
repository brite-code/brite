import * as Immutable from 'immutable';

import {TypeIdentifier} from './identifier';

export type Type = PolymorphicType;

export const BooleanType: Type = {kind: 'Constant', constant: {kind: 'Boolean'}}; // prettier-ignore
export const NumberType: Type = {kind: 'Constant', constant: {kind: 'Number'}};
export const StringType: Type = {kind: 'Constant', constant: {kind: 'String'}};
export const BottomType: Type = {kind: 'Bottom'};

export type MonomorphicType<T = never> =
  | T
  | {
      readonly kind: 'Variable';
      readonly identifier: TypeIdentifier;
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

export type PolymorphicType<T = never> = QuantifiedType<
  BottomType<MonomorphicType<T>>
>;

export type BottomType<T = never> = T | {readonly kind: 'Bottom'};

export type QuantifiedType<T = never> =
  | T
  | {
      readonly kind: 'Quantified';
      readonly prefix: Prefix;
      readonly body: BottomType<MonomorphicType>;
    };

export type Bound = {
  readonly kind: 'flexible' | 'rigid';
  readonly type: Type;
};

export class Prefix {
  public static empty = new Prefix(Immutable.Map());

  private readonly bindings: Immutable.Map<TypeIdentifier, Bound>;

  private constructor(bindings: Immutable.Map<TypeIdentifier, Bound>) {
    this.bindings = bindings;
  }

  get(identifier: TypeIdentifier): Bound | undefined {
    return this.bindings.get(identifier);
  }

  set(identifier: TypeIdentifier, bound: Bound): Prefix {
    return new Prefix(this.bindings.set(identifier, bound));
  }

  /**
   * Merges this prefix with another prefix.
   *
   * We expect that the prefixes have disjoint domains, so we panic if we find
   * they both contain the same type variable.
   */
  merge(other: Prefix): Prefix {
    const bindings = this.bindings.mergeWith(() => {
      throw new Error('Expected distinct types in merged prefixes.');
    }, other.bindings);
    return new Prefix(bindings);
  }
}
