import {TypeIdentifier} from './identifier';

export type Type = PolymorphicType;

export const BooleanType: Type = {kind: 'Constant', constant: {kind: 'Boolean'}}; // prettier-ignore
export const NumberType: Type = {kind: 'Constant', constant: {kind: 'Number'}};
export const StringType: Type = {kind: 'Constant', constant: {kind: 'String'}};
export const BottomType: BottomType<never> = {kind: 'Bottom'};

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
      readonly prefix: ReadonlyMap<TypeIdentifier, Bound>;
      readonly body: BottomType<MonomorphicType>;
    };

export type Bound = {
  readonly kind: 'flexible' | 'rigid';
  readonly type: Type;
};
