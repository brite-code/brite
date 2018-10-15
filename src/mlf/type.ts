const opaque = Symbol();

export type TypeIdentifier = number & typeof opaque;

export namespace TypeIdentifier {
  export function create(x: number): TypeIdentifier {
    return x as TypeIdentifier;
  }
}

export type Type = PolymorphicType;

export const BooleanType: MonomorphicType = {kind: 'Constant', constant: {kind: 'Boolean'}}; // prettier-ignore
export const NumberType: MonomorphicType = {kind: 'Constant', constant: {kind: 'Number'}}; // prettier-ignore
export const StringType: MonomorphicType = {kind: 'Constant', constant: {kind: 'String'}}; // prettier-ignore
export const BottomType: BottomType = {kind: 'Bottom'};

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
