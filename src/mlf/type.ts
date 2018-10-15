export type Type = PolymorphicType;

export type MonomorphicType<T = never> =
  | T
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

export type PolymorphicType<T = never> = QuantifiedType<
  BottomType<MonomorphicType<T>>
>;

export type BottomType<T = never> = T | {readonly kind: 'Bottom'};

export type QuantifiedType<T = never> =
  | T
  | {
      readonly kind: 'Quantified';
      readonly bindings: ReadonlyMap<string, Bound>;
      readonly body: BottomType<MonomorphicType>;
    };

export type Bound = {
  readonly kind: 'flexible' | 'rigid';
  readonly type: Type;
};
