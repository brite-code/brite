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
