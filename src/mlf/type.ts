import {TypeIdentifier} from './identifier';

export type Constant =
  | {readonly kind: 'Boolean'}
  | {readonly kind: 'Number'}
  | {readonly kind: 'String'};

export type MonomorphicType =
  | {
      readonly kind: 'Variable';
      readonly identifier: TypeIdentifier;
    }
  | {
      readonly kind: 'Constant';
      readonly constant: Constant;
    }
  | {
      readonly kind: 'Function';
      readonly parameter: MonomorphicType;
      readonly body: MonomorphicType;
    };

export type PolymorphicType =
  | MonomorphicType
  | {
      readonly kind: 'Bottom';
    }
  | {
      readonly kind: 'Quantification';
      readonly binding: TypeIdentifier;
      readonly bound: PolymorphicType;
      readonly boundType: 'flexible' | 'rigid';
      readonly body: PolymorphicType;
    };

export const BooleanType: MonomorphicType = {
  kind: 'Constant',
  constant: {kind: 'Boolean'},
};

export const NumberType: MonomorphicType = {
  kind: 'Constant',
  constant: {kind: 'Number'},
};

export const StringType: MonomorphicType = {
  kind: 'Constant',
  constant: {kind: 'String'},
};

export const BottomType: PolymorphicType = {kind: 'Bottom'};
