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
