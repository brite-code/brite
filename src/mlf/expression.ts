import {Identifier} from './identifier';

export type Constant =
  | {readonly kind: 'Boolean'; readonly value: boolean}
  | {readonly kind: 'Number'; readonly value: number}
  | {readonly kind: 'String'; readonly value: string};

export type Expression =
  | {
      readonly kind: 'Variable';
      readonly identifier: Identifier;
    }
  | {
      readonly kind: 'Constant';
      readonly constant: Constant;
    }
  | {
      readonly kind: 'Abstraction';
      readonly parameter: Identifier;
      readonly body: Expression;
    }
  | {
      readonly kind: 'Application';
      readonly callee: Expression;
      readonly argument: Expression;
    }
  | {
      readonly kind: 'Binding';
      readonly binding: Identifier;
      readonly value: Expression;
      readonly body: Expression;
    };
