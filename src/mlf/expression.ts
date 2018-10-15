import {Reported} from './diagnostics';

export type Constant =
  | {readonly kind: 'Boolean'; readonly value: boolean}
  | {readonly kind: 'Number'; readonly value: number}
  | {readonly kind: 'String'; readonly value: string};

export type Expression<Diagnostic = never, Type = undefined> = {
  readonly type: Type;
  readonly description: ExpressionDescription<Diagnostic, Type>;
};

export type ExpressionDescription<Diagnostic, Type> =
  | {
      readonly kind: 'Variable';
      readonly identifier: string;
    }
  | {
      readonly kind: 'Constant';
      readonly constant: Constant;
    }
  | {
      readonly kind: 'Function';
      readonly parameter: string;
      readonly body: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Application';
      readonly callee: Expression<Diagnostic, Type>;
      readonly argument: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Binding';
      readonly binding: string;
      readonly value: Expression<Diagnostic, Type>;
      readonly body: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Error';
      readonly error: Reported<Diagnostic>;
    };
