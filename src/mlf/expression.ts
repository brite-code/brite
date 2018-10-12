import {ReportedDiagnostic} from './diagnostics';
import {Identifier} from './identifier';

export type Constant =
  | {readonly kind: 'Boolean'; readonly value: boolean}
  | {readonly kind: 'Number'; readonly value: number}
  | {readonly kind: 'String'; readonly value: string};

export type Expression<Error = never, Type = undefined> = {
  readonly type: Type;
  readonly description: ExpressionDescription<Error, Type>;
};

export type ExpressionDescription<Error, Type> =
  | {
      readonly kind: 'Variable';
      readonly identifier: Identifier;
    }
  | {
      readonly kind: 'Constant';
      readonly constant: Constant;
    }
  | {
      readonly kind: 'Function';
      readonly parameter: Identifier;
      readonly body: Expression<Error, Type>;
    }
  | {
      readonly kind: 'Application';
      readonly callee: Expression<Error, Type>;
      readonly argument: Expression<Error, Type>;
    }
  | {
      readonly kind: 'Binding';
      readonly binding: Identifier;
      readonly value: Expression<Error, Type>;
      readonly body: Expression<Error, Type>;
    }
  | {
      readonly kind: 'Error';
      readonly error: ReportedDiagnostic<Error>;
    };
