import {Reported} from './diagnostics';

const opaque = Symbol();

export type Identifier = string & typeof opaque;

export namespace Identifier {
  export function create(x: string): Identifier {
    return x as Identifier;
  }
}

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
      readonly identifier: Identifier;
    }
  | {
      readonly kind: 'Constant';
      readonly constant: Constant;
    }
  | {
      readonly kind: 'Function';
      readonly parameter: Identifier;
      readonly body: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Application';
      readonly callee: Expression<Diagnostic, Type>;
      readonly argument: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Binding';
      readonly binding: Identifier;
      readonly value: Expression<Diagnostic, Type>;
      readonly body: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Error';
      readonly error: Reported<Diagnostic>;
    };
