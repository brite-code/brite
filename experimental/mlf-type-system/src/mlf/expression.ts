import {Reported} from './diagnostics';
import {Type} from './type';

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
      readonly kind: 'Call';
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

export namespace Expression {
  export function variable(identifier: string): Expression {
    return {
      type: undefined,
      description: {kind: 'Variable', identifier},
    };
  }

  export function boolean(value: boolean): Expression {
    return {
      type: undefined,
      description: {kind: 'Constant', constant: {kind: 'Boolean', value}},
    };
  }

  export function number(value: number): Expression {
    return {
      type: undefined,
      description: {kind: 'Constant', constant: {kind: 'Number', value}},
    };
  }

  export function string(value: string): Expression {
    return {
      type: undefined,
      description: {kind: 'Constant', constant: {kind: 'String', value}},
    };
  }

  export function function_(parameter: string, body: Expression): Expression {
    return {
      type: undefined,
      description: {kind: 'Function', parameter, body},
    };
  }

  export function call(callee: Expression, argument: Expression): Expression {
    return {
      type: undefined,
      description: {kind: 'Call', callee, argument},
    };
  }

  export function binding(
    binding: string,
    value: Expression,
    body: Expression
  ): Expression {
    return {
      type: undefined,
      description: {kind: 'Binding', binding, value, body},
    };
  }

  export function error<Diagnostic>(
    error: Reported<Diagnostic>
  ): Expression<Diagnostic> {
    return {
      type: undefined,
      description: {kind: 'Error', error},
    };
  }

  export namespace Typed {
    export function variable(
      type: Type,
      identifier: string
    ): Expression<never, Type> {
      return {
        type,
        description: {kind: 'Variable', identifier},
      };
    }

    export function boolean(value: boolean): Expression<never, Type> {
      return {
        type: Type.boolean,
        description: {kind: 'Constant', constant: {kind: 'Boolean', value}},
      };
    }

    export function number(value: number): Expression<never, Type> {
      return {
        type: Type.number,
        description: {kind: 'Constant', constant: {kind: 'Number', value}},
      };
    }

    export function string(value: string): Expression<never, Type> {
      return {
        type: Type.string,
        description: {kind: 'Constant', constant: {kind: 'String', value}},
      };
    }

    export function function_<Diagnostic = never>(
      type: Type,
      parameter: string,
      body: Expression<Diagnostic, Type>
    ): Expression<Diagnostic, Type> {
      return {
        type,
        description: {kind: 'Function', parameter, body},
      };
    }

    export function call<Diagnostic = never>(
      type: Type,
      callee: Expression<Diagnostic, Type>,
      argument: Expression<Diagnostic, Type>
    ): Expression<Diagnostic, Type> {
      return {
        type,
        description: {kind: 'Call', callee, argument},
      };
    }

    export function binding<Diagnostic = never>(
      binding: string,
      value: Expression<Diagnostic, Type>,
      body: Expression<Diagnostic, Type>
    ): Expression<Diagnostic, Type> {
      return {
        type: body.type,
        description: {kind: 'Binding', binding, value, body},
      };
    }

    export function error<Diagnostic>(
      type: Type,
      error: Reported<Diagnostic>
    ): Expression<Diagnostic, Type> {
      return {
        type,
        description: {kind: 'Error', error},
      };
    }
  }
}
