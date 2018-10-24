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
      readonly name: string;
    }
  | {
      readonly kind: 'Constant';
      readonly constant: Constant;
    }
  | {
      readonly kind: 'Function';
      readonly param: string;
      readonly body: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Call';
      readonly callee: Expression<Diagnostic, Type>;
      readonly arg: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Binding';
      readonly name: string;
      readonly value: Expression<Diagnostic, Type>;
      readonly body: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Error';
      readonly error: Reported<Diagnostic>;
    };

export namespace Expression {
  export function variable(name: string): Expression {
    return {
      type: undefined,
      description: {kind: 'Variable', name},
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

  export function function_(param: string, body: Expression): Expression {
    return {
      type: undefined,
      description: {kind: 'Function', param, body},
    };
  }

  export function call(callee: Expression, arg: Expression): Expression {
    return {
      type: undefined,
      description: {kind: 'Call', callee, arg},
    };
  }

  export function binding(
    name: string,
    value: Expression,
    body: Expression
  ): Expression {
    return {
      type: undefined,
      description: {kind: 'Binding', name, value, body},
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
      name: string
    ): Expression<never, Type> {
      return {
        type,
        description: {kind: 'Variable', name},
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
      param: string,
      body: Expression<Diagnostic, Type>
    ): Expression<Diagnostic, Type> {
      return {
        type,
        description: {kind: 'Function', param, body},
      };
    }

    export function call<Diagnostic = never>(
      type: Type,
      callee: Expression<Diagnostic, Type>,
      arg: Expression<Diagnostic, Type>
    ): Expression<Diagnostic, Type> {
      return {
        type,
        description: {kind: 'Call', callee, arg},
      };
    }

    export function binding<Diagnostic = never>(
      name: string,
      value: Expression<Diagnostic, Type>,
      body: Expression<Diagnostic, Type>
    ): Expression<Diagnostic, Type> {
      return {
        type: body.type,
        description: {kind: 'Binding', name, value, body},
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
