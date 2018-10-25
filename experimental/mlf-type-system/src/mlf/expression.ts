import {Reported} from './diagnostics';
import {Polytype, Type} from './type';

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
      readonly name: string;
      readonly value: Expression<Diagnostic, Type>;
      readonly body: Expression<Diagnostic, Type>;
    }
  | {
      readonly kind: 'Annotation';
      readonly value: Expression<Diagnostic, Type>;
      readonly type: Polytype;
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
    name: string,
    value: Expression,
    body: Expression
  ): Expression {
    return {
      type: undefined,
      description: {kind: 'Binding', name, value, body},
    };
  }

  export function annotation(value: Expression, type: Type): Expression {
    return {
      type: undefined,
      description: {kind: 'Annotation', value, type},
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
      name: string,
      value: Expression<Diagnostic, Type>,
      body: Expression<Diagnostic, Type>
    ): Expression<Diagnostic, Type> {
      return {
        type: body.type,
        description: {kind: 'Binding', name, value, body},
      };
    }

    export function annotation<Diagnostic = never>(
      value: Expression<Diagnostic, Type>,
      type: Type
    ): Expression<Diagnostic, Type> {
      return {
        type,
        description: {kind: 'Annotation', value, type},
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
