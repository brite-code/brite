import * as Immutable from 'immutable';

import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {Identifier} from './identifier';
import {
  BooleanType,
  BottomType,
  NumberType,
  Prefix,
  StringType,
  Type,
} from './type';

export function check<Diagnostic>(
  diagnostics: Diagnostics<CheckError<Diagnostic>>,
  prefix: Prefix,
  context: Context,
  expression: Expression<Diagnostic>
): Expression<CheckError<Diagnostic>, Type> {
  switch (expression.description.kind) {
    case 'Variable': {
      const variable = expression.description;
      const identifier = variable.identifier;
      const type = context.get(identifier);
      if (type !== undefined) {
        return {type, description: variable};
      } else {
        return {
          type: BottomType,
          description: {
            kind: 'Error',
            error: diagnostics.report({
              kind: 'UnboundVariable',
              identifier,
            }),
          },
        };
      }
    }

    case 'Constant': {
      switch (expression.description.constant.kind) {
        case 'Boolean':
          return {type: BooleanType, description: expression.description};
        case 'Number':
          return {type: NumberType, description: expression.description};
        case 'String':
          return {type: StringType, description: expression.description};
        default:
          const never: never = expression.description.constant;
          return never;
      }
    }

    case 'Function': {
      const function_ = expression.description;
      return;
    }

    case 'Application': {
      return;
    }

    case 'Binding': {
      const binding = expression.description;
      const value = check(diagnostics, prefix, context, binding.value);
      const body = check(
        diagnostics,
        prefix,
        context.set(binding.binding, value.type),
        binding.body
      );
      return {
        type: body.type,
        description: {kind: 'Binding', binding: binding.binding, value, body},
      };
    }

    case 'Error':
      return {type: BottomType, description: expression.description};

    default:
      const never: never = expression.description;
      return never;
  }
}

export class Context {
  private readonly bindings: Immutable.Map<Identifier, Type>;

  private constructor(bindings: Immutable.Map<Identifier, Type>) {
    this.bindings = bindings;
  }

  get(identifier: Identifier): Type | undefined {
    return this.bindings.get(identifier);
  }

  set(identifier: Identifier, type: Type): Context {
    return new Context(this.bindings.set(identifier, type));
  }
}

export type CheckError<T> =
  | T
  | {
      readonly kind: 'UnboundVariable';
      readonly identifier: Identifier;
    };
