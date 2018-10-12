import {Diagnostics} from './diagnostics';
import {Context, Prefix} from './environment';
import {Expression} from './expression';
import {Identifier} from './identifier';
import {
  BooleanType,
  BottomType,
  NumberType,
  PolymorphicType,
  StringType,
} from './type';

export function check<Error>(
  diagnostics: Diagnostics,
  prefix: Prefix,
  context: Context,
  expression: Expression<Error>
): Expression<Error | CheckError, PolymorphicType> {
  switch (expression.description.kind) {
    case 'Variable': {
      const identifier = expression.description.identifier;
      const type = context.bindings.get(identifier);
      if (type !== undefined) {
        return {type, description: expression.description};
      } else {
        return {
          type: BottomType,
          description: {
            kind: 'Error',
            error: Diagnostics.report<CheckError>(diagnostics, {
              kind: 'VariableUnbound',
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

    case 'Binding': {
      const binding = expression.description;
      const value = check(diagnostics, prefix, context, binding.value);
      const body = check(
        diagnostics,
        prefix,
        {bindings: context.bindings.set(binding.binding, value.type)},
        binding.body
      );
      return {
        type: body.type,
        description: {kind: 'Binding', binding: binding.binding, value, body},
      };
    }

    default:
      const never: never = expression.description;
      return never;
  }
}

export type CheckError = {
  readonly kind: 'VariableUnbound';
  readonly identifier: Identifier;
};
