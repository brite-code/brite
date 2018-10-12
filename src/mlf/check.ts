import {Diagnostics} from './diagnostics';
import {Context, Prefix} from './environment';
import {Expression} from './expression';
import {Identifier} from './identifier';
import {BottomType, PolymorphicType} from './type';

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
  }
}

export type CheckError = {
  readonly kind: 'VariableUnbound';
  readonly identifier: Identifier;
};
