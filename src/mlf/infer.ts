import * as Immutable from 'immutable';

import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {Identifier} from './identifier';
import {Prefix} from './prefix';
import {BooleanType, BottomType, NumberType, StringType, Type} from './type';

export function infer<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  prefix: Prefix,
  context: Immutable.Map<Identifier, Type>,
  expression: Expression<Diagnostic>
): {
  readonly prefix: Prefix;
  readonly expression: Expression<InferError<Diagnostic>, Type>;
} {
  switch (expression.description.kind) {
    case 'Variable': {
      const variable = expression.description;
      const identifier = variable.identifier;
      const type = context.get(identifier);
      if (type !== undefined) {
        return {prefix, expression: {type, description: variable}};
      } else {
        return {
          prefix,
          expression: {
            type: BottomType,
            description: {
              kind: 'Error',
              error: diagnostics.report({
                kind: 'UnboundVariable',
                identifier,
              }),
            },
          },
        };
      }
    }

    case 'Constant': {
      let type: Type;
      switch (expression.description.constant.kind) {
        case 'Boolean':
          type = BooleanType;
          break;
        case 'Number':
          type = NumberType;
          break;
        case 'String':
          type = StringType;
          break;
        default:
          const never: never = expression.description.constant;
          type = never;
          break;
      }
      return {prefix, expression: {type, description: expression.description}};
    }

    case 'Function': {
      const function_ = expression.description;

      // Push a new scope and create a fresh type variable for the parameter
      // type. It starts out with a flexible bound on the bottom type.
      const prefix1 = prefix.pushScope(Immutable.Map());
      const {prefix: prefix2, identifier: parameterType} = prefix1.newVariable({
        kind: 'flexible',
        type: BottomType,
      });

      // Infer the type of the function body. Making sure to add the new
      // parameter type to our context.
      const {prefix: prefix3, expression: body} = infer(
        diagnostics,
        prefix2,
        context.set(function_.parameter, {
          kind: 'Variable',
          identifier: parameterType,
        }),
        function_.body
      );

      // The type of the function body is polymorphic. That means we can’t add
      // it directly to our monomorphic function type. So we create a new type
      // variable in the scope we just pushed that will be available in our
      // popped scope prefix. Also create our quantified function type.
      const {prefix: prefix4, identifier: bodyType} = prefix3.newVariable({
        kind: 'flexible',
        type: body.type,
      });
      const {prefix: prefix5, bindings} = prefix4.popScope();
      const type: Type = {
        kind: 'Quantified',
        prefix: bindings,
        body: {
          kind: 'Function',
          parameter: {kind: 'Variable', identifier: parameterType},
          body: {kind: 'Variable', identifier: bodyType},
        },
      };

      return {
        prefix: prefix5,
        expression: {
          type,
          description: {
            kind: 'Function',
            parameter: function_.parameter,
            body,
          },
        },
      };
    }

    case 'Application': {
      return;
    }

    case 'Binding': {
      const binding = expression.description;
      const {prefix: prefix1, expression: value} = infer(
        diagnostics,
        prefix,
        context,
        binding.value
      );
      const {prefix: prefix2, expression: body} = infer(
        diagnostics,
        prefix1,
        context.set(binding.binding, value.type),
        binding.body
      );
      return {
        prefix: prefix2,
        expression: {
          type: body.type,
          description: {kind: 'Binding', binding: binding.binding, value, body},
        },
      };
    }

    case 'Error':
      return {
        prefix,
        expression: {type: BottomType, description: expression.description},
      };

    default:
      const never: never = expression.description;
      return never;
  }
}

export type InferError<T> =
  | T
  | {
      readonly kind: 'UnboundVariable';
      readonly identifier: Identifier;
    };
