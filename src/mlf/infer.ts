import * as Immutable from 'immutable';

import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {Identifier} from './identifier';
import {Prefix} from './prefix';
import {BooleanType, BottomType, NumberType, StringType, Type} from './type';
import {UnifyError, unify} from './unify';

/**
 * Infers the type of an untyped expression. Detects type incompatibilities and
 * converts malformed expressions into error expressions so they will crash
 * at runtime.
 */
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
    // A variable references some value in the current context. The
    // variable’s type is that value’s type. If no such value exists then we
    // report an unbound variable.
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

    // Constant expressions have a native type.
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
      const {prefix: prefix2, identifier: parameterType} = prefix1.add({
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
      const {prefix: prefix4, identifier: bodyType} = prefix3.add({
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
      const application = expression.description;
      const prefix1 = prefix.pushScope(Immutable.Map());

      // Infer the types for the expression being called and the argument to
      // that expression.
      const {prefix: prefix2, expression: callee} = infer(
        diagnostics,
        prefix1,
        context,
        application.callee
      );
      const {prefix: prefix3, expression: argument} = infer(
        diagnostics,
        prefix2,
        context,
        application.argument
      );

      // Create three new type variables for the callee, argument, and body. We
      // do not yet know the type for the body. The following unification should
      // resolve the body type.
      const {prefix: prefix4, identifier: calleeType} = prefix3.add({
        kind: 'flexible',
        type: callee.type,
      });
      const {prefix: prefix5, identifier: argumentType} = prefix4.add({
        kind: 'flexible',
        type: argument.type,
      });
      const {prefix: prefix6, identifier: bodyType} = prefix5.add({
        kind: 'flexible',
        type: BottomType,
      });

      // Unify the type of the callee with the function type we expect. This
      // should solve any unknown type variables.
      const {prefix: prefix7, error} = unify(
        diagnostics,
        prefix6,
        {kind: 'Variable', identifier: calleeType},
        {
          kind: 'Function',
          parameter: {kind: 'Variable', identifier: argumentType},
          body: {kind: 'Variable', identifier: bodyType},
        }
      );

      // The type of this application is quantified by the type variables
      // we created.
      const {prefix: prefix8, bindings} = prefix7.popScope();
      const type: Type = {
        kind: 'Quantified',
        prefix: bindings,
        body: {kind: 'Variable', identifier: bodyType},
      };

      return {
        prefix: prefix8,
        // If there was an error during unification then we need to return an
        // error expression which will fail at runtime instead of an
        // application expression.
        expression:
          error === undefined
            ? {type, description: {kind: 'Application', callee, argument}}
            : {type, description: {kind: 'Error', error}},
      };
    }

    // A binding infers a type for its value and introduces that value into
    // scope for the body.
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

    // Errors have the bottom type since they will crash at runtime.
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
  | UnifyError<T>
  | {
      readonly kind: 'UnboundVariable';
      readonly identifier: Identifier;
    };
