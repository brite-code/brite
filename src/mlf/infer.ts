import * as Immutable from 'immutable';

import {booleanType, bottomType, numberType, stringType} from './builder';
import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {Prefix} from './prefix';
import {MonomorphicType, Type} from './type';
import {UnifyError, unify} from './unify';

/**
 * Infers the type of an untyped expression. Detects type incompatibilities and
 * converts malformed expressions into error expressions so they will crash
 * at runtime.
 */
export function infer<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  scope: Immutable.Map<string, Type>,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, Type> {
  return inferExpression(diagnostics, new Prefix(), scope, expression);
}

function inferExpression<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  prefix: Prefix,
  scope: Immutable.Map<string, Type>,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, Type> {
  switch (expression.description.kind) {
    // A variable references some value in the current context. The
    // variable’s type is that value’s type. If no such value exists then we
    // report an unbound variable.
    case 'Variable': {
      const variable = expression.description;
      const identifier = variable.identifier;
      const type = scope.get(identifier);
      if (type !== undefined) {
        return {type, description: variable};
      } else {
        return {
          type: bottomType,
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

    // Constant expressions have a native type.
    case 'Constant': {
      let type: Type;
      switch (expression.description.constant.kind) {
        case 'Boolean':
          type = booleanType;
          break;
        case 'Number':
          type = numberType;
          break;
        case 'String':
          type = stringType;
          break;
        default:
          const never: never = expression.description.constant;
          type = never;
          break;
      }
      return {type, description: expression.description};
    }

    case 'Function': {
      const function_ = expression.description;

      const {
        result: {parameterType, bodyType, body},
        bindings,
      } = prefix.quantify(() => {
        // Introduce a new type variable for the function parameter. Through the
        // inference of our function body we should solve this to a proper type.
        const parameterType = prefix.add({kind: 'flexible', type: bottomType});

        // Infer our function body. Introducing the variable we just defined
        // into scope.
        const body = inferExpression(
          diagnostics,
          prefix,
          scope.set(function_.parameter, {
            kind: 'Variable',
            identifier: parameterType,
          }),
          function_.body
        );

        // The type of our function body is a type variable with a flexible
        // bound on the polymorphic body type. This is so that the body may be
        // instantiated to different types.
        let bodyType: MonomorphicType;
        if (body.type.kind === 'Quantified' || body.type.kind === 'Bottom') {
          const identifier = prefix.add({kind: 'flexible', type: body.type});
          bodyType = {kind: 'Variable', identifier};
        } else {
          bodyType = body.type;
        }

        return {parameterType, bodyType, body};
      });

      // Create the type of our function. It is quantified by at least the
      // parameter type and body type.
      let type: Type = {
        kind: 'Function',
        parameter: {kind: 'Variable', identifier: parameterType},
        body: bodyType,
      };

      for (const [binding, bound] of Array.from(bindings).reverse()) {
        type = {kind: 'Quantified', binding, bound, body: type};
      }

      return {
        type,
        description: {
          kind: 'Function',
          parameter: function_.parameter,
          body,
        },
      };
    }

    case 'Call': {
      const call = expression.description;

      const {
        result: {bodyType, callee, argument, error},
        bindings,
      } = prefix.quantify(() => {
        // Infer the types for our callee and argument inside of our type
        // variable quantification.
        const callee = inferExpression(diagnostics, prefix, scope, call.callee);
        const argument = inferExpression(
          diagnostics,
          prefix,
          scope,
          call.argument
        );

        // Convert the callee and argument types to monomorphic types which we
        // can use in a monomorphic function constructor.

        let calleeType: MonomorphicType;
        if (
          callee.type.kind === 'Quantified' ||
          callee.type.kind === 'Bottom'
        ) {
          const identifier = prefix.add({kind: 'flexible', type: callee.type});
          calleeType = {kind: 'Variable', identifier};
        } else {
          calleeType = callee.type;
        }

        let argumentType: MonomorphicType;
        if (
          argument.type.kind === 'Quantified' ||
          argument.type.kind === 'Bottom'
        ) {
          const identifier = prefix.add({kind: 'flexible', type: argument.type}); // prettier-ignore
          argumentType = {kind: 'Variable', identifier};
        } else {
          argumentType = argument.type;
        }

        // Create a fresh type variable for the body type. This type will be
        // solved during unification.
        const bodyType = prefix.add({kind: 'flexible', type: bottomType});

        // Unify the type of the callee with the function type we expect. This
        // should solve any unknown type variables.
        const {error} = unify(diagnostics, prefix, calleeType, {
          kind: 'Function',
          parameter: argumentType,
          body: {kind: 'Variable', identifier: bodyType},
        });

        return {bodyType, callee, argument, error};
      });

      let type: Type = {kind: 'Variable', identifier: bodyType};
      for (const [binding, bound] of Array.from(bindings).reverse()) {
        type = {kind: 'Quantified', binding, bound, body: type};
      }

      // If there was an error during unification then we need to return an
      // error expression which will fail at runtime instead of an
      // call expression.
      return error === undefined
        ? {type, description: {kind: 'Call', callee, argument}}
        : {type, description: {kind: 'Error', error}};
    }

    // A binding infers a type for its value and introduces that value into
    // scope for the body. In a typical ML type system we would perform
    // generalization at let-bindings. However, in our MLF type system we keep
    // variables as polymorphic until they are applied. At which point we
    // instantiate them.
    case 'Binding': {
      const binding = expression.description;
      const value = inferExpression(diagnostics, prefix, scope, binding.value);
      const body = inferExpression(
        diagnostics,
        prefix,
        scope.set(binding.binding, value.type),
        binding.body
      );
      return {
        type: body.type,
        description: {kind: 'Binding', binding: binding.binding, value, body},
      };
    }

    // Runtime errors have the bottom type since they will crash at runtime.
    case 'Error':
      return {type: bottomType, description: expression.description};

    default:
      const never: never = expression.description;
      return never;
  }
}

export type InferError<T> =
  | UnifyError<T>
  | {
      readonly kind: 'UnboundVariable';
      readonly identifier: string;
    };
