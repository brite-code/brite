import {BindingMap} from './bindings';
import * as t from './builder';
import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {Prefix} from './prefix';
import {Type} from './type';
import {UnifyError, unify} from './unify';

/**
 * Infers the type of an untyped expression. Detects type incompatibilities and
 * converts malformed expressions into error expressions so they will crash
 * at runtime.
 */
export function infer<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  scope: BindingMap<string, Type>,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, Type> {
  return inferExpression(diagnostics, new Prefix(), scope, expression);
}

function inferExpression<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  prefix: Prefix,
  scope: BindingMap<string, Type>,
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
        return t.variableExpressionTyped(type, identifier);
      } else {
        return t.errorExpressionTyped(
          t.bottomType,
          diagnostics.report({
            kind: 'UnboundVariable',
            identifier,
          })
        );
      }
    }

    // Constant expressions have a native type.
    case 'Constant': {
      const constant = expression.description.constant;
      switch (constant.kind) {
        case 'Boolean':
          return t.booleanExpressionTyped(constant.value);
        case 'Number':
          return t.numberExpressionTyped(constant.value);
        case 'String':
          return t.stringExpressionTyped(constant.value);
        default:
          const never: never = constant;
          return never;
      }
    }

    case 'Function': {
      const fun = expression.description;

      const {
        result: {parameterType, bodyType, body},
        bindings,
      } = prefix.quantify(() => {
        // Introduce a new type variable for the function parameter. Through the
        // inference of our function body we should solve this to a proper type.
        const parameterType = t.variableType(
          prefix.add({
            kind: 'flexible',
            type: t.bottomType,
          })
        );

        // Infer our function body. Introducing the variable we just defined
        // into scope.
        scope.push(fun.parameter, parameterType);
        const body = inferExpression(diagnostics, prefix, scope, fun.body);
        scope.pop(fun.parameter);

        // The type of our function body is a type variable with a flexible
        // bound on the polymorphic body type. This is so that the body may be
        // instantiated to different types.
        const bodyType = Type.isMonomorphic(body.type)
          ? body.type
          : t.variableType(prefix.add({kind: 'flexible', type: body.type}));

        return {parameterType, bodyType, body};
      });

      // Create the type of our function. It is quantified by at least the
      // parameter type and body type.
      let type: Type = t.functionType(parameterType, bodyType);
      for (const {binding, bound} of bindings) {
        type = t.quantifiedType(binding, bound, type);
      }

      return t.functionExpressionTyped(type, fun.parameter, body);
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
        const calleeType = Type.isMonomorphic(callee.type)
          ? callee.type
          : t.variableType(prefix.add({kind: 'flexible', type: callee.type}));
        const argumentType = Type.isMonomorphic(argument.type)
          ? argument.type
          : t.variableType(prefix.add({kind: 'flexible', type: argument.type}));

        // Create a fresh type variable for the body type. This type will be
        // solved during unification.
        const bodyType = t.variableType(
          prefix.add({kind: 'flexible', type: t.bottomType})
        );

        // Unify the type of the callee with the function type we expect. This
        // should solve any unknown type variables.
        const {error} = unify(
          diagnostics,
          prefix,
          calleeType,
          t.functionType(argumentType, bodyType)
        );

        return {bodyType, callee, argument, error};
      });

      let type: Type = bodyType;
      for (const {binding, bound} of bindings) {
        type = t.quantifiedType(binding, bound, type);
      }

      // If there was an error during unification then we need to return an
      // error expression which will fail at runtime instead of an
      // call expression.
      return error === undefined
        ? t.callExpressionTyped(type, callee, argument)
        : t.errorExpressionTyped(type, error);
    }

    // A binding infers a type for its value and introduces that value into
    // scope for the body. In a typical ML type system we would perform
    // generalization at let-bindings. However, in our MLF type system we keep
    // variables as polymorphic until they are applied. At which point we
    // instantiate them.
    case 'Binding': {
      const binding = expression.description;
      const value = inferExpression(diagnostics, prefix, scope, binding.value);
      scope.push(binding.binding, value.type);
      const body = inferExpression(diagnostics, prefix, scope, binding.body);
      scope.pop(binding.binding);
      return t.bindingExpressionTyped(binding.binding, value, body);
    }

    // Runtime errors have the bottom type since they will crash at runtime.
    case 'Error':
      return t.errorExpressionTyped(t.bottomType, expression.description.error);

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
