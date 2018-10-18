import {BindingMap} from './bindings';
import * as t from './builder';
import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {Prefix} from './prefix';
import {Bound, MonomorphicType, Type} from './type';
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

      // Introduce a new type variable for the function parameter. Through the
      // inference of our function body we should solve this to a proper type.
      const parameterType = t.variableType(
        prefix.pushWithGeneratedName({
          kind: 'flexible',
          type: t.bottomType,
        })
      );

      // Infer our function body type. Introducing the variable we just defined
      // into scope.
      scope.push(fun.parameter, parameterType);
      const body = inferExpression(diagnostics, prefix, scope, fun.body);
      scope.pop();

      // If the type of our body is polymorphic then we need to quantify the
      // type of our function by our body type.
      let type: Type;
      if (Type.isMonomorphic(body.type)) {
        type = t.functionType(parameterType, body.type);
      } else {
        const bound: Bound = {kind: 'flexible', type: body.type};
        const identifier = prefix.pushWithGeneratedName(bound);
        type = prefix.pop(
          t.functionType(parameterType, t.variableType(identifier))
        );
      }

      // Quantify our function type by the parameter type variable.
      type = prefix.pop(type);

      return t.functionExpressionTyped(type, fun.parameter, body);
    }

    case 'Call': {
      const call = expression.description;

      // Infer the types for our callee and argument inside of our type
      // variable quantification.
      const callee = inferExpression(diagnostics, prefix, scope, call.callee);
      const argument = inferExpression(
        diagnostics,
        prefix,
        scope,
        call.argument
      );

      // Counts the type variables we declare so that we can pop them out of
      // scope later.
      let pops: number = 0;

      // Convert the callee to a monomorphic type. If the callee type is
      // polymorphic then we need to add a type variable to our prefix.
      let calleeType: MonomorphicType;
      if (Type.isMonomorphic(callee.type)) {
        calleeType = callee.type;
      } else {
        const bound: Bound = {kind: 'flexible', type: callee.type};
        const identifier = prefix.pushWithGeneratedName(bound);
        pops++;
        calleeType = t.variableType(identifier);
      }

      // Convert the argument type to a monomorphic type. If the argument type
      // is polymorphic then we need to add a type variable to our prefix.
      let argumentType: MonomorphicType;
      if (Type.isMonomorphic(argument.type)) {
        argumentType = argument.type;
      } else {
        const bound: Bound = {kind: 'flexible', type: argument.type};
        const identifier = prefix.pushWithGeneratedName(bound);
        pops++;
        argumentType = t.variableType(identifier);
      }

      // Create a fresh type variable for the body type. This type will be
      // solved during unification.
      const bodyType = t.variableType(
        prefix.pushWithGeneratedName({kind: 'flexible', type: t.bottomType})
      );

      // Unify the type of the callee with the function type we expect. This
      // should solve any unknown type variables.
      const error = unify(
        diagnostics,
        prefix,
        calleeType,
        t.functionType(argumentType, bodyType)
      );

      // Create our return type by quantifying in the reverse order of which we
      // added our local type variables.
      let type = prefix.pop(bodyType);
      for (let i = 0; i < pops; i++) type = prefix.pop(type);

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
      scope.pop();
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
