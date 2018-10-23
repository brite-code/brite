import * as Immutable from 'immutable';

import * as t from './builder';
import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {Bound, MonomorphicType, Type} from './type';
import {TypeVariable, UnifyError, unify} from './unify';

/**
 * Infers the type of an untyped expression. Detects type incompatibilities and
 * converts malformed expressions into error expressions so they will crash
 * at runtime.
 */
export function infer<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  context: Iterable<[string, Type]>,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, Type> {
  id = 1;
  return inferExpression(
    diagnostics,
    Immutable.Map(),
    Immutable.Map(context),
    expression
  );
}

let id = 1;

function inferExpression<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  prefix: Immutable.Map<string, TypeVariable>,
  context: Immutable.Map<string, Type>,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, Type> {
  switch (expression.description.kind) {
    // A variable references some value in the current context. The
    // variable’s type is that value’s type. If no such value exists then we
    // report an unbound variable.
    case 'Variable': {
      const variable = expression.description;
      const identifier = variable.identifier;
      const type = context.get(identifier);
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
      const parameterIdentifier = `$${id++}`;
      const parameterType = t.variableType(parameterIdentifier);
      const parameterBound: Bound = {kind: 'flexible', type: t.bottomType};
      const parameterVariable = TypeVariable.newRoot(prefix, parameterBound);
      prefix = prefix.set(parameterIdentifier, parameterVariable);

      // Infer our function body type. Introducing the variable we just defined
      // into scope.
      const body = inferExpression(
        diagnostics,
        prefix,
        context.set(fun.parameter, parameterType),
        fun.body
      );

      // If the type of our body is polymorphic then we need to quantify the
      // type of our function by our body type.
      let type: Type;
      if (Type.isMonomorphic(body.type)) {
        type = t.functionType(parameterType, body.type);
      } else {
        const identifier = `$${id++}`;
        const bound: Bound = {kind: 'flexible', type: body.type};
        const typeVariable = TypeVariable.newRoot(prefix, bound);
        type = typeVariable.quantify(
          'todo',
          t.functionType(parameterType, t.variableType(identifier))
        );
      }

      // Quantify the type by our parameter type variable.
      type = parameterVariable.quantify('todo', type);

      return t.functionExpressionTyped(type, fun.parameter, body);
    }

    case 'Call': {
      const call = expression.description;

      // Infer the types for our callee and argument inside of our type
      // variable quantification.
      const callee = inferExpression(diagnostics, prefix, context, call.callee);
      const argument = inferExpression(
        diagnostics,
        prefix,
        context,
        call.argument
      );

      // Keeps track of the type variables we declare so that we can quantify
      // them later.
      const localTypeVariables: Array<TypeVariable> = [];

      // Convert the callee to a monomorphic type. If the callee type is
      // polymorphic then we need to add a type variable to our prefix.
      let calleeType: MonomorphicType;
      if (Type.isMonomorphic(callee.type)) {
        calleeType = callee.type;
      } else {
        const identifier = `$${id++}`;
        const bound: Bound = {kind: 'flexible', type: callee.type};
        const typeVariable = TypeVariable.newRoot(prefix, bound);
        prefix = prefix.set(identifier, typeVariable);
        localTypeVariables.push(typeVariable);
        calleeType = t.variableType(identifier);
      }

      // Convert the argument type to a monomorphic type. If the argument type
      // is polymorphic then we need to add a type variable to our prefix.
      let argumentType: MonomorphicType;
      if (Type.isMonomorphic(argument.type)) {
        argumentType = argument.type;
      } else {
        const identifier = `$${id++}`;
        const bound: Bound = {kind: 'flexible', type: argument.type};
        const typeVariable = TypeVariable.newRoot(prefix, bound);
        prefix = prefix.set(identifier, typeVariable);
        localTypeVariables.push(typeVariable);
        argumentType = t.variableType(identifier);
      }

      // Create a fresh type variable for the body type. This type will be
      // solved during unification.
      const bodyIdentifier = `$${id++}`;
      const bodyBound: Bound = {kind: 'flexible', type: t.bottomType};
      const bodyTypeVariable = TypeVariable.newRoot(prefix, bodyBound);
      prefix = prefix.set(bodyIdentifier, bodyTypeVariable);
      const bodyType = t.variableType(bodyIdentifier);

      // Unify the type of the callee with the function type we expect. This
      // should solve any unknown type variables.
      const error = unify(
        diagnostics,
        prefix,
        calleeType,
        t.functionType(argumentType, bodyType)
      );

      // Quantify our body type in the reverse order of which we added our local
      // type variables.
      const type = localTypeVariables.reduceRight(
        (type, localTypeVariable) => localTypeVariable.quantify('todo', type),
        bodyTypeVariable.quantify('todo', bodyType)
      );

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
      const value = inferExpression(
        diagnostics,
        prefix,
        context,
        binding.value
      );
      const body = inferExpression(
        diagnostics,
        prefix,
        context.set(binding.binding, value.type),
        binding.body
      );
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
