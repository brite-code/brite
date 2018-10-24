import * as Immutable from 'immutable';

import {BindingMap} from './bindings';
import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {State} from './state';
import {Bound, MonomorphicType, PolymorphicType, Type} from './type';
import {TypeVariable, UnifyError, unify} from './unify';

/**
 * Infers the type of an untyped expression. Detects type incompatibilities and
 * converts malformed expressions into error expressions so they will crash
 * at runtime.
 */
export function infer<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  scope: Iterable<[string, Type]>,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, Type> {
  return inferExpression(
    diagnostics,
    new BindingMap(scope),
    new State(),
    expression
  );
}

function inferExpression<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  scope: BindingMap<string, Type>,
  state: State,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, Type> {
  switch (expression.description.kind) {
    // A variable references some value in the current context. The
    // variable’s type is that value’s type. If no such value exists then we
    // report an unbound variable.
    case 'Variable': {
      const variable = expression.description;
      const identifier = variable.name;
      const type = scope.get(identifier);
      if (type !== undefined) {
        return Expression.Typed.variable(type, identifier);
      } else {
        return Expression.Typed.error(
          Type.bottom,
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
          return Expression.Typed.boolean(constant.value);
        case 'Number':
          return Expression.Typed.number(constant.value);
        case 'String':
          return Expression.Typed.string(constant.value);
        default:
          const never: never = constant;
          return never;
      }
    }

    case 'Function': {
      const function_ = expression.description;

      // Increment the level. This is because we will generalize all dead type
      // variables at the end of type inference.
      state.incrementLevel();

      // Introduce a new type variable for the function parameter. Through the
      // inference of our function body we should solve this to a proper type.
      const parameterType = state.newType();

      // Infer our function body type. Introducing the variable we just defined
      // into scope.
      scope.push(function_.param, parameterType);
      const body = inferExpression(diagnostics, scope, state, function_.body);
      scope.pop();

      // If the type of our body is polymorphic then we need to quantify the
      // type of our function by our body type.
      const type = Type.isMonomorphic(body.type)
        ? Type.function_(parameterType, body.type)
        : state.newTypeWithBound(Type.flexibleBound(body.type));

      // Generalize dead type variables at this level before continuing on.
      const newType = generalize(prefix, level, type);

      // Decrement the level now that we’ve generalized the types we created at
      // this level.
      state.decrementLevel();

      return Expression.Typed.function_(newType, function_.param, body);
    }

    case 'Call': {
      const call = expression.description;

      // Increment the level. This is because we will generalize all dead type
      // variables at the end of type inference.
      state.incrementLevel();

      // Infer the types for our callee and argument inside of our type
      // variable quantification.
      const callee = inferExpression(diagnostics, scope, state, call.callee);
      const arg = inferExpression(diagnostics, scope, state, call.arg);

      // Convert the callee to a monomorphic type. If the callee type is
      // polymorphic then we need to add a type variable to our prefix.
      const calleeType = Type.isMonomorphic(callee.type)
        ? callee.type
        : state.newTypeWithBound(Type.flexibleBound(callee.type));

      // Convert the argument type to a monomorphic type. If the argument type
      // is polymorphic then we need to add a type variable to our prefix.
      const argType = Type.isMonomorphic(arg.type)
        ? arg.type
        : state.newTypeWithBound(Type.flexibleBound(arg.type));

      // Create a fresh type variable for the body type. This type will be
      // solved during unification.
      const bodyType = state.newType();

      // Unify the type of the callee with the function type we expect. This
      // should solve any unknown type variables.
      const error = unify(
        diagnostics,
        prefix,
        level,
        calleeType,
        Type.function_(argType, bodyType)
      );

      // Generalize the body type and return it.
      const type: Type = generalize(prefix, level, bodyType);

      // Decrement the level now that we’ve generalized the types we created at
      // this level.
      state.decrementLevel();

      // If there was an error during unification then we need to return an
      // error expression which will fail at runtime instead of an
      // call expression.
      return error === undefined
        ? Expression.Typed.call(type, callee, arg)
        : Expression.Typed.error(type, error);
    }

    // A binding infers a type for its value and introduces that value into
    // scope for the body. In a typical ML type system we would perform
    // generalization at let-bindings. However, in our MLF type system we keep
    // variables as polymorphic until they are applied. At which point we
    // instantiate them.
    case 'Binding': {
      // Infer bindings in a loop to avoid stack overflows from lots of
      // recursive function calls.
      const values = [];
      while (expression.description.kind === 'Binding') {
        const binding = expression.description;
        const value = inferExpression(diagnostics, scope, state, binding.value);
        scope.push(binding.name, value.type);
        values.push({name: binding.name, value});
        expression = binding.body;
      }

      // Infer the type of the body expression.
      const body = inferExpression(diagnostics, scope, state, expression);

      // Remove all bindings from scope and rebuild a typed binding expression.
      // Note that our bindings will be popped in the reverse order to which
      // they were pushed.
      let result = body;
      while (values.length !== 0) {
        scope.pop();
        const {name, value} = values.pop()!; // tslint:disable-line no-non-null-assertion
        result = Expression.Typed.binding(name, value, result);
      }

      return result;
    }

    // Runtime errors have the bottom type since they will crash at runtime.
    case 'Error':
      return Expression.Typed.error(Type.bottom, expression.description.error);

    default:
      const never: never = expression.description;
      return never;
  }
}

function generalize(
  prefix: Immutable.Map<string, TypeVariable>,
  level: number,
  type: MonomorphicType
): Type {
  function generalizeMonomorphicType(
    quantifications: Array<{readonly name: string; readonly bound: Bound}>,
    prefix: Immutable.Map<string, TypeVariable>,
    level: number,
    type: MonomorphicType
  ): MonomorphicType {
    // If the type’s level is smaller than our current level then all type
    // variables inside the type are still alive and should not be generalized.
    if (type.level.get() < level) return type;

    switch (type.description.kind) {
      case 'Variable': {
        const variable = prefix.get(type.description.name);
        if (variable === undefined) return type;
        const bound = variable.getBound();
        // TODO: Prevent double quantification.
        quantifications.push({name: type.description.name, bound});
        return type;
      }

      case 'Boolean':
      case 'Number':
      case 'String':
        return type;

      case 'Function': {
        const {param, body} = type.description;
        return Type.function_(
          generalizeMonomorphicType(quantifications, prefix, level, param),
          generalizeMonomorphicType(quantifications, prefix, level, body)
        );
      }

      default:
        const never: never = type.description;
        return never;
    }
  }

  // function generalizePolymorphicType(
  //   quantifications: Array<{readonly name: string; readonly bound: Bound}>,
  //   prefix: Immutable.Map<string, TypeVariable>,
  //   level: number,
  //   type: PolymorphicType
  // ): PolymorphicType {
  //   // If the type’s level is smaller than our current level then all type
  //   // variables inside the type are still alive and should not be generalized.
  //   if (type.level.get() < level) return type;

  //   switch (type.description.kind) {
  //     case 'Bottom':
  //       return type;
  //   }
  // }

  // Add the generalized type variables as quantifications.
  const quantifications: Array<{
    readonly name: string;
    readonly bound: Bound;
  }> = [];
  const newType = generalizeMonomorphicType(
    quantifications,
    prefix,
    level,
    type
  );
  return quantifications.reduceRight<PolymorphicType>(
    (type, {name, bound}) => Type.quantify(name, bound, type),
    newType
  );
}

export type InferError<T> =
  | UnifyError<T>
  | {
      readonly kind: 'UnboundVariable';
      readonly identifier: string;
    };
