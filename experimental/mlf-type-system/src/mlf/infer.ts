import * as Immutable from 'immutable';

import {DerivableValue} from '../utils/derive';

import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {Bound, MonomorphicType, PolymorphicType, Type} from './type';
import {TypeVariable, UnifyError, unify} from './unify';

let id = 1;

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
  const level = 0;
  return inferExpression(
    diagnostics,
    Immutable.Map(),
    Immutable.Map(context),
    level,
    expression
  );
}

function inferExpression<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  prefix: Immutable.Map<string, TypeVariable>,
  context: Immutable.Map<string, Type>,
  level: number,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, Type> {
  switch (expression.description.kind) {
    // A variable references some value in the current context. The
    // variable’s type is that value’s type. If no such value exists then we
    // report an unbound variable.
    case 'Variable': {
      const variable = expression.description;
      const identifier = variable.name;
      const type = context.get(identifier);
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
      const fun = expression.description;

      // Increment the level. This is because we will generalize all dead type
      // variables at the end of type inference.
      level += 1;

      // Introduce a new type variable for the function parameter. Through the
      // inference of our function body we should solve this to a proper type.
      const {prefix: newPrefix, type: parameterType} = newTypeVariable(
        prefix,
        level,
        Type.flexibleBound(Type.bottom)
      );
      prefix = newPrefix;

      // Infer our function body type. Introducing the variable we just defined
      // into scope.
      const body = inferExpression(
        diagnostics,
        prefix,
        context.set(fun.param, parameterType),
        level,
        fun.body
      );

      // If the type of our body is polymorphic then we need to quantify the
      // type of our function by our body type.
      let type: MonomorphicType;
      if (Type.isMonomorphic(body.type)) {
        type = Type.function_(parameterType, body.type);
      } else {
        const {prefix: newPrefix, type: bodyType} = newTypeVariable(
          prefix,
          level,
          Type.flexibleBound(body.type)
        );
        prefix = newPrefix;
        type = bodyType;
      }

      // Generalize dead type variables at this level before continuing on.
      const newType = generalize(prefix, level, type);

      return Expression.Typed.function_(newType, fun.param, body);
    }

    case 'Call': {
      const call = expression.description;

      // Increment the level. This is because we will generalize all dead type
      // variables at the end of type inference.
      level += 1;

      // Infer the types for our callee and argument inside of our type
      // variable quantification.
      const callee = inferExpression(
        diagnostics,
        prefix,
        context,
        level,
        call.callee
      );
      const argument = inferExpression(
        diagnostics,
        prefix,
        context,
        level,
        call.arg
      );

      // Convert the callee to a monomorphic type. If the callee type is
      // polymorphic then we need to add a type variable to our prefix.
      let calleeType: MonomorphicType;
      if (Type.isMonomorphic(callee.type)) {
        calleeType = callee.type;
      } else {
        const {prefix: newPrefix, type} = newTypeVariable(
          prefix,
          level,
          Type.flexibleBound(callee.type)
        );
        prefix = newPrefix;
        calleeType = type;
      }

      // Convert the argument type to a monomorphic type. If the argument type
      // is polymorphic then we need to add a type variable to our prefix.
      let argumentType: MonomorphicType;
      if (Type.isMonomorphic(argument.type)) {
        argumentType = argument.type;
      } else {
        const {prefix: newPrefix, type} = newTypeVariable(
          prefix,
          level,
          Type.flexibleBound(argument.type)
        );
        prefix = newPrefix;
        argumentType = type;
      }

      // Create a fresh type variable for the body type. This type will be
      // solved during unification.
      const {prefix: newPrefix, type: bodyType} = newTypeVariable(
        prefix,
        level,
        Type.flexibleBound(Type.bottom)
      );
      prefix = newPrefix;

      // Unify the type of the callee with the function type we expect. This
      // should solve any unknown type variables.
      const error = unify(
        diagnostics,
        prefix,
        level,
        calleeType,
        Type.function_(argumentType, bodyType)
      );

      // Generalize the body type and return it.
      const type: Type = generalize(prefix, level, bodyType);

      // If there was an error during unification then we need to return an
      // error expression which will fail at runtime instead of an
      // call expression.
      return error === undefined
        ? Expression.Typed.call(type, callee, argument)
        : Expression.Typed.error(type, error);
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
        level,
        binding.value
      );
      const body = inferExpression(
        diagnostics,
        prefix,
        context.set(binding.name, value.type),
        level,
        binding.body
      );
      return Expression.Typed.binding(binding.name, value, body);
    }

    // Runtime errors have the bottom type since they will crash at runtime.
    case 'Error':
      return Expression.Typed.error(Type.bottom, expression.description.error);

    default:
      const never: never = expression.description;
      return never;
  }
}

function newTypeVariable(
  prefix: Immutable.Map<string, TypeVariable>,
  currentLevel: number,
  bound: Bound
): {
  readonly prefix: Immutable.Map<string, TypeVariable>;
  readonly type: MonomorphicType;
} {
  const identifier = `$${id++}`;
  const level = new DerivableValue(currentLevel);
  const type = Type.variableWithLevel(identifier, level);
  const newPrefix = prefix.set(
    identifier,
    new TypeVariable(level, prefix, bound)
  );
  return {prefix: newPrefix, type};
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
