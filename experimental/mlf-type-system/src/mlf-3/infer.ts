import {BindingMap} from './bindings';
import {Context} from './context';
import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {MonomorphicType, PolymorphicType, Type} from './type';

export type InferError<T> =
  | T
  | {
      readonly kind: 'UnboundVariable';
      readonly identifier: string;
    };

function inferExpression<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  context: Context,
  scope: BindingMap<string, PolymorphicType>,
  level: number,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, PolymorphicType> {
  switch (expression.description.kind) {
    // A variable references some value in the current context. The
    // variable’s type is that value’s type. If no such value exists then we
    // report an unbound variable.
    case 'Variable': {
      const variable = expression.description;
      const identifier = variable.identifier;
      const type = scope.get(identifier);
      if (type !== undefined) {
        return Expression.Typed.variable(type, identifier);
      } else {
        return Expression.Typed.error(
          context.newType(),
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

    // A binding infers a type for its value and introduces that value into
    // scope for the body. In a typical ML type system we would perform
    // generalization at let-bindings. However, in our MLF type system we keep
    // variables as polymorphic until they are applied. At which point we
    // instantiate them.
    case 'Binding': {
      const binding = expression.description;
      const value = inferExpression(diagnostics, context, scope, binding.value);
      scope.push(binding.binding, value.type);
      const body = inferExpression(diagnostics, context, scope, binding.body);
      scope.pop();
      return Expression.Typed.binding(binding.binding, value, body);
    }

    case 'Function': {
      const function_ = expression.description;

      level += 1;

      const parameterType = context.newType();

      scope.push(function_.parameter, parameterType);
      const body = inferExpression(diagnostics, context, scope, function_.body);
      scope.pop();

      let bodyType: MonomorphicType;
      if (Type.isMonomorphic(body.type)) {
        bodyType = body.type;
      } else {
        bodyType = context.newTypeWithBound({
          kind: 'flexible',
          type: body.type,
        });
      }

      Type.function_(parameterType, bodyType);

      throw new Error('unimplemented');
    }

    // Runtime errors have the bottom type since they will crash at runtime.
    case 'Error':
      return Expression.Typed.error(
        context.newType(),
        expression.description.error
      );

    default:
      const never: never = expression.description;
      return never;
  }
}

function instantiate(context: Context, type: PolymorphicType): PolymorphicType {
  return instantiatePolymorphicType(context, new BindingMap(), type);

  function instantiatePolymorphicType(
    context: Context,
    scope: BindingMap<string, MonomorphicType>,
    type: PolymorphicType
  ): PolymorphicType {
    switch (type.kind) {
      case 'Quantify': {
        let pops = 0;
        // Decompose quantifications in a loop to guard against a
        // stack overflow.
        while (type.kind === 'Quantify') {
          pops++;
          const bound = {
            kind: type.bound.kind,
            type: instantiatePolymorphicType(context, scope, type.bound.type),
          };
          scope.push(type.name, context.newTypeWithBound(bound));
          type = type.body;
        }
        const newType = instantiatePolymorphicType(context, scope, type);
        for (let i = 0; i < pops; i++) scope.pop();
        return newType;
      }

      case 'Bottom':
        return type;

      default:
        return instantiateMonomorphicType(context, scope, type);
    }
  }

  function instantiateMonomorphicType(
    context: Context,
    scope: BindingMap<string, MonomorphicType>,
    type: MonomorphicType
  ): MonomorphicType {
    switch (type.kind) {
      case 'Variable': {
        const newType = scope.get(type.name);
        return newType || type;
      }

      case 'Function': {
        const param = instantiateMonomorphicType(context, scope, type.param);
        const body = instantiateMonomorphicType(context, scope, type.body);
        return Type.function_(param, body);
      }

      case 'Boolean':
      case 'Number':
      case 'String':
      case 'Constructed':
        return type;

      default:
        const never: never = type;
        return never;
    }
  }
}
