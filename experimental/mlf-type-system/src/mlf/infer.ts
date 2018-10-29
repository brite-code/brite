import {BindingMap} from '../utils/bindings';

import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {Prefix} from './prefix';
import {Polytype, Type} from './type';
import {UnifyError, unify} from './unify';

export type InferError<T> =
  | UnifyError<T>
  | {
      readonly kind: 'UnboundVariable';
      readonly identifier: string;
    };

/**
 * Infers the type of an untyped expression. Detects type incompatibilities and
 * converts malformed expressions into error expressions so they will crash
 * at runtime.
 */
export function infer<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  variables: Iterable<[string, Type]>,
  expression: Expression<Diagnostic>
): Expression<InferError<Diagnostic>, Type> {
  const scope = new BindingMap(variables);
  const prefix = new Prefix();
  const result = inferExpression(diagnostics, scope, prefix, expression);
  if (!prefix.isEmpty()) {
    throw new Error('Not all type variables were cleaned up from the prefix.');
  }
  return result;
}

function inferExpression<Diagnostic>(
  diagnostics: Diagnostics<InferError<Diagnostic>>,
  scope: BindingMap<string, Type>,
  prefix: Prefix,
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
        // Handle the error case by calling `inferExpression()` again so we
        // don’t duplicate logic.
        return inferExpression(
          diagnostics,
          scope,
          prefix,
          Expression.error(
            diagnostics.report({
              kind: 'UnboundVariable',
              identifier,
            })
          )
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
      prefix.incrementLevel();

      // Introduce a new type variable for the function parameter. Through the
      // inference of our function body we should solve this to a proper type.
      const parameterType = prefix.fresh();

      // Infer our function body type. Introducing the variable we just defined
      // into scope.
      scope.push(function_.parameter, parameterType);
      const body = inferExpression(diagnostics, scope, prefix, function_.body);
      scope.pop();

      // If the type of our body is polymorphic then we need to quantify the
      // type of our function by our body type.
      const bodyType = Type.isMonotype(body.type)
        ? body.type
        : prefix.freshWithBound(Type.flexibleBound(body.type));

      // Generalize the function type. We must generalize before deallocating
      // all the type variables at this level.
      const type = generalize(prefix, Type.function_(parameterType, bodyType));

      // Decrement the level after generalizing.
      prefix.decrementLevel();

      return Expression.Typed.function_(type, function_.parameter, body);
    }

    case 'Call': {
      const call = expression.description;

      // Increment the level. This is because we will generalize all dead type
      // variables at the end of type inference.
      prefix.incrementLevel();

      // Infer the types for our callee and argument inside of our type
      // variable quantification.
      const callee = inferExpression(diagnostics, scope, prefix, call.callee);
      const argument = inferExpression(
        diagnostics,
        scope,
        prefix,
        call.argument
      );

      // Convert the callee to a monomorphic type. If the callee type is
      // polymorphic then we need to add a type variable to our prefix.
      const calleeType = Type.isMonotype(callee.type)
        ? callee.type
        : prefix.freshWithBound(Type.flexibleBound(callee.type));

      // Convert the argument type to a monomorphic type. If the argument type
      // is polymorphic then we need to add a type variable to our prefix.
      const argumentType = Type.isMonotype(argument.type)
        ? argument.type
        : prefix.freshWithBound(Type.flexibleBound(argument.type));

      // Create a fresh type variable for the body type. This type will be
      // solved during unification.
      const bodyType = prefix.fresh();

      // Unify the type of the callee with the function type we expect. This
      // should solve any unknown type variables.
      const error = unify(
        diagnostics,
        prefix,
        calleeType,
        Type.function_(argumentType, bodyType)
      );

      // Generalize the body type and return it. We must generalize before
      // deallocating all the type variables at this level.
      const type = generalize(prefix, bodyType);

      // Decrement the level after generalizing.
      prefix.decrementLevel();

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
      // Infer bindings in a loop to avoid stack overflows from lots of
      // recursive function calls.
      const values = [];
      while (expression.description.kind === 'Binding') {
        const binding = expression.description;
        const value = inferExpression(
          diagnostics,
          scope,
          prefix,
          binding.value
        );
        scope.push(binding.name, value.type);
        values.push({name: binding.name, value});
        expression = binding.body;
      }

      // Infer the type of the body expression.
      const body = inferExpression(diagnostics, scope, prefix, expression);

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

    // An annotation allows us to expect a type for a given expression. We
    // require type annotations for a function argument to be used
    // polymorphically. For example the auto function needs an annotation for
    // the function parameter: λx.x x
    case 'Annotation': {
      const annotation = expression.description;

      // Increment the level before creating new type variables.
      prefix.incrementLevel();

      // Infer the annotation expression.
      const value = inferExpression(
        diagnostics,
        scope,
        prefix,
        annotation.value
      );

      // Produce a monotype for the value type.
      const valueType = Type.isMonotype(value.type)
        ? value.type
        : prefix.freshWithBound(Type.flexibleBound(value.type));

      // Produce a monotype for the annotation type.
      const annotationType = Type.isMonotype(annotation.type)
        ? annotation.type
        : prefix.freshWithBound(Type.rigidBound(annotation.type));

      // Unify the value and annotation types.
      const error = unify(diagnostics, prefix, valueType, annotationType);

      // Decrement the level and destroy the type variables we created.
      prefix.decrementLevel();

      // If unification was not a success then we return an error expression.
      // The error expression is still of the annotation type, however.
      return error === undefined
        ? Expression.Typed.annotation(value, annotation.type)
        : Expression.Typed.error(annotation.type, error);
    }

    // Runtime errors have the bottom type since they will crash at runtime.
    case 'Error':
      return Expression.Typed.error(Type.bottom, expression.description.error);

    default:
      const never: never = expression.description;
      return never;
  }
}

/**
 * Turns type variables at a level larger then the current level into generic
 * quantified type bounds.
 */
function generalize(prefix: Prefix, type: Polytype): Polytype {
  const quantify = new Set<string>();
  generalize(quantify, prefix, type);
  // Quantify our type for every variable in the `quantify` set. The order of
  // the `quantify` set does matter! Since some type variables may have a
  // dependency on others.
  const quantifyReverse = Array(quantify.size);
  let i = 0;
  for (const name of quantify) {
    quantifyReverse[quantify.size - i++ - 1] = name;
  }
  for (const name of quantifyReverse) {
    const {bound} = prefix.lookup(name);
    type = Type.quantify(name, bound, type);
  }
  return type;

  // Iterate over every free type variable and determine if we need to
  // quantify it.
  function generalize(quantify: Set<string>, prefix: Prefix, type: Polytype) {
    for (const name of Type.getFreeVariables(type)) {
      const {level, bound} = prefix.lookup(name);
      // If we have a type variable with a level greater than our current
      // level then we need to quantify that type variable.
      if (level >= prefix.getLevel()) {
        // If `quantify` already contains this type variable then we don’t
        // need to add it again.
        if (!quantify.has(name)) {
          // Generalize the dead type variables in our bound as well.
          generalize(quantify, prefix, bound.type);
          // It is important that we add the name to `quantify` _after_ we
          // generalize the bound type. The order of variables in `quantify`
          // does matter.
          quantify.add(name);
        }
      }
    }
  }
}
