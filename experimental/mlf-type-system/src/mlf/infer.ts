import {BindingMap} from './bindings';
import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {State} from './state';
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
          state.newType(),
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
      const bodyType = Type.isMonotype(body.type)
        ? body.type
        : state.newTypeWithBound(Type.flexibleBound(body.type));

      // Decrement the level before generalizing.
      state.decrementLevel();

      // Generalize dead type variables at this level before continuing on.
      const type = generalize(state, Type.function_(parameterType, bodyType));

      return Expression.Typed.function_(type, function_.param, body);
    }

    case 'Call': {
      const call = expression.description;

      // Increment the level. This is because we will generalize all dead type
      // variables at the end of type inference.
      state.incrementLevel();

      // Infer the types for our callee and argument inside of our type
      // variable quantification.
      const callee = inferExpression(diagnostics, scope, state, call.callee);
      const arg = inferExpression(diagnostics, scope, state, call.argument);

      // Convert the callee to a monomorphic type. If the callee type is
      // polymorphic then we need to add a type variable to our prefix.
      const calleeType = Type.isMonotype(callee.type)
        ? callee.type
        : state.newTypeWithBound(Type.flexibleBound(callee.type));

      // Convert the argument type to a monomorphic type. If the argument type
      // is polymorphic then we need to add a type variable to our prefix.
      const argType = Type.isMonotype(arg.type)
        ? arg.type
        : state.newTypeWithBound(Type.flexibleBound(arg.type));

      // Create a fresh type variable for the body type. This type will be
      // solved during unification.
      const bodyType = state.newType();

      // Unify the type of the callee with the function type we expect. This
      // should solve any unknown type variables.
      const error = unify(
        diagnostics,
        state,
        calleeType,
        Type.function_(argType, bodyType)
      );

      // Decrement the level before generalizing.
      state.decrementLevel();

      // Generalize the body type and return it.
      const type = generalize(state, bodyType);

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
      return Expression.Typed.error(
        state.newType(),
        expression.description.error
      );

    default:
      const never: never = expression.description;
      return never;
  }
}

/**
 * Turns type variables at a level larger then the current level into generic
 * quantified type bounds.
 */
function generalize(state: State, type: Polytype): Polytype {
  const quantify = new Set();
  generalize(quantify, state, type);
  // Quantify our type for every variable in the `quantify` set. The order of
  // the `quantify` set does matter! Since some type variables may have a
  // dependency on others.
  const quantifyReverse = Array(quantify.size);
  quantify.forEach((name, i) => {
    quantifyReverse[quantify.size - i - 1] = name;
  });
  quantifyReverse.forEach(name => {
    const {bound} = state.lookupType(name);
    type = Type.quantify(name, bound, type);
  });
  return type;

  // Iterate over every free type variable and determine if we need to
  // quantify it.
  function generalize(quantify: Set<string>, state: State, type: Polytype) {
    Type.forEachFreeVariable(type, name => {
      const {level, bound} = state.lookupType(name);
      // If we have a type variable with a level greater than our current
      // level then we need to quantify that type variable.
      if (level >= state.getLevel()) {
        // If `quantify` already contains this type variable then we don’t
        // need to add it again.
        if (!quantify.has(name)) {
          // Generalize the dead type variables in our bound as well. It is
          // important that we call this function before we add the variable
          // name to `quantify`. The order of type variables in `quantify`
          // does matter.
          if (bound.type !== undefined) generalize(quantify, state, bound.type);
          quantify.add(name);
        }
      }
    });
  }
}
