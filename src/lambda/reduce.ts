import {Identifier, Term, TermType, application, abstraction} from './term';

/**
 * Uses beta-reduction. Avoids alpha-conversion with variable shadowing.
 */
function reduce(term: Term, variables: Map<Identifier, Term>): Term {
  switch (term.type) {
    // If there is a substitution for the variable then use it. Otherwise
    // leave the variable in place.
    case TermType.Variable: {
      const variable = variables.get(term.name);
      return variable !== undefined ? variable : term;
    }
    // If we are reducing an abstraction that means it was not defined. So we
    // need to leave all variables in place. Duplicate variable names can
    // be shadowed.
    case TermType.Abstraction: {
      const parameter = term.parameter;
      let body = term.body;
      const variable = variables.get(parameter);
      if (variable !== undefined) {
        variables.delete(parameter);
        body = reduce(body, variables);
        variables.set(parameter, variable);
      } else {
        body = reduce(body, variables);
      }
      return body !== term.body ? abstraction(parameter, body) : term;
    }
    // If applying an abstraction then substitute the argument for the
    // parameter. Otherwise leave the application as it is.
    case TermType.Application: {
      const callee = reduce(term.callee, variables);
      const argument = reduce(term.argument, variables);
      if (
        callee.type === TermType.Abstraction &&
        // Cheap trick to avoid infinite unrolling of the fixpoint combinator.
        // Is this correct in general?
        callee !== argument
      ) {
        const parameter = callee.parameter;
        const variable = variables.get(parameter);
        variables.set(parameter, argument);
        const body = reduce(callee.body, variables);
        if (variable !== undefined) {
          variables.set(parameter, variable);
        } else {
          variables.delete(parameter);
        }
        return body;
      } else {
        return callee !== term.callee || argument !== term.argument
          ? application(callee, argument)
          : term;
      }
    }
  }
}

/**
 * Simplifies a lambda term as much as possible using one simple rule:
 * `(λx.M) E → (M[x:=E])`. In other words, applying an abstraction substitues
 * the parameter name for the applied term.
 */
function reduceStart(term: Term) {
  return reduce(term, new Map());
}

export {reduceStart as reduce};
