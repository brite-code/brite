/**
 * An identifier in a lambda calculus term.
 */
export type Identifier = string;

// Create unique, private, symbols in this file. Programmers outside this file
// won’t have access to these symbols and thus must stick to the functions
// we provide.
const VariableType = Symbol('Variable');
const AbstractionType = Symbol('Abstraction');
const ApplicationType = Symbol('Application');

/**
 * A [lambda calculus][1] term.
 *
 * [1]: https://en.wikipedia.org/wiki/Lambda_calculus
 */
export type Term = Variable | Abstraction | Application;

/**
 * A character or string representing a parameter or mathematical/logical value.
 */
export type Variable = {
  readonly type: typeof VariableType;
  readonly name: Identifier;
};

/**
 * Function definition. The variable becomes bound in the term.
 */
export type Abstraction = {
  readonly type: typeof AbstractionType;
  readonly parameter: Identifier;
  readonly body: Term;
};

/**
 * Applying a function to an argument.
 */
export type Application = {
  readonly type: typeof ApplicationType;
  readonly callee: Term;
  readonly argument: Term;
};

export function variable(name: Identifier): Variable {
  return {
    type: VariableType,
    name,
  };
}

export function abstraction(parameter: Identifier, body: Term): Abstraction {
  return {
    type: AbstractionType,
    parameter,
    body,
  };
}

export function application(callee: Term, argument: Term): Application {
  return {
    type: ApplicationType,
    callee,
    argument,
  };
}

/**
 * Parses a lambda calculus term.
 *
 * - `x` → `variable(x)`
 * - `(λx.y)` → `abstraction(x, y)`
 * - `(x y)` → `application(x, y)`
 */
export const parse = (() => {
  const identifierStart = /\w/;
  const identifierContinue = /[\w\d]/;
  const whitespace = /\s/;

  function parse(source: string): Term {
    const [term, lastStep] = parseTerm(source[Symbol.iterator]());
    if (!lastStep.done) {
      throw new Error(`Unexpected token "${lastStep.value}" expected ending`);
    }
    return term;
  }

  /**
   * Does the actual lambda calculus parsing on an iterator.
   */
  function parseTerm(
    iterator: Iterator<string>,
  ): [Term, IteratorResult<string>] {
    const step = parseToken(iterator);

    // Parse an abstraction.
    if (step.value === 'λ') {
      const [parameter, nextStep] = parseIdentifier(iterator);
      expectToken(nextStep, '.');
      const [body, nextNextStep] = parseTerm(iterator);
      return [abstraction(parameter, body), nextNextStep];
    }

    // Parses an unwrapped term.
    let [term, nextStep] = parseUnwrappedTerm(iterator, step);

    // Parse an application.
    while (!nextStep.done && nextStep.value !== ')') {
      const [argument, nextNextStep] = parseUnwrappedTerm(iterator, nextStep);
      term = application(term, argument);
      nextStep = nextNextStep;
    }

    return [term, nextStep];
  }

  /**
   * Parses an unwrapped term. Either a variable or a wrapped term.
   */
  function parseUnwrappedTerm(
    iterator: Iterator<string>,
    step: IteratorResult<string> = parseToken(iterator),
  ): [Term, IteratorResult<string>] {
    // Parse a term inside parentheses.
    if (step.value === '(') {
      const [term, nextStep] = parseTerm(iterator);
      expectToken(nextStep, ')');
      return [term, parseWhitespace(iterator)];
    }

    // Parse a variable.
    if (identifierStart.test(step.value)) {
      const [identifier, nextToken] = parseIdentifier(iterator, step);
      return [variable(identifier), nextToken];
    }

    // Throw an unexpected token error.
    const token = step.value;
    throw new Error(
      `Unexpected token "${token}" expected lambda calculus term`,
    );
  }

  /**
   * Parses an identifier from the iterator.
   *
   * Provide a second parameter when you needed to peek to determine whether or
   * not to parse an identifier.
   */
  function parseIdentifier(
    iterator: Iterator<string>,
    step: IteratorResult<string> = parseToken(iterator),
  ): [Identifier, IteratorResult<string>] {
    if (!identifierStart.test(step.value)) {
      throw new Error(`Unexpected token "${step.value}" expected identifier`);
    }
    let identifier = step.value;
    step = iterator.next();
    while (!step.done && identifierContinue.test(step.value)) {
      identifier += step.value;
      step = iterator.next();
    }
    if (!step.done && whitespace.test(step.value)) {
      step = parseWhitespace(iterator);
    }
    return [identifier, step];
  }

  /**
   * Parses all the whitespace in the iterator and returns the next step.
   */
  function parseWhitespace(iterator: Iterator<string>): IteratorResult<string> {
    let step = iterator.next();
    while (!step.done && whitespace.test(step.value)) {
      step = iterator.next();
    }
    return step;
  }

  /**
   * Returns the next "token" in the iterator. Throws if the iterator ends.
   */
  function parseToken(iterator: Iterator<string>): IteratorResult<string> {
    const step = parseWhitespace(iterator);
    if (step.done) throw new Error('Unexpected ending');
    return step;
  }

  /**
   * Throws an error if the token is not what was expected.
   */
  function expectToken(step: IteratorResult<string>, token: string) {
    if (step.done) throw new Error('Unexpected ending');
    if (step.value !== token) {
      throw new Error(
        `Unexpected token: "${step.value}" expected token "${token}"`,
      );
    }
  }

  return parse;
})();

/**
 * Simplifies a lambda term as much as possible using one simple rule:
 * `(λx.M) E → (M[x:=E])`. In other words, applying an abstraction substitues
 * the parameter name for the applied term.
 */
export const reduce = (() => {
  /**
   * Uses beta-reduction. Avoids alpha-conversion with variable shadowing.
   */
  function reduce(term: Term, variables: Map<Identifier, Term>): Term {
    switch (term.type) {
      // If there is a substitution for the variable then use it. Otherwise
      // leave the variable in place.
      case VariableType: {
        const variable = variables.get(term.name);
        return variable !== undefined ? variable : term;
      }
      // If we are reducing an abstraction that means it was not defined. So we
      // need to leave all variables in place. Duplicate variable names can
      // be shadowed.
      case AbstractionType: {
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
      case ApplicationType: {
        const callee = reduce(term.callee, variables);
        const argument = reduce(term.argument, variables);
        if (
          callee.type === AbstractionType &&
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

  return (term: Term) => reduce(term, new Map());
})();
