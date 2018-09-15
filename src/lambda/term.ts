/**
 * An identifier in a lambda calculus term.
 */
export type Identifier = string;

/**
 * The type of a term. We want to keep this private to the `lambda` directory.
 */
export const enum TermType {
  Variable = 'Variable',
  Abstraction = 'Abstraction',
  Application = 'Application',
}

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
  readonly type: TermType.Variable;
  readonly name: Identifier;
};

/**
 * Function definition. The variable becomes bound in the term.
 */
export type Abstraction = {
  readonly type: TermType.Abstraction;
  readonly parameter: Identifier;
  readonly body: Term;
};

/**
 * Applying a function to an argument.
 */
export type Application = {
  readonly type: TermType.Application;
  readonly callee: Term;
  readonly argument: Term;
};

export function variable(name: Identifier): Variable {
  return {
    type: TermType.Variable,
    name,
  };
}

export function abstraction(parameter: Identifier, body: Term): Abstraction {
  return {
    type: TermType.Abstraction,
    parameter,
    body,
  };
}

export function application(callee: Term, argument: Term): Application {
  return {
    type: TermType.Application,
    callee,
    argument,
  };
}
