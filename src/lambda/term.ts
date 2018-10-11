/**
 * The type of a term. We want to keep this private to the `lambda` directory.
 */
export const enum TermType {
  Variable = 'Variable',
  Abstraction = 'Abstraction',
  Application = 'Application',
  Native = 'Native',
}

/**
 * A [lambda calculus][1] term encoded with [De Bruijn indexing][2].
 *
 * [1]: https://en.wikipedia.org/wiki/Lambda_calculus
 * [2]: https://en.wikipedia.org/wiki/De_Bruijn_index
 */
export type Term = VariableTerm | AbstractionTerm | ApplicationTerm;

/**
 * A character or string representing a parameter or mathematical/logical value.
 */
export type VariableTerm = {
  readonly type: TermType.Variable;
  readonly index: number;
};

/**
 * Function definition. The variable becomes bound in the term.
 */
export type AbstractionTerm = {
  readonly type: TermType.Abstraction;
  readonly parameter: Binding;
  readonly body: Term;
};

/**
 * Information about a variable binding.
 */
export type Binding = {
  readonly name: string;
};

/**
 * Applying a function to an argument.
 *
 * Applications are evaluated strictly. First evaluate the callee and then
 * the argument.
 */
export type ApplicationTerm = {
  readonly type: TermType.Application;
  readonly callee: Term;
  readonly argument: Term;
};

/**
 * Creates a variable term.
 */
export function variable(index: number): VariableTerm {
  return {
    type: TermType.Variable,
    index,
  };
}

/**
 * Creates an abstraction term.
 */
export function abstraction(parameter: string, body: Term): AbstractionTerm {
  return {
    type: TermType.Abstraction,
    parameter: {name: parameter},
    body,
  };
}

/**
 * Creates an application term.
 */
export function application(callee: Term, argument: Term): ApplicationTerm {
  return {
    type: TermType.Application,
    callee,
    argument,
  };
}

/**
 * Creates a binding term.
 *
 * A binding term `let x = z in y` is equal to the standard lambda calculus
 * term `(λx.y) z`. A binding extension like this enables us to write more
 * fluent lambda calculus programs.
 *
 * Binding terms are syntax sugar over the pure lambda calculus.
 */
export function binding(name: string, value: Term, body: Term): Term {
  return application(abstraction(name, body), value);
}
