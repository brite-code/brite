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
  Native = 'Native',
}

/**
 * A [lambda calculus][1] term.
 *
 * Parameterized by the native serialization result type. Since one may embed
 * native terms with custom serialization rules.
 *
 * [1]: https://en.wikipedia.org/wiki/Lambda_calculus
 */
export type Term<T> =
  | VariableTerm
  | AbstractionTerm<T>
  | ApplicationTerm<T>
  | NativeTerm<T>;

/**
 * A character or string representing a parameter or mathematical/logical value.
 */
export type VariableTerm = {
  readonly type: TermType.Variable;
  readonly name: Identifier;
};

/**
 * Function definition. The variable becomes bound in the term.
 */
export type AbstractionTerm<T> = {
  readonly type: TermType.Abstraction;
  readonly parameter: Identifier;
  readonly body: Term<T>;
};

/**
 * Applying a function to an argument.
 *
 * Applications are evaluated strictly. First evaluate the callee and then
 * the argument.
 */
export type ApplicationTerm<T> = {
  readonly type: TermType.Application;
  readonly callee: Term<T>;
  readonly argument: Term<T>;
};

/**
 * A native term serializes some custom native code for the target platform.
 *
 * It takes an array of input terms. Corresponding serialized values for these
 * terms are provided to the `serialize()` function.
 */
export type NativeTerm<T> = {
  readonly type: TermType.Native;
  readonly inputs: ReadonlyArray<Term<T>>;
  readonly serialize: (inputs: ReadonlyArray<T>) => T;
};

/**
 * Creates a variable term.
 */
export function variable(name: Identifier): VariableTerm {
  return {
    type: TermType.Variable,
    name,
  };
}

/**
 * Creates an abstraction term.
 */
export function abstraction<T>(
  parameter: Identifier,
  body: Term<T>,
): AbstractionTerm<T> {
  return {
    type: TermType.Abstraction,
    parameter,
    body,
  };
}

/**
 * Creates an application term.
 */
export function application<T>(
  callee: Term<T>,
  argument: Term<T>,
): ApplicationTerm<T> {
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
export function binding<T>(
  name: Identifier,
  value: Term<T>,
  body: Term<T>,
): Term<T> {
  return application(abstraction(name, body), value);
}

/**
 * Creates a native term.
 *
 * There is no such equivalent in lambda calculus. This is an extension for our
 * language to be practically useful.
 */
export function native<T>(
  inputs: ReadonlyArray<Term<T>>,
  serialize: (inputs: ReadonlyArray<T>) => T,
): Term<T> {
  return {
    type: TermType.Native,
    inputs,
    serialize,
  };
}

/**
 * Recursively finds all the free variables in `term`. If `term` has a free
 * variable that does not exist in the `bound` set then it is added to the
 * `free` set.
 */
function getFreeVariables<T>(
  term: Term<T>,
  bound: Set<Identifier>,
  free: Set<Identifier>,
) {
  switch (term.type) {
    case TermType.Variable: {
      if (!bound.has(term.name)) free.add(term.name);
      break;
    }
    case TermType.Abstraction: {
      if (!bound.has(term.parameter)) {
        bound.add(term.parameter);
        getFreeVariables(term.body, bound, free);
        bound.delete(term.parameter);
      } else {
        getFreeVariables(term.body, bound, free);
      }
      break;
    }
    case TermType.Application: {
      getFreeVariables(term.callee, bound, free);
      getFreeVariables(term.argument, bound, free);
      break;
    }
    case TermType.Native: {
      for (const variable of term.inputs) {
        getFreeVariables(variable, bound, free);
      }
      break;
    }
  }
}

/**
 * Returns all the free variables in the provided term.
 */
function getFreeVariablesStart<T>(term: Term<T>): ReadonlySet<Identifier> {
  const bound = new Set();
  const free = new Set();
  getFreeVariables(term, bound, free);
  return free;
}

export {getFreeVariablesStart as getFreeVariables};
