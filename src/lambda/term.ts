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
 * Parameterized by the native serialization term. Since one may embed native
 * terms with custom serialization rules.
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
 */
export type ApplicationTerm<T> = {
  readonly type: TermType.Application;
  readonly callee: Term<T>;
  readonly argument: Term<T>;
};

/**
 * A native term serializes some custom native code for the target platform.
 *
 * It takes an array of “free” variables. Corresponding terms for these
 * variables are provided to the `serialize()` function.
 */
export type NativeTerm<T> = {
  readonly type: TermType.Native;
  readonly variables: ReadonlyArray<Identifier>;
  readonly serialize: (variables: ReadonlyArray<T>) => T;
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
 * Creates a native term.
 */
export function native<T>(
  variables: ReadonlyArray<Identifier>,
  serialize: (variables: ReadonlyArray<T>) => T,
): Term<T> {
  return {
    type: TermType.Native,
    variables,
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
      for (const name of term.variables) {
        if (!bound.has(name)) free.add(name);
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
