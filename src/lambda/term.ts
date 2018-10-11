import * as t from '@babel/types';

/**
 * The type of a term. We want to keep this private to the `lambda` directory.
 */
export const enum TermType {
  Variable = 'Variable',
  Abstraction = 'Abstraction',
  Application = 'Application',
  Conditional = 'Conditional',
  Native = 'Native',
}

/**
 * A [lambda calculus][1] term encoded with [De Bruijn indexing][2].
 *
 * [1]: https://en.wikipedia.org/wiki/Lambda_calculus
 * [2]: https://en.wikipedia.org/wiki/De_Bruijn_index
 */
export type Term =
  | VariableTerm
  | AbstractionTerm
  | ApplicationTerm
  | ConditionalTerm
  | NativeTerm;

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
 * A conditional abstraction in pure lambda calculus with [church encoded
 * booleans][1] might look like `λc t f.c t f`. Where the church encoded
 * booleans are `λt f.t` for true and `λt f.f` for false. So the program:
 *
 * ```
 * let true = λt f.t in
 * let false = λt f.f in
 * let if = λc t f.c t f in
 * if true a b
 * ```
 *
 * Evaluates to `a` as expected. However, our lambda calculus is [strictly
 * evaluated][2]. This means that `a` and `b` have already been evaluated. If
 * `b` contains some expensive computation it will always be evaluated even
 * though the result is never used.
 *
 * So we add a special term for conditionals. The consequent and alternate will
 * only be evaluated if they are needed.
 *
 * [1]: https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans
 * [2]: https://en.wikipedia.org/wiki/Eager_evaluation
 */
export type ConditionalTerm = {
  readonly type: TermType.Conditional;
  readonly test: Term;
  readonly consequent: Term;
  readonly alternate: Term;
};

/**
 * A native term serializes some custom native code for all target platforms.
 *
 * It takes an array of input terms. Corresponding serialized values for these
 * terms are provided to the `serialize()` function.
 */
export type NativeTerm = {
  readonly type: TermType.Native;
  readonly inputs: ReadonlyArray<Term>;
  readonly serializers: NativeTermSerializers;
};

/**
 * The serializer functions for native terms. We have a serializer for
 * each back-end.
 */
export type NativeTermSerializers = {
  readonly js: (inputs: ReadonlyArray<t.Expression>) => t.Expression;
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

/**
 * Creates a conditional expression.
 */
export function conditional(
  test: Term,
  consequent: Term,
  alternate: Term,
): Term {
  return {
    type: TermType.Conditional,
    test,
    consequent,
    alternate,
  };
}

/**
 * Creates a native term.
 *
 * There is no such equivalent in lambda calculus. This is an extension for our
 * language to be practically useful.
 */
export function native(
  inputs: ReadonlyArray<Term>,
  serializers: NativeTermSerializers,
): NativeTerm {
  return {
    type: TermType.Native,
    inputs,
    serializers,
  };
}
