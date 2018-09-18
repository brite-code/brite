import {
  Term,
  VariableTerm,
  AbstractionTerm,
  ApplicationTerm,
  NativeTerm,
  TermType,
  abstraction,
  application,
  native,
} from './term';

/**
 * - `undefined`: No action.
 * - `false`: Skip visiting this term.
 * - `Term`: Replace the term with this new term. The new term will be
 *   recursively visited. We will not call the corresponding “leave” function
 *   for the old term.
 */
export type VisitorEnter<T> = void | false | Term<T>;

/**
 * - `undefined`: No action.
 * - `Term`: Replace the term with this new term. The new term will not be
 *   recursively visited.
 */
export type VisitorLeave<T> = void | Term<T>;

/**
 * Functions which are called while we visit a lambda calculus term.
 */
export type Visitor<T> = {
  readonly enterVariable: (term: VariableTerm) => VisitorEnter<T>;
  readonly leaveVariable: (term: VariableTerm) => VisitorLeave<T>;
  readonly enterAbstraction: (term: AbstractionTerm<T>) => VisitorEnter<T>;
  readonly leaveAbstraction: (term: AbstractionTerm<T>) => VisitorLeave<T>;
  readonly enterApplication: (term: ApplicationTerm<T>) => VisitorEnter<T>;
  readonly leaveApplication: (term: ApplicationTerm<T>) => VisitorLeave<T>;
  readonly enterNative: (term: NativeTerm<T>) => VisitorEnter<T>;
  readonly leaveNative: (term: NativeTerm<T>) => VisitorLeave<T>;
};

/**
 * A visitor that does nothing.
 */
export const noopVisitor: Visitor<any> = {
  enterVariable: () => {},
  leaveVariable: () => {},
  enterAbstraction: () => {},
  leaveAbstraction: () => {},
  enterApplication: () => {},
  leaveApplication: () => {},
  enterNative: () => {},
  leaveNative: () => {},
};

/**
 * A single frame object for our stack. We push/pop these to the stack and then
 * operate on them.
 */
type StackFrame<T> = {
  state: number;
  term: Term<T>;
  args: Array<Term<T>>;
  updated: boolean;
};

/**
 * Recursively visits a term. Calling the visitor callbacks all along the way.
 */
export function visit<T>(initialTerm: Term<T>, callbacks: Visitor<T>): Term<T> {
  // Implemented without recursion so that we don’t easily hit stack overflow
  // limits. However, this does make the implementation just a bit convoluted.

  let returnTerm: Term<T> = initialTerm;
  const stack: Array<StackFrame<T>> = [
    {state: 0, term: initialTerm, args: [], updated: false},
  ];
  while (stack.length !== 0) {
    let enter: VisitorEnter<T> = undefined;
    let leaving = false;
    let leave: VisitorLeave<T> = undefined;
    let {state, term, args, updated} = stack.pop()!;

    if (state > 1 && args[state - 2] !== returnTerm) {
      args[state - 2] = returnTerm;
      updated = true;
    }

    switch (term.type) {
      case TermType.Variable: {
        switch (state) {
          case 0: {
            enter = callbacks.enterVariable(term);
            break;
          }
          case 1: {
            leave = callbacks.leaveVariable(term);
            leaving = true;
            break;
          }
        }
        break;
      }
      case TermType.Abstraction: {
        switch (state) {
          case 0: {
            enter = callbacks.enterAbstraction(term);
            break;
          }
          case 1: {
            args.push(term.body);
            break;
          }
          case 2: {
            if (updated) {
              const [body] = args;
              term = abstraction(term.parameter, body);
            }
            leave = callbacks.leaveAbstraction(term);
            leaving = true;
            break;
          }
        }
        break;
      }
      case TermType.Application: {
        switch (state) {
          case 0: {
            enter = callbacks.enterApplication(term);
            break;
          }
          case 1: {
            args.push(term.callee);
            break;
          }
          case 2: {
            args.push(term.argument);
            break;
          }
          case 3: {
            if (updated) {
              const [callee, argument] = args;
              term = application(callee, argument);
            }
            leave = callbacks.leaveApplication(term);
            leaving = true;
            break;
          }
        }
        break;
      }
      case TermType.Native: {
        if (state === 0) {
          enter = callbacks.enterNative(term);
        } else if (state === term.inputs.length + 1) {
          if (updated) {
            term = native(args, term.serialize);
          }
          leave = callbacks.leaveNative(term);
          leaving = true;
        } else {
          args.push(term.inputs[state - 1]);
        }
      }
    }

    if (state === 0) {
      if (enter === false) {
        returnTerm = term;
      } else if (enter !== undefined) {
        stack.push({
          state: 0,
          term: enter,
          args: [],
          updated: false,
        });
      } else {
        stack.push({
          state: 1,
          term,
          args: [],
          updated: false,
        });
      }
    } else if (!leaving) {
      stack.push({
        state: state + 1,
        term,
        args,
        updated,
      });
      stack.push({
        state: 0,
        term: args[args.length - 1],
        args: [],
        updated: false,
      });
    } else {
      if (leave !== undefined) {
        returnTerm = leave;
      } else {
        returnTerm = term;
      }
    }
  }
  return returnTerm;
}
