import * as t from '@babel/types';
import {Identifier} from '../term';

/**
 * A stack of JavaScript scopes. Manages things like identifier collision and
 * scope statements.
 */
export class ScopeStack {
  /**
   * Creates a new scope stack. At the very end returns the statements from the
   * root scope.
   */
  static create<T>(f: (scope: ScopeStack) => T): [T, Array<t.Statement>] {
    const scopeStack = new ScopeStack();
    const result = f(scopeStack);
    const scope = scopeStack.stack.pop()!;
    return [result, scope.statements];
  }

  /**
   * The current stack of JavaScript scopes. Will always have at least
   * one scope.
   */
  private readonly stack: Array<Scope> = [newScope()];

  private constructor() {}

  /**
   * Nests the call to `f` in a new scope.
   */
  nest<T>(f: () => T): [T, Array<t.Statement>] {
    this.stack.push(newScope());
    const result = f();
    const scope = this.stack.pop()!;
    return [result, scope.statements];
  }

  /**
   * Declares a variable in our scope and returns an escaped, deduplicated,
   * JavaScript identifier.
   */
  declareVariable(identifier: Identifier): t.Identifier {
    const dedupe = this.identifierDedupeNumber(identifier) + 1;
    this.currentScope().identifierDedupe.set(identifier, dedupe);
    const jsIdentifier =
      dedupe === 1
        ? t.identifier(identifier)
        : t.identifier(`${identifier}$${dedupe}`);
    this.currentScope().variables.set(identifier, {identifier: jsIdentifier});
    return jsIdentifier;
  }

  /**
   * Resolves a variable in our scope stack and returns its escaped identifier.
   */
  resolveVariable(identifier: Identifier): t.Identifier | undefined {
    for (let i = this.stack.length - 1; i >= 0; i--) {
      const variable = this.stack[i].variables.get(identifier);
      if (variable !== undefined) {
        return variable.identifier;
      }
    }
    return undefined;
  }

  /**
   * Adds a statement to the current scope.
   */
  addStatement(statement: t.Statement) {
    this.currentScope().statements.push(statement);
  }

  /**
   * Returns the current scope. One is always guaranteed to exist.
   */
  private currentScope(): Scope {
    return this.stack[this.stack.length - 1]!;
  }

  /**
   * It is valid to use an identifier multiple times in the same Brite scope.
   * However, this is not valid in JavaScript. So we maintain a count of every
   * time an identifier is used in our JavaScript scope so that we can
   * deduplicate identifiers.
   */
  private identifierDedupeNumber(identifier: string): number {
    return this.currentScope().identifierDedupe.get(identifier) || 0;
  }
}

/**
 * A single JavaScript scope.
 */
type Scope = {
  identifierDedupe: Map<Identifier, number>;
  variables: Map<Identifier, Variable>;
  statements: Array<t.Statement>;
};

/**
 * A single JavaScript variable.
 */
type Variable = {
  identifier: t.Identifier;
};

/**
 * Creates a new empty `Scope`.
 */
function newScope(): Scope {
  return {
    identifierDedupe: new Map(),
    variables: new Map(),
    statements: [],
  };
}
