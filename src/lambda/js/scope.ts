import * as t from '@babel/types';

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
    const scope = scopeStack.root;
    return [result, scope.statements];
  }

  private readonly root = newScope();
  private readonly stack: Array<Scope> = [];

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
   * Adds a statement to the current scope.
   */
  addStatement(statement: t.Statement) {
    this.currentScope().statements.push(statement);
  }

  /**
   * Returns the current scope. One is always guaranteed to exist.
   */
  private currentScope(): Scope {
    return this.stack[this.stack.length - 1] || this.root;
  }
}

/**
 * A single JavaScript scope.
 */
type Scope = {
  statements: Array<t.Statement>;
};

/**
 * Creates a new empty `Scope`.
 */
function newScope(): Scope {
  return {
    statements: [],
  };
}
