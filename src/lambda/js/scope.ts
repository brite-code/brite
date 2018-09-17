import * as t from '@babel/types';
import {Identifier} from '../term';

/**
 * A stack of JavaScript scopes. Manages things like identifier collision.
 */
export class ScopeStack {
  /**
   * The current stack of JavaScript scopes. Will always have at least
   * one scope.
   */
  private readonly stack: Array<Scope> = [newScope()];

  /**
   * Nests the call to `f` in a new scope.
   */
  nest<T>(f: () => T): T {
    this.stack.push(newScope());
    const result = f();
    this.stack.pop()!;
    return result;
  }

  /**
   * Declares a variable in our scope and returns an escaped, deduplicated,
   * JavaScript identifier.
   */
  declareVariable(identifier: Identifier): t.Identifier {
    const newIdentifier = this.createIdentifier(identifier);
    this.currentScope().variables.set(identifier, {identifier: newIdentifier});
    return newIdentifier;
  }

  /**
   * Resolves a variable in our scope stack and returns its escaped identifier.
   */
  resolveVariable(identifier: Identifier): t.Identifier {
    for (let i = this.stack.length - 1; i >= 0; i--) {
      const variable = this.stack[i].variables.get(identifier);
      if (variable !== undefined) {
        return variable.identifier;
      }
    }
    throw new Error(`Could not resolve variable "${identifier}"`);
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
   *
   * Also increments the dedupe number for this identifier.
   */
  private identifierDedupeNumber(identifier: string): number {
    const scope = this.currentScope();
    const dedupe = (scope.identifierDedupe.get(identifier) || 0) + 1;
    scope.identifierDedupe.set(identifier, dedupe);
    return dedupe;
  }

  /**
   * Creates an identifier for a variable declared in the programmer’s source
   * code. Of the form: "\(name)$\(dedupe)". The dollar sign (`$`) is a valid
   * JavaScript identifier character, but not a valid Brite identifier
   * character. So we can use it to deduplicate identifiers.
   */
  private createIdentifier(name: string): t.Identifier {
    let identifier = name;
    const dedupe = this.identifierDedupeNumber(name);
    if (dedupe !== 1) identifier += `$${dedupe}`;
    return t.identifier(identifier);
  }

  /**
   * Creates an internal identifier. Internal identifiers are guaranteed to
   * never collide with programmer defined identifiers. Identifiers are of the
   * form: "_$\(name)\(dedupe)". These will never collide with programmer
   * identifiers since the dollar sign (`$`) is not valid in Brite identifiers
   * and a lone underscore (`_`) is not a valid Brite identifier.
   */
  createInternalIdentifier(name: string): t.Identifier {
    let identifier = `_$${name}`;
    const dedupe = this.identifierDedupeNumber(identifier);
    if (dedupe !== 1 || name === '') identifier += dedupe;
    return t.identifier(identifier);
  }
}

/**
 * A single JavaScript scope.
 */
type Scope = {
  identifierDedupe: Map<Identifier, number>;
  variables: Map<Identifier, Variable>;
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
  };
}
