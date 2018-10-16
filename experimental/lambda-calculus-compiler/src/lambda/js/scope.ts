import * as t from '@babel/types';

import {Binding} from '../term';

/**
 * A stack of JavaScript scopes. Manages things like identifier collision.
 */
export class Scope {
  private readonly bindings: Array<t.Identifier> = [];
  private readonly variables: Array<Map<string, number>> = [new Map()];

  /**
   * Executes the function in a new JavaScript block.
   */
  block<T>(f: () => T): T {
    this.variables.push(new Map());
    const result = f();
    this.variables.pop();
    return result;
  }

  /**
   * Executes the function with access to the provided binding.
   */
  binding<T>(binding: Binding, f: (variable: t.Identifier) => T): T {
    const variable = this.newVariable(binding.name);
    this.bindings.push(variable);
    const result = f(variable);
    this.bindings.pop();
    return result;
  }

  /**
   * Resolves a binding by index.
   */
  resolve(index: number): t.Identifier {
    const binding = this.bindings[this.bindings.length - index];
    if (binding === undefined) {
      throw new Error('Could not find binding.');
    }
    return binding;
  }

  /**
   * Creates a new identifier for a variable in this scope based off the
   * provided name.
   */
  newVariable(name: string): t.Identifier {
    let count = 0;
    for (let i = this.variables.length - 1; i >= 0; i--) {
      const actualCount = this.variables[i].get(name);
      if (actualCount !== undefined) {
        count = actualCount;
        break;
      }
    }
    this.variables[this.variables.length - 1].set(name, count + 1);
    if (reservedWords.has(name)) {
      name = `${name}_`;
    }
    if (count > 0 || name === '') {
      return t.identifier(`${name}$${count + 1}`);
    } else {
      return t.identifier(name);
    }
  }

  /**
   * Creates a new internal variable.
   */
  newInternalVariable(): t.Identifier {
    return this.newVariable('');
  }
}

/**
 * Reserved words according to the ECMAScript specification.
 */
const reservedWords = new Set([
  'null',
  'true',
  'false',
  'if',
  'in',
  'do',
  'var',
  'for',
  'new',
  'try',
  'this',
  'else',
  'case',
  'void',
  'with',
  'enum',
  'while',
  'break',
  'catch',
  'throw',
  'const',
  'yield',
  'class',
  'super',
  'return',
  'typeof',
  'delete',
  'switch',
  'export',
  'import',
  'default',
  'finally',
  'extends',
  'function',
  'continue',
  'debugger',
  'instanceof',
  'implements',
  'interface',
  'package',
  'private',
  'protected',
  'public',
  'static',
  'let',
]);
