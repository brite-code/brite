import {Bound, Monotype, Type} from './type';

const unresolvedBound: Bound = {kind: 'flexible', type: Type.bottom};

type TypeVariableEntry = {
  readonly level: number;
  readonly bound: Bound;
};

export class State {
  private level = 0;
  private nextType = 0;
  private readonly typeVariables = new Map<string, TypeVariableEntry>();

  getLevel(): number {
    return this.level;
  }

  incrementLevel() {
    this.level++;
  }

  decrementLevel() {
    this.level--;
  }

  newType(): Monotype {
    const name = `$${this.nextType++}`;
    this.typeVariables.set(name, {level: this.level, bound: unresolvedBound});
    return Type.variable(name);
  }

  newTypeWithBound(bound: Bound): Monotype {
    const name = `$${this.nextType++}`;
    this.typeVariables.set(name, {level: this.level, bound});
    return Type.variable(name);
  }

  lookupType(name: string): {readonly level: number; readonly bound: Bound} {
    const entry = this.typeVariables.get(name);
    if (entry === undefined) {
      throw new Error(`Type variable not found: "${name}"`);
    }
    return entry;
  }

  updateType(name: string, bound: Bound) {
    if (!this.typeVariables.has(name)) {
      throw new Error(`Type variable not found: "${name}"`);
    }
    this.typeVariables.set(name, {level: this.level, bound});
  }
}
