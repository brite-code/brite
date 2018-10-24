import {Bound, MonomorphicType, Type} from './type';

const unresolvedTypeBound: Bound = {kind: 'flexible', type: Type.bottom};

export class State {
  private level = 0;
  private nextType = 0;
  private readonly typeVariables = new Map<string, Bound>();

  getLevel(): number {
    return this.level;
  }

  incrementLevel() {
    this.level++;
  }

  decrementLevel() {
    this.level--;
  }

  newType(): MonomorphicType {
    const name = `$${this.nextType++}`;
    return Type.variable(name);
  }

  newTypeWithBound(bound: Bound): MonomorphicType {
    const name = `$${this.nextType++}`;
    this.typeVariables.set(name, bound);
    return Type.variable(name);
  }

  lookupType(name: string): Bound {
    if (name[0] !== '$') throw new Error('May only lookup generated types.');
    return this.typeVariables.get(name) || unresolvedTypeBound;
  }

  updateType(name: string, bound: Bound) {
    if (name[0] !== '$') throw new Error('May only update generated types.');
    this.typeVariables.set(name, bound);
  }
}
