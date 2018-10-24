import {MonomorphicType, PolymorphicTypeBound, Type} from './type';

const unresolvedTypeBound: PolymorphicTypeBound = {
  kind: 'flexible',
  type: Type.bottom,
};

export class Context {
  private nextType = 0;
  private readonly typeVariables = new Map<string, PolymorphicTypeBound>();

  newType(level: number): MonomorphicType {
    const name = `$${this.nextType++}`;
    return Type.variable(name);
  }

  newTypeWithBound(bound: PolymorphicTypeBound): MonomorphicType {
    const name = `$${this.nextType++}`;
    this.typeVariables.set(name, bound);
    return Type.variable(name);
  }

  lookupType(name: string): PolymorphicTypeBound {
    return this.typeVariables.get(name) || unresolvedTypeBound;
  }

  updateType(name: string, bound: PolymorphicTypeBound) {
    this.typeVariables.set(name, bound);
  }
}
