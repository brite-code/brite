const opaque = Symbol();

export type Identifier = string & typeof opaque;

export namespace Identifier {
  export function create(x: string): Identifier {
    return x as Identifier;
  }
}

export type TypeIdentifier = number & typeof opaque;

export namespace TypeIdentifier {
  export function create(x: number): TypeIdentifier {
    return x as TypeIdentifier;
  }
}
