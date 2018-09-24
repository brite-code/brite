export type Identifier = string & typeof IdentifierTag;

export const enum Keyword {
  Underscore = '_',
}

// A private symbol we use to make our `Identifier` type emulate an opaque type
// with an intersection.
const IdentifierTag = Symbol();
