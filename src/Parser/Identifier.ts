/**
 * Brite identifiers follow the [Unicode identifier specification][1]. Including
 * the optional underscore (`_`) character, but not the optional dollar sign
 * (`$`) character. Brite identifiers support no medial characters.
 *
 * Brite identifiers also support any number of apostrophes at the end of an
 * identifier. This allows programmers to denote “prime” versions of variable
 * names as a convenience.
 *
 * There are some keywords which have a valid identifier syntax but are not
 * acceptable identifiers since we reserve these words for other
 * syntactic purposes.
 *
 * [1]: http://www.unicode.org/reports/tr31/
 */
export type Identifier = string & typeof IdentifierTag;

// A private symbol we use to make our `Identifier` type emulate an opaque type
// with an intersection.
const IdentifierTag = Symbol();

/**
 * An `Identifier` but without any keywords.
 */
export type BindingIdentifier = Identifier & typeof BindingIdentifierTag;

// A private symbol we use to make our `BindingIdentifier` type emulate an
// opaque type with an intersection.
const BindingIdentifierTag = Symbol();

/**
 * Creates a binding identifier and throws an error if it is not valid.
 */
export function ident(source: string): BindingIdentifier {
  const identifier = Identifier.create(source);
  if (!identifier) {
    throw new Error(`Invalid identifier: "${source}"`);
  }
  const bindingIdentifier = BindingIdentifier.create(identifier);
  if (!bindingIdentifier) {
    throw new Error(`Invalid binding identifier: "${source}"`);
  }
  return bindingIdentifier;
}

export namespace Identifier {
  /**
   * Creates an identifier. If the string is not a valid identifier then we
   * return undefined.
   */
  export function create(identifier: string): Identifier | undefined {
    if (!isIdentifier(identifier)) return undefined;
    return identifier as any; // tslint:disable-line no-any
  }

  /**
   * Creates an identifier assuming the string provided is valid. Does not even
   * check for keywords.
   */
  export function createAssumingValid(identifier: string): Identifier {
    return identifier as Identifier;
  }

  /**
   * Checks if a string is a valid identifier and that the identifier does not
   * conflict with any keywords.
   */
  export function isIdentifier(identifier: string): boolean {
    if (identifier.length < 1) return false;
    let finish = false;
    for (let i = 0; i < identifier.length; i++) {
      if (i === 0) {
        if (!isStart(identifier[0])) return false;
        continue;
      }
      if (!finish && isContinue(identifier[i])) {
        continue;
      }
      if (isFinish(identifier[i])) {
        if (!finish) finish = true;
        continue;
      }
      return false;
    }
    return true;
  }

  /**
   * The start of an identifier. Notably does not include numbers.
   *
   * All of `ID_Start` in the [Unicode identifier specification][1] and an
   * underscore (`_`) character.
   *
   * [1]: http://www.unicode.org/reports/tr31/
   */
  export function isStart(c: string): boolean {
    return /[_a-zA-Z]/.test(c);
  }

  /**
   * The continuing characters of an identifier.
   *
   * All of `ID_Continue` in the [Unicode identifier specification][1] and an
   * underscore (`_`) character.
   *
   * [1]: http://www.unicode.org/reports/tr31/
   */
  export function isContinue(c: string): boolean {
    return /[_a-zA-Z0-9]/.test(c);
  }

  /**
   * Matches an apostrophe character as an optional finishing character of
   * an identifier.
   */
  export function isFinish(c: string): boolean {
    return c === "'";
  }
}

export namespace BindingIdentifier {
  /**
   * Creates a `BindingIdentifier` from an identifier. If the identifier
   * conflicts with a binding identifier keyword then we return none.
   */
  export function create(
    identifier: Identifier
  ): BindingIdentifier | undefined {
    if (isKeyword(identifier)) return undefined;
    return identifier as any; // tslint:disable-line no-any
  }

  /**
   * Is this string a binding keyword? If so then return the keyword. Otherwise
   * return undefined.
   */
  export function isKeyword(c: string): boolean {
    switch (c) {
      case '_':
      case 'if':
      case 'then':
      case 'else':
      case 'case':
      case 'of':
      case 'return':
      case 'loop':
      case 'while':
      case 'do':
      case 'for':
      case 'in':
      case 'break':
      case 'continue':
        return true;
      default:
        return false;
    }
  }
}
