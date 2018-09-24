import {Result, Ok, Err} from '../Result';

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

/**
 * Valid identifier syntax which we reserve for other syntactic purposes. We try
 * to keep the set of keywords small since every keyword we reserve has the
 * potential to frustrate the programmer when they try to use that name in their
 * own programs.
 */
export const enum Keyword {
  Underscore = '_',
}

// A private symbol we use to make our `Identifier` type emulate an opaque type
// with an intersection.
const IdentifierTag = Symbol();

export namespace Identifier {
  /**
   * Creates an identifier assuming the string provided is valid. Does not even
   * check for keywords.
   */
  export function createAssumingValid(identifier: string): Identifier {
    return identifier as Identifier;
  }

  /**
   * Creates an identifier assuming that the syntax is valid. Still checks if
   * the identifier is a keyword or not.
   */
  export function createAssumingValidSyntax(
    identifier: string
  ): Result<Identifier, Keyword> {
    const keyword = isKeyword(identifier);
    if (keyword === undefined) {
      return Ok(identifier as Identifier);
    } else {
      return Err(keyword);
    }
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

  /**
   * Is this string a keyword? If so then return the keyword. Otherwise
   * return undefined.
   */
  function isKeyword(c: string): Keyword | undefined {
    switch (c) {
      case '_':
        return Keyword.Underscore;
      default:
        return undefined;
    }
  }
}
