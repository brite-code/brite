import {Err, Ok, Result} from '../Utils/Result';

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
 * An `Identifier` but without `BindingKeyword`.
 */
export type BindingIdentifier = string & typeof BindingIdentifierTag;

// A private symbol we use to make our `BindingIdentifier` type emulate an
// opaque type with an intersection.
const BindingIdentifierTag = Symbol();

/**
 * Valid identifier syntax which we reserve for other syntactic purposes. We try
 * to keep the set of keywords small since every keyword we reserve has the
 * potential to frustrate the programmer when they try to use that name in their
 * own programs.
 */
export const enum Keyword {
  Underscore = '_',
}

/**
 * Keywords we don’t allow to be used unqualified in bindings. We do this to
 * reserve some words for syntactic purposes.
 *
 * NOTE: We may not need all these keywords.
 */
export const enum BindingKeyword {
  If = 'if',
  Then = 'then',
  Else = 'else',
  Match = 'match',
  Return = 'return',
  Loop = 'loop',
  While = 'while',
  For = 'for',
  In = 'in',
  Break = 'break',
  Continue = 'continue',
}

/**
 * Creates an identifier and throws an error if it is not valid.
 */
export function ident(identifier: string): Identifier {
  const id = Identifier.create(identifier);
  if (!id) throw new Error(`Invalid identifier: "${identifier}"`);
  return id;
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
   * Creates an identifier assuming that the syntax is valid. Still checks if
   * the identifier is a keyword or not.
   */
  export function createAssumingValidSyntax(
    identifier: string
  ): Result<Identifier, Keyword> {
    const keyword = getKeyword(identifier);
    if (keyword === undefined) {
      return Ok(identifier as Identifier);
    } else {
      return Err(keyword);
    }
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
    return !getKeyword(identifier);
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
  export function getKeyword(c: string): Keyword | undefined {
    switch (c) {
      case '_':
        return Keyword.Underscore;
      default:
        return undefined;
    }
  }

  /**
   * Is this string a binding keyword? If so then return the keyword. Otherwise
   * return undefined.
   */
  export function getBindingKeyword(c: string): BindingKeyword | undefined {
    switch (c) {
      case 'if':
        return BindingKeyword.If;
      case 'then':
        return BindingKeyword.Then;
      case 'else':
        return BindingKeyword.Else;
      case 'match':
        return BindingKeyword.Match;
      case 'return':
        return BindingKeyword.Return;
      case 'loop':
        return BindingKeyword.Loop;
      case 'while':
        return BindingKeyword.While;
      case 'for':
        return BindingKeyword.For;
      case 'in':
        return BindingKeyword.In;
      case 'break':
        return BindingKeyword.Break;
      case 'continue':
        return BindingKeyword.Continue;
      default:
        return undefined;
    }
  }
}
