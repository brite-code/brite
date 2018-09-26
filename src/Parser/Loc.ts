import {assert} from '../Utils/Assert';

/**
 * A character position in the source code by line and column. Both lines and
 * columns start at 1 instead of 0.
 */
export class Pos {
  /**
   * Gets the initial position. In the initial position the line is 1 and the
   * column is also 1.
   */
  public static initial() {
    return new Pos(1, 1);
  }

  // NOTE: Use `Int16` when converting to Brite. So one `Pos` is 32 bits and
  // will fit in a single JavaScript number. One `Loc` will be 64 bits.
  public readonly line: number;
  public readonly column: number;

  constructor(line: number, column: number) {
    this.line = line;
    this.column = column;
  }

  /**
   * Checks if this `Pos` equals another.
   */
  equals(other: Pos) {
    return this.line === other.line && this.column === other.column;
  }

  /**
   * Compares this `Pos` to another.
   *
   * - If `this` is smaller than `other` return -1.
   * - If `this` is the same as `other` return 0.
   * - If `this` is larger than `other` return 1.
   */
  compare(other: Pos): -1 | 0 | 1 {
    if (this.line < other.line) {
      return -1;
    } else if (this.line > other.line) {
      return 1;
    } else if (this.column < other.column) {
      return -1;
    } else if (this.column > other.column) {
      return 1;
    } else {
      return 0;
    }
  }

  /**
   * Returns the smaller `Pos`.
   */
  min(other: Pos): Pos {
    if (this.compare(other) > 0) return other;
    return this;
  }

  /**
   * Returns the larger `Pos`.
   */
  max(other: Pos): Pos {
    if (this.compare(other) < 0) return other;
    return this;
  }
}

/**
 * Parses a location from a string. Throws an error if the syntax is invalid.
 */
export function loc(source: string): Loc {
  const loc = Loc.parse(source);
  if (loc === undefined) {
    throw new Error(`Invalid location syntax: "${source}"`);
  }
  return loc;
}

/**
 * A location range in source code. Spans across many lines and columns to
 * highlight a particular range. Represented with a starting position and an
 * ending position.
 */
export class Loc {
  /**
   * Parse a `Loc` from a string.
   */
  static parse(source: string): Loc | undefined {
    const [a, b = ''] = source.split('-', 2);
    const [a1, a2 = ''] = a.split(':', 2);
    const startLine = parseInt(a1, 10);
    if (isNaN(startLine)) return undefined;
    let start;
    if (a2 === '') {
      start = new Pos(1, startLine);
    } else {
      const startColumn = parseInt(a2, 10);
      if (isNaN(startColumn)) return undefined;
      start = new Pos(startLine, startColumn);
    }
    if (b === '') {
      return new Loc(start, start);
    } else {
      const [b1, b2 = ''] = b.split(':', 2);
      const endLine = parseInt(b1, 10);
      if (isNaN(endLine)) return undefined;
      let end;
      if (b2 === '') {
        end = new Pos(1, endLine);
      } else {
        const endColumn = parseInt(b2, 10);
        if (isNaN(endColumn)) return undefined;
        end = new Pos(endLine, endColumn);
      }
      if (start.compare(end) !== 1) {
        return new Loc(start, end);
      } else {
        return undefined;
      }
    }
  }

  /**
   * Creates a location just for this position.
   */
  static pos(line: number, column: number) {
    const pos = new Pos(line, column);
    return new Loc(pos, pos);
  }

  public readonly start: Pos;
  public readonly end: Pos;

  constructor(start: Pos, end: Pos) {
    assert(start.compare(end) !== 1, 'Start should be smaller then end.');
    this.start = start;
    this.end = end;
  }

  /**
   * Creates a new `Loc` that spans the range between the two
   * provided locations.
   */
  between(other: Loc): Loc {
    return new Loc(this.start.min(other.start), this.end.max(other.end));
  }
}
