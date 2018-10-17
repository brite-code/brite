/**
 * Generates unique human-readable names for things.
 *
 * “And Taborlin the Great said to the stone: “BREAK!” and the stone broke...”
 */
export class Namer {
  private counter = 0;
  private readonly reusable: Array<number> = [];

  /**
   * Generates a new human-readable name.
   */
  generate(): string {
    const counter = this.reusable.pop();
    if (counter !== undefined) return nameFromCounter(counter);
    const name = nameFromCounter(this.counter);
    this.counter = this.counter + 1;
    return name;
  }

  /**
   * Marks a name as reusable.
   */
  reuse(name: string) {
    const counter = counterFromName(name);
    if (counter !== undefined && counter < this.counter) {
      this.reusable.push(counter);
    }
  }
}

/**
 * Get a human readable name from a counter value.
 */
export function nameFromCounter(counter: number): string {
  if (counter >= letters.length) {
    return `t${counter - letters.length + 1}`;
  } else {
    return letters[counter];
  }
}

/**
 * Tries to decode a type variable created by `typeVariableName()` back into
 * a number.
 */
export function counterFromName(name: string): number | undefined {
  let count = lettersToIndex.get(name);
  if (count !== undefined) return count;
  if (!/^t[1-9][0-9]*$/.test(name)) return undefined;
  count = parseInt(name.slice(1), 10);
  if (isNaN(count) || count < 1) return undefined;
  return count + letters.length - 1;
}

// All the ASCII lowercase letters. We use these for creating type
// variable names.
const letters: ReadonlyArray<string> = 'abcdefghijklmnopqrstuvwxyz'.split('');

// A map of ASCII lowercase letters to its integer index in the letters array.
const lettersToIndex = new Map<string, number>(
  letters.map((letter, i): [string, number] => [letter, i])
);
