/**
 * Asserts that a condition passes.
 */
export function assert(condition: boolean, message: string) {
  if (!condition) { throw new Error(`Assertion failed: ${message}`); }
}
