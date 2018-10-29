/**
 * Value representing the result of a computation which had either a successful
 * state or exception state.
 */
export type Result<T, E> =
  | {readonly kind: 'Ok'; readonly value: T}
  | {readonly kind: 'Err'; readonly value: E};

/**
 * Creates an ok result.
 */
export function Ok<T>(value: T): Result<T, never> {
  return {kind: 'Ok', value};
}

/**
 * Creates an exceptional result.
 */
export function Err<E>(value: E): Result<never, E> {
  return {kind: 'Err', value};
}
