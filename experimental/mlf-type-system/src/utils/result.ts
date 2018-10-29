export type Result<T, E> =
  | {readonly kind: 'Ok'; readonly value: T}
  | {readonly kind: 'Err'; readonly value: E};

export function Ok<T>(value: T): Result<T, never> {
  return {kind: 'Ok', value};
}

export function Err<E>(value: E): Result<never, E> {
  return {kind: 'Err', value};
}
