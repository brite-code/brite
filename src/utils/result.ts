export type Result<T, E> =
  | {readonly kind: 'ok'; readonly value: T}
  | {readonly kind: 'err'; readonly value: E};

export function Ok<T>(value: T): Result<T, never> {
  return {
    kind: 'ok',
    value,
  };
}

export function Err<E>(value: E): Result<never, E> {
  return {
    kind: 'err',
    value,
  };
}
