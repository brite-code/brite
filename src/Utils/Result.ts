/**
 * The type of `Result`.
 */
export const enum ResultType {
  Ok = 'Ok',
  Err = 'Err',
}

/**
 * Computations end in some result which could either be some value or some
 * exception indicating an error occurred.
 */
export type Result<T, E> =
  | {
      readonly type: ResultType.Ok;
      readonly value: T;
    }
  | {
      readonly type: ResultType.Err;
      readonly value: E;
    };

/**
 * Creates an ok result.
 */
export function Ok<T>(value: T): Result<T, never> {
  return {
    type: ResultType.Ok,
    value,
  };
}

/**
 * Creates an exceptional result.
 */
export function Err<E>(value: E): Result<never, E> {
  return {
    type: ResultType.Err,
    value,
  };
}
