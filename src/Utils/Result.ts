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

export namespace Result {
  /**
   * Returns the “ok” value from the result or returns the provided
   * default value.
   */
  export function unwrapOr<T>(result: Result<T, unknown>, other: T): T {
    switch (result.type) {
      case ResultType.Ok:
        return result.value;
      case ResultType.Err:
        return other;
    }
  }

  /**
   * Returns the “ok” value or calls the other function with the error value to
   * produce an alternative value to return.
   */
  export function unwrapOrElse<T, E>(
    result: Result<T, E>,
    other: (error: E) => T
  ): T {
    switch (result.type) {
      case ResultType.Ok:
        return result.value;
      case ResultType.Err:
        return other(result.value);
    }
  }
}
