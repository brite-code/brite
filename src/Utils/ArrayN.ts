import {assert} from './Assert';

export interface ReadonlyArray1<T> extends ReadonlyArray<T> {
  0: T;
}

export namespace ReadonlyArray1 {
  export function create<T>(array: ReadonlyArray<T>): ReadonlyArray1<T> {
    assert(array.length >= 1, 'Array cannot have less than one element');
    return array as ReadonlyArray1<T>;
  }
}

export interface ReadonlyArray2<T> extends ReadonlyArray<T> {
  0: T;
  1: T;
}

export namespace ReadonlyArray2 {
  export function create<T>(array: ReadonlyArray<T>): ReadonlyArray2<T> {
    assert(array.length >= 2, 'Array cannot have less than two elements');
    return array as ReadonlyArray2<T>;
  }
}
