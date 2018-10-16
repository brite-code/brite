import {assert} from './Assert';

export interface ReadonlyArray1<T> extends ReadonlyArray<T> {
  0: T;
}

export interface Array1<T> extends ReadonlyArray1<T> {
  push(value: T): void;
}

export namespace Array1 {
  export function create<T>(array: Array<T>): Array1<T> {
    assert(array.length >= 1, 'Array cannot have less than one element');
    return array as any; // tslint:disable-line no-any
  }
}

export interface ReadonlyArray2<T> extends ReadonlyArray<T> {
  0: T;
  1: T;
}

export interface Array2<T> extends ReadonlyArray2<T> {
  push(value: T): void;
}

export namespace Array2 {
  export function create<T>(array: Array<T>): Array2<T> {
    assert(array.length >= 2, 'Array cannot have less than two elements');
    return array as any; // tslint:disable-line no-any
  }
}
