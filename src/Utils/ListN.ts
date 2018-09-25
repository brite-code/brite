import {assert} from './Assert';

export class List1<T> {
  constructor(array: ReadonlyArray<T>) {
    assert(array.length >= 1, 'Array cannot have less than one element');
  }
}

export class List2<T> {
  constructor(array: ReadonlyArray<T>) {
    assert(array.length >= 2, 'Array cannot have less than two elements');
  }
}
