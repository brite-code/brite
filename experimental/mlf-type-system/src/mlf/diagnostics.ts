const opaque = Symbol();

export type Reported<T> = T & typeof opaque;

export class Diagnostics<T> implements Iterable<T> {
  private readonly reported: Array<T> = [];

  report(diagnostic: T): Reported<T> {
    this.reported.push(diagnostic);
    return diagnostic as Reported<T>;
  }

  [Symbol.iterator]() {
    return this.reported[Symbol.iterator]();
  }
}
