// The TypeScript types which come with Immutable.js are frequently innacurate,
// so we define our own for the subset of Immutable.js that we use.
declare module 'immutable' {
  export function Map<K, V>(): Map<K, V>;

  export interface Map<K, V> extends MapCommonMethods<K, V> {
    merge(other: Map<K, V>, ...others: Array<Map<K, V>>): this;
    mergeWith(
      merger: (value1: V, value2: V, key: K) => V,
      other: Map<K, V>,
      ...others: Array<Map<K, V>>
    ): this;
    withMutations(mutator: (mutable: MutableMap<K, V>) => void): this;
    asMutable(): TemporaryMutableMap<K, V>;
  }

  interface MapCommonMethods<K, V> {
    set(key: K, value: V): this;
    get(key: K): V | undefined;
    has(key: K): boolean;
  }

  export interface MutableMap<K, V> extends MapCommonMethods<K, V> {}

  export interface TemporaryMutableMap<K, V> extends MutableMap<K, V> {
    asImmutable(): Map<K, V>;
  }

  export interface List<T> extends Iterable<T> {
    first(): T | undefined;
    push(value: T): List<T>;
    unshift(value: T): List<T>;
    shift(): List<T>;
    update(index: number, updater: (value: T) => T): List<T>;
    findLast(predicate: (value: T, key: number) => boolean): T | undefined;
    forEach(sideEffect: (value: T, key: number) => void | false): number;
  }
}
