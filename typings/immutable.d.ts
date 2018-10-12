// The TypeScript types which come with Immutable.js are frequently innacurate,
// so we define our own for the subset of Immutable.js that we use.
declare module 'immutable' {
  export class Map<K, V> {
    set(key: K, value: V): this;
    get(key: K): V | undefined;
  }
}
