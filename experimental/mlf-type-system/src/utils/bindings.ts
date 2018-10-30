/**
 * A data structure for efficient manipulation of a binding list with shadowed
 * variables in imperative code.
 *
 * For example, in the lambda calculus you might have
 * `λx.( ... (λy.λx.pair x y) ... )`. We
 * need an efficient way to resolve `x` and `y`. We also may need to access the
 * shadowed `x` again after the deeper `x` binding goes out of scope.
 *
 * This data structure provides this service.
 */
export class BindingMap<K, V> {
  // We organize bindings into a map where each key is associated with an array
  // of values. The last value in the array is the current binding. Previous
  // values in the array are shadowed.
  private readonly bindings = new Map<K, Array<V>>();

  // When we pop, we pop the last key that we pushed.
  private readonly bindingsStack: Array<K> = [];

  constructor(bindings?: Iterable<[K, V]>) {
    if (bindings !== undefined) {
      for (const [key, value] of bindings) {
        this.push(key, value);
      }
    }
  }

  /**
   * Adds a value for the provided key into the map. If there is already a
   * binding for this key then it will be shadowed. To reveal the shadowed
   * binding once this new binding goes out of scope call `BindingMap.pop()`.
   */
  push(key: K, value: V) {
    this.bindingsStack.push(key);
    const shadowedBindings = this.bindings.get(key);
    if (shadowedBindings !== undefined) {
      shadowedBindings.push(value);
    } else {
      this.bindings.set(key, [value]);
    }
  }

  /**
   * Removes the last binding we pushed. Returns the value of this binding.
   */
  pop(): {readonly key: K; readonly value: V} | undefined {
    const key = this.bindingsStack.pop();
    if (key === undefined) return undefined;
    const shadowedBindings = this.bindings.get(key);
    if (shadowedBindings !== undefined) {
      const value = shadowedBindings.pop()!; // tslint:disable-line no-non-null-assertion
      if (shadowedBindings.length === 0) {
        this.bindings.delete(key);
      }
      return {key, value};
    } else {
      return undefined;
    }
  }

  /**
   * Gets value for the provided binding key. If the key has shadowed values we
   * return the most recent.
   */
  get(key: K): V | undefined {
    const shadowedBindings = this.bindings.get(key);
    if (shadowedBindings === undefined) return undefined;
    return shadowedBindings[shadowedBindings.length - 1];
  }

  /**
   * Returns true if we have a binding for the provided key.
   */
  has(key: K): boolean {
    return this.bindings.has(key);
  }

  /**
   * Is this bindings map empty?
   */
  isEmpty(): boolean {
    return this.bindingsStack.length === 0;
  }

  /**
   * Returns the count of all distinct keys in the map.
   */
  distinctKeysCount(): number {
    return this.bindings.size;
  }

  /**
   * Returns an iterator of all the distinct keys in the map. No order is
   * guaranteed. Even if a key has multiple values it will only appear once in
   * the iterator.
   */
  distinctKeys(): IterableIterator<K> {
    return this.bindings.keys();
  }

  /**
   * Returns an iterator of all the distinct entries in the map. No order is
   * guaranteed. Even if a key has multiple values we will only return the
   * latest value in the iterator.
   */
  distinctEntries(): IterableIterator<[K, V]> {
    const entries = this.bindings.entries();
    const iterator: IterableIterator<[K, V]> = {
      [Symbol.iterator]: () => iterator,
      next: () => {
        const entry = entries.next();
        if (entry.done === true) {
          return {done: true, value: undefined as never};
        }
        const value = entry.value[1][entry.value[1].length - 1];
        const newEntry: [K, V] = [entry.value[0], value];
        return {done: false, value: newEntry};
      },
    };
    return iterator;
  }
}
