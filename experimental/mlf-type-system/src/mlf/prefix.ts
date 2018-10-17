import {BindingMap} from './bindings';
import * as t from './builder';
import {Namer} from './namer';
import {Bound, Type} from './type';

type Entry = {
  bound: Bound;
};

/**
 * Manages the type variables of our program during type inferences. Handles
 * naming collisions, bound updates, fresh variable creation, and more.
 */
export class Prefix {
  private readonly namer = new Namer();
  private readonly bindings = new BindingMap<string, Entry>();

  constructor(bindings?: Iterable<[string, Bound]>) {
    if (bindings !== undefined) {
      for (const [binding, bound] of bindings) {
        this.push(binding, bound);
      }
    }
  }

  /**
   * Adds a bound to the prefix, giving the bound a unique identifier. Returns
   * the identifier so that the bound may be retrieved from the prefix later.
   *
   * Make sure to call `Prefix.pop()` or `Prefix.quantify()` when the binding
   * goes out of scope.
   *
   * If our prefix has a parent then we only add the binding to this prefix. Not
   * our parent prefix. We will make sure that the name we create is unique in
   * our parent prefix as well, though.
   */
  add(bound: Bound): string {
    // Find an identifier in our bindings map that has not already been taken.
    let identifier = this.namer.generate();
    while (this.bindings.has(identifier)) {
      identifier = this.namer.generate();
    }
    // Add the bound to our bindings map.
    this.push(identifier, bound);
    return identifier;
  }

  /**
   * Pushes a bound into our prefix. If a type variable of this identifier is
   * already defined in our prefix we don’t override it. Instead we only shadow
   * the binding.
   *
   * Make sure to call `Prefix.quantify()` when the binding goes out of scope.
   */
  push(identifier: string, bound: Bound) {
    this.bindings.push(identifier, {bound});
  }

  /**
   * Removes a bound from our prefix and uses it to quantify the provided type.
   */
  quantify(identifier: string, type: Type): Type {
    const entry = this.bindings.pop(identifier);
    if (entry === undefined) return type;
    // If there are no more identifiers with this name then we can reuse it.
    this.namer.reuse(identifier);
    // Quantify the provided type with our bound.
    return t.quantifiedType(identifier, entry.bound, type);
  }

  /**
   * Gets the entry for the provided type identifier. This method is private to
   * this file which is why we use a symbol.
   */
  private getEntry(identifier: string): Entry | undefined {
    return this.bindings.get(identifier);
  }

  /**
   * Gets the bound for the provided type identifier.
   */
  get(identifier: string): Bound | undefined {
    const entry = this.bindings.get(identifier);
    return entry !== undefined ? entry.bound : undefined;
  }

  /**
   * Does a type variable for this binding exist in our prefix?
   */
  has(identifier: string): boolean {
    return this.bindings.has(identifier);
  }

  /**
   * Are there no bindings in our prefix? Also makes sure there are no bindings
   * in our parent prefix.
   */
  isEmpty(): boolean {
    return this.bindings.isEmpty();
  }
}

/**
 * A prefix with an alternate bindings map and a parent prefix. We can use this
 * to fork our prefix’s reality into many alternatives without modifying the
 * parent prefix.
 */
export class ForkedPrefix {
  private readonly parent: Prefix;
  private readonly bindings = new BindingMap<string, Entry>();

  constructor(parent: Prefix) {
    this.parent = parent;
  }

  /**
   * Pushes a type variable to our local prefix.
   */
  push(identifier: string, bound: Bound) {
    this.bindings.push(identifier, {bound});
  }

  /**
   * Pops a type variable from our local prefix.
   *
   * If the type variable does not exist in this forked prefix but does exist in
   * our parent prefix then we will not pop from our parent prefix.
   */
  pop(identifier: string) {
    this.bindings.pop(identifier);
  }

  /**
   * Pops a type variable from our local prefix and uses the bound to quantify
   * the provided type.
   *
   * If the type variable does not exist in this forked prefix but does exist in
   * our parent prefix then we will not pop from our parent prefix.
   */
  quantify(identifier: string, type: Type): Type {
    const entry = this.bindings.pop(identifier);
    if (entry === undefined) return type;
    // Quantify the provided type with our bound.
    return t.quantifiedType(identifier, entry.bound, type);
  }

  /**
   * Gets the entry for the provided type identifier. This method is private to
   * this file which is why we use a symbol.
   */
  private getEntry(identifier: string): Entry | undefined {
    const entry = this.bindings.get(identifier);
    if (entry !== undefined) {
      return entry;
    } else {
      // @ts-ignore: `getEntry()` is only private in the context of this file.
      return this.parent.getEntry(identifier);
    }
  }

  /**
   * Gets the bound for the provided type identifier. If it does not exist in
   * the forked bindings then we look in the parent prefix.
   */
  get(identifier: string): Bound | undefined {
    const entry = this.bindings.get(identifier);
    return entry !== undefined ? entry.bound : this.parent.get(identifier);
  }

  /**
   * Returns true if a type variable with the provided identifier _is not_
   * defined in the local prefix but _is_ defined in the parent prefix.
   */
  isInParent(identifier: string): boolean {
    if (this.bindings.has(identifier)) return false;
    return this.parent.has(identifier);
  }

  /**
   * Are there no bindings in our local prefix? Does not consider whether or not
   * there are bindings in our parent prefix.
   */
  isLocallyEmpty(): boolean {
    return this.bindings.isEmpty();
  }

  /**
   * Updates a type variable with the provided identifier in our prefix. May
   * update a type variable in a parent prefix.
   */
  update(identifier: string, bound: Bound) {
    const entry = this.getEntry(identifier);
    if (entry === undefined) {
      throw new Error('Can only update an existing binding.');
    }
    // Update the entry’s bound.
    entry.bound = bound;
  }
}
