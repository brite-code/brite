import {BindingMap} from './bindings';
import * as t from './builder';
import {Bound, Type} from './type';

type Entry = {
  level: number;
  moved: boolean;
  readonly counter: number | undefined;
  bound: Bound;
  readonly dependencies: Array<{
    readonly binding: string;
    readonly entry: Entry;
  }>;
};

/**
 * Manages the type variables of our program during type inferences. Handles
 * naming collisions, bound updates, fresh variable creation, and more.
 */
export class Prefix {
  /**
   * Creates an empty prefix.
   */
  static create(): Prefix {
    return new Prefix();
  }

  /**
   * Creates a prefix with a parent.
   */
  static withParent(parent: Prefix): Prefix {
    return new Prefix(parent);
  }

  /**
   * Creates a prefix with some bindings already set.
   */
  static withBindings(bindings: Iterable<[string, Bound]>): Prefix {
    const prefix = new Prefix();
    for (const [binding, bound] of bindings) {
      prefix.push(binding, bound);
    }
    return prefix;
  }

  private level: number;
  private counter: number;
  private readonly bindings = new BindingMap<string, Entry>();
  private readonly parent: Prefix | undefined;

  private constructor(parent?: Prefix) {
    this.level = parent ? parent.level : 0;
    this.counter = parent ? parent.counter : 0;
    this.parent = parent;
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
    let identifier = typeVariableName(this.counter);
    while (this.has(identifier)) {
      this.counter = this.counter + 1;
      identifier = typeVariableName(this.counter);
    }
    // Add the bound to our bindings map.
    this.pushEntry(identifier, {counter: this.counter, bound});
    // Increment our counter by one.
    this.counter = this.counter + 1;
    return identifier;
  }

  /**
   * Pushes a bound into our prefix. If a type variable of this identifier is
   * already defined in our prefix we don’t override it. Instead we only shadow
   * the binding.
   *
   * Make sure to call `Prefix.pop()` or `Prefix.quantify()` when the binding
   * goes out of scope.
   *
   * We only push to the local prefix and not the parent prefix if we have one.
   */
  push(identifier: string, bound: Bound) {
    this.pushEntry(identifier, {counter: undefined, bound});
  }

  /**
   * Pushes an entry to our local prefix.
   */
  private pushEntry(
    identifier: string,
    entry: Pick<Entry, Exclude<keyof Entry, 'level' | 'moved' | 'dependencies'>>
  ) {
    this.bindings.push(identifier, {
      ...entry,
      level: this.level,
      moved: false,
      dependencies: [],
    });
    this.level += 1;
  }

  /**
   * Pops an entry from our local prefix.
   */
  private popEntry(identifier: string): Entry | undefined {
    this.level -= 1;
    const entry = this.bindings.pop(identifier);
    // If this type variable’s name was generated then reset our counter to
    // this entry’s counter so that we can reuse the generated name.
    if (entry !== undefined && entry.counter !== undefined) {
      this.counter = entry.counter;
    }
    // Return the entry.
    return entry;
  }

  /**
   * Removes a bound from our local prefix when it goes out of scope. Returns
   * the bound if the identifier references an actual type variable.
   *
   * Remember that this operation is _local only_. We never pop our
   * parent prefix.
   */
  pop(identifier: string) {
    this.popEntry(identifier);
  }

  /**
   * Removes a bound from our local prefix and uses it to quantify the
   * provided type.
   */
  quantify(identifier: string, type: Type): Type {
    const entry = this.popEntry(identifier);
    if (entry === undefined) return type;
    if (entry.moved) return type;
    const newType = t.quantifiedType(identifier, entry.bound, type);
    return entry.dependencies.reduceRight(
      (type, dependency) =>
        dependency.entry.level === entry.level
          ? t.quantifiedType(dependency.binding, dependency.entry.bound, type)
          : type,
      newType
    );
  }

  /**
   * Gets the entry for the provided type identifier.
   */
  private getEntry(identifier: string): Entry | undefined {
    const entry = this.bindings.get(identifier);
    if (entry !== undefined) {
      return entry;
    } else if (this.parent !== undefined) {
      return this.parent.getEntry(identifier);
    } else {
      return undefined;
    }
  }

  /**
   * Gets the bound for the provided type identifier.
   */
  get(identifier: string): Bound | undefined {
    const entry = this.bindings.get(identifier);
    if (entry !== undefined) {
      return entry.bound;
    } else if (this.parent !== undefined) {
      return this.parent.get(identifier);
    } else {
      return undefined;
    }
  }

  /**
   * Does this prefix have a type variable with the provided identifier?
   */
  has(identifier: string): boolean {
    if (this.bindings.has(identifier)) return true;
    if (this.parent !== undefined) return this.parent.has(identifier);
    return false;
  }

  /**
   * Returns true if a type variable with the provided identifier _is not_
   * defined in the local prefix but _is_ defined in the parent prefix.
   */
  isInParent(identifier: string): boolean {
    if (this.parent === undefined) return false;
    if (this.bindings.has(identifier)) return false;
    return this.parent.has(identifier);
  }

  /**
   * Are there no bindings in our prefix? Also makes sure there are no bindings
   * in our parent prefix.
   */
  isEmpty(): boolean {
    return (
      this.bindings.isEmpty() && (this.parent ? this.parent.isEmpty() : true)
    );
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
    // Check to see if any of the free variables in our new type need to
    // be moved.
    for (const dependencyIdentifier of Type.getFreeVariables(bound.type)) {
      const dependencyEntry = this.getEntry(dependencyIdentifier);
      // If our dependency entry is at a lower level then our dependant entry we
      // don’t need to do anything since it is already in-scope.
      //
      // TODO: IMPORTANT: But what if the dependency is shadowed?
      if (dependencyEntry === undefined) continue;
      if (dependencyEntry.level < entry.level) continue;
      // TODO: IMPORTANT: But what if the dependency is listed twice?
      // TODO: IMPORTANT: But what if the dependency shadows an important type variable?
      // TODO: IMPORTANT: But what if two type variables unify to this entry?
      dependencyEntry.level = entry.level;
      dependencyEntry.moved = true;
      entry.dependencies.push({
        binding: dependencyIdentifier,
        entry: dependencyEntry,
      });
    }
  }
}

/**
 * Get a type variable name for the current counter value.
 */
export function typeVariableName(counter: number): string {
  if (counter >= letters.length) {
    return `t${counter - letters.length + 1}`;
  } else {
    return letters[counter];
  }
}

/**
 * Tries to decode a type variable created by `typeVariableName()` back into
 * a number.
 */
export function typeVariableCounter(name: string): number | undefined {
  let count = lettersToIndex.get(name);
  if (count !== undefined) return count;
  if (!/^t[1-9][0-9]*$/.test(name)) return undefined;
  count = parseInt(name.slice(1), 10);
  if (isNaN(count) || count < 1) return undefined;
  return count + letters.length - 1;
}

// All the ASCII lowercase letters. We use these for creating type
// variable names.
const letters: ReadonlyArray<string> = 'abcdefghijklmnopqrstuvwxyz'.split('');

// A map of ASCII lowercase letters to its integer index in the letters array.
const lettersToIndex = new Map<string, number>(
  letters.map((letter, i): [string, number] => [letter, i])
);
