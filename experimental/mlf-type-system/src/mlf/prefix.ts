import {BindingMap} from './bindings';
import * as t from './builder';
import {Bound, Type} from './type';

/**
 * The type variable environment of our program during type instance.
 * Responsible for introducing type variables into scope and taking them out
 * with a quantification. Also performs dependency and liveness analysis.
 *
 * TODO: Rename prefix. It only makes sense if you read the MLF paper.
 */
export class Prefix {
  // A map of type variable names to an entry for that type variable. The entry
  // mostly includes the bound but also includes things like
  // dependency analysis.
  private readonly bindings = new BindingMap<string, Entry>();

  // The level of the prefix represents how many type variables there currently
  // are in this prefix. We use this to determine if a type variable will die
  // too soon.
  private level: number;

  // Used to generate human-readable type variable names.
  private generatedName: number;

  // Lookups that miss in the local prefix go to the parent prefix if one
  // exists. Used for “forking” the prefix like in unification where we need to
  // separate prefixes for each of our types.
  //
  // The parent prefix really shouldn’t be mutated while we are operating on a
  // child prefix but we currently do not force this invariant.
  private readonly parent: Prefix | undefined;

  constructor(parent?: Prefix) {
    this.level = parent !== undefined ? parent.level : 0;
    this.generatedName = parent !== undefined ? parent.generatedName : 0;
    this.parent = parent;
  }

  /**
   * Adds a new type variable with the provided bound to the prefix.
   *
   * Calling `Prefix.pop()` removes the last type variable to be pushed from
   * the prefix.
   */
  push(binding: string, bound: Bound) {
    const level = this.level++;
    this.bindings.push(binding, {
      level,
      bound,
      // Has no dependencies on type variables with a larger level since
      // currently this type variable has the largest level.
      dependencies: [],
      // All type variables start with one dependent. The prefix itself!
      dependents: 1,
      generatedName: undefined,
    });
  }

  /**
   * Generates a unique name and adds a new type variable with the provided
   * bound to the prefix under that unique name.
   *
   * Calling `Prefix.pop()` removes the last type variable to be pushed from
   * the prefix.
   */
  pushWithGeneratedName(bound: Bound): string {
    // Generate a unique name in this prefix.
    let binding = generateName(this.generatedName);
    while (this.has(binding)) {
      binding = generateName(this.generatedName++);
    }
    // Increment the generated name so we don’t try and reuse this one.
    const generatedName = this.generatedName++;
    const level = this.level++;
    this.bindings.push(binding, {
      level,
      bound,
      // Has no dependencies on type variables with a larger level since
      // currently this type variable has the largest level.
      dependencies: [],
      // All type variables start with one dependent. The prefix itself!
      dependents: 1,
      generatedName,
    });
    return binding;
  }

  /**
   * Removes the last binding to be pushed from the prefix and uses that binding
   * to quantify the provided type.
   *
   * If a type variable that is still in the prefix was updated to depend on
   * the last type variable to be pushed then we only remove the last type
   * variable from the scope and we don’t quantify the provided type.
   *
   * If a type variable depends on some other type variables which are no longer
   * accessible in the scope then we will also quantify the provided type with
   * those type variables.
   *
   * For example, say we have a prefix which looks like:
   *
   * ```
   * a = ⊥        // Level 1
   * b = number   // Level 2
   * c = b → b    // Level 3
   * ```
   *
   * Then we updated `a` to `c` so now we have
   *
   * ```
   * a = c        // Level 1
   * b = number   // Level 2
   * c = b → b    // Level 3
   * ```
   *
   * `c` and `b` are going to be popped off the prefix when we call
   * `Prefix.pop()`, but `a` still depends on them! So when we call
   * `Prefix.pop(T)` instead of quantifying `T` with `c` we return `T`
   * unmodified. We do the same after calling `Prefix.pop(U)` a second time when
   * we would have quantified `U` with `b` if `a` did not depend on it.
   *
   * Finally, when we call `Prefix.pop(V)` we get
   * `∀(b = number).∀(c = b → b).∀(a = c).V`. Popping that third time gave us
   * all of our types at once since `a` depended on them.
   */
  pop(type: Type): Type {
    const last = this.bindings.pop();
    if (last === undefined) return type;
    const {key: binding, value: entry} = last;
    this.level--;
    if (entry.generatedName !== undefined) {
      this.generatedName = entry.generatedName;
    }
    return quantify(binding, entry, type);
  }

  /**
   * Gets an entry in the prefix. If it does not exist locally then we check our
   * parent prefix.
   */
  private getEntry(binding: string): Entry | undefined {
    const entry = this.bindings.get(binding);
    if (entry !== undefined) {
      return entry;
    } else if (this.parent !== undefined) {
      return this.parent.getEntry(binding);
    } else {
      return undefined;
    }
  }

  /**
   * Gets a bound in the prefix. If it does not exist locally then we check our
   * parent prefix.
   */
  get(binding: string): Bound | undefined {
    const entry = this.getEntry(binding);
    return entry !== undefined ? entry.bound : undefined;
  }

  /**
   * Checks to see if a bound exists in the prefix. If it does not exist locally
   * then we check our parent prefix.
   */
  has(binding: string): boolean {
    return (
      this.bindings.has(binding) ||
      (this.parent !== undefined ? this.parent.has(binding) : false)
    );
  }

  /**
   * Returns true if a type variable with the provided identifier _is not_
   * defined in the local prefix but _is_ defined in the parent prefix.
   */
  isInParent(binding: string): boolean {
    if (this.parent === undefined) return false;
    if (this.bindings.has(binding)) return false;
    return this.parent.has(binding);
  }

  /**
   * Updates a type variable `binding` in `updatePrefix`. While using
   * `boundPrefix` for lookups of free type variables in the `bound` type.
   *
   * If the bound references type variables that will be removed by
   * `Prefix.pop()` before the type variable we are updating, then we will set
   * a dependency on the free type variables to make sure they don’t die.
   */
  static update(
    updatePrefix: Prefix,
    boundPrefix: Prefix,
    binding: string,
    bound: Bound
  ) {
    const entry = updatePrefix.getEntry(binding);
    if (entry === undefined) throw new Error('Type variable not found.');
    // Update the type variable entry to the new bound. Also clear the entry’s
    // dependencies since a new type will have new dependencies. Make sure to
    // decrement the dependents count for old dependencies.
    entry.bound = bound;
    for (const dependency of entry.dependencies) dependency.entry.dependents--;
    entry.dependencies = [];
    // Look at all the free type variables in our bound’s type. We need to add
    // dependencies for those which will be `Prefix.pop()`ed before the type
    // variable we are updating.
    for (const dependencyBinding of Type.getFreeVariables(bound.type)) {
      const dependencyEntry = boundPrefix.getEntry(dependencyBinding);
      if (dependencyEntry === undefined) continue;
      // If this dependency is at a higher level then `entry` we need to add
      // this dependency to the `entry` dependency array. If it is not then we
      // know this dependency will be always be in scope for `entry`.
      if (dependencyEntry.level >= entry.level) {
        dependencyEntry.level = entry.level;
        // Add a dependency to `entry` and increment our dependency entry’s
        // dependent count.
        dependencyEntry.dependents++;
        entry.dependencies.push({
          binding: dependencyBinding,
          entry: dependencyEntry,
        });
      }
    }
  }
}

type Entry = {
  level: number;
  bound: Bound;
  dependencies: Array<{readonly binding: string; readonly entry: Entry}>;
  dependents: number;
  readonly generatedName: number | undefined;
};

/**
 * Quantifies the provided type by the binding and its dependencies if the
 * binding has no dependents.
 */
function quantify(binding: string, entry: Entry, type: Type): Type {
  // If there are still more then zero dependents we can’t quantify this binding
  // just yet.
  entry.dependents--;
  if (entry.dependents < 0) throw new Error('Less than zero dependents.');
  if (entry.dependents > 0) return type;
  // If we have a bottom type then there are no free type variables so don’t
  // bother quantifying.
  if (type.kind !== 'Bottom') {
    // TODO: Naming collisions?
    type = t.quantifiedType(binding, entry.bound, type);
  }
  // Recursively try and quantify with all our dependencies. We will quantify
  // dependencies with no more dependents.
  for (let i = entry.dependencies.length - 1; i >= 0; i--) {
    const dependency = entry.dependencies[i];
    if (dependency.entry.dependents === 0) continue;
    type = quantify(dependency.binding, dependency.entry, type);
  }
  return type;
}

/**
 * Get a human readable name from a counter value.
 */
function generateName(number: number): string {
  if (number >= letters.length) {
    return `t${number - letters.length + 1}`;
  } else {
    return letters[number];
  }
}

// All the ASCII lowercase letters. We use these for creating type
// variable names.
const letters: ReadonlyArray<string> = 'abcdefghijklmnopqrstuvwxyz'.split('');
