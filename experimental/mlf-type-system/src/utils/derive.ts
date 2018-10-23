export abstract class Derivable<T> {
  protected readonly derivations: Array<Derivation<unknown>> | undefined;

  abstract get(): T;

  then<U>(compute: (value: T) => U): Derivable<U> {
    const derivation = new Derivation(() => compute(this.get()));
    if (this.derivations !== undefined) this.derivations.push(derivation);
    return derivation;
  }

  static then2<A, B, C>(
    a: Derivable<A>,
    b: Derivable<B>,
    compute: (a: A, b: B) => C
  ): Derivable<C> {
    const derivation = new Derivation(() => compute(a.get(), b.get()));
    if (a.derivations !== undefined) a.derivations.push(derivation);
    if (b.derivations !== undefined) b.derivations.push(derivation);
    return derivation;
  }
}

export class DerivableValue<T> extends Derivable<T> {
  private value: T;
  protected readonly derivations: Array<Derivation<unknown>> = [];

  constructor(value: T) {
    super();
    this.value = value;
  }

  set(value: T) {
    this.value = value;
    for (let i = 0; i < this.derivations.length; i++) {
      this.derivations[i].invalidate();
    }
  }

  get(): T {
    return this.value;
  }
}

export class DerivableConstant<T> extends Derivable<T> {
  private readonly value: T;

  constructor(value: T) {
    super();
    this.value = value;
  }

  get(): T {
    return this.value;
  }
}

class Derivation<T> extends Derivable<T> {
  private invalid = true;
  private value: T | undefined = undefined;
  private compute: () => T;
  protected readonly derivations: Array<Derivation<unknown>> = [];

  constructor(compute: () => T) {
    super();
    this.compute = compute;
  }

  invalidate() {
    if (this.invalid === false) {
      this.invalid = true;
      this.value = undefined;
      for (let i = 0; i < this.derivations.length; i++) {
        this.derivations[i].invalidate();
      }
    }
  }

  get(): T {
    if (this.invalid === true) {
      this.invalid = false;
      this.value = this.compute();
    }
    return this.value!; // tslint:disable-line no-non-null-assertion
  }
}
