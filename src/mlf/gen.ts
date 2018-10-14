import * as Immutable from 'immutable';
import {Generator, check, gen, property} from 'testcheck';

import {TypeIdentifier} from './identifier';
import {Prefix} from './prefix';
import {Bound, MonomorphicType, Type} from './type';

/**
 * Testcheck.js runner for Jest.
 */
export function testCheck<T>(
  name: string,
  generator: Generator<T>,
  f: (value: T) => boolean
) {
  test(`testcheck: ${name}`, () => {
    const prop = property(generator, f);
    const {fail} = check(prop);
    expect(fail).toBe(undefined);
  });
}

type StateType = {
  readonly prefix: Prefix;
};

/**
 * A state monad to provide some state when generating things.
 */
class State<T> {
  public static return<T>(value: T): State<T> {
    return new State(state => ({value, state}));
  }

  public static then2<T, U, V>(
    monad1: State<T>,
    monad2: State<U>,
    g: (value1: T, value2: U) => State<V>
  ): State<V> {
    return new State(state => {
      const {value: value1, state: state1} = monad1.f(state);
      const {value: value2, state: state2} = monad2.f(state1);
      return g(value1, value2).f(state2);
    });
  }

  public static add(bound: Bound): State<TypeIdentifier> {
    return new State(state => {
      const {prefix, identifier} = state.prefix.add(bound);
      return {value: identifier, state: {...state, prefix}};
    });
  }

  public static quantify(monad: State<Type>): State<Type> {
    return new State(state0 => {
      const state1 = {
        ...state0,
        prefix: state0.prefix.pushScope(Immutable.Map()),
      };
      const {value: type, state: state2} = monad.f(state1);
      const {prefix: prefix3, bindings} = state2.prefix.popScope();
      const state3 = {...state2, prefix: prefix3};
      return {
        value: createQuantifiedType(bindings, type),
        state: state3,
      };
    });
  }

  private readonly f: (state: StateType) => {value: T; state: StateType};

  private constructor(f: (state: StateType) => {value: T; state: StateType}) {
    this.f = f;
  }

  public then<U>(g: (value: T) => State<U>): State<U> {
    return new State(state => {
      const {value: value1, state: state1} = this.f(state);
      return g(value1).f(state1);
    });
  }

  public run(): {
    readonly value: T;
    readonly prefix: Prefix;
  } {
    const {value, state} = this.f({
      prefix: Prefix.empty,
    });
    return {
      value,
      prefix: state.prefix,
    };
  }
}

/**
 * Generates a polymorphic or monomorphic type.
 */
export const genType: Generator<State<Type>> = gen.nested(
  genType =>
    gen.oneOfWeighted<State<Type>>([
      // Variable type
      [
        10,
        gen
          .array([
            gen.oneOf<'flexible' | 'rigid'>(['flexible', 'rigid']),
            genType,
          ])
          .then(([kind, type]) =>
            type.then(type =>
              State.add({kind, type}).then(identifier =>
                State.return<Type>({kind: 'Variable', identifier})
              )
            )
          ),
      ],

      // Function type
      [
        10,
        gen.array([genType, genType]).then(([parameter, body]) =>
          State.then2(
            intoMonomorphicType(parameter),
            intoMonomorphicType(body),
            (parameter, body) =>
              State.return<MonomorphicType>({
                kind: 'Function',
                parameter,
                body,
              })
          )
        ),
      ],

      // Quantified type
      [5, genType.then(State.quantify)],

      // Ignored type variable
      [
        1,
        gen
          .array([
            gen.oneOf<'flexible' | 'rigid'>(['flexible', 'rigid']),
            genType,
            genType,
          ])
          .then(([kind, boundType, type]) =>
            boundType.then(boundType =>
              State.add({kind, type: boundType}).then(() => type)
            )
          ),
      ],
    ]),
  gen.oneOfWeighted([
    // Boolean type
    [3, State.return<Type>({kind: 'Constant', constant: {kind: 'Boolean'}})],

    // Number type
    [3, State.return<Type>({kind: 'Constant', constant: {kind: 'Number'}})],

    // String type
    [3, State.return<Type>({kind: 'Constant', constant: {kind: 'String'}})],

    // Bottom type
    [1, State.return<Type>({kind: 'Bottom'})],
  ])
);

/**
 * Generates a monomorphic type.
 */
export const genMonomorphicType: Generator<
  State<MonomorphicType>
> = genType.then(intoMonomorphicType);

function intoMonomorphicType(monad: State<Type>): State<MonomorphicType> {
  return monad.then(
    type =>
      type.kind === 'Quantified' || type.kind === 'Bottom'
        ? State.add({kind: 'rigid', type}).then(identifier =>
            State.return<MonomorphicType>({kind: 'Variable', identifier})
          )
        : State.return(type)
  );
}

function createQuantifiedType(
  prefix: Immutable.Map<TypeIdentifier, Bound>,
  body: Type
): Type {
  if (body.kind === 'Quantified') {
    return {
      kind: 'Quantified',
      prefix: prefix.merge(body.prefix),
      body: body.body,
    };
  } else {
    return {kind: 'Quantified', prefix, body};
  }
}
