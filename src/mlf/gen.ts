import {Generator, check, gen, property} from 'testcheck';

import {Prefix} from './prefix';
import {MonomorphicType, Type} from './type';

const OK = Symbol('OK');

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
    const {result, fail} = check(prop);
    if (typeof result !== 'boolean') {
      throw result;
    }
    expect(fail ? fail[0] : OK).toBe(OK);
  });
}

/**
 * Generates a polymorphic or monomorphic type.
 */
export const genType: Generator<(prefix: Prefix) => Type> = gen.nested(
  genType =>
    gen.oneOfWeighted<(prefix: Prefix) => Type>([
      // Variable type
      [
        10,
        gen
          .array([
            gen.oneOf<'flexible' | 'rigid'>(['flexible', 'rigid']),
            genType,
          ])
          .then(([kind, createType]) => (prefix: Prefix): Type => {
            const type = createType(prefix);
            const identifier = prefix.add({kind, type});
            return {kind: 'Variable', identifier};
          }),
      ],

      // Function type
      [
        10,
        gen
          .array([genType, genType])
          .then(
            ([createParameterType, createBodyType]) => (
              prefix: Prefix
            ): Type => {
              const parameter = intoMonomorphicType(
                prefix,
                createParameterType(prefix)
              );
              const body = intoMonomorphicType(prefix, createBodyType(prefix));
              return {kind: 'Function', parameter, body};
            }
          ),
      ],

      // Quantified type
      [
        5,
        genType.then(createType => (prefix: Prefix): Type => {
          prefix.captureStart();
          const type = createType(prefix);
          const bindings = prefix.captureStop();
          if (type.kind === 'Quantified') {
            for (const [identifier, bound] of type.prefix) {
              bindings.set(identifier, bound);
            }
            return {kind: 'Quantified', prefix: bindings, body: type.body};
          } else {
            return {kind: 'Quantified', prefix: bindings, body: type};
          }
        }),
      ],

      // Ignored type variable
      [
        1,
        gen
          .array([
            gen.oneOf<'flexible' | 'rigid'>(['flexible', 'rigid']),
            genType,
            genType,
          ])
          .then(
            ([kind, createBoundType, createType]) => (prefix: Prefix): Type => {
              prefix.add({kind, type: createBoundType(prefix)});
              return createType(prefix);
            }
          ),
      ],
    ]),
  gen
    .oneOfWeighted([
      // Boolean type
      [3, gen.return<Type>({kind: 'Constant', constant: {kind: 'Boolean'}})],

      // Number type
      [3, gen.return<Type>({kind: 'Constant', constant: {kind: 'Number'}})],

      // String type
      [3, gen.return<Type>({kind: 'Constant', constant: {kind: 'String'}})],

      // Bottom type
      [1, gen.return<Type>({kind: 'Bottom'})],
    ])
    .then(type => (_prefix: Prefix) => type)
);

/**
 * Generates a monomorphic type.
 */
export const genMonomorphicType: Generator<
  (prefix: Prefix) => MonomorphicType
> = genType.then(createType => (prefix: Prefix) =>
  intoMonomorphicType(prefix, createType(prefix))
);

function intoMonomorphicType(prefix: Prefix, type: Type): MonomorphicType {
  if (type.kind === 'Quantified' || type.kind === 'Bottom') {
    const identifier = prefix.add({kind: 'rigid', type});
    return {kind: 'Variable', identifier};
  } else {
    return type;
  }
}
