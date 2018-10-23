import {Prefix} from './prefix';
import {equivalent} from './relation';
import {Polytype, Type} from './type';

const equivalenceSuccess: ReadonlyArray<{
  readonly only?: boolean;
  readonly a: Polytype;
  readonly b: Polytype;
}> = [
  {a: Type.unit, b: Type.unit},
  {a: Type.boolean, b: Type.boolean},
  {a: Type.number, b: Type.number},
  {a: Type.string, b: Type.string},
  {a: Type.bottom, b: Type.bottom},
  {
    a: Type.function_(Type.number, Type.number),
    b: Type.function_(Type.number, Type.number),
  },
  {
    a: Type.quantify('x', Type.flexibleBound(Type.unit), Type.variable('x')),
    b: Type.quantify('x', Type.flexibleBound(Type.unit), Type.variable('x')),
  },
  {
    a: Type.quantify('x', Type.rigidBound(Type.unit), Type.variable('x')),
    b: Type.quantify('x', Type.rigidBound(Type.unit), Type.variable('x')),
  },
  {
    a: Type.quantify(
      'x',
      Type.rigidBound(Type.unit),
      Type.function_(Type.number, Type.variable('x'))
    ),
    b: Type.quantify(
      'x',
      Type.rigidBound(Type.unit),
      Type.function_(Type.number, Type.variable('x'))
    ),
  },
  {
    a: Type.quantify('x', Type.rigidBound(Type.unit), Type.number),
    b: Type.number,
  },
  {
    a: Type.quantifyUnbounded('x', Type.variable('x')),
    b: Type.quantifyUnbounded('x', Type.variable('x')),
  },
  {
    a: Type.quantifyUnbounded(
      'x',
      Type.function_(Type.variable('x'), Type.variable('x'))
    ),
    b: Type.quantifyUnbounded(
      'x',
      Type.function_(Type.variable('x'), Type.variable('x'))
    ),
  },
  {
    a: Type.quantifyUnbounded(
      'y',
      Type.quantifyUnbounded(
        'x',
        Type.function_(Type.variable('x'), Type.variable('x'))
      )
    ),
    b: Type.quantifyUnbounded(
      'x',
      Type.function_(Type.variable('x'), Type.variable('x'))
    ),
  },
  {
    a: Type.quantifyUnbounded(
      'x',
      Type.quantifyUnbounded(
        'y',
        Type.function_(Type.variable('x'), Type.variable('x'))
      )
    ),
    b: Type.quantifyUnbounded(
      'x',
      Type.function_(Type.variable('x'), Type.variable('x'))
    ),
  },
  {
    a: Type.quantifyUnbounded(
      'a',
      Type.quantifyUnbounded(
        'b',
        Type.function_(Type.variable('a'), Type.variable('b'))
      )
    ),
    b: Type.quantifyUnbounded(
      'b',
      Type.quantifyUnbounded(
        'a',
        Type.function_(Type.variable('a'), Type.variable('b'))
      )
    ),
  },
  {
    a: Type.quantifyUnbounded(
      'x',
      Type.quantify(
        'y',
        Type.rigidBound(Type.variable('x')),
        Type.quantifyUnbounded(
          'x',
          Type.function_(Type.variable('x'), Type.variable('y'))
        )
      )
    ),
    b: Type.quantifyUnbounded(
      'x',
      Type.quantify(
        'y',
        Type.rigidBound(Type.variable('x')),
        Type.quantifyUnbounded(
          'x',
          Type.function_(Type.variable('x'), Type.variable('y'))
        )
      )
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(
        Type.quantifyUnbounded(
          'z',
          Type.function_(Type.variable('z'), Type.variable('z'))
        )
      ),
      Type.quantify(
        'y',
        Type.rigidBound(Type.variable('x')),
        Type.quantifyUnbounded(
          'x',
          Type.function_(
            Type.variable('x'),
            Type.function_(Type.variable('x'), Type.variable('y'))
          )
        )
      )
    ),
    b: Type.quantify(
      'x',
      Type.flexibleBound(
        Type.quantifyUnbounded(
          'z',
          Type.function_(Type.variable('z'), Type.variable('z'))
        )
      ),
      Type.quantify(
        'y',
        Type.rigidBound(Type.variable('x')),
        Type.quantifyUnbounded(
          'x',
          Type.function_(
            Type.variable('x'),
            Type.function_(Type.variable('x'), Type.variable('y'))
          )
        )
      )
    ),
  },
  {
    a: Type.quantify('x', Type.flexibleBound(Type.number), Type.variable('x')),
    b: Type.quantify('y', Type.flexibleBound(Type.number), Type.variable('y')),
  },
  {
    a: Type.quantify('x', Type.rigidBound(Type.number), Type.variable('x')),
    b: Type.quantify('y', Type.rigidBound(Type.number), Type.variable('y')),
  },
  {
    a: Type.quantify('x', Type.flexibleBound(Type.number), Type.variable('x')),
    b: Type.number,
  },
  {
    a: Type.quantify('x', Type.rigidBound(Type.number), Type.variable('x')),
    b: Type.number,
  },
  {
    a: Type.quantify('x', Type.flexibleBound(Type.number), Type.variable('x')),
    b: Type.quantify('x', Type.rigidBound(Type.number), Type.variable('x')),
  },
  {
    a: Type.quantify(
      'x',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('x')
    ),
    b: Type.quantify(
      'y',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('y')
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('x')
    ),
    b: Type.quantify(
      'y',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('y')
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('x')
    ),
    b: Type.quantify(
      'x',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('x')
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('x')
    ),
    b: Type.quantify(
      'y',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('y')
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('x')
    ),
    b: Type.unit,
  },
  {
    a: Type.quantify(
      'x',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.unit)),
      Type.variable('x')
    ),
    b: Type.unit,
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(
        Type.quantifyUnbounded('z', Type.function_(Type.unit, Type.unit))
      ),
      Type.variable('x')
    ),
    b: Type.quantify(
      'x',
      Type.rigidBound(
        Type.quantifyUnbounded(
          'z',
          Type.quantifyUnbounded('z', Type.function_(Type.unit, Type.unit))
        )
      ),
      Type.variable('x')
    ),
  },
];

const equivalenceFailure: ReadonlyArray<{
  readonly only?: boolean;
  readonly a: Polytype;
  readonly b: Polytype;
}> = [
  {a: Type.unit, b: Type.boolean},
  {a: Type.boolean, b: Type.number},
  {a: Type.number, b: Type.string},
  {a: Type.string, b: Type.unit},
  {a: Type.bottom, b: Type.unit},
  {
    a: Type.function_(Type.number, Type.number),
    b: Type.function_(Type.string, Type.string),
  },
  {
    a: Type.function_(Type.number, Type.number),
    b: Type.function_(Type.string, Type.number),
  },
  {
    a: Type.function_(Type.number, Type.number),
    b: Type.function_(Type.number, Type.string),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
    b: Type.quantify(
      'x',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
    b: Type.quantify(
      'y',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('y')
    ),
  },
  {
    a: Type.quantify('x', Type.flexibleBound(Type.number), Type.variable('x')),
    b: Type.quantify('x', Type.flexibleBound(Type.string), Type.variable('x')),
  },
  {
    a: Type.quantify('x', Type.rigidBound(Type.number), Type.variable('x')),
    b: Type.quantify('x', Type.rigidBound(Type.string), Type.variable('x')),
  },
  {
    a: Type.quantify(
      'x',
      Type.rigidBound(Type.unit),
      Type.function_(Type.number, Type.variable('x'))
    ),
    b: Type.quantify(
      'x',
      Type.rigidBound(Type.unit),
      Type.function_(Type.string, Type.variable('x'))
    ),
  },
  {
    a: Type.quantify('x', Type.rigidBound(Type.unit), Type.number),
    b: Type.string,
  },
  {
    a: Type.quantifyUnbounded('x', Type.variable('x')),
    b: Type.quantifyUnbounded('y', Type.variable('y')),
  },
  {
    a: Type.quantifyUnbounded(
      'x',
      Type.function_(Type.variable('x'), Type.variable('x'))
    ),
    b: Type.quantifyUnbounded(
      'y',
      Type.function_(Type.variable('y'), Type.variable('y'))
    ),
  },
  {
    a: Type.quantifyUnbounded(
      'a',
      Type.quantifyUnbounded(
        'b',
        Type.function_(Type.variable('a'), Type.variable('b'))
      )
    ),
    b: Type.quantifyUnbounded(
      'a',
      Type.quantifyUnbounded(
        'b',
        Type.function_(Type.variable('b'), Type.variable('a'))
      )
    ),
  },
  {
    a: Type.quantifyUnbounded(
      'a',
      Type.quantifyUnbounded(
        'b',
        Type.function_(Type.variable('a'), Type.variable('b'))
      )
    ),
    b: Type.quantifyUnbounded(
      'b',
      Type.quantifyUnbounded(
        'a',
        Type.function_(Type.variable('b'), Type.variable('a'))
      )
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(
        Type.quantifyUnbounded(
          'z',
          Type.function_(Type.variable('z'), Type.variable('z'))
        )
      ),
      Type.quantify(
        'y',
        Type.rigidBound(Type.variable('x')),
        Type.quantifyUnbounded(
          'x',
          Type.function_(Type.variable('x'), Type.variable('y'))
        )
      )
    ),
    b: Type.quantify(
      'x',
      Type.flexibleBound(
        Type.quantifyUnbounded(
          'z',
          Type.function_(Type.variable('z'), Type.unit)
        )
      ),
      Type.quantify(
        'y',
        Type.rigidBound(Type.variable('x')),
        Type.quantifyUnbounded(
          'x',
          Type.function_(Type.variable('x'), Type.variable('y'))
        )
      )
    ),
  },
  {
    a: Type.quantifyUnbounded(
      'x',
      Type.quantify(
        'y',
        Type.rigidBound(Type.variable('x')),
        Type.quantify(
          'x',
          Type.flexibleBound(
            Type.quantifyUnbounded(
              'z',
              Type.function_(Type.variable('z'), Type.variable('z'))
            )
          ),
          Type.function_(
            Type.variable('x'),
            Type.function_(Type.variable('x'), Type.variable('y'))
          )
        )
      )
    ),
    b: Type.quantifyUnbounded(
      'x',
      Type.quantify(
        'y',
        Type.rigidBound(Type.variable('x')),
        Type.quantify(
          'x',
          Type.flexibleBound(
            Type.quantifyUnbounded(
              'z',
              Type.function_(Type.variable('z'), Type.unit)
            )
          ),
          Type.function_(
            Type.variable('x'),
            Type.function_(Type.variable('x'), Type.variable('y'))
          )
        )
      )
    ),
  },
  {
    a: Type.quantify('x', Type.flexibleBound(Type.number), Type.variable('x')),
    b: Type.quantify('y', Type.flexibleBound(Type.string), Type.variable('y')),
  },
  {
    a: Type.quantify('x', Type.rigidBound(Type.number), Type.variable('x')),
    b: Type.quantify('y', Type.rigidBound(Type.string), Type.variable('y')),
  },
  {
    a: Type.quantify('x', Type.flexibleBound(Type.number), Type.variable('x')),
    b: Type.string,
  },
  {
    a: Type.quantify('x', Type.rigidBound(Type.number), Type.variable('x')),
    b: Type.string,
  },
  {
    a: Type.quantify('x', Type.flexibleBound(Type.number), Type.variable('x')),
    b: Type.quantify('x', Type.rigidBound(Type.string), Type.variable('x')),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
    b: Type.quantify(
      'x',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
    b: Type.quantify(
      'y',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('y')
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(
        Type.quantifyUnbounded(
          'z',
          Type.function_(Type.variable('z'), Type.unit)
        )
      ),
      Type.variable('x')
    ),
    b: Type.quantify(
      'x',
      Type.rigidBound(
        Type.quantifyUnbounded(
          'z',
          Type.quantifyUnbounded(
            'z',
            Type.function_(Type.variable('z'), Type.unit)
          )
        )
      ),
      Type.variable('x')
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
    b: Type.unit,
  },
  {
    a: Type.quantify(
      'x',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
    b: Type.unit,
  },
];

describe('equivalence', () => {
  for (const {only, a, b} of equivalenceSuccess) {
    const declareTest = only ? test.only : test;
    const name =
      Polytype.toDisplayString(a) + ' ≡ ' + Polytype.toDisplayString(b);
    declareTest(name, () => {
      expect(equivalent(Prefix.empty, a, b)).toEqual(true);
      expect(equivalent(Prefix.empty, b, a)).toEqual(true);
    });
  }

  for (const {only, a, b} of equivalenceFailure) {
    const declareTest = only ? test.only : test;
    const name =
      Polytype.toDisplayString(a) + ' ≢ ' + Polytype.toDisplayString(b);
    declareTest(name, () => {
      expect(equivalent(Prefix.empty, a, b)).toEqual(false);
      expect(equivalent(Prefix.empty, b, a)).toEqual(false);
    });
  }
});
