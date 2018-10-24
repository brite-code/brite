import {equivalent} from './equivalence';
import {Prefix} from './prefix';
import {Polytype, Type} from './type';

const equivalenceSuccess: ReadonlyArray<{
  readonly only?: boolean;
  readonly prefix?: Prefix;
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
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.quantify(
        'x',
        Type.flexibleBound(Type.variable('x')),
        Type.variable('x')
      )
    ),
    b: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
  },
  {
    a: Type.quantify(
      'a',
      Type.flexibleBound(Type.unit),
      Type.function_(Type.variable('a'), Type.variable('a'))
    ),
    b: Type.function_(Type.unit, Type.unit),
  },
  {
    a: Type.quantify(
      'a',
      Type.rigidBound(Type.unit),
      Type.function_(Type.variable('a'), Type.variable('a'))
    ),
    b: Type.function_(Type.unit, Type.unit),
  },
  {
    a: Type.quantify(
      'a',
      Type.rigidBound(Type.quantifyUnbounded('x', Type.variable('x'))),
      Type.variable('a')
    ),
    b: Type.quantifyUnbounded('x', Type.variable('x')),
  },
  {
    a: Type.quantify(
      'a',
      Type.flexibleBound(Type.quantifyUnbounded('x', Type.variable('x'))),
      Type.variable('a')
    ),
    b: Type.quantifyUnbounded('x', Type.variable('x')),
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
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('x')
    ),
    b: Type.quantify(
      'y',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.variable('y')
    ),
  },
  {
    a: Type.quantifyUnbounded('x', Type.variable('x')),
    b: Type.quantifyUnbounded('y', Type.variable('y')),
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
      'a',
      Type.flexibleBound(Type.quantifyUnbounded('b', Type.variable('b'))),
      Type.variable('a')
    ),
    b: Type.quantifyUnbounded('b', Type.variable('b')),
  },
  {
    a: Type.quantify(
      'a',
      Type.rigidBound(Type.quantifyUnbounded('b', Type.variable('b'))),
      Type.variable('a')
    ),
    b: Type.quantifyUnbounded('b', Type.variable('b')),
  },
  {
    prefix: Prefix.empty
      .add('b', Type.flexibleBound(Type.bottom))
      .add(
        'a',
        Type.rigidBound(Type.function_(Type.variable('b'), Type.variable('b')))
      ),
    a: Type.variable('a'),
    b: Type.function_(Type.variable('b'), Type.variable('b')),
  },
  {
    prefix: Prefix.empty
      .add('b', Type.flexibleBound(Type.bottom))
      .add(
        'a',
        Type.rigidBound(Type.function_(Type.variable('b'), Type.variable('b')))
      ),
    a: Type.function_(Type.variable('a'), Type.unit),
    b: Type.function_(
      Type.function_(Type.variable('b'), Type.variable('b')),
      Type.unit
    ),
  },
  {
    a: Type.quantifyUnbounded(
      'a',
      Type.quantify(
        'b',
        Type.rigidBound(Type.function_(Type.variable('a'), Type.variable('a'))),
        Type.function_(Type.variable('b'), Type.variable('b'))
      )
    ),
    b: Type.quantifyUnbounded(
      'a',
      Type.function_(
        Type.function_(Type.variable('a'), Type.variable('a')),
        Type.function_(Type.variable('a'), Type.variable('a'))
      )
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.rigidBound(Type.unit),
      Type.quantify(
        'x',
        Type.rigidBound(Type.variable('x')),
        Type.variable('x')
      )
    ),
    b: Type.unit,
  },
];

const equivalenceFailure: ReadonlyArray<{
  readonly only?: boolean;
  readonly prefix?: Prefix;
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
      Type.function_(Type.variable('x'), Type.unit)
    ),
    b: Type.quantify(
      'x',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.function_(Type.variable('x'), Type.unit)
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.function_(Type.variable('x'), Type.unit)
    ),
    b: Type.quantify(
      'y',
      Type.rigidBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.function_(Type.variable('y'), Type.unit)
    ),
  },
  {
    a: Type.quantify(
      'x',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.function_(Type.variable('x'), Type.unit)
    ),
    b: Type.quantify(
      'y',
      Type.flexibleBound(Type.quantifyUnbounded('z', Type.variable('z'))),
      Type.function_(Type.variable('y'), Type.unit)
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
    a: Type.quantifyUnbounded(
      'x',
      Type.function_(Type.variable('x'), Type.unit)
    ),
    b: Type.quantifyUnbounded(
      'y',
      Type.function_(Type.variable('y'), Type.unit)
    ),
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
      Type.flexibleBound(
        Type.quantifyUnbounded(
          'z',
          Type.function_(Type.variable('z'), Type.unit)
        )
      ),
      Type.function_(Type.variable('x'), Type.unit)
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
      Type.function_(Type.variable('x'), Type.unit)
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
  {
    a: Type.quantify(
      'a',
      Type.flexibleBound(Type.unit),
      Type.function_(Type.variable('a'), Type.variable('a'))
    ),
    b: Type.function_(Type.boolean, Type.unit),
  },
  {
    a: Type.quantify(
      'a',
      Type.flexibleBound(Type.quantifyUnbounded('b', Type.variable('b'))),
      Type.function_(Type.variable('a'), Type.unit)
    ),
    b: Type.quantifyUnbounded(
      'b',
      Type.function_(Type.variable('b'), Type.unit)
    ),
  },
  {
    a: Type.quantify(
      'a',
      Type.rigidBound(Type.quantifyUnbounded('b', Type.variable('b'))),
      Type.function_(Type.variable('a'), Type.unit)
    ),
    b: Type.quantifyUnbounded(
      'b',
      Type.function_(Type.variable('b'), Type.unit)
    ),
  },
  {
    // In Definition 1.5.1 of the [MLF Thesis][1] this example is called out
    // explicitly as one the equivalence relation should not support.
    //
    // [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf

    a: Type.quantify(
      'a1',
      Type.rigidBound(Type.quantifyUnbounded('x', Type.variable('x'))),
      Type.quantify(
        'a2',
        Type.rigidBound(Type.quantifyUnbounded('x', Type.variable('x'))),
        Type.function_(Type.variable('a1'), Type.variable('a2'))
      )
    ),
    b: Type.quantify(
      'a',
      Type.rigidBound(Type.quantifyUnbounded('x', Type.variable('x'))),
      Type.function_(Type.variable('a'), Type.variable('a'))
    ),
  },
  {
    a: Type.quantify(
      'a1',
      Type.flexibleBound(Type.quantifyUnbounded('x', Type.variable('x'))),
      Type.quantify(
        'a2',
        Type.flexibleBound(Type.quantifyUnbounded('x', Type.variable('x'))),
        Type.function_(Type.variable('a1'), Type.variable('a2'))
      )
    ),
    b: Type.quantify(
      'a',
      Type.flexibleBound(Type.quantifyUnbounded('x', Type.variable('x'))),
      Type.function_(Type.variable('a'), Type.variable('a'))
    ),
  },
];

for (const {only, prefix, a, b} of equivalenceSuccess) {
  const declareTest = only ? test.only : test;
  let name = Polytype.toDisplayString(a) + ' ≡ ' + Polytype.toDisplayString(b);
  if (prefix) {
    name = `${prefix.toDisplayString()} ${name}`;
  }
  declareTest(name, () => {
    expect(equivalent(prefix || Prefix.empty, a, b)).toEqual(true);
    expect(equivalent(prefix || Prefix.empty, b, a)).toEqual(true);
    if (a.kind === 'Quantify') {
      let prefix2 = prefix || Prefix.empty;
      let a2: Polytype = a;
      while (a2.kind === 'Quantify') {
        prefix2 = prefix2.add(a2.name, a2.bound);
        a2 = a2.body;
      }
      expect(equivalent(prefix2, a2, b)).toEqual(true);
      expect(equivalent(prefix2, b, a2)).toEqual(true);
    }
    if (b.kind === 'Quantify') {
      let prefix2 = prefix || Prefix.empty;
      let b2: Polytype = b;
      while (b2.kind === 'Quantify') {
        prefix2 = prefix2.add(b2.name, b2.bound);
        b2 = b2.body;
      }
      expect(equivalent(prefix2, a, b2)).toEqual(true);
      expect(equivalent(prefix2, b2, a)).toEqual(true);
    }
  });
}

for (const {only, prefix, a, b} of equivalenceFailure) {
  const declareTest = only ? test.only : test;
  let name = Polytype.toDisplayString(a) + ' ≢ ' + Polytype.toDisplayString(b);
  if (prefix) {
    name = `${prefix.toDisplayString()} ${name}`;
  }
  declareTest(name, () => {
    expect(equivalent(prefix || Prefix.empty, a, b)).toEqual(false);
    expect(equivalent(prefix || Prefix.empty, b, a)).toEqual(false);
    if (a.kind === 'Quantify') {
      let prefix2 = prefix || Prefix.empty;
      let a2: Polytype = a;
      while (a2.kind === 'Quantify') {
        prefix2 = prefix2.add(a2.name, a2.bound);
        a2 = a2.body;
      }
      expect(equivalent(prefix2, a2, b)).toEqual(false);
      expect(equivalent(prefix2, b, a2)).toEqual(false);
    }
    if (b.kind === 'Quantify') {
      let prefix2 = prefix || Prefix.empty;
      let b2: Polytype = b;
      while (b2.kind === 'Quantify') {
        prefix2 = prefix2.add(b2.name, b2.bound);
        b2 = b2.body;
      }
      expect(equivalent(prefix2, a, b2)).toEqual(false);
      expect(equivalent(prefix2, b2, a)).toEqual(false);
    }
  });
}
