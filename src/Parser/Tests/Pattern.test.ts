import {Err, Ok, Result, ResultType} from '../../Utils/Result';
import {
  AliasPattern,
  BindingName,
  BindingPattern,
  DeconstructPattern,
  FunctionExpression,
  FunctionType,
  HolePattern,
  ListPattern,
  Name,
  Pattern,
  QualifiedPattern,
  RecordPattern,
  RecordPatternProperty,
  ReferenceExpression,
  ReferenceType,
  TuplePattern,
  TuplePatternElement,
  UnitExpression,
  UnitPattern,
  WrappedPattern,
} from '../Ast';
import {
  ExpectedBindingIdentifier,
  ExpectedEnd,
  ExpectedExpression,
  ExpectedGlyph,
  ExpectedPattern,
  ExpectedType,
  ExpressionIntoPatternError,
  ParserErrorType,
  UnexpectedTokenError,
} from '../Error';
import {BindingIdentifier, Identifier, ident} from '../Identifier';
import {EndToken, Glyph, GlyphToken, IdentifierToken, Lexer} from '../Lexer';
import {loc} from '../Loc';
import {expressionIntoPattern, parseExpression, parsePattern} from '../Parser';

const cases = [
  {
    source: 'foo',
    result: Ok(BindingPattern(loc('1-3'), ident('foo'))),
  },
  {
    source: 'in',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('1-2'), 'in' as Identifier),
        ExpectedPattern
      )
    ),
  },
  {source: '_', result: Ok(HolePattern(loc('1')))},
  {
    source: '_x',
    result: Ok(BindingPattern(loc('1-2'), ident('_x'))),
  },
  {
    source: '()',
    result: Ok(UnitPattern(loc('1-2'))),
  },
  {
    source: '(foo)',
    result: Ok(
      WrappedPattern(
        loc('1-5'),
        BindingPattern(loc('2-4'), ident('foo')),
        undefined
      )
    ),
  },
  {
    source: '(a, b)',
    result: Ok(
      TuplePattern(loc('1-6'), [
        TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
        TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
      ])
    ),
  },
  {
    source: '(a, b, c, d)',
    result: Ok(
      TuplePattern(loc('1-12'), [
        TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
        TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
        TuplePatternElement(BindingPattern(loc('8'), ident('c')), undefined),
        TuplePatternElement(BindingPattern(loc('11'), ident('d')), undefined),
      ])
    ),
  },
  {
    source: '(,)',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('2'), Glyph.Comma), ExpectedPattern)
    ),
  },
  {
    source: '(foo,)',
    result: Ok(
      WrappedPattern(
        loc('1-6'),
        BindingPattern(loc('2-4'), ident('foo')),
        undefined
      )
    ),
  },
  {
    source: '(a, b,)',
    result: Ok(
      TuplePattern(loc('1-7'), [
        TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
        TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
      ])
    ),
  },
  {
    source: '(a, b, c, d,)',
    result: Ok(
      TuplePattern(loc('1-13'), [
        TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
        TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
        TuplePatternElement(BindingPattern(loc('8'), ident('c')), undefined),
        TuplePatternElement(BindingPattern(loc('11'), ident('d')), undefined),
      ])
    ),
  },
  {
    source: '(x:)',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('4'), Glyph.ParenRight), ExpectedType)
    ),
  },
  {
    source: '(x: T)',
    result: Ok(
      WrappedPattern(
        loc('1-6'),
        BindingPattern(loc('2'), ident('x')),
        ReferenceType(loc('5'), ident('T'))
      )
    ),
  },
  {
    source: '(a: A, b: B)',
    result: Ok(
      TuplePattern(loc('1-12'), [
        TuplePatternElement(
          BindingPattern(loc('2'), ident('a')),
          ReferenceType(loc('5'), ident('A'))
        ),
        TuplePatternElement(
          BindingPattern(loc('8'), ident('b')),
          ReferenceType(loc('11'), ident('B'))
        ),
      ])
    ),
  },
  {
    source: '(a: A, b: B, c: C, d: D)',
    result: Ok(
      TuplePattern(loc('1-24'), [
        TuplePatternElement(
          BindingPattern(loc('2'), ident('a')),
          ReferenceType(loc('5'), ident('A'))
        ),
        TuplePatternElement(
          BindingPattern(loc('8'), ident('b')),
          ReferenceType(loc('11'), ident('B'))
        ),
        TuplePatternElement(
          BindingPattern(loc('14'), ident('c')),
          ReferenceType(loc('17'), ident('C'))
        ),
        TuplePatternElement(
          BindingPattern(loc('20'), ident('d')),
          ReferenceType(loc('23'), ident('D'))
        ),
      ])
    ),
  },
  {
    source: '(x: T,)',
    result: Ok(
      WrappedPattern(
        loc('1-7'),
        BindingPattern(loc('2'), ident('x')),
        ReferenceType(loc('5'), ident('T'))
      )
    ),
  },
  {
    source: '(a: A, b: B,)',
    result: Ok(
      TuplePattern(loc('1-13'), [
        TuplePatternElement(
          BindingPattern(loc('2'), ident('a')),
          ReferenceType(loc('5'), ident('A'))
        ),
        TuplePatternElement(
          BindingPattern(loc('8'), ident('b')),
          ReferenceType(loc('11'), ident('B'))
        ),
      ])
    ),
  },
  {
    source: '{}',
    result: Ok(RecordPattern(loc('1-2'), [])),
  },
  {
    source: '{ foo }',
    result: Ok(
      RecordPattern(loc('1-7'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('3-5'), ident('foo')),
          undefined
        ),
      ])
    ),
  },
  {
    source: '{ foo, bar }',
    result: Ok(
      RecordPattern(loc('1-12'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('3-5'), ident('foo')),
          undefined
        ),
        RecordPatternProperty(
          Name(loc('8-10'), ident('bar')),
          BindingPattern(loc('8-10'), ident('bar')),
          undefined
        ),
      ])
    ),
  },
  {
    source: '{ foo, bar, qux, lit }',
    result: Ok(
      RecordPattern(loc('1-22'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('3-5'), ident('foo')),
          undefined
        ),
        RecordPatternProperty(
          Name(loc('8-10'), ident('bar')),
          BindingPattern(loc('8-10'), ident('bar')),
          undefined
        ),
        RecordPatternProperty(
          Name(loc('13-15'), ident('qux')),
          BindingPattern(loc('13-15'), ident('qux')),
          undefined
        ),
        RecordPatternProperty(
          Name(loc('18-20'), ident('lit')),
          BindingPattern(loc('18-20'), ident('lit')),
          undefined
        ),
      ])
    ),
  },
  {
    source: '{ foo: T }',
    result: Ok(
      RecordPattern(loc('1-10'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('3-5'), ident('foo')),
          ReferenceType(loc('8'), ident('T'))
        ),
      ])
    ),
  },
  {
    source: '{ foo: T, bar: U }',
    result: Ok(
      RecordPattern(loc('1-18'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('3-5'), ident('foo')),
          ReferenceType(loc('8'), ident('T'))
        ),
        RecordPatternProperty(
          Name(loc('11-13'), ident('bar')),
          BindingPattern(loc('11-13'), ident('bar')),
          ReferenceType(loc('16'), ident('U'))
        ),
      ])
    ),
  },
  {
    source: '{ foo: T, bar: U, qux: V, lit: W }',
    result: Ok(
      RecordPattern(loc('1-34'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('3-5'), ident('foo')),
          ReferenceType(loc('8'), ident('T'))
        ),
        RecordPatternProperty(
          Name(loc('11-13'), ident('bar')),
          BindingPattern(loc('11-13'), ident('bar')),
          ReferenceType(loc('16'), ident('U'))
        ),
        RecordPatternProperty(
          Name(loc('19-21'), ident('qux')),
          BindingPattern(loc('19-21'), ident('qux')),
          ReferenceType(loc('24'), ident('V'))
        ),
        RecordPatternProperty(
          Name(loc('27-29'), ident('lit')),
          BindingPattern(loc('27-29'), ident('lit')),
          ReferenceType(loc('32'), ident('W'))
        ),
      ])
    ),
  },
  {
    source: '{ foo = foo2 }',
    result: Ok(
      RecordPattern(loc('1-14'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('9-12'), ident('foo2')),
          undefined
        ),
      ])
    ),
  },
  {
    source: '{ foo = foo2, bar = bar2 }',
    result: Ok(
      RecordPattern(loc('1-26'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('9-12'), ident('foo2')),
          undefined
        ),
        RecordPatternProperty(
          Name(loc('15-17'), ident('bar')),
          BindingPattern(loc('21-24'), ident('bar2')),
          undefined
        ),
      ])
    ),
  },
  {
    source: '{ foo: T = x }',
    result: Ok(
      RecordPattern(loc('1-14'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('12'), ident('x')),
          ReferenceType(loc('8'), ident('T'))
        ),
      ])
    ),
  },
  {
    source: '{ foo?: T }',
    result: Ok(
      RecordPattern(loc('1-11'), [
        RecordPatternProperty(
          Name(loc('3-5'), ident('foo')),
          BindingPattern(loc('3-5'), ident('foo')),
          ReferenceType(loc('9'), ident('T')),
          {optional: true}
        ),
      ])
    ),
  },
  {
    source: '{ foo? }',
    result: Err(
      UnexpectedTokenError(
        GlyphToken(loc('8'), Glyph.BraceRight),
        ExpectedGlyph(Glyph.Colon)
      )
    ),
  },
  {
    source: "{ in = in' }",
    result: Ok(
      RecordPattern(loc('1-12'), [
        RecordPatternProperty(
          Name(loc('3-4'), 'in' as BindingIdentifier),
          BindingPattern(loc('8-10'), ident("in'")),
          undefined
        ),
      ])
    ),
  },
  {
    source: '{ in }',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('3-4'), 'in' as Identifier),
        ExpectedBindingIdentifier
      )
    ),
  },
  {
    source: '[]',
    result: Ok(ListPattern(loc('1-2'), [])),
  },
  {
    source: '[a]',
    result: Ok(ListPattern(loc('1-3'), [BindingPattern(loc('2'), ident('a'))])),
  },
  {
    source: '[a, b]',
    result: Ok(
      ListPattern(loc('1-6'), [
        BindingPattern(loc('2'), ident('a')),
        BindingPattern(loc('5'), ident('b')),
      ])
    ),
  },
  {
    source: '[a, b, c, d]',
    result: Ok(
      ListPattern(loc('1-12'), [
        BindingPattern(loc('2'), ident('a')),
        BindingPattern(loc('5'), ident('b')),
        BindingPattern(loc('8'), ident('c')),
        BindingPattern(loc('11'), ident('d')),
      ])
    ),
  },
  {
    source: 'a.b',
    result: Ok(
      QualifiedPattern(loc('1-3'), [
        Name(loc('1'), ident('a')),
        Name(loc('3'), ident('b')),
      ])
    ),
  },
  {
    source: 'a.b.c',
    result: Ok(
      QualifiedPattern(loc('1-5'), [
        Name(loc('1'), ident('a')),
        Name(loc('3'), ident('b')),
        Name(loc('5'), ident('c')),
      ])
    ),
  },
  {
    source: 'a.b.c.d',
    result: Ok(
      QualifiedPattern(loc('1-7'), [
        Name(loc('1'), ident('a')),
        Name(loc('3'), ident('b')),
        Name(loc('5'), ident('c')),
        Name(loc('7'), ident('d')),
      ])
    ),
  },
  {
    source: 'in.yay',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('1-2'), 'in' as Identifier),
        ExpectedPattern
      )
    ),
  },
  {
    source: 'yay.in',
    result: Ok(
      QualifiedPattern(loc('1-6'), [
        Name(loc('1-3'), ident('yay')),
        Name(loc('5-6'), 'in' as BindingIdentifier),
      ])
    ),
  },
  {
    source: 'foo\n()',
    result: Err(
      UnexpectedTokenError(GlyphToken(loc('2:1'), Glyph.ParenLeft), ExpectedEnd)
    ),
  },
  {
    source: 'a()',
    result: Ok(
      DeconstructPattern(loc('1-3'), [Name(loc('1'), ident('a'))], [])
    ),
  },
  {
    source: 'a.b()',
    result: Ok(
      DeconstructPattern(
        loc('1-5'),
        [Name(loc('1'), ident('a')), Name(loc('3'), ident('b'))],
        []
      )
    ),
  },
  {
    source: 'a.b.c.d()',
    result: Ok(
      DeconstructPattern(
        loc('1-9'),
        [
          Name(loc('1'), ident('a')),
          Name(loc('3'), ident('b')),
          Name(loc('5'), ident('c')),
          Name(loc('7'), ident('d')),
        ],
        []
      )
    ),
  },
  {
    source: 'T(a)',
    result: Ok(
      DeconstructPattern(
        loc('1-4'),
        [Name(loc('1'), ident('T'))],
        [BindingPattern(loc('3'), ident('a'))]
      )
    ),
  },
  {
    source: 'T(a, b)',
    result: Ok(
      DeconstructPattern(
        loc('1-7'),
        [Name(loc('1'), ident('T'))],
        [
          BindingPattern(loc('3'), ident('a')),
          BindingPattern(loc('6'), ident('b')),
        ]
      )
    ),
  },
  {
    source: 'T(a, b, c, d)',
    result: Ok(
      DeconstructPattern(
        loc('1-13'),
        [Name(loc('1'), ident('T'))],
        [
          BindingPattern(loc('3'), ident('a')),
          BindingPattern(loc('6'), ident('b')),
          BindingPattern(loc('9'), ident('c')),
          BindingPattern(loc('12'), ident('d')),
        ]
      )
    ),
  },
  {
    source: 'in()',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('1-2'), 'in' as Identifier),
        ExpectedPattern
      )
    ),
  },
  {
    source: 'in.yay()',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('1-2'), 'in' as Identifier),
        ExpectedPattern
      )
    ),
  },
  {
    source: 'yay.in()',
    result: Ok(
      DeconstructPattern(
        loc('1-8'),
        [
          Name(loc('1-3'), ident('yay')),
          Name(loc('5-6'), 'in' as BindingIdentifier),
        ],
        []
      )
    ),
  },
  {
    source: 'x\nis ()',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('2:1-2:2'), ident('is')),
        ExpectedEnd
      )
    ),
  },
  {
    source: 'x is ()',
    result: Ok(
      AliasPattern(
        loc('1-7'),
        BindingName(loc('1'), ident('x')),
        UnitPattern(loc('6-7'))
      )
    ),
  },
  {
    source: 'in is ()',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('1-2'), 'in' as Identifier),
        ExpectedPattern
      )
    ),
  },
  {
    source: 'x\nis ()',
    result: Err(
      UnexpectedTokenError(
        IdentifierToken(loc('2:1-2:2'), ident('is')),
        ExpectedEnd
      )
    ),
  },
  {
    source: '((): T -> U)',
    result: Ok(
      WrappedPattern(
        loc('1-12'),
        UnitPattern(loc('2-3')),
        FunctionType(
          loc('6-11'),
          [ReferenceType(loc('6'), ident('T'))],
          ReferenceType(loc('11'), ident('U'))
        )
      )
    ),
  },
];

cases.forEach(({source, result}) => {
  test(source.replace(/\n/g, '\\n'), () => {
    expect(parsePattern(Lexer.create(source))).toEqual(result);
  });
});

describe('from expression', () => {
  cases.forEach(({source, result: expectedResult}) => {
    test(source.replace(/\n/g, '\\n'), () => {
      const lexer = Lexer.create(source);
      const result = parseExpression(lexer);
      switch (result.type) {
        case ResultType.Ok: {
          const pattern = expressionIntoPattern(lexer.next(), result.value);
          expect(Ok(pattern)).toEqual(expectedResult);
          break;
        }
        case ResultType.Err: {
          const error =
            result.value.type === ParserErrorType.UnexpectedToken
              ? UnexpectedTokenError(
                  result.value.unexpected,
                  result.value.expected === ExpectedExpression
                    ? ExpectedPattern
                    : result.value.expected
                )
              : result.value;
          expect(Err(error)).toEqual(expectedResult);
          break;
        }
      }
    });
  });
});

describe('expression into pattern', () => {
  const cases: ReadonlyArray<{
    source: string;
    result: Result<Pattern, ExpressionIntoPatternError>;
  }> = [
    {
      source: 'x',
      result: Ok(BindingPattern(loc('1'), ident('x'))),
    },
    {
      source: '_',
      result: Ok(HolePattern(loc('1'))),
    },
    {
      source: '()',
      result: Ok(UnitPattern(loc('1-2'))),
    },
    {
      source: '(a, b)',
      result: Ok(
        TuplePattern(loc('1-6'), [
          TuplePatternElement(BindingPattern(loc('2'), ident('a')), undefined),
          TuplePatternElement(BindingPattern(loc('5'), ident('b')), undefined),
        ])
      ),
    },
    {
      source: '(() -> x, y)',
      result: Err(
        ExpressionIntoPatternError(
          FunctionExpression(
            loc('2-8'),
            [],
            [],
            undefined,
            ReferenceExpression(loc('8'), ident('x'))
          ),
          EndToken(loc('13'))
        )
      ),
    },
    {
      source: '(y, () -> x)',
      result: Err(
        ExpressionIntoPatternError(
          FunctionExpression(
            loc('5-11'),
            [],
            [],
            undefined,
            ReferenceExpression(loc('11'), ident('x'))
          ),
          EndToken(loc('13'))
        )
      ),
    },
    {
      source: '{ a = b }',
      result: Ok(
        RecordPattern(loc('1-9'), [
          RecordPatternProperty(
            Name(loc('3'), ident('a')),
            BindingPattern(loc('7'), ident('b')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ o | a = b }',
      result: Err(
        ExpressionIntoPatternError(
          ReferenceExpression(loc('3'), ident('o')),
          EndToken(loc('14'))
        )
      ),
    },
    {
      source: '{ a = () -> x }',
      result: Err(
        ExpressionIntoPatternError(
          FunctionExpression(
            loc('7-13'),
            [],
            [],
            undefined,
            ReferenceExpression(loc('13'), ident('x'))
          ),
          EndToken(loc('16'))
        )
      ),
    },
    {
      source: '[a, b]',
      result: Ok(
        ListPattern(loc('1-6'), [
          BindingPattern(loc('2'), ident('a')),
          BindingPattern(loc('5'), ident('b')),
        ])
      ),
    },
    {
      source: '[() -> x, y]',
      result: Err(
        ExpressionIntoPatternError(
          FunctionExpression(
            loc('2-8'),
            [],
            [],
            undefined,
            ReferenceExpression(loc('8'), ident('x'))
          ),
          EndToken(loc('13'))
        )
      ),
    },
    {
      source: '[y, () -> x]',
      result: Err(
        ExpressionIntoPatternError(
          FunctionExpression(
            loc('5-11'),
            [],
            [],
            undefined,
            ReferenceExpression(loc('11'), ident('x'))
          ),
          EndToken(loc('13'))
        )
      ),
    },
    {
      source: 'a.b.c',
      result: Ok(
        QualifiedPattern(loc('1-5'), [
          Name(loc('1'), ident('a')),
          Name(loc('3'), ident('b')),
          Name(loc('5'), ident('c')),
        ])
      ),
    },
    {
      source: '().p',
      result: Err(
        ExpressionIntoPatternError(
          UnitExpression(loc('1-2')),
          EndToken(loc('5'))
        )
      ),
    },
    {
      source: '().a.b',
      result: Err(
        ExpressionIntoPatternError(
          UnitExpression(loc('1-2')),
          EndToken(loc('7'))
        )
      ),
    },
    {
      source: 'f()',
      result: Ok(
        DeconstructPattern(loc('1-3'), [Name(loc('1'), ident('f'))], [])
      ),
    },
    {
      source: '()()',
      result: Err(
        ExpressionIntoPatternError(
          UnitExpression(loc('1-2')),
          EndToken(loc('5'))
        )
      ),
    },
    {
      source: '().p()',
      result: Err(
        ExpressionIntoPatternError(
          UnitExpression(loc('1-2')),
          EndToken(loc('7'))
        )
      ),
    },
    {
      source: 'f(() -> x)',
      result: Err(
        ExpressionIntoPatternError(
          FunctionExpression(
            loc('3-9'),
            [],
            [],
            undefined,
            ReferenceExpression(loc('9'), ident('x'))
          ),
          EndToken(loc('11'))
        )
      ),
    },
    {
      source: 'x is ()',
      result: Ok(
        AliasPattern(
          loc('1-7'),
          BindingName(loc('1'), ident('x')),
          UnitPattern(loc('6-7'))
        )
      ),
    },
    {
      source: '() is ()',
      result: Err(
        ExpressionIntoPatternError(
          UnitExpression(loc('1-2')),
          EndToken(loc('9'))
        )
      ),
    },
    {
      source: '(x)',
      result: Ok(
        WrappedPattern(
          loc('1-3'),
          BindingPattern(loc('2'), ident('x')),
          undefined
        )
      ),
    },
    {
      source: '(() -> x)',
      result: Err(
        ExpressionIntoPatternError(
          FunctionExpression(
            loc('2-8'),
            [],
            [],
            undefined,
            ReferenceExpression(loc('8'), ident('x'))
          ),
          EndToken(loc('10'))
        )
      ),
    },
  ];

  cases.forEach(({source, result}) => {
    test(source, () => {
      const lexer = Lexer.create(source);
      const expression = parseExpression(lexer);
      if (expression.type !== ResultType.Ok) {
        throw new Error('Could not parse expression.');
      }
      let pattern: Result<Pattern, unknown>;
      try {
        pattern = Ok(expressionIntoPattern(lexer.next(), expression.value));
      } catch (error) {
        pattern = Err(error);
      }
      expect(pattern).toEqual(result);
    });
  });
});
