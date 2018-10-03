import {Err, Ok} from '../../Utils/Result';
import {
  AliasPattern,
  BinaryExpression,
  BinaryExpressionOperator,
  BindingName,
  BindingPattern,
  BreakExpression,
  CallExpression,
  ConditionalExpression,
  ContinueExpression,
  FunctionExpression,
  FunctionParameter,
  HoleExpression,
  HolePattern,
  ListExpression,
  ListPattern,
  LogicalExpression,
  LogicalExpressionOperator,
  LoopExpression,
  MatchCase,
  MatchExpression,
  MemberExpression,
  Name,
  PatternExpression,
  RecordExpression,
  RecordExpressionProperty,
  RecordPattern,
  ReferenceExpression,
  ReferenceType,
  ReturnExpression,
  TupleExpression,
  TupleExpressionElement,
  TuplePattern,
  TuplePatternElement,
  TypeParameter,
  UnaryExpression,
  UnaryExpressionOperator,
  UnitExpression,
  UnitPattern,
  UnitType,
  WrappedExpression,
} from '../Ast';
import {
  ExpectedBindingIdentifier,
  ExpectedEnd,
  ExpectedExpression,
  ExpectedGlyph,
  ExpectedLineSeparator,
  ExpectedType,
  UnexpectedTokenError,
} from '../Error';
import {BindingIdentifier, Identifier, ident} from '../Identifier';
import {Glyph, GlyphToken, IdentifierToken, Lexer} from '../Lexer';
import {loc} from '../Loc';
import {parseExpression} from '../Parser';

function lex(source: string): Lexer {
  return Lexer.create(source);
}

describe('expression', () => {
  [
    {
      source: 'foo',
      result: Ok(ReferenceExpression(loc('1-3'), ident('foo'))),
    },
    {
      source: 'in',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('1-2'), 'in' as Identifier),
          ExpectedExpression
        )
      ),
    },
    {
      source: '_',
      result: Ok(HoleExpression(loc('1'))),
    },
    {
      source: '()',
      result: Ok(UnitExpression(loc('1-2'))),
    },
    {
      source: '(foo)',
      result: Ok(
        WrappedExpression(
          loc('1-5'),
          ReferenceExpression(loc('2-4'), ident('foo')),
          undefined
        )
      ),
    },
    {
      source: '(a, b)',
      result: Ok(
        TupleExpression(loc('1-6'), [
          TupleExpressionElement(
            ReferenceExpression(loc('2'), ident('a')),
            undefined
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('5'), ident('b')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '(a, b, c, d)',
      result: Ok(
        TupleExpression(loc('1-12'), [
          TupleExpressionElement(
            ReferenceExpression(loc('2'), ident('a')),
            undefined
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('5'), ident('b')),
            undefined
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('8'), ident('c')),
            undefined
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('11'), ident('d')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '(,)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('2'), Glyph.Comma),
          ExpectedExpression
        )
      ),
    },
    {
      source: '(foo,)',
      result: Ok(
        WrappedExpression(
          loc('1-6'),
          ReferenceExpression(loc('2-4'), ident('foo')),
          undefined
        )
      ),
    },
    {
      source: '(a, b,)',
      result: Ok(
        TupleExpression(loc('1-7'), [
          TupleExpressionElement(
            ReferenceExpression(loc('2'), ident('a')),
            undefined
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('5'), ident('b')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '(a, b, c, d,)',
      result: Ok(
        TupleExpression(loc('1-13'), [
          TupleExpressionElement(
            ReferenceExpression(loc('2'), ident('a')),
            undefined
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('5'), ident('b')),
            undefined
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('8'), ident('c')),
            undefined
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('11'), ident('d')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '(x:)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('4'), Glyph.ParenRight),
          ExpectedType
        )
      ),
    },
    {
      source: '(x: T)',
      result: Ok(
        WrappedExpression(
          loc('1-6'),
          ReferenceExpression(loc('2'), ident('x')),
          ReferenceType(loc('5'), ident('T'))
        )
      ),
    },
    {
      source: '(a: A, b: B)',
      result: Ok(
        TupleExpression(loc('1-12'), [
          TupleExpressionElement(
            ReferenceExpression(loc('2'), ident('a')),
            ReferenceType(loc('5'), ident('A'))
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('8'), ident('b')),
            ReferenceType(loc('11'), ident('B'))
          ),
        ])
      ),
    },
    {
      source: '(a: A, b: B, c: C, d: D)',
      result: Ok(
        TupleExpression(loc('1-24'), [
          TupleExpressionElement(
            ReferenceExpression(loc('2'), ident('a')),
            ReferenceType(loc('5'), ident('A'))
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('8'), ident('b')),
            ReferenceType(loc('11'), ident('B'))
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('14'), ident('c')),
            ReferenceType(loc('17'), ident('C'))
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('20'), ident('d')),
            ReferenceType(loc('23'), ident('D'))
          ),
        ])
      ),
    },
    {
      source: '(x: T,)',
      result: Ok(
        WrappedExpression(
          loc('1-7'),
          ReferenceExpression(loc('2'), ident('x')),
          ReferenceType(loc('5'), ident('T'))
        )
      ),
    },
    {
      source: '(a: A, b: B,)',
      result: Ok(
        TupleExpression(loc('1-13'), [
          TupleExpressionElement(
            ReferenceExpression(loc('2'), ident('a')),
            ReferenceType(loc('5'), ident('A'))
          ),
          TupleExpressionElement(
            ReferenceExpression(loc('8'), ident('b')),
            ReferenceType(loc('11'), ident('B'))
          ),
        ])
      ),
    },
    {
      source: '{}',
      result: Ok(RecordExpression(loc('1-2'), undefined, [])),
    },
    {
      source: '{ foo }',
      result: Ok(
        RecordExpression(loc('1-7'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('3-5'), ident('foo')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo, bar }',
      result: Ok(
        RecordExpression(loc('1-12'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('3-5'), ident('foo')),
            undefined
          ),
          RecordExpressionProperty(
            Name(loc('8-10'), ident('bar')),
            ReferenceExpression(loc('8-10'), ident('bar')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo, bar, qux, lit }',
      result: Ok(
        RecordExpression(loc('1-22'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('3-5'), ident('foo')),
            undefined
          ),
          RecordExpressionProperty(
            Name(loc('8-10'), ident('bar')),
            ReferenceExpression(loc('8-10'), ident('bar')),
            undefined
          ),
          RecordExpressionProperty(
            Name(loc('13-15'), ident('qux')),
            ReferenceExpression(loc('13-15'), ident('qux')),
            undefined
          ),
          RecordExpressionProperty(
            Name(loc('18-20'), ident('lit')),
            ReferenceExpression(loc('18-20'), ident('lit')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo: T }',
      result: Ok(
        RecordExpression(loc('1-10'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('3-5'), ident('foo')),
            ReferenceType(loc('8'), ident('T'))
          ),
        ])
      ),
    },
    {
      source: '{ foo: T, bar: U }',
      result: Ok(
        RecordExpression(loc('1-18'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('3-5'), ident('foo')),
            ReferenceType(loc('8'), ident('T'))
          ),
          RecordExpressionProperty(
            Name(loc('11-13'), ident('bar')),
            ReferenceExpression(loc('11-13'), ident('bar')),
            ReferenceType(loc('16'), ident('U'))
          ),
        ])
      ),
    },
    {
      source: '{ foo: T, bar: U, qux: V, lit: W }',
      result: Ok(
        RecordExpression(loc('1-34'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('3-5'), ident('foo')),
            ReferenceType(loc('8'), ident('T'))
          ),
          RecordExpressionProperty(
            Name(loc('11-13'), ident('bar')),
            ReferenceExpression(loc('11-13'), ident('bar')),
            ReferenceType(loc('16'), ident('U'))
          ),
          RecordExpressionProperty(
            Name(loc('19-21'), ident('qux')),
            ReferenceExpression(loc('19-21'), ident('qux')),
            ReferenceType(loc('24'), ident('V'))
          ),
          RecordExpressionProperty(
            Name(loc('27-29'), ident('lit')),
            ReferenceExpression(loc('27-29'), ident('lit')),
            ReferenceType(loc('32'), ident('W'))
          ),
        ])
      ),
    },
    {
      source: '{ foo = foo2 }',
      result: Ok(
        RecordExpression(loc('1-14'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('9-12'), ident('foo2')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo = foo2, bar = bar2 }',
      result: Ok(
        RecordExpression(loc('1-26'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('9-12'), ident('foo2')),
            undefined
          ),
          RecordExpressionProperty(
            Name(loc('15-17'), ident('bar')),
            ReferenceExpression(loc('21-24'), ident('bar2')),
            undefined
          ),
        ])
      ),
    },
    {
      source: '{ foo: T = x }',
      result: Ok(
        RecordExpression(loc('1-14'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('12'), ident('x')),
            ReferenceType(loc('8'), ident('T'))
          ),
        ])
      ),
    },
    {
      source: '{ foo?: T }',
      result: Ok(
        RecordExpression(loc('1-11'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-5'), ident('foo')),
            ReferenceExpression(loc('3-5'), ident('foo')),
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
        RecordExpression(loc('1-12'), undefined, [
          RecordExpressionProperty(
            Name(loc('3-4'), 'in' as BindingIdentifier),
            ReferenceExpression(loc('8-10'), ident("in'")),
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
      source: '{ {} | }',
      result: Ok(
        RecordExpression(
          loc('1-8'),
          RecordExpression(loc('3-4'), undefined, []),
          []
        )
      ),
    },
    {
      source: '{ {} | a = x }',
      result: Ok(
        RecordExpression(
          loc('1-14'),
          RecordExpression(loc('3-4'), undefined, []),
          [
            RecordExpressionProperty(
              Name(loc('8'), ident('a')),
              ReferenceExpression(loc('12'), ident('x')),
              undefined
            ),
          ]
        )
      ),
    },
    {
      source: '{ {} | a = x, b = y, c = z }',
      result: Ok(
        RecordExpression(
          loc('1-28'),
          RecordExpression(loc('3-4'), undefined, []),
          [
            RecordExpressionProperty(
              Name(loc('8'), ident('a')),
              ReferenceExpression(loc('12'), ident('x')),
              undefined
            ),
            RecordExpressionProperty(
              Name(loc('15'), ident('b')),
              ReferenceExpression(loc('19'), ident('y')),
              undefined
            ),
            RecordExpressionProperty(
              Name(loc('22'), ident('c')),
              ReferenceExpression(loc('26'), ident('z')),
              undefined
            ),
          ]
        )
      ),
    },
    {
      source: '{ yo | }',
      result: Ok(
        RecordExpression(
          loc('1-8'),
          ReferenceExpression(loc('3-4'), ident('yo')),
          []
        )
      ),
    },
    {
      source: '{ yo | a = x }',
      result: Ok(
        RecordExpression(
          loc('1-14'),
          ReferenceExpression(loc('3-4'), ident('yo')),
          [
            RecordExpressionProperty(
              Name(loc('8'), ident('a')),
              ReferenceExpression(loc('12'), ident('x')),
              undefined
            ),
          ]
        )
      ),
    },
    {
      source: '{ yo | a = x, b = y, c = z }',
      result: Ok(
        RecordExpression(
          loc('1-28'),
          ReferenceExpression(loc('3-4'), ident('yo')),
          [
            RecordExpressionProperty(
              Name(loc('8'), ident('a')),
              ReferenceExpression(loc('12'), ident('x')),
              undefined
            ),
            RecordExpressionProperty(
              Name(loc('15'), ident('b')),
              ReferenceExpression(loc('19'), ident('y')),
              undefined
            ),
            RecordExpressionProperty(
              Name(loc('22'), ident('c')),
              ReferenceExpression(loc('26'), ident('z')),
              undefined
            ),
          ]
        )
      ),
    },
    {
      source: '{ if x then y else z | a = b }',
      result: Ok(
        RecordExpression(
          loc('1-30'),
          ConditionalExpression(
            loc('3-20'),
            ReferenceExpression(loc('6'), ident('x')),
            ReferenceExpression(loc('13'), ident('y')),
            ReferenceExpression(loc('20'), ident('z'))
          ),
          [
            RecordExpressionProperty(
              Name(loc('24'), ident('a')),
              ReferenceExpression(loc('28'), ident('b')),
              undefined
            ),
          ]
        )
      ),
    },
    {
      source: '{ x is T | a = b }',
      result: Ok(
        RecordExpression(
          loc('1-18'),
          PatternExpression(
            loc('3-8'),
            ReferenceExpression(loc('3'), ident('x')),
            BindingPattern(loc('8'), ident('T'))
          ),
          [
            RecordExpressionProperty(
              Name(loc('12'), ident('a')),
              ReferenceExpression(loc('16'), ident('b')),
              undefined
            ),
          ]
        )
      ),
    },
    {
      source: '{ x == y | a = b }',
      result: Ok(
        RecordExpression(
          loc('1-18'),
          BinaryExpression(
            loc('3-8'),
            BinaryExpressionOperator.Equal,
            ReferenceExpression(loc('3'), ident('x')),
            ReferenceExpression(loc('8'), ident('y'))
          ),
          [
            RecordExpressionProperty(
              Name(loc('12'), ident('a')),
              ReferenceExpression(loc('16'), ident('b')),
              undefined
            ),
          ]
        )
      ),
    },
    {
      source: '{ in | }',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('3-4'), 'in' as Identifier),
          ExpectedExpression
        )
      ),
    },
    {
      source: '[]',
      result: Ok(ListExpression(loc('1-2'), [])),
    },
    {
      source: '[a]',
      result: Ok(
        ListExpression(loc('1-3'), [ReferenceExpression(loc('2'), ident('a'))])
      ),
    },
    {
      source: '[a, b]',
      result: Ok(
        ListExpression(loc('1-6'), [
          ReferenceExpression(loc('2'), ident('a')),
          ReferenceExpression(loc('5'), ident('b')),
        ])
      ),
    },
    {
      source: '[a, b, c, d]',
      result: Ok(
        ListExpression(loc('1-12'), [
          ReferenceExpression(loc('2'), ident('a')),
          ReferenceExpression(loc('5'), ident('b')),
          ReferenceExpression(loc('8'), ident('c')),
          ReferenceExpression(loc('11'), ident('d')),
        ])
      ),
    },
    {
      source: 'match x: ()',
      result: Ok(
        MatchExpression(
          loc('1-11'),
          ReferenceExpression(loc('7'), ident('x')),
          []
        )
      ),
    },
    {
      source: 'match x: (y)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('12'), Glyph.ParenRight),
          ExpectedGlyph(Glyph.Arrow)
        )
      ),
    },
    {
      source: 'match x: ([] -> y; _ -> z)',
      result: Ok(
        MatchExpression(
          loc('1-26'),
          ReferenceExpression(loc('7'), ident('x')),
          [
            MatchCase(
              [ListPattern(loc('11-12'), [])],
              undefined,
              ReferenceExpression(loc('17'), ident('y'))
            ),
            MatchCase(
              [HolePattern(loc('20'))],
              undefined,
              ReferenceExpression(loc('25'), ident('z'))
            ),
          ]
        )
      ),
    },
    {
      source: 'match x: (\n  [] -> y\n  _ -> z\n)',
      result: Ok(
        MatchExpression(
          loc('1:1-4:1'),
          ReferenceExpression(loc('1:7'), ident('x')),
          [
            MatchCase(
              [ListPattern(loc('2:3-2:4'), [])],
              undefined,
              ReferenceExpression(loc('2:9'), ident('y'))
            ),
            MatchCase(
              [HolePattern(loc('3:3'))],
              undefined,
              ReferenceExpression(loc('3:8'), ident('z'))
            ),
          ]
        )
      ),
    },
    {
      source: 'match x: (_ -> y; () -> z)',
      result: Ok(
        MatchExpression(
          loc('1-26'),
          ReferenceExpression(loc('7'), ident('x')),
          [
            MatchCase(
              [HolePattern(loc('11'))],
              undefined,
              ReferenceExpression(loc('16'), ident('y'))
            ),
            MatchCase(
              [UnitPattern(loc('19-20'))],
              undefined,
              ReferenceExpression(loc('25'), ident('z'))
            ),
          ]
        )
      ),
    },
    {
      source: 'match x: (\n  _ -> y\n  () -> z\n)',
      result: Ok(
        MatchExpression(
          loc('1:1-4:1'),
          ReferenceExpression(loc('1:7'), ident('x')),
          [
            MatchCase(
              [HolePattern(loc('2:3'))],
              undefined,
              ReferenceExpression(loc('2:8'), ident('y'))
            ),
            MatchCase(
              [UnitPattern(loc('3:3-3:4'))],
              undefined,
              ReferenceExpression(loc('3:9'), ident('z'))
            ),
          ]
        )
      ),
    },
    {
      source: 'match x: (\n  _ -> y()\n  () -> z\n)',
      result: Ok(
        MatchExpression(
          loc('1:1-4:1'),
          ReferenceExpression(loc('1:7'), ident('x')),
          [
            MatchCase(
              [HolePattern(loc('2:3'))],
              undefined,
              CallExpression(
                loc('2:8-2:10'),
                ReferenceExpression(loc('2:8'), ident('y')),
                []
              )
            ),
            MatchCase(
              [UnitPattern(loc('3:3-3:4'))],
              undefined,
              ReferenceExpression(loc('3:9'), ident('z'))
            ),
          ]
        )
      ),
    },
    {
      source: 'match x: (_ -> y () -> z)',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('21-22'), Glyph.Arrow),
          ExpectedLineSeparator
        )
      ),
    },
    {
      source: 'match x: (_ | _ -> y)',
      result: Ok(
        MatchExpression(
          loc('1-21'),
          ReferenceExpression(loc('7'), ident('x')),
          [
            MatchCase(
              [HolePattern(loc('11')), HolePattern(loc('15'))],
              undefined,
              ReferenceExpression(loc('20'), ident('y'))
            ),
          ]
        )
      ),
    },
    {
      source: 'match x: (_ if y -> z)',
      result: Ok(
        MatchExpression(
          loc('1-22'),
          ReferenceExpression(loc('7'), ident('x')),
          [
            MatchCase(
              [HolePattern(loc('11'))],
              ReferenceExpression(loc('16'), ident('y')),
              ReferenceExpression(loc('21'), ident('z'))
            ),
          ]
        )
      ),
    },
    {
      source: 'match a: (_ if b -> c -> d)',
      result: Ok(
        MatchExpression(
          loc('1-27'),
          ReferenceExpression(loc('7'), ident('a')),
          [
            MatchCase(
              [HolePattern(loc('11'))],
              ReferenceExpression(loc('16'), ident('b')),
              FunctionExpression(
                loc('21-26'),
                [],
                [
                  FunctionParameter(
                    BindingPattern(loc('21'), ident('c')),
                    undefined
                  ),
                ],
                ReferenceExpression(loc('26'), ident('d'))
              )
            ),
          ]
        )
      ),
    },
    {
      source: 'match a: (_ if () -> c -> d)',
      result: Ok(
        MatchExpression(
          loc('1-28'),
          ReferenceExpression(loc('7'), ident('a')),
          [
            MatchCase(
              [HolePattern(loc('11'))],
              UnitExpression(loc('16-17')),
              FunctionExpression(
                loc('22-27'),
                [],
                [
                  FunctionParameter(
                    BindingPattern(loc('22'), ident('c')),
                    undefined
                  ),
                ],
                ReferenceExpression(loc('27'), ident('d'))
              )
            ),
          ]
        )
      ),
    },
    {
      source: 'match x: ().p',
      result: Ok(
        MemberExpression(
          loc('1-13'),
          MatchExpression(
            loc('1-11'),
            ReferenceExpression(loc('7'), ident('x')),
            []
          ),
          Name(loc('13'), ident('p'))
        )
      ),
    },
    {
      source: 'o.p',
      result: Ok(
        MemberExpression(
          loc('1-3'),
          ReferenceExpression(loc('1'), ident('o')),
          Name(loc('3'), ident('p'))
        )
      ),
    },
    {
      source: '().p',
      result: Ok(
        MemberExpression(
          loc('1-4'),
          UnitExpression(loc('1-2')),
          Name(loc('4'), ident('p'))
        )
      ),
    },
    {
      source: '(a, b).p',
      result: Ok(
        MemberExpression(
          loc('1-8'),
          TupleExpression(loc('1-6'), [
            TupleExpressionElement(
              ReferenceExpression(loc('2'), ident('a')),
              undefined
            ),
            TupleExpressionElement(
              ReferenceExpression(loc('5'), ident('b')),
              undefined
            ),
          ]),
          Name(loc('8'), ident('p'))
        )
      ),
    },
    {
      source: '{}.p',
      result: Ok(
        MemberExpression(
          loc('1-4'),
          RecordExpression(loc('1-2'), undefined, []),
          Name(loc('4'), ident('p'))
        )
      ),
    },
    {
      source: '{ a = b }.p',
      result: Ok(
        MemberExpression(
          loc('1-11'),
          RecordExpression(loc('1-9'), undefined, [
            RecordExpressionProperty(
              Name(loc('3'), ident('a')),
              ReferenceExpression(loc('7'), ident('b')),
              undefined
            ),
          ]),
          Name(loc('11'), ident('p'))
        )
      ),
    },
    {
      source: '[].p',
      result: Ok(
        MemberExpression(
          loc('1-4'),
          ListExpression(loc('1-2'), []),
          Name(loc('4'), ident('p'))
        )
      ),
    },
    {
      source: 'o.a.b.c',
      result: Ok(
        MemberExpression(
          loc('1-7'),
          MemberExpression(
            loc('1-5'),
            MemberExpression(
              loc('1-3'),
              ReferenceExpression(loc('1'), ident('o')),
              Name(loc('3'), ident('a'))
            ),
            Name(loc('5'), ident('b'))
          ),
          Name(loc('7'), ident('c'))
        )
      ),
    },
    {
      source: 'f().p',
      result: Ok(
        MemberExpression(
          loc('1-5'),
          CallExpression(
            loc('1-3'),
            ReferenceExpression(loc('1'), ident('f')),
            []
          ),
          Name(loc('5'), ident('p'))
        )
      ),
    },
    {
      source: '(o).p',
      result: Ok(
        MemberExpression(
          loc('1-5'),
          WrappedExpression(
            loc('1-3'),
            ReferenceExpression(loc('2'), ident('o')),
            undefined
          ),
          Name(loc('5'), ident('p'))
        )
      ),
    },
    {
      source: 'f()',
      result: Ok(
        CallExpression(
          loc('1-3'),
          ReferenceExpression(loc('1'), ident('f')),
          []
        )
      ),
    },
    {
      source: 'f(a)',
      result: Ok(
        CallExpression(loc('1-4'), ReferenceExpression(loc('1'), ident('f')), [
          ReferenceExpression(loc('3'), ident('a')),
        ])
      ),
    },
    {
      source: 'f(a, b)',
      result: Ok(
        CallExpression(loc('1-7'), ReferenceExpression(loc('1'), ident('f')), [
          ReferenceExpression(loc('3'), ident('a')),
          ReferenceExpression(loc('6'), ident('b')),
        ])
      ),
    },
    {
      source: 'f(a, b, c, d)',
      result: Ok(
        CallExpression(loc('1-13'), ReferenceExpression(loc('1'), ident('f')), [
          ReferenceExpression(loc('3'), ident('a')),
          ReferenceExpression(loc('6'), ident('b')),
          ReferenceExpression(loc('9'), ident('c')),
          ReferenceExpression(loc('12'), ident('d')),
        ])
      ),
    },
    {
      source: 'f\n()',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('2:1'), Glyph.ParenLeft),
          ExpectedEnd
        )
      ),
    },
    {
      source: 'f<>',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('3'), Glyph.GreaterThan),
          ExpectedExpression
        )
      ),
    },
    {
      source: 'f<>()',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('3'), Glyph.GreaterThan),
          ExpectedExpression
        )
      ),
    },
    {
      source: 'f\n<>()',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('2:2'), Glyph.GreaterThan),
          ExpectedExpression
        )
      ),
    },
    {
      source: 'f<>\n()',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('3'), Glyph.GreaterThan),
          ExpectedExpression
        )
      ),
    },
    {
      source: 'f\n<>\n()',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('2:2'), Glyph.GreaterThan),
          ExpectedExpression
        )
      ),
    },
    {
      source: 'f<A>()',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('4'), Glyph.GreaterThan),
          ExpectedEnd
        )
      ),
    },
    {
      source: 'f<A, B>()',
      result: Err(
        UnexpectedTokenError(GlyphToken(loc('4'), Glyph.Comma), ExpectedEnd)
      ),
    },
    {
      source: 'f<A, B, C, D>()',
      result: Err(
        UnexpectedTokenError(GlyphToken(loc('4'), Glyph.Comma), ExpectedEnd)
      ),
    },
    {
      source: 'f<A, B>(c, d)',
      result: Err(
        UnexpectedTokenError(GlyphToken(loc('4'), Glyph.Comma), ExpectedEnd)
      ),
    },
    {
      source: '()()',
      result: Ok(CallExpression(loc('1-4'), UnitExpression(loc('1-2')), [])),
    },
    {
      source: '().p()',
      result: Ok(
        CallExpression(
          loc('1-6'),
          MemberExpression(
            loc('1-4'),
            UnitExpression(loc('1-2')),
            Name(loc('4'), ident('p'))
          ),
          []
        )
      ),
    },
    {
      source: 'x -> y',
      result: Ok(
        FunctionExpression(
          loc('1-6'),
          [],
          [FunctionParameter(BindingPattern(loc('1'), ident('x')), undefined)],
          ReferenceExpression(loc('6'), ident('y'))
        )
      ),
    },
    {
      source: '() -> x',
      result: Ok(
        FunctionExpression(
          loc('1-7'),
          [],
          [],
          ReferenceExpression(loc('7'), ident('x'))
        )
      ),
    },
    {
      source: '(x) -> y',
      result: Ok(
        FunctionExpression(
          loc('1-8'),
          [],
          [FunctionParameter(BindingPattern(loc('2'), ident('x')), undefined)],
          ReferenceExpression(loc('8'), ident('y'))
        )
      ),
    },
    {
      source: '(x, y) -> z',
      result: Ok(
        FunctionExpression(
          loc('1-11'),
          [],
          [
            FunctionParameter(BindingPattern(loc('2'), ident('x')), undefined),
            FunctionParameter(BindingPattern(loc('5'), ident('y')), undefined),
          ],
          ReferenceExpression(loc('11'), ident('z'))
        )
      ),
    },
    {
      source: '(a, b, c, d) -> e',
      result: Ok(
        FunctionExpression(
          loc('1-17'),
          [],
          [
            FunctionParameter(BindingPattern(loc('2'), ident('a')), undefined),
            FunctionParameter(BindingPattern(loc('5'), ident('b')), undefined),
            FunctionParameter(BindingPattern(loc('8'), ident('c')), undefined),
            FunctionParameter(BindingPattern(loc('11'), ident('d')), undefined),
          ],
          ReferenceExpression(loc('17'), ident('e'))
        )
      ),
    },
    {
      source: '(a: A) -> b',
      result: Ok(
        FunctionExpression(
          loc('1-11'),
          [],
          [
            FunctionParameter(
              BindingPattern(loc('2'), ident('a')),
              ReferenceType(loc('5'), ident('A'))
            ),
          ],
          ReferenceExpression(loc('11'), ident('b'))
        )
      ),
    },
    {
      source: '(a: A, b: B) -> c',
      result: Ok(
        FunctionExpression(
          loc('1-17'),
          [],
          [
            FunctionParameter(
              BindingPattern(loc('2'), ident('a')),
              ReferenceType(loc('5'), ident('A'))
            ),
            FunctionParameter(
              BindingPattern(loc('8'), ident('b')),
              ReferenceType(loc('11'), ident('B'))
            ),
          ],
          ReferenceExpression(loc('17'), ident('c'))
        )
      ),
    },
    {
      source: '(a, b: B) -> c',
      result: Ok(
        FunctionExpression(
          loc('1-14'),
          [],
          [
            FunctionParameter(BindingPattern(loc('2'), ident('a')), undefined),
            FunctionParameter(
              BindingPattern(loc('5'), ident('b')),
              ReferenceType(loc('8'), ident('B'))
            ),
          ],
          ReferenceExpression(loc('14'), ident('c'))
        )
      ),
    },
    {
      source: '(a: A, b) -> c',
      result: Ok(
        FunctionExpression(
          loc('1-14'),
          [],
          [
            FunctionParameter(
              BindingPattern(loc('2'), ident('a')),
              ReferenceType(loc('5'), ident('A'))
            ),
            FunctionParameter(BindingPattern(loc('8'), ident('b')), undefined),
          ],
          ReferenceExpression(loc('14'), ident('c'))
        )
      ),
    },
    {
      source: '(()) -> x',
      result: Ok(
        FunctionExpression(
          loc('1-9'),
          [],
          [FunctionParameter(UnitPattern(loc('2-3')), undefined)],
          ReferenceExpression(loc('9'), ident('x'))
        )
      ),
    },
    {
      source: '((x, y)) -> z',
      result: Ok(
        FunctionExpression(
          loc('1-13'),
          [],
          [
            FunctionParameter(
              TuplePattern(loc('2-7'), [
                TuplePatternElement(
                  BindingPattern(loc('3'), ident('x')),
                  undefined
                ),
                TuplePatternElement(
                  BindingPattern(loc('6'), ident('y')),
                  undefined
                ),
              ]),
              undefined
            ),
          ],
          ReferenceExpression(loc('13'), ident('z'))
        )
      ),
    },
    {
      source: '((a: A, b: B)) -> c',
      result: Ok(
        FunctionExpression(
          loc('1-19'),
          [],
          [
            FunctionParameter(
              TuplePattern(loc('2-13'), [
                TuplePatternElement(
                  BindingPattern(loc('3'), ident('a')),
                  ReferenceType(loc('6'), ident('A'))
                ),
                TuplePatternElement(
                  BindingPattern(loc('9'), ident('b')),
                  ReferenceType(loc('12'), ident('B'))
                ),
              ]),
              undefined
            ),
          ],
          ReferenceExpression(loc('19'), ident('c'))
        )
      ),
    },
    {
      source: '({}) -> x',
      result: Ok(
        FunctionExpression(
          loc('1-9'),
          [],
          [FunctionParameter(RecordPattern(loc('2-3'), []), undefined)],
          ReferenceExpression(loc('9'), ident('x'))
        )
      ),
    },
    {
      source: '<>() -> x',
      result: Ok(
        FunctionExpression(
          loc('1-9'),
          [],
          [],
          ReferenceExpression(loc('9'), ident('x'))
        )
      ),
    },
    {
      source: '<T>() -> x',
      result: Ok(
        FunctionExpression(
          loc('1-10'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          [],
          ReferenceExpression(loc('10'), ident('x'))
        )
      ),
    },
    {
      source: '<T, U>() -> x',
      result: Ok(
        FunctionExpression(
          loc('1-13'),
          [
            TypeParameter(Name(loc('2'), ident('T')), []),
            TypeParameter(Name(loc('5'), ident('U')), []),
          ],
          [],
          ReferenceExpression(loc('13'), ident('x'))
        )
      ),
    },
    {
      source: '<T: A>() -> x',
      result: Ok(
        FunctionExpression(
          loc('1-13'),
          [
            TypeParameter(Name(loc('2'), ident('T')), [
              ReferenceType(loc('5'), ident('A')),
            ]),
          ],
          [],
          ReferenceExpression(loc('13'), ident('x'))
        )
      ),
    },
    {
      source: '<T: A + B>() -> x',
      result: Ok(
        FunctionExpression(
          loc('1-17'),
          [
            TypeParameter(Name(loc('2'), ident('T')), [
              ReferenceType(loc('5'), ident('A')),
              ReferenceType(loc('9'), ident('B')),
            ]),
          ],
          [],
          ReferenceExpression(loc('17'), ident('x'))
        )
      ),
    },
    {
      source: '<T> x -> x',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('5'), ident('x')),
          ExpectedGlyph(Glyph.ParenLeft)
        )
      ),
    },
    {
      source: '<T>(()) -> x',
      result: Ok(
        FunctionExpression(
          loc('1-12'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          [FunctionParameter(UnitPattern(loc('5-6')), undefined)],
          ReferenceExpression(loc('12'), ident('x'))
        )
      ),
    },
    {
      source: '<T>((): ()) -> x',
      result: Ok(
        FunctionExpression(
          loc('1-16'),
          [TypeParameter(Name(loc('2'), ident('T')), [])],
          [FunctionParameter(UnitPattern(loc('5-6')), UnitType(loc('9-10')))],
          ReferenceExpression(loc('16'), ident('x'))
        )
      ),
    },
    {
      source: 'x is ()',
      result: Ok(
        PatternExpression(
          loc('1-7'),
          ReferenceExpression(loc('1'), ident('x')),
          UnitPattern(loc('6-7'))
        )
      ),
    },
    {
      source: 'in is ()',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('1-2'), 'in' as Identifier),
          ExpectedExpression
        )
      ),
    },
    {
      source: '() is ()',
      result: Ok(
        PatternExpression(
          loc('1-8'),
          UnitExpression(loc('1-2')),
          UnitPattern(loc('7-8'))
        )
      ),
    },
    {
      source: '() is () is ()',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('10-11'), ident('is')),
          ExpectedEnd
        )
      ),
    },
    {
      source: '() is x is ()',
      result: Ok(
        PatternExpression(
          loc('1-13'),
          UnitExpression(loc('1-2')),
          AliasPattern(
            loc('7-13'),
            BindingName(loc('7'), ident('x')),
            UnitPattern(loc('12-13'))
          )
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
      source: 'if x then y else z',
      result: Ok(
        ConditionalExpression(
          loc('1-18'),
          ReferenceExpression(loc('4'), ident('x')),
          ReferenceExpression(loc('11'), ident('y')),
          ReferenceExpression(loc('18'), ident('z'))
        )
      ),
    },
    {
      source: 'if x then y',
      result: Ok(
        ConditionalExpression(
          loc('1-11'),
          ReferenceExpression(loc('4'), ident('x')),
          ReferenceExpression(loc('11'), ident('y')),
          undefined
        )
      ),
    },
    {
      source: 'if x then y else z()',
      result: Ok(
        ConditionalExpression(
          loc('1-20'),
          ReferenceExpression(loc('4'), ident('x')),
          ReferenceExpression(loc('11'), ident('y')),
          CallExpression(
            loc('18-20'),
            ReferenceExpression(loc('18'), ident('z')),
            []
          )
        )
      ),
    },
    {
      source: 'if x then y()',
      result: Ok(
        ConditionalExpression(
          loc('1-13'),
          ReferenceExpression(loc('4'), ident('x')),
          CallExpression(
            loc('11-13'),
            ReferenceExpression(loc('11'), ident('y')),
            []
          ),
          undefined
        )
      ),
    },
    {
      source: 'if a then b else if c then d else e',
      result: Ok(
        ConditionalExpression(
          loc('1-35'),
          ReferenceExpression(loc('4'), ident('a')),
          ReferenceExpression(loc('11'), ident('b')),
          ConditionalExpression(
            loc('18-35'),
            ReferenceExpression(loc('21'), ident('c')),
            ReferenceExpression(loc('28'), ident('d')),
            ReferenceExpression(loc('35'), ident('e'))
          )
        )
      ),
    },
    {
      source: 'if a then if b then c else d else e',
      result: Ok(
        ConditionalExpression(
          loc('1-35'),
          ReferenceExpression(loc('4'), ident('a')),
          ConditionalExpression(
            loc('11-28'),
            ReferenceExpression(loc('14'), ident('b')),
            ReferenceExpression(loc('21'), ident('c')),
            ReferenceExpression(loc('28'), ident('d'))
          ),
          ReferenceExpression(loc('35'), ident('e'))
        )
      ),
    },
    {
      source: 'if if a then b else c then d else e',
      result: Ok(
        ConditionalExpression(
          loc('1-35'),
          ConditionalExpression(
            loc('4-21'),
            ReferenceExpression(loc('7'), ident('a')),
            ReferenceExpression(loc('14'), ident('b')),
            ReferenceExpression(loc('21'), ident('c'))
          ),
          ReferenceExpression(loc('28'), ident('d')),
          ReferenceExpression(loc('35'), ident('e'))
        )
      ),
    },
    {
      source: 'a + if x then y else z + b',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('5-6'), 'if' as Identifier),
          ExpectedExpression
        )
      ),
    },
    {
      source: 'continue',
      result: Ok(ContinueExpression(loc('1-8'))),
    },
    {
      source: 'continue x',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('10'), ident('x')),
          ExpectedEnd
        )
      ),
    },
    {
      source: 'continue\nx',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('2:1'), ident('x')),
          ExpectedEnd
        )
      ),
    },
    {
      source: 'return',
      result: Ok(ReturnExpression(loc('1-6'), undefined)),
    },
    {
      source: 'return x',
      result: Ok(
        ReturnExpression(loc('1-8'), ReferenceExpression(loc('8'), ident('x')))
      ),
    },
    {
      source: 'return\nx',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('2:1'), ident('x')),
          ExpectedEnd
        )
      ),
    },
    {
      source: 'break',
      result: Ok(BreakExpression(loc('1-5'), undefined)),
    },
    {
      source: 'break x',
      result: Ok(
        BreakExpression(loc('1-7'), ReferenceExpression(loc('7'), ident('x')))
      ),
    },
    {
      source: 'break\nx',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('2:1'), ident('x')),
          ExpectedEnd
        )
      ),
    },
    {
      source: 'return <T>() -> x',
      result: Ok(
        ReturnExpression(
          loc('1-17'),
          FunctionExpression(
            loc('8-17'),
            [TypeParameter(Name(loc('9'), ident('T')), [])],
            [],
            ReferenceExpression(loc('17'), ident('x'))
          )
        )
      ),
    },
    {
      source: 'return -x',
      result: Ok(
        ReturnExpression(
          loc('1-9'),
          UnaryExpression(
            loc('8-9'),
            UnaryExpressionOperator.Negative,
            ReferenceExpression(loc('9'), ident('x'))
          )
        )
      ),
    },
    {
      source: 'match a: (_ if return b -> c -> d)',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('16-21'), 'return' as Identifier),
          ExpectedExpression
        )
      ),
    },
    {
      source: 'match a: (_ if if b then c else d -> e -> f)',
      result: Err(
        UnexpectedTokenError(
          IdentifierToken(loc('16-17'), 'if' as Identifier),
          ExpectedExpression
        )
      ),
    },
    {
      source: '_.p',
      result: Ok(
        MemberExpression(
          loc('1-3'),
          HoleExpression(loc('1')),
          Name(loc('3'), ident('p'))
        )
      ),
    },
    {
      source: '!x',
      result: Ok(
        UnaryExpression(
          loc('1-2'),
          UnaryExpressionOperator.Not,
          ReferenceExpression(loc('2'), ident('x'))
        )
      ),
    },
    {
      source: '!!x',
      result: Ok(
        UnaryExpression(
          loc('1-3'),
          UnaryExpressionOperator.Not,
          UnaryExpression(
            loc('2-3'),
            UnaryExpressionOperator.Not,
            ReferenceExpression(loc('3'), ident('x'))
          )
        )
      ),
    },
    {
      source: '!!!!x',
      result: Ok(
        UnaryExpression(
          loc('1-5'),
          UnaryExpressionOperator.Not,
          UnaryExpression(
            loc('2-5'),
            UnaryExpressionOperator.Not,
            UnaryExpression(
              loc('3-5'),
              UnaryExpressionOperator.Not,
              UnaryExpression(
                loc('4-5'),
                UnaryExpressionOperator.Not,
                ReferenceExpression(loc('5'), ident('x'))
              )
            )
          )
        )
      ),
    },
    {
      source: '-x',
      result: Ok(
        UnaryExpression(
          loc('1-2'),
          UnaryExpressionOperator.Negative,
          ReferenceExpression(loc('2'), ident('x'))
        )
      ),
    },
    {
      source: '--x',
      result: Ok(
        UnaryExpression(
          loc('1-3'),
          UnaryExpressionOperator.Negative,
          UnaryExpression(
            loc('2-3'),
            UnaryExpressionOperator.Negative,
            ReferenceExpression(loc('3'), ident('x'))
          )
        )
      ),
    },
    {
      source: '!-x',
      result: Ok(
        UnaryExpression(
          loc('1-3'),
          UnaryExpressionOperator.Not,
          UnaryExpression(
            loc('2-3'),
            UnaryExpressionOperator.Negative,
            ReferenceExpression(loc('3'), ident('x'))
          )
        )
      ),
    },
    {
      source: '-!x',
      result: Ok(
        UnaryExpression(
          loc('1-3'),
          UnaryExpressionOperator.Negative,
          UnaryExpression(
            loc('2-3'),
            UnaryExpressionOperator.Not,
            ReferenceExpression(loc('3'), ident('x'))
          )
        )
      ),
    },
    {
      source: 'a * b',
      result: Ok(
        BinaryExpression(
          loc('1-5'),
          BinaryExpressionOperator.Multiply,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('5'), ident('b'))
        )
      ),
    },
    {
      source: 'a / b',
      result: Ok(
        BinaryExpression(
          loc('1-5'),
          BinaryExpressionOperator.Divide,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('5'), ident('b'))
        )
      ),
    },
    {
      source: 'a % b',
      result: Ok(
        BinaryExpression(
          loc('1-5'),
          BinaryExpressionOperator.Remainder,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('5'), ident('b'))
        )
      ),
    },
    {
      source: 'a * b * c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Multiply,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Multiply,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: 'a * b * c * d * e',
      result: Ok(
        BinaryExpression(
          loc('1-17'),
          BinaryExpressionOperator.Multiply,
          BinaryExpression(
            loc('1-13'),
            BinaryExpressionOperator.Multiply,
            BinaryExpression(
              loc('1-9'),
              BinaryExpressionOperator.Multiply,
              BinaryExpression(
                loc('1-5'),
                BinaryExpressionOperator.Multiply,
                ReferenceExpression(loc('1'), ident('a')),
                ReferenceExpression(loc('5'), ident('b'))
              ),
              ReferenceExpression(loc('9'), ident('c'))
            ),
            ReferenceExpression(loc('13'), ident('d'))
          ),
          ReferenceExpression(loc('17'), ident('e'))
        )
      ),
    },
    {
      source: 'a * b / c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Divide,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Multiply,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: 'a * b % c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Remainder,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Multiply,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: 'a / b * c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Multiply,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Divide,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: 'a % b * c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Multiply,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Remainder,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: '(a * b) * c',
      result: Ok(
        BinaryExpression(
          loc('1-11'),
          BinaryExpressionOperator.Multiply,
          WrappedExpression(
            loc('1-7'),
            BinaryExpression(
              loc('2-6'),
              BinaryExpressionOperator.Multiply,
              ReferenceExpression(loc('2'), ident('a')),
              ReferenceExpression(loc('6'), ident('b'))
            ),
            undefined
          ),
          ReferenceExpression(loc('11'), ident('c'))
        )
      ),
    },
    {
      source: 'a * (b * c)',
      result: Ok(
        BinaryExpression(
          loc('1-11'),
          BinaryExpressionOperator.Multiply,
          ReferenceExpression(loc('1'), ident('a')),
          WrappedExpression(
            loc('5-11'),
            BinaryExpression(
              loc('6-10'),
              BinaryExpressionOperator.Multiply,
              ReferenceExpression(loc('6'), ident('b')),
              ReferenceExpression(loc('10'), ident('c'))
            ),
            undefined
          )
        )
      ),
    },
    {
      source: 'a + b',
      result: Ok(
        BinaryExpression(
          loc('1-5'),
          BinaryExpressionOperator.Add,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('5'), ident('b'))
        )
      ),
    },
    {
      source: 'a - b',
      result: Ok(
        BinaryExpression(
          loc('1-5'),
          BinaryExpressionOperator.Subtract,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('5'), ident('b'))
        )
      ),
    },
    {
      source: 'a + b + c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Add,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Add,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: 'a + b + c + d + e',
      result: Ok(
        BinaryExpression(
          loc('1-17'),
          BinaryExpressionOperator.Add,
          BinaryExpression(
            loc('1-13'),
            BinaryExpressionOperator.Add,
            BinaryExpression(
              loc('1-9'),
              BinaryExpressionOperator.Add,
              BinaryExpression(
                loc('1-5'),
                BinaryExpressionOperator.Add,
                ReferenceExpression(loc('1'), ident('a')),
                ReferenceExpression(loc('5'), ident('b'))
              ),
              ReferenceExpression(loc('9'), ident('c'))
            ),
            ReferenceExpression(loc('13'), ident('d'))
          ),
          ReferenceExpression(loc('17'), ident('e'))
        )
      ),
    },
    {
      source: 'a + b - c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Subtract,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Add,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: 'a - b + c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Add,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Subtract,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: '(a + b) + c',
      result: Ok(
        BinaryExpression(
          loc('1-11'),
          BinaryExpressionOperator.Add,
          WrappedExpression(
            loc('1-7'),
            BinaryExpression(
              loc('2-6'),
              BinaryExpressionOperator.Add,
              ReferenceExpression(loc('2'), ident('a')),
              ReferenceExpression(loc('6'), ident('b'))
            ),
            undefined
          ),
          ReferenceExpression(loc('11'), ident('c'))
        )
      ),
    },
    {
      source: 'a + (b + c)',
      result: Ok(
        BinaryExpression(
          loc('1-11'),
          BinaryExpressionOperator.Add,
          ReferenceExpression(loc('1'), ident('a')),
          WrappedExpression(
            loc('5-11'),
            BinaryExpression(
              loc('6-10'),
              BinaryExpressionOperator.Add,
              ReferenceExpression(loc('6'), ident('b')),
              ReferenceExpression(loc('10'), ident('c'))
            ),
            undefined
          )
        )
      ),
    },
    {
      source: 'a * b + c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Add,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Multiply,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: 'a + b * c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.Add,
          ReferenceExpression(loc('1'), ident('a')),
          BinaryExpression(
            loc('5-9'),
            BinaryExpressionOperator.Multiply,
            ReferenceExpression(loc('5'), ident('b')),
            ReferenceExpression(loc('9'), ident('c'))
          )
        )
      ),
    },
    {
      source: 'a * (b + c)',
      result: Ok(
        BinaryExpression(
          loc('1-11'),
          BinaryExpressionOperator.Multiply,
          ReferenceExpression(loc('1'), ident('a')),
          WrappedExpression(
            loc('5-11'),
            BinaryExpression(
              loc('6-10'),
              BinaryExpressionOperator.Add,
              ReferenceExpression(loc('6'), ident('b')),
              ReferenceExpression(loc('10'), ident('c'))
            ),
            undefined
          )
        )
      ),
    },
    {
      source: '(a + b) * c',
      result: Ok(
        BinaryExpression(
          loc('1-11'),
          BinaryExpressionOperator.Multiply,
          WrappedExpression(
            loc('1-7'),
            BinaryExpression(
              loc('2-6'),
              BinaryExpressionOperator.Add,
              ReferenceExpression(loc('2'), ident('a')),
              ReferenceExpression(loc('6'), ident('b'))
            ),
            undefined
          ),
          ReferenceExpression(loc('11'), ident('c'))
        )
      ),
    },
    {
      source: 'a * b + c * d',
      result: Ok(
        BinaryExpression(
          loc('1-13'),
          BinaryExpressionOperator.Add,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Multiply,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          BinaryExpression(
            loc('9-13'),
            BinaryExpressionOperator.Multiply,
            ReferenceExpression(loc('9'), ident('c')),
            ReferenceExpression(loc('13'), ident('d'))
          )
        )
      ),
    },
    {
      source: 'a * (b + c) * d',
      result: Ok(
        BinaryExpression(
          loc('1-15'),
          BinaryExpressionOperator.Multiply,
          BinaryExpression(
            loc('1-11'),
            BinaryExpressionOperator.Multiply,
            ReferenceExpression(loc('1'), ident('a')),
            WrappedExpression(
              loc('5-11'),
              BinaryExpression(
                loc('6-10'),
                BinaryExpressionOperator.Add,
                ReferenceExpression(loc('6'), ident('b')),
                ReferenceExpression(loc('10'), ident('c'))
              ),
              undefined
            )
          ),
          ReferenceExpression(loc('15'), ident('d'))
        )
      ),
    },
    {
      source: 'a < b',
      result: Ok(
        BinaryExpression(
          loc('1-5'),
          BinaryExpressionOperator.LessThan,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('5'), ident('b'))
        )
      ),
    },
    {
      source: 'a > b',
      result: Ok(
        BinaryExpression(
          loc('1-5'),
          BinaryExpressionOperator.GreaterThan,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('5'), ident('b'))
        )
      ),
    },
    {
      source: 'a <= b',
      result: Ok(
        BinaryExpression(
          loc('1-6'),
          BinaryExpressionOperator.LessThanOrEqual,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('6'), ident('b'))
        )
      ),
    },
    {
      source: 'a >= b',
      result: Ok(
        BinaryExpression(
          loc('1-6'),
          BinaryExpressionOperator.GreaterThanOrEqual,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('6'), ident('b'))
        )
      ),
    },
    {
      source: '() < () > ()',
      result: Err(
        UnexpectedTokenError(
          GlyphToken(loc('9'), Glyph.GreaterThan),
          ExpectedEnd
        )
      ),
    },
    {
      source: 'a < b < c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.LessThan,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.LessThan,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          ReferenceExpression(loc('9'), ident('c'))
        )
      ),
    },
    {
      source: 'a < b + c',
      result: Ok(
        BinaryExpression(
          loc('1-9'),
          BinaryExpressionOperator.LessThan,
          ReferenceExpression(loc('1'), ident('a')),
          BinaryExpression(
            loc('5-9'),
            BinaryExpressionOperator.Add,
            ReferenceExpression(loc('5'), ident('b')),
            ReferenceExpression(loc('9'), ident('c'))
          )
        )
      ),
    },
    {
      source: 'a == b',
      result: Ok(
        BinaryExpression(
          loc('1-6'),
          BinaryExpressionOperator.Equal,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('6'), ident('b'))
        )
      ),
    },
    {
      source: 'a != b',
      result: Ok(
        BinaryExpression(
          loc('1-6'),
          BinaryExpressionOperator.EqualNot,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('6'), ident('b'))
        )
      ),
    },
    {
      source: 'a == b == c',
      result: Ok(
        BinaryExpression(
          loc('1-11'),
          BinaryExpressionOperator.Equal,
          BinaryExpression(
            loc('1-6'),
            BinaryExpressionOperator.Equal,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('6'), ident('b'))
          ),
          ReferenceExpression(loc('11'), ident('c'))
        )
      ),
    },
    {
      source: 'a + b == c + d',
      result: Ok(
        BinaryExpression(
          loc('1-14'),
          BinaryExpressionOperator.Equal,
          BinaryExpression(
            loc('1-5'),
            BinaryExpressionOperator.Add,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('5'), ident('b'))
          ),
          BinaryExpression(
            loc('10-14'),
            BinaryExpressionOperator.Add,
            ReferenceExpression(loc('10'), ident('c')),
            ReferenceExpression(loc('14'), ident('d'))
          )
        )
      ),
    },
    {
      source: 'a && b',
      result: Ok(
        LogicalExpression(
          loc('1-6'),
          LogicalExpressionOperator.And,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('6'), ident('b'))
        )
      ),
    },
    {
      source: 'a || b',
      result: Ok(
        LogicalExpression(
          loc('1-6'),
          LogicalExpressionOperator.Or,
          ReferenceExpression(loc('1'), ident('a')),
          ReferenceExpression(loc('6'), ident('b'))
        )
      ),
    },
    {
      source: 'a && b && c',
      result: Ok(
        LogicalExpression(
          loc('1-11'),
          LogicalExpressionOperator.And,
          LogicalExpression(
            loc('1-6'),
            LogicalExpressionOperator.And,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('6'), ident('b'))
          ),
          ReferenceExpression(loc('11'), ident('c'))
        )
      ),
    },
    {
      source: 'a && b && c && d',
      result: Ok(
        LogicalExpression(
          loc('1-16'),
          LogicalExpressionOperator.And,
          LogicalExpression(
            loc('1-11'),
            LogicalExpressionOperator.And,
            LogicalExpression(
              loc('1-6'),
              LogicalExpressionOperator.And,
              ReferenceExpression(loc('1'), ident('a')),
              ReferenceExpression(loc('6'), ident('b'))
            ),
            ReferenceExpression(loc('11'), ident('c'))
          ),
          ReferenceExpression(loc('16'), ident('d'))
        )
      ),
    },
    {
      source: 'a && b || c && d',
      result: Ok(
        LogicalExpression(
          loc('1-16'),
          LogicalExpressionOperator.Or,
          LogicalExpression(
            loc('1-6'),
            LogicalExpressionOperator.And,
            ReferenceExpression(loc('1'), ident('a')),
            ReferenceExpression(loc('6'), ident('b'))
          ),
          LogicalExpression(
            loc('11-16'),
            LogicalExpressionOperator.And,
            ReferenceExpression(loc('11'), ident('c')),
            ReferenceExpression(loc('16'), ident('d'))
          )
        )
      ),
    },
    {
      source: 'loop: x',
      result: Ok(
        LoopExpression(loc('1-7'), ReferenceExpression(loc('7'), ident('x')))
      ),
    },
    {
      source: 'loop: o.p',
      result: Ok(
        LoopExpression(
          loc('1-9'),
          MemberExpression(
            loc('7-9'),
            ReferenceExpression(loc('7'), ident('o')),
            Name(loc('9'), ident('p'))
          )
        )
      ),
    },
    {
      source: 'loop: (o).p',
      result: Ok(
        LoopExpression(
          loc('1-11'),
          MemberExpression(
            loc('7-11'),
            WrappedExpression(
              loc('7-9'),
              ReferenceExpression(loc('8'), ident('o')),
              undefined
            ),
            Name(loc('11'), ident('p'))
          )
        )
      ),
    },
  ].forEach(({source, result}) => {
    test(source.replace(/\n/g, '\\n'), () => {
      expect(parseExpression(lex(source))).toEqual(result);
    });
  });
});
