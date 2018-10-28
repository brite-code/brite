import {Diagnostics} from './diagnostics';
import {InferError, infer} from './infer';
import {parseExpression, parseType} from './parser';
import {Type} from './type';

const prelude = new Map<string, Type>(
  [
    ['true', 'boolean'],
    ['false', 'boolean'],
    ['neg', 'number → number'],
    ['succ', 'number → number'],
    ['id', '∀x.x → x'],
    ['app', '∀(a, b).(a → b) → a → b'],
    ['choose', '∀a.a → a → a'],
    ['auto', '∀(a = ∀a.a → a).a → a'],
  ].map(([name, type]): [string, Type] => [name, parseType(type)])
);

const cases: ReadonlyArray<{
  readonly expression: string;
  readonly type: string;
  readonly errors?: ReadonlyArray<string>;
}> = [
  {
    expression: 'neg 42',
    type: '∀($0 = number).$0',
  },
  {
    expression: 'neg true',
    type: '∀($0 = number).$0',
    errors: ['number ≢ boolean'],
  },
  {
    expression: 'λ(f, x).f x',
    type: '∀($1, $2, $0 = $1 → $2).$0 → $1 → $2',
  },
  {
    expression: 'app neg',
    type: '∀($2 = number, $3 = number, $1 = $2 → $3).$1',
  },
  {
    expression: 'app neg 42',
    type: '∀($5 = number).$5',
  },
  {
    expression: 'app id',
    type: '∀($4, $5 = $4, $3 = $5, $2 = $3 → $4).$2',
  },
  {
    expression: 'app id 42',
    type: '∀($7 = number).$7',
  },
  {
    expression: 'choose id',
    type: '∀($1 ≥ ∀x.x → x, $3 = $1, $2 = $3 → $3).$2',
  },
  {
    expression: 'choose id 42',
    type: '∀($6, $5 = $6 → $6).$5',
    errors: ['$6 → $6 ≢ number'],
  },
  {
    expression: 'λx.x x',
    type: '∀($0, $2 ≥ ∀$1.$1).$0 → $2',
    errors: ['infinite type'],
  },
  {
    expression: 'λx.λy.let z = choose x y in x y',
    type:
      '∀($6, $4 = $6, $0 = $4, $10 ≥ ∀($1 = $4, $9 ≥ ∀$8.$8).$1 → $9).$0 → $10',
    errors: ['infinite type'],
  },
  {
    expression: 'λx.λy.let z = choose y x in x y',
    type:
      '∀($6, $4 = $6, $0 = $4, $10 ≥ ∀($1 = $4, $9 ≥ ∀$8.$8).$1 → $9).$0 → $10',
    errors: ['infinite type'],
  },
  {
    expression: 'λx.λy.let z = choose x y in y x',
    type:
      '∀($6, $4 = $6, $0 = $4, $10 ≥ ∀($1 = $4, $9 ≥ ∀$8.$8).$1 → $9).$0 → $10',
    errors: ['infinite type'],
  },
  {
    expression: 'λx.λy.let z = choose y x in y x',
    type:
      '∀($6, $4 = $6, $0 = $4, $10 ≥ ∀($1 = $4, $9 ≥ ∀$8.$8).$1 → $9).$0 → $10',
    errors: ['infinite type'],
  },
  {
    expression: 'λx.let x = (x: ∀a.a → a) in x x',
    type:
      '∀($1 = ∀a.a → a, $0 = $1, $6 ≥ ∀($3 ≥ ∀a.a → a, $4 = $3).$4).$0 → $6',
  },
  {
    expression: 'choose id id',
    type: '∀($11, $7 = $11, $6 = $7 → $7).$6',
  },
  {
    expression: 'choose id succ',
    type: '∀($6 = number, $5 = $6 → $6).$5',
  },
  {
    expression: 'choose succ id',
    type: '∀($5 = number → number).$5',
  },
  {
    expression: 'choose id auto',
    type: '∀($11, $7 = $11 → $11, $6 = $7 → $7).$6',
  },
  {
    expression: 'choose auto id',
    type: '∀($7, $8 = $7 → $7, $6 = $8 → $8).$6',
  },
  {
    expression: 'id auto',
    type: '∀($1 ≥ ∀(a = ∀a.a → a).a → a, $2 = $1).$2',
  },
  {
    expression: '(λx.x id) auto',
    type: '∀($6, $10 = $6, $5 = $10 → $10).$5',
  },
  {
    expression: '(λx.x (λx.x)) (λx.let x = (x: ∀a.a → a) in x x)',
    type: '∀($21, $13 = $21 → $21).$13',
  },
  {
    expression: 'app auto id',
    type: '∀($14, $10 = $14, $9 = $10 → $10).$9',
  },
  {
    expression: '(λf.λx.f x) (λx.let x = (x: ∀a.a → a) in x x) (λx.x)',
    type: '∀($29, $26 = $29 → $29).$26',
  },
];

for (const {
  expression: expressionSource,
  type: typeSource,
  errors: expectedErrors = [],
} of cases) {
  test(expressionSource, () => {
    const expressionUntyped = parseExpression(expressionSource);
    const diagnostics = new Diagnostics<InferError<never>>();
    const expression = infer(diagnostics, prelude, expressionUntyped);
    expect(Type.toDisplayString(expression.type)).toEqual(typeSource);
    expect([...diagnostics].map(toTestString)).toEqual(expectedErrors);
  });
}

function toTestString(error: InferError<never>): string {
  switch (error.kind) {
    case 'UnboundVariable':
      return `unbound variable "${error.identifier}"`;

    case 'IncompatibleTypes': {
      const expected = Type.toDisplayString(error.expected);
      const actual = Type.toDisplayString(error.actual);
      return `${expected} ≢ ${actual}`;
    }

    case 'InfiniteType':
      return 'infinite type';

    default:
      const never: never = error;
      return never;
  }
}
