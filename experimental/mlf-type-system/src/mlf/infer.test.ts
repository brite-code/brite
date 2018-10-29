import {Diagnostics} from './diagnostics';
import {InferError, infer} from './infer';
import {parseExpression, parseType} from './parser';
import {Type} from './type';

const prelude = new Map<string, Type>(
  [
    ['true', 'boolean'],
    ['false', 'boolean'],
    ['succ', 'number → number'],
    ['id', '∀x.x → x'],
    ['app', '∀(a, b).(a → b) → a → b'],
    ['choose', '∀a.a → a → a'],
    ['auto', '∀(a = ∀a.a → a).a → a'],
    ['undefined', '∀x.x'],
  ].map(([name, type]): [string, Type] => [name, parseType(type)])
);

const cases: ReadonlyArray<{
  readonly only?: true;
  readonly expression: string;
  readonly type: string;
  readonly errors?: ReadonlyArray<string>;
}> = [
  {
    expression: 'nope',
    type: '⊥',
    errors: ['unbound variable "nope"'],
  },
  {
    expression: 'succ 42',
    type: '∀($0 = number).$0',
  },
  {
    expression: 'succ true',
    type: '∀($0 = number).$0',
    errors: ['number ≢ boolean'],
  },
  {
    expression: 'succ nope',
    type: '∀($1 = number).$1',
    errors: ['unbound variable "nope"'],
  },
  {
    expression: 'succ undefined',
    type: '∀($1 = number).$1',
  },
  {
    expression: 'true',
    type: 'boolean',
  },
  {
    expression: 'let x = true in x',
    type: 'boolean',
  },
  {
    expression: 'let x = true in let y = x in y',
    type: 'boolean',
  },
  {
    expression: 'let x = true in let y = x in x',
    type: 'boolean',
  },
  {
    expression: 'λx.true',
    type: '∀$0.$0 → boolean',
  },
  {
    expression: 'λx.x',
    type: '∀$0.$0 → $0',
  },
  {
    expression: 'λx.let y = x in y',
    type: '∀$0.$0 → $0',
  },
  {
    expression: 'λx.choose x 42',
    type: '∀($3 = number, $0 = $3, $7 ≥ ∀($5 = number).$5).$0 → $7',
  },
  {
    expression: 'λx.choose 42 x',
    type: '∀($0 = number, $8 ≥ ∀($5 = number).$5).$0 → $8',
  },
  {
    expression: 'λx.let y = choose x 42 in y',
    type: '∀($3 = number, $0 = $3, $7 ≥ ∀($5 = number).$5).$0 → $7',
  },
  {
    expression: 'λx.let y = choose x 42 in x',
    type: '∀($3 = number, $0 = $3).$0 → $0',
  },
  {
    expression: 'λx.let y = choose 42 x in x',
    type: '∀($0 = number).$0 → $0',
  },
  {
    expression: 'λx.choose x id',
    type: '∀($5 ≥ ∀x.x → x, $3 = $5, $0 = $3, $8 ≥ ∀($6 = $5).$6).$0 → $8',
  },
  {
    expression: 'λx.choose id x',
    type: '∀($7, $0 = $7 → $7, $11 ≥ ∀($6 = $7 → $7).$6).$0 → $11',
  },
  {
    expression: 'λx.let _ = choose x id in x',
    type: '∀($5 ≥ ∀x.x → x, $3 = $5, $0 = $3).$0 → $0',
  },
  {
    expression: 'λx.let _ = choose id x in x',
    type: '∀($7, $0 = $7 → $7).$0 → $0',
  },
  {
    expression: '(λx.choose x true) 42',
    type: '∀($9 = boolean).$9',
    errors: ['boolean ≢ number'],
  },
  {
    expression: '(λx.choose true x) 42',
    type: '∀($10 = boolean).$10',
    errors: ['boolean ≢ number'],
  },
  {
    expression: '(λx.let _ = choose x true in x) 42',
    type: '∀($8 = boolean).$8',
    errors: ['boolean ≢ number'],
  },
  {
    expression: '(λx.let _ = choose true x in x) 42',
    type: '∀($9 = boolean).$9',
    errors: ['boolean ≢ number'],
  },
  {
    expression: 'true 42',
    type: '∀$0.$0',
    errors: ['number → $0 ≢ boolean'],
  },
  {
    expression: 'λ(f, x).choose (f x) x',
    type: '∀($7, $5 = $7, $1 = $5, $2 = $5, $0 = $1 → $2).$0 → $1 → $7',
  },
  {
    expression: '(λ(f, x).choose (f x) x) (λx.x)',
    type: '∀($18, $13 = $18, $14 = $13, $15 = $14, $12 = $15 → $13).$12',
  },
  {
    expression: '(λ(f, x).choose (f x) x) (λx.undefined)',
    type: '∀($19, $14 = $19, $15 = $14, $16 = $15, $13 = $16 → $14).$13',
  },
  {
    expression: 'λ(f, x).choose (f x) undefined',
    type:
      '∀($1, $7 ≥ ∀x.x, $5 = $7, $2 = $5, $0 = $1 → $2, $11 ≥ ∀($10 ≥ ∀($8 = $7).$8).$1 → $10).$0 → $11',
  },
  {
    expression: '(42: boolean)',
    type: 'boolean',
    errors: ['boolean ≢ number'],
  },
  {
    expression:
      'let f = (undefined: boolean → number) in (f: number → boolean)',
    type: 'number → boolean',
    errors: ['boolean ≢ number', 'boolean ≢ number'],
  },
  {
    expression: 'let f = (undefined: boolean → number) in (f: string → string)',
    type: 'string → string',
    errors: ['boolean ≢ string', 'string ≢ number'],
  },
  {
    expression: 'λ(f, x).f x',
    type: '∀($1, $2, $0 = $1 → $2).$0 → $1 → $2',
  },
  {
    expression: 'app succ',
    type: '∀($2 = number, $3 = number, $1 = $2 → $3).$1',
  },
  {
    expression: 'app succ 42',
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
  {
    expression: 'λ_.undefined',
    type: '∀($0, $1 ≥ ∀x.x).$0 → $1',
  },
  {
    expression: '(λ_.undefined: ∀(a, b).a → b)',
    type: '∀(a, b).a → b',
  },
  {
    expression: 'let magic = λ_.undefined in magic 42',
    type: '∀$3.$3',
  },
  {
    expression: 'let magic = (λ_.undefined: ∀(a, b).a → b) in magic 42',
    type: '∀$10.$10',
  },
  {
    expression: 'let id = λx.x in (id: ∀x.x → number)',
    type: '∀x.x → number',
  },
  {
    expression:
      'let id = λx.x in let id = (id: ∀x.x → number) in (id: ∀x.x → x)',
    type: '∀x.x → x',
  },
  {
    expression: 'let f = λx.42 in (f: ∀x.x → string)',
    type: '∀x.x → string',
    errors: ['string ≢ number'],
  },
  {
    // only: true,
    expression: 'auto succ',
    type: '∀($2 = number, $1 = $2 → $2).$1',
    errors: [''],
  },
  {
    // only: true,
    expression:
      'let annotate = (undefined: ∀(a = ∀x.x → number, b ≥ ∀x.x → number).a → b) in annotate id',
    type: '∀($12, $9 = $12 → number).$9',
    errors: [''],
  },
  {
    // only: true,
    expression:
      'let annotate = (undefined: ∀(a = ∀x.x → number, b ≥ ∀x.x → number).a → b) in let id = annotate id in succ (id true)',
    type: '∀($22 = number).$22',
    errors: [''],
  },
  {
    expression: 'let id = λx.x in let _ = (id: ∀x.x → number) in id',
    type: '∀$0.$0 → $0',
    errors: [''],
  },
  {
    // only: true,
    expression: '(λx.x: ∀x.x → number)',
    type: '∀x.x → number',
    errors: [''],
  },
  {
    // only: true,
    expression: 'let id = (λx.x: ∀x.x → number) in succ (id true)',
    type: '∀($9 = number).$9',
    errors: [''],
  },
  {
    // only: true,
    expression: 'let id = (λx.true: ∀x.x → x) in succ (id 42)',
    type: '∀($9 = number).$9',
    errors: [''],
  },
  {
    expression: 'let id = λx.x in succ (id true)',
    type: '∀($5 = number).$5',
    errors: ['number ≢ boolean'],
  },
  {
    expression: 'let id = λx.x in let succ = λx.succ (id x) in succ true',
    type: '∀($8 = number).$8',
    errors: ['number ≢ boolean'],
  },
];

for (const {
  only,
  expression: expressionSource,
  type: typeSource,
  errors: expectedErrors = [],
} of cases) {
  const defineTest = only ? test.only : test;
  defineTest(expressionSource, () => {
    const expressionUntyped = parseExpression(expressionSource);
    const diagnostics = new Diagnostics<InferError<never>>();
    const expression = infer(diagnostics, prelude, expressionUntyped);
    expect(Type.toDisplayString(expression.type)).toEqual(typeSource);
    expect([...Type.getFreeVariables(expression.type)]).toEqual([]);
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
