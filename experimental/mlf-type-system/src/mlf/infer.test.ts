import {Diagnostics} from './diagnostics';
import {Expression} from './expression';
import {InferError, infer} from './infer';
import {parseExpression, parseType} from './parser';
import {Type} from './type';

const prelude = new Map<string, Type>(
  [
    ['neg', 'number → number'],
    ['succ', 'number → number'],
    ['id', '∀x.x → x'],
    ['app', '∀(a, b).(a → b) → a → b'],
    ['choose', '∀a.a → a → a'],
    ['auto', '∀(a = ∀a.a → a).a → a'],
  ].map(([name, type]): [string, Type] => [name, parseType(type)])
);

const cases: ReadonlyArray<string> = [
  'λ(f, x).f x',
  'app neg',
  'app neg 42',
  'app id',
  'app id 42',
  'choose id',
  'choose id 42',
  'λx.x x',
  'λx.λy.let z = choose x y in x y',
  'λx.λy.let z = choose y x in x y',
  'λx.λy.let z = choose x y in y x',
  'λx.λy.let z = choose y x in y x',
  'λx.let x = (x: ∀a.a → a) in x x',
  'choose id id',
  'choose id succ',
  'choose succ id',
  'choose id auto',
  'choose auto id',
  'id auto',
  '(λx.x id) auto',
  '(λx.x (λx.x)) (λx.let x = (x: ∀a.a → a) in x x)',
  'app auto id',
  '(λf.λx.f x) (λx.let x = (x: ∀a.a → a) in x x) (λx.x)',
];

for (const input of cases) {
  test(input, () => {
    const untypedExpression = parseExpression(input);
    const diagnostics = new Diagnostics<InferError<never>>();
    const expression = infer(diagnostics, prelude, untypedExpression);
    console.log(Type.toDisplayString(expression.type));
  });
}
