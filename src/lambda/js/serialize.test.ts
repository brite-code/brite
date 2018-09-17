import generate from '@babel/generator';
import {parse} from '../parse';
import {serialize} from './serialize';

[
  ['λx.x', 'x => x;'],
  ['λx.λy.x y', 'x => y => x(y);'],

  // fix-point combinator
  [
    'λf.(λx.λv.((f (x x)) v)) (λx.λv.((f (x x)) v))',
    `f => {\n  const x = x => v => f(x(x))(v);\n\n  return v => f(x(x))(v);\n};`,
  ],

  ['(λx.x) (λy.y)', 'const x = y => y;\n\nx;'],
  ['let x = (λy.y) in x', 'const x = y => y;\n\nx;'],
  ['(λx.(λx.x) x) (λy.y)', 'const x = y => y;\n\nconst x$2 = x;\nx$2;'],
  ['(λx.(λx.x) x) (λx.x)', 'const x = x => x;\n\nconst x$2 = x;\nx$2;'],

  [
    'let x = (λx.x) in let x = (λx.x) in (λx.x) x',
    'const x = x => x;\n\nconst x$2 = x => x;\n\nconst x$3 = x$2;\nx$3;',
  ],

  [
    'let x = (λx.let x = x in x) in let x = (λx.let x = x in x) in x',
    'const x = x => {\n  const x$2 = x;\n  return x$2;\n};\n\nconst x$2 = x => {\n  const x$2 = x;\n  return x$2;\n};\n\nx$2;',
  ],
].forEach(([input, output]) => {
  test(input, () => {
    expect(generate(serialize(parse(input))).code).toEqual(output);
  });
});
