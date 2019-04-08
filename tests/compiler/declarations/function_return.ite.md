# Compiler Test: `function_return`

## JS
```js
function f1() {
  const a = true;
  const b = true;
  return a && b;
}

function f2() {
  const a = true;
  const b = true;
  const c = a && b;
}

function f3() {
  const a = true;
  const b = true;
  const c = a && b;
  return c;
}
```
