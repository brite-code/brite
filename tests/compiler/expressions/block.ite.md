# Compiler Test: `block`

## JS
```js
function f1() {
  const a = true;
  const b = true;
  const c = true;
  const d = true;
  c && d;
  const x = undefined;
  const e = true;
  const f = true;
  const y = undefined;
  const g = true;
  const h = true;
  const z = g && h;
  const a2 = true;
  const b2 = true;
  const c2 = true;
  const d2 = true;
  c2 && d2;
  const x2 = undefined;
  const e2 = true;
  const f2 = true;
  const y2 = undefined;
  const g2 = true;
  const h2 = true;
  const z2 = g2 && h2;
}
```
