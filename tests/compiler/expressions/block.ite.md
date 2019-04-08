# Compiler Test: `block`

## JS
```js
function f1() {
  const a = true;
  const b = true;
  const c = true;
  const d = true;
  c && d;
  const x1 = undefined;
  const e = true;
  const f = true;
  const y1 = undefined;
  const g = true;
  const h = true;
  const z1 = g && h;
  const x2 = () => {};
  const y2 = () => {
    const e = true;
    const f = true;
  };
  const z2 = () => {
    const g = true;
    const h = true;
    return g && h;
  };
  const a2 = true;
  const b2 = true;
  const c2 = true;
  const d2 = true;
  c2 && d2;
  const x12 = undefined;
  const e2 = true;
  const f2 = true;
  const y12 = undefined;
  const g2 = true;
  const h2 = true;
  const z12 = g2 && h2;
  const x22 = () => {};
  const y22 = () => {
    const e = true;
    const f = true;
  };
  const z22 = () => {
    const g = true;
    const h = true;
    return g && h;
  };
}
```
