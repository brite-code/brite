# Compiler Test: `logical`

## JS
```js
const a = true;
const b = false;
const c = a && b;
const d = a || b;
const e = a && b && c && b;
const f = a || b || c || b;
const g = a && b || c && d;
const h = a && (b || c) && d;
const i = a || b && c || d;
const j = (a || b) && (c || d);
```
