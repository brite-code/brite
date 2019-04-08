# Compiler Test: `function`

## JS
```js
function main() {
  const x1 = () => {};
  const x2 = () => {
    const a = true;
    const b = true;
  };
  const x3 = () => {
    const a = true;
    const b = true;
    return a && b;
  };
  const x4 = () => true && false;
}
```
