# Compiler Test: `declaration_already_exists`

## Errors
- (5:5-5:6) Can not use the name `f` again.
  - (1:5-1:6) `f`

## JS
```js
function f() {
  return true;
}

function f2() {
  return false;
}
```
