# Learn Brite in 5 Minutes

Brite is simple which makes building products easy. So easy, in fact, that you
can learn Brite in 5 minutes.

Brite is also incredibly powerful. By baking effect tracking into the type
system you can implicitly provide context, use concurrency fearlessly, fine-tune
your build, and much more.

You don’t need to worry about Brite’s advanced features to build products,
though. Just the core language which you can learn in 5 minutes.

So let’s get started with variables.

```
let answer = 42;
```

To add a variable you use the `let` keyword as shown above. You can assign any
value you want to a variable, like a number.

```
let celsius = 24;
let fahrenheit = (celsius * 1.8) + 32;
```

You can use a variable in another variable. Like in this Celsius to Fahrenheit
conversion.

This example also shows that you can use the standard mathematical operators.
Such as addition (`+`), subtraction (`-`), multiplication (`*`), and division
(`/`).

To represent text you create a string with double quotes (`"`).

```
let name = "Kate";
```

You may embed one string within another by “escaping” a value with a backslash
(`\`) and parentheses (`()`).

```
let name = "Kate";
let message = "Hello \(name)";
```

Here we embed the value `"Kate"` in the string `message`. So `message` is equal
to `"Hello Kate"`. This feature is called “string interpolation” in other
programming languages.

Some operations are not allowed. Like subtracting a string from a number.

```
let nope = 100 - "wut";
```

Brite will analyze your program and give you the location of incorrect code. You
may still execute code with errors, but it might crash.

---

Objects allow you to store many values together.

```
let person = {
  id: 4,
  name: "Mark",
};

let message = "Hello \(person.name)";
```

Here we have a person object with a number `id` property and a string `name`
property. To access these properties we write `person.id` or `person.name`. We
embed the `name` property in `message` to create the string `"Hello Mark"`.

---

To reuse code you can create a function. Let’s say we want to reuse our celsius
to fahrenheit conversion from earlier.

```
fun celsiusToFahrenheit(celsius) {
  (celsius * 1.8) + 32
}

let fahrenheit = celsiusToFahrenheit(24);
```

We create a function with the `fun` keyword. (Pun intended, because Brite is
fun!) We name this function `celsiusToFahrenheit` and take a single `celsius`
parameter. We call this function to calculate our result.

Brite has first-class functions. This means you can write functions anywhere in
your program.

```
fun twice(n, f) {
  f(f(n))
}

twice(0, fun(n) { n + 3 });
```

Here we call `twice` with the function `fun(n) { n + 3 }`.
