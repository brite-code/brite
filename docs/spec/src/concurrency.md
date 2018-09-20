# Concurrency

Concurrency is a property of programs which can be executed in any order. This is an incredibly powerful property for a program to have since it allows frameworks to dynamically schedule work based on urgency or split work accross multiple computer cores to drastically speed up the program.

Yet concurrency in most programming languages comes at the cost of a lot of uncertainty. Programs which depend on shared mutable data may subtly break as operations on that data are re-ordered. Brite, however, enables the programmer to build concurrent programs fearlessly.

Concurrency in Brite is used to enable:

- Scheduling of work based on priority in user interface applications.
- Distributing work across multiple cores to be executed in parallel.
- Distributing work across multiple computers (including client/servers).

Next we will discuss tasks. The primitive which enables concurrent applications. While we discuss tasks we will ignore the topic of mutability. In the section afterwards we will address mutability and how Brite programs can be fearlessly concurrent in spite of mutability.

## Tasks

The Brite concurrency primitive is the `Task<T>` object. The `Task<T>` object represents some execution unit of the same Brite program. A `Task<T>` could be running anywhere. The same thread, a different thread, or a different computer entirely.

A task is constructed with a function that takes no arguments and returns no values.

```ite example
task = Task.new(() -> ())
```

Being a part of the “same” Brite program is an important note here. The global optimization of a Brite program means small changes to the program may break the task interface.

## Mutability

A common trend in modern programming languages is to either ban mutability entirely or to manage mutability in the type system.

The most popular of the “ban mutability” camp is Haskell. Monads enable some form of mutability or side-effects. However, operations in monads are explicitly ordered. The most popular of the manage mutability” camp is Rust. Rust uses references to manage who has mutable access to a given piece of data at any time.

Brite does neither of these things. Brite allows the programmer to go crazy with programs that mutate arbitrarily. Like JavaScript, OCaml, or Erlang. However, mutability in Brite does have restrictions:

1. Immutability is the default.
2. Mutability can only be enabled in classes.
3. Mutable values may not be shared among processes.

The first two restrictions mostly affect code style. They force mutability to be abstracted into class interfaces only when absolutely necessary.

The third restriction is what enables Brite to have mutability while also having a powerful concurrency story. Only immutable values can be shared between processes!

To enable the sharing of a class among multiple processes you need to implement the `Send` interface. You may only implement `Send` if your class has no mutable fields and all field types also implement `Send`. If a base class implements `Send` then child classes must also implement `Send`.

Records and tuples cannot have mutable properties so if all their components are also `Send` then the record and tuple implements `Send` itself. In that way the `Send` interface is special.

```ite example
class MyMutableValue<T>(mutable value: T)

class MyImmutableValue<T>(value: T) implements Send

myFunction() -> (
  // Mutable values may be changed in the current process
  // but cannot be sent to another process.
  mutableValue = MyMutableValue(1)
  mutableValue.value := 2

  // Immutable values cannot be changed so they may
  // implement the `Send` interface.
  immutableValue = MyImmutableValue(3)
)
```

To have mutability across processes you can create a channel interface for updating a mutable value owned by a single process. All channel messages are synchronized so all updates will occur synchronously in the order the messages were received.
