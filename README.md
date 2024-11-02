# rinilang

Ri is an experiment on language design. It is a concurrent, functional, OOP-like language. It's
designed to be simple, with a low learning curve, while still being powerful enough to build large
and robust scalable systems.

- [Cuttle for rustaceans](#cuttle-for-rustaceans)
- [Design](#design)
  - [Operator overloading](#operator-overloading)
  - [Zen](#zen)
  - [Roadmap](#roadmap)

## Ri for Rustaceans

Superficially, Ri is very similar to Rust. Some syntax has been adapted for readability, and also
for better familiarity for users of other languages. These are, mainly:

- Semicolons are not necessary. The parser will automatically insert them.
- Type inference is global, so function signatures are fully optional, even if you deal with generic
code. However, exported types which do not have a signature will emit a warning
- Modules and types do not need `::`. Instead, `.` is used. This is nicer to read and does not have
any problems since its easy to deduce at compile time whether something is a type or module.
- `case` is used instead of `match`. This is more familiar to other languages, and doesn't hide the
fact that it is simply a safer, more powerful `switch`.
- `type` is used instead of `struct` and `enum`.
- `#[derive(..)]` is achieved through `type Foo: Eq, Ord`
- Types can be hinted in any expression: `parse("foo") : T` is preferred over `parse.<T>("foo")` (or
Rust's turbofish `::<>`)

### `Option` types are implicit

Instead of `Option<T>`, optional types are denoted `T?`, and the empty value is denoted `null`.
Contrary to C, you cannot create a `null` value out of any object, *only* `T?`. It's essentially
implemented in the same way as Rust's `Option<T>`, but its nicer to read and easier to refactor.

```rs
// os.getenv() returns `String?`

let port: Int = os.getenv("PORT").read()    // error! `getenv` could be null!
let port: Int? = os.getenv("PORT")?.read()  // OK: port will be null if the variable is unset

startServer(port ?? 8080) // you can provide a default value like this

// or, you can run some code conditionally
if let path = os.getenv("FOO") {
  // ...
}
```

### Statements can't be expressions

In Rust, the `match`, `loop`, `while`, `for`, and `if` constructs are expressions, which leadings to
some quite ugly terse statements. In Ri, `if` expressions have a different, more readable syntax:

```rs
let foo = if cond() then a else b
```

Unlike `if` statements, you cannot omit the `else` clause, so nesting is not ambiguous. It is
analogous to the ternary operator found in many languages (`x ? a : b`), but is more readable with
nested conditions.

`case` is still like Rust's `match`, allowing you to use implicit returns more nicely:

```rs
fn hello(name) {
  case name {
    "rini" -> "Oh, hey me"
    _ -> "Hi there, " ++ name ++ "!"
  }
}
```

## Design

### Operator overloading

Operator overloading in Ri is done using type classes. Unlike most languages, `(+)`, `(-)` etc
cannot simply be overloaded for *any* type, they must implement *`Number`*. Just like `(==)` is
expected to compare two values equal, the arithmetic operators are expected to do arithmetic.

**TBD:** The main idea is that the operators should follow a numeric tree. This will also include
`Complex`, `Rational` and `BigInt` types

**TBD:** Literals could be overloaded: `1` would have type `any Integral`, while the compiler will
automatically choose `Int` in most cases (Haskell does this via the monomorphism restriction[^1]).
Overloading `String` literals is also useful for type-safe APIs for markup.

[^1]: https://wiki.haskell.org/Monomorphism_restriction

- Equality: `Eq`, `(==)`, `(!=)`
- Ordering: `Ord`, `compare`, `(<)`, `(<=)`, `(>)`, `(>=)`
- Integer arithmetic: `Integral`
  - `(+)`, `(-)`, `(*)`, `(%)` (modulo) and `(//)` (euclidian division)
  - `maxBound`, `minBound` minimum and maximum values
- Floating arithmetic: `Floating`
  - `(/)` (fractional division)

### Unresolved bits

Some ramblings about things I have yet to decide on the language.

#### Notation for types

Should we follow the conventional template-like syntax? (e.g. `List<Int>`)

It's a double edged sword, but adopting a style closer to ML-type languages clears out some
confusion about advanced types:

```rs
// Conventional syntax, for reference
type Option<T> {
  Some(T)
  None
}

fn map<T, U>(opt: Option<T>, f: T -> U) -> Option<U>

fn map<T, U, S: Sequence>(obj: S<T>, T -> U) -> S<U>

// Function-like syntax:
// All `Option` really is, is a type function. `a` is a parameter, just like in a regular function,
// and it returns a concrete type when called.
type Option(a) {
  Some(a)
  None
}

// We can get rid of explicit parameters in functions, but it may not actually be good to
fn map(opt: Option<a>, f: a -> b) -> Option<b>

// Constraints get weird...
fn map(T, U, S: Sequence)(obj: S(T), f: T -> U) -> S(U)
```

Haskell constraints are also kind of incompatible with Rust syntax, take for example:

```hs
class Monad m => Stream s m t where
  uncons :: s -> m (Maybe (t, s))
```

Granted, you likely wouldn't need code like that in Cuttle, since we don't use monads as much. It
can be expressed with associated types:

```rs
trait Stream<M: Monad> {
  type Item;

  fn uncons(self) -> M<Option<(Self::Item, Self)>>
}
```

So perhaps complex constraints can be avoided overall with associated types. I have yet to make the
connection between them and Haskell's type system.

### Zen

- Readability still counts.
- One way to do it. Many ways to structure it.
- Fatal errors will happen eventually. Handle them!
- Interfaces make good abstractions.
- But make sure you're abstracting many things and not one.

### Roadmap

- [ ] Bootstrap compiler/interpreter (Haskell, likely outputting C, not very optimised)
- [ ] Runtime system:
  - [ ] Concurrent garbage collector
  - [ ] Lightweight threading, asynchronous I/O
  - [ ] Parallelism with multiple runtime threads
- [ ] Language specification (will be made in parallel to the compiler/runtime)
- [ ] Module system, packages, and build tool
- [ ] Standard libraries:
  - [ ] Standard types and builtins
  - [ ] I/O libraries: filesystem, sockets, TCP, etc.
  - [ ] Higher-level protocols: HTTP(S), WebSockets
  - [ ] Data serialization: JSON with reflection
  - [ ] Framework-agnostic libraries: common API/interfaces for web servers
- [ ] Ecosystem libraries:
  - [ ] Web routing, frameworks
  - [ ] HTML templating
  - [ ] Parsing libraries
- [ ] Self-hosting compiler:
  - [ ] x86-64 compiler, dynamic and static linking
  - [ ] Stable, future-compatible spec
