# C. rini's Unnamed concurrenTT LanguagE

This project is an experiment with language design. The language is codenamed Cuttle, but it's name
upon release will likely be different.

Cuttle is a concurrent, Functional Object-Oriented language. It's designed to be simple, with a easy
learning curve, while still being powerful enough to build large scalable systems.

- [Cuttle for rustaceans](#cuttle-for-rustaceans)
- [Design](#design)
  - [Operator overloading](#operator-overloading)
  - [Zen](#zen)
  - [Roadmap](#roadmap)

## Cuttle for Rustaceans

Superficially, Cuttle is very similar to Rust. Some syntax has been adapted for readability, and
also for better familiarity for users of other languages. These are, mainly:

- Semicolons are not necessary. The parser will automatically insert them.
- Type signatures are not necessary most of the time, but the compiler will suggest you to add them
for any exported functions and constants. Type inference is global, and it will even infer generics!
- Functions and types do not need `::`. Instead, `.` is used. This is nicer to read and does not
have any problems since its easy to deduce at compile time whether something is a type or module.
- `case` is used instead of `match`. This is more familiar to other languages, and doesn't hide the
fact that it is simply a safer, more powerful `switch`.
- `type` is used instead of `struct` and `enum`.
- `#[derive(..)]` is achieved through `type Foo: Eq, Ord`

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

## Design

### Operator overloading

Operator overloading in Cuttle is done using type classes. Unlike most languages, `(+)`, `(-)` etc
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

(please ignore the section below its where I pretend the language already exists)

```
trait Sequence {
  const empty: Self
  fn append(Self, Self) -> Self
}

operator (++) Sequence.append

trait Eq {
  fn eq(x: Self, x: Self) { !(x != y) }
  fn ne(x: Self, y: Self) { !(x == y) }
}

operator (==) Eq.eq
operator (!=) Eq.ne

type Ordering { Less, Equal, Greater }

trait Ord: Eq {
  fn cmp(Self, Self) -> Ordering {
    case {
      if x == y -> Equal
      if x <= y -> Less
      _ -> Greater
    }
  }

  fn le(x, y) { let Less | Equal = compare(x, y) }
  fn ge(x, y) { y <= x }
  fn lt(x, y) { !(x >= y) }
  fn gt(x, y) { !(y >= x) }

  fn max(x, y) { if x <= y then x else y }
  fn min(x, y) { if x <= y then y else x }
}

operator (<=) Ord.le
operator (>=) Org.ge
operator (<) Ord.lt
operator (>) Ord.gt

trait Number {
  const minBound: Self
  const maxBound: Self
}
```

```abnf
Block = "{" *( Stmt eol ) "}"

CaseExpr = "case" Expr "{" *( CaseArm eol ) "}"

CaseArm = Pattern [ "if" Expr ] "->" Expr

Const = Integer / Float

Integer =  [ sign ] 1*digit 1*( *"_" *digit )
Integer =/ [ sign ] "0o" 1*( *"_" *octDigit )
Integer =/ [ sign ] "0x" 1*( *"_" *hexDigit )
Float   =  [ sign ] 1*digit "." 1*digit [ ( "e" / "E" ) 1*digit ]

sign      = "+" / "-"
digit     = %x30-39 ; 0-9
octDigit = %x30-36 ; 0-6
hexDigit = digit / %x41-46 / %61-%66 ; 0-9, a-f, A-F

skip = " " / comment
comment = "//" *(!eol .)
comment =/ "/*" *(!"*/" .) "*/"
eol = "\n" / "\r\n"
```

```
fn runTask(name: String) -> Maid<()> {
  let tasks <- asks(.taskfile)
  let task = tasks.find(|x| x.name == name) ?? throw UnknownTask(name)

  if let true <- asks(.dryRun) { return }

  case task.lang {
    "bash" | "sh" -> {
      let quiet <- asks(.quiet)
      runProc("sh", "maidtask.sh", [if quiet then "-euv" else "-eu"], input)
    }
    "haskell" | "hs" -> runProc("runhaskell", "MaidTask.hs", ["-W"], input)
    "javascript" | "js" -> runProc("node", "maidtask.js", [], input)
    _ -> throw UnknownLanguage(lang)
  }
}

fn runProc(cmd, file, args, input) {
  guard let false <- asks(.dryRun)

  let name, tmp = os.mktemp(file) // -> String, File
  tmp.write(input) // &File, r -> ()
  tmp.close() // File -> (), drop
  case os.process(cmd, args ++ [name]).wait() {
    ExitFailure(i) -> exit(i)
    _ -> {}
  }
}

fn findTasks(blocks: [Block]) -> [Task] {
  case blocks {
    [Heading(h, _), Paragraph(p), ..blocks] if p.words() == magic ->
      findTasks(blocks.takeUntil(match Heading(h2, _) if h2 <= h))
    [_, ..rest] -> findTasks(rest)
    [] -> []
  }
where
  const magic = ["<!--", "maid-tasks", "-->"]

  fn findName(blocks) {
    case blocks {
      [Heading(_, name), ..rest] -> findDesc(emptyTask, rest)
      [_, ..rest] -> findName(rest)
      [] -> []
    }
  }
  fn findDesc(task, blocks) {
    case blocks {
      [Paragraph(desc), ..rest] -> findCode(Task { desc, ..task }, rest)
      rest -> findCode(Task { desc: "[No description]", ..task }, rest)
    }
  }
  fn findCode(task, blocks) {
    case blocks {
      [Code(lang, code), ..rest] -> Task { lang, code, ..task }
      [Paragraph(_), ..rest] = findCode(task, rest)
      rest -> findTask(rest)
    }
  }
}

fn listTasks() {
  let file, tasks <- asks(.taskfile)
  let style <- asks(.style)

  print(style.primary)
  println("Tasks in {}" % file.filename())
  println()
  for task of tasks {
    print(style.secondary)
    println("  {}" % task.name)
    print(style.tertiary)
    println(task.desc.lines().map("    " ++).unlines())
  }
}

fn parseMarkdown(src: String) -> [Block] {
  let lines = src.lines()

  for line of lines {
    case {
      if isBlank(line) -> nil
      if isHeading(line) -> {
        let hashes, line = line.breakOnce(!= '#')
        Heading(hashes.len(), line.strip())
      }
      if isFenced(line) -> {
        let end, lang = line.breakOnce(!= line[0])
        let src = lines.takeUntil(|x| x.startsWith(end))
        Code(lang, src.unlines())
      }
      if isIndented(line) -> {
        let src = lines.takeWhile(|x| isIndented(x) || isBlank(x))
        Code("", src.map(|x| x[4..]).unlines())
      }
      _ -> {
        let src = lines.takeUntil(|x| isHeading(x) || isFenced(x) || isBlank(x))
        Paragraph(src.unlines())
      }
    }
  }
where
  fn isBlank(x) { x.all(isSpace) }
  fn isHeading(x) { x.startsWith("#") }
  fn isIndented(x) { x.startsWith("    ") }
  fn isFenced(x) { x.startsWith("```") || x.startsWith("~~~") }
}

type Block {
  Heading { depth: Int, text: String }
  Code { lang: String, text: String }
  Paragraph { text: String }
}
```
