# hafly

<p align="center">
  <a href="https://haskell.org/">
    <img src="https://img.shields.io/badge/Language-Haskell-blue">
  </a>
  <a href="https://github.com/Sintrastes/hafly/actions/workflows/build.yml">
    <img src="https://github.com/Sintrastes/hafly/actions/workflows/build.yml/badge.svg">
  </a>
</p>

hafly (pronounced "half-lee") is a simple dynamic embeddable scripting language in Haskell.

See [hafly-web-repl](https://github.com/Sintrastes/hafly-web-repl) for a repo implementing an embeddable hafly REPL.

# Why?

I wanted a simple, easy to learn scripting language that was easy to embed in pure Haskell projects (so it's easy to target, for instance, ghcjs as well as native targets) that still meshes well with Haskell idioms. (Simple expression language + a monadic block syntax that can be interpreted in an arbitrary monad? Hell yeah!) As far as I could tell, such a thing did not exist -- so I made one!

Comparison with other projects:

  * [hslua](https://hackage.haskell.org/package/hslua): Easy for beginners, embeddable, yet is not pure haskell (hard to use in GHCJS).
  * [codeworld](https://github.com/google/codeworld) haskell: Possibly another good option in this space -- an educational variant of Haskell that can be run in GHCJS.
  * [hint](https://github.com/haskell-hint/hint): Very nice -- yet pretty heavyweight, and Haskell itself is not nescesarialy the easiest for beginners.
  * Various lisp/scheme implementations: Maybe not the easiest to embed. Arguably not the most intuitive syntax for beginners. Too dissimilar to Haskell.

# Why dynamically typed?

I wanted something quick and no-nonsense (implementation-wise), and easy to extend with arbitrary Haskell types and functions without mucking about with a bunch of singletons and/or type families -- building a dynamic language at first seemed like the easiest way to do that.

Given sufficent time (and/or interested contributors!) I'll probably add optional static type checking capabilities in the future.

# Features / Progress

 * [x] Naturally sandboxed and extensible.
 * [x] Multiple Dispatch (i.e. the dymically-typed version of multi-parameter type classes) 
 * [x] Higher-order functions
 * [x] Everything is an expression
 * [x] Recursion
 * [x] Syntax: What if Haskell... but with a bit of Kotlin/Rust thrown in for good measure?
 * [x] Kotlin-esque string templating.
 * [ ] IORef-backed ML-like references (only in IO!)
 * [x] Sequential blocks that can be bound to any monad.
 * [ ] Flexible binding of names in sequential blocks (do notation++)™.
 * [ ] Simple pattern matching.
 * [x] Flexible records -- because we're not cavemen.
 * [x] Record dot and [Uniform Function Call Syntax](https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax).
 * [ ] Functorial polymorphism (A generalizaton of [Array Programming Languages](https://en.wikipedia.org/wiki/Array_programming))
 * [ ] Monadic polymorphism (Compare effect polymorphism in [koka](https://koka-lang.github.io/koka/doc/book.html#why-effects))

# Examples

## Recursive Function Definitions

```haskell
fact = \n -> if(n == 0) 1 
    else n * fact (n - 1)
```

## Function Application Operator

```haskell
squared = \x -> x * x

> squared $ squared 2.5
39.0625
```

## Function Composition

```haskell
f = \x -> x + 2
g = \x -> x * 5

h  = g of f
h' = f then g

> h 1
15

> h' 1
15
```

## Universal Function Call Syntax

```haskell
squared = \x -> x * x

> 4.squared
8

> 2.5.squared.squared
39.0625
```

## Scheduling Task

```haskell
-- Interperted in a monad allowing for the scheduling of 
-- actions in IO.
remindMe time timeToRemindAgain todo = schedule time {
    result <- prompt "Don't forget! $todo";
    case result {
        Ok -> done;
        Cancel -> {
            schedule timeToRemindAgain {
                remindMe timeToRemindAgain timeToRemindAgain todo
            }
        };
    }
}
```

## User Interface

```haskell
-- Interpreted in a "builder" monad for a UI.
-- builds up a record of the form:
--    [ name: "bob", age: 42 ]
entryForm = Column {
    Row {
        Text "Name: ";
        nameEntry <- TextEntry
            .bind(name)
    };
    Row {
        Text "Age: ";
        ageEntry <- TextEntry
            .bind(age)
    }
}
```

How does it work?
-----------------

To set up your own embedded interpreter, you need to build an `InterpreterContext` -- which contains all the data (such as "built-in" functions and operators) needed to interpret a hafly program. Hafly comes with an optional `base :: InterpreterContext` "standard library" of sorts that you can use to get started -- but this can be customized to your own needs.

Once you have that, with `interpret :: InterpreterContext -> Ast -> Either TypeError Dynamic` you can take a hafly `Ast` and interpret it as a `Dynamic` value from `Data.Dynamic`. You can then attempt to grab a value of some concrete Haskell type out of that `Dynamic` with `Data.Dynamic`'s `fromDynamic`.

Hafly comes with a few built-in functions for interpreting hafly programs in specific contexts. For instance, `interpretIO` can be used to interpret a hafly program representing an IO action.
