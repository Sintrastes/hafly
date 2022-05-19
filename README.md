# hafly

hafly (pronounced "half-lee") is a simple dynamic embeddable scripting language in Haskell.

# Why?

I wanted a simple, easy to learn scripting language that was easy to embed in pure Haskell projects (so it's easy to target, for instance, ghcjs as well as native targets) that still meshes well with Haskell idioms. (Simple expression language + a monadic block syntax that can be interpreted in an arbitrary monad? Hell yeah!) As far as I could tell, such a thing did not exist -- so I made one!

Comparison with other projects:

  * hslua: Easy for beginners, embeddable, yet is not pure haskell (hard to use in GHCJS).
  * codeworld haskell: Possibly another good option in this space -- an educational variant of Haskell that can be run in GHCJS.
  * hint: Very nice -- yet pretty heavyweight, and Haskell itself is not nescesarialy the easiest for beginners.
  * Various lisp/scheme implementations: Maybe not the easiest to embed. Arguably a bit of a tricky syntax for beginners.

# Why dynamically typed?

I wanted something quick and no-nonsense (implementation-wise), and easy to extend with arbitrary Haskell types and functions without mucking about with a bunch of singletons and/or type families -- building a dynamic language at first seemed like the easiest way to do that.

Given sufficent time (and/or interested contributors!) I'll probably add optional static type checking capabilities in the future.

# Features

 * Syntax: What if Haskell... but with a bit of Kotlin thrown in for good measure?
 * Sequential blocks that can be bound to any monad.
 * Flexible binding of names in sequential blocks (do notation++)™.
 * Simple pattern matching.
 * Flexible records -- because we're not cavemen.
 * Record dot and universal function call syntax.


# Examples

```
-- Interperted in a monad allowing for the scheduling of 
-- actions in IO.
remindMe time timeToRemindAgain todo = schedule time {
    result = prompt "Don't forget! $todo"
    case result {
        Ok -> done
        Cancel -> {
            schedule timeToRemindAgain {
                remindMe timeToRemindAgain timeToRemindAgain todo
            }
        }
    }
}
```

```
-- Interpreted in a "builder" monad for a UI.
-- builds up a record of the form:
--    { name: "bob", age: 42 }
entryForm = Column {
    Row {
        Text "Name: "
        nameEntry = TextEntry
            .bind(name)
    }
    Row {
        Text "Age: "
        ageEntry = TextEntry
            .bind(age)
    }
}
```
