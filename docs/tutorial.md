
# Getting Started

To get started playing around with Hafly, you may want to install the _hafly interpreter_ for your operating system. However, this is actually optional -- as you can just as well use the web-based version of the hafly interpreter.

When opening the hafly interperter for the first time, you'll be greeted with a caret `>` symbol, prompting you to enter some text. The types of things you can enter at this prompt can roughly be understood as either:
 
  1. A query.
  2. A command.
  3. A new definition.

For example, by entering `> 2 + 2`, and then pressing the enter key, you are essentially asking the hafly interpreter (querying it): "What is 2 + 2?" -- to which it will respond: `4`. You can try out entering any sort of basic arithmetic calclations this way with `+`, `-`, `*` and `/` (for division), and Hafly should be able to answer your query for you.

However, you can also issue _commands_ to the interpreter which tell your computer to preform some kind of action. For instance, in programming language tutorial tradition, to print the string "Hello, world!" to the terminal output, you can tell Hafly:

```
> printLn "Hello, world!"
```

to which it will gladly output `Hello, world!` for you on the next line.

# Conceptual Introduction

The fundamental concept behind the Hafly language is that of _expressions_, and Hafly is an _expression-driven_ language. If you're familiar with other programming languages, or even the ideas behind them, this might seem a bit strange at first, but ultimately it makes Hafly a simpler and more cohesive language.

Consider an analogy that is sometimes given for computer programs by comparing them with _recipes_. For instance, let's say we're interested in making an omlette. A recipe for a basic omlette might go something like:

To make an omlette:

  * First, crack some eggs into a bowl.
  * Next, add some salt and whisk them together.
  * Heat up a pan on the stovetop over medium heat.
  * Once it has heated, add in some butter.
  * Once the butter has heated, pour in the egg mixure.
  * Wait until the egg mixutre has mostly solidified.
  * Fold the omlette in half with a spatula.
  * Plate and serve!

Many programming language's "default" way of working is something like that -- you give the computer a set list of instructions telling it _exactly_ how to do something -- most likely in more detail than was outlined above, since we need to give our instruction to "dumb" machines, rather than smart human beings who can fill in missing details, and fill in the blanks in a recipe. For instance, translated into a programming language syntax, this set of instructions might look something like:

```
makeOmlette(bowl, eggs, pan, butter, salt) {
    crackEggs(into = bowl)
    add(salt, bowl)
    whiskContents(bowl)
    heat(pan)
    transferContents(bowl, pan)
    waitUntilSolidified(pan)
    foldContents(pan)
    serveContentsOf(pan)
}
```

Hafly still lets you work this way if you really need to. For instance, in some areas such as building interactive programs (for instance, prompting the user for input, and printing things to the screen), it is hard to work _without_ thinking of things in this way. However, the core way of thinking about things when using Hafly is more like _making definitions_ of things rather than listing specific instructions.

For instance, in Hafly, the typical way of approaching the problem of say, making an omlette, would be to define what an omlette _is_, rather than enumerating the specific steps of making one. For instance:

A basic omlette is a dish made up of cooked, beaten eggs that have been folded in half.

This reads more like the definition of a dish that one might find in a cook book or encyclopedia! 

In Hafly, this might look something like:

```
basicOmlette = cooked (beaten eggs) >> foldedInHalf
```

where the funny-looking `>>` symbol in the above may be read in English in the above example as something like "that have been".

# Expressions and Definitions

So, with all that background philosophizing out of the way: What actually is an expression?

An expression in Hafly is just a symbolic expression describing how something is built up out of smaller parts. For instance, a very common type of expression that you will find in both everyday life, as well as in the vast majority of programming languages is a _numeric expression_ -- things like `2 + 2`, `42`, `12 * 7 - 2`, `14 / 7`. 

Hafly lets you _define_ expressions by giving them a name, followed by an equals sign, followed by the expression itself. For instance:

```
myAge = 32
```

or

```
myDogsAge = 11
```

Hafly also lets you _reference_ expressions in other expressions in the same file.

```
myDogsAge = 11
myDogsAgeHumanYears = 7 * myDogsAge
```

One of the nice things about Hafly is that because we're just giving _definitions_ here, rather than sequences of steps, when you're writing out expressions like that that depends on other expressions -- you don't have to worry at all about the order that you define them in. You could just as easily have defined the above as:

```
myDogsAgeHumanYears = 7 * myDogsAge
myDogsAge = 11
```

and Hafly would have no issue correctly interpreting your definitions!

# Types

Every valid expression in Hafly has a _type_ associated with it. For instance, `4`, `2`, and `-72` have type `Int`, as they are integers (i.e. whole numbers), whereas `4.0`, `2.1`, and `-32.791` have type `Double`, as they are decimal numbers -- and `"4"`, `"4.0"`, and `"Hello, world"` have type `String`, as they are strings of text.

Types can be viewed as a mechanism of specifying for what sorts of values different operations or functions will _make sense_. For instance, the addition operator `+` makes sense when combining numeric types like `Int` and `Double`s, so expressions like these are perfectly "valid":

  * `2 + 2`
  * `2.0 + 3`
  * `5.1 + 2.0 + 7.3`

Yet something like `2 + "Hello world!"` is _not_ valid, as we're trying to add a number to a string, which doesn't really make much sense! If you try to type something like this into the hafly interpreter, it will give you a _type error_ telling you exactly what went wrong.

TODO: Example

The current version of Hafly is _dynamically typed_ -- which is just a fancy way of saying that if you write a Hafly program that has invalid expressions in it like `2 + "Bob"`, you won't get any errors until Hafly actually tries to _evaluate_ that expression.

For instance, if you write:

```
if (someTrueCondition) \
  2 + 2 \
else 2 + "Bang!"
```

at the `hafly` interpreter (where for instance, `someTrueCondition` could be defined as just `True`), there will be no issues, as the "else" brach of an `if-else` statement is not evaluated unless the condition in the `if( ... )` is `False`.

By contrast, in a _statically typed_ language like Haskell, before evaluating your program (or expression), there is a _type-checking_ stage that is run that prevents (or warns, depending on the language!) the user from running their program if there are problematic expressions like `2 + "Bang!"` that could be evaluated at run-time. 

While which approach is better is often a matter of fierce debate amongst software developers, _statically typed languages_ are generally seen as being _safer_ (especially for large projects) as you can check for the abscense of such type errors before you even run your program -- but some developers view dynaically typed languages as being better for smaller applications and scripts where this isn't as much of an issue.

## Conditionals

## Records

# Functions

Raw values like `4.2`, `"Bob"`, and even records like `{ name: "Bob", age: 42 }` are not very useful on their own -- they're just raw data! To do interesting things, we need ways to _act on_ values to produce new, more interesting values.

Hafly actually offers two different ways of acting on values -- in line with it's expression-oriented philosophy. The first way of acting on values is with _functions_, and the second one is with _sequential expressions_, which we'll go over in the next section of this tutorial.

Functions in Hafly should not be confused with what are sometimes also called "functions" in other programming languages. Oftentimes what are called "functions" in other languages, like Java, C++, Python, Javascript and what have you are essentially just named sequences of actions for the computer to preform (like we talked about in our omlette recipe example). 

Functions in Hafly are the same as _mathematical_ functions, like you may have learned about in a primary/grade-school algebra course, or a university _discrete mathematics_ course. 

Now, before you start groaning and taking out your high-school algebra textbook and pouring over the definitions of things like domain and codomain, or trying to work out something like the definiion of `(g º f)(x)` -- don't worry about it. We're going to cover this from scratch in an easy to understand way.

Functions are essentially things that take some input value of a given type, and produce some output value of (potentially) a different type. That's it! Let's look at a few examples to solidify this point. 

`toString` is a function in Hafly that takes numbers and converts them into their string representation -- and to apply a function to it's argument in Hafly, we just juxtapose the name of the function next to it's argument. For example, `toString 2` outputs `"2"`, and `toString 3.2` outputs `"3.2"`.

Functions can also have more than one argument, for instance, if we have a function `add` that adds two numbers, then we can call it by writing `add`, followed by all of it's arguments seperated by spaces like so: `add 2 3`. This brings up another important example -- all of the familiar numeric operations in Hafly: `+`, `-`, `*`, and `/` -- are just functions with a specail syntax for calling them.

Sometimes people will call functions that are called this way _operators_ -- but conceptually they're exactly the same as functions. They're just called differently.

## What functions are not

Hopefully by this point, you should have a fairly good understanding of what functions are, and how to use them with Hafly. However, before we go on, it is important to also clarify what functions are _not_.

Functions are defined as mappings from one type of value (or more in the case of multi-argument functions) to another type of value -- however, this should be understood as being the _exclusive_ feature of functions -- they can't do anything beyond this mapping.

For instance, `printLn` as seen in our introductary "Hello world" program in _not_ a function.

Why not? Well, it certainly takes in an input (a `String`) -- and it may not immediately be obvious, but it _does_ produce an ouput (a specail type in Hafly called `Unit` with a single value of that type, written `()`). However, that's not all `printLn` does -- it also prints something to the screen! Thus, it can't be a function.

## User-defined Functions

Just using the built-in functions of Hafly will only get you so far. Eventually you'll want to write your own functions! There are at least two good reasons for doing this:

  1. Documentation. Sometimes the intention between a pattern of an expressions might not be clear. Functions let us give those patterns a name, and to apply those patterns by reffering to them by name. 
  2. Abstraction. Even when working with expressions, rather than statements -- it is possible to work at different levels of detail, and functions let us define higher-levels of abstraction that save us from working with lower-level details.

You'll see this in action as we continue on with this tutorial -- but first, let's see what defining a function in Hafly looks like. Let's go back to our example in section ... where we calculated `myDogsAgeHumanYears` based on `myDogsAge`. What if we wanted to calculate _any_ dog's age in human years? We could write a function to do that for us:

```
inPersonYears = \dogYears -> dogYears * 7
```

Notice here that a function is defined just the same as any other expression in Hafly. There is no specail syntax for this. A function in Hafly will always either be a built-in function (like `+`, or `toString`), or a _function expression_ -- what other languages might call a _lambda expression_.

We chose the term _function expression_, as the connection with the [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) isn't of particular concern to the nacent language learner -- and _function expression_ just makes more sense -- it's an expression for defining a function!

Now, let's go back to our `inPersonYears` example, and break it down peice by piece to help make sense of the syntax. It can be read:

```
-- inPersonYears
inPersonYears 
  -- is
    = 
    -- A function of dogYears
      \ dogYears 
        -- Returning 
          -> 
            -- The inputted value in dog years, 
            -- times seven.
            dogYears * 7
```

## Higher-order functions

# Actions and Sequences



# Example: User Interfaces

Ok -- let's do a bit of an exercise. Open up your phone (or laptop, or tablet -- what have you) -- and set a timer for 15 minutes from now. Chances are, unless you're some kind of command-line guru, you probably used a GUI (or _graphical user interface_ -- _user interface_ or UI for short) to accomplish that task. And the fact of the matter is, as useful as it might be, most people are not command line gurus.

Even if you are a command-line guru, or would like to be (which, considering that you're learning a scriping language, there's probably a good chance you do) -- visual feedback and manipulation of data (two things GUIs provide) are still incredibly important tools.

Luckily for us, Hafly provides some very convinient tools for building up UIs -- be they graphical, or [textual](https://github.com/reflex-frp/reflex-vty).

In the last section, we saw how to define basic _sequences_ of _actions_ in Hafly. However, that same sequence syntax is also incredibly convinient for describing different sorts of expressions -- such as expressions describing the behavior and layout of user interfaces.


