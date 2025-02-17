# What is Lux?

Lux is a new programming language in the making.

It's meant to be a functional, statically-typed Lisp that will run on several platforms, such as the Java Virtual Machine and JavaScript, Python, Lua, or Ruby interpreters.

## What's the current version?

0.8.0

## How far ahead is the project?

Lux is in the **beta** stage.

The JVM compiler is pretty stable and the standard library has grown to a respectable size.

Also, support for JavaScript, Python, Lua, and Ruby has been added.

## What's the license?

[Mozilla Public License Version 2.0](https://mozilla.org/MPL/2.0/)

## What's interesting about the language?

### Inspirations

The language is mostly inspired by the following 3 languages:

* Clojure (syntax)
* Haskell (functional programming)
* Standard ML (polymorphism)

### Concurrency

Lux supports multiple paradigms for concurrent programming:

* Threads and atomic references.
* Asynchronous programming (i.e. promises & futures).
* Functional Reactive Programming (FRP).
* Software-Transactional Memory (STM).
* The actor model.

More paradigms will be supported in the future.

### Multi-platform

Lux can compile to JVM bytecode, and thereby it can run anywhere Java can.

On top of that, Lux can compile to JavaScript code, Python, Ruby, and Lua.

This makes Lux an extremely versatile language.

And more platforms are coming in the future!

**Note**: Lux code can also be compiled into libraries that can be consumed in any of the platforms Lux can compile to; which means Lux makes for amazing glue code for polyglot projects.

### Extensibility

Lux is being built to be the most extensible and versatile language ever made.

Not only can its syntax be extended through macros, but even the semantics of the language, its available roster of optimizations, and even its mechanisms for code-generation can be extended with a mechanism for compiler extension which is similar to its mechanism for macro definition.

A new (experimental) meta-compiler architecture has been added which will enable Lux to become on its own a platform for polyglot programming and language experimentation.

### Types

They are implemented as plain-old data-structures whose expressions get eval'ed by the compiler and integrated into the type-checker.

That means it's actually possible to generate types via functions and macros.

They can also be accessed from within macros to generate all sorts of type-driven code.

### Macros

Unlike in most other lisps, Lux macros are monadic.

The `(Meta a)` type is the one responsible for the magic by threading `Lux` compiler-state instances through macros.

You can use `macro` to define these monadic macros.

Alternatively, you can use the `syntax` macro, which also offers monadic parsing of `Code` tokens for convenience.

## Is there a community for this?

We have a Discord server: https://discord.gg/YPvEGANkch

Come join the forum: http://luxlang.freeforums.net/

If you want to communicate with me directly, just email: luxlisp@gmail.com

## How can I edit Lux code?

Check out the Emacs plugin for it: https://github.com/LuxLang/lux/tree/master/lux-mode

## Where do I learn Lux?

The main resource is [the book](documentation/book/the_lux_programming_language/index.md).

It will always be up-to-date with the latest stable version of the language.

Also, you can check out [the documentation for the currently available modules](documentation/library/standard/jvm.md).

## How can I contribute?

For starters, you can check out the [Trello board](https://trello.com/b/VRQhvXjs/lux-jvm-compiler) for Lux development.

I'll be putting there tasks that people can contribute to; both in the compiler and outside (like plugins for editors).

Writing libraries in Lux will also help a lot in making this a more practical language for day to day use.

##### Copyright (c) 2014-2025 Eduardo Emilio Julián Pereyra. All rights reserved.
