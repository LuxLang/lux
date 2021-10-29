[![Gitter](https://badges.gitter.im/LuxProgrammingLanguage/community.svg)](https://gitter.im/LuxProgrammingLanguage/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

## What is Lux?

Lux is a new programming language in the making.

It's meant to be a functional, statically-typed Lisp that will run on several platforms, such as the Java Virtual Machine and JavaScript, Python, Lua, or Ruby interpreters.

### What's the current version?

0.6.3

### How far ahead is the project?

Lux is in the **beta** stage.

The JVM compiler is pretty stable and the standard library has grown to a respectable size.

Also, new experimental support for JavaScript, Python, Lua, and Ruby has been added.

### What's the license?

[Custom License](license.txt)

Read carefully before using this project, as the license disallows commercial use, and has other conditions which may be undesirable for some.

However, commercial use is allowed for patrons under the terms of the [Patron License](PATRON_LICENSE.md).

You can become a patron by supporting Lux through [Patreon](https://www.patreon.com/lux_programming_language).

## What's interesting about the language?

### Inspirations

The language is mostly inspired by the following 3 languages:

* Clojure (syntax, overall look & feel)
* Haskell (functional programming)
* Standard ML (module system)

### Types

They are implemented as plain-old data-structures whose expressions get eval'ed by the compiler and integrated into the type-checker.

That means it's actually possible to generate types via functions and macros.

### Module system

The module system is heavily inspired by Standard ML.

The main difference between Lux and Standard ML is that Standard ML separates interfaces/signatures and implementations/structures from the rest of the language, whereas Lux implements them on top of the base language.

How?

By implementing interfaces/signatures as record-types and implementations/structures as actual records.

##### But, why not just use type-classes?

Haskell's type-class system forces the user to only specify 1 instance for any given type-class and its argument.

If there are more than 1 possible valid instances (as is the case for Monoid of Int), you have to resort to _newtype hacks_ to be able to provide alternative implementations.

By using a system like Standard ML's, that problem is averted.

Additionally, by hosting the module system on top of records, which are regular values, you get the further benefit that structures can be parameterized at run-time just like any other value.

You can also write functions that take and return structures (as _functors_ do in Standard ML), and you can generate structures on the fly.

> Also, Lux now offers a mechanism for easy polymorphism, just like Haskell's type-classes, but built upon its module system, thanks to the `library/lux/type/auto` module and its `##` macro.

> You can learn more about that by reading the book and the documentation.

### Functional programming

While the means to do Java-interop are provided, Lux is commited to functional programming.

Functions are curried and partial application is as simple as just applying a function to less arguments than it needs (as in Haskell).

e.g.

```
... Add 1 to each number in the list.
(each (+ 1) (list 1 2 3 4 5))
```

### Macros

Unlike in most other lisps, Lux macros are monadic.

The `(Meta a)` type is the one responsible for the magic by threading `Lux` compiler-state instances through macros.

You can use `macro:` to define these monadic macros.

Alternatively, you can use the `syntax:` macro, which also offers monadic parsing of Code tokens for convenience.

### Custom pattern-matching

##### Wait... wut?

Custom pattern-matching basically means that you can use macros to provide custom syntax and features on top of the pattern-matching macro `case`.

For instance, the `list` and `list&` macros are used to build lists.

But you can also use them to destructure lists inside pattern-matching:

```
... Try to pattern-match against a list, extract its elements and multiply them.
(: (Maybe Nat)
   (case (: (List Nat)
            (list 2 3))
     {#Item x {#Item y {#End}}}
     {#Some (* x y)}

     _
     {#None}))

(: (Maybe Nat)
   (case (: (List Nat)
            (list 2 3))
     (^ (list x y))
     {#Some (* x y)}
   
     _
     {#None}))
```

There is also the special **^or** macro, which introduces *or patterns*:

```
(type: Weekday
  (Variant
    {#Monday}
    {#Tuesday}
    {#Wednesday}
    {#Thursday}
    {#Friday}
    {#Saturday}
    {#Sunday})))

... Returns TRUE if it's either Saturday OR Sunday.
(def: (weekend? day)
  (-> Weekday Bit)
  (case day
    (^or {#Saturday}
         {#Sunday})
    true

    _
    false))
```

> Please note that `^` and `^or` are just macros like any other and anyone can implement them.

### Is there a community for this?

Say hi at Gitter: https://gitter.im/LuxProgrammingLanguage/community

Come join the forum: http://luxlang.freeforums.net/

If you want to communicate with me directly, just email: luxlisp@gmail.com

### How can I edit Lux code?

Check out the Emacs plugin for it: https://github.com/LuxLang/lux/tree/master/lux-mode

### Where do I learn Lux?

The main resource is [the book](documentation/book/the_lux_programming_language/index.md).

It will always be up-to-date with the latest stable version of the language.

Also, you can check out [the documentation for the currently available modules](documentation/library/standard/jvm.md).

### How can I contribute?

For starters, you can check out the [Trello board](https://trello.com/b/VRQhvXjs/lux-jvm-compiler) for Lux development.

I'll be putting there tasks that people can contribute to; both in the compiler and outside (like plugins for editors).

Writing libraries in Lux will also help a lot in making this a more practical language for day to day use.

##### Copyright (c) 2014-2021 Eduardo Julian. All rights reserved.
