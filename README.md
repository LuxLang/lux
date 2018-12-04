## What is Lux?

Lux is a new programming language in the making.

It's meant to be a functional, statically-typed Lisp that will run on several platforms, such as the Java Virtual Machine and JavaScript interpreters.

### What's the current version?

0.5.0

### How far ahead is the project?

Lux is in the **beta** stage.

The JVM compiler is pretty stable and the standard library has grown to a respectable size.

Also, new experimental support for Android has been added.

### How can I use it?

You should use the Leiningen plugin for Lux to compile your programs and manage your dependencies.

You can find it here: https://github.com/LuxLang/lux/tree/master/lux-lein

After compiling your program, this will generate a directory named "target" and put all the .class files there.

Then, you can run the program like this:

	java -jar target/jvm/program.jar

### Sample

To take a look at sample Lux projects, check these repositories:

* https://github.com/LuxLang/tutorial1
* https://github.com/LuxLang/lux/tree/master/luxdoc

The `luxdoc` program was actually used to generate the documentation for the standard library (located here: https://luxlang.github.io/lux/)

### What's the license?

[Custom License](license.txt)

Read carefully before using this project, as the license disallows commercial use, and has other conditions which may be undesirable for some.

## What's interesting about the language?

### Inspirations

The language is mostly inspired by the following 3 languages:

* Haskell (functional programming)
* Clojure (syntax, overall look & feel)
* ML (module system)

The compiler is even implemented in Clojure.

### Types

They are implemented as plain-old data-structures whose expressions get eval'ed by the compiler and integrated into the type-checker.

That means it's actually possible to generate types via functions and macros.

### Module system

The module system is heavily inspired by ML, and both signatures and structures are supported.

The main difference between Lux and ML is that ML separates signatures and structures from the rest of the language, whereas Lux implements them on top of the base language.

How?

By implementing signatures as record-types and structures as actual records.

##### But, why not just use type-classes?

Haskell's type-class system forces the user to only specify 1 instance for any given type-class and its argument.

If there are more than 1 possible valid instances (as is the case for Monoid of Int), you have to resort to _newtype hacks_ to be able to provide alternative implementations.

By using a system like ML's, that problem is averted.

Additionally, by hosting the module system on top of records, which are regular values, you get the further benefit that structures can be parameterized at run-time just like any other value.

You can also write functions that take and return structures (as _functors_ do in ML), and you can generate structures on the fly.

> Also, Lux now offers a mechanism for easy polymorphism, just like Haskell's type-classes, but built upon it's module system, thanks to the `lux/type/auto` module and its `:::` macro.

> You can learn more about that by reading the book and the documentation.

### Functional programming

While the means to do Java-interop are provided, Lux is commited to functional programming.

Functions are curried and partial application is as simple as just applying a function to less arguments than it needs (as in Haskell).

e.g.

	(map (i.+ 1) (list 1 2 3 4 5))

### Macros

Unlike in most other lisps, Lux macros are monadic.

The **(Lux a)** type is the one responsible for the magic by threading **Compiler** instances through macros.

You can use **macro:** to define these monadic macros.

Alternatively, you can use the **syntax:** macro, which also offers monadic parsing of Code tokens for convenience.

### Custom pattern-matching

##### Wait... wut?

Custom pattern-matching basically means that you can use macros to provide custom syntax and features on top of the pattern-matching macro `case`.

For instance, the **list** and **list&** macros are used to build lists.
But you can also use them to destructure lists inside pattern-matching:

	(case (: (List Int) (list 1 2 3))
	  (#Cons x (#Cons y (#Cons z #Nil)))
	  (#Some ($_ i.* x y z))

	  _
	  #None)

	(case (: (List Int) (list 1 2 3))
	  (^ (list x y z))
	  (#Some ($_ i.* x y z))

	  _
	  #None)

There is also the special **^or** macro, which introduces *or patterns*:

	(type: Weekday
	  #Monday
	  #Tuesday
	  #Wednesday
	  #Thursday
	  #Friday
	  #Saturday
	  #Sunday))

	(def: (weekend? day)
	  (-> Weekday Bool)
	  (case day
	    (^or #Saturday #Sunday)
	    true

	    _
	    false))

> Please note: ^ and ^or are just macros like any other and anyone can implement them.

### Is there a community for this?

Come join the forum at: http://luxlang.freeforums.net/

If you want to communicate with me directly, just email me at luxlisp@gmail.com

### How can I edit Lux code?

Check out the Emacs plugin for it: https://github.com/LuxLang/lux/tree/master/lux-mode

### Where do I learn Lux?

The main resource is the book: https://www.gitbook.com/book/luxlang/the-lux-programming-language/details

It will always be up-to-date with the latest stable version of the language.

Also, you can check out the documentation for the currently available modules: https://luxlang.github.io/lux/

### How can I contribute?

For starters, you can check out the Trello board for Lux development: https://trello.com/b/VRQhvXjs/lux-jvm-compiler

I'll be putting there tasks that people can contribute to; both in the compiler and outside (like plugins for editors).

Writing libraries in Lux will also help a lot in making this a more practical language for day to day use.

##### Copyright (c) 2014-2018 Eduardo Julian. All rights reserved.
