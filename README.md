[![Join the chat at https://gitter.im/LuxLang/lux](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/LuxLang/lux?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## What is Lux?

Lux is a new programming language in the making.
It's meant to be a functional, statically-typed Lisp that will run on several platforms, such as the Java Virtual Machine and JavaScript interpreters.

### What's the current version?

0.4.0

### How far ahead is the project?

Lux is finally in the **beta** stage. The JVM compiler is pretty stable and the standard library has grown to a respectable size.

### How can I use it?

You should use the Leiningen plugin for Lux to compile your programs and manager your dependencies.
You can find it here: https://github.com/LuxLang/lux-lein

After compiling your program, this will generate a directory named "target" and put all the .class files there.
Then, you can run the program like this:

	java -jar target/jvm/program.jar

### Sample

To take a look at a sample Lux project, please take a look at this repository: https://github.com/LuxLang/luxdoc

The program in there was actually used to generate most of the documentation for the standard library in the wiki (located over here: https://github.com/LuxLang/lux/wiki/Standard-Library)

### What's the license?

Mozilla Public License v2.0

## What's interesting about the language?

### Inspirations

The language is mostly inspired by the following 3 languages:

* Haskell (functional programming)
* Clojure (syntax, overall look & feel)
* ML (module system)

The compiler is even implemented in Clojure.

### Types

They are implemented as plain-old data-structures whose expressions get eval'ed by the compiler and integrated into the type-checker.
That means it's actually possible to generate types via functions.
However, most of the types in the prelude are generated via several macros that provide a more pleasant syntax to types.

If you wonder what types look like without makeup, feel free to read the first few hundred lines of lux.lux.

### Module system

The module system is heavily inspired by ML, and both signatures & structures are supported.

The main difference between Lux and ML is that ML separates signatures & structures from the rest of the language, whereas Lux implements them on-top of the base language.

How?
By implementing signatures as record-types and structures as actual records.

##### But, why not just use type-classes?

Haskell's type-class system forces the user to only specify 1 instance for any given type-class and it's argument.
This means that if there are more than 1 possible valid instances (as is the case for Monoid of Int), you have to resort to newtype hacks to be able to provide alternative implementations.

By using a system like ML's, that problem is averted.
Also, by hosting the module system on top of records, which are regular values, you get the further benefit that structures can be parameterized at runtime just like any other data-structures.
You can also write functions that take and return structures (as Functors do in ML) and you can generate structures on the fly.

### Functional programming

While the means to do Java-interop will be provided (and there are already a few ways to do it that you can look-up inside lux.lux), Lux is commited to functional programming.

Functions are curried and partial application is as simple as just applying a function to less arguments than it needs (as in Haskell).

e.g.

	(map (+ 1) (list 1 2 3 4 5))

### Code portability

Many languages nowadays support compilation to multiple platforms (e.g. Haskell, Scala, Clojure).
However, sharing code between platforms can be a pain in the neck due to the following reasons:

* differences in features between platforms
* the languages weren't originally designed with the goal of running both native/JVM and in JavaScript

Lux is being designed from the ground-up to target multiple platforms and achieve maximum reusability of code with minimum hassle.

The mechanism hasn't been added yet to the language (mainly because there's only 1 compiler at the moment), but it will come pretty soon in one of the upcoming releases.

### Macros

Unlike in most other lisps, Lux macros are monadic.
The **(Lux a)** type is the one responsibly for the magic by threading **Compiler** instances through macros.
Macros must have the **Macro** type and then be declared as macros.

However, just using the **defmacro** macro will take care of it for you.
Alternatively, you can use the **defsyntax** macro, which also offers monadic parsing of AST tokens for convenience.

### Custom pattern-matching

##### Wait... wut?

Custom pattern-matching basically means that you can use macros to provide custom syntax & features on top of the pattern-matching macro (case).

For instance, the **list** and **list&** macros are used to build lists.
But you can also use them to destructure them inside pattern-matching:

	(case (: (List Int) (list 1 2 3))
	  (#Cons x (#Cons y (#Cons z #Nil)))
	  (#Some ($_ * x y z))

	  _
	  #None)

	(case (: (List Int) (list 1 2 3))
	  (\ (list x y z))
	  (#Some ($_ * x y z))

	  _
	  #None)

There is also the special **\or** macro, which introduces *or patterns*:

	(type: Weekday
	  (| #Monday
		 #Tuesday
		 #Wednesday
		 #Thursday
		 #Friday
		 #Saturday
		 #Sunday))

	(def: (weekend? day)
	  (-> Weekday Bool)
	  (case day
		(\or #Saturday #Sunday)
		true

		_
		false))

##### Please note: \ and \or are just macros like any other and anyone can implement them.

I'll be adding more useful pattern-matching macros in upcoming releases of the language.

If you want to see how they work, just check out their implementation inside lux.lux

### Is there a community for this?

Come join the budding community by joining the discussion group at: https://groups.google.com/forum/#!forum/lux-programming-language

If you want to communicate with me directly, just email me at luxlisp@gmail.com

### How can I edit Lux code?

Check out the Emacs plugin for it: https://github.com/LuxLang/lux-mode

### Where do I learn Lux?

Head to the wiki and check out the documentation for the currently available modules.

You should also check out the Lux DevLog, where I talk about different topics related to Lux and post tutorials: http://luxlang.blogspot.com/

### How can I contribute?

For starters, you can check out the Trello board for Lux development: https://trello.com/b/VRQhvXjs/lux-jvm-compiler

I'll be putting there tasks that people can contribute to, both in the compiler and outside (like plugins for editors).

Writing libraries in Lux will also help a lot in making this a more practical language for day to day use.

Communication is done over Gitter and the Google group.

##### Copyright (c) 2015-2016 Eduardo Julian. All rights reserved.

