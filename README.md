## What is Lux?

Lux is a new programming language in the making.
It's meant to be a functional, statically-typed Lisp that will run on several platforms, such as the Java Virtual Machine and JavaScript interpreters.

### What's the current version?

v0.1

### How far ahead is the project?

The Java-bytecode compiler is close to completion.

Some features are missing and the compiler is not as fast as I would like.

However, some small programs can be written to try out Lux and get a feeling for the language.

### How can I use it?

Download the 0.1 compiler from here: https://github.com/LuxLang/lux/raw/master/lux-jvm-0.1.0-standalone.jar

Right now, the current version of Lux (0.1) is mostly to play around with the language, so it's a bit limited on what you can do.
Once you download the compiler, you'll want to create a directory named "source" in the same directory where the compiler is located.

"source" must contain 2 files.
One will be the Lux prelude (lux.lux), the other will be program.lux
You can write anything you want inside program.lux to play around with the language.

##### Note: You can download the lux.lux & program.lux files in the source/ directory in this repo to get started.

To run the compiler, open your terminal and write this:

	java -jar lux-jvm-0.1.0-standalone.jar

This will generate a directory named "output" and put all the .class files there.
Then, you can package the program and run it using this:

	cd output && jar cvf program.jar * && java -cp "program.jar" program && cd ..

### What's the license?

Eclipse Public License v1.0

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

The module system is heavily inspired by ML, and both signatures & structures are suported.

The main difference between Lux and ML is that ML separates signaturs & structures from the rest of the language, whereas Lux implements them on-top of the base language.

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

	(let [inc (int:+ 1)]
	  (map inc (list 1 2 3 4 5)))

### Code portability

Many languages nowadays support compilation to multiple platforms (e.g. Haskell, Scala, Clojure).
However, sharing code between platforms can be a pain in the neck due to the following reasons:

* differences in features between platforms
* the languages weren't originally designed with the goal of running both native/JVM and in JavaScript

Lux is being designed from the ground up to target multiple platforms and achieve maximum reusability of code with minimum hassle.

The mechanism hasn't been added yet to the language (mainly because there's only 1 compiler at the moment), but it will come pretty soon in one of the upcoming releases.

### Macros

Unlike in most other lisps, Lux macros are monadic.
The **(Lux a)** type is the one responsibly for the magic by treading **CompilerState** instances through macros.
Macros must have the **Macro** and then be declared as macros.

However, just using the **defmacro** macro will take care of it for you.

However, in an upcoming release you'll get another macro for defining macros.
It will be named **defsyntax** and will use monadic parsing of AST tokens to parse the syntax.

If you want to see how macros are implemented, you can take a look at *lux.lux*.

### Custom pattern-matching

##### Wait... wut?

Custom pattern-matching basically means that you can use macros to provide custom syntax & features on top of the pattern-matching macro (case).

For instance, the **list** and **list&** macros are used to build lists.
But you can also use them to destructure them inside pattern-matching:

	(case (: (List Int) (list 1 2 3))
	  (#Cons [x (#Cons [y (#Cons [z #Nil])])])
	  (#Some ($ int:* x y z))

	  _
	  #None)

	(case (: (List Int) (list 1 2 3))
	  (\ (list 1 2 3))
	  (#Some ($ int:* x y z))

	  _
	  #None)

There is also the special **\or** macro, which instroduces *or patterns*

	(deftype Weekday
	  (| #Monday
		 #Tuesday
		 #Wednesday
		 #Thursday
		 #Friday
		 #Saturday
		 #Sunday))

	(def (weekend? day)
	  (-> Weekday Bool)
	  (case day
		(\or #Saturday #Sunday)
		true

		_
		false))

##### Please note: \ and \or are just macros like and other and anyone can implement them.

I'll be adding more useful pattern-matching macros in upcoming releases of the language.

If you want to see how they work, just check out their implementation inside lux.lux

### Is there a community for this?

Lux was just born today.

Come join the budding community by joining the discussion group at: https://groups.google.com/forum/#!forum/lux-programming-language

If you want to communicate with me directly, just email me at luxlisp@gmail.com

### How can I edit Lux code?

Check out the Emacs plugin for it: https://github.com/LuxLang/lux-mode

## What's available?

### Base syntax

Comments

	## This is a single-line comment
	## Multi-line comments are comming soon

Bool (implemented as java.lang.Boolean)

	true
	false

Int (implemented as java.lang.Long)

	1
	20
	12345

Real (implemented as java.lang.Double)

	1.23
	0.5

Char (implemented as java.lang.Character)

	#"a"
	#"\n"

Text (implemented as java.lang.String)

	"yolo"
	"Hello\nWorld!"

Forms

	(+ 1 2)
	(lambda [x] (foo 10 x))

Symbols

	foo     ## Unprefixed symbol (compiler will assume it's in the current module)
	bar;baz ## Prefixed symbol (compiler will assume it's in the module specified by the prefix)
	;fold   ## With just the semi-colon, compiler wil assume it's the same as lux;fold
	;;quux  ## With 2 semi-colons, it will get automatically prefixed with the current-module

Tags

	#Nil
	#lux;Cons
	#;Some
	#;;MyTag

Tuples

	[]
	["yolo" 10 true]

Variants (aka sum-types, aka discriminated unions)

	#Nil
	(#Cons [10 #Nil])

Records

	{#name "Eduardo" #alive? true}

### Types
	(deftype Bool (^ java.lang.Boolean))

	(deftype Int (^ java.lang.Long))

	(deftype Real (^ java.lang.Double))

	(deftype Char (^ java.lang.Character))

	(deftype Text (^ java.lang.String))

	(deftype Void (|))

	(deftype Ident (, Text Text))

	(deftype (List a)
	  (| #Nil
		 (#Cons [a (List a)])))

	(deftype (Maybe a)
	  (| #None
		 (#Some a)))

	(deftype Type ...)

	(deftype (Meta m d)
	  (| (#Meta [m d])))

	(deftype Syntax ...)

	(deftype (Either l r)
	  (| (#Left l)
		 (#Right r)))

	(deftype Reader ...)

	(deftype LuxVar ...)

	(deftype CompilerState ...)

	(deftype (Lux a)
	  (-> CompilerState (Either Text (, CompilerState a))))

	(deftype Macro
	  (-> (List Syntax) (Lux (List Syntax))))

	(deftype (IO a)
	  (-> (,) a))

### Macros
###### defmacro
e.g.

	(defmacro #export (and tokens)
	  (case (reverse tokens)
		(\ (list& last init))
		(return (: (List Syntax)
		           (list (fold (: (-> Syntax Syntax Syntax)
		                          (lambda [post pre]
		                            (` (if (~ pre)
		                                 true
		                                 (~ post)))))
		                       last
		                       init))))
		
		_
		(fail "and requires >=1 clauses.")))

###### comment
e.g.

	(comment 1 2 3 4) ## Same as not writing anything...

###### list
e.g.

	(list 1 2 3)
	=> (#Cons [1 (#Cons [2 (#Cons [3 #Nil])])])

###### list&
e.g.

	(list& 0 (list 1 2 3))
	=> (#Cons [0 (list 1 2 3)])

###### lambda
e.g.

	(def const
	  (lambda [x y] x))

	(def const
	  (lambda const [x y] x))

###### let
e.g.

	(let [x (foo bar)
		  y (baz quux)]
	  ...)

###### $
e.g.

	## Text/string concatenation
	($ text:++ "Hello, " name ".\nHow are you?")
	=> (text:++ "Hello, " (text:++ name ".\nHow are you?"))

###### |>
e.g.

	## Piping macro
	(|> elems (map ->text) (interpose " ") (fold text:++ ""))
	=>
	(fold text:++ ""
		  (interpose " "
		             (map ->text elems)))

###### if
e.g.

	(if true
	  "Oh, yeah"
	  "Aw hell naw!")

###### ^
e.g.

	## Macro to treat classes as types
	(^ java.lang.Object)

###### ,
e.g.

	## Tuples
	(, Text Int Bool)
	
	(,) ## The empty tuple, aka "unit"

###### |
e.g.

	(| #Yes #No)
	
	(,) ## The empty variant, aka "void"

###### &
e.g.

	## Records
	(& #name Text
	   #age Int
	   #alive? Bool)

###### ->
e.g.
	
	## Function types
	(-> Int Int Int) ## This is the type of a function that takes 2 Ints and returns an Int

###### All
e.g.

	## Universal quantification.
	(All List [a]
		 (| #Nil
		    (#Cons [a (List a)])))

	## It must be explicit, unlike in Haskell. Rank-n types will be possible as well as existential types
	(All [a]
	  (-> a a))

###### type`

	## This macro is not meant to be used directly. It's used by :, :!, deftype, struct, sig

###### io

	## Just makes sure whatever computation you do returns an IO type. It's here mostly for host-interop.

###### :
e.g.

	## The type-annotation macro
	(: (List Int) (list 1 2 3))

###### :!
e.g.

	## The type-coercion macro
	(:! Dinosaur (list 1 2 3))

###### deftype
e.g.

	## The type-definition macro
	(deftype (List a)
	  (| #Nil
		 (#Cons [a (List a)])))

###### exec
e.g.

	## Sequential execution of expressions (great for side-effects).
	## But please use the io macro to help keep the purity.
	(exec
	  (println "#1")
	  (println "#2")
	  (println "#3")
	  "YOLO")

###### def
e.g.

	## Macro for definining global constants/functions.
	(def (rejoin-pair pair)
	  (-> (, Syntax Syntax) (List Syntax))
	  (let [[left right] pair]
		(list left right)))

###### case
e.g.

	## The pattern-matching macro.
	## Allows the usage of macros within the patterns to provide custom syntax.
	(case (: (List Int) (list 1 2 3))
	  (#Cons [x (#Cons [y (#Cons [z #Nil])])])
	  (#Some ($ int:* x y z))

	  _
	  #None)

	(case (: (List Int) (list 1 2 3))
	  (\ (list 1 2 3))
	  (#Some ($ int:* x y z))

	  _
	  #None)

	(deftype Weekday
	  (| #Monday
		 #Tuesday
		 #Wednesday
		 #Thursday
		 #Friday
		 #Saturday
		 #Sunday))

	(def (weekend? day)
	  (-> Weekday Bool)
	  (case day
		(\or #Saturday #Sunday)
		true

		_
		false))

###### \

	## It's a special macro meant to be used with case

###### \or

	## It's a special macro meant to be used with case

###### `

	## Quasi-quotation as a macro. Unquote (~) and unquote-splice (~@) must also be used as forms
	e.g.
	(` (def (~ name)
		 (lambda [(~@ args)]
		   (~ body))))

###### sig

	## Not mean to be used directly. Prefer defsig

###### struct

	## Not mean to be used directly. Prefer defstruct

###### defsig
e.g.

	## Definition of signatures ala ML
	(defsig #export (Ord a)
	  (: (-> a a Bool)
		 <)
	  (: (-> a a Bool)
		 <=)
	  (: (-> a a Bool)
		 >)
	  (: (-> a a Bool)
		 >=))

###### defstruct
e.g.

	## Definition of structures ala ML
	(defstruct #export Int:Ord (Ord Int)
	  (def (< x y)
		(jvm-llt x y))
	  (def (<= x y)
		(or (jvm-llt x y)
		    (jvm-leq x y)))
	  (def (> x y)
		(jvm-lgt x y))
	  (def (>= x y)
		(or (jvm-lgt x y)
		    (jvm-leq x y))))

###### and
e.g.

	(and true false true) ## => false

###### or
e.g.

	(or true false true) ## => true

###### alias-lux

	## Just creates local aliases of everything defined & exported in lux.lux
e.g.

	(;alias-lux)

###### using
e.g.

	## The Lux equivalent to ML's open.
	## Opens up a structure and provides all the definitions as local variables.
	(using Int:Ord
	  (< 5 10))

### Functions
###### fold

	(All [a b]
	  (-> (-> a b a) a (List b) a))

###### reverse

	(All [a]
	  (-> (List a) (List a)))

###### map

	(All [a b]
	  (-> (-> a b) (List a) (List b)))

###### any?

	(All [a]
	  (-> (-> a Bool) (List a) Bool))

.

	## Function composition: (. f g) => (lambda [x] (f (g x)))
	(All [a b c]
	  (-> (-> b c) (-> a b) (-> a c)))

###### int:+

	(-> Int Int Int)

###### int:-

	(-> Int Int Int)

###### int:*

	(-> Int Int Int)

###### int:/

	(-> Int Int Int)

###### int:%

	(-> Int Int Int)

###### int:=

	(-> Int Int Bool)

###### int:>

	(-> Int Int Bool)

###### int:<

	(-> Int Int Bool)

###### real:+

	(-> Real Real Real)

###### real:-

	(-> Real Real Real)

###### real:*

	(-> Real Real Real)

###### real:/

	(-> Real Real Real)

###### real:%

	(-> Real Real Real)

###### real:=

	(-> Real Real Bool)

###### real:>

	(-> Real Real Bool)

###### real:<

	(-> Real Real Bool)

###### length

	## List length
	(All [a]
	  (-> (List a) Int))

###### not

	(-> Bool Bool)

###### text:++

	## Text/string concatenation
	(-> Text Text Text)

###### get-module-name

	## Obtain the name of the currently-compiling module while in a macro.
	(Lux Text)

###### find-macro

	## Given the name of a macro, try to obtain it.
	(-> Ident (Lux (Maybe Macro)))

###### normalize

	## Normalizes a name so if it lacks a module prefix, it gets the one of the current module.
	(-> Ident (Lux Ident))

###### ->text

	(-> (^ java.lang.Object) Text)

###### interpose

	(All [a]
	  (-> a (List a) (List a)))

###### syntax:show

	## Turn Lux synta into user-readable text. (Note: it's not pretty-printed)
	(-> Syntax Text)

###### macro-expand

	## The standard macro-expand function.
	(-> Syntax (Lux (List Syntax)))

###### gensym

	## Can't forget gensym!
	(-> Text (Lux Syntax))

###### macro-expand-1

	(-> Syntax (Lux Syntax))

###### id

	(All [a] (-> a a))

###### print

	## Neither print or println return IO right now because I've yet to implement monads & do-notation
	## Note: The implementation inside lux.lux is not meant for public consumption.
	## Note: You'll get an implementation of monads, functors et al. soon enough...
	(-> Text (,))

###### println

	(-> Text (,))

###### some

	(All [a b]
	  (-> (-> a (Maybe b)) (List a) (Maybe b)))

### Signatures
###### Eq

	(defsig #export (Eq a)
	  (: (-> a a Bool)
		 =))

###### Show

	(defsig #export (Show a)
	  (: (-> a Text)
		 show))

###### Ord

	(defsig #export (Ord a)
	  (: (-> a a Bool)
		 <)
	  (: (-> a a Bool)
		 <=)
	  (: (-> a a Bool)
		 >)
	  (: (-> a a Bool)
		 >=))

### Structures
###### Int:Eq
###### Real:Eq

###### Bool:Show
###### Int:Show
###### Real:Show
###### Char:Show

###### Int:Ord
###### Real:Ord

## Caveats

### Errors
The compiler is not fully stable so you might get an error if you do anything funny.

Also, the error messages could really use an overhaul, so any error message you get will probably startle you.

Don't worry about it, version 0.2 will improve error reporting a lot.
If you have any doubts, feel free to ask/complain in the Google Group.

### Tags

Tags that are unprefixed will just assume the prefix of the current module you're in.

If you want to write variants/records with tags from lux.lux, you must do 1 of the following 2 alternatives:

* Fully prefix them: #lux;Cons
* Use the ; short-cut: #;Cons

##### Copyright (c) 2015 Eduardo Julian. All rights reserved.

