# Chapter 2: The Basics

_Where you will learn the fundamentals of Lux programming._

---

## Modules

Lux programs are made of modules.

A module is a file containing Lux code, and bearing the extension `.lux` at the end of its name (like our `main.lux` file).

Modules contain a single _module statement_, various definitions and a few other kinds of statements as top-level code (that is, code that is not nested within other code).

## Definitions

Definitions are the top-level or global values that are declared within a module.

They may be of different types, such as constant values or functions, or even fancier things like types, signatures or structures (more on those in later chapters).

Also, definitions may be private to a module, or exported so other modules can refer to them. By default, all definitions are private.

## Values

Values are just entities which carry some sort of information.

Every value has a type, which describes its properties.

Lux supports a variety of basic and composite values:

* **Bit**: `#0` and `#1` values. They take the role of boolean values in other languages, and the `false` and `true` constants mask them for greater familiarity.
* **Nat**: Unsigned 64-bit integers.
* **Int**: Signed 64-bit integers.
* **Rev**: Unsigned 64-bit numbers in the interval [0,1).
* **Frac**: Signed 64-bit floating-point numbers.
* **Text**: Strings.
* **Unit**: A special value that sort-of represents an empty value or a non-value.
* **Variant**: A value of a particular type, from a set of heterogeneous options.
* **Tuple**: An ordered group of heterogeneous values which may be handled as a single entity.
* **Function**: A first-class function or procedure which may be invoked, or passed around like other values.

## Types

Types are descriptions of values that the compiler uses to make sure that programs are correct and invalid operations (such as multiplying two bits) are never performed.

The thing that makes Lux types special is that they are first-class values, the same as bits and ints (albeit, a little more complex).

They are data-structures, and they even have a type... named `Type` (_I know, it's **so** meta_). 

We'll talk more about that in later chapters.

## Macros

Macros are special functions that get invoked at compile time, and that have access to the full state of the compiler.

The reason they run during compilation is that they can perform transformations on code, which is a very useful thing to implement various features, DSLs (domain-specific languages) and optimizations.

We'll also explore macros further in later chapters.

## Comments

```
## They look like this.
## They all start with 2 continuous # characters and go on until the end of the line.
```

## Expressions

An expression is code that may perform calculations in order to generate a value.

Data literals (like int, tuple or function literals) are expressions, but so are function calls, pattern-matching and other complex code which yields values.

Macro calls can also be involved if the macro in question generates code that constitutes an expression.

## Statements

Statements looks similar to expressions, except that their purpose is not to produce a value, but to communicate something to the compiler.

This is a bit of a fuzzy line, since some things which also communicate stuff to the compiler are actually expressions (for example, type annotations, which we'll see in next chapter).

Examples of statements are module statements and definitions of all kinds (such as program definitions).

## Programs

Lux doesn't have special "main" functions/procedures/methods that you define, but the `program:` macro accomplishes the same thing and works similarly.

It takes a list of command-line inputs and must produce some sort of action to be performed as the program's behavior.

That action must be of type `(IO Any)`, which just means it is a synchronous process which produces any value (regardless of type) once it is finished.

## Command-Line Interface

Lux programs can have graphical user interfaces, and in the future they may run in various environments with much different means of interfacing with users, or other programs.

But as a bare minimum, the Lux standard library provides the means to implement command-line interfaces, through the functionality in the `lux/control/parser/cli` module.

That module implements a variety of parsers for implementing rich command-line argument processing, and you should definitely take a look at it once you're ready to write your first serious Lux program.

## Functional Programming

This is the main paradigm behind Lux, and there are a few concepts that stem from it which you should be familiar with:

* **Immutable Values**: The idea is that once you have created a value of any type, it's frozen forever. Any changes you wish to introduce must be done by creating a new value with the differences you want. Think, for instance, of the number 5. If you have 2 variables with the same number, and you decide to change the value in one variable to 8, you wouldn't want the other variable to be affected. Well, the same idea applies to all values. This is clearly a departure from the imperative and object-oriented style of having all data be mutable, but it introduces a level of safety and reliability in functional programs that is missing in the imperative style.
* **First-Class Functions**: This just means that functions are values like any other. In most languages, functions/methods/procedures are more like features you register in the compiler for later use, but that just remain static in the background until you invoke them. In functional programming, you can actually pass functions as arguments to other functions, or return them as well. You can store functions in variables and inside data-structures, and you can even produce new functions on the fly at run-time.
* **Closures**: Functions that get generated at run-time can also "capture" their environment (the set of local variables within the function's reach), and become closures. This is the name for a function which "closes over" its environment, making it capable to access those values long after the function was originally created. This allows you to create functions as templates which get customized at run-time with values from their environment.

Now, let's talk a bit more about the program we saw last time.

In the previous chapter we compiled and ran a Lux program, but nothing has been explained yet. Let's review the code and see in detail what was done.

```
(.module:
  {#.doc "This will be our program's main module."}
  [library
   [lux #*
    [program (#+ program:)]
    ["." debug]
    [control
     ["." io]]]])

(program: args
  (io.io (debug.log! "Hello, world!")))
```

The first part of this program is the module declaration.

All Lux modules automatically import the `library/lux` module, but they don't locally import every single definition, so everything would have to be accessed by using the `lux.` prefix or the `;` (short-cut) prefix.

To avoid that, we import the `library/lux` module in a plain way.

	By the way, what I just explained about the `library/lux` module is the reason why we couldn't just use the module macro as `module:`.

Then we import the `library/lux/control/io` module. We're giving this module an alias, using that `"."` syntax. The way aliasing works here is that it replaces the period/dot with the short name of the import, and so `.` becomes `io`, and that is the alias given to the import. The same process happens when we import the `library/lux/debug` module. This might seems weird and sort of useless, but the aliasing syntax has some more features and flexibility, enabling you to have your own naming convention when importing modules.

Notice how we express nested modules (up to arbitrary depths) by simply nesting in brackets.

Finally, we import the `library/lux/program` module. Notice how the syntax is a bit different in this case. Here, we're saying that we don't want to locally import any definition within it, except `program:`. We use a similar syntax when importing the `library/lux` module, except with an asterisk instead of a plus sign. That just means we want to locally import all the public definitions, instead of just listing the ones we want.

Now, let's analyse the actual code!

We're defining the entry point of our program (what in many other languages is referred to as the _main function/procedure/method_). We'll be receiving all the command-line arguments in a `(List Text)` called `args`, and we must produce a value of type `(IO Any)` (the `IO` type being defined in `library/lux/control/io`, but omitted here for brevity).

	We'll go into more detail about what `IO` and `Any` mean in the next chapter.

Suffice it to say that the `debug.log!` function will produce a value of type `Any` after printing/logging our `"Hello, world!"` text, and the `io.io` macro will wrap that in the `IO` type.

That `(IO Any)` value will then be run by the system at run-time, giving us the result we want.

---

Now that we've discussed some of the basics of what goes on inside of Lux programs, it's time for us to explore the language in a little bit more depth.

See you in [the next chapter](chapter_3.md)!

