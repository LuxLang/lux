# Appendix A: Import syntax

You've already seen some import syntax, but now you'll see all the options available.

If you recall [Chapter 1](chapter_1.md), there was this example code:

```clojure
(.using
 [library
  [lux (.except)
   [program (.only program)]
   ["[0]" debug]
   [control
    ["[0]" io]]]])
```

Here, we're importing the `library/lux` module.

The `(.except)` option means _locally_ import every definition exported by the `library/lux` module.

This allows usage of those definitions without having to give them the `library/lux.` prefix, or even the `.` shortcut prefix.

This may cause some issues if you import 2 definitions with the same name from different modules; or if you get a definition from one module, but then write your own definition with the same name in your code.

In those circumstances, the compiler will complain, saying that you can't re-define `X`; where `X` is the name of the definition.

Then, we import `library/lux/program`, but we **only** import locally the `program` definition.
That is what the `.only` option allows.

	There is also a `.only` option which means locally import everything **except** the specified definitions_.
	You could use it like this: `[your_module (.except foo bar baz)]`

Finally, we import both the `library/lux/debug` and `library/lux/control/io` modules.

In neither case do we import any of their definitions locally.

We also give both of those modules local aliases.

That is what that `"[0]"` syntax does.

The `.using` macro recognizes that syntax for aliases and replaces the `[0]` with the import name directly to the right.

That means:
* `"[0]"` + `debug` = `debug`
* `"[0]"` + `io` = `io`

This might not seem like a big deal, but the aliasing syntax allows you to give imports arbitrary names, so long as you take into account the substitutions that may happen.

So, for example:
* `"my_[0]"` + `debug` = `my_debug`
* `"[0]_[0]"` + `io` = `io_io`

It is also important to note that while imports can be nested for convenience, they don't have to be.

The `.using` declaration could just as easily been written like this:

```clojure
(.using
 [library/lux (.except)]
 [library/lux/program (.only program)]
 ["debug" library/lux/debug]
 ["io" library/lux/control/io])
```

You might also guess that `library` was not imported as a module because it was neither given an alias, not had any definitions specified as local imports.

Any module-path fragments included in the import syntax without such options will not be imported and will simply be assumed to be part of the module-paths of the sub-modules specified under them.

---

It is also possible to have the `.using` macro open interface implementations for you when importing the modules that contain them.

For example:

```clojure
(.using
 [library
  [lux (.except)
   [data
    [collection
     ["[0]" list (.use "[1]::[0]" functor monoid)]]]]])
```

The import above would locally import:
* `list::each`, from `functor`.
* `list::identity`, from `monoid`.
* `list::composite`, from `monoid`.

Here, we can also see some additional syntax for aliasing.

First of all, when opening implementations, aliasing syntax is used to determine the names of the local implementation imports.

The `[0]` is replaced with the name of the implementation member.

The `[1]` is bound to the name of the _context_ of the import.

In this case, the implementations are coming from the `library/lux/data/collection/list` module, so that is the context.

And since that module has been imported with the local alias `list`, that is the name that replaces the `[1]` in the aliasing syntax for the implementation imports.

And that is how we end up with the list of names I enumerated above.

The `[1]` syntax for aliasing can also be used between modules, and not just when importing implementation members.

For example:

```clojure
(.using
 [library
  [lux (.except)
   [data
    ["[0]" collection (.only)
     ["[1]/[0]" list (.use "[1]::[0]" functor monoid)]]]]])
```

Would locally import:
* `collection/list::each`, from `functor`.
* `collection/list::identity`, from `monoid`.
* `collection/list::composite`, from `monoid`.

The context between module imports corresponds to the closest ancestor path which has itself been aliased.

Non-aliased paths don't count as context.

This means:

```clojure
(.using
 [library
  [lux (.except)
   ["[0]" data
    [collection
     ["[1]/[0]" list (.use "[1]::[0]" functor monoid)]]]]])
```

Would locally import:
* `data/list::each`, from `functor`.
* `data/list::identity`, from `monoid`.
* `data/list::composite`, from `monoid`.

	Also, that `"_"`/`"ignore"` syntax you may have noticed means _do not import this module; just give it an alias I can refer to later as a context_.

I should also note that you can **both** locally import definitions and open implementations as parts of the same module import.

For example:

```clojure
(.using
 [library
  [lux (.except)
   [data
    [collection
     ["[0]" list (.only repeated size) (.use "[1]::[0]" monad)]]]]])
```

---

Another important feature of module imports is relative addressing, which comes in 2 flavors.

For the first one, suppose you have the following directory structure:

```clojure
program
	foo
		bar
		baz
			quux
test
	foo
		bar
		baz
			quux
```

And you're writing code in the `program/foo/baz` module.

You can import other modules in the hierarchy like this:

```clojure
... In program/foo/baz
(.using
 [library
  [lux (.except)]]
 ["[0]" /quux] ... program/foo/baz/quux, aliased as /quux
 ["[0]" //bar] ... program/foo/bar, aliased as //bar
 ["[0]" ///] ... program, aliased as ///
  )
```

A single forward slash (`/`) signifies _"this module"_ in the hierarchy, so anything after the forward slash is assumed to be under _this module_.

Two forward slashes (`//`) signify _"the module above"_, and any forward slash after that allows you to go further **up** the hierarchy.

In the case of `program`, it's enough to just specify three forward slashes (`///`) for the `.using` macro to know which module you're referring to.

You can think about it like this:

* `program/foo/baz` + `/` + `quux` = `program/foo/baz/quux`
* `program/foo/baz` + `//` + `bar` =  `program/foo` + `/` + `bar` = `program/foo/bar`
* `program/foo/baz` + `///` =  `program/foo` + `//` = `program` + `/` = `program`

Also, this relative path syntax can be nested, like so:

```clojure
... In program/foo/baz
(.using
 [library
  [lux (.except)]]
 [/
  ["[0]" quux]] ... program/foo/baz/quux, aliased as quux
 [//
  ["[0]" bar] ... program/foo/bar, aliased as bar
  ]
 ["[0]" ///] ... program, aliased as ///
 )
```

Or even:

```clojure
... In program/foo/baz
(.using
 [library
  [lux (.except)]]
 [/
  ["[0]" quux] ... program/foo/baz/quux, aliased as quux
  [//
   ["[0]" bar] ... program/foo/bar, aliased as bar
   ["program" //] ... program, aliased as program
   ]])
```

You may have noticed that when importing `program`, we went from `///` to `//`.

That is because, since it's nested under another `//`, it's relative to `program/foo` instead of `program/foo/baz`, so only 1 step up is necessary instead of the 2 steps a `///` would provide.

---

For the second way to do relative imports, you can see this example:

```clojure
... In program/foo/baz
(.using
 [library
  [lux (.except)]]
 [\\test
  ["[0]" /] ... test/foo/baz, aliased as /
  ]
 ... Alternatively
 ["[0]" \\test] ... test/foo/baz, aliased as \\test
 ... Or
 [\\
  [\test
   ["[0]" /] ... test/foo/baz, aliased as /
   ]]
 )
```

The backslash (`\`) works in the reverse direction to the forward slash (`/`).

If the forward slash allows you append paths to the **back**, and to move **up** the hierarchy from the end; then the backslash allows you to append paths to the **front**, and the move **down** the hierarchy from the beginning.

Why would you want such a thing?

Because it allows you to easily establish parallel hierarchies of modules, which is a useful way to separate orthogonal aspects of your program (like the `program` and `test` hierarchies in our example).

Then, by using this relative syntax, you can refer to one hierarchy from another in an easy way.

