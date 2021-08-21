# Appendix A: Import syntax

You've already seen some import syntax, but now you'll see all the options available.

If you recall [Chapter 1](chapter_1.md), there was this example code:

```
(.module:
  [library
   [lux #*
    [program (#+ program:)]
    ["." debug]
    [control
     ["." io]]]])
```

Here, we're importing the `library/lux` module.
The `#*`/`#all` option means _locally import every definition exported by the `library/lux` module_.

This allows usage of those definitions without having to give them the `library/lux.` prefix, or even the `.` shortcut prefix.

This may cause some issues if you import 2 definitions with the same name from different modules; or if you get a definition from one module, but then write your own definition with the same name in your code.
In those circumstances, the compiler will complain, saying that you can't re-define `X`; where `X` is the name of the definition.

Then, we import `library/lux/program`, but we **only** import locally the `program:` definition.
That is what the `#+`/`#only` option allows.

	There is also a `#-`/`#exclude` option which means locally import everything **except** the specified definitions_.
	You could use it like this: `[program (#- foo bar baz)]`

Finally, we import both the `library/lux/debug` and `library/lux/control/io` modules.
In neither case do we import any of their definitions locally.
We also give both of those modules local aliases.
That is what that `"."` syntax does.
The `.module:` macro recognizes that syntax for aliases and replaces the dot/period with the import name directly to the right.
That means:
* `"."` + `debug` = `debug`
* `"."` + `io` = `io`

This might not seem like a big deal, but the aliasing syntax allows you to give imports arbitrary names, so long as you take into account the substitutions that may happen.

So, for example:
* `"my_."` + `debug` = `my_debug`
* `"._."` + `io` = `io_io`

It is also important to note that while imports can be nested for convenience, they don't have to be.

The `.module:` declaration could just as easily been written like this:

```
(.module:
  [library/lux #*]
  [library/lux/program (#+ program:)]
  ["debug" library/lux/debug]
  ["io" library/lux/control/io])
```

You might also guess that `library` was not imported as a module because it was neither given an alias, not had any definitions specified as local imports.
Any module-path fragments included in the import syntax without such options will not be imported and will simply be assumed to be part of the module-paths of the sub-modules specified under them.

---

It is also possible to have the `.module:` macro open interface implementations for you when importing the modules that contain them.
For example:

```
(.module:
  [library
   [lux #*
    [data
     [collection
      ["." list ("#::." functor monoid)]]]]])
```

The import above would locally import:
* `list::map`, from `functor`.
* `list::identity`, from `monoid`.
* `list::compose`, from `monoid`.

Here, we can also see some additional syntax for aliasing.
First of all, when opening implementations, aliasing syntax is used to determine the names of the local implementation imports.
The `.` is replaced with the name of the implementation member.
The `#` is bound to the name of the _context_ of the import.
In this case, the implementations are coming from the `library/lux/data/collection/list` module, so that is the context.
And since that module has been imported with the local alias `list`, that is the name that replaces the `#` in the aliasing syntax for the implementation imports.
And that is how we end up with the list of names I enumerated above.

The `#` syntax for aliasing can also be used between modules, and not just when importing implementation members.

For example:

```
(.module:
  [library
   [lux #*
    [data
     ["." collection #_
      ["#/." list ("#::." functor monoid)]]]]])
```

Would locally import:
* `collection/list::map`, from `functor`.
* `collection/list::identity`, from `monoid`.
* `collection/list::compose`, from `monoid`.

The context between module imports corresponds to the closest ancestor path which has itself been aliased.
Non-aliased paths don't count as context.

This means:

```
(.module:
  [library
   [lux #*
    ["." data #_
     [collection
      ["#/." list ("#::." functor monoid)]]]]])
```

Would locally import:
* `data/list::map`, from `functor`.
* `data/list::identity`, from `monoid`.
* `data/list::compose`, from `monoid`.

	Also, that `#_`/`#ignore` syntax you may have noticed means _do not import this module; just give it an alias I can refer to later as a context_.

I should also note that you can **both** locally import definitions and open implementations as parts of the same module import.

For example:

```
(.module:
  [library
   [lux #*
    [data
     [collection
      ["." list (#+ repeated size) ("#::." monad)]]]]])
```

---

Another important feature of module imports is relative addressing, which comes in 2 flavors.

For the first one, suppose you have the following directory structure:

```
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

```
... In program/foo/baz
(.module:
  [library
   [lux #*]]
  ["." /quux] ... program/foo/baz/quux, aliased as /quux
  ["." //bar] ... program/foo/bar, aliased as //bar
  ["." ///] ... program, aliased as ///
  )
```

A single forward slash (`/`) signifies _"this module"_ in the hierarchy, so anything after the forward slash is assumed to be under _this module_.
Two forward slashes (`//`) signify _"the module above"_, and any forward slash after that allows you to go further up the hierarchy.
In the case of `program`, it's enough to just specify three forward slashes (`///`) for the `.module:` macro to know which module you're referring to.

You can think about it like this:

* `program/foo/baz` + `/` + `quux` = `program/foo/baz/quux`
* `program/foo/baz` + `//` + `bar` =  `program/foo` + `/` + `bar` = `program/foo/bar`
* `program/foo/baz` + `///` =  `program/foo` + `//` = `program` + `/` = `program`

Also, this relative path syntax can be nested, like so:

```
... In program/foo/baz
(.module:
  [library
   [lux #*]]
  [/
   ["." quux]] ... program/foo/baz/quux, aliased as quux
  [//
   ["." bar] ... program/foo/bar, aliased as bar
   ]
  ["." ///] ... program, aliased as ///
  )
```

Or even:

```
... In program/foo/baz
(.module:
  [library
   [lux #*]]
  [/
   ["." quux] ... program/foo/baz/quux, aliased as quux
   [//
    ["." bar] ... program/foo/bar, aliased as bar
    ["program" //] ... program, aliased as program
    ]])
```

You may have noticed that when importing `program`, we went from `///` to `//`.
That is because, since it's nested under another `//`, it's relative to `program/foo` instead of `program/foo/baz`, so only 1 step up is necessary instead of the 2 steps a `///` would provide.

---

For the second way to do relative imports, you can see this example:

```
... In program/foo/baz
(.module:
  [library
   [lux #*]]
  [\\test
   ["." /] ... test/foo/baz, aliased as /
   ]
  ... Alternatively
  ["." \\test] ... test/foo/baz, aliased as \\test
  ... Or
  [\\
   [\test
    ["." /] ... test/foo/baz, aliased as /
    ]]
  )
```

The backslash (`\`) works in the reverse direction to the forward slash (`/`).
If the forward slash allows you append paths to the back, and to move up the hierarchy from the end; then the backslash allows you to append paths to the front, and the move down the hierarchy from the beginning.

Why would you want such a thing?

Because it allows you to easily establish parallel hierarchies of modules, which is a useful way to separate orthogonal aspects of your program (like the `program` and `test` hierarchies in our example).
Then, by using this relative syntax, you can refer to one hierarchy from another in an easy way.

