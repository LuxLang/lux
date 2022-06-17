# Chapter 11: Syntax macros

_Where science turns into magic once more._

---

You've now learned how to create your own macros to make your own custom syntax, and the features involved.

I would advice you to take a look at the many macros in the Lux Standard Library for inspiration as to what can be accomplished.

In the meantime, let's find out how to take our macro chops to the next level.

---

The `library/lux/control/parser/code` module houses some powerful tools.

For starters, it's the home of the (_code_) `Parser` type:

```clojure
(type: .public Parser
  (//.Parser (List Code)))
```

Which is based on the _generic_ `Parser` type from the `library/lux/control/parser` module:

```clojure
(type: .public (Parser s a)
  (-> s (Try [s a])))
```

	**Note**: This is also a functorial/monadic type.

`Parser` (_from `library/lux/control/parser/code`_) is the type of code-parsers: parsers which analyze `Code` nodes to extract arbitrary information.

The `Parser` type works with streams of inputs instead of single elements, and it often consumes some of those inputs, which is why the output involves an updated list of `Code`s.

There are many such code-parsers (and combinators) in the `library/lux/control/parser/code` module, and you should definitely take a look at what's available in the documentation.

Then, in the `library/lux/macro/syntax` module, there is a mechanism for defining macros: the `syntax:` macro.

```clojure
... A more advanced way to define macros than 'macro:'.
... The inputs to the macro can be parsed in complex ways through the use of syntax parsers.
... The macro body is also (implicitly) run in the Meta monad, to save some typing.
... Also, the compiler state can be accessed through the *lux* binding.
(syntax: .public (object [.let [imports (class_imports *lux*)
                                class_vars (list)]
                          super (opt (super_class_decl^ imports class_vars))
                          interfaces (tuple (some (super_class_decl^ imports class_vars)))
                          constructor_args (constructor_args^ imports class_vars)
                          methods (some (overriden_method_def^ imports))])
  (let [def_code ($_ text#composite "anon-class:"
                     (spaced (list (super_class_decl$ (maybe.else object_super_class super))
                                   (with_brackets (spaced (list#each super_class_decl$ interfaces)))
                                   (with_brackets (spaced (list#each constructor_arg$ constructor_args)))
                                   (with_brackets (spaced (list#each (method_def$ id) methods))))))]
    (in (list (` ((~ (code.text def_code))))))))
```

	This example is a macro for making anonymous _JVM_ classes that lives in `lux/ffi`.

The difference between `macro:` and `syntax:` is that `syntax:` allows you to parse, in a structured manner, the inputs to your macro, thereby reducing considerably the complexity necessary for making _big_ macros.

Also, because you're using code-parsers for the hard work, you can write reusable parsers that you can share throughout your macros, if you want to have common syntax.

You can even compose your parsers, or use parsers from someone else's library.

	There are already some small modules under `library/lux/macro/syntax/` which house some reusable code-parsers and code-generators.

Additionally, `syntax:` binds the `Lux` value on a variable called `*lux*`, so you can use it during your parsing.

What do those code-parsers look like?

Here is an example:

```clojure
... Taken from library/lux/math/infix.

(.require
  [library
   [lux "*"
    [abstract
     [monad {"+" do}]]
    [control
     ["<>" parser ("[1]#[0]" functor)
      ["<[0]>" code {"+" Parser}]]]
    [data
     ["[0]" product]
     [collection
      ["[0]" list ("[1]#[0]" mix)]]]
    [macro
     [syntax {"+" syntax:}]
     ["[0]" code]]
    [math
     [number
      ["n" nat]
      ["i" int]]]]])

(type: Infix
  (Rec Infix
    (Variant
     {#Const Code}
     {#Call (List Code)}
     {#Unary Code Infix}
     {#Binary Infix Code Infix})))

(def literal
  (Parser Code)
  ($_ <>.either
      (<>#each code.bit <code>.bit)
      (<>#each code.nat <code>.nat)
      (<>#each code.int <code>.int)
      (<>#each code.rev <code>.rev)
      (<>#each code.frac <code>.frac)
      (<>#each code.text <code>.text)
      (<>#each code.symbol <code>.symbol)))

(def expression
  (Parser Infix)
  (<| <>.rec (function (_ expression))
      ($_ <>.or
          ..literal
          (<code>.form (<>.many <code>.any))
          (<code>.tuple (<>.and <code>.any expression))
          (<code>.tuple (do <>.monad
                          [init_subject expression
                           init_op <code>.any
                           init_param expression
                           steps (<>.some (<>.and <code>.any expression))]
                          (in (list#mix (function (_ [op param] [_subject _op _param])
                                          [{#Binary _subject _op _param} op param])
                                        [init_subject init_op init_param]
                                        steps))))
          )))
```

And here are some examples of syntax macros:

```clojure
... Also from library/lux/math/infix.

(def (prefix infix)
  (-> Infix Code)
  (case infix
    {#Const value}
    value
    
    {#Call parts}
    (code.form parts)

    {#Unary op subject}
    (` ((~ op) (~ (prefix subject))))
    
    {#Binary left op right}
    (` ((~ op) (~ (prefix right)) (~ (prefix left))))))

(syntax: .public (infix [expr ..expression])
  (in (list (..prefix expr))))
```

```clojure
(syntax: .public (^stream& [patterns (<code>.form (<>.many <code>.any))
                            body <code>.any
                            branches (<>.some <code>.any)])
  (with_symbols [g!stream]
    (let [body+ (` (let [(~+ (|> patterns
                                 (list#each (function (_ pattern)
                                              (list (` [(~ pattern) (~ g!stream)])
                                                    (` ((~! //.result) (~ g!stream))))))
                                 list#conjoint))]
                     (~ body)))]
      (in (list& g!stream body+ branches)))))
```

```clojure
(syntax: .public (cond> [_ _reversed_
                         prev <code>.any
                         else body^
                         _ _reversed_
                         branches (<>.some (<>.and body^ body^))])
  (with_symbols [g!temp]
    (in (list (` (let [(~ g!temp) (~ prev)]
                   (cond (~+ (do list.monad
                               [[test then] branches]
                               (list (` (|> (~ g!temp) (~+ test)))
                                     (` (|> (~ g!temp) (~+ then))))))
                         (|> (~ g!temp) (~+ else)))))))))
```

	By the way, the body of `syntax:` runs inside a `(do library/lux/meta.monad [] ...)` expression, so you have immediate access to `Monad`'s `in` method for simple macros, like the last one.

---

This may be a short chapter, but not because its subject is small.

The opportunities that code-parsers open are fantastic, as it puts within your reach macros which would otherwise be much harder to implement correctly.

Don't worry about complex inputs: your macros can implement entire new embedded programming languages if you want them to.

Code-parsers can generate any data-type you want, so you can easily translate the information in the input syntax to whatever data-model you need.

But, now that we've spent 3 chapters about metaprogramming in Lux, I think it's fair that we clear our minds a little by looking at other subjects.

You're going to learn how to go beyond Lux and interact with everything and everyone.

See you in [the next chapter](chapter_12.md)!

