# Chapter 11: Syntax Macros

_Where science turns into magic once more._

---

You've now learned how to create your own macros to make your own custom syntax, and the features involved.

I would advice you to take a look at the many macros in the Lux Standard Library for inspiration as to what can be accomplished.

In the meantime, let's find out how to take our macro chops to the next level.

---

The `library/lux/control/parser/code` module houses some powerful tools.

For starters, it's the home of the (_code_) `Parser` type:

```
(type: .public Parser
  {#.doc "A Lux code parser."}
  (//.Parser (List Code)))
```

Which is based on the `Parser` type from the `library/lux/control/parser` module:

```
(type: .public (Parser s a)
  {#.doc "A generic parser."}
  (-> s (Try [s a])))
```

	**Note**: This is also a functorial/monadic type.

`Parser` (_from `library/lux/control/parser/code`_) is the type of code-parsers: parsers which analyze `Code` nodes to extract arbitrary information.

The `Parser` type works with streams of inputs instead of single elements, and it often consumes some of those inputs, which is why the output involves an updated list of `Code`s.

There are many such code-parsers (and combinators) in the `library/lux/control/parser/code` module, and you should definitely take a look at what's available in the documentation.

Then, in the `library/lux/macro/syntax` module, there is a mechanism for defining macros: the `syntax:` macro.

```
"A more advanced way to define macros than 'macro:'."
"The inputs to the macro can be parsed in complex ways through the use of syntax parsers."
"The macro body is also (implicitly) run in the Meta monad, to save some typing."
"Also, the compiler state can be accessed through the *compiler* binding."
(syntax: .public (object {.let [imports (class_imports *compiler*)]}
                   {.let [class_vars (list)]}
                   {super (opt (super_class_decl^ imports class_vars))}
                   {interfaces (tuple (some (super_class_decl^ imports class_vars)))}
                   {constructor_args (constructor_args^ imports class_vars)}
                   {methods (some (overriden_method_def^ imports))})
  (let [def_code ($_ text\compose "anon-class:"
                     (spaced (list (super_class_decl$ (maybe.else object_super_class super))
                                   (with_brackets (spaced (list\map super_class_decl$ interfaces)))
                                   (with_brackets (spaced (list\map constructor_arg$ constructor_args)))
                                   (with_brackets (spaced (list\map (method_def$ id) methods))))))]
    (in (list (` ((~ (code.text def_code))))))))
```

	This example is a macro for making anonymous _JVM_ classes that lives in `lux/ffi`.

The difference between `macro:` and `syntax:` is that `syntax:` allows you to parse, in a structured manner, the inputs to your macro, thereby reducing considerably the complexity necessary for making _big_ macros.

Also, because you're using code-parsers for the hard work, you can write reusable parsers that you can share throughout your macros, if you want to have common syntax. You can even compose your parsers, or use parsers from someone else's library.

	There are already small modules under `library/lux/macro/syntax/` which house some reusable code-parsers and code-generators.

Additionally, `syntax:` binds the `Lux` value on a variable called `*lux*`, so you can use it during your parsing.

What do those code-parsers look like?

Here is an example:

```
... Taken from library/lux/math/infix.

(.module:
  {#.doc "Common mathematical constants and functions."}
  [library
   [lux #*
    [abstract
     [monad (#+ do)]]
    [control
     ["<>" parser ("#\." functor)
      ["<.>" code (#+ Parser)]]]
    [data
     ["." product]
     [collection
      ["." list ("#\." fold)]]]
    [macro
     [syntax (#+ syntax:)]
     ["." code]]
    [math
     [number
      ["n" nat]
      ["i" int]]]]])

(type: #rec Infix
  (#Const Code)
  (#Call (List Code))
  (#Unary Code Infix)
  (#Binary Infix Code Infix))

(def: literal
  (Parser Code)
  ($_ <>.either
      (<>\map code.bit <code>.bit)
      (<>\map code.nat <code>.nat)
      (<>\map code.int <code>.int)
      (<>\map code.rev <code>.rev)
      (<>\map code.frac <code>.frac)
      (<>\map code.text <code>.text)
      (<>\map code.identifier <code>.identifier)
      (<>\map code.tag <code>.tag)))

(def: expression
  (Parser Infix)
  (<| <>.rec (function (_ expression))
      ($_ <>.or
          ..literal
          (<code>.form (<>.many <code>.any))
          (<code>.tuple (<>.and <code>.any expression))
          (<code>.tuple ($_ <>.either
                            (do <>.monad
                              [_ (<code>.this! (' #and))
                               init_subject expression
                               init_op <code>.any
                               init_param expression
                               steps (<>.some (<>.and <code>.any expression))]
                              (in (product.right (list\fold (function (_ [op param] [subject [_subject _op _param]])
                                                              [param [(#Binary _subject _op _param)
                                                                      (` and)
                                                                      (#Binary subject op param)]])
                                                            [init_param [init_subject init_op init_param]]
                                                            steps))))
                            (do <>.monad
                              [init_subject expression
                               init_op <code>.any
                               init_param expression
                               steps (<>.some (<>.and <code>.any expression))]
                              (in (list\fold (function (_ [op param] [_subject _op _param])
                                               [(#Binary _subject _op _param) op param])
                                             [init_subject init_op init_param]
                                             steps)))
                            ))
          )))
```

And here are some examples of syntax macros:

```
... Also from library/lux/math/infix.

(def: (prefix infix)
  (-> Infix Code)
  (case infix
    (#Const value)
    value
    
    (#Call parts)
    (code.form parts)

    (#Unary op subject)
    (` ((~ op) (~ (prefix subject))))
    
    (#Binary left op right)
    (` ((~ op) (~ (prefix right)) (~ (prefix left))))))

(syntax: .public (infix {expr ..expression})
  {#.doc (example "Infix math syntax."
                  (infix [x i.* +10])
                  (infix [[x i.+ y] i.* [x i.- y]])
                  (infix [sin [x i.+ y]])
                  (infix [[x n.< y] and [y n.< z]])
                  (infix [#and x n.< y n.< z])
                  (infix [(n.* 3 9) gcd 450])

                  "The rules for infix syntax are simple."
                  "If you want your binary function to work well with it."
                  "Then take the argument to the right (y) as your first argument,"
                  "and take the argument to the left (x) as your second argument.")}
  (in (list (..prefix expr))))
```

```
(syntax: .public (^sequence& {patterns (<code>.form (<>.many <code>.any))}
                             body
                             {branches (<>.some <code>.any)})
  {#.doc (example "Allows destructuring of sequences in pattern-matching expressions."
                  "Caveat emptor: Only use it for destructuring, and not for testing values within the sequences."
                  (let [(^sequence& x y z _tail) (some_sequence_func +1 +2 +3)]
                    (func x y z)))}
  (with_identifiers [g!sequence]
    (let [body+ (` (let [(~+ (list\join (list\map (function (_ pattern)
                                                    (list (` [(~ pattern) (~ g!sequence)])
                                                          (` ((~! //.result) (~ g!sequence)))))
                                                  patterns)))]
                     (~ body)))]
      (in (list& g!sequence body+ branches)))))
```

```
(syntax: .public (cond> {_ _reversed_}
                        prev
                        {else body^}
                        {_ _reversed_}
                        {branches (p.some (p.and body^ body^))})
  {#.doc (example "Branching for pipes."
                  "Both the tests and the bodies are piped-code, and must be given inside a tuple."
                  (|> +5
                      (cond> [i.even?] [(i.* +2)]
                             [i.odd?] [(i.* +3)]
                             [(new> -1 [])])))}
  (with_identifiers [g!temp]
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

See you in the next chapter!

