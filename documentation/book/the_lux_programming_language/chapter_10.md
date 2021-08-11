# Chapter 10: Code and macros

_Where magic turns into science._

---

I've talked about many macros in this book.

There's a macro for _this_ and a macro for _that_.

You use macros for defining stuff, for making types and functions and lists, for doing pattern-matching, and for control-flow.

There's a macro for everything.
Yet, I haven't even shown a macro being defined yet.

Quiet your mind, young grasshopper. You're about to be enlightened.

But first, you need to learn a few things.

## The AST

The word **AST** stands for _Abstract Syntax Tree_.

An AST is a representation of the syntax of a programming language, and compilers use them for the sake of analyzing the source-code (like, by type-checking it), and then generating the binary/byte-code output.

You might think that's none of your business.
Only compiler writers have to worry about that stuff, right?

Oh, you have much to learn, young grasshopper.

You see, the power of macros lies in the fact that (_to some extent_) users of the language can play the role of language designers and implementers.

Macros allow you to implement your own features in the language and to have them _look and feel_ just like native features.

I mean, beyond the native syntax for writing numbers, text, variants, tuples and records, every single thing you have written so far has been macros.

Module statements? _Yep, macros_.

Definition statements? _Yep, macros_.

Function expressions? _Yep, macros_.

And you'd have never suspected those weren't native Lux features had I not told you they were macros.

Now, just imagine making your own!

But macros work with the Lux _AST_, so that's the first thing you need to master.

Check it out:

```
(type: #export Location
  {#module Text
   #line   Nat
   #column Nat})

(type: #export (Ann m v)
  {#meta  m
   #datum v})

(type: #export (Code' w)
  (#Bit Bit)
  (#Nat Nat)
  (#Int Int)
  (#Rev Rev)
  (#Frac Frac)
  (#Text Text)
  (#Identifier Name)
  (#Tag Name)
  (#Form (List (w (Code' w))))
  (#Tuple (List (w (Code' w))))
  (#Record (List [(w (Code' w)) (w (Code' w))])))

(type: #export Code
  (Ann Location (Code' (Ann Location))))
```

The `Code` type is the one you'll be interacting with, but all it does is wrap (recursively) the _incomplete_ `Code'` type, giving it some meta-data `Ann`otations to know where each _AST_ node comes from in your source-code.

The real magic is in the `Code'` type, where you can see all the alternative syntactic elements.

The `Name` type (from the `library/lux` module), is just a `[Text Text]` type.
The first part holds the module/prefix of the identifier/tag, and the second part holds the name itself. So `library/lux/data/collection/list.reversed` becomes `["library/lux/data/collection/list" "reversed"]`, and `map` becomes `["" "map"]`.

	`list.reversed` would become `["library/lux/data/collection/list" "reversed"]` anyway, because aliases get resolved prior to analysis and macro expansion.

Forms are `(syntactic structures delimited by parentheses)`, and tuples are `[syntactic structures delimited by brackets]`.
Records `{#have lists #of pairs}` of `Code`s instead of single `Code`s, because everything must come in key-value pairs.

## Quotations

We know everything we need to extract information from the `Code` type, but how do we build `Code` values?

Do we have to build it with our bare hands using variants and tuples?

That sounds... exhausting.

Well, we don't have to. There are actually many nice tools for making our lives easier.

One nice resource within our reach is the `library/lux/macro/code` module, which contains a variety of functions for building `Code` values, so we don't have to worry about cursors and variants and all that stuff.

But, even with that, things would get tedious.
Imagine having to generate an entire function definition (or something even larger), by having to call a bunch of functions for every small thing you want.

Well, don't fret. The Lux Standard Library already comes with a powerful mechanism for easily generating any code you want and you don't even need to import it (i.e. it's in the `library/lux` module).

```
... Quotation as a macro.
(' "YOLO")
```

Quotation is a mechanism that allows you to write the code you want to generate, and then builds the corresponding `Code` value.

The `'` macro is the simplest version, which does exactly what I just described.

This would turn the text `"YOLO"` into `[{#.module "" #.line 0 #.column 0} (#.Text "YOLO")]`.
If you want to know what that would look like with the tools at `library/lux/macro/code`, it would be: `(text "YOLO")`.

The beautiful thing is that `(' (you can use the "'" #macro [to generate {arbitrary code} without] worrying (about the "complexity")))`.

```
... Hygienic quasi-quotation as a macro.
... Unquote (~) and unquote-splice (~+) must also be used as forms.
... All unprefixed identifiers will receive their parent module's prefix if imported; otherwise will receive the prefix of the module on which the quasi-quote is being used.
(` (def: (~ name)
     (function ((~ name) (~+ args))
       (~ body))))
```

This is a variation on the `'` macro that allows you to do templating with the code you want to generate.

Everything you write will be generated _as is_, except those forms which begin with `~` or `~+`.

`~` means: _evaluate this expression and use its `Code` value_.

`~+` means: _the value of this expression is a list of `Code`s, and I want to splice all of them in the surrounding `Code` node_.

With these tools, you can introduce a lot of complexity and customization into your code generation, which would be a major hassle if you had to build the `Code` nodes yourself.

	You may be wondering what does "hygienic" means in this context.
	It just means that if you use any identifier in your template which may refer to an in-scope definition or local variable, the identifier will be resolved to it.
	Any identifier that does not correspond to any known in-scope definition or variable will trigger a compile-time error.
	This ensures that if you make a mistake writing your template code, it will be easy to spot during development.
	Also, it will be harder to collide (by mistake) with user code if you, for instance, write the code for making a local variable named `foo`, and then the person using your macro uses a different `foo` somewhere in their code.

```
... Unhygienic quasi-quotation as a macro.
... Unquote (~) and unquote-splice (~+) must also be used as forms.
(`' (def: (~ name)
      (function ((~ name) (~+ args))
        (~ body))))
```

Finally, there is this variation, which removes the hygiene check.

Out of the 3 variations, the one you'll most likely use is the **2nd one**, since it provides both **safety** and **power**.

## Macros

Now that you know how to generate code like a pro, it's time to see how macros get made.

First, let's check the type of macros:

```
(type: .public Macro
  (primitive "#Macro"))
```

That does not look particularly useful.
What the hell is a `"#Macro"`?

Fundamentally, all macros are functions.
However, the compiler cannot treat them as normal functions because they must be applied to code at compile-time, rather than run-time.

For this reason, the Lux compiler must have some way to identify macros as distinct from functions.
It does so by labelling (_type-wise_) with this funky type.

There is, however, another type which elucidates what is going on with macros.

```
(type: .public Macro'
  (-> (List Code) (Meta (List Code))))
```

You might remember from the previous chapter that you can only access the `Lux` compiler state inside of macros.
Now, you can see how everything connects.

You define macros by using the `macro:` macro (_so meta..._):

```
(macro: .public (name_of tokens)
  {#.doc (doc "Given an identifier or a tag, gives back a 2 tuple with the module and name parts, both as Text."
              (name_of #.doc)
              "=>"
              ["library/lux" "doc"])}
  (case tokens
    (^template [<tag>]
      [(^ (list [_ (<tag> [module name])]))
       (\ meta.monad in (list (` [(~ (code.text module)) (~ (code.text name))])))])
    ([#Identifier] [#Tag])
    
    _
    (meta.failure "Wrong syntax for 'name_of'.")))
```

Here's another example:

```
(macro: .public (else tokens state)
  {#.doc (doc "Allows you to provide a default value that will be used"
              "if a (Maybe x) value turns out to be #.None."
              "Note: the expression for the default value will not be computed if the base computation succeeds."
              (else +20 (#.Some +10))
              "=>"
              +10
              --------------------------
              (else +20 #.None)
              "=>"
              +20)}
  (case tokens
    (^ (.list else maybe))
    (let [g!temp (macro.gensym "")]
      (#.Right [state (.list (` (case (~ maybe)
                                  (#.Some (~ g!temp))
                                  (~ g!temp)

                                  #.None
                                  (~ else))))]))

    _
    (#.Left "Wrong syntax for else")))
```

	You may want to read [Appendix C](appendix_c.md) to learn about the pattern-matching macros used in these examples.

As you can see, I'm using both quotation and the functions from the `library/lux/macro/code` module to generate code here.

I'm also using the `gensym` function from `library/lux/macro`, which generates unique identifiers for usage within code templates in order to avoid collision with any code provided by the user of the macro.

The macro receives the raw `List` of `Code` tokens and must process them manually to extract any information it needs for code generation.
After that, a new `List` of `Code` tokens must be generated.

If there are any macros in the output, they will be _expanded_ further until only primitive/native syntax remains that the Lux compiler can then analyze and compile.

	You may be wondering what is the relationship between the `Macro` and `Macro'` types.
	When you define a macro, you define it as a function, which is to say a `Macro'` type.
	But once it has been defined, it gets re-labelled as a `Macro`, so that way the Lux compiler can distinguish it from other functions.
	This is all done for you by the `macro:` macro, so there's no need to worry about it.

---

You have learned how to use one of the greatest superpowers that Lux has to offer.

But, if you're like me, you might be getting the nagging feeling that something is not right here.

I mean, if I have to pattern-match against the code I receive; what happens when my macros have complex inputs?

Clearly, analyzing the input code is far more difficult than generating it with the quoting macros.

Don't worry about it.
Because in the next chapter, you will learn a more sophisticated method of macro definition that will make writing complex macros a breeze.

See you in [the next chapter](chapter_11.md)!
