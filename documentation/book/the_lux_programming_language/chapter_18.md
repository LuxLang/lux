# Chapter 18: Extensions

_Where you will teach new tricks to an old compiler._

Lux is a member of the Lisp family of languages.

As such, it has macros, which are a classic staple of lisps which allow programmers to extend the syntax of their language to implement all sorts of new features, domain-specific-languages (or _DSLs_, for short), optimizations, and even entire paradigms, such as object-oriented programming.

However, compilers do much more than just process syntax.

In statically-typed languages, compilers also type-check code; and even in dynamically-typed languages, compilers often optimize your code, and generate output code for either the machine the program will run on, or some sort of virtual machine or interpreter.

Macros, however, don't extend any of these other aspects of compilation; only the syntax of the language.

And so, even lisps keep the programmer out of most of the activities of the compiler.

Lisps may be more extensible than most other languages, but they are not _completely extensible_.

With Lux, however, I want to take the traditional extensibility of lisps and take it to its ultimate expression.

Not only should syntax be extensible, but also type-checking, optimization, and code-generation.

And who knows? Maybe _even more_ aspects will be extensible in the future.

But, for now, let's see what Lux has got to offer.

---

Now, before we get into the details, let's first discuss the syntax being used for just a moment.

```clojure
("js object do" "replaceAll" template [pattern replacement])
```

_What is up with that text-based syntax?_

Well, here's the deal.

Most lisps have what they call _special forms_.

Basically, these are macro-like expressions which provide some fundamental features of the language.

Special forms, however, are not macros.

They are default types of expressions provided by the compiler/interpreter.

They are named by symbols/identifiers, such as `let`, or `if`, and because of that, the symbols/identifiers used to name them are reserved.

That is to say, if there is a special form named `if`, that means you cannot make your own definition named `if`, because then the compiler/interpreter might get confused as to whether somebody is using the special form, or an arbitrary definition.

The term **keyword** is often used in programming languages to refer to symbols/identifiers considered to be special by the compiler/interpreter, and reserved for the exclusive use of the language designer.

Personally, I hate the idea of having special privileges that users of Lux lack.

I think that, as a designer, I'd be cheating if I could do things nobody else could, such as using reserved names.

So, I didn't like the idea of naming extensions with identifiers, because then those identifiers would effectively become reserved keywords.

Instead, since an expression that attempts to call `Text` as a function is meaningless in Lux, as `Text`s are not functions; I thought it would be nice to then use that otherwise meaningless syntax by having `Text` literals name extensions.

So, that's the story.

Extensions are named by `Text` literals to avoid reserving identifiers as keywords, thereby protecting the programmer's freedom to name their definitions however they want.

---

Now that we've got the origin story out of the way, let's talk business.

How can you install your own extensions?

Well, the first thing to note is that there are different types of extensions.

Remember what I said about the compiler doing different activities, such as type-checking, optimization and code-generation?

Well, you can't really expect an extension meant for type-checking to work during code-generation.

The work done on either phase would be too different to what was necessary for the other phase.

And so, Lux provides 4 different types of extensions.

	However, as far as this tutorial is concerned, we will only be covering 3 of them, as the 4th one is very different to the other 3 in what its purpose is and what you're supposed to do with it, and I'd feel more comfortable teaching it later, once I figure out a reasonable way for normal Lux programmers to make use of it.

---

The first type of extension we'll see is the `Analysis` extension:

```clojure
(.using
 [library
  [lux "*"
   [extension {"+" [analysis: synthesis: generation:]}]
   ["@" target
    ["[0]" jvm]
    ["[0]" js]
    ["[0]" python]
    ["[0]" lua]
    ["[0]" ruby]]
   [abstract
    ["[0]" monad {"+" [do]}]]
   [control
    ["<>" parser
     ["<[0]>" code]
     ["<[0]>" analysis]
     ["<[0]>" synthesis]]]
   [data
    [collection
     ["[0]" sequence]]]
   [tool
    [compiler
     ["[0]" phase]
     [language
      [lux
       ["[0]" analysis]
       ["[0]" synthesis]
       ["[0]" directive]
       [phase
        [analysis
         ["[0]" type]]]]]]]]])

(analysis: ("my triple" self phase archive [elementC <code>.any])
  (do phase.monad
    [[type elementA] (type.with_inference
                       (phase archive elementC))
     _ (type.infer (.Tuple type type type))]
    (in (analysis.tuple (list elementA elementA elementA)))))
```

If you want to write your own extensions, the first you'll want to do is import the `library/lux/extension` module.

It contains macros for easily implementing extensions.

These macros handle parsing the inputs to your extensions using the monadic parsing infrastructure Lux provides.

Each type of extension takes a different type of input, and produces a different type of output.

In the case of `Analysis` extensions, they take `(List Code)` as an input, and produce a single `Analysis` node as output.

	By the way, _"analysis"_ is the name Lux gives to the process of type-checking and verifying program correctness.

Here, we've got a _trivial_ extension where we take a single value, and we produce a triple of the same value.

As you can see, we're doing some type-inferencing to first figure out the type of our input value, and then to signal to the compiler what the type of our `("my triple" ???)` ought to be.

	Also, one thing all phases of the compiler (such as analysis) have in common is that they are instances of the `Phase` type, defined in `library/lux/tool/compiler/phase`.
	That is why I make use of the `Monad` implementation for `Phase` in this example.

Now, we have converted fairly exceptional code, using our `"my triple"` extension, into fairly normal code that Lux knows how to handle, which is just a 3-tuple of the same value.

As you can probably guess, this great power comes with great responsibility.

Since you are in total control of the type-checking that is happening, it is entirely possible to fool the compiler into believing the wrong things about your code.

It is **very important** to be **careful**, when _implementing extensions_, that the output of those extensions is **correct in every situation**; because, _unlike with normal Lux code_, the compiler **cannot verify** that _your extension_ is not doing something that it shouldn't.

I have gifted you _promethean fire_.

**DO NOT MAKE ME REGRET IT** ;)

Also, you might have noticed that, besides the input code we're parsing, our extension is receiving 3 extra parameters:

* `self`: This is just a `Text` that contains the name of the extension itself (in this case, the value `"my triple"`). It might seem redundant, but it can be useful to refer to the extension's name within its own implementation without having to constantly repeat the name literally.
* `phase`: This is the Lux compiler's implementation of the phase an extension is part of (in this case, the `Analysis` phase). It allows you to process your inputs the same way the Lux compiler does, before doing something special with them (like triplicating them, as in this example).
* `archive`: The `Archive` is a special data-structure used by the Lux compiler to store valuable information gathered during the compilation process. It is very important, but this might not be the best place to get into its relevance and what can be done with it. For now, just make sure to pass it around when invoking the `phase` function.

---

```clojure
(analysis: ("my quadruple" self phase archive [elementC <code>.any])
  (do phase.monad
    [[type elementA] (type.with_inference
                       (phase archive elementC))
     _ (type.infer (.Tuple type type type type))]
    (in {analysis.#Extension self (list elementA)})))

(synthesis: ("my quadruple" self phase archive [elementA <analysis>.any])
  (do phase.monad
    [elementS (phase archive elementA)]
    (in (synthesis.tuple (list elementS elementS elementS elementS)))))
```

The `Synthesis` phase is where we do optimizations.

`Synthesis` extensions take a `(List Analysis)` as input, and produce a single `Synthesis` node as output.

Currently, the optimization infrastructure Lux provides is not very sophisticated, and much of it has yet to be properly exposed to programmers, so you'll probably not be working too much in this layer for now.

---

```clojure
(analysis: ("my quintuple" self phase archive [elementC <code>.any])
  (do phase.monad
    [[type elementA] (type.with_inference
                       (phase archive elementC))
     _ (type.infer (.Tuple type type type type type))]
    (in {analysis.#Extension self (list elementA)})))

(generation: ("my quintuple" self phase archive [elementS <synthesis>.any])
  (do phase.monad
    [elementG (phase archive elementS)]
    (in (for {@.jvm (row.row (#jvm.Embedded elementG)
                             (#jvm.Stack #jvm.DUP)
                             (#jvm.Stack #jvm.DUP)
                             (#jvm.Stack #jvm.DUP)
                             (#jvm.Stack #jvm.DUP))
              @.js (js.array (list elementG
                                   elementG
                                   elementG
                                   elementG
                                   elementG))}))))
```

Now, let's talk about the star of the show: _generation extensions_.

	_Generation_ is just a short way of saying _code-generation_, and it's the part of the compiler that generates the actual output that you eventually execute as a program.
	Also, I'd like to use this opportunity to point out that when Lux encounters a usage of an extension during any phase, and it does not know this extension, it just proceeds to process the parameters to the extension, and then hands over the extension call, with the processed parameters, to the next phase.
	As a consequence, we can write a generation extension without having to write the preceeding `Synthesis` extension, because we can trust Lux to handle things reasonably and then use our generation extension when its turn comes up.

Generation extensions take a `(List Synthesis)` as input, and produce _suitable code_ as output.

Since Lux offers different compilers that target different platforms, it is impossible for this phase to produce a single type of output.

Instead, the type of the output of generation extensions will depend on which compiler you're using.

In this case, we're implementing an extension that only works on the JVM, and on JavaScript.

In the case of the JVM, our output type is `library/lux/target/jvm.Bytecode`; and in the case of JavaScript, our output type is `library/lux/target/js.Expression`.

I will not go into detail about the machinery in these modules, as that is what the documentation of the Standard Library is for.

But, suffice it to say, that there are plenty of functions and useful machinery in there to write syntactically correct output code; and this is the same machinery that the Lux compiler itself uses, so you don't need to worry about your output being in any way different from what Lux itself produces.

---

My goal with extensions has been to take the ideas of Lisp and take them to their ultimate conclusions.

To push what is possible as far as imagination can reach.

If you have no clue what to do with this power, you are in good company.

The tools at our disposal both enable, and limit, our imagination.

If you have been using lisp languages for a while, you're probably well aware of the immense power that macros can provide.

Sadly, most programmers are unfamiliar with macros, and many among them want to stay away from macros, because they fear that all that power will be abused and get out of hand.

They are afraid of power because they never learned how to use it.

Even we lispers have been denied the power to completely control the behavior of our compilers, and so we do not know what's possible and what amazing progress can be made with these tools.

It is my hope that now that I have exposed the means to control and extend the compiler in these new directions, brilliant minds will seize this opportunity to discover new means to extend the power of programmers.

Perhaps you, _my dear reader_, will be one such mind.

Before you close this book, you might want to read [the last few words I've got to offer](conclusion.md).

