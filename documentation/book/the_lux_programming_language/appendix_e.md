# Appendix E: Lux implementation details

If you read [Chapter 6](chapter_6.md), you encountered Lux's funny way of encoding variants, tuples and functions.

You may be wondering: _how can this possibly have good performance?_

And: _what benefit can this possible have?_

I'll tackle those questions one at a time.

## How can this possibly have good performance?

First, let me explain how things get compiled down in the JVM.

Tuples are compiled as object arrays.
That means an n-tuple is (_roughly_) an n-array.

	The reason why I say _"roughly"_ will be explained shortly.

Variants, on the other hand, are 3-arrays.

* The first element is the int value of its associated tag.
* The second element is a kind of boolean flag used internally by the Lux run-time infrastructure.
* The third element contains the variant's value.

Finally, functions produce custom classes, and function values are just objects of those classes.

These classes contain everything the function needs:

* its compiled code.
* its environment/closure.
* any partially-applied arguments it may have.

How, then, can all of this be made efficient?

Does applying a function `f` to arguments `a`, `b` and `c` create intermediate function values because you can only apply it one argument at a time?

Do tuples consume a lot of memory because everything gets nested?

**Not really.**

With regards to tuples, remember what I said: _an n-tuple is (roughly) an n-array_.

If you write `[#0 12 -34 +56.78 "nine"]`, Lux will actually compile it down as a 5-array, instead of a series of nested 2-arrays.

However, if you have a variable `foo` which contains the last two arguments, and you build your tuple like `[#0 12 -34 foo]`, Lux will compile it as a 4-array, with the last element pointing to the `[+56.78 "nine"]` sub-tuple.

But, as I said in [Chapter 6](chapter_6.md), Lux treats both the same.

_How does that work?_

Well, Lux knows how to work with both flat and nested tuples and it can do so efficiently; so ultimately it doesn't matter.

It will all be transparent to you.

When it comes to variants, the situation is similar in some ways, but different in others.

Regardless, Lux also knows how to work with the different cases efficiently (which is important for pattern-matching, not just for variant/tuple construction).

Finally, we have to consider functions.

Merging nested functions into a single one that can work like all the nested versions turns out to be pretty easy.

Just allocate enough space for all the (potentially) partially-applied arguments, plus space for the environment/closure.

If you invoke the function with all the arguments, you just run it.

If you invoke it with less than needed, you just use the space you have to store the partial arguments and generate a single new instance with the extra data (instead of generating a new function object for every argument you apply).

And if you're invoking a partially applied function, then you run it with the partial arguments and the new arguments.

Piece of cake.

## What benefit can this possible have?

I already explained in [Chapter 6](chapter_6.md) how the nested nature of Lux functions enables partial application (a useful day-to-day feature that saves you from writing a lot of boilerplate).

What about variants and tuples?

Well, the cool thing is that this makes your data-structures composable, a property that enables you to implement many really cool features.

One that I really like and has turned out to be very useful to me, is that you can use _combinators_ for various data-types that produce single bits of data, and you can fuse them to generate composite data-types, with minimal plumbing.

	You can see _combinators_ as functions that allow you to provide an extra layer of functionality on top of other components, or that allow you to fuse components to get more complex ones.

Here are some examples from the `library/lux/ffi` module, where I have some types and code-parsers for the many macros implemented there:

```clojure
(type: .public Privacy
  (Variant
   {#PublicP}
   {#PrivateP}
   {#ProtectedP}
   {#DefaultP}))

(def: privacy_modifier^
  (Parser Privacy)
  (let [(^open "[0]") <>.monad]
    ($_ <>.or
        (<code>.this! (' "public"))
        (<code>.this! (' "private"))
        (<code>.this! (' "protected"))
        (in []))))
```

Here, I have a variant type, and I'm creating a code-parser that produces instances of it by simply combining smaller parsers (that just produce unit values, if they succeed) through the `<>.or` combinator.

	These code-parsers and combinators are defined in the `library/lux/control/parser/code` module, and the `library/lux/control/parser` module.

`<>.or` is a combinator for generating variant types.

Its tuple counterpart is called `<>.and` (also, remember that records are tuples, so you'd use the same function).

This wouldn't be possible if variant types weren't nested/composable; forcing me to write custom ad-hoc code instead of taking advantage of common, reusable infrastructure.

Here's an example of `<>.and` in action:

```clojure
... From library/lux/target/jvm/type
(type: .public Argument
  [Text (Type Value)])

... From library/lux/ffi
(def: (argument^ type_vars)
  (-> (List (Type Var)) (Parser Argument))
  (<code>.tuple (<>.and <code>.local_symbol
                        (..type^ type_vars))))
```

The cool thing is that these combinators show up not just in syntax parsers, but also in command-line argument parsing, lexing, concurrenct/asynchronous operations, error-handling and in many other contexts.

The nested/composable semantics of Lux entities provide a flexibility that enables powerful features (such as this) to be built on top.

