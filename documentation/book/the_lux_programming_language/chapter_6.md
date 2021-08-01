# Chapter 6: Types in Detail

_Where you will learn the truth behind types._

---

We've talked about Lux types already, but only in a very high-level way.

On this chapter, you'll see how types are constructed, and hopefully that will give you some insight to understand better the subjects of later chapters.

```
(type: #export #rec Type
  (#Primitive Text (List Type))
  (#Sum Type Type)
  (#Product Type Type)
  (#Function Type Type)
  (#Parameter Nat)
  (#Var Nat)
  (#Ex Nat)
  (#UnivQ (List Type) Type)
  (#ExQ (List Type) Type)
  (#Apply Type Type)
  (#Named Name Type))
```

This is the type of types.

Crazy, right?

But as I've said before, Lux types are values like any other.

`Type` is a variant type, which just means that there are multiple options for type values.

Also, you may have noticed that `#rec` tag in the definition. You need to add it whenever you're defining a recursive type that takes no parameters.

So, the definition of `List` doesn't need it, but the definition of `Type` does.

Let's go over each of them.

---

```
(#Primitive Text (List Type))
```

This is what connects Lux's type-system with the host platform's. These types represent classes (in the JVM), with their respective parameters, if they have them (as would be the case for `ArrayList<Long>` in the JVM).

---

```
(#Sum Type Type)
(#Product Type Type)
```

You may have noticed that none of those options are called `#Variant` or `#Tuple`. The reason is that variants and tuples are just names for mathematical constructs called "sums" and "products". Funny names, right?

Well, mathematicians see variants as a way of "adding" types and tuples as a way of "multiplying" types, Of course, it's a bit difficult to picture that if you're thinking of numbers.

But a way to see variants is as an _"OR"_ operation for types: you get this option _OR_ that option. Conversely, tuples are like an _"AND"_ operation for types: you get this type _AND_ that type.

But, you may be wondering: "why do `#Variant` and `#Tuple` only take 2 types, instead of a list like `#Primitive` does?"

Well, as it turns out, you don't need a list of types to implement variants and tuples, because you can actually chain `#Variant` and `#Tuple` with other instances of themselves to get the same effect.

What do I mean?

Well, let me show you. To the left, you'll see the type as it's written in normal Lux code, and to the right you'll see the type value it generates.

```
(|)                   => Nothing
(| Bit)               => Bit
(| Bit Int)           => (#Sum Bit Int)
(| Bit Int Real)      => (#Sum Bit (#Sum Int Real))
(| Bit Int Real Char) => (#Sum Bit (#Sum Int (#Sum Real Char)))

(&)                   => Any
(& Bit)               => Bit
(& Bit Int)           => (#Product Bit Int)
(& Bit Int Real)      => (#Product Bit (#Product Int Real))
(& Bit Int Real Char) => (#Product Bit (#Product Int (#Product Real Char)))
```

You can see where this is going.

If I have a way to to pair up 2 types, and I can nest that, then I can chain things as much as I want to get the desired length.

What is a variant/tuple of 1 type? It's just the type itself; no pairing required.

This embedding means that [true 123 456.789 "X"] is the same as [true [123 456.789 "X"]], and the same as [true [123 [456.789 "X"]]].

It also means 5 is the same as [5], and [[5]], and [[[[[5]]]]].

As far as the compiler is concerned, there are no differences.

That might sound crazy, but there are some really cool benefits to all of this. If you're curious about that, you can check out [Appendix E](appendix_e.md) for more information on how Lux handles this sort of stuff.

And what happens when the variant/tuple has 0 types? That's when `Nothing` and `Any` come into play.

`Nothing` is a type that has no instances; which is to say, there's no expression which can yield a value of such a type.

It might seem oddd to have a type which has no instancces, but it can be useful to model computations which fail at runtime (thereby yielding no value).

So, another way of thinking of `Nothing` is as the type of failed expressions.

`Any`, on the other hand, is the opposite.

You can think of it as the super-type of all other types: the type of all values.

This means that not only `(: Nat 123)`, but also `(: Any 123)`.

Since `Any` does not give you any specific information about a value, it only tells you that a value exists, regardless of what its specific type happens to be.

So, whenever a function accepts or returns a dummy value of some kind, `Any` is a good candidate for that.

An easy way to create values of type `Any` is with the _empty tuple_ syntax `[]`.

In the same way that you cannot have empty tuple types, you also cannot make empty tuples.

But Lux sees that syntax and just sticks some simple constant value in there for you.

You might think that dummy values are, well, _dumb_, but they show up all the time.

Consider the `Maybe` type:

```
(type: #export (Maybe a)
  #None
  (#Some a))
```

The `#Some` tag holds values of type `a`, but what does `#None` hold? Nothing?

Well, `Maybe` is a variant, which means it's a `#Sum`, which looks like this:

```
(#Sum Type Type)
```

So we know that `#None` must hold _something_. But what?

Well, `Any`thing, really.

So the type definition for `Maybe` is equivalent to this:

```
(type: #export (Maybe a)
  (#None Any)
  (#Some a))
```

If you don't care what value you store somewhere, then you can store `Any` value in there.

In practice, you can create instances of `Maybe` by writing this `(#None [])`, or `(#None 123)`, or just `#None`.

If you only write the tag, then Lux treats it as if you paired it up with an empty tuple.

So `#None` is equivalent to `(#None [])`.

---

```
(#Function Type Type)
```

Now that we have discussed variant and tuple types, it shouldn't come as a surprise that a similar trick can be done with function types.

You see, if you can implement functions of 1 argument, you can implement functions of N arguments, where N > 1.

All I need to do is to embed the rest of the function as the return value to the outer function.

	It might sound like this whole business of embedding tuples, variants and functions inside one another must be super inefficient; but trust me: Lux has taken care of that.

	The Lux compiler features many optimizations that compile things down in a way that gives you maximum efficiency. So, to a large extent, these embedded encodings are there for the semantics of the language, but not as something that you'll pay for at run-time.

One of the cool benefits of this approach to functions is Lux's capacity to have partially applied functions.

Yep, that's a direct consequence of this theoretical model.

---

```
(#Parameter Nat)
```

This type is there mostly for keeping track of type-parameters in _universal and existential quantification_.

We'll talk about those later. But, suffice it to say that `#Parameter` helps them do their magic.

---

```
(#Var Nat)
```

These are type variables.

They are used during type-inference by the compiler, but they're also part of what makes universal quantification (with the `All` macro) able to adjust itself to the types you use it with.

Type-variables start _unbound_ (which means they're not associated with any type), but once they have been successfully matched with another type, they become bound to it, and every time you use them afterwards it's as if you're working with the original type.

Type-variables, however, cannot be _re-bound_ once they have been set, to avoid inconsistencies during type-checking.

---

```
(#Ex Nat)
```

An existential type is an interesting concept (which is related, but not the same as existential quantification).

You can see it as a type that exists, but is unknown to you. It's like receiving a type in a box you can't open.

What can you do with it, then? You can compare it to other types, and the comparison will only succeed if it is matched against itself.

It may sound like a useless thing, but it can power some advanced techniques.

---

```
(#UnivQ (List Type) Type)
```

This is what the `All` macro generates: _universal quantification_.

That `(List Type)` you see there is meant to be the _context_ of the universal quantification. It's kind of like the environment of a function closure, only with types.

The other `Type` there is the _body_ of the universal quantification.

To understand better what's going on, let's transform the type of our `iterate_list` function from [Chapter 5](chapter_5.md) into its type value.

```
(All [a b] (-> (-> a b) (List a) (List b)))

## =>

## (#.UnivQ #.End (#.UnivQ #.End (-> (-> (#.Parameter 3) (#.Parameter 1)) (List (#.Parameter 3)) (List (#.Parameter 1))))
```

	**Note**: I didn't transform the type entirely to avoid unnecessary verbosity.

As you can see, I do the same embedding trick to have universal quantification with multiple parameters.

Also, `a` and `b` are just nice syntactic labels that get transformed into `#Parameter` types.

	The reason the type-parameters have those IDs is due to a technique called [De Bruijn Indices](https://en.wikipedia.org/wiki/De_Bruijn_index).

---

```
(#ExQ (List Type) Type)
```

Existential quantification works pretty much the same way as universal quantification.

Its associated macro is `Ex`.

Whereas universal quantification works with type-variables, existential quantification works with existential types.

---

```
(#Apply Type Type)
```

This is the opposite of quantification.

`#Apply` is what you use to parameterize your quantified types; to customize them as you need.

With `#Apply`, `(List Int)` transforms into `(#Apply Int List)`.

For multi-parameter types, like `Dictionary` (from `lux/data/collection/dictionary`), `(Dictionary Text User)` would become `(#Apply User (#Apply Text Dictionary))`.

	As you can see, the nesting is slightly different than how it is for tuples, variant and functions.

---

```
(#Named Name Type)
```

`#Named` is less of a necessity and more of a convenience.

The type-system would work just fine without it, but users of the language probably wouldn't appreciate it while reading documentation or error messages.

`#Named` is what gives the name _"List"_ to the `List` type, so you can actually read about it everywhere without getting bogged down in implementation details.

You see, Lux's type system is structural in nature, rather than nominal (the dominating style in programming languages).

That means all that matters is how a type is built; not what you call it.

That implies 2 types with different names, but the exact same value, would actually type-check in your code.

That may sound odd (if you come from Java or other languages with nominal types), but it's actually very convenient and enables you to do some pretty nifty tricks.

	For more information on that structural types, head over to [Appendix E](appendix_e.md).

`#Named` gives Lux's type-system a bit of a nominal feel for the convenience of programmers.

## Regarding Error Messages

When you get error messages from the type-checker during your coding sessions, types will show up in intuitive ways most of the time, with a few exceptions you might want to know.

Existential types show up in error messages like `⟨e:246⟩` (where 246 is the ID of the type). Whereas type-variables show up like `⌈v:278⌋`.

Those types tend to show up when there are errors in the definition of some polymorphic function.

---

You may be tired of reading about types, considering that they are (to a large degree) an implementation detail of the language.

However, one of the key features of Lux is that types can be accessed and manipulated by programmers (often in macros) to implement various powerful features.

In the next chapter, you'll get acquainted with one such feature.

See you in [the next chapter](chapter_7.md)!

