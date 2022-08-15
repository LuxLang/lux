# Chapter 5: Control flow

_Where you will learn how to give intelligence to your code._

---

## Branching

So far, we've only seen very simple examples of program/function logic that are very straightforward.

But real world programs are far from straightforward, and there's a lot of testing and decision-making involved in how they operate.

The first important concept to master in any programming language is how to make decisions and _branch_ the path our program takes, and Lux offers one primary way of doing this: _pattern-matching_.

But before we head into that, let's first see 2 weaker mechanisms for branching that might be familiar to programmers coming from other programming languages.

### If

We've already met the humble `if` expression in the previous chapter.

As explained there, the expression takes the following form:

```clojure
(if test
  then
  else)
```

Where `test`, `then` and `else` are arbitrary Lux expressions.

In terms of types, it works like this:

```clojure
(is X (if (is Bit test)
        (is X then)
        (is X else)))
```

Here is an example:

```clojure
(if true
  "Oh, yeah!"
  "Aw, hell naw!")
```

So, both branches must produce values of the same type for the type-checker to let it pass.

### Cond

`cond` is like a more general version of the `if` macro.

For those of you coming from conventional programming languages, `cond` is like a chain of _if-else_ statements/expressions, with a default _else_ branch at the end, in case all fails.

It looks like this:

```clojure
(cond test-1 then-1
      test-2 then-2
      ...
      test-n then-n
      else)
```

And, in terms of types, it looks like this:

```clojure
(is X (cond (is Bit test-1) (is X then-1)
            (is Bit test-2) (is X then-2)
            ...
            (is Bit test-n) (is X then-n)
            (is X else)))
```

Here is an example:

```clojure
(cond (n.even? num) "even"
      (n.odd? num)  "odd"
      ... else-branch
      "???")
```

So, it's easy to intuit how `cond` would de-sugar into several nested `if` expressions.

Also, I'd like to point out that both `if` and `cond` are macros, instead of native Lux syntax.

The reason for that is simply that they can both be implemented in terms of pattern-matching.

## Pattern-matching

Some of you may not be familiar with the concept of pattern-matching if you come from non-functional programming languages, or from FP languages that lack pattern-matching (e.g. _Clojure_).

Pattern-matching is similar to the branching expressions that we saw previously, except that instead of being based on making boolean tests, it's based on comparing patterns against data, and executing a branch if the pattern matches that data.

The beautiful thing is that the pattern can be very complicated (even involving the binding of variables), and so the testing can be very powerful.

We can see its power by looking at some examples.

For instance, the `factorial'` function you saw in the previous chapter could have been written like this:

```clojure
(def (factorial' acc n)
  (-> Nat Nat Nat)
  (when n
    0 acc
    _ (factorial' (n.* n acc) (-- n))
    ))
```

As you may imagine, `when` is the pattern-matching macro.

It takes the data you want to pattern-match against (in this case, the `n` variable), and then tests it against several patterns until it finds a match, in which case it executes its branch.

Here, we test if `n` equals `0`. If it does, we just return the `acc` value.

Otherwise, we have a _default_ branch with a pattern that doesn't test anything called `_`. That will handle the case where the input is greater than 0.

The _"default"_ branch works because we're binding the value of `n` onto a variable called `_`, and binding always succeeds, which is why we can use that branch as a default.

However, since it is binding a variable, that means we could have used `_` instead of `n` during our calculations; like this:

```clojure
(def (factorial' acc n)
  (-> Nat Nat Nat)
  (when n
    0 acc
    _ (factorial' (n.* _ acc) (-- _))
    ))
```

However, as a convention, `_` is used as the name for values we don't care about and don't plan to use in our code.

---

Pattern-matching doesn't limit itself only to `Nat`s, and can also be used with `Bit`s, `Int`s, `Rev`s, `Frac`s, `Text`s, variants, tuples, records, and much more!

Regarding the _"much more"_ claim, you should check out [Appendix C](appendix_c.md), where I discuss a powerful extension mechanism called **pattern-matching macros**.

Here are a couple more examples so you can see the possibilities.

```clojure
(let [test true]
  (when test
    #1 "Oh, yeah!"
    #0 "Aw, hell naw!"
   ))
```

```clojure
(when (list 1 2 3)
  {.#Item x {.#Item y {.#Item z {.#End}}}}
  {.#Some (n.+ x (n.* y z))}

  _
  {.#None})
```

In the first example, you'll notice that we have rewritten the prior `if` example in terms of pattern-matching.

Also, you'll notice the introduction of a new macro, called `let`.

`let` is a simple way to create local-variables in Lux.

Its syntax looks like this:

```clojure
(is X (let [var-1 expr-1
            var-2 expr-2
            ...
            var-n expr-n]
        (is X body)))
```

Where the types of the variables will correspond to those of their matching expressions, and the type of the `let` expression will be the same as that of its body.

Also, remember when I told you that you can use pattern-matching to bind variables?

Well, guess what! `let` is implemented in terms of `when`, and it just gives you a more convenient way to bind variables than to go through all the trouble of doing pattern-matching.

Now, in the second example, we're deconstructing a list in order to extract its individual elements.

The `List` type is defined like this:

```clojure
(type (List a)
  {#End}
  {#Item a (List a)})
```

`.#End` represents the empty list, while `.#Item` constructs a list by prepending an element to the beginning of another list.

With pattern-matching, we're opening our list up to 3 levels in order to extract its 3 items and do a simple math calculation.

If the match succeeds, we produce a value of type `(Maybe Int)` by wrapping our result with the `.#Some` tag, from the `Maybe` type.

If the match fails, we just produce nothing, by using the `.#None` tag, also from the `Maybe` type.

While `List` allows you to group an arbitrary number of values into a single structure, `Maybe` is for values you may or may not have.

Also, you may have noticed how different (and uncomfortable!) it is to pattern-match against a `List`, since you have to use its real syntax, with its tags; whereas to build the list we can just piggy-back on the `list` macro.

Don't worry too much about it, because there's a better way to do it that also allows us to use the `list` macro. If you're curious about it, head over to [Appendix C](appendix_c.md) to learn more about **pattern-matching macros**.

## Looping

Alright. So, we know several ways to branch, and also how to bind variables.

But we know life isn't just about making decisions.

Sometimes, you just have to do your work over and over again until you're done.

That's what _looping_ is for!

### Recursion

In functional programming, _recursion_ is the main mechanism for looping in your code.

Recursion is nothing more than the capacity for a function to call itself (often with different parameters than the initial call).

It's not hard to see how this mechanism can be used to loop in any way you want, and we've already seen examples of recursion in action.

```clojure
(def (factorial' acc n)
  (-> Nat Nat Nat)
  (if (n.= 0 n)
    acc
    (factorial' (n.* n acc) (-- n))))
```

The `factorial'` function calls itself with an ever increasing accumulator (that will hold the eventual result), and an ever decreasing input value.

Recursion in many languages is seen as a risky operation, since programming languages have what are called `"stacks"`, which are structures holding the parameters to functions and the return addresses for where to send the results once the functions are done.

Every function call you issue pushes a new frame onto the stack, and if enough frames are pushed, eventually the stack _"overflows"_ its capacity, causing the program to fail.

However, an old trick that has been employed for a long time in programming languages is called _tail-call optimization_, and it allows you to optimize recursive calls that are in a _"tail position"_; that is, a position where the result of the call can just be returned immediately, instead of needing any further processing.

Our example `factorial'` function has its recursive call in the _tail position_ (and thus, can benefit from that optimization).

This alternative doesn't:

```clojure
(def (factorial' acc n)
  (-> Nat Nat Nat)
  (if (n.= 0 n)
    acc
    (n.+ 0 (factorial' (n.* n acc) (-- n)))))
```

Can you spot the difference?

In the alternative, the result of the recursive call would have to be sent to the `n.+` function as its second parameter, so it wouldn't be a _tail-call_.

The beautiful thing about tail-call optimization (or _TCO_) is that it removes the recursion altogether, eliminating the possibility for stack overflows.

Pretty neat, huh?

	For the sake of correctness, I must point out that Lux doesn't implement _full_ tail-call optimization, since that would require some extra power that Lux can't implement, since it's too low-level and (currently) the JVM doesn't offer the means to achieve that.

	For that reason, going forward, I will refer to what Lux does as _tail-recursion optimization_ (or _TRO_), instead.

### Loop

Some of you may be more familiar with the _for loops_ or the _while loops_ of other programming languages and need some time to wrap your heads around recursion. That's OK.

Lux also offers a macro that gives you a slightly similar experience to those kinds of loops, which can get your mind off recursion just a little bit.

To see it in action, let's rewrite (once more!) our `factorial` function:

```clojure
(def (factorial n)
  (-> Nat Nat)
  (loop (again [acc 1
                n n])
    (if (n.= +0 n)
      acc
      (again (n.* n acc) (-- n)))))
```

We have eliminated our dependency on the `factorial'` function.

Just like with `let`, we're creating some local variables, but these are going to change on each iteration.

Then, in the body, we perform the usual `if` test, and if the number is not `0`, then I use the `again` operator (which only works inside of loop) to update the values of my variables for the next iteration.

## Piping

Piping isn't really control-flow per se, but I include it here because it is a powerful technique for organizing code that involves taking multiple steps to perform a calculation.

It's based on using a single macro, called `|>`, which allows you to write deeply nested code in a flat way, making it much easier to read and understand, and also giving it a bit of an imperative flavor.

Here is a simple example to see how it works:

```clojure
(|> elems
    (each to_text)
    (interposed " ")
    (mix append_text ""))

... =>
... (mix append_text ""
...      (interposed " "
...                  (each to_text elems)))
```

If you read carefully, you'll see that each element (from left to right) gets lodged at the end of the next expression and the pattern continues until everything has been nested.

A good convention to follow in functional programming (and especially in Lux), is that the most important argument to a function (or its _subject_) ought to be the last argument the function takes.

One of the really cool benefits of this convention is that your code becomes very amenable to piping, as the nesting is only done in one way.

It's not hard to see how much easier to read and understand the piped version is, compared to the resulting code.

Also, you might be interested to know that piping can also be extended in really cool ways (similarly to how pattern-matching can be extended).

The way is to use **piping macros** (you may be noticing a theme here).

If you want to know more about those, feel free to check out [Appendix D](appendix_d.md), where I review them in detail.

Oh, and before I forget, there is also a macro for doing reverse piping (which can be very useful in some situations).

Our previous example would look like this:

```clojure
(<| (mix append_text "")
    (interposed " ")
    (each to_text)
    elems)
```

## Higher-Order Functions

I can't finish this chapter without talking about one of the coolest features in the world of functional programming.

So far, we have seen several control-flow mechanisms that could potentially exist in any language/paradigm, but now we'll talk about something native to the FP landscape.

You already know that in the world of functional programming, functions are _first-class values_.

That just means functions can be treated like other values (such as `Int`s, `Bit`s and `Text`s).

You can create new functions at run-time, you can pass functions around as arguments to other functions and you can combine functions in arbitrary ways.

Well, we haven't really seen that in action yet.

It's time to put that theory into practice... with an example:

```clojure
(def (iterate_list f list)
  (All (_ a b) (-> (-> a b) (List a) (List b)))
  (when list
    {.#End}
    {.#End}

    {.#Item head tail}
    {.#Item (f head) (iterate_list f tail)}))
```

This is a function that allows you to transform lists in arbitrary ways.

However, you may notice that we're seeing many new things.

For instance, what is that `All` thingie over there, and what does it do? Is it even a type?

Well, it's not _technically_ a type.

It's actually a macro for creating types (in the same way that `->` is a macro that creates types).

The difference is that `All` allows you to create _universally-quantified types_.

That's just a fancy way of saying that your types are not fixed to working in a particular way, but are flexible enough to allow some variability.

Here, it's being used to make a function that can takes lists with elements of any type (denoted by the _type variable_ `a`), and can produce lists with elements of any other type (denoted by the _type variable_ `b`), so long as you give it a function `f`, that transforms values of type `a` into values of type `b`.

That... is... _mind-blowing_!

In other programming languages, whenever you want to process the elements of a sequence (say, an array) you have to write something like a _for loop_ with some index variable, some condition... and then the code for actually _working with the data_.

But here, we're pretty much defining a function that takes care of all the ceremony, so you just need to give it the operation you wish to perform on each element, and the data.

You could use it like this:

```clojure
(iterate_list (n.* 5) (list 0 1 2 3 4 5 6 7 8 9))

... => (list 0 5 10 15 20 25 30 35 40 45)
```

Pretty cool, huh!

But this is just scratching the surface of what's possible.

As it turns out, higher-order functions (that is, functions which take or produce other functions) are at the foundation of many advanced techniques in functional programming.

Mastering this little trick will prove invaluable to you as you delve deeper into the mysteries of functional programming.

---

Alright.

We've seen quite a lot so far.

But, don't get complacent!

You've only seen the basics of Lux and the next chapters are going to expose some of the more advanced features the language.

Brace yourself, great power is coming!

See you in [the next chapter](chapter_6.md)!

