# Appendix B: Math in Lux

Math in Lux is a bit different from what you might be used to in other languages.

For starters, Lux is a _lisp_, which means that it uses prefix syntax for everything.

That means, familiar operations such as `3 + 5` or `8 = 8` get written as `(+ 3 5)` and `(= 8 8)`.

There's also the issue of different operators for different types.

Whereas other programming languages often overload the math operators `+`, `-`, `>`, etc. for all numeric (and some non-numeric) types, Lux actual offers different versions for different types (defined in the respective modules for those types).

The differences may look trivial, but since the numeric types are treated differently in Lux, you must be aware of which function-set you're using when working with your data.

However, this is not the biggest difference in Lux's math operators in comparison to other languages.
The difference that takes the crown is the ordering of the arguments.

What do I mean?

In most languages you'd write `4 - 7`.
In other lisps you'd write `(- 4 7)`.
But in Lux, you'd write `(- 7 4)`.

_What is going on!? This is so bizarre!_

Calm down.
Everything is going to be fine.

What's going on is that in functional programming, there is this convention of putting the most significant argument to a function as the last one.
In the case of math functions, this would be the argument on which you're operating. I call it the _"subject"_ of the function.

In the case of the subtraction operation, it would be the 4, since you're subtracting 7 from it.

In most lisps, the order of the arguments is such that the subject is the first argument; but not so in Lux.

Now, you may be wondering: _what could possibly be the benefit of making this bizarre change?_

Piping. Piping convinced me to make this change.

You see; this might look very impractical to those accustomed to the old way, but when you're writing complex calculations with many levels of nesting, being able to pipe your operations helps a lot, and this style of doing math lends itself perfectly for it.

Consider this:

```
(|> x (/ scale) (pow 3.0) (- shift))
```

If I was using the traditional way of doing math, I wouldn't be able to pipe it, and it would look like this:

```
(- (pow (/ x scale)
        3.0)
   shift)
```

	`pow` is the _power_ function, located in `library/lux/math`.

You can complain all you want about me breaking with tradition, but that just looks ugly.

So, I'm just going to say it: I broke with tradition because tradition wasn't helping, and I'm not one to just comply because I'm supposed to.

However, I'm not without a heart; and I know that a lot of people would prefer to have a more... traditional way of doing math.

So, for you guys, I've introduced a special macro in the `library/lux/math/infix` module.

It's called `infix`, and it allows you to do infix math, with nested expressions.

Here's an example:

```
(infix [[3.0 pow 2.0] + [5.0 * 8.0]])
```

So, that corresponds to `3^2 + 5*8`.

Note that `infix` doesn't enforce any grouping rules, since you can actually use it with arbitrary functions that you import or define (even with partially applied functions).

The rule is simple, the argument to the right of the operator will be taken first, and then the argument to the left.

So `[3.0 pow 2.0]` becomes `(pow 2.0 3.0)`, and `[5.0 * 8.0]` becomes `(* 8.0 5.0)`.
Thus, the infix syntax is transformed into Lux's prefix variation.

---

I know that Lux's way of doing math is a bit... foreign; but I made that change to ensure math fit the rest of the language perfectly.

Hopefully you'll come to see that getting used to the new way is a piece of cake and has its advantages as soon as you write complex calculations.

