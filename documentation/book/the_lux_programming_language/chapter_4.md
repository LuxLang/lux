# Chapter 4: Functions and definitions

_Where you will learn how to build your own Lux code._

---

OK, so you've seen several explanations and details so far, but you haven't really seen how to make use of all of this information.

No worries. You're about to find out!

First, let's talk about how to make your own functions.

```clojure
(function (plus_two x) (inc (inc x)))
```

Here's the first example.
This humble function increases a `Nat` twice.

What is it's type?

Well, I'm glad you asked.

```clojure
(: (-> Nat Nat)
   (function (plus_two x) (inc (inc x))))
```

That `->` thingie you see there is a macro for generating function types.
It works like this:

```clojure
(-> arg1 arg2 ... argN return)
```

The types of the arguments and the return type can be any type you want (even other function types, but more on that later).

How do we use our function? Just put it at the beginning for a form:

```clojure
((function (plus_two x) (inc (inc x))) 5)
## => 7
```

Cool, but... inconvenient.

It would be awful to have to use functions that way.

How do we use the `plus_two` function without having to inline its definition (like the `debug.log!` function we used previously)?

Well, we just need to define it!

```clojure
(def: plus_two
  (: (-> Nat Nat)
     (function (_ x)
       (inc (inc x)))))
```

Or, alternatively:

```clojure
(def: plus_two
  (-> Nat Nat)
  (function (_ x)
    (inc (inc x))))
```

Notice how the `def:` macro can take the type of its value before the value itself, so we don't need to wrap it in the type-annotation `:` macro.

Now, we can use the square function more conveniently.

```clojure
(plus_two 7)
## => 9
```

Nice!

Also, I forgot to mention another form of the `def:` macro which is even more convenient:

```clojure
(def: (plus_two x)
  (-> Nat Nat)
  (inc (inc x)))
```

The `def:` macro is very versatile, and it allows us to define constants and functions.

If you omit the type, the compiler will try to infer it for you, and you will get an error if there are any ambiguities.

You will also get an error if you add the types but there's something funny with your code and things don't match up.

Error messages keep improving on each release, but in general you'll be getting the **file, line and column** on which an error occurs, and if it's a type-checking error, you'll usually get the type that was expected and the actual type of the offending expression... in multiple levels, as the type-checker analyses things in several steps. That way, you can figure out what's going on by seeing the more localized error alongside the more general, larger-scope error.

---

Functions, of course, can take more than one argument, and you can even refer to a function within its own body (also known as recursion).

Check this one out:

```clojure
(def: (factorial' acc n)
  (-> Nat Nat Nat)
  (if (n.= 0 n)
    acc
    (factorial' (n.* n acc) (dec n))))

(def: (factorial n)
  (-> Nat Nat)
  (factorial' 1 n))
```

And if we just had the function expression itself, it would look like this:

```clojure
(function (factorial' acc n)
  (if (n.= 0 n)
    acc
    (factorial' (n.* n acc) (dec n))))
```

Here, we're defining the `factorial` function by counting down on the input and multiplying some accumulated value on each step. We're using an intermediary function `factorial'` to have access to an accumulator for keeping the in-transit output value, and we're using an `if` expression (one of the many macros in the `library/lux` module) coupled with a recursive call to iterate until the input is 0 and we can just return the accumulated value.

As it is (hopefully) easy to see, the `if` expression takes a _test_ expression as its first argument, a _"then"_ expression as its second argument, and an _"else"_ expression as its third argument.

Both the `n.=` and the `n.*` functions operate on `Nat`s, and `dec` is a function for decreasing `Nat`s; that is, to subtract 1 from the `Nat`.

You might be wondering what's up with that `n.` prefix.

The reason it exists is that Lux's arithmetic functions are not polymorphic on the numeric types, and so there are similar functions for each type.

If you import the module for `Nat` numbers, like so:

```clojure
(.module
  [library
   [lux
    [math
     [number
      ["n" nat]]]]])
```

You can access all definitions in the "library/lux/math/number/nat" module by just using the "n." prefix.

	I know it looks annoying, but later in the book you'll discover a way to do math on any Lux number without having to worry about types and prefixes.

Also, it might be good to explain that Lux functions can be partially applied. This means that if a function takes N arguments, and you give it M arguments, where M < N, then instead of getting a compilation error, you'll just get a new function that takes the remaining arguments and then runs as expected.

That means, our factorial function could have been implemented like this:

```clojure
(def: factorial
  (-> Nat Nat)
  (factorial' +1))
```

Or, to make it shorter:

```clojure
(def: factorial (factorial' +1))
```

Nice, huh?

---

We've seen how to make our own definitions, which are the fundamental components in Lux programs.

We've also seen how to make functions, which is how you make your programs **do** things.

Next, we'll make things more interesting, with _branching_, _loops_ and _pattern-matching_!

See you in [the next chapter](chapter_5.md)!

