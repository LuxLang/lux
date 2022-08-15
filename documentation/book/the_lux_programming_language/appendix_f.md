# Appendix F: Implicit polymorphism

If you've used Lux's interfaces and implementations already (with the `at` macro), you've probably noticed that you need to pass around the specific implementations you need every time you want to call some interface's method.

That can become tiresome if you need to do it all the time, and specially if you come from languages that do method-selection for you automatically.

Object-oriented languages do polymorphism in an easy way, because they link objects to the method table of their associated classes, and when you call a method on an object, the run-time system can figure out where the code that needs to be run lies within the program's memory.

Languages with type-classes, such as Haskell, perform that look-up at compile-time, by using the type-information present in the compilation context to figure out which implementation (or _instance_) of a type-class is suitable to each particular circumstance.

Lux, on the other hand, forces you to be specific about the implementations that you're going to use.

While that gives you a level of power and flexibility you wouldn't otherwise have in other languages, it also introduces the problem that when what you want doesn't warrant that level of power, you have to pay the tax it involves nonetheless.

But, that sounds like a raw deal.

Why do you have to pay for something you're not taking advantage of?

Clearly, there is an asymmetry here.

It is a feature that is most useful in the few instances when you want full power.

At any other point, it's a hindrance.

Well... there is an alternative.

The Lux Standard Library includes a module called `library/lux/type/implicit`, which provides a macro called `a/an` (or, alternatively, `a` or `an`), that serves as an easier-to-use alternative to the `at` macro.

What it does is that instead of requiring the implementation you want to use, it only requires the name of the method you want to call and the arguments.

Then, at compile-time, it does some type-checking and some look-ups and selects an implementation for you that will satisfy those requirements.

That implementation can come from the local-var environment, from the definitions in your own module, or even from the exported definitions of the modules you're importing.

That way, you can use `at` whenever you need precision and power, and use `a/an` whenever you're doing more lightweight programming.

Fantastic!

This is how you'd use it:

```clojure
... Equality for nats
(at nat.equivalence = x y)
... vs
(an = x y)
```

```clojure
... Equality for lists of nats
(at (list.equivalence nat.equivalence) =
    (list.indices 10)
    (list.indices 10))
... vs
(an = (list.indices 10) (list.indices 10))
```

```clojure
... Functor mapping
(at list.functor each ++ (list.indices 10))
... vs
(an each ++ (list.indices 10))
```

---

Thanks to implicit polymorphism, you don't have to choose between power and ease of use.

Just do a static-import of the `library/lux/type/implicit` module, and you'll get the `a/an` available and ready for action.

