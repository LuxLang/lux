# Chapter 8: Functors and Monads

_Where I will try to explain something really confusing, and you'll pretend you understand to avoid hurting my feelings._

---

OK. It's time to get serious.

The following topics are known to be troublesome to teach, so I can only promise you that I will try really, really hard not too say something confusing (or stupid).

## Functors

Functors and monads are both mathematical concepts that are prevalent in **Category Theory**. You may have heard of it before. It's a branch of abstract mathematics that many are drawing inspiration from when developing new tools and techniques for functional programming.

But I will not go down the route of explaining things to you from a mathematical perspective... as I'm not confident that's going to work.

---

Imagine that you have some data (maybe an `Int`, or a `Text`). You can work with that data: you can pass it around, apply functions to it, print it to the console/terminal, or pattern-match against it.

Well, imagine a functor as some kind of wrapper on your data. You can still access what's inside, but the wrapper itself offers special superpowers, and each wrapper is different.

For instance, there's one wrapper that allows us to have (or not to have) our data.

Schrodinger's wrapper (although most people prefer to call it `Maybe`).

That wrapper happens to be a type, as **all** functor wrappers are types.

But not just any type. You see, functors have requirements.

```
(interface: #export (Functor f)
  (: (All [a b]
       (-> (-> a b) (f a) (f b)))
     map))
```

This is the `Functor` interface, from `lux/abstract/functor`.

As you can see, it only has a single member: the `map` function.

The parameter type `f` is very special, because instead of being a simple type (like `Int` or `Text`), it's actually a parameterized type (with a single parameter). That explains why it's being used the way it is in the type of `map`.

Not every parameterized type can be a functor, but if the type is something that you can open to work with its inner elements, then it becomes a good candidate.

	And you would be surprised how many things fit that requirement.

Remember that `Maybe` type we talked about? Let's see how it plays with `Functor`.

```
(type: (Maybe a)
  #.None
  (#.Some a))
```

We've seen `Maybe` before, but now we can check out how it's implemented.

	By the way, it lives in the `library/lux` module, so you don't need to import anything.

Here is its `Functor` implementation.

```
(implementation: #export functor
  (Functor Maybe)
  
  (def: (map f ma)
    (case ma
      #.None     #.None
      (#.Some a) (#.Some (f a)))))

## This one lives in the library/lux/data/maybe module, though.
```

We'll know how everything fits if we fill in the blanks for `map`'s type:

```
(All [a b]
  (-> (-> a b) (Maybe a) (Maybe b))
```

So, the job of `map` here is to take a `Maybe` containing some `a` value, and somehow transform it into a `b`, without escaping the `Maybe`.

By looking at the `Functor` implementation, we can see how this works out.

We can actually pattern-match against the entire input and handle the different cases, using the given function to transform our `a` into a `b`.

Not that hard.

Oh, and remember our `iterate_list` function from [chapter 5](chapter_5.md)?

Turns out, that's just the `Functor` implementation from `lux/data/collection/list`:

```
(implementation: #export functor
  (Functor List)
  
  (def: (map f ma)
    (case ma
      #.End          #.End
      (#.Item a ma') (#.Item (f a) (map f ma')))))
```

Not bad.

In the case of `List`, the wrapper superpower it provides is the capacity to handle multiple values as a group. This can be used for some really cool techniques; like implementing non-deterministic computations by treating every list element as a branching value (but let's not go down that rabbit-hole for now).

The power of functors is that they allow you to decorate your types with extra functionality. You can still access the inner data and the `map` function will take advantage of the wrapper's properties to give you that extra power you want.

You can implement things like stateful computations, error-handling, logging, I/O, asynchronous concurrency and many other crazy things with the help of functors.

However, to make them really easy to use, you might want to add some extra functionality.

## Monads

One thing you may have noticed about the `Functor` interface is that you have a way to operate on functorial values, but you don't have any _standardized_ means of creating them.

I mean, you can use the `list` and `list&` macros to create lists and the `#.None` and `#.Some` tags for `Maybe`, but there is no unified way for creating **any** functorial value.

Well, let me introduce you to `Monad`:

```
(interface: #export (Monad m)
  (: (Functor m)
     &functor)
  (: (All [a]
       (-> a (m a)))
     in)
  (: (All [a]
       (-> (m (m a)) (m a)))
     join))
```

This interface extends `Functor` with both the capacity to wrap a normal value `in` functorial structure, and to `join` 2 layers of functorial structure into a single one.

Sweet!

Wrapping makes working with functors so much easier because you don't need to memorize a bunch of tags, macros or functions in order to create the structures that you need.

And being able to join layers of functorial structure allows you to write dynamic computations which make use of the functorial structure and that can depend on the value of previous functorial computations.

To get a taste for it, let's check out another functorial type.

Remember what I said about error-handling?

```
(type: #export (Try a)
  (#Failure Text)
  (#Success a))
```

This type expresses errors as `Text` values (and it lives in the `lux/control/try` module).

Here are the relevant `Functor` and `Monad` implementations:

```
(implementation: #export functor
  (Functor Try)
  
  (def: (map f ma)
    (case ma
      (#Failure msg)
      (#Failure msg)
      
      (#Success datum)
      (#Success (f datum)))))

(implementation: #export monad
  (Monad Try)
  
  (def: &functor ..functor)

  (def: (in a)
    (#Success a))

  (def: (join mma)
    (case mma
      (#Failure msg)
      (#Failure msg)
      
      (#Success ma)
      ma)))
```

If you listen to functional programmers, you'll likely get the impression that the invention of monads rivals the invention of the wheel. It is this incredibly powerful and fundamental abstraction for a lot of functional programs.

The thing about `Monad` is that, with it, you can use `map` functions that also generate wrapped values (and take advantage of their special properties), and then you can collapse/merge/combine those values into a "joined" value by using the `join` function.

Let's see that in action:

```
(.module:
  [library
    [lux #*
      [data
        [collection
          ["." list]]]]])

(open: list.monad)

(def foo
  (|> (list 1 2 3 4)
      (map (list.repeated 3))
      join))

## The value of 'foo' is:
(list 1 1 1 2 2 2 3 3 3 4 4 4)
```

_It's magic!_

Not really. It's just the `Monad` for `List`:

```
(implementation: #export functor
  (Functor List)
  
  (def: (map f ma)
    (case ma
      #.End
      #.End
      
      (#.Item a ma')
      (#.Item (f a) (map f ma')))))

(implementation: #export fold
  (Fold List)
  
  (def: (fold f init xs)
    (case xs
      #.End
      init

      (#.Item x xs')
      (fold f (f x init) xs'))))

(implementation: #export monoid
  (All [a] (Monoid (List a)))
  
  (def: identity #.End)
  (def: (compose xs ys)
    (case xs
      #.End
      ys
      
      (#.Item x xs')
      (#.Item x (compose xs' ys)))))

(open: "." ..monoid)

(implementation: #export monad
  (Monad List)
  
  (def: &functor ..functor)

  (def: (in a)
    (#.Item a #.End))

  (def: (join list_of_lists)
    (|> list_of_lists reversed (fold compose identity))))

## The fold function is for doing incremental iterative computations.
## Here, we're using it to build the total output list by composing/concatenating all the input lists in our `list_of_lists`.
```

`Monad`s are incredibly powerful, since being able to use the special power of our `Functor` while `map`ping functions allows us to layer that power in complex ways.

But... you're probably thinking that writing a bunch of `map`s followed by `join`s is a very tedious process. And, you're right!

If functional programmers had to subject themselves to that kind of tedium all the time, they'd probably not be so excited about monads.

Time for the VIP treatment.

## The `do` Macro

These macros always show up at the right time to saves us from our hurdles!

```
(.module:
  [library
    [lux #*
      [data
        ["." maybe]]]])

## Macro for easy concatenation of monadic operations.
(do maybe.monad
  [x (f0 123)
   #let [y (f1 x)] ## #let enables you to use full-featured let-expressions within do
   z (f2 y)]
  (wrap (f3 z)))
```

The `do` macro allows us to write monadic code with great ease (it's almost as if we're just making `let` bindings).

Just tell it which `Monad` implementation you want, and it will write all the steps in your computation piece by piece using `map` and `join` without you having to waste your time with all the boilerplate.

Finally, whatever you write as the body of the `do`, it must result in a functorial/monadic value (in this case, a `Maybe` value).

**Remember**: `join` may collapse/merge/combine layers of the functor, but it never escapes it, and _neither can you_ (generally-speaking).

---

`Functor`s and `Monad`s have a bad reputation for being difficult to understand, but hopefully I didn't botch this explanation _too much_.

Personally, I think the best way to understand functors and monads is to read different implementations of them for various types (and maybe write a few of your own).

	For that, feel free to peruse the Lux Standard Library at your leisure.

This is the sort of think that you need to learn by intuition and kind of _get the feel for_.

Hopefully, you'll be able to get a feel for them in the next chapters, because we're going to be exploring a lot of monads from here on.

So, buckle-up, cowboy. This ride is about to get bumpy.

See you in the next chapter!

