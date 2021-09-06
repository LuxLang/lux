# Chapter 14: Concurrency

_Where you will harness the power of modern computing._

---

Concurrency is one of the most important subjects in modern programming because of the pressing need to improve the efficiency of programs; coupled with the troublesome limitation that increasing the speed of individual CPUs is becoming harder and harder.

The solution, at the moment, is to increase the number of CPUs in the machine to allow programmers to run their code in parallel.

The problem is that new models of computation which take concurrency into account have had to be developed to address this need, but nobody knows yet which of the many alternatives is _the right one_, or if there is even a right one at all.

Until now, most programmers just thought about their programs in a purely sequential manner; and, as we explore some of these concurrency models, you'll notice that they try to restore some of this feeling, while at the same time _scheduling_ work to happen concurrently, without (almost) any programmer involvement.

Lux takes the approach of providing the means to use multiple concurrency models, since I couldn't decide on which was _the right one_.

Some languages (like _Erlang_ and _Go_) choose to commit themselves to one model or another, but I'm not confident that the software industry (as a whole) is experienced enough with concurrency as to declare any one model the winner.

The result: more variety.

And watch out, because the amount of concurrency models may increase with future Lux releases.

Anyhow, let's quit the chit-chat and dive in!

## Asynchronous computation

This is my favorite one, because it can be used for almost anything, whereas I see the other modules as more specialized tools for certain use cases.

This model is based on concepts you may be familiar with: _futures_ and _promises_.

**Futures** are basically concurrent and asynchronous computations which run and yield a value, which you can later access.

**Promises** are more like storage locations (which may be set only once), which you can use for communication by setting their values in one process, and reading it from another.

Some languages offer both (often used in conjunction), while others only offer one (while also kind of giving it properties of the other).

I pretty much came to the conclusion that, for all intents and purposes, their similarities were much greater than their differences.

So, I just fused them.

And so, Lux implements promises in the `library/lux/control/concurrency/async` module, by means of the `Async` type.

You can run `IO` computations concurrently using the `future` function (which returns an `Async` that will contain the result of the computation).

You can also bind functions to `Async` values in order to be notified when they have been _resolved_ (the term for when the value of the `Async` is set).

By means of this ability to watch `Async` values, it's possible to implement `Functor` and `Monad` for `Async`, which is precisely what is done in the standard library.

The result is that, through the `do` macro, you can implement complex concurrent and asynchronous computations that look and feel just like synchronous ones.

If you're curious about how that looks, take a peek:

```clojure
(def: .public (and left right)
  (All [a b] (-> (Async a) (Async b) (Async [a b])))
  (do monad
    [a left
     b right]
    (in [a b])))
```

	Oh, and did I mention there are _combinators_ in that module?

If you didn't know there was some magic going on in the `Async` type, you wouldn't have suspected this was concurrent code. It looks just like any other old synchronous code you might have use with any other monad.

Pretty neat, huh?

## Functional Reactive Programming

FRP is based on the idea of _values that change over time_, and structuring your programs to dynamically respond to those changes in a **reactive** way.

The way its implemented in Lux is through the `Channel` type in `library/lux/control/concurrency/frp` (itself implemented on top of `Async`).
`Channel` instances are (potentially infinite) sequences of values that you can process in various ways as new values come into being.
`Channel` instances can be closed, but they may also go on forever if you'd like them to.

The `library/lux/control/concurrency/frp` module offers various functions for processing channels in various them (some of them generating new channels), and the `Channel` type also happens to be a monad, so you can write fairly complex and powerful code with it.

## Software Transactional Memory

	Implemented in the `library/lux/control/concurrency/stm` module.

STM is quite a different beast from the other 2 approaches, in that they address the problem of how do I propagate information within the system, while STM deals with how to keep data in one place, where it can be accessed and modified concurrently by multiple processes.

It works by having variables which may be read from and written to, but only within _transactions_, which could be seen as descriptions of changes to be made to one (or more) variables in an atomic, consistent and isolated way.

Let's break down those last 3 terms:

* **Atomic**: This just means that if more than one change needs to be made in a transaction, either all gets done, or none. There is no room for partial results.
* **Consistent**: This just means that transactional computations will take the set of variables they operate from one valid state to another. This is largely a consecuence of transactions being atomic.
* **Isolated**: This means that transactions run in isolation (or, without interference) from one another, thereby ensuring no transaction may see or modify any in-trasaction value being computed somewhere else, and they all get the _impression_ that they are the only transaction running at any given time.

For those of you familiar with relational databases, this might remind you of their _ACID_ properties (with the caveat that Lux's STM is non-durable, as it works entirely in memory).

The way it works is by running multiple transactions concurrently, and then committing their results to the affected variables.
If 2 transactions modify any common variables, the first one to commit wins, and the second one would be re-calculated to take into account the changes to those variables.
This implies that transactions are sensitive to some "version" of the variables they involve and that is correct.
That is the mechanism use to avoid collisions and ensure no inconsistencies ever arise.

The relevant types are `Var`, which corresponds to the variables, and `STM` which are computations which transform transactions in some way and yield results.

Like `IO` and unlike `Async`, just writing `STM` computations doesn't actually run them, and you must call the `commit!` function to actually schedule the system to execute them (receiving a `Async` value for the result of the transaction).

You may also `follow!` variables to get `Channel`s of their values if you're interesting in tracking them.

## The Actor Model

The _actor model_ is also very different from the other models in that, while they deal with computations which produce values concurrently, the _actor model_ is all about processes running concurrently and communicating with one another.

You can't run an actor and just wait for it to finish to get the result. For all you know, it may never end and just run forever.

Also, interaction with actors is based on _message-passing_, and an actor may consume an indefinite number of such messages (and send messages to other actors).

The relevant module is the `library/lux/control/concurrency/actor` module, and the relevant type is:

```clojure
(abstract: .public (Actor s)
  ...
  )
```

`Actor`s have mailboxes for receiving messages, to which they react.

It's also possible to kill an actor (although it can also die _naturally_ if it encounters a failure condition during normal execution).

And if it dies, you'll receive its state at the time of death, a list of unconsumed messages from its mailbox and an error message detailing the cause of death.

Just from this definition, it's easy to see that actors are stateful (a necessity for modeling a variety of complex behaviors).

To create an actor, you must first specify its `Behavior`:

```clojure
(type: .public (Behavior o s)
  {#on_init (-> o s)
   #on_mail (-> (Mail s) s (Actor s) (Async (Try s)))})
```

These functions know how to initialize an actor, and how to react to incoming mail.

You can then call the `spawn!` function with an initial state and a `Behavior`.

But writing complex actors with multiple options for its messages can be messy with these tools, so a macro was made to simplify that.

```clojure
... Defines a named actor, with its behavior and internal state.
... Messages for the actor must be defined after the on_mail handler.
(actor: .public (stack a)
  {}

  (List a)

  ((on_mail mail state self)
   (do (try.with async.monad)
     [.let [_ (debug.log! "BEFORE")]
      output (mail state self)
      .let [_ (debug.log! "AFTER")]]
     (in output)))

  (message: .public (push {value a} state self)
    Nat
    (let [state' (#.Item value state)]
      (async.resolved (#try.Success [state' (list.size state')])))))

(actor: .public counter
  {}

  Nat

  (message: .public (count! {increment Nat} state self)
    Any
    (let [state' (n.+ increment state)]
      (async.resolved (#try.Success [state' []]))))

  (message: .public (read! state self)
    Nat
    (async.resolved (#try.Success [state state]))))
```

For every method you define, a function will be defined in your module with the same name, and taking the same arguments, plus the actor.
That function will always take the actor itself as its last argument, and will return an `Async` of the return type.

You can either die with a `#librarylux/control/try.Failure` value, or continue on to the next message with a `#librarylux/control/try.Success` containing an updated _actor state_, and a _return value_ for the method.
The type of the return value must match the type following the method signature.

---

In this chapter, you have learned how to use the many tools Lux offers to tap into the multi-processor power of modern computing systems.

But if you think about it, being able to hold onto values or pass them around concurrently is rather useless unless you have some important and complex data to move around in the first place; and so far we have only dealt with fairly simple data-structures.

Well, read the next chapter if you want to learn how to take your data to the next level with the help of persistent data structures.

See you in [the next chapter](chapter_15.md)!

