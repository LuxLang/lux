# Chapter 15: Persistent data structures

_Where you will learn a new way to organize your data._

---

So far, you know how to use variants, tuples and records to structure your data.

You're also familiar with lists, and you might even have come up with clever ways to use lists to implement key-value structures, like _property lists_.

But the reality is that there are many different ways to organize data, and how you implement the mechanisms to do so will have an impact on the performance of your programs.

That is why there are so many different types of data-structures, and so many different implementations for them.

But not all such implementations fit into the functional paradigm of keeping all your data immutable, and most implementations of data-structures are actually mutable, and meant for imperative programming.

Now, let's not be naïve.
Everybody can figure out that making a data-structure immutable and just copying the whole thing every time you want to make a change would make those data-structures prohibitively expensive to use.

Luckily for us, there is a way to have immutable data-structures that have _reasonable performance_.
The reason why they are _fast enough_ to be used, is that they are designed to re-use as many nodes as they can whenever an update needs to be done, in order to avoid wasteful re-work wherever possible.

Make no mistake, they are still not as fast as their mutable counterparts (which you can still access by doing host-interop), but they are designed with high-performance in mind, and so they tend to be _fast enough_ for most use-cases.

Lux offers a variety of these persistent data-structures.
Here are some examples:

## Rows

	Located in `library/lux/data/collection/row`.

These are similar to lists in that they are sequential data-structures, but there are a few differences:

1. Whereas lists prepend values to the front, rows append them to the back.
2. Random access on lists has a complexity of O(N), whereas it's O(log N) for rows.

Rows are a great substitute for lists whenever random access is a must, and their implementation ensures updates are as cheap as possible.

## Queues

	Located in `library/lux/data/collection/queue`.

Queues are the classic first-in first-out (FIFO) data-structure.

Use them whenever processing order matters, but you need to add to the back (unlike lists, where order matters but add to the front).

## Dictionaries

	Located in `library/lux/data/collection/dictionary`.

This is your standard key-value data-structure.

Known by other names (tables, maps, etc), dictionaries give you efficient access and updating functionality.

All you need to do is give it a `Hash` instance (from `library/lux/abstract/hash`) for your _"key"_ type, and you're good to go.

## Sets

	Located in `library/lux/data/collection/set`.

This is similar to dictionaries in that a `Hash` implementation is needed, but instead of it being a key-value data-structure, it only stores values (and then tells you if any given value is a member of the set).

This is a useful data-structure for modelling group membership and keeping track of things. Plus, there are several set-theoretic operations defined in that module.

## Persistent data structures and Software Transactional Memory

This is a killer combination.

Instead of using mutable data-structures for your changing program data, you can just use persistent data-structures, with the mutability being delegated the the STM system.

This will make concurrently working with these data-structures a piece of cake, since you never have to worry about synchronizing/locking anything to avoid simultaneous updating, or any of the other crazy things programmers have to do to avoid data corruption.

## Arrays: the not-so-persistent data structures

	Located in `library/lux/data/collection/array`.

The `library/lux/data/collection/array` module features mutable arrays you can use if you need fast access and mutation and are willing to run the risks involved with using mutable data.

Another possible use is to implement other data-structures (and, as it turns out, rows, dictionaries and sets all rely on arrays in their implementations).

Also, it's worth nothing that in the _JVM_, this corresponds to _object arrays_.
If you want primitive arrays, you should check out the functionality for that in `library/lux/ffi`.

---

It may seem odd that this chapter doesn't feature any code samples, but most of what you need to know is already located in the standard library documentation.

These data-structures are very easy to use and offer decent performance, so you're encouraged to use them to model all your data processing code.

The next chapter is going to be slightly different, in that we're going to be learning not how to write programs, but how to test them.

See you in the next chapter!

