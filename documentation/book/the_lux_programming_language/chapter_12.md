# Chapter 12: I/O

_Where you will learn how to interact with the outside world._

---

_I/O_ (short for _input and output_) is a very important subject in programming.

Arguably, it's the only reason why we make programs in the first place.

Software wouldn't be very useful if we couldn't use files, or interact with foreign devices (like sensors, or other computers), or interact with our users through GUIs.

_I/O_ is fundamental to software development; but, like every fundamental thing in programming, there are many approaches to it, and some of them are in opposition to others.

I would say there are 2 main schools of thought when it comes to the role of _I/O_ in computer programs.

## Implicit _I/O_

These guys say that all programs are fundamentally about the _I/O_.

Everything, in one way or another, revolves around _I/O_, and it is a pervasive force that defines what computing is about from its very foundations.

Most programming languages you may be familiar with subscribe to this idea (either by choice of the language designer(s) or by simply following standard practice).

Here, we see operations which have side-effects (such as printing to standard output, or reading files, or connecting to another computer over the network) be treated like operations which lack them (like adding two numbers), with the side-effects being seen as some kind of magical property exhibited by those operations, which neither the language nor its libraries are obligated to handle or track in any special way.

Side-effects tend to be mentioned in documentation, but the lack of separation means that programmers must be wary of what they're using if the want to avoid the unfortunate consequences of careless coding.

	A common pitfall happens to involve concurrency, where operations being executed on multiple threads, and having access to common resources, can cause data-corruption and other problems during run-time.

However, programmers who subscribe to this philosophy don't tend to see that as a sign that their languages or tools lack anything, but that programmers need to be careful and mindful of what they code, and that the effectful nature of programming is something to be embraced, rather than constrained.

## Explicit _I/O_

These guys are a very different breed.

Just like static typing is used to impose some order in programs and to make explicit that which only lived in the minds (and comments) of programmers; _explicit I/O_ acknowledges that _effectful computations_ are fundamentally different from _pure computations_ (that is, computations which just process their inputs and calculate their outputs).

Making _I/O_ explicit, rather than being a denial of the very nature of programs, is an acknowledgement of this reality, and a reification of something which only used to live in the minds of programmers (and their comments).

By making I/O explicit, it can be separated from pure computations (thus, avoiding many common mistakes); it can be isolated and handled in safe places (thus avoiding its often pervasive nature); and it can be better integrated into the design of programs by making it a more concrete element.

In a way, it can even be said that immutable data-types (a staple of functional programming that Lux embraces) are just another form of explicit _I/O_.

Being able to change data after it was created can easily propagate changes beyond your local scope and affect threads and data-structures you aren't even aware of in ways that may be troublesome.

But, just like having immutable data-types makes change explicit and a concrete part of one's design, making _I/O_ explicit achieves a similar goal, with regards to one's interactions with the _outside world_.

While immutable data-types deal with the mechanics of the _inner world_ of programs, explicit _I/O_ deals with interaction with the _outside world_; from the very near to the very far.

Lux chooses explicit _I/O_ as its underlying model.

	It may seem odd that I have to justify the choice of explicit _I/O_ in Lux; but, at the time of this writing, implicit _I/O_ is the industry standard, with many even doubting the benefits of an alternative approach.

	The only _major_ language which also adopts this model is Haskell (from which Lux takes _much inspiration_), in its efforts to maintain theoretical purity.

## How does explicit _I/O_ work?

Now that I have (hopefully) convinced you that this is a good idea, how does it work?

Well, by using types; of course!

If you head to the `library/lux/control/io` module, you will find the following type definition:

```clojure
(abstract: .public (IO a)
  ...
  )
```

`IO` is a type that embodies this model, by making it something that the type-system can recognize and work with.

If you explore the `library/lux/control/io` module, you'll find the means to `run!` (or _execute_) these `IO` operations.

The `io` macro, on the other hand, gives you the way to _label_ effectful code as such, by wrapping it inside the `IO` type.

	However, it's unlikely you'll need to use the `io` macro yourself very often, as some tools we'll see later can take care of that for you.

## What `IO` operations are available?

Sadly, this is one area where Lux (currently) falls short.

There are 2 reasons for that:

1. Lux's cross-platform ideals.
2. Lux's youth.

Different platforms (e.g. JVM, Node.js, web browsers, CLR) offer different tools for doing _I/O_, which makes providing cross-platform _I/O_ functionality a bit of a challenge.

As Lux grows (as a language and as a community), many of the gaps regarding _I/O_ will be filled and there will (hopefully) be some sort of standard _I/O_ infrastructure that will be relatively platform-independent.

In the meantime, _I/O_ will depend on the capabilities of the host platform, accessed through the mechanisms Lux provides for doing host inter-operation (which is, coincidentally, the subject of the next chapter).

This is not as bad as it sounds; and one way or another, programmers will always want to reach to their host platforms to access features beyond what Lux can provide.

And this is fine.

Rather than trying to be what everybody needs and wants all the time, Lux chooses to focus on the subset of things it can do _really_ well, and leaves the rest on the hands of the platform, and the clever programmers who use both.

	But, I must confess, there is _one_ way of doing _I/O_ Lux **does provide** in a cross-platform way, which is the `library/lux/debug.log!` function (which prints to standard output).

	However, `library/lux/debug.log!` has type `(-> Text Any)`, instead of the expected `(-> Text (IO Any))`. The reason is that it was meant for casual logging (as done while debugging), instead of serious _I/O_, and I felt that forcing people to use `IO` while logging would have made the process too tedious.

---

This chapter has been mostly theoretical, as I had to make the case for why explicit _I/O_ is a valuable tool before I could actually explain what it was.

Here, you have learned how to walk outside and yell.

Next, I shall teach you how to walk inside and whisper... to the host platform.

See you in [the next chapter](chapter_13.md)!

