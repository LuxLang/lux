# Appendix D: The art of piping

I'm a big fan of piping.

No kidding.

Piping is my favorite way of writing code.

It's almost like a game to me.

I try to figure out ways to get my code to be more pipe-sensitive, to see how far I can get while piping my code.

	My personal record is 14 steps.

## Piping macros in the standard library

Anyhow, after looking at some of the innovations in Clojure on the piping department, I decided to come up with my own tricks to try to get Lux to become a piping superpower.

I added the `library/lux/control/pipe` module, which contains several macros meant to be used within the `|>` macro, and which extend it with awesome capabilities.

Take a look at these babies:

```clojure
... Loops for pipes.
... Both the testing and calculating steps are pipes and must be given inside tuples.
(|> 1
    (loop> [(n.< 10)]
           [++]))
```

`loop>` takes a test _tuple_ and a body _tuple_.

The reason is that each of those tuples represents the steps on an implicit piping macro (_oh, yeah!_).

So `[(n.< 10)]` is like `(|> value (n.< 10))`, and `[++]` is like `(|> value ++)`.

Which value? Whatever has been piped into `loop>` from the underlying `|>` (in this case, the value `1`).

---

```clojure
... Branching for pipes.
... Both the tests and the bodies are piped-code, and must be given inside a tuple.
... If a last else-pipe isn't given, the piped-argument will be used instead.
(|> 5
    (cond> [i.even?] [(i.* 2)]
           [i.odd?]  [(i.* 3)]
           ... else branch
           [(new> -1 [])]))
```

We have looping, and now we have branching; with a `cond`-inspired piping macro (complete with _else_ branch, just in case).

But what's that thing over there? That `new>` thing?

Well, it's another piping macro. Of course!

```clojure
... Ignores the piped argument, and begins a new pipe.
(|> 20
    (i.* 3)
    (i.+ 4)
    (new> 0 [++]))
```

`new>` establishes a new piping sequence that ignores any previous one.

Useful in certain kinds of situations.

---

```clojure
... Gives a name to the piped-argument, within the given expression.
(|> 5
    (let> @ (+ @ @)))
```

`let>` binds the current value piped into it so you can refer to it multiple times within it's body.

Pretty nifty, huh?

---

```clojure
... Pattern-matching for pipes.
... The bodies of each branch are NOT pipes; just regular values.
(|> 5
    (case> 0 "zero"
           1 "one"
           2 "two"
           3 "three"
           4 "four"
           5 "five"
           6 "six"
           7 "seven"
           8 "eight"
           9 "nine"
           _ "???"))
```

Yeah, that's right!

I just couldn't resist rolling full-blown pattern-matching into this.

You'll thank me later.

---

```clojure
... Monadic pipes.
... Each steps in the monadic computation is a pipe and must be given inside a tuple.
(|> 5
    (do> identity.monad
         [(i.* 3)]
         [(i.+ 4)]
         [+]))
```

And just to show you I'm serious, I did the unthinkable.

Piped macro expressions!

## How to make your own piping macros

They're easier to make than pattern-matching macros.

All you need is a macro that takes anything you want as parameters, but always takes as its last argument _the computation so far_, as it has been constructed by the `|>` macro prior to the call to your piping macro.

As an example, here's the definition for `let>`:

```clojure
(syntax: .public (let> [binding <code>.any
                        body <code>.any
                        prev <code>.any])
  (in (list (` (let [(~ binding) (~ prev)]
                 (~ body))))))
```

---

All this looks like madness, but I just couldn't contain myself.

Piping is one of the few ways of writing code that just amuses me whenever I do it.

These macros can keep you in the flow while you're writing complex code, so you don't have to switch so much between piping-code and non-piping-code.

Oh... and did I mention the `|>>` macro?

It generates for you a single-argument function that will immediately pipe its argument through all the steps you give it?

```clojure
(only (|>> (member? forbidden-definitions)
           not)
      all_definitions)
```

