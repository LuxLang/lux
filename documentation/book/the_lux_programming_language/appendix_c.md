# Appendix C: Pattern-matching macros

Pattern-matching is a native Lux feature, and yet `when` is a macro.

_Why?_, you may wonder. _What does being a macro add to the mix?_

Well, as it turns out, by making `when` be a macro, Lux can perform some compile-time calculations which ultimately enable a set of really cool features to be implemented: custom pattern-matching.

Most languages with pattern-matching have a fixed set of rules and patterns for how everything works.

Not so with Lux.

Lux provides a set of default mechanisms, but by using macros where patterns are located, `when` can expand those macro calls to get the myriad benefits they offer.

But enough chit-chat.

Let's see them in action.

  **Note**: The following examples assume you `.require` the `["^" library/lux/meta/macro/pattern]` module.

## Pattern-matching macros in the Standard Library

```clojure
(when (list 1 2 3)
  (list x y z)
  {.#Some (+ x (* y z))}

  _
  {.#None})
```

You may remember how annoying it was to pattern-match against lists in the [Chapter 5](chapter_5.md) example.

Well, `when` will expand any **normal** macros inside your patterns before proceeding to pattern-match against the input data.

```clojure
... Multi-level pattern matching.
... Useful in situations where the result of a branch depends on further refinements on the values being matched.
... For example:
(when (split (size static) uri)
  (^.multi {.#Some [chunk uri']}
           [(text::= static chunk) #1])
  (match_uri endpoint? parts' uri')

  _
  {.#Left (format "Static part " (%.text static) " doesn't match URI: " uri)})

... Short-cuts can be taken when using boolean tests.
... The example above can be rewritten as...
(when (split (size static) uri)
  (^.multi {.#Some [chunk uri']}
           (text::= static chunk))
  (match_uri endpoint? parts' uri')

  _
  {.#Left (format "Static part " (%.text static) " doesn't match URI: " uri}))
```

I **love** `multi`.

It's one of those features you don't need often, but when you do, it saves the day.

The possibilities are endless when it comes to the refinement you can do, and when you consider what you'd have to do to get the same results without it, it's easy to see how much code it saves you.

```clojure
... Allows you to simultaneously bind and de-structure a value.
(def (hash (^.let set [element_hash _]))
  (list#mix (function (_ elem acc)
              (n.+ (# element_hash hash elem) acc))
            0
            (set.list set)))
```

`let` is for when you want to deal with a value both as a whole and in parts.

```clojure
... Same as the "use" macro, but meant to be used as a pattern-matching macro for generating local bindings.
... Can optionally take an aliasing text for the generated local bindings.
(def .public (range (.open "[0]") from to)
  (All (_ a) (-> (Enum a) a a (List a)))
  (range' <= succ from to))
```

`.open` allows you to open structures using local variables during pattern-matching.

It's excellent when taking structures as function arguments, or when opening structures locally in `let` expressions.

```clojure
... Or-patterns.
(type: .public Day
  (Variant
   {#Sunday}
   {#Monday}
   {#Tuesday}
   {#Wednesday}
   {#Thursday}
   {#Friday}
   {#Saturday}))

(def (weekend? day)
  (-> Day Bit)
  (when day
    (^.or {#Saturday} {#Sunday})
    true

    _
    false))
```

`or` patterns allow you to have multiple patterns with the same branch.

It's a real time-saver.

```clojure
... It's similar to do-template, but meant to be used during pattern-matching.
(def (beta_reduce env type)
  (-> (List Type) Type Type)
  (when type
    {.#Primitive name params}
    {.#Primitive name (list#map (beta_reduce env) params)}

    (^.with_template [<tag>]
     [{<tag> left right}
      {<tag> (beta_reduce env left) (beta_reduce env right)}])
    ([.#Sum]
     [.#Product]
     [.#Function]
     [.#Apply])

    (^.with_template [<tag>]
     [{<tag> old_env def}
      (when old_env
        {.#End}
        {<tag> env def}
 
        _
        type)])
    ([.#UnivQ]
     [.#ExQ])

    {.#Parameter idx}
    (maybe.else type (list.item idx env))

    {.#Named name type}
    (beta_reduce env type)

    _
    type))
```

`with_template` is `or`'s big brother.

Whereas `or` demands that you provide the exact patterns you want (and with a single branch body), `with_template` lets you provide templates for both the patterns and bodies, and then fills the blanks with all the given parameters.

You can save yourself quite a lot of typing (and debugging) by reusing a lot of pattern-matching code with this macro.

It's a great asset!

```clojure
... Allows destructuring of streams in pattern-matching expressions.
... Caveat emptor: Only use it for destructuring, and not for testing values within the streams.
(let [(stream.pattern x y z _tail) (some_stream_function 1 2 3)]
  (func x y z))
```

`stream.pattern` hails from the `library/lux/data/collection/stream` module, and it's quite special, because it allows you to de-structure something you normally wouldn't be able to: functions.

You see, Lux streams (as defined in `library/lux/data/collection/stream`) are implemented using functions.

The reason is that they are infinite in scope, and having an infinite data-structure would be... well... impossible (_unless you manage to steal a computer with infinite RAM from space aliens_).

Well, no biggie!

`stream.pattern` does some black magic to make sure you can de-structure your stream just like any other data-structure.

## How to make your own

The technique is very simple.

Just create a normal macro that will take as a first argument a form containing all the _parameters_ to your pattern-matching macro.

Its second argument will be the body associated with the pattern.

After that, you'll receive all the branches (pattern + body) following the call to PM-macro.

You can process all your inputs as you wish, but in the end you must produce an even number of outputs (even, because the outputs must take the form of pattern+body pairs).

These will be further macro-expanded until all macros have been dealt with and only primitive patterns remain, so `when` can just go ahead and do normal pattern-matching.

You may wonder: _why do I receive the body?_

The answer is simple: depending on your macro, you may be trying to provide some advance functionality that requires altering (or replicating) the code of the body.

For example, `^.or` copies the body for every alternative pattern you give it, and `^.with_template` must both copy and customize the bodies (and patterns) according to the parameters you give it.

But receiving the next set of branches takes the cake for the weirdest input.

_What gives?_

It's simple: some macros are so advanced that they require altering not just their bodies, but anything that comes later.

A great example of that is the `^.multi` macro.

`^.multi` performs some large-scale transformations on your code which require getting access to the rest of the code after a given usage of `^.multi`.

However, most of the time, you'll just return the branches (and sometimes the body) unchanged.

To make things easier to understand, here is the implementation of the `or` macro, from the `library/lux/meta/macro/pattern` module:

```clojure
(def .public or
  (pattern
   (macro (_ tokens)
     (when tokens
       (list.partial [_ {.#Form patterns}] body branches)
       (when patterns
         {.#End}
         (///.failure (..wrong_syntax_error (symbol ..or)))

         _
         (.let [pairs (.|> patterns
                           (list#each (function (_ pattern) (list pattern body)))
                           list#conjoint)]
           (///#in (list#composite pairs branches))))
       _
       (///.failure (..wrong_syntax_error (symbol ..or)))))))
```

