# Appendix C: Pattern-matching macros

Pattern-matching is a native Lux feature, and yet `case` is a macro.

_Why?_, you may wonder. _What does being a macro add to the mix?_

Well, as it turns out, by making `case` be a macro, Lux can perform some compile-time calculations which ultimately enable a set of really cool features to be implemented: custom pattern-matching.

Most languages with pattern-matching have a fixed set of rules and patterns for how everything works.
Not so with Lux.

Lux provides a set of default mechanisms, but by using macros where patterns are located, `case` can expand those macro calls to get the myriad benefits they offer.

But enough chit-chat.
Let's see them in action.

## Pattern-matching macros in the Standard Library

```
(case (list 1 2 3)
  (^ (list x y z))
  (#.Some (+ x (* y z)))

  _
  #.None)
```

You may remember how annoying it was to pattern-match against lists in the [Chapter 5](chapter_5.md) example.

Well, by using the `^` pattern-matching macro, you can use any normal macros you want inside the pattern to profit from their code-construction capacities.

```
... Multi-level pattern matching.
... Useful in situations where the result of a branch depends on further refinements on the values being matched.
... For example:
(case (split (size static) uri)
  (^multi (#.Some [chunk uri'])
          {(text::= static chunk) true})
  (match_uri endpoint? parts' uri')

  _
  (#.Left (format "Static part " (%.text static) " doesn't match URI: " uri)))

... Short-cuts can be taken when using boolean tests.
... The example above can be rewritten as...
(case (split (size static) uri)
  (^multi (#.Some [chunk uri'])
          (text::= static chunk))
  (match_uri endpoint? parts' uri')

  _
  (#.Left (format "Static part " (%.text static) " doesn't match URI: " uri)))
```

I **love** `^multi`.

It's one of those features you don't need often, but when you do, it saves the day.

The possibilities are endless when it comes to the refinement you can do, and when you consider what you'd have to do to get the same results without it, it's easy to see how much code it saves you.

```
... Allows you to simultaneously bind and de-structure a value.
(def: (hash (^@ set [element_hash _]))
  (list::mix (function (_ elem acc)
               (n.+ (\ element_hash hash elem) acc))
             0
             (set.list set)))
```

`^@` is for when you want to deal with a value both as a whole and in parts.

```
... Same as the "open" macro, but meant to be used as a pattern-matching macro for generating local bindings.
... Can optionally take a "prefix" text for the generated local bindings.
(def: #export (range (^open ".") from to)
  (All [a] (-> (Enum a) a a (List a)))
  (range' <= succ from to))
```

`^open` allows you to open structures using local variables during pattern-matching.

It's excellent when taking structures as function arguments, or when opening structures locally in `let` expressions.

```
... Or-patterns.
(type: Weekday
  #Monday
  #Tuesday
  #Wednesday
  #Thursday
  #Friday
  #Saturday
  #Sunday)

(def: (weekend? day)
  (-> Weekday Bool)
  (case day
    (^or #Saturday #Sunday)
    true

    _
    false))
```

`^or` patterns allow you to have multiple patterns with the same branch.

It's a real time-saver.

```
... It's similar to do-template, but meant to be used during pattern-matching.
(def: (beta_reduce env type)
  (-> (List Type) Type Type)
  (case type
    (#.Primitive name params)
    (#.Primitive name (list::map (beta_reduce env) params))

    (^template [<tag>]
     [(<tag> left right)
      (<tag> (beta_reduce env left) (beta_reduce env right))])
    ([#.Sum]
     [#.Product]
     [#.Function]
     [#.Apply])

    (^template [<tag>]
     [(<tag> old_env def)
      (case old_env
        #.End
        (<tag> env def)
 
        _
        type)])
    ([#.UnivQ]
     [#.ExQ])

    (#.Parameter idx)
    (maybe.else type (list.item idx env))

    (#.Named name type)
    (beta_reduce env type)

    _
    type))
```

`^template` is `^or`'s big brother.

Whereas `^or` demands that you provide the exact patterns you want (and with a single branch body), `^template` lets you provide templates for both the patterns and bodies, and then fills the blanks with all the given parameters.

You can save yourself quite a lot of typing (and debugging) by reusing a lot of pattern-matching code with this macro.

It's a great asset!

```
... Allows you to extract record members as local variables with the same names.
... For example:
(let [(^slots [#foo #bar #baz]) quux]
  (f foo bar baz))
```

`^slots` is great for working with records, as it allows you to create local variables with the names of the tags you specify.

Now you can work with record member values with ease.

```
... Allows destructuring of streams in pattern-matching expressions.
... Caveat emptor: Only use it for destructuring, and not for testing values within the streams.
(let [(^sequence& x y z _tail) (some_sequence_function 1 2 3)]
  (func x y z))
```

`^sequence&` hails from the `library/lux/data/collection/sequence` module, and it's quite special, because it allows you to de-structure something you normally wouldn't be able to: functions.

You see, Lux sequences (as defined in `library/lux/data/collection/sequence`) are implemented using functions.

The reason is that they are infinite in scope, and having an infinite data-structure would be... well... impossible (_unless you manage to steal a computer with infinite RAM from space aliens_).

Well, no biggie!

`^sequence&` does some black magic to make sure you can de-structure your stream just like any other data-structure.

## How to Make your Own

The technique is very simple.

Just create a normal macro that will take as a first argument a form containing all the _parameters_ to your pattern-matching macro.

Its second argument will be the body associated with the pattern.

After that, you'll receive all the branches (pattern + body) following the call to PM-macro.

You can process all your inputs as you wish, but in the end you must produce an even number of outputs (even, because the outputs must take the form of pattern+body pairs).

These will be further macro-expanded until all macros have been dealt with and only primitive patterns remain, so `case` can just go ahead and do normal pattern-matching.

You may wonder: _why do I receive the body?_

The answer is simple: depending on your macro, you may be trying to provide some advance functionality that requires altering (or replicating) the code of the body.

For example, `^or` copies the body for every alternative pattern you give it, and `^template` must both copy and customize the bodies (and patterns) according to the parameters you give it.

But receiving the next set of branches takes the cake for the weirdest input.

_What gives?_

It's simple: some macros are so advanced that they require altering not just their bodies, but anything that comes later.

A great example of that is the `^multi` macro (which is actually the reason those inputs are given to pattern-matching macros in the first place).

`^multi` performs some large-scale transformations on your code which require getting access to the rest of the code after a given usage of `^multi`.

However, most of the time, you'll just return the branches (and sometimes the body) unchanged.

To make things easier to understand, here is the implementation of the `^` macro, from the `library/lux` module:

```
(macro: (^ tokens)
  (case tokens
    (#Item [_ (#Form (#Item pattern #End))] (#Item body branches))
    (do meta_monad
      [pattern+ (full_expansion pattern)]
      (case pattern+
        (#Item pattern' #End)
        (in (list& pattern' body branches))
        
        _
        (failure "^ can only expand to 1 pattern.")))
    
    _
    (failure "Wrong syntax for ^ macro")))
```

The `^` prefix given to PM-macros was chosen simply to make them stand-out when you see them in code.

There is nothing special attached to that particular character.

