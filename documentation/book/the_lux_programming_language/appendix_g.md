# Appendix G: Regular expressions

Working with text is a pretty common and fundamental thing in day-to-day programming.

Lux's approach to doing it is with the use of composable, monadic text parsers.

The idea is that a parser is a function that takes some text input, performs some calculations which consume that input, and then returns some value, and (the remaining) unconsumed input.

Of course, the parser may fail, in which case the user should receive some meaningful error message to figure out what happened.

The `library/lux/control/parser/text` library provides a type, and a host of combinators, for building and working with text parsers.

```
(type: .public Offset
  Nat)

(type: .public Parser
  (//.Parser [Offset Text]))

... And from library/lux/control/parser

(type: .public (Parser s a)
  (-> s (Try [s a])))
```

A good example of text parsers being used is the `library/lux/data/format/json` module, which implements full JSON serialization.

---

However, programmers coming from other programming languages may be familiar with a different approach to test processing that has been very popular for a number of years now: regular expressions.

Regular expressions offer a short syntax to building text parsers that is great for writing quick text-processing tools.

Lux also offers support for this style in its `library/lux/data/text/regex` module, which offers the `regex` macro.

The `regex` macro, in turn, compiles the given syntax into a text parser, which means you can combine both approaches, for maximum flexibility.

Here are some samples for regular expressions:

```
... Literals
(regex "a")

... Wildcards
(regex ".")

... Escaping
(regex "\.")

... Character classes
(regex "\d")

(regex "\p{Lower}")

(regex "[abc]")

(regex "[a-z]")

(regex "[a-zA-Z]")

(regex "[a-z&&[def]]")

... Negation
(regex "[^abc]")

(regex "[^a-z]")

(regex "[^a-zA-Z]")

(regex "[a-z&&[^bc]]")

(regex "[a-z&&[^m-p]]")

... Combinations
(regex "aa")

(regex "a?")

(regex "a*")

(regex "a+")

... Specific amounts
(regex "a{2}")

... At least
(regex "a{1,}")

... At most
(regex "a{,1}")

... Between
(regex "a{1,2}")

... Groups
(regex "a(.)c")

(regex "a(b+)c")

(regex "(\d{3})-(\d{3})-(\d{4})")

(regex "(\d{3})-(?:\d{3})-(\d{4})")

(regex "(?<code>\d{3})-\k<code>-(\d{4})")

(regex "(?<code>\d{3})-\k<code>-(\d{4})-\\0")

(regex "(\d{3})-((\d{3})-(\d{4}))")

... Alternation
(regex "a|b")

(regex "a(.)(.)|b(.)(.)")
```

Another awesome feature of the `regex` macro is that it will build fully type-safe code for you.

This is important because the groups and alternations that you use in your regular expression will affect the type of the `regex` expression.

For example:

```
... This returns a single piece of text
(regex "a{1,}")

... But this one returns a pair of texts
... The first is the whole match: aXc
... And the second is the thing that got matched: the X itself
(regex "a(.)c")

... That means, these are the types of these regular-expressions:
(: (Parser Text)
   (regex "a{1,}"))

(: (Parser [Text Text])
   (regex "a(.)c"))
```

---

The benefits of parsers are that they are a bit easier to understand when reading them (due to their verbosity), and that they are very easy to combine (thanks to their monadic nature, and the combinator library).

The benefits of regular expressions are their familiarity to a lot of programmers, and how quick they are to write.

Ultimately, it makes the most sense to provide both mechanisms to Lux programmers, and let everyone choose whatever they find most useful.

