# Bit

The "bit" is the Lux equivalent to the bool/boolean in other programming languages.

There are 2 options:

* `#0`, which is equivalent to `false`, in other programming languages.
* `#1`, which is equivalent to `true`, in other programming languages.

# Nat(ural)

These are unsigned, 64-bit integers (that is to say: the positive numbers, and zero).

They look like consecutive digits, potentially separated by commas.

For example:

* `0`
* `123,456,789`
* `123456789`
* `1,23,456,789,`

The only caveat is that the number must not start with a comma.

# Int(eger)

These are signed, 64-bit integers.

They look like consecutive digits, potentially separated by commas, with a mandatory plus or minus sign at the beginning.

For example:

* `+0` or `-0`
* `+123,456,789`
* `-123456789`
* `+1,23,456,789,`

The only caveat is that the number must not start with a comma (after the sign).

# Rev(olution)

These are unsigned, 64-bit fractions.

They look like consecutive digits, potentially separated by commas, with a mandatory period/dot at the beginning.

They represent numbers between 0 and 1 (without including 1).

For example:

* `.0`
* `.123,456,789`
* `.123456789`
* `.1,23,456,789,`

The only caveat is that the number must not start with a comma (after the period/dot).

# Frac(tion)

These are signed, 64-bit floating-point numbers.

They are what other languages call double-precision floating-point numbers.

They look like an int(eger), immediately followed by a rev(olution).

For example:

* `+0.0`
* `-123.456,789`
* `+123.456789`
* `-1.23,456,789,`

The only caveat is that the number must not start with a comma (after the period/dot).

# Text

Text is the equivalent to _string_ in other programming languages.

It is a delimited chunk of text, syntactically enclosed in double-quotes.

Unlike in other programming languages, Lux text does not have escaping syntax for writing difficult-to-express characters within text.

For example:

* `"this is a simple example"`
* `"here, the \n will not be replaced by a new-line, since there is no escape syntax"`
* `"the backslash (i.e. \) stands for itself and serves no special function"`

# Identifier

Identifiers (called symbols in other lisp languages) are used as _names_ for functions, variables, etc.

They look like sequences of characters with the following rules.

* They cannot contain parentheses, brackets, braces, whitespace or double-quotes (i.e. `()`, or `[]`, or `{}`, or ` `, or `"`).
* They cannot contain periods/dots or hash characters (i.e. `.`, or `#`).
* They cannot begin with a digit, but may have digits throughout the rest of the identifier.
* They can be written as 2 parts (separated by a single period/dot), or as a single part (which may begin with either 1 or 2 period/dots).

For example:

* `this-is.an-identifier`
* `and_so_is_this_one`
* `.ThisOneToo`
* `..This4thOne,Also`

# Tag

Tags look similar to identifiers, and obey the same syntactic rules, with 1 additional rule: they must all begin with a single hash character.

For example:

* `#this-is.a-tag`
* `#and_so_is_this_one`
* `#.ThisOneToo`
* `#..This4thOne,Also`

# Form

Forms are composite syntactic structures made up of a (possibly empty) sequence of syntax tokens, delimited by parentheses.

Any valid syntax token may be inside a form, including other forms.

For example:

* `(this #is "a" valid.form)`
* `(+ 123 456)`
* `(def my-constant +3.14)`

# Tuple

Tuples are composite syntactic structures made up of a (possibly empty) sequence of syntax tokens, delimited by brackets.

Any valid syntax token may be inside a tuple, including other tuples.

For example:

* `[this #is "a" valid.form]`
* `[+ 123 456]`
* `[def my-constant +3.14]`

# Record

Records are composite syntactic structures made up of a (possibly empty) sequence of syntax tokens **pairs**, delimited by braces.

Any valid syntax token may be inside a record, including other records.

For example:

* `{this #is "a" valid.form}`
* `{+ [123 456]}`
* `{def {my-constant +3.14}}`

Because pairs of syntax tokens are expected, _the number of elements in a record must always be an **even** number_.

# White space

The only white space that the Lux parser recognizes as serving a function within the language are the normal space character (which separates syntax tokens within a line), and the new-line (which separates lines).

Other white space characters serve no purpose within the language, and may even be used as part of identifiers and tags (although that is not advisable).

The only exception to this rule is the carriage-return, which can show-up next to new-lines in text generated in some operating systems.

The Lux parser can recognize the carriage-return, and processes it along its associated new-line, in order to accomodate files which contain it.

# Comment

Comments in Lux are single-line.

They start with **2** hash signs, and they continue until the end of the line.

For example:

```
## This is a comment.
### So is this.

#but-this-is-a-tag.so-don't-do-this!!!
```

