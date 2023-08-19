# References

0. [Basic writing and formatting syntax](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)
0. [Autolinked references and URLs](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/autolinked-references-and-urls)

# Templates

0. [(Commit)](https://github.com/LuxLang/lux/commit/)

# Tasks

## To Do

0. Implement type-level naming ([Ghosts of Dependent Proofs](https://kataskeue.com/gdp.pdf)-style), in order to give unique names to classes, to force distinctions between them beyond their state types.
0. An anonymous type for un-tagged sums. E.g. `{3 #1 x}` = `(All (_ a b c d) (Or a b c (Or d (type_of x))))` && `{3 #0 x}` = `(All (_ a b c d) (Or a b c (Or (type_of x) d)))`
0. Re-name `Codec#format` to `injection` and `Codec#value` to `projection`.
0. Re-name `Codec` to `Embedding`.
   * [Embedding](https://en.wikipedia.org/wiki/Embedding)
0. Re-name `Format` to `Injection`.
0. Re-name `Parser` to `Projection`.
0. `<-` as reverse-order function syntax.
0. Notation for 2-adic numbers (as a sibling to `Rev`)
   * [Mathematicians Use Numbers Differently From The Rest of Us](https://www.youtube.com/watch?v=tRaq4aYPzCc)
   * [Fractions and p-adic numbers | Real numbers and limits Math Foundations 90 | N J Wildberger](https://www.youtube.com/watch?v=XXRwlo_MHnI)
0. Get rid of `#Sum`, `#Product`, and `#Function` types in `Type`, and replace them with custom-named instances of Nominal.
0. Re-name Ratio to Frac(tion). Then, introduce an integer-based alternative to Frac(tion) called Rat(tional).
   * [Fraction](https://en.wikipedia.org/wiki/Fraction)
0. Have alternative arithmetic operators for Frac(tion) that do not normalize the result after the operation (+', -', *', /').
   * Normalization would obfuscate [an interesting fact about working with infinity](https://youtu.be/pE01mIrsw74?list=PL5A714C94D40392AB&t=1010).
   * ^ Which is: (infinity + point) == (infinity / point) == (infinity * (/ point))
   * It may also obfusface the possibility that there are multiple infinities (and multiple zeroes) which may be equivalente to one another, but not necessarily equal.
   * Such inequality may prove useful in some calculations, but it would be erased during normalization (leading to ambiguities).
0. Have a special rules for Frac(tion)/Rat(ional) multiplication that says:
   * `a/b * b/c = a/c` && `a/b * c/a = c/b` && `a/b * c/d = ac/bd`.
   * The above rule can make `a/0 * 0/b = a/b` and `0/a * b/0 = b/a`, bypassing any possibility for `0/0`.
   * It may also make the de-normalized zeroes and infinities carry useful information, instead of losing it through normalization.
0. Extract the variable link-ring machinery being used by both the `control/logic` and `meta/type/check` modules into its own module.
0. Replace recursive type calls from `(0 "")` to `(0 0)`
0. Fix bug wherein private aliases in module `A` of public definitions in module `B` are seen in module `C`, if it imports `A`.
0. Re-name `check.subsumes?` to `check.subsumed?`.
0. Get rid of `Dec` encoding/decoding default extensions.

## Done

0. Re-name Frac(tion) to Dec(imal).
   * [Decimal](https://en.wikipedia.org/wiki/Decimal)
0. Implement polytypic counting/length via induction.
0. [(Commit)](https://github.com/LuxLang/lux/commit/9b6682b709a5ce3d00b696dadb60d56c869edbf2) Re-implement all the current polytypic code just using induction.
   * [(Commit)](https://github.com/LuxLang/lux/commit/403d5a46c8cbada8e6eb5d457a98b1e8d9df87f3)
   * [(Commit)](https://github.com/LuxLang/lux/commit/caab080b72a358b63d6a289e0bf6e929d00873fb)
0. [(Commit)](https://github.com/LuxLang/lux/commit/caab080b72a358b63d6a289e0bf6e929d00873fb) Allow for an alternative syntax for anonymous Variant/Sum construction where just a statically-known bit and a term are provided.
   * e.g. `{#0 left}`, or `{#1 right}`. They are equivalent to similar terms with a `0` _lefts_ term prepended.
0. [(Commit)](https://github.com/LuxLang/lux/commit/403d5a46c8cbada8e6eb5d457a98b1e8d9df87f3) Aliases: `Any => True/Verum` && `Nothing => False/Falsum`

