# References

0. [Basic writing and formatting syntax](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)
0. [Autolinked references and URLs](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/autolinked-references-and-urls)

# Templates

0. [(Commit)](https://github.com/LuxLang/lux/commit/) 
   * []()

# Tasks

## To Do

0. Get rid of the `.in_module#` extension. It can be replaced by a macro that piggy-backs on the existing `quoted_module` mechanism.
0. Replace recursive type calls from `(0 "")` to `(0 0)`
0. Extract the variable link-ring machinery being used by both the `control/logic` and `meta/type/check` modules into its own module.
0. Implement type-level naming ([Ghosts of Dependent Proofs](https://kataskeue.com/gdp.pdf)-style), in order to give unique names to classes, to force distinctions between them beyond their state types.
0. An anonymous type for un-tagged sums. E.g. `{3 #1 x}` = `(All (_ a b c d) (Or a b c (Or d (type_of x))))` && `{3 #0 x}` = `(All (_ a b c d) (Or a b c (Or (type_of x) d)))`
0. Notation for 2-adic numbers (as a sibling to `Rev`)
   * [Mathematicians Use Numbers Differently From The Rest of Us](https://www.youtube.com/watch?v=tRaq4aYPzCc)
   * [Fractions and p-adic numbers | Real numbers and limits Math Foundations 90 | N J Wildberger](https://www.youtube.com/watch?v=XXRwlo_MHnI)
0. Have alternative arithmetic operators for `Frac`(tion) that do not
 normalize the result after the operation (`+'`, `-'`, `*'`, `/'`).
   * Normalization would obfuscate [an interesting fact about working with infinity](https://youtu.be/pE01mIrsw74?list=PL5A714C94D40392AB&t=1010).
   * ^ Which is: (infinity + point) == (infinity / point) == (infinity * (/ point))
   * It may also obfusface the possibility that there are multiple infinities (and multiple zeroes) which may be equivalente to one another, but not necessarily equal.
   * Such inequality may prove useful in some calculations, but it would be erased during normalization (leading to ambiguities).
0. Have a special rules for `Frac`(tion)/`Rat`(ional) multiplication that says:
   * `a/b * b/c = a/c` && `a/b * c/a = c/b` && `a/b * c/d = ac/bd`.
   * The above rule can make `a/0 * 0/b = a/b` and `0/a * b/0 = b/a`, bypassing any possibility for `0/0`.
   * It may also make the de-normalized zeroes and infinities carry useful information, instead of losing it through normalization.
0. Unify the handling of globals between extensions, analysis & declaration.
0. Replace the usages of single-use variables with the expressions they are bound to, in order to eliminate unnecessary register allocations.
0. Polytypic Binary format machinery.
0. Go from having I32 variant tags to I8 tags.
0. Implement extensible pattern-matching in the compiler, guided by the experiment in `control/pattern`.
0. Dissolve `math/random` into the rest of the standard library.
0. Dissolve `injection/.../text` into the rest of the standard library.
0. Add `<`, `<=`, `>`, `>=` definitions to every module that has an `Order`.
0. Unsafe text module.
0. Optimize compilation of pattern-matching expressions like (case <input> <literal> <then> _ <else>) into if expressions that test the literal, instead of full-blown pattern-matching.
   * Add if_i64 & if_f64 & if_text synthesis nodes.
0. Full names for number modules. `nat` => `natural`, `int` => `integer`, `rev` => `revolution`, `dec` => `decimal`, `frac` => `fraction`, `rat` => `rational`.
   * Also, big-name aliases for the types.

## Done

0. Fix bug wherein two imported modules can have the same alias.
0. [(Commit)](https://github.com/LuxLang/lux/commit/1bead83039b77e360ba3c8bb8237115fefc2bc2e) Add a `=` definition to every module that has an `Equivalence`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/ad2bd2abad4d7e014791257af066aa964c5c5aa5) Eliminated the bootstrapping compiler
0. [(Commit)](https://github.com/LuxLang/lux/commit/90bdd8c16e6864f36dfe44b716c48266a44549c4) Better syntax for the `.when#` extension.
0. [(Commit)](https://github.com/LuxLang/lux/commit/3b9cad357e2dcc44a42d5fa01cc380908b08970a) Re-name the `left` and `right` macros in the prelude. Also, get rid of their highlighting in `lux-mode`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/6de33f8b2c7b3804be4bd5ec04fb3c4b0a3efe79) Make type-normalization no longer confuse local type parameters with globally-defined types/macros.
   * Remove instances of `(.except left right)`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/c7daf84dfd365df8da5b381dfb1d3fca9afd62a7) Favor partial calls over successive calls when compiling function application.
0. [(Commit)](https://github.com/LuxLang/lux/commit/b7fb27e49cba22cdbad5ffc034b32963b35c05f4) Re-name `Codec` to `Embedding`. Alternatively, re-name to `Format`.
   * [Embedding](https://en.wikipedia.org/wiki/Embedding)
   * [Format](https://en.wikipedia.org/wiki/Content_format)
0. [(Commit)](https://github.com/LuxLang/lux/commit/b32652a350e543d9343d8b6c859773937474ae7b) Get rid of `library/lux/world/db`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/ddbbca49f87ab84d8ee8cf8568d38c27542a6e67) Replace uses of `All` with `for_all` or `for_any`. Do the same for `Ex`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/ddbbca49f87ab84d8ee8cf8568d38c27542a6e67) Get rid of `licentia`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/31dacc61d04fad87f0e0c6e67220f4fc83dee9cb) Add extensions for pattern-matching and function definition, instead of having custom syntax for both.
0. [(Commit)](https://github.com/LuxLang/lux/commit/ffe8cb4d1a728e2289b04eeb57f64cbababc58cd) Re-name `Format` to `Injection`.
   * [Injection](https://en.wikipedia.org/wiki/Injective_function)
0. [(Commit)](https://github.com/LuxLang/lux/commit/ffe8cb4d1a728e2289b04eeb57f64cbababc58cd) Fix bug wherein private aliases in module `A` of public definitions in module `B` are seen in module `C`, if it imports `A`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/6620a2ae31aa199874497c631f704c36d9244304) Re-name `Parser` to `Projection`.
   * [Projection](https://en.wikipedia.org/wiki/Projection_(relational_algebra))
0. [(Commit)](https://github.com/LuxLang/lux/commit/6620a2ae31aa199874497c631f704c36d9244304) Introduce an `Int`-based alternative to `Frac`(tion) called `Rat`(tional).
   * [Rational number](https://en.wikipedia.org/wiki/Rational_number)
0. [(Commit)](https://github.com/LuxLang/lux/commit/b548a7ebc71cd1d8150eb57d811c39afa80f20f3) `<-` as reverse-order function syntax.
0. [(Commit)](https://github.com/LuxLang/lux/commit/b548a7ebc71cd1d8150eb57d811c39afa80f20f3) Re-name `Codec#encoded` to `injection`, and `Codec#decoded` to `projection`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/7c144b96453c9aa28436dcffd90af7cbe6c323f6) Move `abstract/mix` to `algorithm/mix`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/7c144b96453c9aa28436dcffd90af7cbe6c323f6) Re-name Ratio to Frac(tion).
   * [Fraction](https://en.wikipedia.org/wiki/Fraction)
0. [(Commit)](https://github.com/LuxLang/lux/commit/9291b0f9e2799063ddb09d97bd0f7ebfaaac59ee) Re-name `check.subsumes?` to `check.subsumed?`.
0. [(Commit)](https://github.com/LuxLang/lux/commit/9291b0f9e2799063ddb09d97bd0f7ebfaaac59ee) Get rid of `Dec` encoding/decoding default extensions.
0. [(Commit)](https://github.com/LuxLang/lux/commit/6fff5228b1b92c9ee37a27b584be10547f337782) Re-name Frac(tion) to Dec(imal).
   * [Decimal](https://en.wikipedia.org/wiki/Decimal)
0. [(Commit)](https://github.com/LuxLang/lux/commit/6fff5228b1b92c9ee37a27b584be10547f337782) Implement polytypic counting/length via induction.
0. [(Commit)](https://github.com/LuxLang/lux/commit/9b6682b709a5ce3d00b696dadb60d56c869edbf2) Re-implement all the current polytypic code just using induction.
   * [(Commit)](https://github.com/LuxLang/lux/commit/403d5a46c8cbada8e6eb5d457a98b1e8d9df87f3)
   * [(Commit)](https://github.com/LuxLang/lux/commit/caab080b72a358b63d6a289e0bf6e929d00873fb)
0. [(Commit)](https://github.com/LuxLang/lux/commit/caab080b72a358b63d6a289e0bf6e929d00873fb) Allow for an alternative syntax for anonymous Variant/Sum construction where just a statically-known bit and a term are provided.
   * e.g. `{#0 left}`, or `{#1 right}`. They are equivalent to similar terms with a `0` _lefts_ term prepended.
0. [(Commit)](https://github.com/LuxLang/lux/commit/403d5a46c8cbada8e6eb5d457a98b1e8d9df87f3) Aliases: `Any => True/Verum` && `Nothing => False/Falsum`

