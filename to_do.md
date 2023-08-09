# References

0. [Basic writing and formatting syntax](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)

# Tasks

## To Do

0. Implement type-level naming ("Ghosts of Dependent Proofs"-style), in order to give unique names to classes, to force distinctions between them beyond their state types.
0. An anonymous type for un-tagged sums. E.g. `{3 #1 x}` = `(All (_ a b c d) (Or a b c (Or d (type_of x))))` && `{3 #0 x}` = `(All (_ a b c d) (Or a b c (Or (type_of x) d)))`
0. Re-name Codec#format to "injection" and Codec#value to "projection".
0. Re-name "Codec" to "Embedding".
   * [Embedding](https://en.wikipedia.org/wiki/Embedding)
0. Implement polytypic counting/length via induction.
0. Re-implement all the current polytypic code just using induction.
0. Re-name Format to Injection.
0. Re-name Parser to Projection.
0. <- as reverse-order function syntax.

## Done

