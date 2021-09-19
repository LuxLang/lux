# library/lux

## Definitions

### \#

```clojure
Macro
```

Allows accessing the value of a implementation's member\.

```clojure
(# codec encoded)

................................................................
................................................................

... Also allows using that value as a function.

(# codec encoded +123)
```

### $\_

```clojure
Macro
```

Right\-association for the application of binary functions over variadic arguments\.

```clojure
($_ text#composite "Hello, " name ". How are you?")

... =>

(text#composite "Hello, " (text#composite name ". How are you?"))
```

### '

```clojure
Macro
```

Quotation as a macro\.

```clojure
(' YOLO)
```

### \+\+

```clojure
(All (_ _0)
  (-> (I64 _0) (I64 _0)))
```

Increment function\.

### \-\-

```clojure
(All (_ _0)
  (-> (I64 _0) (I64 _0)))
```

Decrement function\.

### \->

```clojure
Macro
```

Function types\.

```clojure
... This is the type of a function that takes 2 Ints and returns an Int.

(-> Int Int Int)
```

### :

```clojure
Macro
```

The type\-annotation macro\.

```clojure
(: (List Int)
   (list +1 +2 +3))
```

### :as

```clojure
Macro
```

The type\-coercion macro\.

```clojure
(:as Dinosaur
     (list +1 +2 +3))
```

### :expected

```clojure
Macro
```

Coerces the given expression to the type of whatever is expected\.

```clojure
(: Dinosaur
   (:expected (: (List Nat)
                 (list 1 2 3))))
```

### :let

```clojure
Macro
```

Local bindings for types\.

```clojure
(:let [side (Either Int Frac)]
  (List [side side]))
```

### :of

```clojure
Macro
```

Generates the type corresponding to a given expression\.

```clojure
(let [my_num +123]
  (:of my_num))

... ==

Int

................................................................
................................................................

(:of +123)

... ==

Int
```

### :parameter

```clojure
Macro
```

WARNING: Please stay away from this macro; it's very likely to be removed in a future version of Lux\.Allows you to refer to the type\-variables in a polymorphic function's type, by their index\.

```clojure
... In the example below, 0 corresponds to the 'a' variable.

(def: public  (of_list list)
  (All (_ a) (-> (List a) (Sequence a)))
  (list#mix add
            (: (Sequence (:parameter 0))
               empty)
            list))
```

### <<|

```clojure
Macro
```

Similar to the reverse piping macro, but rather than taking an initial object to work on, creates a function for taking it\.

```clojure
(<<| (mix text#composite "")
     (interposed " ")
     (list#each int#encoded))

... =>

(function (_ <it>)
  (mix text#composite ""
       (interposed " "
                   (list#each int#encoded
                              <it>))))
```

### <|

```clojure
Macro
```

Reverse piping macro\.

```clojure
(<| (mix text#composite "")
    (interposed " ")
    (list#each int#encoded)
    elems)

... =>

(mix text#composite ""
     (interposed " "
                 (list#each int#encoded
                            elems)))
```

### Alias

```clojure
... Type
[Text Text]
```

### All

```clojure
Macro
```

Universal quantification\.

```clojure
(All (_ a)
  (-> a a))

................................................................
................................................................

... A name can be provided, to specify a recursive type.

(All (List a)
  (Or Any
      [a (List a)]))
```

### And

```clojure
Macro
```

An alias for the Tuple type constructor\.

```clojure
(= (Tuple Bit Nat Text)
   (And Bit Nat Text))

................................................................
................................................................

(= (Tuple)
   (And))
```

### \(Ann meta\_data datum\)

```clojure
... Type
(Record
 [#meta meta_data
  #datum datum])
```

The type of things that can be annotated with meta\-data of arbitrary types\.

### Any

```clojure
... Type
(Ex (Any _0)
  _0)
```

The type of things whose type is irrelevant\.
It can be used to write functions or data\-structures that can take, or return, anything\.

### \(Bindings key value\)

```clojure
... Type
(Record
 [#counter Nat
  #mappings (List [key value])])
```

### Bit

```clojure
... Type
(Primitive "#Bit")
```

Your standard, run\-of\-the\-mill boolean values \(as \#0 or \#1 bits\)\.

### Code

```clojure
... Type
(Ann Location (Code' (Ann Location)))
```

The type of Code nodes for Lux syntax\.

### \(Code' w\)

```clojure
... Type
(Variant
 {#Bit Bit}
 {#Nat Nat}
 {#Int Int}
 {#Rev Rev}
 {#Frac Frac}
 {#Text Text}
 {#Symbol Symbol}
 {#Form (List (w (Code' w)))}
 {#Variant (List (w (Code' w)))}
 {#Tuple (List (w (Code' w)))})
```

### Definition

```clojure
... Type
[Bit Type Any]
```

Represents all the data associated with a definition: its type, its annotations, and its value\.

### \(Either left right\)

```clojure
... Type
(Variant
 {#Left left}
 {#Right right})
```

A choice between two values of different types\.

### Ex

```clojure
Macro
```

Existential quantification\.

```clojure
(Ex (_ a)
  [(Codec Text a) a])

................................................................
................................................................

... A name can be provided, to specify a recursive type.

(Ex (Self a)
  [(Codec Text a)
   a
   (List (Self a))])
```

### Frac

```clojure
... Type
(Primitive "#Frac")
```

Your standard, run\-of\-the\-mill floating\-point \(fractional\) numbers\.

### Global

```clojure
... Type
(Variant
 {#Definition Definition}
 {#Type Bit Type (Or [Text (List Text)] [Text (List Text)])}
 {#Tag Label}
 {#Slot Label}
 {#Alias Alias})
```

Represents all the data associated with a global constant\.

### \(I64 kind\)

```clojure
... Type
(Primitive "#I64" kind)
```

64\-bit integers without any semantics\.

### Info

```clojure
... Type
(Record
 [#target Text
  #version Text
  #mode Mode])
```

Information about the current version and type of compiler that is running\.

### Int

```clojure
... Type
(Primitive "#I64" (Primitive "#Int"))
```

Your standard, run\-of\-the\-mill integer numbers\.

### Interface

```clojure
Macro
```

Interface definition\.

```clojure
(type: public  (Order a)
  (Interface
   (: (Equivalence a)
      &equivalence)
   (: (-> a a Bit)
      <)))
```

### Label

```clojure
... Type
[Bit Type (List Text) Nat]
```

### \(List item\)

```clojure
... Type
(Variant
 {#End Any}
 {#Item item (List item)})
```

A potentially empty list of values\.

### Location

```clojure
... Type
(Record
 [#module Text
  #line Nat
  #column Nat])
```

Locations are for specifying the location of Code nodes in Lux files during compilation\.

### Lux

```clojure
... Type
(Rec Lux
 (Record
  [#info Info
   #source Source
   #location Location
   #current_module (Maybe Text)
   #modules (List [Text Module])
   #scopes (List Scope)
   #type_context Type_Context
   #expected (Maybe Type)
   #seed Nat
   #scope_type_vars (List Nat)
   #extensions Any
   #eval (-> Type Code Lux (Or Text [Lux Any]))
   #host Any]))
```

Represents the state of the Lux compiler during a run\.
It is provided to macros during their invocation, so they can access compiler data\.
Caveat emptor: Avoid fiddling with it, unless you know what you're doing\.

### Macro

```clojure
... Type
(Primitive "#Macro")
```

Functions that run at compile\-time and allow you to transform and extend the language in powerful ways\.

### Macro'

```clojure
... Type
(-> (List Code) (Meta (List Code)))
```

### \(Maybe value\)

```clojure
... Type
(Variant
 {#None Any}
 {#Some value})
```

A potentially missing value\.

### \(Meta it\)

```clojure
... Type
(-> Lux (Either Text [Lux it]))
```

Computations that can have access to the state of the compiler\.
These computations may fail, or modify the state of the compiler\.

### Mode

```clojure
... Type
(Variant
 {#Build Any}
 {#Eval Any}
 {#Interpreter Any})
```

A sign that shows the conditions under which the compiler is running\.

### Module

```clojure
... Type
(Record
 [#module_hash Nat
  #module_aliases (List [Text Text])
  #definitions (List [Text Global])
  #imports (List Text)
  #module_state Module_State])
```

All the information contained within a Lux module\.

### Module\_State

```clojure
... Type
(Variant
 {#Active Any}
 {#Compiled Any}
 {#Cached Any})
```

### Nat

```clojure
... Type
(Primitive "#I64" (Primitive "#Nat"))
```

Natural numbers \(unsigned integers\)\.
They start at zero \(0\) and extend in the positive direction\.

### Nothing

```clojure
... Type
(All (Nothing _0)
  _0)
```

The type of things whose type is undefined\.
Useful for expressions that cause errors or other 'extraordinary' conditions\.

### Or

```clojure
Macro
```

An alias for the Union type constructor\.

```clojure
(= (Union Bit Nat Text)
   (Or Bit Nat Text))

................................................................
................................................................

(= (Union)
   (Or))
```

### Primitive

```clojure
Macro
```

Macro to treat define new primitive types\.

```clojure
(Primitive "java.lang.Object")

................................................................
................................................................

(Primitive "java.util.List" [(Primitive "java.lang.Long")])
```

### Rec

```clojure
Macro
```

Parameter\-less recursive types\.

```clojure
... A name has to be given to the whole type, to use it within its body.

(Rec Int_List
  (Or Any
      [Int Int_List]))

................................................................
................................................................

... Can also be used with type: and labelled-type definitions.

(type: Type
  (Rec @
    (Variant
     {#Primitive Text (List @)}
     {#Sum @ @}
     {#Product @ @}
     {#Function @ @}
     {#Parameter Nat}
     {#Var Nat}
     {#Ex Nat}
     {#UnivQ (List @) @}
     {#ExQ (List @) @}
     {#Apply @ @}
     {#Named Symbol @})))
```

### Record

```clojure
Macro
```

Syntax for defining labelled/slotted product/tuple types\.
WARNING: Only use it within the type: macro\.

```clojure
(type: Refer
  (Record
   [#refer_defs Referrals
    #refer_open (List Openings)]))
```

### Ref

```clojure
... Type
(Variant
 {#Local Nat}
 {#Captured Nat})
```

### Rev

```clojure
... Type
(Primitive "#I64" (Primitive "#Rev"))
```

Fractional numbers that live in the interval \[0,1\)\.
Useful for probability, and other domains that work within that interval\.

### Scope

```clojure
... Type
(Record
 [#name (List Text)
  #inner Nat
  #locals (Bindings Text [Type Nat])
  #captured (Bindings Text [Type Ref])])
```

### Source

```clojure
... Type
[Location Nat Text]
```

### Symbol

```clojure
... Type
[Text Text]
```

A name for a Lux definition\.
It includes the module of provenance\.

### Text

```clojure
... Type
(Primitive "#Text")
```

Your standard, run\-of\-the\-mill string values\.

### Tuple

```clojure
Macro
```

Tuple types\.

```clojure
(Tuple Bit Nat Text)

................................................................
................................................................

(= Any
   (Tuple))
```

### Type

```clojure
... Type
(Rec Type
 (Variant
  {#Primitive Text (List Type)}
  {#Sum Type Type}
  {#Product Type Type}
  {#Function Type Type}
  {#Parameter Nat}
  {#Var Nat}
  {#Ex Nat}
  {#UnivQ (List Type) Type}
  {#ExQ (List Type) Type}
  {#Apply Type Type}
  {#Named Symbol Type}))
```

This type represents the data\-structures that are used to specify types themselves\.

### Type\_Context

```clojure
... Type
(Record
 [#ex_counter Nat
  #var_counter Nat
  #var_bindings (List [Nat (Maybe Type)])])
```

### Union

```clojure
Macro
```

Union types\.

```clojure
(Union Bit Nat Text)

................................................................
................................................................

(= Nothing
   (Union))
```

### Variant

```clojure
Macro
```

Syntax for defining labelled/tagged sum/union types\.
WARNING: Only use it within the type: macro\.

```clojure
(type: Referrals
  (Variant
   {#All}
   {#Only (List Text)}
   {#Exclude (List Text)}
   {#Ignore}
   {#Nothing}))
```

### ^

```clojure
Macro
```

Macro\-expanding patterns\.
It's a special macro meant to be used with 'case'\.

```clojure
(case (: (List Int)
         (list +1 +2 +3))
  (^ (list x y z))
  {#Some ($_ * x y z)}

  _
  {#None})
```

### ^@

```clojure
Macro
```

Allows you to simultaneously bind and de\-structure a value\.

```clojure
(def: (hash (^@ set [member_hash _]))
  (list#mix (function (_ elem acc)
              (+ acc
                 (# member_hash hash elem)))
            0
            (library/lux/data/collection/set.listset)))
```

### ^code

```clojure
Macro
```

Generates pattern\-matching code for Code values in a way that looks like code\-templating\.

```clojure
(: (Maybe Nat)
   (case (` (#0 123 +456.789))
     (^code (#0 (~ [_ {#Nat  number}]) +456.789))
     {#Some  number}

     _
     {#None}))
```

### ^multi

```clojure
Macro
```

Multi\-level pattern matching\.
Useful in situations where the result of a branch depends on further refinements on the values being matched\.

```clojure
(case (split (size static) uri)
  (^multi {#Some [chunk uri']}
          [(text#= static chunk) #1])
  (match_uri endpoint? parts' uri')

  _
  {#Left (format "Static part " (%t static) " does not match URI: " uri)})

................................................................
................................................................

... Short-cuts can be taken when using bit tests.

... The example above can be rewritten as...

(case (split (size static) uri)
  (^multi {#Some [chunk uri']}
          (text#= static chunk))
  (match_uri endpoint? parts' uri')

  _
  {#Left (format "Static part " (%t static) " does not match URI: " uri)})
```

### ^open

```clojure
Macro
```

Same as the 'open' macro, but meant to be used as a pattern\-matching macro for generating local bindings\.
Takes an 'alias' text for the generated local bindings\.

```clojure
(def: public  (range enum from to)
  (All (_ a) (-> (Enum a) a a (List a)))
  (let [(^open "[0]") enum]
    (loop [end to
           output {#End}]
      (cond (< end from)
            (again (pred end) {#Item  end output})

            (< from end)
            (again (succ end) {#Item  end output})


            {#Item  end output}))))
```

### ^or

```clojure
Macro
```

Or\-patterns\.
It's a special macro meant to be used with 'case'\.

```clojure
(type: Weekday
  (Variant
   {#Monday}
   {#Tuesday}
   {#Wednesday}
   {#Thursday}
   {#Friday}
   {#Saturday}
   {#Sunday}))

(def: (weekend? day)
  (-> Weekday Bit)
  (case day
    (^or {#Saturday} {#Sunday})
    #1

    _
    #0))
```

### ^template

```clojure
Macro
```

It's similar to template, but meant to be used during pattern\-matching\.

```clojure
(def: (reduced env type)
  (-> (List Type) Type Type)
  (case type
    {#Primitive  name params}
    {#Primitive  name (list#each (reduced env) params)}

    (^template [<tag>]
      [{<tag> left right}
       {<tag> (reduced env left) (reduced env right)}])
    ([#Sum]  [#Product])

    (^template [<tag>]
      [{<tag> left right}
       {<tag> (reduced env left) (reduced env right)}])
    ([#Function]  [#Apply])

    (^template [<tag>]
      [{<tag> old_env def}
       (case old_env
         {#End}
         {<tag> env def}

         _
         type)])
    ([#UnivQ]  [#ExQ])

    {#Parameter  idx}
    (else type (library/lux/data/collection/list.itemidx env))

    _
    type))
```

### ^|>

```clojure
Macro
```

Pipes the value being pattern\-matched against prior to binding it to a variable\.

```clojure
(case input
  (^|> value [++ (% 10) (max 1)])
  (foo value))
```

### \_$

```clojure
Macro
```

Left\-association for the application of binary functions over variadic arguments\.

```clojure
(_$ text#composite "Hello, " name ". How are you?")

... =>

(text#composite (text#composite "Hello, " name) ". How are you?")
```

### \`

```clojure
Macro
```

Hygienic quasi\-quotation as a macro\.
Unquote \(~\) and unquote\-splice \(~\+\) must also be used as forms\.
All unprefixed macros will receive their parent module's prefix if imported; otherwise will receive the prefix of the module on which the quasi\-quote is being used\.

```clojure
(` (def: (~ name)
     (function ((~' _) (~+ args))
       (~ body))))
```

### \`'

```clojure
Macro
```

Unhygienic quasi\-quotation as a macro\.
Unquote \(~\) and unquote\-splice \(~\+\) must also be used as forms\.

```clojure
(`' (def: (~ name)
      (function (_ (~+ args))
        (~ body))))
```

### \`\`

```clojure
Macro
```

Delimits a controlled \(spliced\) macro\-expansion\.
Uses a \(~~\) special form to specify where to expand\.

```clojure
(`` (some expression
          (~~ (some macro which may yield 0 or more results))))
```

### and

```clojure
Macro
```

Short\-circuiting 'and'\.

```clojure
(and #1 #0)

... =>

#0

................................................................
................................................................

(and #1 #1)

... =>

#1
```

### as\_is

```clojure
Macro
```

Given a \(potentially empty\) list of codes, just returns them immediately, without any work done\.
This may seen useless, but it has its utility when dealing with controlled\-macro\-expansion macros\.

```clojure
(with_expansions [<operands> (as_is 1
                                    2
                                    3
                                    4)]
  ($_ + <operands>))
```

### case

```clojure
Macro
```

The pattern\-matching macro\.
Allows the usage of macros within the patterns to provide custom syntax\.

```clojure
(case (: (List Int)
         (list +1 +2 +3))
  {#Item x {#Item y {#Item z {#End}}}}
  {#Some ($_ * x y z)}

  _
  {#None})
```

### char

```clojure
Macro
```

If given a 1\-character text literal, yields the char\-code of the sole character\.

```clojure
(: Nat
   (char "A"))

... =>

65
```

### comment

```clojure
Macro
```

Throws away any code given to it\.
Great for commenting\-out code, while retaining syntax high\-lighting and formatting in your text editor\.

```clojure
(comment
  (def: (this will not)
    (Be Defined)
    (because it will be (commented out))))
```

### cond

```clojure
Macro
```

Conditional branching with multiple test conditions\.

```clojure
(cond (even? num) "WHEN even"
      (odd? num) "WHEN odd"
      "ELSE")
```

### def:

```clojure
Macro
```

Defines global constants/functions\.

```clojure
(def: branching_exponent
  Int
  +5)

................................................................
................................................................

... The type is optional.

(def: branching_exponent
  +5)

................................................................
................................................................

(def: (pair_list pair)
  (-> [Code Code] (List Code))
  (let [[left right] pair]
    (list left right)))

................................................................
................................................................

... Can pattern-match on the inputs to functions.

(def: (pair_list [left right])
  (-> [Code Code] (List Code))
  (list left right))
```

### exec

```clojure
Macro
```

Sequential execution of expressions \(great for side\-effects\)\.

```clojure
(exec
  (log! "#1")
  (log! "#2")
  (log! "#3")
  "YOLO")
```

### false

```clojure
Bit
```

The boolean FALSE value\.

### for

```clojure
Macro
```

Selects the appropriate code for a given target\-platform when compiling Lux to it\.
Look\-up the available targets in library/lux/target\.

```clojure
(def: js
  "JavaScript")

(for ["JVM" (do jvm stuff)
      js (do js stuff)]
     (do default stuff))
```

### function

```clojure
Macro
```

Syntax for creating functions\.

```clojure
(: (All (_ a b)
     (-> a b a))
   (function (_ x y)
     x))

................................................................
................................................................

... Allows for giving the function itself a name, for the sake of recursion.

(: (-> Nat Nat)
   (function (factorial n)
     (case n
       0 1
       _ (* n  (factorial (-- n))))))
```

### global

```clojure
Bit
```

The export policy for public/global definitions\.

### i64

```clojure
(-> (I64 Any) I64)
```

Safe type\-casting for I64 values\.

### if

```clojure
Macro
```

Picks which expression to evaluate based on a bit test value\.

```clojure
(if #1
  "Oh, yeah!"
  "Aw hell naw!")

... =>

... Oh, yeah!

................................................................
................................................................

(if #0
  "Oh, yeah!"
  "Aw hell naw!")

... =>

... Aw hell naw!
```

### implementation

```clojure
Macro
```

Express a value that implements an interface\.

```clojure
(: (Order Int)
   (implementation
    (def: &equivalence
      equivalence)
    (def: (< reference subject)
      (< reference subject))))
```

### implementation:

```clojure
Macro
```

Interface implementation\.

```clojure
(implementation: public  order
  (Order Int)
  (def: &equivalence
    equivalence)
  (def: (< test subject)
    (< test subject)))
```

### int

```clojure
(-> (I64 Any) Int)
```

Safe type\-casting for I64 values\.

### let

```clojure
Macro
```

Creates local bindings\.
Can \(optionally\) use pattern\-matching macros when binding\.

```clojure
(let [x (foo bar)
      y (baz quux)]
  (op x y))
```

### list

```clojure
Macro
```

List literals\.

```clojure
(: (List Nat)
   (list 0 1 2 3))
```

### list&

```clojure
Macro
```

List literals, with the last element being a tail\-list\.

```clojure
(: (List Nat)
   (list& 0 1 2 3
          (: (List Nat)
             (list 4 5 6))))
```

### local

```clojure
Bit
```

The export policy for private/local definitions\.

### loop

```clojure
Macro
```

Allows arbitrary looping, using the 'again' form to re\-start the loop\.
Can be used in monadic code to create monadic loops\.

```clojure
(loop [count +0
       x init]
  (if (< +10 count)
    (again (++ count) (f x))
    x))

................................................................
................................................................

... Loops can also be given custom names.

(loop my_loop
  [count +0
   x init]
  (if (< +10 count)
    (my_loop (++ count) (f x))
    x))
```

### macro

```clojure
(-> Macro Macro')
```

### macro:

```clojure
Macro
```

Macro\-definition macro\.

```clojure
(macro: public  (symbol tokens)
  (case tokens
    (^template [<tag>]
      [(^ (list [_ {<tag> [module name]}]))
       (in (list (` [(~ (text$ module)) (~ (text$ name))])))])
    ([#Symbol])

    _
    (failure "Wrong syntax for symbol")))
```

### module\_separator

```clojure
Text
```

Character used to separate the parts of module names\.
Value: "/"

### nat

```clojure
(-> (I64 Any) Nat)
```

Safe type\-casting for I64 values\.

### not

```clojure
(-> Bit Bit)
```

Bit negation\.

```clojure
(not #1)

... =>

#0

................................................................
................................................................

(not #0)

... =>

#1
```

### open:

```clojure
Macro
```

Opens a implementation and generates a definition for each of its members \(including nested members\)\.

```clojure
(open: "i:[0]" order)

... =>

(def: i:= (# order =))

(def: i:< (# order <))
```

### or

```clojure
Macro
```

Short\-circuiting 'or'\.

```clojure
(or #1 #0)

... =>

#1

................................................................
................................................................

(or #0 #0)

... =>

#0
```

### panic\!

```clojure
(-> Text Nothing)
```

Causes an error, with the given error message\.

```clojure
(panic! "OH NO!")
```

### prelude\_module

```clojure
Text
```

The name of the prelude module
Value: "library/lux"

### private

```clojure
Bit
```

The export policy for private/local definitions\.

### public

```clojure
Bit
```

The export policy for public/global definitions\.

### rev

```clojure
(-> (I64 Any) Rev)
```

Safe type\-casting for I64 values\.

### revised@

```clojure
Macro
```

Modifies the value of a record at a given tag, based on some function\.

```clojure
(revised@ #age ++ person)

................................................................
................................................................

... Can also work with multiple levels of nesting.

(revised@ [#foo #bar #baz] func my_record)

................................................................
................................................................

... And, if only the slot/path and (optionally) the value are given, generates a mutator function.

(let [updater (revised@ [#foo #bar #baz] func)]
  (updater my_record))

(let [updater (revised@ [#foo #bar #baz])]
  (updater func my_record))
```

### same?

```clojure
(All (_ _0)
  (-> _0 _0 Bit))
```

Tests whether the 2 values are identical \(not just 'equal'\)\.

```clojure
... This one should succeed:

(let [value +5]
  (same? value
         value))

................................................................
................................................................

... This one should fail:

(same? +5
       (+ +2 +3))
```

### static

```clojure
Macro
```

Resolves the names of definitions to their values at compile\-time, assuming their values are either:
\* Bit
\* Nat
\* Int
\* Rev
\* Frac
\* Text

```clojure
(def: my_nat 123)

(def: my_text "456")

(and (case [my_nat my_text]
       (^ (static [documentation/lux.my_natdocumentation/lux.my_text]))
       true

       _
       false)
     (case [my_nat my_text]
       (^ [(static documentation/lux.my_nat)(static documentation/lux.my_text)])
       true

       _
       false))
```

### symbol

```clojure
Macro
```

Gives back a 2 tuple with the module and name parts, both as Text\.

```clojure
(symbol documentation/lux.#doc)

... =>

["documentation/lux" "#doc"]
```

### template

```clojure
Macro
```

```clojure
... By specifying a pattern (with holes), and the input data to fill those holes, repeats the pattern as many times as necessary.

(template [<name> <diff>]
  [(def: public  <name>
     (-> Int Int)
     (+ <diff>))]

  [++ +1]
  [-- -1])
```

### template:

```clojure
Macro
```

Define macros in the style of template and ^template\.
For simple macros that do not need any fancy features\.

```clojure
(template: (square x)
  (* x x))
```

### true

```clojure
Bit
```

The boolean TRUE value\.

### try

```clojure
Macro
```

```clojure
(: Foo
   (case (: (Either Text Bar)
            (try (: Bar
                    (risky computation which may panic))))
     {#Right  success}
     (: Foo
        (do something after success))

     {#Left  error}
     (: Foo
        (recover from error))))
```

### type

```clojure
Macro
```

Takes a type expression and returns its representation as data\-structure\.

```clojure
(type (All (_ a)
        (Maybe (List a))))
```

### type:

```clojure
Macro
```

The type\-definition macro\.

```clojure
(type: (List a)
  {#End}
  {#Item a (List a)})
```

### undefined

```clojure
Macro
```

Meant to be used as a stand\-in for functions with undefined implementations\.
Undefined expressions will type\-check against everything, so they make good dummy implementations\.
However, if an undefined expression is ever evaluated, it will raise a runtime error\.

```clojure
(def: (square x)
  (-> Int Int)
  (undefined))
```

### using

```clojure
Macro
```

Module\-definition macro\.

```clojure
(using
  [lux "*"
   [control
    ["M" monad "*"]]
   [data
    maybe
    ["[0]" name ("[1]#[0]" codec)]]
   [macro
    code]]
  [//
   [type ("[0]" equivalence)]])
```

### value@

```clojure
Macro
```

Accesses the value of a record at a given tag\.

```clojure
(value@ #field my_record)

................................................................
................................................................

... Can also work with multiple levels of nesting.

(value@ [#foo #bar #baz] my_record)

................................................................
................................................................

... And, if only the slot/path is given, generates an accessor function.

(let [getter (value@ [#foo #bar #baz])]
  (getter my_record))
```

### with@

```clojure
Macro
```

Sets the value of a record at a given tag\.

```clojure
(with@ #name "Lux" lang)

................................................................
................................................................

... Can also work with multiple levels of nesting.

(with@ [#foo #bar #baz] value my_record)

................................................................
................................................................

... And, if only the slot/path and (optionally) the value are given, generates a mutator function.

(let [setter (with@ [#foo #bar #baz] value)]
  (setter my_record))

(let [setter (with@ [#foo #bar #baz])]
  (setter value my_record))
```

### with\_expansions

```clojure
Macro
```

Controlled macro\-expansion\.
Bind an arbitraty number of Code nodes resulting from macro\-expansion to local bindings\.
Wherever a binding appears, the bound Code nodes will be spliced in there\.

```clojure
(def: test
  Test
  (with_expansions
    [<tests> (template [<function> <parameter> <expected>]
               [(cover [<function>]
                       (compare <text>
                                (# codec encoded <function> <parameter>)))]

               [bit #1                "#1"]
               [int +123              "+123"]
               [frac +123             "+123.0"]
               [text "123"            "'123'"]
               [symbol ["yolo" "lol"] "yolo.lol"]
               [form (list (bit #1))  "(#1)"]
               [tuple (list (bit #1)) "[#1]"])]

    ($_ and
        <tests>)))
```

### |>

```clojure
Macro
```

Piping macro\.

```clojure
(|> elems
    (list#each int#encoded)
    (interposed " ")
    (mix text#composite ""))

... =>

(mix text#composite ""
     (interposed " "
                 (list#each int#encoded
                            elems)))
```

### |>>

```clojure
Macro
```

Similar to the piping macro, but rather than taking an initial object to work on, creates a function for taking it\.

```clojure
(|>> (list#each int#encoded)
     (interposed " ")
     (mix text#composite ""))

... =>

(function (_ <it>)
  (mix text#composite ""
       (interposed " "
                   (list#each int#encoded <it>))))
```

## Missing documentation

1. `` __adjusted_quantified_type__ ``

___

# library/lux/abstract/apply

## Definitions

### \(Apply f\)

```clojure
... .Type
(Record
 [&functor (library/lux/abstract/functor.Functor f)
  on (All (_ _0 _1) (-> (f _0) (f (-> _0 _1)) (f _1)))])
```

Applicative functors\.

### composite

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (Apply _0) (Apply _1) (Apply (All (_ _2) (_0 (_1 _2))))))
```

Applicative functor composition\.

___

# library/lux/abstract/codec

## Definitions

### \(Codec medium value\)

```clojure
... .Type
(Record
 [encoded (-> value medium)
  decoded (-> medium (library/lux/control/try.Try value))])
```

A way to move back\-and\-forth between a type and an alternative representation for it\.

### composite

```clojure
(All (_ _0 _1 _2)
  (-> (Codec _2 _1) (Codec _1 _0) (Codec _2 _0)))
```

Codec composition\.

```clojure
(: (Codec c a)
   (composite (: (Codec c b)
                 cb_codec)
              (: (Codec b a)
                 ba_codec)))
```

___

# library/lux/abstract/comonad

## Definitions

### \(CoMonad \!\)

```clojure
... .Type
(Record
 [&functor (library/lux/abstract/functor.Functor !)
  out (All (_ _0) (-> (! _0) _0))
  disjoint (All (_ _0) (-> (! _0) (! (! _0))))])
```

Co\-monads are the opposite/complement to monads\.
Co\-monadic structures are often infinite in size and built upon lazily\-evaluated functions\.

### be

```clojure
.Macro
```

A co\-monadic parallel to the 'do' macro\.

```clojure
(let [square (function (_ n) (* n n))]
  (be comonad
    [inputs (iterate ++ +2)]
    (square (out inputs))))
```

___

# library/lux/abstract/comonad/cofree

## Definitions

### \(CoFree \! it\)

```clojure
... .Type
[it (! (CoFree ! it))]
```

The CoFree CoMonad\.

### comonad

```clojure
(All (_ _0)
  (-> (library/lux/abstract/functor.Functor _0) (library/lux/abstract/comonad.CoMonad (CoFree _0))))
```

### functor

```clojure
(All (_ _0)
  (-> (library/lux/abstract/functor.Functor _0) (library/lux/abstract/functor.Functor (CoFree _0))))
```

___

# library/lux/abstract/enum

## Definitions

### \(Enum it\)

```clojure
... .Type
(Record
 [&order (library/lux/abstract/order.Order it)
  succ (-> it it)
  pred (-> it it)])
```

Enumerable types, with a notion of moving forward and backwards through a type's instances\.

### range

```clojure
(All (_ _0)
  (-> (Enum _0) _0 _0 (.List _0)))
```

An inclusive \[from, to\] range of values\.

```clojure
(range enum from to)
```

___

# library/lux/abstract/equivalence

## Definitions

### \(Equivalence it\)

```clojure
... .Type
(Record
 [#= (-> it it .Bit)])
```

Equivalence for a type's instances\.

### functor

```clojure
(library/lux/abstract/functor/contravariant.Functor Equivalence)
```

### rec

```clojure
(All (_ _0)
  (-> (-> (Equivalence _0) (Equivalence _0)) (Equivalence _0)))
```

A recursive equivalence combinator\.

```clojure
(rec recursive_equivalence)
```

___

# library/lux/abstract/functor

## Definitions

### \(And left right\)

```clojure
... .Type
(All (_ _0)
  [(left _0) (right _0)])
```

### \(Fix \!\)

```clojure
... .Type
(! (Fix !))
```

### \(Functor \!\)

```clojure
... .Type
(Record
 [#each (All (_ _0 _1) (-> (-> _0 _1) (! _0) (! _1)))])
```

### \(Or left right\)

```clojure
... .Type
(All (_ _0)
  (Or (left _0) (right _0)))
```

### \(Then outer inner\)

```clojure
... .Type
(All (_ _0)
  (outer (inner _0)))
```

### composite

```clojure
(All (_ _0 _1)
  (-> (Functor _0) (Functor _1) (Functor (Then _0 _1))))
```

Functor composition\.

### product

```clojure
(All (_ _0 _1)
  (-> (Functor _0) (Functor _1) (Functor (And _0 _1))))
```

Product composition for functors\.

### sum

```clojure
(All (_ _0 _1)
  (-> (Functor _0) (Functor _1) (Functor (Or _0 _1))))
```

Co\-product \(sum\) composition for functors\.

___

# library/lux/abstract/functor/contravariant

## Definitions

### \(Functor \!\)

```clojure
... .Type
(Record
 [#each (All (_ _0 _1) (-> (-> _1 _0) (! _0) (! _1)))])
```

The contravariant functor\.

___

# library/lux/abstract/hash

## Definitions

### \(Hash it\)

```clojure
... .Type
(Record
 [&equivalence (library/lux/abstract/equivalence.Equivalence it)
  hash (-> it .Nat)])
```

A way to produce hash\-codes for a type's instances\.

### functor

```clojure
(library/lux/abstract/functor/contravariant.Functor Hash)
```

___

# library/lux/abstract/interval

## Definitions

### \(Interval it\)

```clojure
... .Type
(Record
 [&enum (library/lux/abstract/enum.Enum it)
  bottom it
  top it])
```

A representation of top and bottom boundaries for an ordered type\.

### after?

```clojure
(All (_ _0)
  (-> _0 (Interval _0) .Bit))
```

### before?

```clojure
(All (_ _0)
  (-> _0 (Interval _0) .Bit))
```

### between

```clojure
(All (_ _0)
  (-> (library/lux/abstract/enum.Enum _0) _0 _0 (Interval _0)))
```

### borders?

```clojure
(All (_ _0)
  (-> (Interval _0) _0 .Bit))
```

Where a value is at the border of an interval\.

### complement

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0)))
```

The inverse of an interval\.

### ends\_with?

```clojure
(All (_ _0)
  (-> _0 (Interval _0) .Bit))
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Interval _0)))
```

### finishes?

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) .Bit))
```

### inner?

```clojure
(All (_ _0)
  (-> (Interval _0) .Bit))
```

### intersection

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) (Interval _0)))
```

An interval spanned by both predecessors\.

### meets?

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) .Bit))
```

Whether an interval meets another one on its bottom/lower side\.

### nested?

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) .Bit))
```

### outer?

```clojure
(All (_ _0)
  (-> (Interval _0) .Bit))
```

### overlaps?

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) .Bit))
```

### precedes?

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) .Bit))
```

### singleton

```clojure
(All (_ _0)
  (-> (library/lux/abstract/enum.Enum _0) _0 (Interval _0)))
```

An interval where both top and bottom are the same value\.

```clojure
(singleton enum elem)
```

### singleton?

```clojure
(All (_ _0)
  (-> (Interval _0) .Bit))
```

### starts?

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) .Bit))
```

### starts\_with?

```clojure
(All (_ _0)
  (-> _0 (Interval _0) .Bit))
```

### succeeds?

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) .Bit))
```

### touches?

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) .Bit))
```

### union

```clojure
(All (_ _0)
  (-> (Interval _0) (Interval _0) (Interval _0)))
```

An interval that spans both predecessors\.

### within?

```clojure
(All (_ _0)
  (-> (Interval _0) _0 .Bit))
```

___

# library/lux/abstract/mix

## Definitions

### \(Mix structure\)

```clojure
... .Type
(Record
 [#mix (All (_ _0 _1) (-> (-> _1 _0 _0) _0 (structure _1) _0))])
```

Iterate over a structure's values to build a summary value\.

### with\_monoid

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monoid.Monoid _1) (Mix _0) (_0 _1) _1))
```

Mix a mixable structure using the monoid's identity as the initial value\.

```clojure
(with_monoid monoid mix value)
```

___

# library/lux/abstract/monad

## Definitions

### \(Monad it\)

```clojure
... .Type
(Record
 [&functor (library/lux/abstract/functor.Functor it)
  in (All (_ _0) (-> _0 (it _0)))
  conjoint (All (_ _0) (-> (it (it _0)) (it _0)))])
```

A monad is a monoid in the category of endofunctors\.
What's the problem?

### all

```clojure
(All (_ _0 _1)
  (-> (Monad _0) (.List (_0 _1)) (_0 (.List _1))))
```

Run all the monadic values in the list and produce a list of the base values\.

```clojure
(all monad)
```

### do

```clojure
.Macro
```

Macro for easy concatenation of monadic operations\.

```clojure
(do monad
  [y (f1 x)
   z (f2 z)]
  (in (f3 z)))
```

### each

```clojure
(All (_ _0 _1 _2)
  (-> (Monad _0) (-> _1 (_0 _2)) (.List _1) (_0 (.List _2))))
```

Apply a monadic function to all values in a list\.

```clojure
(each monad function items)
```

### mix

```clojure
(All (_ _0 _1 _2)
  (-> (Monad _0) (-> _2 _1 (_0 _1)) _1 (.List _2) (_0 _1)))
```

Mix a list with a monadic function\.

```clojure
(mix monad function initial_value items)
```

### only

```clojure
(All (_ _0 _1 _2)
  (-> (Monad _0) (-> _1 (_0 .Bit)) (.List _1) (_0 (.List _1))))
```

Filter the values in a list with a monadic function\.

```clojure
(only monad predicate items)
```

### then

```clojure
(All (_ _0 _1 _2)
  (-> (Monad _0) (-> _1 (_0 _2)) (_0 _1) (_0 _2)))
```

Apply a function with monadic effects to a monadic value and yield a new monadic value\.

```clojure
(then monad function)
```

___

# library/lux/abstract/monad/free

## Definitions

### \(Free \! it\)

```clojure
... .Type
(Variant
 {#Pure it}
 {#Effect (! (Free ! it))})
```

The Free Monad\.

### apply

```clojure
(All (_ _0)
  (-> (library/lux/abstract/functor.Functor _0) (library/lux/abstract/apply.Apply (Free _0))))
```

### functor

```clojure
(All (_ _0)
  (-> (library/lux/abstract/functor.Functor _0) (library/lux/abstract/functor.Functor (Free _0))))
```

### monad

```clojure
(All (_ _0)
  (-> (library/lux/abstract/functor.Functor _0) (library/lux/abstract/monad.Monad (Free _0))))
```

___

# library/lux/abstract/monoid

## Definitions

### \(Monoid it\)

```clojure
... .Type
(Record
 [identity it
  composite (-> it it it)])
```

A way to compose values\.
Includes an identity value which does not alter any other value when combined with\.

### and

```clojure
(All (_ _0 _1)
  (-> (Monoid _0) (Monoid _1) (Monoid [_0 _1])))
```

___

# library/lux/abstract/order

## Definitions

### <=

```clojure
Comparison
```

Less than or equal\.

### >

```clojure
Comparison
```

Greater than\.

### >=

```clojure
Comparison
```

Greater than or equal\.

### \(Choice it\)

```clojure
... .Type
(-> (Order it) it it it)
```

A choice comparison between two values, with the knowledge of how to order them\.

### \(Comparison it\)

```clojure
... .Type
(-> (Order it) it it .Bit)
```

An arbitrary comparison between two values, with the knowledge of how to order them\.

### \(Order it\)

```clojure
... .Type
(Record
 [&equivalence (library/lux/abstract/equivalence.Equivalence it)
  < (-> it it .Bit)])
```

A signature for types that possess some sense of ordering among their elements\.

### functor

```clojure
(library/lux/abstract/functor/contravariant.Functor Order)
```

### max

```clojure
Choice
```

Maximum\.

### min

```clojure
Choice
```

Minimum\.

___

# library/lux/abstract/predicate

## Definitions

### \(Predicate it\)

```clojure
... .Type
(-> it .Bit)
```

A question that can be asked of a value, yield either false \(\#0\) or true \(\#1\)\.

### all

```clojure
Predicate
```

A predicate that always succeeds\.

### and

```clojure
(All (_ _0)
  (-> (Predicate _0) (Predicate _0) (Predicate _0)))
```

A predicate that meets both predecessors\.

### complement

```clojure
(All (_ _0)
  (-> (Predicate _0) (Predicate _0)))
```

The opposite of a predicate\.

### difference

```clojure
(All (_ _0)
  (-> (Predicate _0) (Predicate _0) (Predicate _0)))
```

A predicate that meeds 'base', but not 'sub'\.

### functor

```clojure
(library/lux/abstract/functor/contravariant.Functor Predicate)
```

### intersection

```clojure
(All (_ _0)
  (library/lux/abstract/monoid.Monoid (Predicate _0)))
```

### none

```clojure
Predicate
```

A predicate that always fails\.

### or

```clojure
(All (_ _0)
  (-> (Predicate _0) (Predicate _0) (Predicate _0)))
```

A predicate that meets either predecessor\.

### rec

```clojure
(All (_ _0)
  (-> (-> (Predicate _0) (Predicate _0)) (Predicate _0)))
```

Ties the knot for a recursive predicate\.

### union

```clojure
(All (_ _0)
  (library/lux/abstract/monoid.Monoid (Predicate _0)))
```

___

# library/lux/control/concatenative

## Definitions

### &&

```clojure
(All (_ _0 _1 _2)
  (-> [[_2 _0] _1] [_2 _0 _1]))
```

Groups the 2 topmost stack values as a 2\-tuple\.

### =>

```clojure
.Macro
```

Concatenative function types\.

```clojure
(=> [Nat] [Nat])

................................................................
................................................................

(All (_ a)
  (-> a (=> [] [a])))

................................................................
................................................................

(All (_ t)
  (=> [t] []))

................................................................
................................................................

(All (_ a b c)
  (=> [a b c] [b c a]))

................................................................
................................................................

(All (_ ,,,0 ,,,1)
  (=> [then (=> ,,,0 ,,,1)
       else (=> ,,,0 ,,,1)]
      ,,,0 [Bit then else] ,,,1))
```

### ?

```clojure
(All (_ _0 _1)
  (-> [[[_1 .Bit] _0] _0] [_1 _0]))
```

Choose the top value when \#0 and the second\-to\-top when \#1\.

### apply

```clojure
.Macro
```

A generator for functions that turn arity N functions into arity N concatenative functions\.

```clojure
(: (=> [Nat] [Nat])
   ((apply 1) ++))
```

### apply/1

```clojure
(All (_ _0 _1)
  (-> (-> _0 _1) (All (_ _2) (-> [_2 _0] [_2 _1]))))
```

Lift a function of arity 1 into a concatenative function of arity 1\.

### apply/2

```clojure
(All (_ _0 _1 _2)
  (-> (-> _0 _1 _2) (All (_ _3) (-> [[_3 _0] _1] [_3 _2]))))
```

Lift a function of arity 2 into a concatenative function of arity 2\.

### apply/3

```clojure
(All (_ _0 _1 _2 _3)
  (-> (-> _0 _1 _2 _3) (All (_ _4) (-> [[[_4 _0] _1] _2] [_4 _3]))))
```

Lift a function of arity 3 into a concatenative function of arity 3\.

### apply/4

```clojure
(All (_ _0 _1 _2 _3 _4)
  (-> (-> _0 _1 _2 _3 _4) (All (_ _5) (-> [[[[_5 _0] _1] _2] _3] [_5 _4]))))
```

Lift a function of arity 4 into a concatenative function of arity 4\.

### apply/5

```clojure
(All (_ _0 _1 _2 _3 _4 _5)
  (-> (-> _0 _1 _2 _3 _4 _5) (All (_ _6) (-> [[[[[_6 _0] _1] _2] _3] _4] [_6 _5]))))
```

Lift a function of arity 5 into a concatenative function of arity 5\.

### apply/6

```clojure
(All (_ _0 _1 _2 _3 _4 _5 _6)
  (-> (-> _0 _1 _2 _3 _4 _5 _6) (All (_ _7) (-> [[[[[[_7 _0] _1] _2] _3] _4] _5] [_7 _6]))))
```

Lift a function of arity 6 into a concatenative function of arity 6\.

### apply/7

```clojure
(All (_ _0 _1 _2 _3 _4 _5 _6 _7)
  (-> (-> _0 _1 _2 _3 _4 _5 _6 _7) (All (_ _8) (-> [[[[[[[_8 _0] _1] _2] _3] _4] _5] _6] [_8 _7]))))
```

Lift a function of arity 7 into a concatenative function of arity 7\.

### apply/8

```clojure
(All (_ _0 _1 _2 _3 _4 _5 _6 _7 _8)
  (-> (-> _0 _1 _2 _3 _4 _5 _6 _7 _8) (All (_ _9) (-> [[[[[[[[_9 _0] _1] _2] _3] _4] _5] _6] _7] [_9 _8]))))
```

Lift a function of arity 8 into a concatenative function of arity 8\.

### call

```clojure
(All (_ _0 _1)
  (-> [_0 (-> _0 _1)] _1))
```

Executes an anonymous block on the stack\.

### compose

```clojure
(All (_ _0 _1 _2 _3)
  (-> [[_3 (-> _0 _1)] (-> _1 _2)] [_3 (-> _0 _2)]))
```

Function composition\.

```clojure
(library/lux/math/number/nat.=(library/lux/math/number/nat.+2 sample)
     (||> (push sample)
          (push (|>> (push 1) n/+))
          (push (|>> (push 1) n/+))
          compose
          call))
```

### dip

```clojure
(All (_ _0 _1)
  (-> [[_0 _1] (-> _0 _0)] [_0 _1]))
```

Executes a block on the stack, save for the topmost value\.

### dip/2

```clojure
(All (_ _0 _1 _2)
  (-> [[[_0 _1] _2] (-> _0 _0)] [[_0 _1] _2]))
```

Executes a block on the stack, save for the 2 topmost values\.

### do

```clojure
(All (_ _0 _1)
  (-> [[_0 (-> _1 [_0 .Bit])] (-> _0 _1)] [[_1 (-> _1 [_0 .Bit])] (-> _0 _1)]))
```

Do\-while loop expression\.

```clojure
(library/lux/math/number/nat.=(++ sample)
     (||> (push sample)
          (push (push false))
          (push (|>> (push 1) n/+))
          do while))
```

### drop

```clojure
(All (_ _0 _1)
  (-> [_1 _0] _1))
```

Drop/pop a value from the top of the stack\.

### dup

```clojure
(All (_ _0 _1)
  (-> [_1 _0] [[_1 _0] _0]))
```

Duplicate the top of the stack\.

### f/%

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Frac]))
```

% for Frac arithmetic\.

### f/\*

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Frac]))
```

\* for Frac arithmetic\.

### f/\+

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Frac]))
```

\+ for Frac arithmetic\.

### f/\-

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Frac]))
```

\- for Frac arithmetic\.

### f//

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Frac]))
```

/ for Frac arithmetic\.

### f/<

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Bit]))
```

< for Frac arithmetic\.

### f/<=

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Bit]))
```

<= for Frac arithmetic\.

### f/=

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Bit]))
```

= for Frac arithmetic\.

### f/>

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Bit]))
```

> for Frac arithmetic\.

### f/>=

```clojure
(All (_ _0)
  (-> [[_0 .Frac] .Frac] [_0 .Bit]))
```

>= for Frac arithmetic\.

### i/%

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Int]))
```

% for Int arithmetic\.

### i/\*

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Int]))
```

\* for Int arithmetic\.

### i/\+

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Int]))
```

\+ for Int arithmetic\.

### i/\-

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Int]))
```

\- for Int arithmetic\.

### i//

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Int]))
```

/ for Int arithmetic\.

### i/<

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Bit]))
```

< for Int arithmetic\.

### i/<=

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Bit]))
```

<= for Int arithmetic\.

### i/=

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Bit]))
```

= for Int arithmetic\.

### i/>

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Bit]))
```

> for Int arithmetic\.

### i/>=

```clojure
(All (_ _0)
  (-> [[_0 .Int] .Int] [_0 .Bit]))
```

>= for Int arithmetic\.

### if

```clojure
(All (_ _0 _1)
  (-> [[[_0 .Bit] (-> _0 _1)] (-> _0 _1)] _1))
```

If expression\.

```clojure
(same? "then"
       (||> (push true)
            (push "then")
            (push "else")
            if))
```

### loop

```clojure
(All (_ _0)
  (-> [_0 (-> _0 [_0 .Bit])] _0))
```

Executes a block as a loop until it yields \#0 to stop\.

### n/%

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Nat]))
```

% for Nat arithmetic\.

### n/\*

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Nat]))
```

\* for Nat arithmetic\.

### n/\+

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Nat]))
```

\+ for Nat arithmetic\.

### n/\-

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Nat]))
```

\- for Nat arithmetic\.

### n//

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Nat]))
```

/ for Nat arithmetic\.

### n/<

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Bit]))
```

< for Nat arithmetic\.

### n/<=

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Bit]))
```

<= for Nat arithmetic\.

### n/=

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Bit]))
```

= for Nat arithmetic\.

### n/>

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Bit]))
```

> for Nat arithmetic\.

### n/>=

```clojure
(All (_ _0)
  (-> [[_0 .Nat] .Nat] [_0 .Bit]))
```

>= for Nat arithmetic\.

### nip

```clojure
(All (_ _0 _1 _2)
  (-> [[_2 _0] _1] [_2 _1]))
```

Drop the second\-to\-last value from the top of the stack\.

### partial

```clojure
(All (_ _0 _1 _2)
  (-> [[_0 _2] (-> [_0 _2] _1)] [_0 (-> _0 _1)]))
```

Partial application\.

```clojure
(library/lux/math/number/nat.=(library/lux/math/number/nat.+sample sample)
     (||> (push sample)
          (push sample)
          (push n/+)
          partial
          call))
```

### push

```clojure
(All (_ _0)
  (-> _0 (All (_ _1) (-> _1 [_1 _0]))))
```

Push a value onto the stack\.

### r/%

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Rev]))
```

% for Rev arithmetic\.

### r/\*

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Rev]))
```

\* for Rev arithmetic\.

### r/\+

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Rev]))
```

\+ for Rev arithmetic\.

### r/\-

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Rev]))
```

\- for Rev arithmetic\.

### r//

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Rev]))
```

/ for Rev arithmetic\.

### r/<

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Bit]))
```

< for Rev arithmetic\.

### r/<=

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Bit]))
```

<= for Rev arithmetic\.

### r/=

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Bit]))
```

= for Rev arithmetic\.

### r/>

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Bit]))
```

> for Rev arithmetic\.

### r/>=

```clojure
(All (_ _0)
  (-> [[_0 .Rev] .Rev] [_0 .Bit]))
```

>= for Rev arithmetic\.

### rotL

```clojure
(All (_ _0 _1 _2 _3)
  (-> [[[_3 _0] _1] _2] [[[_3 _1] _2] _0]))
```

Rotes the 3 topmost stack values to the left\.

### rotR

```clojure
(All (_ _0 _1 _2 _3)
  (-> [[[_3 _0] _1] _2] [[[_3 _2] _0] _1]))
```

Rotes the 3 topmost stack values to the right\.

### swap

```clojure
(All (_ _0 _1 _2)
  (-> [[_2 _0] _1] [[_2 _1] _0]))
```

Swaps the 2 topmost stack values\.

### when

```clojure
(All (_ _0)
  (-> [[_0 .Bit] (-> _0 _0)] _0))
```

Only execute the block when \#1\.

### while

```clojure
(All (_ _0 _1)
  (-> [[_0 (-> _0 [_1 .Bit])] (-> _1 _0)] _1))
```

While loop expression\.

```clojure
(library/lux/math/number/nat.=(library/lux/math/number/nat.+distance start)
     (||> (push start)
          (push (|>> dup
                     (push start) n/-
                     (push distance) n/<))
          (push (|>> (push 1) n/+))
          while))
```

### word:

```clojure
.Macro
```

A named concatenative function\.

```clojure
(word: square
  (=> [Nat] [Nat])

  dup
  (apply/2 library/lux/math/number/nat.*))
```

### ||>

```clojure
.Macro
```

A self\-contained sequence of concatenative instructions\.

```clojure
(same? value
       (||> (push sample)))

................................................................
................................................................

(||> (push 123)
     dup
     n/=)
```

### ||L

```clojure
(All (_ _0 _1 _2)
  (-> [_2 _0] [_2 (Or _0 _1)]))
```

Left\-injects the top into sum\.

### ||R

```clojure
(All (_ _0 _1 _2)
  (-> [_2 _1] [_2 (Or _0 _1)]))
```

Right\-injects the top into sum\.

___

# library/lux/control/concurrency/actor

The actor model of concurrency\.

## Definitions

### \(Actor state\)

```clojure
... .Type
(Primitive "library/lux/control/concurrency/actor.Actor" state)
```

An entity that can react to messages \(mail\) sent to it concurrently\.

### \(Behavior input state\)

```clojure
... .Type
(Record
 [#on_init (-> input state)
  #on_mail (-> (Mail state) state (Actor state) (library/lux/control/concurrency/async.Async (library/lux/control/try.Try state)))])
```

An actor's behavior when mail is received and when a fatal error occurs\.

### \(Mail state\)

```clojure
... .Type
(-> state (Actor state) (library/lux/control/concurrency/async.Async (library/lux/control/try.Try state)))
```

A one\-way message sent to an actor, without expecting a reply\.

### \(Message state output\)

```clojure
... .Type
(-> state (Actor state) (library/lux/control/concurrency/async.Async (library/lux/control/try.Try [state output])))
```

A two\-way message sent to an actor, expecting a reply\.

### \(Obituary state\)

```clojure
... .Type
[.Text state (.List (-> state (Actor state) (library/lux/control/concurrency/async.Async (library/lux/control/try.Try state))))]
```

Details on the death of an actor\.

### Stop

```clojure
... .Type
(library/lux/control/io.IO .Any)
```

A signal to stop an actor from observing a channel\.

### actor

```clojure
.Macro
```

Defines an anonymous actor, with its behavior and internal state\.
Messages for the actor must be defined after the on\_mail handler\.

```clojure
(actor [Nat
        123]
       ((on_mail message state self)
        (message (++ state) self)))
```

### actor:

```clojure
.Macro
```

Defines a named actor, with its behavior and internal state\.
Messages for the actor must be defined after the on\_mail handler\.

```clojure
(actor: .public (stack a)
  (List a)

  ((on_mail mail state self)
   (do (try.with async.monad)
     [.let [_ (debug.log! "BEFORE")]
      output (mail state self)
      .let [_ (debug.log! "AFTER")]]
     (in output)))

  (message: .public (push [value a] state self)
    (List a)
    (let [state' {.#Item value state}]
      (async.resolved {try.#Success [state' state']}))))

(actor: .public counter
  Nat

  (message: .public (count! [increment Nat] state self)
    Any
    (let [state' (n.+ increment state)]
      (async.resolved {try.#Success [state' state']})))

  (message: .public (read! state self)
    Nat
    (async.resolved {try.#Success [state state]})))
```

### alive?

```clojure
(All (_ _0)
  (-> (Actor _0) (library/lux/control/io.IO .Bit)))
```

### dead

```clojure
(library/lux/control/exception.Exception .Any)
```

### default

```clojure
(All (_ _0)
  (Behavior _0 _0))
```

Default actor behavior\.

### mail\!

```clojure
(All (_ _0)
  (-> (Mail _0) (Actor _0) (library/lux/control/io.IO (library/lux/control/try.Try .Any))))
```

Send mail to an actor\.

### message:

```clojure
.Macro
```

A message can access the actor's state through the state parameter\.
A message can also access the actor itself through the self parameter\.
A message's output must be an async containing a 2\-tuple with the updated state and a return value\.
A message may succeed or fail \(in case of failure, the actor dies\)\.

```clojure
(actor: .public (stack a)
  (List a)

  ((on_mail mail state self)
   (do (try.with async.monad)
     [.let [_ (debug.log! "BEFORE")]
      output (mail state self)
      .let [_ (debug.log! "AFTER")]]
     (in output)))

  (message: .public (push [value a] state self)
    (List a)
    (let [state' {.#Item value state}]
      (async.resolved {try.#Success [state' state']}))))

(actor: .public counter
  Nat

  (message: .public (count! [increment Nat] state self)
    Any
    (let [state' (n.+ increment state)]
      (async.resolved {try.#Success [state' state']})))

  (message: .public (read! state self)
    Nat
    (async.resolved {try.#Success [state state]})))
```

### obituary

```clojure
(All (_ _0)
  (-> (Actor _0) (library/lux/control/concurrency/async.Async (Obituary _0))))
```

Await for an actor to stop working\.

### obituary'

```clojure
(All (_ _0)
  (-> (Actor _0) (library/lux/control/io.IO (.Maybe (Obituary _0)))))
```

### observe\!

```clojure
(All (_ _0 _1)
  (-> (-> _0 Stop (Mail _1)) (library/lux/control/concurrency/frp.Channel _0) (Actor _1) (library/lux/control/io.IO .Any)))
```

Use an actor to observe a channel by transforming each datum
flowing through the channel into mail the actor can process\.
Can stop observing the channel by executing the Stop value\.

### poison\!

```clojure
(All (_ _0)
  (-> (Actor _0) (library/lux/control/io.IO (library/lux/control/try.Try .Any))))
```

Kills the actor by sending mail that will kill it upon processing,
but allows the actor to handle previous mail\.

### poisoned

```clojure
(library/lux/control/exception.Exception .Any)
```

### spawn\!

```clojure
(All (_ _0 _1)
  (-> (Behavior _0 _1) _0 (library/lux/control/io.IO (Actor _1))))
```

Given a behavior and initial state, spawns an actor and returns it\.

### tell\!

```clojure
(All (_ _0 _1)
  (-> (Message _0 _1) (Actor _0) (library/lux/control/concurrency/async.Async (library/lux/control/try.Try _1))))
```

Communicate with an actor through message\-passing\.

___

# library/lux/control/concurrency/async

## Definitions

### \(Async it\)

```clojure
... .Type
(Primitive "library/lux/control/concurrency/async.Async" it)
```

Represents values produced by asynchronous computations \(unlike IO, which is synchronous\)\.

### \(Resolver it\)

```clojure
... .Type
(-> it (library/lux/control/io.IO .Bit))
```

The function used to give a value to an async\.
Will signal 'true' if the async has been resolved for the 1st time, 'false' otherwise\.

### after

```clojure
(All (_ _0)
  (-> .Nat _0 (Async _0)))
```

Delivers a value after a certain period has passed\.

```clojure
(after milli_seconds value)
```

### and

```clojure
(All (_ _0 _1)
  (-> (Async _0) (Async _1) (Async [_0 _1])))
```

Combines the results of both asyncs, in\-order\.

```clojure
(and left right)
```

### apply

```clojure
(library/lux/abstract/apply.Apply Async)
```

### async

```clojure
(All (_ _0)
  (-> .Any [(Async _0) (Resolver _0)]))
```

Creates a fresh async that has not been resolved yet\.

```clojure
(async _)
```

### delay

```clojure
(-> .Nat (Async .Any))
```

An async that will be resolved after the specified amount of milli\-seconds\.

```clojure
(delay milli_seconds)
```

### either

```clojure
(All (_ _0)
  (-> (Async _0) (Async _0) (Async _0)))
```

Yields the results of whichever async gets resolved first\.
You cannot tell which one was resolved first\.

```clojure
(either left right)
```

### functor

```clojure
(library/lux/abstract/functor.Functor Async)
```

### future

```clojure
(All (_ _0)
  (-> (library/lux/control/io.IO _0) (Async _0)))
```

Runs an I/O computation on its own thread\.
Returns an async that will eventually host its result\.

```clojure
(future computation)
```

### monad

```clojure
(library/lux/abstract/monad.Monad Async)
```

### or

```clojure
(All (_ _0 _1)
  (-> (Async _0) (Async _1) (Async (Or _0 _1))))
```

Yields the results of whichever async gets resolved first\.
You can tell which one was resolved first through pattern\-matching\.

```clojure
(or left right)
```

### resolved

```clojure
(All (_ _0)
  (-> _0 (Async _0)))
```

Produces an async that has already been resolved to the given value\.

```clojure
(resolved value)
```

### resolved?

```clojure
(All (_ _0)
  (-> (Async _0) (library/lux/control/io.IO .Bit)))
```

Checks whether an async's value has already been resolved\.

### schedule\!

```clojure
(All (_ _0)
  (-> .Nat (library/lux/control/io.IO _0) (Async _0)))
```

Runs an I/O computation on its own thread \(after a specified delay\)\.
Returns an async that will eventually host its result\.

```clojure
(schedule! milli_seconds computation)
```

### upon\!

```clojure
(All (_ _0)
  (-> (-> _0 (library/lux/control/io.IO .Any)) (Async _0) (library/lux/control/io.IO .Any)))
```

Executes the given function as soon as the async has been resolved\.

```clojure
(upon! function async)
```

### value

```clojure
(All (_ _0)
  (-> (Async _0) (library/lux/control/io.IO (.Maybe _0))))
```

Polls an async for its value\.

### within

```clojure
(All (_ _0)
  (-> .Nat (Async _0) (Async (.Maybe _0))))
```

Wait for an async to be resolved within the specified amount of milli\-seconds\.

```clojure
(within milli_seconds async)
```

___

# library/lux/control/concurrency/atom

## Definitions

### \(Atom it\)

```clojure
... .Type
(Primitive "library/lux/control/concurrency/atom.Atom" it)
```

Atomic references that are safe to mutate concurrently\.

### atom

```clojure
(All (_ _0)
  (-> _0 (Atom _0)))
```

### compare\_and\_swap\!

```clojure
(All (_ _0)
  (-> _0 _0 (Atom _0) (library/lux/control/io.IO .Bit)))
```

Only mutates an atom if you can present its current value\.
That guarantees that atom was not updated since you last read from it\.

### read\!

```clojure
(All (_ _0)
  (-> (Atom _0) (library/lux/control/io.IO _0)))
```

### update\!

```clojure
(All (_ _0)
  (-> (-> _0 _0) (Atom _0) (library/lux/control/io.IO [_0 _0])))
```

Updates an atom by applying a function to its current value\.
If it fails to update it \(because some other process wrote to it first\), it will retry until it succeeds\.
The retries will be done with the new values of the atom, as they show up\.

### write\!

```clojure
(All (_ _0)
  (-> _0 (Atom _0) (library/lux/control/io.IO _0)))
```

Writes the given value to an atom\.
If it fails to write it \(because some other process wrote to it first\), it will retry until it succeeds\.

___

# library/lux/control/concurrency/frp

## Definitions

### \(Channel it\)

```clojure
... .Type
(library/lux/control/concurrency/async.Async (.Maybe [it (Channel it)]))
```

An asynchronous channel to distribute values\.

### \(Sink it\)

```clojure
... .Type
(Record
 [close (library/lux/control/io.IO (library/lux/control/try.Try .Any))
  feed (-> it (library/lux/control/io.IO (library/lux/control/try.Try .Any)))])
```

The tail\-end of a channel, which can be written\-to to fee the channel\.

### \(Subscriber it\)

```clojure
... .Type
(-> it (library/lux/control/io.IO (.Maybe .Any)))
```

A function that can receive every value fed into a channel\.

### apply

```clojure
(library/lux/abstract/apply.Apply Channel)
```

### channel

```clojure
(All (_ _0)
  (-> .Any [(Channel _0) (Sink _0)]))
```

Creates a brand\-new channel and hands it over, along with the sink to write to it\.

```clojure
(channel _)
```

### channel\_is\_already\_closed

```clojure
(library/lux/control/exception.Exception .Any)
```

### distinct

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (Channel _0) (Channel _0)))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Channel)
```

### iterations

```clojure
(All (_ _0 _1)
  (-> (-> _0 (library/lux/control/concurrency/async.Async (.Maybe [_0 _1]))) _0 (Channel _1)))
```

### list

```clojure
(All (_ _0)
  (-> (Channel _0) (library/lux/control/concurrency/async.Async (.List _0))))
```

### mix

```clojure
(All (_ _0 _1)
  (-> (-> _1 _0 (library/lux/control/concurrency/async.Async _0)) _0 (Channel _1) (library/lux/control/concurrency/async.Async _0)))
```

Asynchronous mix over channels\.

```clojure
(mix f init channel)
```

### mixes

```clojure
(All (_ _0 _1)
  (-> (-> _1 _0 (library/lux/control/concurrency/async.Async _0)) _0 (Channel _1) (Channel _0)))
```

### monad

```clojure
(library/lux/abstract/monad.Monad Channel)
```

### of\_async

```clojure
(All (_ _0)
  (-> (library/lux/control/concurrency/async.Async _0) (Channel _0)))
```

A one\-element channel containing the output from an async\.

```clojure
(of_async async)
```

### only

```clojure
(All (_ _0)
  (-> (-> _0 .Bit) (Channel _0) (Channel _0)))
```

Produces a new channel based on the old one, only with values
that pass the test\.

```clojure
(only pass? channel)
```

### periodic

```clojure
(-> .Nat [(Channel .Any) (Sink .Any)])
```

### poll

```clojure
(All (_ _0)
  (-> .Nat (library/lux/control/io.IO _0) [(Channel _0) (Sink _0)]))
```

### sequential

```clojure
(All (_ _0)
  (-> .Nat (.List _0) (Channel _0)))
```

Transforms the given list into a channel with the same elements\.

```clojure
(sequential milli_seconds values)
```

### subscribe\!

```clojure
(All (_ _0)
  (-> (Subscriber _0) (Channel _0) (library/lux/control/io.IO .Any)))
```

___

# library/lux/control/concurrency/semaphore

## Definitions

### Barrier

```clojure
... .Type
(Primitive "library/lux/control/concurrency/semaphore.Barrier")
```

A barrier that blocks all processes from proceeding until a given number of processes are parked at the barrier\.

### Limit

```clojure
... .Type
(library/lux/type/refinement.Refined .Nat (Primitive "{New Type @"library/lux/control/concurrency/semaphore",122,2 0}"))
```

A limit for barriers\.

### Mutex

```clojure
... .Type
(Primitive "library/lux/control/concurrency/semaphore.Mutex")
```

A mutual\-exclusion lock that can only be acquired by one process at a time\.

### Semaphore

```clojure
... .Type
(Primitive "library/lux/control/concurrency/semaphore.Semaphore")
```

A tool for controlling access to resources by multiple concurrent processes\.

### barrier

```clojure
(-> Limit Barrier)
```

### block\!

```clojure
(-> Barrier (library/lux/control/concurrency/async.Async .Any))
```

Wait on a barrier until all processes have arrived and met the barrier's limit\.

### limit

```clojure
(library/lux/type/refinement.Refiner .Nat (Primitive "{New Type @"library/lux/control/concurrency/semaphore",122,2 0}"))
```

Produce a limit for a barrier\.

### mutex

```clojure
(-> .Any Mutex)
```

Creates a brand\-new mutex\.

```clojure
(mutex _)
```

### semaphore

```clojure
(-> .Nat Semaphore)
```

```clojure
(semaphore initial_open_positions)
```

### semaphore\_is\_maxed\_out

```clojure
(library/lux/control/exception.Exception .Nat)
```

### signal\!

```clojure
(Ex (_ _0)
  (-> Semaphore (library/lux/control/concurrency/async.Async (library/lux/control/try.Try .Int))))
```

Signal to a semaphore that you're done with your work, and that there is a new open position\.

```clojure
(signal! semaphore)
```

### synchronize\!

```clojure
(All (_ _0)
  (-> Mutex (library/lux/control/io.IO (library/lux/control/concurrency/async.Async _0)) (library/lux/control/concurrency/async.Async _0)))
```

Runs the procedure with exclusive control of the mutex\.

```clojure
(synchronize! mutex procedure)
```

### wait\!

```clojure
(Ex (_ _0)
  (-> Semaphore (library/lux/control/concurrency/async.Async .Any)))
```

Wait on a semaphore until there are open positions\.
After finishing your work, you must 'signal' to the semaphore that you're done\.

```clojure
(wait! semaphore)
```

___

# library/lux/control/concurrency/stm

## Definitions

### \(STM it\)

```clojure
... .Type
(-> Tx [Tx it])
```

A computation which updates a transaction and produces a value\.

### \(Var it\)

```clojure
... .Type
(Primitive "library/lux/control/concurrency/stm.Var" it)
```

A mutable cell containing a value, and observers that will be alerted of any change to it\.

### apply

```clojure
(library/lux/abstract/apply.Apply STM)
```

### commit\!

```clojure
(All (_ _0)
  (-> (STM _0) (library/lux/control/concurrency/async.Async _0)))
```

Commits a transaction and returns its result \(asynchronously\)\.
Note that a transaction may be re\-run an indeterminate number of times if other transactions involving the same variables successfully commit first\.
For this reason, it's important to note that transactions must be free from side\-effects, such as I/O\.

```clojure
(commit! procedure)
```

### follow\!

```clojure
(All (_ _0)
  (-> (Var _0) (library/lux/control/io.IO [(library/lux/control/concurrency/frp.Channel _0) (library/lux/control/concurrency/frp.Sink _0)])))
```

Creates a channel that will receive all changes to the value of the given var\.

```clojure
(follow! target)
```

### functor

```clojure
(library/lux/abstract/functor.Functor STM)
```

### monad

```clojure
(library/lux/abstract/monad.Monad STM)
```

### read

```clojure
(All (_ _0)
  (-> (Var _0) (STM _0)))
```

### update

```clojure
(All (_ _0)
  (-> (-> _0 _0) (Var _0) (STM [_0 _0])))
```

Update a var's value, and return a tuple with the old and the new values\.

```clojure
(update function var)
```

### var

```clojure
(All (_ _0)
  (-> _0 (Var _0)))
```

Creates a new STM var, with a default value\.

```clojure
(var value)
```

### write

```clojure
(All (_ _0)
  (-> _0 (Var _0) (STM .Any)))
```

___

# library/lux/control/concurrency/thread

## Definitions

### parallelism

```clojure
.Nat
```

How many processes can run in parallel\.

### schedule\!

```clojure
(-> .Nat (library/lux/control/io.IO .Any) (library/lux/control/io.IO .Any))
```

Executes an I/O procedure after some milli\-seconds\.

```clojure
(schedule! milli_seconds action)
```

___

# library/lux/control/continuation

## Definitions

### \(Cont input output\)

```clojure
... .Type
(-> (-> input output) output)
```

Continuations\.

### apply

```clojure
(All (_ _0)
  (library/lux/abstract/apply.Apply (All (_ _1) (Cont _1 _0))))
```

### continued

```clojure
(All (_ _0 _1)
  (-> (-> _0 _1) (Cont _0 _1) _1))
```

Continues a continuation thunk\.

```clojure
(continued next cont)
```

### functor

```clojure
(All (_ _0)
  (library/lux/abstract/functor.Functor (All (_ _1) (Cont _1 _0))))
```

### monad

```clojure
(All (_ _0)
  (library/lux/abstract/monad.Monad (All (_ _1) (Cont _1 _0))))
```

### pending

```clojure
.Macro
```

Turns any expression into a function that is pending a continuation\.

```clojure
(pending (some_function some_input))
```

### portal

```clojure
(All (_ _0 _1 _2)
  (-> _0 (Cont [(-> _0 (Cont _1 _2)) _0] _2)))
```

### reset

```clojure
(All (_ _0 _1)
  (-> (Cont _0 _0) (Cont _0 _1)))
```

### result

```clojure
(All (_ _0)
  (-> (Cont _0 _0) _0))
```

Forces a continuation thunk to be evaluated\.

```clojure
(result cont)
```

### shift

```clojure
(All (_ _0)
  (-> (-> (-> _0 (Cont _0 _0)) (Cont _0 _0)) (Cont _0 _0)))
```

### with\_current

```clojure
(All (_ _0 _1 _2)
  (-> (-> (-> _0 (Cont _1 _2)) (Cont _0 _2)) (Cont _0 _2)))
```

Call with current continuation\.

```clojure
(with_current
  (function (_ go)
    (do monad
      [.let [nexus (function (nexus val)
                     (go [nexus val]))]
       _ (go [nexus init])]
      (in (undefined)))))
```

___

# library/lux/control/exception

Pure\-Lux exception\-handling functionality\.

## Definitions

### \(Exception it\)

```clojure
... .Type
(Record
 [#label .Text
  #constructor (-> it .Text)])
```

An exception provides a way to decorate error messages\.

### assertion

```clojure
(All (_ _0)
  (-> (Exception _0) _0 .Bit (library/lux/control/try.Try .Any)))
```

### error

```clojure
(All (_ _0)
  (-> (Exception _0) _0 .Text))
```

Constructs an error message from an exception\.

```clojure
(error exception message)
```

### except

```clojure
(All (_ _0 _1)
  (-> (Exception _0) _0 (library/lux/control/try.Try _1)))
```

Decorate an error message with an Exception and lift it into the error\-handling context\.

```clojure
(except exception message)
```

### exception:

```clojure
.Macro
```

Define a new exception type\.
It mostly just serves as a way to tag error messages for later catching\.

```clojure
... Simple case:

(exception: .public some_exception)

................................................................
................................................................

... Complex case:

(exception: .public [arbitrary type variables] (some_exception [optional Text
                                                                arguments Int])
  optional_body)
```

### listing

```clojure
(All (_ _0)
  (-> (-> _0 .Text) (.List _0) .Text))
```

A numbered report of the entries on a list\.
NOTE: 0\-based numbering\.

```clojure
(listing format entries)
```

### match?

```clojure
(All (_ _0)
  (-> (Exception _0) .Text .Bit))
```

Is this exception the cause of the error message?

```clojure
(match? exception error)
```

### otherwise

```clojure
(All (_ _0)
  (-> (-> .Text _0) (library/lux/control/try.Try _0) _0))
```

If no handler could be found to catch the exception, then run a function as a last\-resort measure\.

```clojure
(otherwise else try)
```

### report

```clojure
.Macro
```

An error report\.

```clojure
(: Text
   (report ["Row 0" value/0]
           ["Row 1" value/1]
           ,,,
           ["Row N" value/N]))
```

### when

```clojure
(All (_ _0 _1)
  (-> (Exception _0) (-> .Text _1) (library/lux/control/try.Try _1) (library/lux/control/try.Try _1)))
```

If a particular exception is detected on a possibly\-erroneous value, handle it\.
If no exception was detected, or a different one from the one being checked, then pass along the original value\.

```clojure
(when exception then try)
```

### with

```clojure
(All (_ _0 _1)
  (-> (Exception _0) _0 (library/lux/control/try.Try _1) (library/lux/control/try.Try _1)))
```

If a computation fails, prepends the exception to the error\.

```clojure
(with exception message computation)
```

___

# library/lux/control/function

## Definitions

### composite

```clojure
(All (_ _0 _1 _2)
  (-> (-> _1 _2) (-> _0 _1) _0 _2))
```

Function composition\.

```clojure
(= ((composite f g) "foo")
   (f (g "foo")))
```

### constant

```clojure
(All (_ _0)
  (-> _0 (All (_ _1) (-> _1 _0))))
```

Create constant functions\.

```clojure
(= ((constant "foo") "bar")
   "foo")
```

### flipped

```clojure
(All (_ _0 _1 _2)
  (-> (-> _0 _1 _2) _1 _0 _2))
```

Flips the order of the arguments of a function\.

```clojure
(= ((flipped f) "foo" "bar")
   (f "bar" "foo"))
```

### identity

```clojure
(All (_ _0)
  (-> _0 _0))
```

Identity function\.
Does nothing to its argument and just returns it\.

```clojure
(same? (identity value)
       value)
```

### monoid

```clojure
(All (_ _0)
  (library/lux/abstract/monoid.Monoid (-> _0 _0)))
```

### on

```clojure
(All (_ _0 _1)
  (-> _0 (-> _0 _1) _1))
```

Simple 1\-argument function application\.

```clojure
(on input function)
```

___

# library/lux/control/function/contract

## Definitions

### post

```clojure
.Macro
```

Post\-conditions\.
Given a predicate and an expression to run, evaluates the expression and then tests the output with the predicate\.
If the predicate returns \#1, returns the value of the expression\.
Otherwise, an error is raised\.

```clojure
(post i.even?
      (i.+ +2 +2))
```

### post\_condition\_failed

```clojure
(library/lux/control/exception.Exception .Code)
```

### pre

```clojure
.Macro
```

Pre\-conditions\.
Given a test and an expression to run, only runs the expression if the test passes\.
Otherwise, an error is raised\.

```clojure
(pre (i.= +4 (i.+ +2 +2))
     (foo +123 +456 +789))
```

### pre\_condition\_failed

```clojure
(library/lux/control/exception.Exception .Code)
```

___

# library/lux/control/function/memo

## Definitions

### \(Memo input output\)

```clojure
... .Type
(library/lux/control/function/mixin.Recursive input (library/lux/control/state.State (library/lux/data/collection/dictionary.Dictionary input output) output))
```

### closed

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/hash.Hash _0) (Memo _0 _1) _0 _1))
```

Memoization confined to a single invocation to the function \(not counting any subsequent recursive invocations\)\.
Memoized results will be re\-used during recursive invocations, but cannot be accessed after the main invocation has ended\.

```clojure
(closed hash memo)
```

### memoization

```clojure
(All (_ _0 _1)
  (library/lux/control/function/mixin.Mixin _0 (library/lux/control/state.State (library/lux/data/collection/dictionary.Dictionary _0 _1) _1)))
```

### none

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/hash.Hash _0) (Memo _0 _1) _0 _1))
```

No memoization at all\.
This is useful as a test control when measuring the effect of using memoization\.

```clojure
(none hash memo)
```

### open

```clojure
(All (_ _0 _1)
  (-> (Memo _0 _1) [(library/lux/data/collection/dictionary.Dictionary _0 _1) _0] [(library/lux/data/collection/dictionary.Dictionary _0 _1) _1]))
```

Memoization where the memoized results can be re\-used accross invocations\.

```clojure
(open memo)
```

___

# library/lux/control/function/mixin

## Definitions

### \(Mixin input output\)

```clojure
... .Type
(-> (-> input output) (-> input output) input output)
```

A partially\-defined function which can be mixed with others to inherit their behavior\.

### \(Recursive input output\)

```clojure
... .Type
(-> (-> input output) input output)
```

An indirectly recursive function\.

### advice

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/predicate.Predicate _0) (Mixin _0 _1) (Mixin _0 _1)))
```

Only apply then mixin when the input meets some criterion\.

```clojure
(advice when then)
```

### after

```clojure
(All (_ _0 _1 _2)
  (-> (library/lux/abstract/monad.Monad _0) (-> _1 _2 (_0 .Any)) (Mixin _1 (_0 _2))))
```

Executes an action after doing the main work\.

```clojure
(after monad action)
```

### before

```clojure
(All (_ _0 _1 _2)
  (-> (library/lux/abstract/monad.Monad _0) (-> _1 (_0 .Any)) (Mixin _1 (_0 _2))))
```

Executes an action before doing the main work\.

```clojure
(before monad action)
```

### fixed

```clojure
(All (_ _0 _1)
  (-> (Mixin _0 _1) _0 _1))
```

Given a mixin, produces a normal function\.

```clojure
(fixed f)
```

### mixed

```clojure
(All (_ _0 _1)
  (-> (Mixin _0 _1) (Mixin _0 _1) (Mixin _0 _1)))
```

Produces a new mixin, where the behavior of the child can make use of the behavior of the parent\.

```clojure
(mixed parent child)
```

### monoid

```clojure
(All (_ _0 _1)
  (library/lux/abstract/monoid.Monoid (Mixin _0 _1)))
```

### nothing

```clojure
Mixin
```

A mixin that does nothing and just delegates work to the next mixin\.

### of\_recursive

```clojure
(All (_ _0 _1)
  (-> (Recursive _0 _1) (Mixin _0 _1)))
```

Transform an indirectly recursive function into a mixin\.

```clojure
(of_recursive recursive)
```

___

# library/lux/control/function/mutual

## Definitions

### def:

```clojure
.Macro
```

Globally\-defined mutually\-recursive functions\.

```clojure
(def:
  [.public (even? number)
   (-> Nat Bit)
   (case number
     0 true
     _ (odd? (-- number)))]

  [.public (odd? number)
   (-> Nat Bit)
   (case number
     0 false
     _ (even? (-- number)))])
```

### let

```clojure
.Macro
```

Locally\-defined mutually\-recursive functions\.

```clojure
(let [(even? number)
      (-> Nat Bit)
      (case number
        0 true
        _ (odd? (-- number)))

      (odd? number)
      (-> Nat Bit)
      (case number
        0 false
        _ (even? (-- number)))]
  (and (even? 4)
       (odd? 5)))
```

___

# library/lux/control/io

A method for abstracting I/O and effectful computations to make it safe while writing pure functional code\.

## Definitions

### \(IO it\)

```clojure
... .Type
(Primitive "library/lux/control/io.IO" it)
```

A type that represents synchronous, effectful computations that may interact with the outside world\.

### apply

```clojure
(library/lux/abstract/apply.Apply IO)
```

### functor

```clojure
(library/lux/abstract/functor.Functor IO)
```

### io

```clojure
.Macro
```

Delays the evaluation of an expression, by wrapping it in an IO 'thunk'\.
Great for wrapping effectful computations \(which will not be performed until the IO is 'run\!'\)\.

```clojure
(io (exec
      (log! msg)
      "Some value..."))
```

### monad

```clojure
(library/lux/abstract/monad.Monad IO)
```

### run\!

```clojure
(All (_ _0 _1)
  (-> (IO _1) _1))
```

A way to execute IO computations and perform their side\-effects\.

___

# library/lux/control/lazy

## Definitions

### \(Lazy it\)

```clojure
... .Type
(Primitive "library/lux/control/lazy.Lazy" it)
```

A value specified by an expression that is calculated only at the last moment possible\.
Afterwards, the value is cached for future reference\.

### apply

```clojure
(library/lux/abstract/apply.Apply Lazy)
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Lazy _0))))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Lazy)
```

### lazy

```clojure
.Macro
```

Specifies a lazy value by providing the expression that computes it\.

```clojure
(lazy eager_computation)
```

### monad

```clojure
(library/lux/abstract/monad.Monad Lazy)
```

### value

```clojure
(All (_ _0)
  (-> (Lazy _0) _0))
```

___

# library/lux/control/maybe

## Definitions

### apply

```clojure
(library/lux/abstract/apply.Apply .Maybe)
```

### else

```clojure
.Macro
```

Allows you to provide a default value that will be used
if a \(Maybe x\) value turns out to be \.\#None\.
Note: the expression for the default value will not be computed if the base computation succeeds\.

```clojure
(else +20 {.#Some +10})

... =>

+10

................................................................
................................................................

(else +20 {.#None})

... =>

+20
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (.Maybe _0))))
```

### functor

```clojure
(library/lux/abstract/functor.Functor .Maybe)
```

### hash

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) (library/lux/abstract/hash.Hash (.Maybe _0))))
```

### lifted

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (_0 _1) (_0 (.Maybe _1))))
```

Wraps a monadic value with Maybe machinery\.

```clojure
(lifted monad)
```

### list

```clojure
(All (_ _0)
  (-> (.Maybe _0) (.List _0)))
```

### monad

```clojure
(library/lux/abstract/monad.Monad .Maybe)
```

### monoid

```clojure
(All (_ _0)
  (library/lux/abstract/monoid.Monoid (.Maybe _0)))
```

### trusted

```clojure
(All (_ _0)
  (-> (.Maybe _0) _0))
```

Assumes that a Maybe value is a \.\#Some and yields its value\.
Raises/throws a runtime error otherwise\.
WARNING: Use with caution\.

```clojure
(trusted trusted_computation)
```

### when

```clojure
.Macro
```

Can be used as a guard in \(co\)monadic be/do expressions\.

```clojure
(do monad
  [value (do_something 1 2 3)
   .when (passes_test? value)]
  (do_something_else 4 5 6))
```

### with

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (library/lux/abstract/monad.Monad (All (_ _1) (_0 (.Maybe _1))))))
```

___

# library/lux/control/parser

## Definitions

### \(Parser state it\)

```clojure
... .Type
(-> state (library/lux/control/try.Try [state it]))
```

A generic parser\.

### after

```clojure
(All (_ _0 _1 _2)
  (-> (Parser _0 _1) (Parser _0 _2) (Parser _0 _2)))
```

Run the parser after another one \(whose output is ignored\)\.

```clojure
(after param subject)
```

### and

```clojure
(All (_ _0 _1 _2)
  (-> (Parser _0 _1) (Parser _0 _2) (Parser _0 [_1 _2])))
```

Sequencing combinator\.

```clojure
(and first second)
```

### apply

```clojure
(All (_ _0)
  (library/lux/abstract/apply.Apply (Parser _0)))
```

### assertion

```clojure
(All (_ _0)
  (-> .Text .Bit (Parser _0 .Any)))
```

Fails with the given message if the test is \#0\.

```clojure
(assertion message test)
```

### at\_least

```clojure
(All (_ _0 _1)
  (-> .Nat (Parser _0 _1) (Parser _0 (.List _1))))
```

Parse at least N times\.

```clojure
(at_least amount parser)
```

### at\_most

```clojure
(All (_ _0 _1)
  (-> .Nat (Parser _0 _1) (Parser _0 (.List _1))))
```

Parse at most N times\.

```clojure
(at_most amount parser)
```

### before

```clojure
(All (_ _0 _1 _2)
  (-> (Parser _0 _1) (Parser _0 _2) (Parser _0 _2)))
```

Run the parser before another one \(whose output is ignored\)\.

```clojure
(before param subject)
```

### between

```clojure
(All (_ _0 _1)
  (-> .Nat .Nat (Parser _0 _1) (Parser _0 (.List _1))))
```

```clojure
(between minimum additional parser)
```

### codec

```clojure
(All (_ _0 _1 _2)
  (-> (library/lux/abstract/codec.Codec _1 _2) (Parser _0 _1) (Parser _0 _2)))
```

Decode the output of a parser using a codec\.

```clojure
(codec codec parser)
```

### either

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (Parser _0 _1) (Parser _0 _1)))
```

Homogeneous alternative combinator\.

```clojure
(either this that)
```

### else

```clojure
(All (_ _0 _1)
  (-> _1 (Parser _0 _1) (Parser _0 _1)))
```

If the given parser fails, returns the default value\.

```clojure
(else value parser)
```

### exactly

```clojure
(All (_ _0 _1)
  (-> .Nat (Parser _0 _1) (Parser _0 (.List _1))))
```

Parse exactly N times\.

```clojure
(exactly amount parser)
```

### failure

```clojure
(All (_ _0 _1)
  (-> .Text (Parser _0 _1)))
```

Always fail with this 'message'\.

```clojure
(failure message)
```

### functor

```clojure
(All (_ _0)
  (library/lux/abstract/functor.Functor (Parser _0)))
```

### lifted

```clojure
(All (_ _0 _1)
  (-> (library/lux/control/try.Try _1) (Parser _0 _1)))
```

Lift a potentially failed computation into a parser\.

```clojure
(lifted operation)
```

### many

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (Parser _0 (.List _1))))
```

1\-or\-more combinator\.

```clojure
(many parser)
```

### maybe

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (Parser _0 (.Maybe _1))))
```

Optionality combinator\.

```clojure
(maybe parser)
```

### monad

```clojure
(All (_ _0)
  (library/lux/abstract/monad.Monad (Parser _0)))
```

### not

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (Parser _0 .Any)))
```

Only succeeds when the underlying parser fails\.

```clojure
(not parser)
```

### only

```clojure
(All (_ _0 _1)
  (-> (-> _1 .Bit) (Parser _0 _1) (Parser _0 _1)))
```

Only succeed when the parser's output passes a test\.

```clojure
(only test parser)
```

### or

```clojure
(All (_ _0 _1 _2)
  (-> (Parser _0 _1) (Parser _0 _2) (Parser _0 (Or _1 _2))))
```

Heterogeneous alternative combinator\.

```clojure
(or left right)
```

### parses

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (Parser _0 .Any)))
```

Ignore a parser's output and just execute it\.

```clojure
(parses parser)
```

### parses?

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (Parser _0 .Bit)))
```

Ignore a parser's output and just verify that it succeeds\.

```clojure
(parses? parser)
```

### rec

```clojure
(All (_ _0 _1)
  (-> (-> (Parser _0 _1) (Parser _0 _1)) (Parser _0 _1)))
```

Combinator for recursive parsers\.

```clojure
(rec parser)
```

### remaining

```clojure
(All (_ _0)
  (Parser _0 _0))
```

Yield the remaining input \(without consuming it\)\.

### result

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) _0 (library/lux/control/try.Try [_0 _1])))
```

Executes the parser on the input\.
Does not verify that all of the input has been consumed by the parser\.
Returns both the parser's output, and a value that represents the remaining input\.

```clojure
(result parser input)
```

### separated\_by

```clojure
(All (_ _0 _1 _2)
  (-> (Parser _0 _2) (Parser _0 _1) (Parser _0 (.List _1))))
```

Parses instances of 'parser' that are separated by instances of 'separator'\.

```clojure
(separated_by separator parser)
```

### some

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (Parser _0 (.List _1))))
```

0\-or\-more combinator\.

```clojure
(some parser)
```

### speculative

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (Parser _0 _1)))
```

Executes a parser, without actually consuming the input\.
That way, the same input can be consumed again by another parser\.

```clojure
(speculative parser)
```

___

# library/lux/control/parser/analysis

## Definitions

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser (.List library/lux/tool/compiler/language/lux/analysis.Analysis))
```

A parser for Lux code analysis nodes\.

### any

```clojure
(Parser library/lux/tool/compiler/language/lux/analysis.Analysis)
```

Matches any value, without discrimination\.

### bit

```clojure
(Parser .Bit)
```

Queries for a bit value\.

### bit\!

```clojure
(-> .Bit (Parser .Any))
```

Assert a specific bit value\.

### cannot\_parse

```clojure
(library/lux/control/exception.Exception (.List library/lux/tool/compiler/language/lux/analysis.Analysis))
```

### constant

```clojure
(Parser .Symbol)
```

Queries for a constant value\.

### constant\!

```clojure
(-> .Symbol (Parser .Any))
```

Assert a specific constant value\.

### end\!

```clojure
(Parser .Any)
```

Ensures there are no more inputs\.

### end?

```clojure
(Parser .Bit)
```

Checks whether there are no more inputs\.

### foreign

```clojure
(Parser .Nat)
```

Queries for a foreign value\.

### foreign\!

```clojure
(-> .Nat (Parser .Any))
```

Assert a specific foreign value\.

### frac

```clojure
(Parser .Frac)
```

Queries for a frac value\.

### frac\!

```clojure
(-> .Frac (Parser .Any))
```

Assert a specific frac value\.

### int

```clojure
(Parser .Int)
```

Queries for a int value\.

### int\!

```clojure
(-> .Int (Parser .Any))
```

Assert a specific int value\.

### local

```clojure
(Parser .Nat)
```

Queries for a local value\.

### local\!

```clojure
(-> .Nat (Parser .Any))
```

Assert a specific local value\.

### nat

```clojure
(Parser .Nat)
```

Queries for a nat value\.

### nat\!

```clojure
(-> .Nat (Parser .Any))
```

Assert a specific nat value\.

### result

```clojure
(All (_ _0)
  (-> (Parser _0) (.List library/lux/tool/compiler/language/lux/analysis.Analysis) (library/lux/control/try.Try _0)))
```

Executes a parser and makes sure no inputs go unconsumed\.

```clojure
(result parser input)
```

### rev

```clojure
(Parser .Rev)
```

Queries for a rev value\.

### rev\!

```clojure
(-> .Rev (Parser .Any))
```

Assert a specific rev value\.

### text

```clojure
(Parser .Text)
```

Queries for a text value\.

### text\!

```clojure
(-> .Text (Parser .Any))
```

Assert a specific text value\.

### tuple

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses only within the context of a tuple's contents\.

```clojure
(tuple parser)
```

### unconsumed\_input

```clojure
(library/lux/control/exception.Exception (.List library/lux/tool/compiler/language/lux/analysis.Analysis))
```

___

# library/lux/control/parser/binary

## Definitions

### Offset

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

An offset for reading within binary data\.

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser [Offset library/lux/data/binary.Binary])
```

A parser for raw binary data\.

### Size

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

The size of a chunk of data within a binary array\.

### any

```clojure
(Parser .Any)
```

Does no parsing, and just returns a dummy value\.

### binary/16

```clojure
(Parser library/lux/data/binary.Binary)
```

Parses a block of data prefixed with a size that is 16 bytes long\.

### binary/32

```clojure
(Parser library/lux/data/binary.Binary)
```

Parses a block of data prefixed with a size that is 32 bytes long\.

### binary/64

```clojure
(Parser library/lux/data/binary.Binary)
```

Parses a block of data prefixed with a size that is 64 bytes long\.

### binary/8

```clojure
(Parser library/lux/data/binary.Binary)
```

Parses a block of data prefixed with a size that is 8 bytes long\.

### binary\_was\_not\_fully\_read

```clojure
(library/lux/control/exception.Exception [.Nat .Nat])
```

### bit

```clojure
(Parser .Bit)
```

### bits/16

```clojure
(Parser .I64)
```

### bits/32

```clojure
(Parser .I64)
```

### bits/64

```clojure
(Parser .I64)
```

### bits/8

```clojure
(Parser .I64)
```

### code

```clojure
(Parser .Code)
```

### end?

```clojure
(Parser .Bit)
```

Checks whether there is no more data to read\.

### frac

```clojure
(Parser .Frac)
```

### int

```clojure
(Parser .Int)
```

### invalid\_tag

```clojure
(library/lux/control/exception.Exception [.Nat .Nat])
```

### list

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser (.List _0))))
```

Parses an arbitrarily long list of values\.

```clojure
(list value)
```

### location

```clojure
(Parser .Location)
```

### maybe

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser (.Maybe _0))))
```

### nat

```clojure
(Parser .Nat)
```

### not\_a\_bit

```clojure
(library/lux/control/exception.Exception .Nat)
```

### offset

```clojure
(Parser Offset)
```

The current offset \(i\.e\. how much data has been read\)\.

### or

```clojure
(All (_ _0 _1)
  (-> (Parser _0) (Parser _1) (Parser (Or _0 _1))))
```

### rec

```clojure
(All (_ _0)
  (-> (-> (Parser _0) (Parser _0)) (Parser _0)))
```

Tie the knot for a recursive parser\.

### remaining

```clojure
(Parser .Nat)
```

How much of the data remains to be read\.

### result

```clojure
(All (_ _0)
  (-> (Parser _0) library/lux/data/binary.Binary (library/lux/control/try.Try _0)))
```

Runs a parser and checks that all the binary data was read by it\.

```clojure
(result parser input)
```

### rev

```clojure
(Parser .Rev)
```

### segment

```clojure
(-> .Nat (Parser library/lux/data/binary.Binary))
```

Parses a chunk of data of a given size\.

```clojure
(segment size)
```

### sequence/16

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser (library/lux/data/collection/sequence.Sequence _0))))
```

Parses a sequence of values prefixed with a size that is 16 bytes long\.

### sequence/32

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser (library/lux/data/collection/sequence.Sequence _0))))
```

Parses a sequence of values prefixed with a size that is 32 bytes long\.

### sequence/64

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser (library/lux/data/collection/sequence.Sequence _0))))
```

Parses a sequence of values prefixed with a size that is 64 bytes long\.

### sequence/8

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser (library/lux/data/collection/sequence.Sequence _0))))
```

Parses a sequence of values prefixed with a size that is 8 bytes long\.

### set

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) (Parser _0) (Parser (library/lux/data/collection/set.Set _0))))
```

```clojure
(set hash value)
```

### set\_elements\_are\_not\_unique

```clojure
(library/lux/control/exception.Exception .Any)
```

### size/16

```clojure
Size
```

### size/32

```clojure
Size
```

### size/64

```clojure
Size
```

### size/8

```clojure
Size
```

### symbol

```clojure
(Parser .Symbol)
```

### text

```clojure
(Parser .Text)
```

### type

```clojure
(Parser .Type)
```

### utf8/16

```clojure
(Parser .Text)
```

Parses a block of \(UTF\-8 encoded\) text prefixed with a size that is 16 bytes long\.

### utf8/32

```clojure
(Parser .Text)
```

Parses a block of \(UTF\-8 encoded\) text prefixed with a size that is 32 bytes long\.

### utf8/64

```clojure
(Parser .Text)
```

Parses a block of \(UTF\-8 encoded\) text prefixed with a size that is 64 bytes long\.

### utf8/8

```clojure
(Parser .Text)
```

Parses a block of \(UTF\-8 encoded\) text prefixed with a size that is 8 bytes long\.

___

# library/lux/control/parser/cli

## Definitions

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser (.List .Text) it)
```

A command\-line interface parser\.

### any

```clojure
(Parser .Text)
```

Just returns the next input without applying any logic\.

### end

```clojure
(Parser .Any)
```

Ensures there are no more inputs\.

### named

```clojure
(All (_ _0)
  (-> .Text (Parser _0) (Parser _0)))
```

Parses a named parameter and yields its value\.

```clojure
(named name value)
```

### parameter

```clojure
(All (_ _0)
  (-> [.Text .Text] (Parser _0) (Parser _0)))
```

Parses a parameter that can have either a short or a long name\.

```clojure
(parameter [short long] value)
```

### parse

```clojure
(All (_ _0)
  (-> (-> .Text (library/lux/control/try.Try _0)) (Parser _0)))
```

Parses the next input with a parsing function\.

```clojure
(parse parser)
```

### result

```clojure
(All (_ _0)
  (-> (Parser _0) (.List .Text) (library/lux/control/try.Try _0)))
```

Executes the parser and verifies that all inputs are processed\.

```clojure
(result parser inputs)
```

### somewhere

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Given a parser, tries to parse it somewhere in the inputs \(i\.e\. not necessarily parsing the immediate inputs\)\.

```clojure
(somewhere cli)
```

### this

```clojure
(-> .Text (Parser .Any))
```

Checks that a token is in the inputs\.

```clojure
(this reference)
```

___

# library/lux/control/parser/code

## Definitions

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser (.List .Code))
```

A Lux code parser\.

### any

```clojure
(Parser .Code)
```

Yields the next input without applying any logic\.

### bit

```clojure
(Parser .Bit)
```

Parses the next bit input\.

### bit\!

```clojure
(-> .Bit (Parser .Any))
```

Checks for a specific bit input\.

### end\!

```clojure
(Parser .Any)
```

Verifies there are no more inputs\.

### end?

```clojure
(Parser .Bit)
```

Checks whether there are no more inputs\.

### form

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses the contents of a form\.

### frac

```clojure
(Parser .Frac)
```

Parses the next frac input\.

### frac\!

```clojure
(-> .Frac (Parser .Any))
```

Checks for a specific frac input\.

### int

```clojure
(Parser .Int)
```

Parses the next int input\.

### int\!

```clojure
(-> .Int (Parser .Any))
```

Checks for a specific int input\.

### local

```clojure
(All (_ _0)
  (-> (.List .Code) (Parser _0) (Parser _0)))
```

Runs parser against the given list of inputs\.

```clojure
(local inputs parser)
```

### local\_symbol

```clojure
(Parser .Text)
```

Parse a local local symbol \(a local symbol that has no module prefix\)\.

### local\_symbol\!

```clojure
(-> .Text (Parser .Any))
```

Checks for a specific local local symbol \(a local symbol that has no module prefix\)\.

### nat

```clojure
(Parser .Nat)
```

Parses the next nat input\.

### nat\!

```clojure
(-> .Nat (Parser .Any))
```

Checks for a specific nat input\.

### next

```clojure
(Parser .Code)
```

Yields the next Code token without consuming it from the input stream\.

### not

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser .Code)))
```

Yields the next Code token if the parser fails\.

```clojure
(not expected_to_fail)
```

### result

```clojure
(All (_ _0)
  (-> (Parser _0) (.List .Code) (library/lux/control/try.Try _0)))
```

Executes a parser against a stream of code, and verifies all the inputs are consumed\.

```clojure
(result parser inputs)
```

### rev

```clojure
(Parser .Rev)
```

Parses the next rev input\.

### rev\!

```clojure
(-> .Rev (Parser .Any))
```

Checks for a specific rev input\.

### symbol

```clojure
(Parser .Symbol)
```

Parses the next symbol input\.

### symbol\!

```clojure
(-> .Symbol (Parser .Any))
```

Checks for a specific symbol input\.

### text

```clojure
(Parser .Text)
```

Parses the next text input\.

### text\!

```clojure
(-> .Text (Parser .Any))
```

Checks for a specific text input\.

### this\!

```clojure
(-> .Code (Parser .Any))
```

Ensures the given Code is the next input\.

```clojure
(this! code)
```

### tuple

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses the contents of a tuple\.

### variant

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses the contents of a variant\.

___

# library/lux/control/parser/environment

## Definitions

### Environment

```clojure
... .Type
(library/lux/data/collection/dictionary.Dictionary Property .Text)
```

An abstraction for environment variables of a program\.

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser Environment it)
```

A parser of environment variables of a program\.

### Property

```clojure
... .Type
(Primitive "#Text")
```

A property in the environment\.

### empty

```clojure
Environment
```

An empty environment\.

### property

```clojure
(-> Property (Parser .Text))
```

```clojure
(property name)
```

### result

```clojure
(All (_ _0)
  (-> (Parser _0) Environment (library/lux/control/try.Try _0)))
```

Executes a parser against the given environment variables\.
Does not check whether all environment variables were parsed, since they're usually an open set\.

```clojure
(result parser environment)
```

### unknown\_property

```clojure
(library/lux/control/exception.Exception Property)
```

___

# library/lux/control/parser/json

## Definitions

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser (.List library/lux/data/format/json.JSON) it)
```

A JSON parser\.

### any

```clojure
(Parser library/lux/data/format/json.JSON)
```

Just returns the JSON input without applying any logic\.

### array

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses the contents of a JSON array\.

```clojure
(array parser)
```

### boolean

```clojure
(Parser library/lux/data/format/json.Boolean)
```

Reads a JSON value as boolean\.

### boolean\!

```clojure
(-> library/lux/data/format/json.Boolean (Parser .Any))
```

Ensures a JSON value is a boolean\.

### boolean?

```clojure
(-> library/lux/data/format/json.Boolean (Parser .Bit))
```

Asks whether a JSON value is a boolean\.

### dictionary

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser (library/lux/data/collection/dictionary.Dictionary .Text _0))))
```

Parses a dictionary\-like JSON object\.

### empty\_input

```clojure
(library/lux/control/exception.Exception .Any)
```

### field

```clojure
(All (_ _0)
  (-> .Text (Parser _0) (Parser _0)))
```

Parses a field inside a JSON object\.
Use this inside the 'object' combinator\.

```clojure
(field field_name parser)
```

### null

```clojure
(Parser library/lux/data/format/json.Null)
```

Reads a JSON value as null\.

### nullable

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser (.Maybe _0))))
```

Enhances parser by adding NULL\-handling\.

```clojure
(nullable parser)
```

### number

```clojure
(Parser library/lux/data/format/json.Number)
```

Reads a JSON value as number\.

### number\!

```clojure
(-> library/lux/data/format/json.Number (Parser .Any))
```

Ensures a JSON value is a number\.

### number?

```clojure
(-> library/lux/data/format/json.Number (Parser .Bit))
```

Asks whether a JSON value is a number\.

### object

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses the contents of a JSON object\.
Use this with the 'field' combinator\.

```clojure
(object parser)
```

### result

```clojure
(All (_ _0)
  (-> (Parser _0) library/lux/data/format/json.JSON (library/lux/control/try.Try _0)))
```

Executes the parser against a JSON object\.
Verifies that all of the JSON was consumed by the parser\.

```clojure
(result parser json)
```

### string

```clojure
(Parser library/lux/data/format/json.String)
```

Reads a JSON value as string\.

### string\!

```clojure
(-> library/lux/data/format/json.String (Parser .Any))
```

Ensures a JSON value is a string\.

### string?

```clojure
(-> library/lux/data/format/json.String (Parser .Bit))
```

Asks whether a JSON value is a string\.

### unconsumed\_input

```clojure
(library/lux/control/exception.Exception (.List library/lux/data/format/json.JSON))
```

### unexpected\_value

```clojure
(library/lux/control/exception.Exception library/lux/data/format/json.JSON)
```

### value\_mismatch

```clojure
(All (_ _0)
  (library/lux/control/exception.Exception [library/lux/data/format/json.JSON library/lux/data/format/json.JSON]))
```

___

# library/lux/control/parser/synthesis

## Definitions

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser (.List library/lux/tool/compiler/language/lux/synthesis.Synthesis))
```

A parser for the Lux compiler's synthesis nodes using during optimization\.

### any

```clojure
(Parser library/lux/tool/compiler/language/lux/synthesis.Synthesis)
```

Yields a synthesis node without subjecting it to any analysis\.

### bit

```clojure
(Parser .Bit)
```

Queries for a bit synthesis node\.

### bit\!

```clojure
(-> .Bit (Parser .Any))
```

Checks for a specific bit synthesis node\.

### cannot\_parse

```clojure
(library/lux/control/exception.Exception (.List library/lux/tool/compiler/language/lux/synthesis.Synthesis))
```

### constant

```clojure
(Parser .Symbol)
```

Queries for a constant synthesis node\.

### constant\!

```clojure
(-> .Symbol (Parser .Any))
```

Checks for a specific constant synthesis node\.

### empty\_input

```clojure
(library/lux/control/exception.Exception .Any)
```

### end\!

```clojure
(Parser .Any)
```

Ensures there are no more inputs\.

### end?

```clojure
(Parser .Bit)
```

Checks whether there are no more inputs\.

### expected\_empty\_input

```clojure
(library/lux/control/exception.Exception (.List library/lux/tool/compiler/language/lux/synthesis.Synthesis))
```

### f64

```clojure
(Parser .Frac)
```

Queries for a f64 synthesis node\.

### f64\!

```clojure
(-> .Frac (Parser .Any))
```

Checks for a specific f64 synthesis node\.

### foreign

```clojure
(Parser .Nat)
```

Queries for a foreign synthesis node\.

### foreign\!

```clojure
(-> .Nat (Parser .Any))
```

Checks for a specific foreign synthesis node\.

### function

```clojure
(All (_ _0)
  (-> library/lux/tool/compiler/arity.Arity (Parser _0) (Parser [(library/lux/tool/compiler/language/lux/analysis.Environment library/lux/tool/compiler/language/lux/synthesis.Synthesis) _0])))
```

Parses the body of a function with the 'expected' arity\.

```clojure
(function expected parser)
```

### i64

```clojure
(Parser (.I64 .Any))
```

Queries for a i64 synthesis node\.

### i64\!

```clojure
(-> (.I64 .Any) (Parser .Any))
```

Checks for a specific i64 synthesis node\.

### local

```clojure
(Parser .Nat)
```

Queries for a local synthesis node\.

### local\!

```clojure
(-> .Nat (Parser .Any))
```

Checks for a specific local synthesis node\.

### loop

```clojure
(All (_ _0 _1)
  (-> (Parser _0) (Parser _1) (Parser [library/lux/tool/compiler/reference/variable.Register _0 _1])))
```

Parses the initial values and the body of a loop\.

```clojure
(loop init_parsers iteration_parser)
```

### result

```clojure
(All (_ _0)
  (-> (Parser _0) (.List library/lux/tool/compiler/language/lux/synthesis.Synthesis) (library/lux/control/try.Try _0)))
```

Executes the parser against the inputs\.
Ensures all inputs are consumed by the parser\.

```clojure
(result parser input)
```

### text

```clojure
(Parser .Text)
```

Queries for a text synthesis node\.

### text\!

```clojure
(-> .Text (Parser .Any))
```

Checks for a specific text synthesis node\.

### tuple

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses the contents of a tuple\.

```clojure
(tuple parser)
```

### unconsumed\_input

```clojure
(library/lux/control/exception.Exception (.List library/lux/tool/compiler/language/lux/synthesis.Synthesis))
```

### wrong\_arity

```clojure
(library/lux/control/exception.Exception [library/lux/tool/compiler/arity.Arity library/lux/tool/compiler/arity.Arity])
```

___

# library/lux/control/parser/text

## Definitions

### Offset

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

An offset into a block of text\.

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser [Offset .Text])
```

A parser for text\.

### Slice

```clojure
... .Type
(Record
 [#basis Offset
  #distance Offset])
```

A slice of a block of text\.

### alpha

```clojure
(Parser .Text)
```

Yields alphabetic characters\.

### alpha\_num

```clojure
(Parser .Text)
```

Yields alphanumeric characters\.

### and

```clojure
(-> (Parser .Text) (Parser .Text) (Parser .Text))
```

Yields the outputs of both parsers composed together\.

```clojure
(and left right)
```

### and\!

```clojure
(-> (Parser Slice) (Parser Slice) (Parser Slice))
```

Yields the outputs of both parsers composed together \(as a slice\)\.

```clojure
(and! left right)
```

### any

```clojure
(Parser .Text)
```

Yields the next character without applying any logic\.

### any\!

```clojure
(Parser Slice)
```

Yields the next character \(as a slice\) without applying any logic\.

### at\_least

```clojure
(-> .Nat (Parser .Text) (Parser .Text))
```

Yields at least N characters\.

### at\_least\!

```clojure
(-> .Nat (Parser Slice) (Parser Slice))
```

Yields at least N characters \(as a slice\)\.

### at\_most

```clojure
(-> .Nat (Parser .Text) (Parser .Text))
```

Yields at most N characters\.

### at\_most\!

```clojure
(-> .Nat (Parser Slice) (Parser Slice))
```

Yields at most N characters \(as a slice\)\.

### between

```clojure
(-> .Nat .Nat (Parser .Text) (Parser .Text))
```

```clojure
(between minimum additional parser)
```

### between\!

```clojure
(-> .Nat .Nat (Parser Slice) (Parser Slice))
```

```clojure
(between! minimum additional parser)
```

### cannot\_match

```clojure
(library/lux/control/exception.Exception .Text)
```

### cannot\_parse

```clojure
(library/lux/control/exception.Exception .Any)
```

### cannot\_slice

```clojure
(library/lux/control/exception.Exception .Any)
```

### character\_does\_not\_satisfy\_predicate

```clojure
(library/lux/control/exception.Exception library/lux/data/text.Char)
```

### character\_should\_be

```clojure
(library/lux/control/exception.Exception [.Text library/lux/data/text.Char])
```

### character\_should\_not\_be

```clojure
(library/lux/control/exception.Exception [.Text library/lux/data/text.Char])
```

### decimal

```clojure
(Parser .Text)
```

Only yields decimal characters\.

### enclosed

```clojure
(All (_ _0)
  (-> [.Text .Text] (Parser _0) (Parser _0)))
```

```clojure
(enclosed [start end] parser)
```

### end\!

```clojure
(Parser .Any)
```

Ensure the parser's input is empty\.

### exactly

```clojure
(-> .Nat (Parser .Text) (Parser .Text))
```

Yields exactly N characters\.

### exactly\!

```clojure
(-> .Nat (Parser Slice) (Parser Slice))
```

Yields exactly N characters \(as a slice\)\.

### expected\_to\_fail

```clojure
(library/lux/control/exception.Exception [Offset .Text])
```

### hexadecimal

```clojure
(Parser .Text)
```

Yields hexadecimal digits\.

### local

```clojure
(All (_ _0)
  (-> .Text (Parser _0) (Parser _0)))
```

Applies a parser against the given input\.

```clojure
(local local_input parser)
```

### lower

```clojure
(Parser .Text)
```

Only yields lowercase characters\.

### many

```clojure
(-> (Parser .Text) (Parser .Text))
```

Yields <name> characters as a single continuous text\.

### many\!

```clojure
(-> (Parser Slice) (Parser Slice))
```

Yields <name> characters as a single continuous text \(as a slice\)\.

### next

```clojure
(Parser .Text)
```

Yields the next character \(without consuming it from the input\)\.

### none\_of

```clojure
(-> .Text (Parser .Text))
```

Yields characters that are not part of a piece of text\.

### none\_of\!

```clojure
(-> .Text (Parser Slice))
```

Yields characters \(as a slice\) that are not part of a piece of text\.

### not

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser .Text)))
```

Produce a character if the parser fails\.

### not\!

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser Slice)))
```

Produce a character \(as a slice\) if the parser fails\.

### octal

```clojure
(Parser .Text)
```

Only yields octal characters\.

### offset

```clojure
(Parser Offset)
```

Yields the current offset into the input\.

### one\_of

```clojure
(-> .Text (Parser .Text))
```

Yields characters that are part of a piece of text\.

### one\_of\!

```clojure
(-> .Text (Parser Slice))
```

Yields characters \(as a slice\) that are part of a piece of text\.

### range

```clojure
(-> .Nat .Nat (Parser .Text))
```

Only yields characters within a range\.

```clojure
(range bottom top)
```

### remaining

```clojure
(Parser .Text)
```

Get all of the remaining input \(without consuming it\)\.

### result

```clojure
(All (_ _0)
  (-> (Parser _0) .Text (library/lux/control/try.Try _0)))
```

Executes a parser against a block of text\.
Verifies that the entire input has been processed\.

```clojure
(result parser input)
```

### satisfies

```clojure
(-> (-> library/lux/data/text.Char .Bit) (Parser .Text))
```

Yields characters that satisfy a predicate\.

```clojure
(satisfies parser)
```

### slice

```clojure
(-> (Parser Slice) (Parser .Text))
```

Converts a slice to a block of text\.

```clojure
(slice parser)
```

### some

```clojure
(-> (Parser .Text) (Parser .Text))
```

Yields <name> characters as a single continuous text\.

### some\!

```clojure
(-> (Parser Slice) (Parser Slice))
```

Yields <name> characters as a single continuous text \(as a slice\)\.

### space

```clojure
(Parser .Text)
```

Yields white\-space\.

### then

```clojure
(All (_ _0 _1)
  (-> (Parser _1) (library/lux/control/parser.Parser _0 .Text) (library/lux/control/parser.Parser _0 _1)))
```

Embeds a text parser into an arbitrary parser that yields text\.

```clojure
(then structured text)
```

### this

```clojure
(-> .Text (Parser .Any))
```

Checks that a specific text shows up in the input\.

```clojure
(this reference)
```

### unconsumed\_input

```clojure
(library/lux/control/exception.Exception [Offset .Text])
```

### upper

```clojure
(Parser .Text)
```

Only yields uppercase characters\.

___

# library/lux/control/parser/tree

## Definitions

### \(Parser it\)

```clojure
... .Type
(All (_ _0)
  (library/lux/control/parser.Parser (library/lux/data/collection/tree/zipper.Zipper it) _0))
```

A parser of arbitrary trees\.

### cannot\_move\_further

```clojure
(library/lux/control/exception.Exception .Any)
```

### down

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move down\.

### end

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move to the last node\.

### left

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move to the left\.

### leftmost

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move to the leftmost node\.

### next

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move to the next node\.

### previous

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move to the previous node\.

### result

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (library/lux/data/collection/tree.Tree _0) (library/lux/control/try.Try _1)))
```

Applies the parser against a tree\.

```clojure
(result parser tree)
```

### result'

```clojure
(All (_ _0 _1)
  (-> (Parser _0 _1) (library/lux/data/collection/tree/zipper.Zipper _0) (library/lux/control/try.Try _1)))
```

Applies the parser against a tree zipper\.

```clojure
(result' parser zipper)
```

### right

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move to the right\.

### rightmost

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move to the rightmost node\.

### start

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move to the root node\.

### up

```clojure
(All (_ _0)
  (Parser _0 .Any))
```

Move up\.

### value

```clojure
(All (_ _0)
  (Parser _0 _0))
```

Yields the value inside the current tree node\.

___

# library/lux/control/parser/type

Parsing of Lux types\.
Used mostly for polytypic programming\.

## Definitions

### Env

```clojure
... .Type
(library/lux/data/collection/dictionary.Dictionary .Nat [.Type .Code])
```

An environment for type parsing\.

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser [Env (.List .Type)] it)
```

A parser of Lux types\.

### adjusted\_idx

```clojure
(-> Env .Nat .Nat)
```

```clojure
(adjusted_idx env idx)
```

### any

```clojure
(Parser .Type)
```

Yields a type, without examination\.

### applied

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses a type application\.

```clojure
(applied poly)
```

### empty\_input

```clojure
(library/lux/control/exception.Exception .Any)
```

### env

```clojure
(Parser Env)
```

Yields the current parsing environment\.

### exactly

```clojure
(-> .Type (Parser .Any))
```

Parses a type exactly\.

### existential

```clojure
(Parser .Nat)
```

Yields an existential type\.

### fresh

```clojure
Env
```

An empty parsing environment\.

### function

```clojure
(All (_ _0 _1)
  (-> (Parser _0) (Parser _1) (Parser [_0 _1])))
```

Parses a function's inputs and output\.

```clojure
(function in_poly out_poly)
```

### local

```clojure
(All (_ _0)
  (-> (.List .Type) (Parser _0) (Parser _0)))
```

Apply a parser to the given inputs\.

```clojure
(local types poly)
```

### named

```clojure
(Parser [.Symbol .Type])
```

Yields a named type\.

### next

```clojure
(Parser .Type)
```

Inspect a type in the input stream without consuming it\.

### not\_application

```clojure
(library/lux/control/exception.Exception .Type)
```

### not\_existential

```clojure
(library/lux/control/exception.Exception .Type)
```

### not\_function

```clojure
(library/lux/control/exception.Exception .Type)
```

### not\_named

```clojure
(library/lux/control/exception.Exception .Type)
```

### not\_parameter

```clojure
(library/lux/control/exception.Exception .Type)
```

### not\_polymorphic

```clojure
(library/lux/control/exception.Exception .Type)
```

### not\_recursive

```clojure
(library/lux/control/exception.Exception .Type)
```

### not\_tuple

```clojure
(library/lux/control/exception.Exception .Type)
```

### not\_variant

```clojure
(library/lux/control/exception.Exception .Type)
```

### parameter

```clojure
(Parser .Code)
```

### parameter\!

```clojure
(-> .Nat (Parser .Any))
```

```clojure
(parameter! id)
```

### polymorphic

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser [.Code (.List .Code) _0])))
```

```clojure
(polymorphic poly)
```

### recursive

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser [.Code _0])))
```

```clojure
(recursive poly)
```

### recursive\_call

```clojure
(Parser .Code)
```

### recursive\_self

```clojure
(Parser .Code)
```

### result

```clojure
(All (_ _0)
  (-> (Parser _0) .Type (library/lux/control/try.Try _0)))
```

Applies a parser against a type\.
Verifies that the parser fully consumes the type's information\.

```clojure
(result poly type)
```

### sub

```clojure
(-> .Type (Parser .Any))
```

Parses a sub type\.

### super

```clojure
(-> .Type (Parser .Any))
```

Parses a super type\.

### tuple

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses the contents of a tuple type\.

### types\_do\_not\_match

```clojure
(library/lux/control/exception.Exception [.Type .Type])
```

### unconsumed\_input

```clojure
(library/lux/control/exception.Exception (.List .Type))
```

### unknown\_parameter

```clojure
(library/lux/control/exception.Exception .Type)
```

### variant

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Parses the contents of a variant type\.

### with\_extension

```clojure
(All (_ _0)
  (-> .Type (Parser _0) (Parser [.Code _0])))
```

```clojure
(with_extension type poly)
```

### wrong\_parameter

```clojure
(library/lux/control/exception.Exception [.Type .Type])
```

___

# library/lux/control/parser/xml

## Definitions

### \(Parser it\)

```clojure
... .Type
(library/lux/control/parser.Parser [library/lux/data/format/xml.Attrs (.List library/lux/data/format/xml.XML)] it)
```

A parser of XML\-encoded data\.

### any

```clojure
(Parser library/lux/data/format/xml.XML)
```

Yields the next node\.

### attribute

```clojure
(-> library/lux/data/format/xml.Attribute (Parser .Text))
```

Yields the value of an attribute in the current node\.

```clojure
(attribute name)
```

### empty\_input

```clojure
(library/lux/control/exception.Exception .Any)
```

### node

```clojure
(All (_ _0)
  (-> library/lux/data/format/xml.Tag (Parser _0) (Parser _0)))
```

Parses the contents of the next node if the tag matches\.

```clojure
(node expected parser)
```

### nowhere

```clojure
(library/lux/control/exception.Exception .Any)
```

### result

```clojure
(All (_ _0)
  (-> (Parser _0) (.List library/lux/data/format/xml.XML) (library/lux/control/try.Try _0)))
```

Applies a parser against a stream of XML documents\.
Verifies that all of the inputs are consumed by the parser\.

```clojure
(result parser documents)
```

### somewhere

```clojure
(All (_ _0)
  (-> (Parser _0) (Parser _0)))
```

Applies the parser somewhere among the remaining inputs; instead of demanding that the parser succeeds against the immediate inputs\.

```clojure
(somewhere parser)
```

### tag

```clojure
(Parser library/lux/data/format/xml.Tag)
```

Yields the tag from the next node\.

### text

```clojure
(Parser .Text)
```

Yields text from a text node\.

### unconsumed\_inputs

```clojure
(library/lux/control/exception.Exception (.List library/lux/data/format/xml.XML))
```

### unexpected\_input

```clojure
(library/lux/control/exception.Exception .Any)
```

### unknown\_attribute

```clojure
(library/lux/control/exception.Exception [library/lux/data/format/xml.Attribute (.List library/lux/data/format/xml.Attribute)])
```

### wrong\_tag

```clojure
(library/lux/control/exception.Exception [library/lux/data/format/xml.Tag library/lux/data/format/xml.Tag])
```

___

# library/lux/control/pipe

Composable extensions to the piping macros \(|> and <|\) that enhance them with various abilities\.

## Definitions

### case>

```clojure
.Macro
```

Pattern\-matching for pipes\.
The bodies of each branch are NOT pipes; just regular values\.

```clojure
(|> +5
    (case> +0 "zero"
           +1 "one"
           +2 "two"
           +3 "three"
           +4 "four"
           +5 "five"
           +6 "six"
           +7 "seven"
           +8 "eight"
           +9 "nine"
           _ "???"))
```

### cond>

```clojure
.Macro
```

Branching for pipes\.
Both the tests and the bodies are piped\-code, and must be given inside a tuple\.

```clojure
(|> +5
    (cond> [i.even?] [(i.* +2)]
           [i.odd?] [(i.* +3)]
           [(new> -1 [])]))
```

### do>

```clojure
.Macro
```

Monadic pipes\.
Each steps in the monadic computation is a pipe and must be given inside a tuple\.

```clojure
(|> +5
    (do> identity.monad
         [(i.* +3)]
         [(i.+ +4)]
         [++]))
```

### exec>

```clojure
.Macro
```

Non\-updating pipes\.
Will generate piped computations, but their results will not be used in the larger scope\.

```clojure
(|> +5
    (exec> [.nat %n log!])
    (i.* +10))
```

### if>

```clojure
.Macro
```

If\-branching\.

```clojure
(same? (if (n.even? sample)
         "even"
         "odd")
       (|> sample
           (if> [n.even?]
                [(new> "even" [])]
                [(new> "odd" [])])))
```

### let>

```clojure
.Macro
```

Gives a name to the piped\-argument, within the given expression\.

```clojure
(n.= 10
     (|> 5
         (let> x (n.+ x x))))
```

### loop>

```clojure
.Macro
```

Loops for pipes\.
Both the testing and calculating steps are pipes and must be given inside tuples\.

```clojure
(|> +1
    (loop> [(i.< +10)]
           [++]))
```

### new>

```clojure
.Macro
```

Ignores the piped argument, and begins a new pipe\.

```clojure
(n.= 1
     (|> 20
         (n.* 3)
         (n.+ 4)
         (new> 0 [++])))
```

### tuple>

```clojure
.Macro
```

Parallel branching for pipes\.
Allows to run multiple pipelines for a value and gives you a tuple of the outputs\.

```clojure
(|> +5
    (tuple> [(i.* +10)]
            [-- (i./ +2)]
            [i#encoded]))

... =>

[+50 +2 "+5"]
```

### when>

```clojure
.Macro
```

Only execute the body when the test passes\.

```clojure
(same? (if (n.even? sample)
         (n.* 2 sample)
         sample)
       (|> sample
           (when> [n.even?]
                  [(n.* 2)])))
```

___

# library/lux/control/reader

## Definitions

### \(Reader environment it\)

```clojure
... .Type
(-> environment it)
```

Computations that have access to some environmental value\.

### apply

```clojure
(All (_ _0)
  (library/lux/abstract/apply.Apply (Reader _0)))
```

### functor

```clojure
(All (_ _0)
  (library/lux/abstract/functor.Functor (Reader _0)))
```

### lifted

```clojure
(All (_ _0 _1 _2)
  (-> (_0 _2) (Reader _1 (_0 _2))))
```

Lift monadic values to the Reader wrapper\.

### local

```clojure
(All (_ _0 _1)
  (-> (-> _0 _0) (Reader _0 _1) (Reader _0 _1)))
```

Run computation with a locally\-modified environment\.

```clojure
(local change proc)
```

### monad

```clojure
(All (_ _0)
  (library/lux/abstract/monad.Monad (Reader _0)))
```

### read

```clojure
(All (_ _0)
  (Reader _0 _0))
```

Get the environment\.

### result

```clojure
(All (_ _0 _1)
  (-> _0 (Reader _0 _1) _1))
```

Executes the reader against the given environment\.

```clojure
(result env proc)
```

### with

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (All (_ _1) (library/lux/abstract/monad.Monad (All (_ _2) (Reader _1 (_0 _2)))))))
```

Monad transformer for Reader\.

```clojure
(with monad)
```

___

# library/lux/control/region

## Definitions

### \(Region r \! it\)

```clojure
... .Type
(-> [r (.List (Cleaner r !))] (! [(.List (Cleaner r !)) (library/lux/control/try.Try it)]))
```

A region where resources may be be claimed and where a side\-effecting computation may be performed\.
Every resource is paired with a function that knows how to clean/reclaim it, to make sure there are no leaks\.

### acquire\!

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (-> _1 (_0 (library/lux/control/try.Try .Any))) _1 (All (_ _2) (Region _2 _0 _1))))
```

Acquire a resource while pairing it a function that knows how to reclaim it\.

```clojure
(acquire! monad cleaner value)
```

### apply

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (All (_ _1) (library/lux/abstract/apply.Apply (Region _1 _0)))))
```

### clean\_up\_error

```clojure
(All (_ _0)
  (library/lux/control/exception.Exception [.Text (library/lux/control/try.Try _0)]))
```

### except

```clojure
(All (_ _0 _1 _2)
  (-> (library/lux/abstract/monad.Monad _0) (library/lux/control/exception.Exception _1) _1 (All (_ _3) (Region _3 _0 _2))))
```

Fail by throwing/raising an exception\.

```clojure
(except monad exception message)
```

### failure

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) .Text (All (_ _2) (Region _2 _0 _1))))
```

Immediately fail with this 'message'\.

```clojure
(failure monad error)
```

### functor

```clojure
(All (_ _0)
  (-> (library/lux/abstract/functor.Functor _0) (All (_ _1) (library/lux/abstract/functor.Functor (Region _1 _0)))))
```

### lifted

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (_0 _1) (All (_ _2) (Region _2 _0 _1))))
```

Lift an effectful computation into a region\-based computation\.

```clojure
(lifted monad operation)
```

### monad

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (All (_ _1) (library/lux/abstract/monad.Monad (Region _1 _0)))))
```

### run\!

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (All (_ _2) (Region _2 _0 _1)) (_0 (library/lux/control/try.Try _1))))
```

Executes a region\-based computation, with a side\-effect determined by the monad\.

```clojure
(run! monad computation)
```

___

# library/lux/control/remember

## Definitions

### fix\_me

```clojure
.Macro
```

A FIXME message with an expiration date\.
Can have an optional piece of code to focus on\.

```clojure
(fix_me "2022-04-01"
  "Do this, that and the other.")

................................................................
................................................................

(fix_me "2022-04-01"
  "Improve the performace."
  (some (complicated (computation 123))))
```

### must\_remember

```clojure
(library/lux/control/exception.Exception [library/lux/time/date.Date library/lux/time/date.Date .Text (.Maybe .Code)])
```

### remember

```clojure
.Macro
```

A message with an expiration date\.
Can have an optional piece of code to focus on\.

```clojure
(remember "2022-04-01"
  "Do this, that and the other.")

................................................................
................................................................

(remember "2022-04-01"
  "Improve the performace."
  (some (complicated (computation 123))))
```

### to\_do

```clojure
.Macro
```

A TODO message with an expiration date\.
Can have an optional piece of code to focus on\.

```clojure
(to_do "2022-04-01"
  "Do this, that and the other.")

................................................................
................................................................

(to_do "2022-04-01"
  "Improve the performace."
  (some (complicated (computation 123))))
```

___

# library/lux/control/security/capability

## Definitions

### \(Capability brand input output\)

```clojure
... .Type
(Primitive "library/lux/control/security/capability.Capability" brand input output)
```

Represents the capability to perform an operation\.
This operation is assumed to have security implications\.

### async

```clojure
(All (_ _0 _1 _2)
  (-> (Capability _0 _1 (library/lux/control/io.IO _2)) (Capability _0 _1 (library/lux/control/concurrency/async.Async _2))))
```

Converts a synchronous I/O\-based capability into an asynchronous capability\.

```clojure
(async capability)
```

### capability:

```clojure
.Macro
```

Defines a capability as a unique type, and a constructor for instances\.

```clojure
(capability: (Can_Duplicate a)
  (can_duplicate a [a a]))

(let [capability (can_duplicate
                  (function (_ value)
                    [value value]))
      [left right] (documentation/lux/control/security/capability.usecapability 123)]
  (same? left right))
```

### use

```clojure
(All (_ _0 _1 _2)
  (-> (Capability _0 _1 _2) _1 _2))
```

Applies a capability against its required input\.

```clojure
(use capability input)
```

___

# library/lux/control/security/policy

## Definitions

### Can\_Conceal

```clojure
... .Type
(Can_Upgrade Privacy)
```

### Can\_Distrust

```clojure
... .Type
(Can_Downgrade Safety)
```

### \(Can\_Downgrade brand % value\)

```clojure
... .Type
(-> (Policy brand value %) value)
```

Represents the capacity to 'downgrade' a value\.

### Can\_Reveal

```clojure
... .Type
(Can_Downgrade Privacy)
```

### Can\_Trust

```clojure
... .Type
(Can_Upgrade Safety)
```

### \(Can\_Upgrade brand % value\)

```clojure
... .Type
(-> value (Policy brand value %))
```

Represents the capacity to 'upgrade' a value\.

### \(Context brand scope %\)

```clojure
... .Type
(-> (Privilege brand %) (scope %))
```

A computational context with an associated policy privilege\.

### \(Delegation brand %from %to\)

```clojure
... .Type
(All (_ _0)
  (-> (Policy brand _0 %from) (Policy brand _0 %to)))
```

Represents the act of delegating policy capacities\.

### \(Policy brand value %\)

```clojure
... .Type
(Primitive "library/lux/control/security/policy.Policy" brand value %)
```

A security policy encoded as the means to 'upgrade' or 'downgrade' in a secure context\.

### Privacy

```clojure
... .Type
(Primitive "library/lux/control/security/policy.Privacy")
```

A security context for privacy\.
Private data is data which cannot be allowed to leak outside of the programmed\.

### Private

```clojure
... .Type
(Policy Privacy)
```

### \(Privilege brand %\)

```clojure
... .Type
(Record
 [#can_upgrade (Can_Upgrade brand %)
  #can_downgrade (Can_Downgrade brand %)])
```

Represents the privilege to both 'upgrade' and 'downgrade' a value\.

### Safe

```clojure
... .Type
(Policy Safety)
```

### Safety

```clojure
... .Type
(Primitive "library/lux/control/security/policy.Safety")
```

A security context for safety\.
Safe data is data coming from outside the program which can be trusted to be properly formatted and lacking injections\.

### apply

```clojure
(All (_ _0 _1)
  (library/lux/abstract/apply.Apply (All (_ _2) (Policy _0 _2 _1))))
```

### delegation

```clojure
(All (_ _0 _1 _2)
  (-> (Can_Downgrade _0 _1) (Can_Upgrade _0 _2) (Delegation _0 _1 _2)))
```

Delegating policy capacities\.

```clojure
(delegation downgrade upgrade)
```

### functor

```clojure
(All (_ _0 _1)
  (library/lux/abstract/functor.Functor (All (_ _2) (Policy _0 _2 _1))))
```

### monad

```clojure
(All (_ _0 _1)
  (library/lux/abstract/monad.Monad (All (_ _2) (Policy _0 _2 _1))))
```

### with\_policy

```clojure
(All (_ _0 _1)
  (Ex (_ _2)
    (-> (Context _0 _1 _2) (_1 _2))))
```

Activates a security context with the priviledge to enforce it's policy\.

```clojure
(type: Password
  (Private Text))

(type: (Policy %)
  (Interface
   (: (-> Text (Password %))
      password)
   (: (-> (Password %) Text)
      unsafe)))

(def: (policy _)
  (Ex (_ %) (-> Any (Policy %)))
  (with_policy
    (: (Context Privacy Policy)
       (function (_ (^open "%::."))
         (implementation
          (def: (password value)
            (%::can_upgrade value))
          (def: (unsafe password)
            (%::can_downgrade password)))))))

................................................................
................................................................

(with_policy context)
```

___

# library/lux/control/state

## Definitions

### \+State

```clojure
... .Type
(All (+State _0 _1 _2)
  (-> _1 (_0 [_1 _2])))
```

Stateful computations decorated by a monad\.

### \(State state it\)

```clojure
... .Type
(-> state [state it])
```

Stateful computations\.

### apply

```clojure
(All (_ _0)
  (library/lux/abstract/apply.Apply (State _0)))
```

### do\_while

```clojure
(All (_ _0)
  (-> (State _0 .Bit) (State _0 .Any) (State _0 .Any)))
```

A stateful do\-while loop\.

```clojure
(do_while condition body)
```

### functor

```clojure
(All (_ _0)
  (library/lux/abstract/functor.Functor (State _0)))
```

### get

```clojure
(All (_ _0)
  (State _0 _0))
```

Read the current state\.

### lifted

```clojure
(All (_ _0 _1 _2)
  (-> (library/lux/abstract/monad.Monad _0) (_0 _2) (+State _0 _1 _2)))
```

Lift monadic values to the \+State wrapper\.

```clojure
(lifted monad ma)
```

### local

```clojure
(All (_ _0 _1)
  (-> (-> _0 _0) (State _0 _1) (State _0 _1)))
```

Run the computation with a locally\-modified state\.

```clojure
(local change action)
```

### monad

```clojure
(All (_ _0)
  (library/lux/abstract/monad.Monad (State _0)))
```

### put

```clojure
(All (_ _0)
  (-> _0 (State _0 .Any)))
```

Set the new state\.

```clojure
(put new_state)
```

### result

```clojure
(All (_ _0 _1)
  (-> _0 (State _0 _1) [_0 _1]))
```

Run a stateful computation\.

```clojure
(result state action)
```

### result'

```clojure
(All (_ _0 _1 _2)
  (-> _1 (+State _0 _1 _2) (_0 [_1 _2])))
```

Execute a stateful computation decorated by a monad\.

```clojure
(result' state action)
```

### update

```clojure
(All (_ _0)
  (-> (-> _0 _0) (State _0 .Any)))
```

Compute the new state\.

```clojure
(update change)
```

### use

```clojure
(All (_ _0 _1)
  (-> (-> _0 _1) (State _0 _1)))
```

Run a function on the current state\.

```clojure
(use user)
```

### while

```clojure
(All (_ _0)
  (-> (State _0 .Bit) (State _0 .Any) (State _0 .Any)))
```

A stateful while loop\.

```clojure
(while condition body)
```

### with

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (library/lux/abstract/monad.Monad (+State _0 _1))))
```

A monad transformer to create composite stateful computations\.

```clojure
(with monad)
```

___

# library/lux/control/thread

## Definitions

### \(Box \! it\)

```clojure
... .Type
(Primitive "library/lux/control/thread.Box" ! it)
```

A mutable box holding a value\.

### \(Thread \! it\)

```clojure
... .Type
(-> ! it)
```

An imperative process with access to mutable values\.

### apply

```clojure
(All (_ _0)
  (library/lux/abstract/apply.Apply (Thread _0)))
```

### box

```clojure
(All (_ _0)
  (-> _0 (All (_ _1) (Thread _1 (Box _1 _0)))))
```

A brand\-new box initialized to the given value\.

```clojure
(box init)
```

### functor

```clojure
(All (_ _0)
  (library/lux/abstract/functor.Functor (Thread _0)))
```

### io

```clojure
(All (_ _0)
  (-> (All (_ _1) (Thread _1 _0)) (library/lux/control/io.IO _0)))
```

Transforms the imperative thread into an I/O computation\.

### monad

```clojure
(All (_ _0)
  (library/lux/abstract/monad.Monad (Thread _0)))
```

### read\!

```clojure
(All (_ _0 _1)
  (-> (Box _0 _1) (Thread _0 _1)))
```

Reads the current value in the box\.

```clojure
(read! box)
```

### result

```clojure
(All (_ _0)
  (-> (All (_ _1) (Thread _1 _0)) _0))
```

Executes the imperative thread in a self\-contained way\.

```clojure
(result thread)
```

### update\!

```clojure
(All (_ _0 _1)
  (-> (-> _0 _0) (Box _1 _0) (Thread _1 _0)))
```

Update a box's value by applying a function to it\.

```clojure
(update! f box)
```

### write\!

```clojure
(All (_ _0)
  (-> _0 (All (_ _1) (-> (Box _1 _0) (Thread _1 .Any)))))
```

Mutates the value in the box\.

```clojure
(write! value box)
```

___

# library/lux/control/try

## Definitions

### \(Try it\)

```clojure
... .Type
(Variant
 {#Failure .Text}
 {#Success it})
```

A computation that can fail with an error message\.

### apply

```clojure
(library/lux/abstract/apply.Apply Try)
```

### else

```clojure
.Macro
```

Allows you to provide a default value that will be used
if a \(Try x\) value turns out to be \#Failure\.
Note: the expression for the default value will not be computed if the base computation succeeds\.

```clojure
(= "bar"
   (else "foo" {#Success   "bar"}))

................................................................
................................................................

(= "foo"
   (else "foo" {#Failure   "KABOOM!"}))
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Try _0))))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Try)
```

### lifted

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (_0 _1) (_0 (Try _1))))
```

Wraps a monadic value with error\-handling machinery\.

```clojure
(lifted monad)
```

### maybe

```clojure
(All (_ _0)
  (-> (Try _0) (.Maybe _0)))
```

```clojure
(maybe try)
```

### monad

```clojure
(library/lux/abstract/monad.Monad Try)
```

### of\_maybe

```clojure
(All (_ _0)
  (-> (.Maybe _0) (Try _0)))
```

```clojure
(of_maybe maybe)
```

### trusted

```clojure
(All (_ _0)
  (-> (Try _0) _0))
```

Assumes a Try value succeeded, and yields its value\.
If it didn't, raises the error as a runtime error\.
WARNING: Use with caution\.

```clojure
(trusted try)
```

### with

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (library/lux/abstract/monad.Monad (All (_ _1) (_0 (Try _1))))))
```

Enhances a monad with error\-handling functionality\.

```clojure
(with monad)
```

___

# library/lux/control/writer

## Definitions

### \(Writer log value\)

```clojure
... .Type
(Record
 [#log log
  #value value])
```

Represents a value with an associated 'log' to record arbitrary information\.

### apply

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monoid.Monoid _0) (library/lux/abstract/apply.Apply (Writer _0))))
```

### functor

```clojure
(All (_ _0)
  (library/lux/abstract/functor.Functor (Writer _0)))
```

### lifted

```clojure
(All (_ _0 _1 _2)
  (-> (library/lux/abstract/monoid.Monoid _0) (library/lux/abstract/monad.Monad _1) (_1 _2) (_1 (Writer _0 _2))))
```

Wraps a monadic value with Writer machinery\.

```clojure
(lifted monoid monad)
```

### monad

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monoid.Monoid _0) (library/lux/abstract/monad.Monad (Writer _0))))
```

### with

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monoid.Monoid _0) (library/lux/abstract/monad.Monad _1) (library/lux/abstract/monad.Monad (All (_ _2) (_1 (Writer _0 _2))))))
```

Enhances a monad with Writer functionality\.

```clojure
(with monoid monad)
```

### write

```clojure
(All (_ _0)
  (-> _0 (Writer _0 .Any)))
```

Write a value to the log\.

```clojure
(write message)
```

___

# library/lux/data/binary

## Definitions

### Binary

```clojure
... .Type
(library/lux/ffi.Object (Primitive "Uint8Array"))
```

A binary BLOB of data\.

### after

```clojure
(-> .Nat Binary Binary)
```

Yields a binary BLOB with at most the specified number of bytes removed\.

```clojure
(after bytes binary)
```

### aggregate

```clojure
(All (_ _0)
  (-> (-> .I64 _0 _0) _0 Binary _0))
```

```clojure
(aggregate f init binary)
```

### copy

```clojure
(-> .Nat .Nat Binary .Nat Binary (library/lux/control/try.Try Binary))
```

Mutates the target binary BLOB by copying bytes from the source BLOB to it\.

```clojure
(copy bytes source_offset source target_offset target)
```

### empty

```clojure
(-> .Nat Binary)
```

A fresh/empty binary BLOB of the specified size\.

```clojure
(empty size)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Binary)
```

### index\_out\_of\_bounds

```clojure
(library/lux/control/exception.Exception [.Nat .Nat])
```

### monoid

```clojure
(library/lux/abstract/monoid.Monoid Binary)
```

### read/16\!

```clojure
(-> .Nat Binary (library/lux/control/try.Try .I64))
```

Read 2 bytes \(16 bits\) at the given index\.

```clojure
(read/16! index binary)
```

### read/32\!

```clojure
(-> .Nat Binary (library/lux/control/try.Try .I64))
```

Read 4 bytes \(32 bits\) at the given index\.

```clojure
(read/32! index binary)
```

### read/64\!

```clojure
(-> .Nat Binary (library/lux/control/try.Try .I64))
```

Read 8 bytes \(64 bits\) at the given index\.

```clojure
(read/64! index binary)
```

### read/8\!

```clojure
(-> .Nat Binary (library/lux/control/try.Try .I64))
```

Read 1 byte \(8 bits\) at the given index\.

```clojure
(read/8! index binary)
```

### size

```clojure
(-> Binary .Nat)
```

### slice

```clojure
(-> .Nat .Nat Binary (library/lux/control/try.Try Binary))
```

Yields a subset of the binary BLOB, so long as the specified range is valid\.

```clojure
(slice offset length binary)
```

### slice\_out\_of\_bounds

```clojure
(library/lux/control/exception.Exception [.Nat .Nat .Nat])
```

### write/16\!

```clojure
(-> .Nat (.I64 .Any) Binary (library/lux/control/try.Try Binary))
```

Write 2 bytes \(16 bits\) at the given index\.

```clojure
(write/16! index value binary)
```

### write/32\!

```clojure
(-> .Nat (.I64 .Any) Binary (library/lux/control/try.Try Binary))
```

Write 4 bytes \(32 bits\) at the given index\.

```clojure
(write/32! index value binary)
```

### write/64\!

```clojure
(-> .Nat (.I64 .Any) Binary (library/lux/control/try.Try Binary))
```

Write 8 bytes \(64 bits\) at the given index\.

```clojure
(write/64! index value binary)
```

### write/8\!

```clojure
(-> .Nat (.I64 .Any) Binary (library/lux/control/try.Try Binary))
```

Write 1 byte \(8 bits\) at the given index\.

```clojure
(write/8! index value binary)
```

## Missing documentation

1. `` cannot_copy_bytes ``

___

# library/lux/data/bit

## Definitions

### codec

```clojure
(library/lux/abstract/codec.Codec .Text .Bit)
```

### complement

```clojure
(All (_ _0)
  (-> (-> _0 .Bit) _0 .Bit))
```

Generates the complement of a predicate\.
That is a predicate that returns the oposite of the original predicate\.

### conjunction

```clojure
(library/lux/abstract/monoid.Monoid .Bit)
```

### disjunction

```clojure
(library/lux/abstract/monoid.Monoid .Bit)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Bit)
```

### hash

```clojure
(library/lux/abstract/hash.Hash .Bit)
```

### no

```clojure
.Bit
```

### off

```clojure
.Bit
```

### on

```clojure
.Bit
```

### yes

```clojure
.Bit
```

___

# library/lux/data/collection/array

## Definitions

### \(Array it\)

```clojure
... .Type
(Primitive "#Array" it)
```

Mutable arrays\.

### any?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (library/lux/abstract/predicate.Predicate (Array _0))))
```

### clone

```clojure
(All (_ _0)
  (-> (Array _0) (Array _0)))
```

Yields a shallow clone of the array\.

```clojure
(clone xs)
```

### contains?

```clojure
(All (_ _0)
  (-> .Nat (Array _0) .Bit))
```

```clojure
(contains? index array)
```

### copy\!

```clojure
(All (_ _0)
  (-> .Nat .Nat (Array _0) .Nat (Array _0) (Array _0)))
```

Writes the contents of one array into the other\.

```clojure
(copy! length src_start src_array dest_start dest_array)
```

### delete\!

```clojure
(All (_ _0)
  (-> .Nat (Array _0) (Array _0)))
```

Mutate the array by deleting the value at the specified index\.

```clojure
(delete! index array)
```

### empty

```clojure
(All (_ _0)
  (-> .Nat (Array _0)))
```

An empty array of the specified size\.

```clojure
(empty size)
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Array _0))))
```

### every?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (library/lux/abstract/predicate.Predicate (Array _0))))
```

### example

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (Array _0) (.Maybe _0)))
```

Yields the first item in the array that satisfies the predicate\.

```clojure
(example p xs)
```

### example\+

```clojure
(All (_ _0)
  (-> (-> .Nat _0 .Bit) (Array _0) (.Maybe [.Nat _0])))
```

Just like 'example', but with access to the index of each value\.

```clojure
(example+ p xs)
```

### filter\!

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (Array _0) (Array _0)))
```

Delete every item of the array that fails to satisfy the predicate\.

```clojure
(filter! p xs)
```

### functor

```clojure
(library/lux/abstract/functor.Functor Array)
```

### list

```clojure
(All (_ _0)
  (-> (.Maybe _0) (Array _0) (.List _0)))
```

Yields a list with every non\-empty item in the array\.
Can use the optional default value when encountering an empty cell in the array\.

```clojure
(list {.#None} array)

(list {.#Some default} array)
```

### mix

```clojure
(library/lux/abstract/mix.Mix Array)
```

### monoid

```clojure
(All (_ _0)
  (library/lux/abstract/monoid.Monoid (Array _0)))
```

### occupancy

```clojure
(All (_ _0)
  (-> (Array _0) .Nat))
```

Finds out how many cells in an array are occupied\.

```clojure
(occupancy array)
```

### of\_list

```clojure
(All (_ _0)
  (-> (.List _0) (Array _0)))
```

```clojure
(of_list xs)
```

### read\!

```clojure
(All (_ _0)
  (-> .Nat (Array _0) (.Maybe _0)))
```

```clojure
(read! index array)
```

### size

```clojure
(All (_ _0)
  (-> (Array _0) .Nat))
```

```clojure
(size array)
```

### type\_name

```clojure
.Text
```

### update\!

```clojure
(All (_ _0)
  (-> .Nat (-> _0 _0) (Array _0) (Array _0)))
```

Mutate the array by updating the value at the specified index\.

```clojure
(update! index transform array)
```

### upsert\!

```clojure
(All (_ _0)
  (-> .Nat _0 (-> _0 _0) (Array _0) (Array _0)))
```

Mutate the array by updating the value at the specified index\.
If there is no value, update and write the default value given\.

```clojure
(upsert! index default transform array)
```

### vacancy

```clojure
(All (_ _0)
  (-> (Array _0) .Nat))
```

Finds out how many cells in an array are vacant\.

```clojure
(vacancy array)
```

### write\!

```clojure
(All (_ _0)
  (-> .Nat _0 (Array _0) (Array _0)))
```

Mutate the array by writing a value to the specified index\.

```clojure
(write! index value array)
```

___

# library/lux/data/collection/bits

## Definitions

### Bits

```clojure
... .Type
(library/lux/data/collection/array.Array Chunk)
```

A bit\-map\.

### Chunk

```clojure
... .Type
(All (Chunk _0)
  (Primitive "#I64" _0))
```

### and

```clojure
(-> Bits Bits Bits)
```

### bit

```clojure
(-> .Nat Bits .Bit)
```

```clojure
(bit index bits)
```

### capacity

```clojure
(-> Bits .Nat)
```

```clojure
(capacity bits)
```

### chunk\_size

```clojure
.Nat
```

### empty

```clojure
Bits
```

### empty?

```clojure
(-> Bits .Bit)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Bits)
```

### flipped

```clojure
(-> .Nat Bits Bits)
```

### intersects?

```clojure
(-> Bits Bits .Bit)
```

```clojure
(intersects? reference sample)
```

### not

```clojure
(-> Bits Bits)
```

```clojure
(not input)
```

### one

```clojure
(-> .Nat Bits Bits)
```

### or

```clojure
(-> Bits Bits Bits)
```

### size

```clojure
(-> Bits .Nat)
```

Measures the size of a bit\-map by counting all the 1s in the bit\-map\.

```clojure
(size bits)
```

### xor

```clojure
(-> Bits Bits Bits)
```

### zero

```clojure
(-> .Nat Bits Bits)
```

___

# library/lux/data/collection/dictionary

## Definitions

### \(Dictionary key value\)

```clojure
... .Type
(Record
 [#hash (library/lux/abstract/hash.Hash key)
  #root (Node key value)])
```

A dictionary implemented as a Hash\-Array Mapped Trie \(HAMT\)\.

### empty

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/hash.Hash _0) (Dictionary _0 _1)))
```

An empty dictionary\.

```clojure
(empty key_hash)
```

### empty?

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) .Bit))
```

### entries

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (.List [_0 _1])))
```

### equivalence

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/equivalence.Equivalence _1) (library/lux/abstract/equivalence.Equivalence (Dictionary _0 _1))))
```

### functor

```clojure
(All (_ _0)
  (library/lux/abstract/functor.Functor (Dictionary _0)))
```

### has

```clojure
(All (_ _0 _1)
  (-> _0 _1 (Dictionary _0 _1) (Dictionary _0 _1)))
```

```clojure
(has key val dict)
```

### has'

```clojure
(All (_ _0 _1)
  (-> _0 _1 (Dictionary _0 _1) (library/lux/control/try.Try (Dictionary _0 _1))))
```

Only puts the KV\-pair if the key is not already present\.

```clojure
(has' key val dict)
```

### key?

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) _0 .Bit))
```

```clojure
(key? dict key)
```

### key\_already\_exists

```clojure
(library/lux/control/exception.Exception .Any)
```

### key\_hash

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (library/lux/abstract/hash.Hash _0)))
```

### keys

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (.List _0)))
```

### lacks

```clojure
(All (_ _0 _1)
  (-> _0 (Dictionary _0 _1) (Dictionary _0 _1)))
```

```clojure
(lacks key dict)
```

### merged

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (Dictionary _0 _1) (Dictionary _0 _1)))
```

Merges 2 dictionaries\.
If any collisions with keys occur, the values of dict2 will overwrite those of dict1\.

```clojure
(merged dict2 dict1)
```

### merged\_with

```clojure
(All (_ _0 _1)
  (-> (-> _1 _1 _1) (Dictionary _0 _1) (Dictionary _0 _1) (Dictionary _0 _1)))
```

Merges 2 dictionaries\.
If any collisions with keys occur, a new value will be computed by applying 'f' to the values of dict2 and dict1\.

```clojure
(merged_with f dict2 dict1)
```

### of\_list

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/hash.Hash _0) (.List [_0 _1]) (Dictionary _0 _1)))
```

```clojure
(of_list key_hash kvs)
```

### re\_bound

```clojure
(All (_ _0 _1)
  (-> _0 _0 (Dictionary _0 _1) (Dictionary _0 _1)))
```

If there is a value under 'from\_key', remove 'from\_key' and store the value under 'to\_key'\.

```clojure
(re_bound from_key to_key dict)
```

### revised

```clojure
(All (_ _0 _1)
  (-> _0 (-> _1 _1) (Dictionary _0 _1) (Dictionary _0 _1)))
```

Transforms the value located at key \(if available\), using the given function\.

```clojure
(revised key f dict)
```

### revised'

```clojure
(All (_ _0 _1)
  (-> _0 _1 (-> _1 _1) (Dictionary _0 _1) (Dictionary _0 _1)))
```

Updates the value at the key; if it exists\.
Otherwise, puts a value by applying the function to a default\.

```clojure
(revised' key default f dict)
```

### size

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) .Nat))
```

### sub

```clojure
(All (_ _0 _1)
  (-> (.List _0) (Dictionary _0 _1) (Dictionary _0 _1)))
```

A sub\-dictionary, with only the specified keys\.

```clojure
(sub keys dict)
```

### value

```clojure
(All (_ _0 _1)
  (-> _0 (Dictionary _0 _1) (.Maybe _1)))
```

```clojure
(value key dict)
```

### values

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (.List _1)))
```

___

# library/lux/data/collection/dictionary/ordered

## Definitions

### \(Dictionary key value\)

```clojure
... .Type
(Record
 [#&order (library/lux/abstract/order.Order key)
  #root (.Maybe (Node key value))])
```

A dictionary data\-structure with ordered entries\.

### empty

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/order.Order _0) (Dictionary _0 _1)))
```

An empty dictionary, employing the given order\.

```clojure
(empty order)
```

### empty?

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) .Bit))
```

### entries

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (.List [_0 _1])))
```

### equivalence

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/equivalence.Equivalence _1) (library/lux/abstract/equivalence.Equivalence (Dictionary _0 _1))))
```

### has

```clojure
(All (_ _0 _1)
  (-> _0 _1 (Dictionary _0 _1) (Dictionary _0 _1)))
```

```clojure
(has key value dict)
```

### key?

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) _0 .Bit))
```

```clojure
(key? dict key)
```

### keys

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (.List _0)))
```

### lacks

```clojure
(All (_ _0 _1)
  (-> _0 (Dictionary _0 _1) (Dictionary _0 _1)))
```

```clojure
(lacks key dict)
```

### max

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (.Maybe _1)))
```

Yields value under the maximum key\.

### min

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (.Maybe _1)))
```

Yields value under the minimum key\.

### of\_list

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/order.Order _0) (.List [_0 _1]) (Dictionary _0 _1)))
```

```clojure
(of_list order list)
```

### revised

```clojure
(All (_ _0 _1)
  (-> _0 (-> _1 _1) (Dictionary _0 _1) (Dictionary _0 _1)))
```

```clojure
(revised key transform dict)
```

### size

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) .Nat))
```

```clojure
(size dict)
```

### value

```clojure
(All (_ _0 _1)
  (-> _0 (Dictionary _0 _1) (.Maybe _1)))
```

```clojure
(value key dict)
```

### values

```clojure
(All (_ _0 _1)
  (-> (Dictionary _0 _1) (.List _1)))
```

___

# library/lux/data/collection/dictionary/plist

## Definitions

### \(PList it\)

```clojure
... .Type
(.List [.Text it])
```

A property list\.
It's a simple dictionary\-like structure with Text keys\.

### contains?

```clojure
(All (_ _0)
  (-> .Text (PList _0) .Bit))
```

```clojure
(contains? key properties)
```

### empty

```clojure
PList
```

### empty?

```clojure
(All (_ _0)
  (-> (PList _0) .Bit))
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (PList _0))))
```

### has

```clojure
(All (_ _0)
  (-> .Text _0 (PList _0) (PList _0)))
```

```clojure
(has key val properties)
```

### keys

```clojure
(All (_ _0)
  (-> (PList _0) (.List .Text)))
```

### lacks

```clojure
(All (_ _0)
  (-> .Text (PList _0) (PList _0)))
```

```clojure
(lacks key properties)
```

### revised

```clojure
(All (_ _0)
  (-> .Text (-> _0 _0) (PList _0) (PList _0)))
```

```clojure
(revised key f properties)
```

### size

```clojure
(All (_ _0)
  (-> (PList _0) .Nat))
```

### value

```clojure
(All (_ _0)
  (-> .Text (PList _0) (.Maybe _0)))
```

```clojure
(value key properties)
```

### values

```clojure
(All (_ _0)
  (-> (PList _0) (.List _0)))
```

___

# library/lux/data/collection/list

## Definitions

### after

```clojure
(All (_ _0)
  (-> .Nat (.List _0) (.List _0)))
```

### all

```clojure
(All (_ _0 _1)
  (-> (-> _0 (.Maybe _1)) (.List _0) (.List _1)))
```

```clojure
(all check xs)
```

### any?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (.List _0) .Bit))
```

### apply

```clojure
(library/lux/abstract/apply.Apply .List)
```

### empty?

```clojure
(All (_ _0)
  (library/lux/abstract/predicate.Predicate (.List _0)))
```

```clojure
(empty? xs)
```

### enumeration

```clojure
(All (_ _0)
  (-> (.List _0) (.List [.Nat _0])))
```

Pairs every element in the list with its index, starting at 0\.

```clojure
(enumeration xs)
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (.List _0))))
```

### every?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (.List _0) .Bit))
```

### example

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (.List _0) (.Maybe _0)))
```

Yields the first value in the list that satisfies the predicate\.

```clojure
(example predicate xs)
```

### first

```clojure
(All (_ _0)
  (-> .Nat (.List _0) (.List _0)))
```

### functor

```clojure
(library/lux/abstract/functor.Functor .List)
```

### hash

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) (library/lux/abstract/hash.Hash (.List _0))))
```

### head

```clojure
(All (_ _0)
  (-> (.List _0) (.Maybe _0)))
```

Yields the first element of a list\.

### indices

```clojure
(All (_ _0)
  (-> .Nat (.List .Nat)))
```

Produces all the valid indices for a given size\.

```clojure
(indices size)
```

### inits

```clojure
(All (_ _0)
  (-> (.List _0) (.Maybe (.List _0))))
```

For a list of size N, yields the first N\-1 elements\.
Will yield a \.\#None for empty lists\.

```clojure
(inits xs)
```

### interposed

```clojure
(All (_ _0)
  (-> _0 (.List _0) (.List _0)))
```

Puts a value between every two elements in the list\.

```clojure
(interposed sep xs)
```

### item

```clojure
(All (_ _0)
  (-> .Nat (.List _0) (.Maybe _0)))
```

Fetches the element at the specified index\.

```clojure
(item i xs)
```

### iterations

```clojure
(All (_ _0)
  (-> (-> _0 (.Maybe _0)) _0 (.List _0)))
```

Generates a list element by element until the function returns \.\#None\.

```clojure
(iterations f x)
```

### last

```clojure
(All (_ _0)
  (-> (.List _0) (.Maybe _0)))
```

```clojure
(last xs)
```

### lifted

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (_0 _1) (_0 (.List _1))))
```

Wraps a monadic value with List machinery\.

```clojure
(lifted monad)
```

### member?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (.List _0) _0 .Bit))
```

```clojure
(member? eq xs x)
```

### mix

```clojure
(library/lux/abstract/mix.Mix .List)
```

### mixes

```clojure
(All (_ _0 _1)
  (-> (-> _0 _1 _1) _1 (.List _0) (.List _1)))
```

```clojure
(mixes f init inputs)
```

### monad

```clojure
(library/lux/abstract/monad.Monad .List)
```

### monoid

```clojure
(All (_ _0)
  (library/lux/abstract/monoid.Monoid (.List _0)))
```

### one

```clojure
(All (_ _0 _1)
  (-> (-> _0 (.Maybe _1)) (.List _0) (.Maybe _1)))
```

```clojure
(one check xs)
```

### only

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (.List _0) (.List _0)))
```

A list with only values that satisfy the predicate\.

```clojure
(only keep? xs)
```

### pairs

```clojure
(All (_ _0)
  (-> (.List _0) (.Maybe (.List [_0 _0]))))
```

Cut the list into pairs of 2\.

```clojure
(pairs list)
```

### partition

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (.List _0) [(.List _0) (.List _0)]))
```

Divide the list into all elements that satisfy a predicate, and all elements that do not\.

```clojure
(partition satisfies? list)
```

### repeated

```clojure
(All (_ _0)
  (-> .Nat _0 (.List _0)))
```

A list of the value x, repeated n times\.

```clojure
(repeated n x)
```

### reversed

```clojure
(All (_ _0)
  (-> (.List _0) (.List _0)))
```

```clojure
(reversed xs)
```

### size

```clojure
(All (_ _0)
  (-> (.List _0) .Nat))
```

```clojure
(size list)
```

### sorted

```clojure
(All (_ _0)
  (-> (-> _0 _0 .Bit) (.List _0) (.List _0)))
```

A list ordered by a comparison function\.

```clojure
(sorted < xs)
```

### split\_at

```clojure
(All (_ _0)
  (-> .Nat (.List _0) [(.List _0) (.List _0)]))
```

```clojure
(split_at n xs)
```

### split\_when

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (.List _0) [(.List _0) (.List _0)]))
```

Segment the list by using a predicate to tell when to cut\.

```clojure
(split_when predicate xs)
```

### sub

```clojure
(All (_ _0)
  (-> .Nat (.List _0) (.List (.List _0))))
```

Segment the list into sub\-lists of \(at most\) the given size\.

```clojure
(sub size list)
```

### tail

```clojure
(All (_ _0)
  (-> (.List _0) (.Maybe (.List _0))))
```

For a list of size N, yields the N\-1 elements after the first one\.

### together

```clojure
(All (_ _0)
  (-> (.List (.List _0)) (.List _0)))
```

The sequential combination of all the lists\.

### until

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (.List _0) (.List _0)))
```

### when

```clojure
.Macro
```

Can be used as a guard in \(co\)monadic be/do expressions\.

```clojure
(do monad
  [value (do_something 1 2 3)
   .when (passes_test? value)]
  (do_something_else 4 5 6))
```

### while

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (.List _0) (.List _0)))
```

### with

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (library/lux/abstract/monad.Monad (All (_ _1) (_0 (.List _1))))))
```

Enhances a monad with List functionality\.

```clojure
(with monad)
```

### zipped

```clojure
.Macro
```

Create list zippers with the specified number of input lists\.

```clojure
(def: zipped/2 (zipped 2))

(def: zipped/3 (zipped 3))

(zipped/3 xs ys zs)

((zipped 3) xs ys zs)
```

### zipped/2

```clojure
(All (_ _0 _1)
  (-> (.List _0) (.List _1) (.List [_0 _1])))
```

### zipped/3

```clojure
(All (_ _0 _1 _2)
  (-> (.List _0) (.List _1) (.List _2) (.List [_0 _1 _2])))
```

### zipped\_with

```clojure
.Macro
```

Create list zippers with the specified number of input lists\.

```clojure
(def: zipped_with/2 (zipped_with 2))

(def: zipped_with/3 (zipped_with 3))

(zipped_with/2 + xs ys)

((zipped_with 2) + xs ys)
```

### zipped\_with/2

```clojure
(All (_ _0 _1 _2)
  (-> (-> _0 _1 _2) (.List _0) (.List _1) (.List _2)))
```

### zipped\_with/3

```clojure
(All (_ _0 _1 _2 _3)
  (-> (-> _0 _1 _2 _3) (.List _0) (.List _1) (.List _2) (.List _3)))
```

___

# library/lux/data/collection/queue

## Definitions

### \(Queue it\)

```clojure
... .Type
(Record
 [#front (.List it)
  #rear (.List it)])
```

A first\-in, first\-out sequential data\-structure\.

### empty

```clojure
Queue
```

### empty?

```clojure
(All (_ _0)
  (-> (Queue _0) .Bit))
```

### end

```clojure
(All (_ _0)
  (-> _0 (Queue _0) (Queue _0)))
```

```clojure
(end val queue)
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Queue _0))))
```

### front

```clojure
(All (_ _0)
  (-> (Queue _0) (.Maybe _0)))
```

Yields the first value in the queue, if any\.

### functor

```clojure
(library/lux/abstract/functor.Functor Queue)
```

### list

```clojure
(All (_ _0)
  (-> (Queue _0) (.List _0)))
```

```clojure
(list queue)
```

### member?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (Queue _0) _0 .Bit))
```

```clojure
(member? equivalence queue member)
```

### next

```clojure
(All (_ _0)
  (-> (Queue _0) (Queue _0)))
```

```clojure
(next queue)
```

### of\_list

```clojure
(All (_ _0)
  (-> (.List _0) (Queue _0)))
```

```clojure
(of_list entries)
```

### size

```clojure
(All (_ _0)
  (-> (Queue _0) .Nat))
```

___

# library/lux/data/collection/queue/priority

## Definitions

### Priority

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

### \(Queue it\)

```clojure
... .Type
(Primitive "library/lux/data/collection/queue/priority.Queue" it)
```

### empty

```clojure
Queue
```

### empty?

```clojure
(All (_ _0)
  (-> (Queue _0) .Bit))
```

### end

```clojure
(All (_ _0)
  (-> Priority _0 (Queue _0) (Queue _0)))
```

```clojure
(end priority value queue)
```

### front

```clojure
(All (_ _0)
  (-> (Queue _0) (.Maybe _0)))
```

### max

```clojure
Priority
```

### member?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (Queue _0) _0 .Bit))
```

```clojure
(member? equivalence queue member)
```

### min

```clojure
Priority
```

### next

```clojure
(All (_ _0)
  (-> (Queue _0) (Queue _0)))
```

### size

```clojure
(All (_ _0)
  (-> (Queue _0) .Nat))
```

___

# library/lux/data/collection/sequence

## Definitions

### \(Sequence it\)

```clojure
... .Type
(Record
 [#level Level
  #size .Nat
  #root (Hierarchy it)
  #tail (Base it)])
```

A sequential data\-structure with fast random access\.

### any?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (Sequence _0) .Bit))
```

### apply

```clojure
(library/lux/abstract/apply.Apply Sequence)
```

### empty

```clojure
Sequence
```

### empty?

```clojure
(All (_ _0)
  (-> (Sequence _0) .Bit))
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Sequence _0))))
```

### every?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (Sequence _0) .Bit))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Sequence)
```

### has

```clojure
(All (_ _0)
  (-> .Nat _0 (Sequence _0) (library/lux/control/try.Try (Sequence _0))))
```

```clojure
(has idx val sequence)
```

### index\_out\_of\_bounds

```clojure
(All (_ _0)
  (library/lux/control/exception.Exception [(Sequence _0) .Nat]))
```

### item

```clojure
(All (_ _0)
  (-> .Nat (Sequence _0) (library/lux/control/try.Try _0)))
```

```clojure
(item idx sequence)
```

### list

```clojure
(All (_ _0)
  (-> (Sequence _0) (.List _0)))
```

```clojure
(list sequence)
```

### member?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (Sequence _0) _0 .Bit))
```

```clojure
(member? equivalence sequence val)
```

### mix

```clojure
(library/lux/abstract/mix.Mix Sequence)
```

### monad

```clojure
(library/lux/abstract/monad.Monad Sequence)
```

### monoid

```clojure
(All (_ _0)
  (library/lux/abstract/monoid.Monoid (Sequence _0)))
```

### of\_list

```clojure
(All (_ _0)
  (-> (.List _0) (Sequence _0)))
```

### prefix

```clojure
(All (_ _0)
  (-> (Sequence _0) (Sequence _0)))
```

```clojure
(prefix sequence)
```

### reversed

```clojure
(All (_ _0)
  (-> (Sequence _0) (Sequence _0)))
```

### revised

```clojure
(All (_ _0)
  (-> .Nat (-> _0 _0) (Sequence _0) (library/lux/control/try.Try (Sequence _0))))
```

```clojure
(revised idx f sequence)
```

### sequence

```clojure
.Macro
```

Sequence literals\.

```clojure
(: (Sequence Nat)
   (sequence 12 34 56 78 90))
```

### size

```clojure
(All (_ _0)
  (-> (Sequence _0) .Nat))
```

### suffix

```clojure
(All (_ _0)
  (-> _0 (Sequence _0) (Sequence _0)))
```

```clojure
(suffix val sequence)
```

### within\_bounds?

```clojure
(All (_ _0)
  (-> (Sequence _0) .Nat .Bit))
```

Determines whether the index is within the bounds of the sequence\.

```clojure
(within_bounds? sequence idx)
```

___

# library/lux/data/collection/set

## Definitions

### \(Set it\)

```clojure
... .Type
(library/lux/data/collection/dictionary.Dictionary it .Any)
```

### difference

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

```clojure
(difference sub base)
```

### empty

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) (Set _0)))
```

### empty?

```clojure
(All (_ _0)
  (-> (Set _0) .Bit))
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Set _0)))
```

### has

```clojure
(All (_ _0)
  (-> _0 (Set _0) (Set _0)))
```

```clojure
(has elem set)
```

### hash

```clojure
(All (_ _0)
  (library/lux/abstract/hash.Hash (Set _0)))
```

### intersection

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

```clojure
(intersection filter base)
```

### lacks

```clojure
(All (_ _0)
  (-> _0 (Set _0) (Set _0)))
```

### list

```clojure
(All (_ _0)
  (-> (Set _0) (.List _0)))
```

### member?

```clojure
(All (_ _0)
  (-> (Set _0) _0 .Bit))
```

### member\_hash

```clojure
(All (_ _0)
  (-> (Set _0) (library/lux/abstract/hash.Hash _0)))
```

### monoid

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) (library/lux/abstract/monoid.Monoid (Set _0))))
```

### of\_list

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) (.List _0) (Set _0)))
```

### predicate

```clojure
(All (_ _0)
  (-> (Set _0) (library/lux/abstract/predicate.Predicate _0)))
```

### size

```clojure
(All (_ _0)
  (-> (Set _0) .Nat))
```

### sub?

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) .Bit))
```

```clojure
(sub? super sub)
```

### super?

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) .Bit))
```

```clojure
(super? sub super)
```

### union

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

___

# library/lux/data/collection/set/multi

## Definitions

### \(Set it\)

```clojure
... .Type
(Primitive "library/lux/data/collection/set/multi.Set" it)
```

A set that keeps track of repetition in its entries\.

### difference

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

### empty

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) (Set _0)))
```

### empty?

```clojure
(All (_ _0)
  (-> (Set _0) .Bit))
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Set _0)))
```

### has

```clojure
(All (_ _0)
  (-> .Nat _0 (Set _0) (Set _0)))
```

```clojure
(has multiplicity elem set)
```

### hash

```clojure
(All (_ _0)
  (library/lux/abstract/hash.Hash (Set _0)))
```

### intersection

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

### lacks

```clojure
(All (_ _0)
  (-> .Nat _0 (Set _0) (Set _0)))
```

```clojure
(lacks multiplicity elem set)
```

### list

```clojure
(All (_ _0)
  (-> (Set _0) (.List _0)))
```

### member?

```clojure
(All (_ _0)
  (-> (Set _0) _0 .Bit))
```

```clojure
(member? set elem)
```

### multiplicity

```clojure
(All (_ _0)
  (-> (Set _0) _0 .Nat))
```

```clojure
(multiplicity set elem)
```

### of\_list

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) (.List _0) (Set _0)))
```

### of\_set

```clojure
(All (_ _0)
  (-> (library/lux/data/collection/set.Set _0) (Set _0)))
```

### size

```clojure
(All (_ _0)
  (-> (Set _0) .Nat))
```

### sub?

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) .Bit))
```

Is 'subject' a sub\-set of 'reference'?

```clojure
(sub? reference subject)
```

### sum

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

### super?

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) .Bit))
```

Is 'subject' a super\-set of 'reference'?

### support

```clojure
(All (_ _0)
  (-> (Set _0) (library/lux/data/collection/set.Set _0)))
```

A set of the unique \(non repeated\) members\.

```clojure
(support set)
```

### union

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

___

# library/lux/data/collection/set/ordered

## Definitions

### \(Set it\)

```clojure
... .Type
(Primitive "library/lux/data/collection/set/ordered.Set" it)
```

A set with ordered entries\.

### difference

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

```clojure
(difference param subject)
```

### empty

```clojure
(All (_ _0)
  (-> (library/lux/abstract/order.Order _0) (Set _0)))
```

### empty?

```clojure
(All (_ _0)
  (-> (Set _0) .Bit))
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Set _0)))
```

### has

```clojure
(All (_ _0)
  (-> _0 (Set _0) (Set _0)))
```

```clojure
(has elem set)
```

### intersection

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

### lacks

```clojure
(All (_ _0)
  (-> _0 (Set _0) (Set _0)))
```

```clojure
(lacks elem set)
```

### list

```clojure
(All (_ _0)
  (-> (Set _0) (.List _0)))
```

### max

```clojure
(All (_ _0)
  (-> (Set _0) (.Maybe _0)))
```

### member?

```clojure
(All (_ _0)
  (-> (Set _0) _0 .Bit))
```

```clojure
(member? set elem)
```

### min

```clojure
(All (_ _0)
  (-> (Set _0) (.Maybe _0)))
```

### of\_list

```clojure
(All (_ _0)
  (-> (library/lux/abstract/order.Order _0) (.List _0) (Set _0)))
```

### size

```clojure
(All (_ _0)
  (-> (Set _0) .Nat))
```

### sub?

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) .Bit))
```

Is 'sub' a sub\-set of 'super'?

```clojure
(sub? super sub)
```

### super?

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) .Bit))
```

Is 'super' a super\-set of 'sub'?

```clojure
(super? sub super)
```

### union

```clojure
(All (_ _0)
  (-> (Set _0) (Set _0) (Set _0)))
```

___

# library/lux/data/collection/stack

## Definitions

### \(Stack it\)

```clojure
... .Type
(Primitive "library/lux/data/collection/stack.Stack" it)
```

A first\-in, last\-out sequential data\-structure\.

### empty

```clojure
Stack
```

### empty?

```clojure
(All (_ _0)
  (-> (Stack _0) .Bit))
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Stack _0))))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Stack)
```

### next

```clojure
(All (_ _0)
  (-> (Stack _0) (.Maybe [_0 (Stack _0)])))
```

```clojure
(next stack)
```

### size

```clojure
(All (_ _0)
  (-> (Stack _0) .Nat))
```

### top

```clojure
(All (_ _0)
  (-> _0 (Stack _0) (Stack _0)))
```

```clojure
(top value stack)
```

### value

```clojure
(All (_ _0)
  (-> (Stack _0) (.Maybe _0)))
```

Yields the top value in the stack, if any\.

```clojure
(value stack)
```

___

# library/lux/data/collection/stream

## Definitions

### \(Stream it\)

```clojure
... .Type
(library/lux/control/continuation.Cont [it (Stream it)])
```

An infinite sequence of values\.

### ^stream&

```clojure
.Macro
```

Allows destructuring of streams in pattern\-matching expressions\.
Caveat emptor: Only use it for destructuring, and not for testing values within the streams\.

```clojure
(let [(^stream& x y z _tail) (some_stream_func +1 +2 +3)]
  (func x y z))
```

### after

```clojure
(All (_ _0)
  (-> .Nat (Stream _0) (Stream _0)))
```

### comonad

```clojure
(library/lux/abstract/comonad.CoMonad Stream)
```

### cycle

```clojure
(All (_ _0)
  (-> [_0 (.List _0)] (Stream _0)))
```

Go over the elements of a list forever\.

```clojure
(cycle [start next])
```

### first

```clojure
(All (_ _0)
  (-> .Nat (Stream _0) (.List _0)))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Stream)
```

### head

```clojure
(All (_ _0)
  (-> (Stream _0) _0))
```

### item

```clojure
(All (_ _0)
  (-> .Nat (Stream _0) _0))
```

```clojure
(item idx stream)
```

### iterations

```clojure
(All (_ _0 _1)
  (-> (-> _0 [_0 _1]) _0 (Stream _1)))
```

A stateful way of infinitely calculating the values of a stream\.

```clojure
(iterations step init)
```

### only

```clojure
(All (_ _0)
  (-> (-> _0 .Bit) (Stream _0) (Stream _0)))
```

A new stream only with items that satisfy the predicate\.

```clojure
(only predicate stream)
```

### partition

```clojure
(All (_ _0)
  (-> (-> _0 .Bit) (Stream _0) [(Stream _0) (Stream _0)]))
```

Split a stream in two based on a predicate\.
The left side contains all entries for which the predicate is \#1\.
The right side contains all entries for which the predicate is \#0\.

```clojure
(partition left? xs)
```

### repeated

```clojure
(All (_ _0)
  (-> _0 (Stream _0)))
```

Repeat a value forever\.

```clojure
(repeated x)
```

### split\_at

```clojure
(All (_ _0)
  (-> .Nat (Stream _0) [(.List _0) (Stream _0)]))
```

### split\_when

```clojure
(All (_ _0)
  (-> (-> _0 .Bit) (Stream _0) [(.List _0) (Stream _0)]))
```

### tail

```clojure
(All (_ _0)
  (-> (Stream _0) (Stream _0)))
```

### until

```clojure
(All (_ _0)
  (-> (-> _0 .Bit) (Stream _0) (Stream _0)))
```

### while

```clojure
(All (_ _0)
  (-> (-> _0 .Bit) (Stream _0) (.List _0)))
```

___

# library/lux/data/collection/tree

## Definitions

### \(Tree it\)

```clojure
... .Type
(Record
 [#value it
  #children (.List (Tree it))])
```

A generic tree data\-structure\.

### branch

```clojure
(All (_ _0)
  (-> _0 (.List (Tree _0)) (Tree _0)))
```

```clojure
(branch value children)
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Tree _0))))
```

### flat

```clojure
(All (_ _0)
  (-> (Tree _0) (.List _0)))
```

All the leaf values of the tree, in order\.

```clojure
(flat tree)
```

### functor

```clojure
(library/lux/abstract/functor.Functor Tree)
```

### leaf

```clojure
(All (_ _0)
  (-> _0 (Tree _0)))
```

```clojure
(leaf value)
```

### mix

```clojure
(library/lux/abstract/mix.Mix Tree)
```

### tree

```clojure
.Macro
```

Tree literals\.

```clojure
(: (Tree Nat)
   (tree 12
         {34 {}
             56 {}
             78 {90 {}}}))
```

___

# library/lux/data/collection/tree/finger

## Definitions

### \(Builder @ tag\)

```clojure
... .Type
(Record
 [leaf (All (_ _0) (-> tag _0 (Tree @ tag _0)))
  branch (All (_ _0) (-> (Tree @ tag _0) (Tree @ tag _0) (Tree @ tag _0)))])
```

A builder for finter tree structures\.

### \(Tree @ tag value\)

```clojure
... .Type
(Primitive "library/lux/data/collection/tree/finger.Tree" @ tag value)
```

A finger tree\.

### builder

```clojure
(All (_ _0)
  (Ex (_ _1)
    (-> (library/lux/abstract/monoid.Monoid _0) (Builder _1 _0))))
```

A new builder using the given monoid\.

```clojure
(builder monoid)
```

### exists?

```clojure
(All (_ _0 _1 _2)
  (-> (library/lux/abstract/predicate.Predicate _1) (Tree _0 _1 _2) .Bit))
```

Verifies that a value exists which meets the predicate\.

```clojure
(exists? predicate tree)
```

### one

```clojure
(All (_ _0 _1 _2)
  (-> (library/lux/abstract/predicate.Predicate _1) (Tree _0 _1 _2) (.Maybe _2)))
```

Finds one value that meets the predicate\.

```clojure
(one predicate tree)
```

### root

```clojure
(All (_ _0 _1 _2)
  (-> (Tree _0 _1 _2) (.Either _2 [(Tree _0 _1 _2) (Tree _0 _1 _2)])))
```

### tag

```clojure
(All (_ _0 _1 _2)
  (-> (Tree _0 _1 _2) _1))
```

### tags

```clojure
(All (_ _0 _1 _2)
  (-> (Tree _0 _1 _2) (.List _1)))
```

```clojure
(tags tree)
```

### value

```clojure
(All (_ _0 _1 _2)
  (-> (Tree _0 _1 _2) _2))
```

```clojure
(value tree)
```

### values

```clojure
(All (_ _0 _1 _2)
  (-> (Tree _0 _1 _2) (.List _2)))
```

```clojure
(values tree)
```

___

# library/lux/data/collection/tree/zipper

## Definitions

### \(Zipper it\)

```clojure
... .Type
(Record
 [#family (.Maybe (Family Zipper it))
  #node (library/lux/data/collection/tree.Tree it)])
```

Tree zippers, for easy navigation and editing of trees\.

### adopt

```clojure
(All (_ _0)
  (-> _0 (Zipper _0) (Zipper _0)))
```

```clojure
(adopt value zipper)
```

### branch?

```clojure
(All (_ _0)
  (-> (Zipper _0) .Bit))
```

### comonad

```clojure
(library/lux/abstract/comonad.CoMonad Zipper)
```

### down

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### end

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### end?

```clojure
(All (_ _0)
  (-> (Zipper _0) .Bit))
```

### equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Zipper _0))))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Zipper)
```

### insert\_left

```clojure
(All (_ _0)
  (-> _0 (Zipper _0) (.Maybe (Zipper _0))))
```

### insert\_right

```clojure
(All (_ _0)
  (-> _0 (Zipper _0) (.Maybe (Zipper _0))))
```

### interpose

```clojure
(All (_ _0)
  (-> _0 (Zipper _0) (Zipper _0)))
```

```clojure
(interpose value zipper)
```

### leaf?

```clojure
(All (_ _0)
  (-> (Zipper _0) .Bit))
```

### left

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### leftmost

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### next

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### previous

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### remove

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### right

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### rightmost

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### set

```clojure
(All (_ _0)
  (-> _0 (Zipper _0) (Zipper _0)))
```

```clojure
(set value zipper)
```

### start

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### start?

```clojure
(All (_ _0)
  (-> (Zipper _0) .Bit))
```

### tree

```clojure
(All (_ _0)
  (-> (Zipper _0) (library/lux/data/collection/tree.Tree _0)))
```

### up

```clojure
(All (_ _0)
  (-> (Zipper _0) (.Maybe (Zipper _0))))
```

### update

```clojure
(All (_ _0)
  (-> (-> _0 _0) (Zipper _0) (Zipper _0)))
```

```clojure
(update transform zipper)
```

### value

```clojure
(All (_ _0)
  (-> (Zipper _0) _0))
```

### zipper

```clojure
(All (_ _0)
  (-> (library/lux/data/collection/tree.Tree _0) (Zipper _0)))
```

___

# library/lux/data/color

## Definitions

### Alpha

```clojure
... .Type
(Primitive "#I64" (Primitive "#Rev"))
```

The degree of transparency of a pigment\.

### CMYK

```clojure
... .Type
(Record
 [#cyan .Frac
  #magenta .Frac
  #yellow .Frac
  #key .Frac])
```

Cyan\-Magenta\-Yellow\-Key color format\.

### Color

```clojure
... .Type
(Primitive "library/lux/data/color.Color")
```

A color value, independent of color format\.

### HSB

```clojure
... .Type
[.Frac .Frac .Frac]
```

Hue\-Saturation\-Brightness color format\.

### HSL

```clojure
... .Type
[.Frac .Frac .Frac]
```

Hue\-Saturation\-Lightness color format\.

### Palette

```clojure
... .Type
(-> Spread .Nat Color (.List Color))
```

### Pigment

```clojure
... .Type
(Record
 [#color Color
  #alpha Alpha])
```

A color with some degree of transparency\.

### RGB

```clojure
... .Type
(Record
 [#red .Nat
  #green .Nat
  #blue .Nat])
```

Red\-Green\-Blue color format\.

### Spread

```clojure
... .Type
(Primitive "#Frac")
```

### addition

```clojure
(library/lux/abstract/monoid.Monoid Color)
```

### analogous

```clojure
Palette
```

A analogous palette\.

```clojure
(analogous spread variations color)
```

### black

```clojure
Color
```

### brighter

```clojure
(-> .Frac Color Color)
```

### clash

```clojure
(-> Color [Color Color Color])
```

A clash color scheme\.

### cmyk

```clojure
(-> Color CMYK)
```

### complement

```clojure
(-> Color Color)
```

The opposite color\.

```clojure
(complement color)
```

### darker

```clojure
(-> .Frac Color Color)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Color)
```

### gray\_scale

```clojure
(-> Color Color)
```

### hash

```clojure
(library/lux/abstract/hash.Hash Color)
```

### hsb

```clojure
(-> Color HSB)
```

### hsl

```clojure
(-> Color HSL)
```

### interpolated

```clojure
(-> .Frac Color Color Color)
```

```clojure
(interpolated ratio end start)
```

### monochromatic

```clojure
Palette
```

A monochromatic palette\.

```clojure
(monochromatic spread variations color)
```

### of\_cmyk

```clojure
(-> CMYK Color)
```

### of\_hsb

```clojure
(-> HSB Color)
```

### of\_hsl

```clojure
(-> HSL Color)
```

### of\_rgb

```clojure
(-> RGB Color)
```

### opaque

```clojure
Alpha
```

The minimum degree of transparency\.

### rgb

```clojure
(-> Color RGB)
```

### saturated

```clojure
(-> .Frac Color Color)
```

### split\_complement

```clojure
(-> Color [Color Color Color])
```

A split\-complement color scheme\.

### square

```clojure
(-> Color [Color Color Color Color])
```

A square color scheme\.

### subtraction

```clojure
(library/lux/abstract/monoid.Monoid Color)
```

### tetradic

```clojure
(-> Color [Color Color Color Color])
```

A tetradic color scheme\.

### translucent

```clojure
Alpha
```

The average degree of transparency\.

### transparent

```clojure
Alpha
```

The maximum degree of transparency\.

### triad

```clojure
(-> Color [Color Color Color])
```

A triad color scheme\.

### un\_saturated

```clojure
(-> .Frac Color Color)
```

### white

```clojure
Color
```

___

# library/lux/data/color/named

## Definitions

### alice\_blue

```clojure
library/lux/data/color.Color
```

R:F0 G:F8 B:FF | alice blue

### antique\_white

```clojure
library/lux/data/color.Color
```

R:FA G:EB B:D7 | antique white

### aqua

```clojure
library/lux/data/color.Color
```

R:0 G:FF B:FF | aqua

### aquamarine

```clojure
library/lux/data/color.Color
```

R:7F G:FF B:D4 | aquamarine

### azure

```clojure
library/lux/data/color.Color
```

R:F0 G:FF B:FF | azure

### beige

```clojure
library/lux/data/color.Color
```

R:F5 G:F5 B:DC | beige

### bisque

```clojure
library/lux/data/color.Color
```

R:FF G:E4 B:C4 | bisque

### black

```clojure
library/lux/data/color.Color
```

R:0 G:0 B:0 | black

### blanched\_almond

```clojure
library/lux/data/color.Color
```

R:FF G:EB B:CD | blanched almond

### blue

```clojure
library/lux/data/color.Color
```

R:0 G:0 B:FF | blue

### blue\_violet

```clojure
library/lux/data/color.Color
```

R:8A G:2B B:E2 | blue violet

### brown

```clojure
library/lux/data/color.Color
```

R:A5 G:2A B:2A | brown

### burly\_wood

```clojure
library/lux/data/color.Color
```

R:DE G:B8 B:87 | burly wood

### cadet\_blue

```clojure
library/lux/data/color.Color
```

R:5F G:9E B:A0 | cadet blue

### chartreuse

```clojure
library/lux/data/color.Color
```

R:7F G:FF B:0 | chartreuse

### chocolate

```clojure
library/lux/data/color.Color
```

R:D2 G:69 B:1E | chocolate

### coral

```clojure
library/lux/data/color.Color
```

R:FF G:7F B:50 | coral

### cornflower\_blue

```clojure
library/lux/data/color.Color
```

R:64 G:95 B:ED | cornflower blue

### cornsilk

```clojure
library/lux/data/color.Color
```

R:FF G:F8 B:DC | cornsilk

### crimson

```clojure
library/lux/data/color.Color
```

R:DC G:14 B:3C | crimson

### cyan

```clojure
library/lux/data/color.Color
```

R:0 G:FF B:FF | cyan

### dark\_blue

```clojure
library/lux/data/color.Color
```

R:0 G:0 B:8B | dark blue

### dark\_cyan

```clojure
library/lux/data/color.Color
```

R:0 G:8B B:8B | dark cyan

### dark\_goldenrod

```clojure
library/lux/data/color.Color
```

R:B8 G:86 B:B | dark goldenrod

### dark\_gray

```clojure
library/lux/data/color.Color
```

R:A9 G:A9 B:A9 | dark gray

### dark\_green

```clojure
library/lux/data/color.Color
```

R:0 G:64 B:0 | dark green

### dark\_khaki

```clojure
library/lux/data/color.Color
```

R:BD G:B7 B:6B | dark khaki

### dark\_magenta

```clojure
library/lux/data/color.Color
```

R:8B G:0 B:8B | dark magenta

### dark\_olive\_green

```clojure
library/lux/data/color.Color
```

R:55 G:6B B:2F | dark olive green

### dark\_orange

```clojure
library/lux/data/color.Color
```

R:FF G:8C B:0 | dark orange

### dark\_orchid

```clojure
library/lux/data/color.Color
```

R:99 G:32 B:CC | dark orchid

### dark\_red

```clojure
library/lux/data/color.Color
```

R:8B G:0 B:0 | dark red

### dark\_salmon

```clojure
library/lux/data/color.Color
```

R:E9 G:96 B:7A | dark salmon

### dark\_sea\_green

```clojure
library/lux/data/color.Color
```

R:8F G:BC B:8F | dark sea green

### dark\_slate\_blue

```clojure
library/lux/data/color.Color
```

R:48 G:3D B:8B | dark slate blue

### dark\_slate\_gray

```clojure
library/lux/data/color.Color
```

R:2F G:4F B:4F | dark slate gray

### dark\_turquoise

```clojure
library/lux/data/color.Color
```

R:0 G:CE B:D1 | dark turquoise

### dark\_violet

```clojure
library/lux/data/color.Color
```

R:94 G:0 B:D3 | dark violet

### deep\_pink

```clojure
library/lux/data/color.Color
```

R:FF G:14 B:93 | deep pink

### deep\_sky\_blue

```clojure
library/lux/data/color.Color
```

R:0 G:BF B:FF | deep sky blue

### dim\_gray

```clojure
library/lux/data/color.Color
```

R:69 G:69 B:69 | dim gray

### dodger\_blue

```clojure
library/lux/data/color.Color
```

R:1E G:90 B:FF | dodger blue

### fire\_brick

```clojure
library/lux/data/color.Color
```

R:B2 G:22 B:22 | fire brick

### floral\_white

```clojure
library/lux/data/color.Color
```

R:FF G:FA B:F0 | floral white

### forest\_green

```clojure
library/lux/data/color.Color
```

R:22 G:8B B:22 | forest green

### fuchsia

```clojure
library/lux/data/color.Color
```

R:FF G:0 B:FF | fuchsia

### gainsboro

```clojure
library/lux/data/color.Color
```

R:DC G:DC B:DC | gainsboro

### ghost\_white

```clojure
library/lux/data/color.Color
```

R:F8 G:F8 B:FF | ghost white

### gold

```clojure
library/lux/data/color.Color
```

R:FF G:D7 B:0 | gold

### goldenrod

```clojure
library/lux/data/color.Color
```

R:DA G:A5 B:20 | goldenrod

### gray

```clojure
library/lux/data/color.Color
```

R:80 G:80 B:80 | gray

### green

```clojure
library/lux/data/color.Color
```

R:0 G:80 B:0 | green

### green\_yellow

```clojure
library/lux/data/color.Color
```

R:AD G:FF B:2F | green yellow

### honey\_dew

```clojure
library/lux/data/color.Color
```

R:F0 G:FF B:F0 | honey dew

### hot\_pink

```clojure
library/lux/data/color.Color
```

R:FF G:69 B:B4 | hot pink

### indian\_red

```clojure
library/lux/data/color.Color
```

R:CD G:5C B:5C | indian red

### indigo

```clojure
library/lux/data/color.Color
```

R:4B G:0 B:82 | indigo

### ivory

```clojure
library/lux/data/color.Color
```

R:FF G:FF B:F0 | ivory

### khaki

```clojure
library/lux/data/color.Color
```

R:F0 G:E6 B:8C | khaki

### lavender

```clojure
library/lux/data/color.Color
```

R:E6 G:E6 B:FA | lavender

### lavender\_blush

```clojure
library/lux/data/color.Color
```

R:FF G:F0 B:F5 | lavender blush

### lawn\_green

```clojure
library/lux/data/color.Color
```

R:7C G:FC B:0 | lawn green

### lemon\_chiffon

```clojure
library/lux/data/color.Color
```

R:FF G:FA B:CD | lemon chiffon

### light\_blue

```clojure
library/lux/data/color.Color
```

R:AD G:D8 B:E6 | light blue

### light\_coral

```clojure
library/lux/data/color.Color
```

R:F0 G:80 B:80 | light coral

### light\_cyan

```clojure
library/lux/data/color.Color
```

R:E0 G:FF B:FF | light cyan

### light\_goldenrod\_yellow

```clojure
library/lux/data/color.Color
```

R:FA G:FA B:D2 | light goldenrod yellow

### light\_gray

```clojure
library/lux/data/color.Color
```

R:D3 G:D3 B:D3 | light gray

### light\_green

```clojure
library/lux/data/color.Color
```

R:90 G:EE B:90 | light green

### light\_pink

```clojure
library/lux/data/color.Color
```

R:FF G:B6 B:C1 | light pink

### light\_salmon

```clojure
library/lux/data/color.Color
```

R:FF G:A0 B:7A | light salmon

### light\_sea\_green

```clojure
library/lux/data/color.Color
```

R:20 G:B2 B:AA | light sea green

### light\_sky\_blue

```clojure
library/lux/data/color.Color
```

R:87 G:CE B:FA | light sky blue

### light\_slate\_gray

```clojure
library/lux/data/color.Color
```

R:77 G:88 B:99 | light slate gray

### light\_steel\_blue

```clojure
library/lux/data/color.Color
```

R:B0 G:C4 B:DE | light steel blue

### light\_yellow

```clojure
library/lux/data/color.Color
```

R:FF G:FF B:E0 | light yellow

### lime

```clojure
library/lux/data/color.Color
```

R:0 G:FF B:0 | lime

### lime\_green

```clojure
library/lux/data/color.Color
```

R:32 G:CD B:32 | lime green

### linen

```clojure
library/lux/data/color.Color
```

R:FA G:F0 B:E6 | linen

### magenta

```clojure
library/lux/data/color.Color
```

R:FF G:0 B:FF | magenta

### maroon

```clojure
library/lux/data/color.Color
```

R:80 G:0 B:0 | maroon

### medium\_aquamarine

```clojure
library/lux/data/color.Color
```

R:66 G:CD B:AA | medium aquamarine

### medium\_blue

```clojure
library/lux/data/color.Color
```

R:0 G:0 B:CD | medium blue

### medium\_orchid

```clojure
library/lux/data/color.Color
```

R:BA G:55 B:D3 | medium orchid

### medium\_purple

```clojure
library/lux/data/color.Color
```

R:93 G:70 B:DB | medium purple

### medium\_sea\_green

```clojure
library/lux/data/color.Color
```

R:3C G:B3 B:71 | medium sea green

### medium\_slate\_blue

```clojure
library/lux/data/color.Color
```

R:7B G:68 B:EE | medium slate blue

### medium\_spring\_green

```clojure
library/lux/data/color.Color
```

R:0 G:FA B:9A | medium spring green

### medium\_turquoise

```clojure
library/lux/data/color.Color
```

R:48 G:D1 B:CC | medium turquoise

### medium\_violet\_red

```clojure
library/lux/data/color.Color
```

R:C7 G:15 B:85 | medium violet red

### midnight\_blue

```clojure
library/lux/data/color.Color
```

R:19 G:19 B:70 | midnight blue

### mint\_cream

```clojure
library/lux/data/color.Color
```

R:F5 G:FF B:FA | mint cream

### misty\_rose

```clojure
library/lux/data/color.Color
```

R:FF G:E4 B:E1 | misty rose

### moccasin

```clojure
library/lux/data/color.Color
```

R:FF G:E4 B:B5 | moccasin

### navajo\_white

```clojure
library/lux/data/color.Color
```

R:FF G:DE B:AD | navajo white

### navy

```clojure
library/lux/data/color.Color
```

R:0 G:0 B:80 | navy

### old\_lace

```clojure
library/lux/data/color.Color
```

R:FD G:F5 B:E6 | old lace

### olive

```clojure
library/lux/data/color.Color
```

R:80 G:80 B:0 | olive

### olive\_drab

```clojure
library/lux/data/color.Color
```

R:6B G:8E B:23 | olive drab

### orange

```clojure
library/lux/data/color.Color
```

R:FF G:A5 B:0 | orange

### orange\_red

```clojure
library/lux/data/color.Color
```

R:FF G:45 B:0 | orange red

### orchid

```clojure
library/lux/data/color.Color
```

R:DA G:70 B:D6 | orchid

### pale\_goldenrod

```clojure
library/lux/data/color.Color
```

R:EE G:E8 B:AA | pale goldenrod

### pale\_green

```clojure
library/lux/data/color.Color
```

R:98 G:FB B:98 | pale green

### pale\_turquoise

```clojure
library/lux/data/color.Color
```

R:AF G:EE B:EE | pale turquoise

### pale\_violet\_red

```clojure
library/lux/data/color.Color
```

R:DB G:70 B:93 | pale violet red

### papaya\_whip

```clojure
library/lux/data/color.Color
```

R:FF G:EF B:D5 | papaya whip

### peach\_puff

```clojure
library/lux/data/color.Color
```

R:FF G:DA B:B9 | peach puff

### peru

```clojure
library/lux/data/color.Color
```

R:CD G:85 B:3F | peru

### pink

```clojure
library/lux/data/color.Color
```

R:FF G:C0 B:CB | pink

### plum

```clojure
library/lux/data/color.Color
```

R:DD G:A0 B:DD | plum

### powder\_blue

```clojure
library/lux/data/color.Color
```

R:B0 G:E0 B:E6 | powder blue

### purple

```clojure
library/lux/data/color.Color
```

R:80 G:0 B:80 | purple

### rebecca\_purple

```clojure
library/lux/data/color.Color
```

R:66 G:33 B:99 | rebecca purple

### red

```clojure
library/lux/data/color.Color
```

R:FF G:0 B:0 | red

### rosy\_brown

```clojure
library/lux/data/color.Color
```

R:BC G:8F B:8F | rosy brown

### royal\_blue

```clojure
library/lux/data/color.Color
```

R:41 G:69 B:E1 | royal blue

### saddle\_brown

```clojure
library/lux/data/color.Color
```

R:8B G:45 B:13 | saddle brown

### salmon

```clojure
library/lux/data/color.Color
```

R:FA G:80 B:72 | salmon

### sandy\_brown

```clojure
library/lux/data/color.Color
```

R:F4 G:A4 B:60 | sandy brown

### sea\_green

```clojure
library/lux/data/color.Color
```

R:2E G:8B B:57 | sea green

### sea\_shell

```clojure
library/lux/data/color.Color
```

R:FF G:F5 B:EE | sea shell

### sienna

```clojure
library/lux/data/color.Color
```

R:A0 G:52 B:2D | sienna

### silver

```clojure
library/lux/data/color.Color
```

R:C0 G:C0 B:C0 | silver

### sky\_blue

```clojure
library/lux/data/color.Color
```

R:87 G:CE B:EB | sky blue

### slate\_blue

```clojure
library/lux/data/color.Color
```

R:6A G:5A B:CD | slate blue

### slate\_gray

```clojure
library/lux/data/color.Color
```

R:70 G:80 B:90 | slate gray

### snow

```clojure
library/lux/data/color.Color
```

R:FF G:FA B:FA | snow

### spring\_green

```clojure
library/lux/data/color.Color
```

R:0 G:FF B:7F | spring green

### steel\_blue

```clojure
library/lux/data/color.Color
```

R:46 G:82 B:B4 | steel blue

### tan

```clojure
library/lux/data/color.Color
```

R:D2 G:B4 B:8C | tan

### teal

```clojure
library/lux/data/color.Color
```

R:0 G:80 B:80 | teal

### thistle

```clojure
library/lux/data/color.Color
```

R:D8 G:BF B:D8 | thistle

### tomato

```clojure
library/lux/data/color.Color
```

R:FF G:63 B:47 | tomato

### turquoise

```clojure
library/lux/data/color.Color
```

R:40 G:E0 B:D0 | turquoise

### violet

```clojure
library/lux/data/color.Color
```

R:EE G:82 B:EE | violet

### wheat

```clojure
library/lux/data/color.Color
```

R:F5 G:DE B:B3 | wheat

### white

```clojure
library/lux/data/color.Color
```

R:FF G:FF B:FF | white

### white\_smoke

```clojure
library/lux/data/color.Color
```

R:F5 G:F5 B:F5 | white smoke

### yellow

```clojure
library/lux/data/color.Color
```

R:FF G:FF B:0 | yellow

### yellow\_green

```clojure
library/lux/data/color.Color
```

R:9A G:CD B:32 | yellow green

___

# library/lux/data/format/binary

## Definitions

### Mutation

```clojure
... .Type
(-> [library/lux/control/parser/binary.Offset library/lux/data/binary.Binary] [library/lux/control/parser/binary.Offset library/lux/data/binary.Binary])
```

A mutation of binary data, tracking where in the data to transform\.

### Specification

```clojure
... .Type
[library/lux/control/parser/binary.Size Mutation]
```

A description of how to transform binary data\.

### \(Writer it\)

```clojure
... .Type
(-> it Specification)
```

An operation that knows how to write information into a binary blob\.

### and

```clojure
(All (_ _0 _1)
  (-> (Writer _0) (Writer _1) (Writer [_0 _1])))
```

```clojure
(and pre post)
```

### any

```clojure
(Writer .Any)
```

### binary/16

```clojure
(Writer library/lux/data/binary.Binary)
```

### binary/32

```clojure
(Writer library/lux/data/binary.Binary)
```

### binary/64

```clojure
(Writer library/lux/data/binary.Binary)
```

### binary/8

```clojure
(Writer library/lux/data/binary.Binary)
```

### bit

```clojure
(Writer .Bit)
```

### bits/16

```clojure
(Writer (.I64 .Any))
```

### bits/32

```clojure
(Writer (.I64 .Any))
```

### bits/64

```clojure
(Writer (.I64 .Any))
```

### bits/8

```clojure
(Writer (.I64 .Any))
```

### code

```clojure
(Writer .Code)
```

### frac

```clojure
(Writer .Frac)
```

### instance

```clojure
(-> Specification library/lux/data/binary.Binary)
```

Given a specification of how to construct binary data, yields a binary blob that matches it\.

### int

```clojure
(Writer .Int)
```

### list

```clojure
(All (_ _0)
  (-> (Writer _0) (Writer (.List _0))))
```

### location

```clojure
(Writer .Location)
```

### maybe

```clojure
(All (_ _0)
  (-> (Writer _0) (Writer (.Maybe _0))))
```

### monoid

```clojure
(library/lux/abstract/monoid.Monoid Specification)
```

### nat

```clojure
(Writer .Nat)
```

### no\_op

```clojure
Specification
```

A specification for empty binary data\.

### or

```clojure
(All (_ _0 _1)
  (-> (Writer _0) (Writer _1) (Writer (Or _0 _1))))
```

```clojure
(or left right)
```

### rec

```clojure
(All (_ _0)
  (-> (-> (Writer _0) (Writer _0)) (Writer _0)))
```

A combinator for recursive writers\.

```clojure
(rec body)
```

### result

```clojure
(All (_ _0)
  (-> (Writer _0) _0 library/lux/data/binary.Binary))
```

Yields a binary blob with all the information written to it\.

```clojure
(result writer value)
```

### rev

```clojure
(Writer .Rev)
```

### segment

```clojure
(-> .Nat (Writer library/lux/data/binary.Binary))
```

Writes at most 'size' bytes of an input binary blob\.

```clojure
(segment size)
```

### sequence/16

```clojure
(All (_ _0)
  (-> (Writer _0) (Writer (library/lux/data/collection/sequence.Sequence _0))))
```

### sequence/32

```clojure
(All (_ _0)
  (-> (Writer _0) (Writer (library/lux/data/collection/sequence.Sequence _0))))
```

### sequence/64

```clojure
(All (_ _0)
  (-> (Writer _0) (Writer (library/lux/data/collection/sequence.Sequence _0))))
```

### sequence/8

```clojure
(All (_ _0)
  (-> (Writer _0) (Writer (library/lux/data/collection/sequence.Sequence _0))))
```

### set

```clojure
(All (_ _0)
  (-> (Writer _0) (Writer (library/lux/data/collection/set.Set _0))))
```

### symbol

```clojure
(Writer .Symbol)
```

### text

```clojure
(Writer .Text)
```

### type

```clojure
(Writer .Type)
```

### utf8/16

```clojure
(Writer .Text)
```

### utf8/32

```clojure
(Writer .Text)
```

### utf8/64

```clojure
(Writer .Text)
```

### utf8/8

```clojure
(Writer .Text)
```

___

# library/lux/data/format/json

Functionality for reading and writing values in the JSON format\.
For more information, please see: http://www\.json\.org/

## Definitions

### Array

```clojure
... .Type
(library/lux/data/collection/sequence.Sequence JSON)
```

### Boolean

```clojure
... .Type
(Primitive "#Bit")
```

### JSON

```clojure
... .Type
(Rec JSON
 (Variant
  {#Null Null}
  {#Boolean Boolean}
  {#Number Number}
  {#String String}
  {#Array (library/lux/data/collection/sequence.Sequence JSON)}
  {#Object (library/lux/data/collection/dictionary.Dictionary String JSON)}))
```

### Null

```clojure
... .Type
(Ex (Null _0)
  _0)
```

### Number

```clojure
... .Type
(Primitive "#Frac")
```

### Object

```clojure
... .Type
(library/lux/data/collection/dictionary.Dictionary String JSON)
```

### String

```clojure
... .Type
(Primitive "#Text")
```

### array\_field

```clojure
(-> .Text JSON (library/lux/control/try.Try Array))
```

A JSON object field getter for arrays\.

### boolean\_field

```clojure
(-> .Text JSON (library/lux/control/try.Try Boolean))
```

A JSON object field getter for booleans\.

### codec

```clojure
(library/lux/abstract/codec.Codec .Text JSON)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence JSON)
```

### field

```clojure
(-> String JSON (library/lux/control/try.Try JSON))
```

A JSON object field getter\.

```clojure
(field key json)
```

### fields

```clojure
(-> JSON (library/lux/control/try.Try (.List String)))
```

Get all the fields in a JSON object\.

```clojure
(fields json)
```

### format

```clojure
(-> JSON .Text)
```

### has

```clojure
(-> String JSON JSON (library/lux/control/try.Try JSON))
```

A JSON object field setter\.

```clojure
(has key value json)
```

### json

```clojure
.Macro
```

A simple way to produce JSON literals\.

```clojure
... null

(json #null)

................................................................
................................................................

... true

(json #1)

................................................................
................................................................

... 123.456

(json +123.456)

................................................................
................................................................

... 'this is a string'

(json "this is a string")

................................................................
................................................................

... ['this' 'is' 'an' 'array']

(json ["this" "is" "an" "array"])

................................................................
................................................................

... {'this' 'is', 'an' 'object'}

(json {"this" "is" "an" "object"})
```

### null?

```clojure
(library/lux/abstract/predicate.Predicate JSON)
```

### number\_field

```clojure
(-> .Text JSON (library/lux/control/try.Try Number))
```

A JSON object field getter for numbers\.

### object

```clojure
(-> (.List [String JSON]) JSON)
```

### object\_field

```clojure
(-> .Text JSON (library/lux/control/try.Try Object))
```

A JSON object field getter for objects\.

### string\_field

```clojure
(-> .Text JSON (library/lux/control/try.Try String))
```

A JSON object field getter for strings\.

___

# library/lux/data/format/tar

## Definitions

### Big

```clojure
... .Type
(Primitive "library/lux/data/format/tar.Big")
```

### Content

```clojure
... .Type
(Primitive "library/lux/data/format/tar.Content")
```

### Contiguous

```clojure
... .Type
[Path library/lux/time/instant.Instant Mode Ownership Content]
```

### Directory

```clojure
... .Type
(Primitive "library/lux/data/format/tar.Path")
```

### Entry

```clojure
... .Type
(Variant
 {#Normal Normal}
 {#Symbolic_Link Symbolic_Link}
 {#Directory Directory}
 {#Contiguous Contiguous})
```

### File

```clojure
... .Type
[Path library/lux/time/instant.Instant Mode Ownership Content]
```

### ID

```clojure
... .Type
(Primitive "library/lux/data/format/tar.Small")
```

### Mode

```clojure
... .Type
(Primitive "library/lux/data/format/tar.Mode")
```

### Name

```clojure
... .Type
(Primitive "library/lux/data/format/tar.Name")
```

### Normal

```clojure
... .Type
[Path library/lux/time/instant.Instant Mode Ownership Content]
```

### Owner

```clojure
... .Type
(Record
 [#name Name
  #id ID])
```

### Ownership

```clojure
... .Type
(Record
 [#user Owner
  #group Owner])
```

### Path

```clojure
... .Type
(Primitive "library/lux/data/format/tar.Path")
```

### Small

```clojure
... .Type
(Primitive "library/lux/data/format/tar.Small")
```

### Symbolic\_Link

```clojure
... .Type
(Primitive "library/lux/data/format/tar.Path")
```

### Tar

```clojure
... .Type
(library/lux/data/collection/sequence.Sequence Entry)
```

### and

```clojure
(-> Mode Mode Mode)
```

### anonymous

```clojure
Name
```

### big

```clojure
(-> .Nat (library/lux/control/try.Try Big))
```

### big\_limit

```clojure
.Nat
```

### content

```clojure
(-> library/lux/data/binary.Binary (library/lux/control/try.Try Content))
```

### data

```clojure
(-> Content library/lux/data/binary.Binary)
```

### execute\_by\_group

```clojure
Mode
```

### execute\_by\_other

```clojure
Mode
```

### execute\_by\_owner

```clojure
Mode
```

### from\_big

```clojure
(-> Big .Nat)
```

### from\_name

```clojure
(-> Name .Text)
```

### from\_path

```clojure
(-> Path library/lux/world/file.Path)
```

### from\_small

```clojure
(-> Small .Nat)
```

### invalid\_end\_of\_archive

```clojure
(library/lux/control/exception.Exception .Any)
```

### invalid\_link\_flag

```clojure
(library/lux/control/exception.Exception .Nat)
```

### invalid\_mode

```clojure
(library/lux/control/exception.Exception .Nat)
```

### mode

```clojure
(-> Mode .Nat)
```

### name

```clojure
(-> .Text (library/lux/control/try.Try Name))
```

### name\_is\_too\_long

```clojure
(library/lux/control/exception.Exception .Text)
```

### name\_size

```clojure
Size
```

### no\_id

```clojure
ID
```

### no\_path

```clojure
Path
```

### none

```clojure
Mode
```

### not\_a\_big\_number

```clojure
(library/lux/control/exception.Exception .Nat)
```

### not\_a\_small\_number

```clojure
(library/lux/control/exception.Exception .Nat)
```

### not\_ascii

```clojure
(library/lux/control/exception.Exception .Text)
```

### parser

```clojure
(library/lux/control/parser/binary.Parser Tar)
```

### path

```clojure
(-> library/lux/world/file.Path (library/lux/control/try.Try Path))
```

### path\_is\_too\_long

```clojure
(library/lux/control/exception.Exception .Text)
```

### path\_size

```clojure
Size
```

### read\_by\_group

```clojure
Mode
```

### read\_by\_other

```clojure
Mode
```

### read\_by\_owner

```clojure
Mode
```

### save\_text

```clojure
Mode
```

### set\_group\_id\_on\_execution

```clojure
Mode
```

### set\_user\_id\_on\_execution

```clojure
Mode
```

### small

```clojure
(-> .Nat (library/lux/control/try.Try Small))
```

### small\_limit

```clojure
.Nat
```

### write\_by\_group

```clojure
Mode
```

### write\_by\_other

```clojure
Mode
```

### write\_by\_owner

```clojure
Mode
```

### writer

```clojure
(library/lux/data/format/binary.Writer Tar)
```

### wrong\_character

```clojure
(library/lux/control/exception.Exception [library/lux/data/text.Char library/lux/data/text.Char])
```

### wrong\_checksum

```clojure
(library/lux/control/exception.Exception [.Nat .Nat])
```

### wrong\_link\_flag

```clojure
(library/lux/control/exception.Exception [Link_Flag Link_Flag])
```

___

# library/lux/data/format/xml

## Definitions

### Attribute

```clojure
... .Type
[.Text .Text]
```

### Attrs

```clojure
... .Type
(library/lux/data/collection/dictionary.Dictionary Attribute .Text)
```

### Tag

```clojure
... .Type
[.Text .Text]
```

### XML

```clojure
... .Type
(Rec XML
 (Variant
  {#Text .Text}
  {#Node Tag Attrs (.List XML)}))
```

### attribute

```clojure
(-> Attribute .Text)
```

The text format of a XML attribute\.

### attributes

```clojure
Attrs
```

An empty set of XML attributes\.

### codec

```clojure
(library/lux/abstract/codec.Codec .Text XML)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence XML)
```

### tag

```clojure
(-> Tag .Text)
```

The text format of a XML tag\.

```clojure
(tag name)
```

___

# library/lux/data/identity

## Definitions

### \(Identity it\)

```clojure
... .Type
it
```

A value, as is, without any extra structure super\-imposed on it\.

### apply

```clojure
(library/lux/abstract/apply.Apply Identity)
```

### comonad

```clojure
(library/lux/abstract/comonad.CoMonad Identity)
```

### functor

```clojure
(library/lux/abstract/functor.Functor Identity)
```

### monad

```clojure
(library/lux/abstract/monad.Monad Identity)
```

___

# library/lux/data/product

Functionality for working with tuples \(particularly 2\-tuples/pairs\)\.

## Definitions

### curried

```clojure
(All (_ _0 _1 _2)
  (-> (-> [_0 _1] _2) _0 _1 _2))
```

Converts a 2\-argument function into nested single\-argument functions\.

```clojure
(curried f)
```

### equivalence

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence _1) (library/lux/abstract/equivalence.Equivalence [_0 _1])))
```

### forked

```clojure
(All (_ _0 _1 _2)
  (-> (-> _0 _1) (-> _0 _2) _0 [_1 _2]))
```

Yields a pair by applying both functions to a single value\.

```clojure
(forked f g)
```

### hash

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/hash.Hash _0) (library/lux/abstract/hash.Hash _1) (library/lux/abstract/hash.Hash [_0 _1])))
```

### left

```clojure
(All (_ _0 _1)
  (-> [_0 _1] _0))
```

The left side of a pair\.

### right

```clojure
(All (_ _0 _1)
  (-> [_0 _1] _1))
```

The right side of a pair\.

### swapped

```clojure
(All (_ _0 _1)
  (-> [_0 _1] [_1 _0]))
```

```clojure
(swapped [left right])
```

### then

```clojure
(All (_ _0 _1 _2 _3)
  (-> (-> _0 _2) (-> _1 _3) [_0 _1] [_2 _3]))
```

Apply functions to both sides of a pair\.

```clojure
(then f g)
```

### uncurried

```clojure
(All (_ _0 _1 _2)
  (-> (-> _0 _1 _2) [_0 _1] _2))
```

Converts nested single\-argument functions into a 2\-argument function\.

```clojure
(uncurried f)
```

___

# library/lux/data/sum

Functionality for working with variants \(particularly 2\-variants\)\.

## Definitions

### either

```clojure
(All (_ _0 _1 _2)
  (-> (-> _0 _2) (-> _1 _2) (Or _0 _1) _2))
```

Applies a function to either side of a 2\-variant\.

```clojure
(either on_left on_right)
```

### equivalence

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence _1) (library/lux/abstract/equivalence.Equivalence (Or _0 _1))))
```

### hash

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/hash.Hash _0) (library/lux/abstract/hash.Hash _1) (library/lux/abstract/hash.Hash (Or _0 _1))))
```

### left

```clojure
(All (_ _0 _1)
  (-> _0 (Or _0 _1)))
```

Lifts value to the left side of a 2\-variant\.

### lefts

```clojure
(All (_ _0 _1)
  (-> (.List (Or _0 _1)) (.List _0)))
```

### partition

```clojure
(All (_ _0 _1)
  (-> (.List (Or _0 _1)) [(.List _0) (.List _1)]))
```

### right

```clojure
(All (_ _0 _1)
  (-> _1 (Or _0 _1)))
```

Lifts value to the right side of a 2\-variant\.

### rights

```clojure
(All (_ _0 _1)
  (-> (.List (Or _0 _1)) (.List _1)))
```

### then

```clojure
(All (_ _0 _1 _2 _3)
  (-> (-> _0 _1) (-> _2 _3) (Or _0 _2) (Or _1 _3)))
```

Applies functions to both sides of a 2\-variant\.

```clojure
(then on_left on_right)
```

___

# library/lux/data/text

## Definitions

### Char

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

A character code number\.

### \\''

```clojure
.Text
```

### \\0

```clojure
.Text
```

### \\a

```clojure
.Text
```

### \\b

```clojure
.Text
```

### \\f

```clojure
.Text
```

### \\n

```clojure
.Text
```

### \\r

```clojure
.Text
```

### \\t

```clojure
.Text
```

### \\v

```clojure
.Text
```

### alarm

```clojure
.Text
```

### all\_split\_by

```clojure
(-> .Text .Text (.List .Text))
```

```clojure
(all_split_by token sample)
```

### back\_space

```clojure
.Text
```

### carriage\_return

```clojure
.Text
```

### char

```clojure
(-> .Nat .Text (.Maybe Char))
```

Yields the character at the specified index\.

```clojure
(char index input)
```

### clip

```clojure
(-> .Nat .Nat .Text (.Maybe .Text))
```

Clips a chunk of text from the input at the specified offset and of the specified size\.

```clojure
(clip offset size input)
```

### clip\_since

```clojure
(-> .Nat .Text (.Maybe .Text))
```

Clips the remaining text from the input at the specified offset\.

```clojure
(clip_since offset input)
```

### contains?

```clojure
(-> .Text .Text .Bit)
```

```clojure
(contains? sub text)
```

### double\_quote

```clojure
.Text
```

### empty?

```clojure
(-> .Text .Bit)
```

### enclosed

```clojure
(-> [.Text .Text] .Text .Text)
```

Surrounds the given content text with left and right side additions\.

```clojure
(enclosed [left right] content)
```

### enclosed'

```clojure
(-> .Text .Text .Text)
```

Surrounds the given content text with the same boundary text\.

```clojure
(enclosed' boundary content)
```

### enclosed\_by?

```clojure
(-> .Text .Text .Bit)
```

```clojure
(enclosed_by? boundary value)
```

### ends\_with?

```clojure
(-> .Text .Text .Bit)
```

```clojure
(ends_with? postfix x)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Text)
```

### form\_feed

```clojure
.Text
```

### format

```clojure
(-> .Text .Text)
```

### hash

```clojure
(library/lux/abstract/hash.Hash .Text)
```

### index

```clojure
(-> .Text .Text (.Maybe .Nat))
```

```clojure
(index pattern input)
```

### index\_since

```clojure
(-> .Nat .Text .Text (.Maybe .Nat))
```

```clojure
(index_since from pattern input)
```

### interposed

```clojure
(-> .Text (.List .Text) .Text)
```

```clojure
(interposed separator texts)
```

### last\_index

```clojure
(-> .Text .Text (.Maybe .Nat))
```

```clojure
(last_index part text)
```

### line\_feed

```clojure
.Text
```

Same as 'new\_line'\.

### lower\_cased

```clojure
(-> .Text .Text)
```

### monoid

```clojure
(library/lux/abstract/monoid.Monoid .Text)
```

### new\_line

```clojure
.Text
```

### null

```clojure
.Text
```

### of\_char

```clojure
(-> Char .Text)
```

### order

```clojure
(library/lux/abstract/order.Order .Text)
```

### prefix

```clojure
(-> .Text .Text .Text)
```

```clojure
(prefix param subject)
```

### replaced

```clojure
(-> .Text .Text .Text .Text)
```

```clojure
(replaced pattern replacement template)
```

### replaced/1

```clojure
(-> .Text .Text .Text .Text)
```

```clojure
(replaced/1 pattern replacement template)
```

### size

```clojure
(-> .Text .Nat)
```

### space

```clojure
.Text
```

### space?

```clojure
(-> Char .Bit)
```

Checks whether the character is white\-space\.

```clojure
(space? char)
```

### split\_at

```clojure
(-> .Nat .Text (.Maybe [.Text .Text]))
```

```clojure
(split_at at x)
```

### split\_by

```clojure
(-> .Text .Text (.Maybe [.Text .Text]))
```

```clojure
(split_by token sample)
```

### starts\_with?

```clojure
(-> .Text .Text .Bit)
```

```clojure
(starts_with? prefix x)
```

### suffix

```clojure
(-> .Text .Text .Text)
```

```clojure
(suffix param subject)
```

### tab

```clojure
.Text
```

### together

```clojure
(-> (.List .Text) .Text)
```

### upper\_cased

```clojure
(-> .Text .Text)
```

### vertical\_tab

```clojure
.Text
```

___

# library/lux/data/text/buffer

## Definitions

### Buffer

```clojure
... .Type
(Primitive "library/lux/data/text/buffer.Buffer")
```

Immutable text buffer for efficient text concatenation\.

### empty

```clojure
Buffer
```

### size

```clojure
(-> Buffer .Nat)
```

### text

```clojure
(-> Buffer .Text)
```

### then

```clojure
(-> .Text Buffer Buffer)
```

___

# library/lux/data/text/encoding

## Definitions

### Encoding

```clojure
... .Type
(Primitive "library/lux/data/text/encoding.Encoding")
```

Encoding formats for text\.

### ascii

```clojure
Encoding
```

'ASCII' text encoding\. 

### cesu\_8

```clojure
Encoding
```

'CESU\-8' text encoding\. 

### ibm\_037

```clojure
Encoding
```

'IBM037' text encoding\. 

### ibm\_1006

```clojure
Encoding
```

'IBM1006' text encoding\. 

### ibm\_1025

```clojure
Encoding
```

'IBM1025' text encoding\. 

### ibm\_1026

```clojure
Encoding
```

'IBM1026' text encoding\. 

### ibm\_1046

```clojure
Encoding
```

'IBM1046' text encoding\. 

### ibm\_1047

```clojure
Encoding
```

'IBM1047' text encoding\. 

### ibm\_1097

```clojure
Encoding
```

'IBM1097' text encoding\. 

### ibm\_1098

```clojure
Encoding
```

'IBM1098' text encoding\. 

### ibm\_1112

```clojure
Encoding
```

'IBM1112' text encoding\. 

### ibm\_1122

```clojure
Encoding
```

'IBM1122' text encoding\. 

### ibm\_1123

```clojure
Encoding
```

'IBM1123' text encoding\. 

### ibm\_1124

```clojure
Encoding
```

'IBM1124' text encoding\. 

### ibm\_1140

```clojure
Encoding
```

'IBM01140' text encoding\. 

### ibm\_1141

```clojure
Encoding
```

'IBM01141' text encoding\. 

### ibm\_1142

```clojure
Encoding
```

'IBM01142' text encoding\. 

### ibm\_1143

```clojure
Encoding
```

'IBM01143' text encoding\. 

### ibm\_1144

```clojure
Encoding
```

'IBM01144' text encoding\. 

### ibm\_1145

```clojure
Encoding
```

'IBM01145' text encoding\. 

### ibm\_1146

```clojure
Encoding
```

'IBM01146' text encoding\. 

### ibm\_1147

```clojure
Encoding
```

'IBM01147' text encoding\. 

### ibm\_1148

```clojure
Encoding
```

'IBM01148' text encoding\. 

### ibm\_1149

```clojure
Encoding
```

'IBM01149' text encoding\. 

### ibm\_1166

```clojure
Encoding
```

'IBM1166' text encoding\. 

### ibm\_1364

```clojure
Encoding
```

'IBM1364' text encoding\. 

### ibm\_1381

```clojure
Encoding
```

'IBM1381' text encoding\. 

### ibm\_1383

```clojure
Encoding
```

'IBM1383' text encoding\. 

### ibm\_273

```clojure
Encoding
```

'IBM273' text encoding\. 

### ibm\_277

```clojure
Encoding
```

'IBM277' text encoding\. 

### ibm\_278

```clojure
Encoding
```

'IBM278' text encoding\. 

### ibm\_280

```clojure
Encoding
```

'IBM280' text encoding\. 

### ibm\_284

```clojure
Encoding
```

'IBM284' text encoding\. 

### ibm\_285

```clojure
Encoding
```

'IBM285' text encoding\. 

### ibm\_290

```clojure
Encoding
```

'IBM290' text encoding\. 

### ibm\_297

```clojure
Encoding
```

'IBM297' text encoding\. 

### ibm\_300

```clojure
Encoding
```

'IBM300' text encoding\. 

### ibm\_33722

```clojure
Encoding
```

'IBM33722' text encoding\. 

### ibm\_420

```clojure
Encoding
```

'IBM420' text encoding\. 

### ibm\_424

```clojure
Encoding
```

'IBM424' text encoding\. 

### ibm\_437

```clojure
Encoding
```

'IBM437' text encoding\. 

### ibm\_500

```clojure
Encoding
```

'IBM500' text encoding\. 

### ibm\_737

```clojure
Encoding
```

'IBM737' text encoding\. 

### ibm\_775

```clojure
Encoding
```

'IBM775' text encoding\. 

### ibm\_833

```clojure
Encoding
```

'IBM833' text encoding\. 

### ibm\_834

```clojure
Encoding
```

'IBM834' text encoding\. 

### ibm\_838

```clojure
Encoding
```

'IBM\-Thai' text encoding\. 

### ibm\_850

```clojure
Encoding
```

'IBM850' text encoding\. 

### ibm\_852

```clojure
Encoding
```

'IBM852' text encoding\. 

### ibm\_855

```clojure
Encoding
```

'IBM855' text encoding\. 

### ibm\_856

```clojure
Encoding
```

'IBM856' text encoding\. 

### ibm\_857

```clojure
Encoding
```

'IBM857' text encoding\. 

### ibm\_858

```clojure
Encoding
```

'IBM00858' text encoding\. 

### ibm\_860

```clojure
Encoding
```

'IBM860' text encoding\. 

### ibm\_861

```clojure
Encoding
```

'IBM861' text encoding\. 

### ibm\_862

```clojure
Encoding
```

'IBM862' text encoding\. 

### ibm\_863

```clojure
Encoding
```

'IBM863' text encoding\. 

### ibm\_864

```clojure
Encoding
```

'IBM864' text encoding\. 

### ibm\_865

```clojure
Encoding
```

'IBM865' text encoding\. 

### ibm\_866

```clojure
Encoding
```

'IBM866' text encoding\. 

### ibm\_868

```clojure
Encoding
```

'IBM868' text encoding\. 

### ibm\_869

```clojure
Encoding
```

'IBM869' text encoding\. 

### ibm\_870

```clojure
Encoding
```

'IBM870' text encoding\. 

### ibm\_871

```clojure
Encoding
```

'IBM871' text encoding\. 

### ibm\_874

```clojure
Encoding
```

'IBM874' text encoding\. 

### ibm\_875

```clojure
Encoding
```

'IBM875' text encoding\. 

### ibm\_918

```clojure
Encoding
```

'IBM918' text encoding\. 

### ibm\_921

```clojure
Encoding
```

'IBM921' text encoding\. 

### ibm\_922

```clojure
Encoding
```

'IBM922' text encoding\. 

### ibm\_930

```clojure
Encoding
```

'IBM930' text encoding\. 

### ibm\_933

```clojure
Encoding
```

'IBM933' text encoding\. 

### ibm\_935

```clojure
Encoding
```

'IBM935' text encoding\. 

### ibm\_937

```clojure
Encoding
```

'IBM937' text encoding\. 

### ibm\_939

```clojure
Encoding
```

'IBM939' text encoding\. 

### ibm\_942

```clojure
Encoding
```

'IBM942' text encoding\. 

### ibm\_942c

```clojure
Encoding
```

'IBM942C' text encoding\. 

### ibm\_943

```clojure
Encoding
```

'IBM943' text encoding\. 

### ibm\_943c

```clojure
Encoding
```

'IBM943C' text encoding\. 

### ibm\_948

```clojure
Encoding
```

'IBM948' text encoding\. 

### ibm\_949

```clojure
Encoding
```

'IBM949' text encoding\. 

### ibm\_949c

```clojure
Encoding
```

'IBM949C' text encoding\. 

### ibm\_950

```clojure
Encoding
```

'IBM950' text encoding\. 

### ibm\_964

```clojure
Encoding
```

'IBM964' text encoding\. 

### ibm\_970

```clojure
Encoding
```

'IBM970' text encoding\. 

### iso2022\_cn\_cns

```clojure
Encoding
```

'ISO2022\-CN\-CNS' text encoding\. 

### iso2022\_cn\_gb

```clojure
Encoding
```

'ISO2022\-CN\-GB' text encoding\. 

### iso\_2022\_cn

```clojure
Encoding
```

'ISO\-2022\-CN' text encoding\. 

### iso\_2022\_jp

```clojure
Encoding
```

'ISO\-2022\-JP' text encoding\. 

### iso\_2022\_jp\_2

```clojure
Encoding
```

'ISO\-2022\-JP\-2' text encoding\. 

### iso\_2022\_kr

```clojure
Encoding
```

'ISO\-2022\-KR' text encoding\. 

### iso\_8859\_1

```clojure
Encoding
```

'ISO\-8859\-1' text encoding\. 

### iso\_8859\_11

```clojure
Encoding
```

'iso\-8859\-11' text encoding\. 

### iso\_8859\_13

```clojure
Encoding
```

'ISO\-8859\-13' text encoding\. 

### iso\_8859\_15

```clojure
Encoding
```

'ISO\-8859\-15' text encoding\. 

### iso\_8859\_2

```clojure
Encoding
```

'ISO\-8859\-2' text encoding\. 

### iso\_8859\_3

```clojure
Encoding
```

'ISO\-8859\-3' text encoding\. 

### iso\_8859\_4

```clojure
Encoding
```

'ISO\-8859\-4' text encoding\. 

### iso\_8859\_5

```clojure
Encoding
```

'ISO\-8859\-5' text encoding\. 

### iso\_8859\_6

```clojure
Encoding
```

'ISO\-8859\-6' text encoding\. 

### iso\_8859\_7

```clojure
Encoding
```

'ISO\-8859\-7' text encoding\. 

### iso\_8859\_8

```clojure
Encoding
```

'ISO\-8859\-8' text encoding\. 

### iso\_8859\_9

```clojure
Encoding
```

'ISO\-8859\-9' text encoding\. 

### koi8\_r

```clojure
Encoding
```

'KOI8\-R' text encoding\. 

### koi8\_u

```clojure
Encoding
```

'KOI8\-U' text encoding\. 

### mac\_arabic

```clojure
Encoding
```

'MacArabic' text encoding\. 

### mac\_central\_europe

```clojure
Encoding
```

'MacCentralEurope' text encoding\. 

### mac\_croatian

```clojure
Encoding
```

'MacCroatian' text encoding\. 

### mac\_cyrillic

```clojure
Encoding
```

'MacCyrillic' text encoding\. 

### mac\_dingbat

```clojure
Encoding
```

'MacDingbat' text encoding\. 

### mac\_greek

```clojure
Encoding
```

'MacGreek' text encoding\. 

### mac\_hebrew

```clojure
Encoding
```

'MacHebrew' text encoding\. 

### mac\_iceland

```clojure
Encoding
```

'MacIceland' text encoding\. 

### mac\_roman

```clojure
Encoding
```

'MacRoman' text encoding\. 

### mac\_romania

```clojure
Encoding
```

'MacRomania' text encoding\. 

### mac\_symbol

```clojure
Encoding
```

'MacSymbol' text encoding\. 

### mac\_thai

```clojure
Encoding
```

'MacThai' text encoding\. 

### mac\_turkish

```clojure
Encoding
```

'MacTurkish' text encoding\. 

### mac\_ukraine

```clojure
Encoding
```

'MacUkraine' text encoding\. 

### name

```clojure
(-> Encoding .Text)
```

### utf\_16

```clojure
Encoding
```

'UTF\-16' text encoding\. 

### utf\_32

```clojure
Encoding
```

'UTF\-32' text encoding\. 

### utf\_8

```clojure
Encoding
```

'UTF\-8' text encoding\. 

### windows\_1250

```clojure
Encoding
```

'windows\-1250' text encoding\. 

### windows\_1251

```clojure
Encoding
```

'windows\-1251' text encoding\. 

### windows\_1252

```clojure
Encoding
```

'windows\-1252' text encoding\. 

### windows\_1253

```clojure
Encoding
```

'windows\-1253' text encoding\. 

### windows\_1254

```clojure
Encoding
```

'windows\-1254' text encoding\. 

### windows\_1255

```clojure
Encoding
```

'windows\-1255' text encoding\. 

### windows\_1256

```clojure
Encoding
```

'windows\-1256' text encoding\. 

### windows\_1257

```clojure
Encoding
```

'windows\-1257' text encoding\. 

### windows\_1258

```clojure
Encoding
```

'windows\-1258' text encoding\. 

### windows\_31j

```clojure
Encoding
```

'windows\-31j' text encoding\. 

### windows\_50220

```clojure
Encoding
```

'windows\-50220' text encoding\. 

### windows\_50221

```clojure
Encoding
```

'windows\-50221' text encoding\. 

### windows\_874

```clojure
Encoding
```

'windows\-874' text encoding\. 

### windows\_949

```clojure
Encoding
```

'windows\-949' text encoding\. 

### windows\_950

```clojure
Encoding
```

'windows\-950' text encoding\. 

### windows\_iso2022jp

```clojure
Encoding
```

'windows\-iso2022jp' text encoding\. 

___

# library/lux/data/text/encoding/utf8

## Definitions

### codec

```clojure
(library/lux/abstract/codec.Codec library/lux/data/binary.Binary .Text)
```

A codec for binary encoding of text as UTF\-8\.

___

# library/lux/data/text/escape

## Definitions

### dangling\_escape

```clojure
(library/lux/control/exception.Exception .Text)
```

### escapable?

```clojure
(-> library/lux/data/text.Char .Bit)
```

### escaped

```clojure
(-> .Text .Text)
```

Yields a escaped version of the text\.

```clojure
(escaped text)
```

### invalid\_escape

```clojure
(library/lux/control/exception.Exception [.Text .Nat library/lux/data/text.Char])
```

### invalid\_unicode\_escape

```clojure
(library/lux/control/exception.Exception [.Text .Nat])
```

### literal

```clojure
.Macro
```

If given a escaped text literal, expands to an un\-escaped version\.

```clojure
(literal   "Line 1\nLine 2")

... =>

(format "Line 1" \n
        "Line 2")
```

### un\_escaped

```clojure
(-> .Text (library/lux/control/try.Try .Text))
```

Yields an un\-escaped text\.
Fails if it was improperly escaped\.

```clojure
(un_escaped text)
```

___

# library/lux/data/text/format

## Definitions

### \(Format it\)

```clojure
... .Type
(-> it .Text)
```

A way to produce readable text from values\.

### bit

```clojure
(Format .Bit)
```

### code

```clojure
(Format .Code)
```

### date

```clojure
(Format library/lux/time/date.Date)
```

### day

```clojure
(Format library/lux/time/day.Day)
```

### duration

```clojure
(Format library/lux/time/duration.Duration)
```

### format

```clojure
.Macro
```

Text interpolation\.

```clojure
(format "Static part " (text static) " does not match URI: " uri)
```

### frac

```clojure
(Format .Frac)
```

### frac/10

```clojure
(Format .Frac)
```

### frac/16

```clojure
(Format .Frac)
```

### frac/2

```clojure
(Format .Frac)
```

### frac/8

```clojure
(Format .Frac)
```

### functor

```clojure
(library/lux/abstract/functor/contravariant.Functor Format)
```

### instant

```clojure
(Format library/lux/time/instant.Instant)
```

### int

```clojure
(Format .Int)
```

### int/10

```clojure
(Format .Int)
```

### int/16

```clojure
(Format .Int)
```

### int/2

```clojure
(Format .Int)
```

### int/8

```clojure
(Format .Int)
```

### json

```clojure
(Format library/lux/data/format/json.JSON)
```

### list

```clojure
(All (_ _0)
  (-> (Format _0) (Format (.List _0))))
```

### location

```clojure
(Format .Location)
```

### maybe

```clojure
(All (_ _0)
  (-> (Format _0) (Format (.Maybe _0))))
```

### mod

```clojure
(All (_ _0)
  (Format (library/lux/math/modular.Mod _0)))
```

### month

```clojure
(Format library/lux/time/month.Month)
```

### nat

```clojure
(Format .Nat)
```

### nat/10

```clojure
(Format .Nat)
```

### nat/16

```clojure
(Format .Nat)
```

### nat/2

```clojure
(Format .Nat)
```

### nat/8

```clojure
(Format .Nat)
```

### ratio

```clojure
(Format library/lux/math/number/ratio.Ratio)
```

### rev

```clojure
(Format .Rev)
```

### rev/10

```clojure
(Format .Rev)
```

### rev/16

```clojure
(Format .Rev)
```

### rev/2

```clojure
(Format .Rev)
```

### rev/8

```clojure
(Format .Rev)
```

### symbol

```clojure
(Format .Symbol)
```

### text

```clojure
(Format .Text)
```

### time

```clojure
(Format library/lux/time.Time)
```

### type

```clojure
(Format .Type)
```

### xml

```clojure
(Format library/lux/data/format/xml.XML)
```

___

# library/lux/data/text/regex

## Definitions

### ^regex

```clojure
.Macro
```

Allows you to test text against regular expressions\.

```clojure
(case some_text
  (^regex "(\d{3})-(\d{3})-(\d{4})"
          [_ country_code area_code place_code])
  do_some_thing_when_number

  (^regex "\w+")
  do_some_thing_when_word

  _
  do_something_else)
```

### incorrect\_quantification

```clojure
(library/lux/control/exception.Exception [.Nat .Nat])
```

### regex

```clojure
.Macro
```

Create lexers using regular\-expression syntax\.

```clojure
... Literals

(regex "a")

................................................................
................................................................

... Wildcards

(regex ".")

................................................................
................................................................

... Escaping

(regex "\.")

................................................................
................................................................

... Character classes

(regex "\d")

(regex "\p{Lower}")

(regex "[abc]")

(regex "[a-z]")

(regex "[a-zA-Z]")

(regex "[a-z&&[def]]")

................................................................
................................................................

... Negation

(regex "[^abc]")

(regex "[^a-z]")

(regex "[^a-zA-Z]")

(regex "[a-z&&[^bc]]")

(regex "[a-z&&[^m-p]]")

................................................................
................................................................

... Combinations

(regex "aa")

(regex "a?")

(regex "a*")

(regex "a+")

................................................................
................................................................

... Specific amounts

(regex "a{2}")

................................................................
................................................................

... At least

(regex "a{1,}")

................................................................
................................................................

... At most

(regex "a{,1}")

................................................................
................................................................

... Between

(regex "a{1,2}")

................................................................
................................................................

... Groups

(regex "a(.)c")

(regex "a(b+)c")

(regex "(\d{3})-(\d{3})-(\d{4})")

(regex "(\d{3})-(?:\d{3})-(\d{4})")

(regex "(?<code>\d{3})-\k<code>-(\d{4})")

(regex "(?<code>\d{3})-\k<code>-(\d{4})-\0")

(regex "(\d{3})-((\d{3})-(\d{4}))")

................................................................
................................................................

... Alternation

(regex "a|b")

(regex "a(.)(.)|b(.)(.)")
```

___

# library/lux/data/text/unicode/block

## Definitions

### Block

```clojure
... .Type
(Primitive "library/lux/data/text/unicode/block.Block")
```

A block of valid unicode characters\.

### alphabetic\_presentation\_forms

```clojure
Block
```

FB00\-FB4F | alphabetic presentation forms

### arabic

```clojure
Block
```

600\-6FF | arabic

### arabic\_presentation\_forms\_a

```clojure
Block
```

FB50\-FDFF | arabic presentation forms a

### arabic\_presentation\_forms\_b

```clojure
Block
```

FE70\-FEFF | arabic presentation forms b

### armenian

```clojure
Block
```

530\-58F | armenian

### arrows

```clojure
Block
```

2190\-21FF | arrows

### basic\_latin

```clojure
Block
```

0\-7F | basic latin

### basic\_latin/decimal

```clojure
Block
```

30\-39 | basic latin/decimal

### basic\_latin/lower

```clojure
Block
```

61\-7A | basic latin/lower

### basic\_latin/upper

```clojure
Block
```

41\-5A | basic latin/upper

### bengali

```clojure
Block
```

980\-9FF | bengali

### block

```clojure
(-> library/lux/data/text.Char .Nat Block)
```

```clojure
(block start additional)
```

### block\_elements

```clojure
Block
```

2580\-259F | block elements

### bopomofo

```clojure
Block
```

3100\-312F | bopomofo

### bopomofo\_extended

```clojure
Block
```

31A0\-31BF | bopomofo extended

### box\_drawing

```clojure
Block
```

2500\-257F | box drawing

### braille\_patterns

```clojure
Block
```

2800\-28FF | braille patterns

### buhid

```clojure
Block
```

1740\-175F | buhid

### cherokee

```clojure
Block
```

13A0\-13FF | cherokee

### cjk\_compatibility

```clojure
Block
```

3300\-33FF | cjk compatibility

### cjk\_compatibility\_forms

```clojure
Block
```

FE30\-FE4F | cjk compatibility forms

### cjk\_compatibility\_ideographs

```clojure
Block
```

F900\-FAFF | cjk compatibility ideographs

### cjk\_radicals\_supplement

```clojure
Block
```

2E80\-2EFF | cjk radicals supplement

### cjk\_symbols\_and\_punctuation

```clojure
Block
```

3000\-303F | cjk symbols and punctuation

### cjk\_unified\_ideographs

```clojure
Block
```

4E00\-9FFF | cjk unified ideographs

### cjk\_unified\_ideographs\_extension\_a

```clojure
Block
```

3400\-4DBF | cjk unified ideographs extension a

### combining\_diacritical\_marks

```clojure
Block
```

300\-36F | combining diacritical marks

### combining\_diacritical\_marks\_for\_symbols

```clojure
Block
```

20D0\-20FF | combining diacritical marks for symbols

### combining\_half\_marks

```clojure
Block
```

FE20\-FE2F | combining half marks

### control\_pictures

```clojure
Block
```

2400\-243F | control pictures

### currency\_symbols

```clojure
Block
```

20A0\-20CF | currency symbols

### cyrillic

```clojure
Block
```

400\-4FF | cyrillic

### cyrillic\_supplementary

```clojure
Block
```

500\-52F | cyrillic supplementary

### devanagari

```clojure
Block
```

900\-97F | devanagari

### dingbats

```clojure
Block
```

2700\-27BF | dingbats

### enclosed\_alphanumerics

```clojure
Block
```

2460\-24FF | enclosed alphanumerics

### enclosed\_cjk\_letters\_and\_months

```clojure
Block
```

3200\-32FF | enclosed cjk letters and months

### end

```clojure
(-> Block library/lux/data/text.Char)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Block)
```

### ethiopic

```clojure
Block
```

1200\-137F | ethiopic

### general\_punctuation

```clojure
Block
```

2000\-206F | general punctuation

### geometric\_shapes

```clojure
Block
```

25A0\-25FF | geometric shapes

### georgian

```clojure
Block
```

10A0\-10FF | georgian

### greek\_and\_coptic

```clojure
Block
```

370\-3FF | greek and coptic

### greek\_extended

```clojure
Block
```

1F00\-1FFF | greek extended

### gujarati

```clojure
Block
```

A80\-AFF | gujarati

### gurmukhi

```clojure
Block
```

A00\-A7F | gurmukhi

### halfwidth\_and\_fullwidth\_forms

```clojure
Block
```

FF00\-FFEF | halfwidth and fullwidth forms

### hangul\_compatibility\_jamo

```clojure
Block
```

3130\-318F | hangul compatibility jamo

### hangul\_jamo

```clojure
Block
```

1100\-11FF | hangul jamo

### hangul\_syllables

```clojure
Block
```

AC00\-D7AF | hangul syllables

### hanunoo

```clojure
Block
```

1720\-173F | hanunoo

### hash

```clojure
(library/lux/abstract/hash.Hash Block)
```

### hebrew

```clojure
Block
```

590\-5FF | hebrew

### high\_private\_use\_surrogates

```clojure
Block
```

DB80\-DBFF | high private use surrogates

### high\_surrogates

```clojure
Block
```

D800\-DB7F | high surrogates

### hiragana

```clojure
Block
```

3040\-309F | hiragana

### ideographic\_description\_characters

```clojure
Block
```

2FF0\-2FFF | ideographic description characters

### ipa\_extensions

```clojure
Block
```

250\-2AF | ipa extensions

### kanbun

```clojure
Block
```

3190\-319F | kanbun

### kangxi\_radicals

```clojure
Block
```

2F00\-2FDF | kangxi radicals

### kannada

```clojure
Block
```

C80\-CFF | kannada

### katakana

```clojure
Block
```

30A0\-30FF | katakana

### katakana\_phonetic\_extensions

```clojure
Block
```

31F0\-31FF | katakana phonetic extensions

### khmer

```clojure
Block
```

1780\-17FF | khmer

### khmer\_symbols

```clojure
Block
```

19E0\-19FF | khmer symbols

### lao

```clojure
Block
```

E80\-EFF | lao

### latin\_1\_supplement

```clojure
Block
```

A0\-FF | latin 1 supplement

### latin\_extended\_a

```clojure
Block
```

100\-17F | latin extended a

### latin\_extended\_additional

```clojure
Block
```

1E00\-1EFF | latin extended additional

### latin\_extended\_b

```clojure
Block
```

180\-24F | latin extended b

### letterlike\_symbols

```clojure
Block
```

2100\-214F | letterlike symbols

### limbu

```clojure
Block
```

1900\-194F | limbu

### low\_surrogates

```clojure
Block
```

DC00\-DFFF | low surrogates

### malayalam

```clojure
Block
```

D00\-D7F | malayalam

### mathematical\_operators

```clojure
Block
```

2200\-22FF | mathematical operators

### miscellaneous\_mathematical\_symbols\_a

```clojure
Block
```

27C0\-27EF | miscellaneous mathematical symbols a

### miscellaneous\_mathematical\_symbols\_b

```clojure
Block
```

2980\-29FF | miscellaneous mathematical symbols b

### miscellaneous\_symbols

```clojure
Block
```

2600\-26FF | miscellaneous symbols

### miscellaneous\_symbols\_and\_arrows

```clojure
Block
```

2B00\-2BFF | miscellaneous symbols and arrows

### miscellaneous\_technical

```clojure
Block
```

2300\-23FF | miscellaneous technical

### mongolian

```clojure
Block
```

1800\-18AF | mongolian

### monoid

```clojure
(library/lux/abstract/monoid.Monoid Block)
```

### myanmar

```clojure
Block
```

1000\-109F | myanmar

### number\_forms

```clojure
Block
```

2150\-218F | number forms

### ogham

```clojure
Block
```

1680\-169F | ogham

### optical\_character\_recognition

```clojure
Block
```

2440\-245F | optical character recognition

### oriya

```clojure
Block
```

B00\-B7F | oriya

### phonetic\_extensions

```clojure
Block
```

1D00\-1D7F | phonetic extensions

### private\_use\_area

```clojure
Block
```

E000\-F8FF | private use area

### runic

```clojure
Block
```

16A0\-16FF | runic

### sinhala

```clojure
Block
```

D80\-DFF | sinhala

### size

```clojure
(-> Block .Nat)
```

### small\_form\_variants

```clojure
Block
```

FE50\-FE6F | small form variants

### spacing\_modifier\_letters

```clojure
Block
```

2B0\-2FF | spacing modifier letters

### specials

```clojure
Block
```

FFF0\-FFFF | specials

### start

```clojure
(-> Block library/lux/data/text.Char)
```

### superscripts\_and\_subscripts

```clojure
Block
```

2070\-209F | superscripts and subscripts

### supplemental\_arrows\_a

```clojure
Block
```

27F0\-27FF | supplemental arrows a

### supplemental\_arrows\_b

```clojure
Block
```

2900\-297F | supplemental arrows b

### supplemental\_mathematical\_operators

```clojure
Block
```

2A00\-2AFF | supplemental mathematical operators

### syriac

```clojure
Block
```

700\-74F | syriac

### tagalog

```clojure
Block
```

1700\-171F | tagalog

### tagbanwa

```clojure
Block
```

1760\-177F | tagbanwa

### tai\_le

```clojure
Block
```

1950\-197F | tai le

### tamil

```clojure
Block
```

B80\-BFF | tamil

### telugu

```clojure
Block
```

C00\-C7F | telugu

### thaana

```clojure
Block
```

780\-7BF | thaana

### thai

```clojure
Block
```

E00\-E7F | thai

### tibetan

```clojure
Block
```

F00\-FFF | tibetan

### unified\_canadian\_aboriginal\_syllabics

```clojure
Block
```

1400\-167F | unified canadian aboriginal syllabics

### variation\_selectors

```clojure
Block
```

FE00\-FE0F | variation selectors

### within?

```clojure
(All (_ _0)
  (-> Block library/lux/data/text.Char .Bit))
```

```clojure
(within? block char)
```

### yi\_radicals

```clojure
Block
```

A490\-A4CF | yi radicals

### yi\_syllables

```clojure
Block
```

A000\-A48F | yi syllables

### yijing\_hexagram\_symbols

```clojure
Block
```

4DC0\-4DFF | yijing hexagram symbols

___

# library/lux/data/text/unicode/set

## Definitions

### Set

```clojure
... .Type
(Primitive "library/lux/data/text/unicode/set.Set")
```

### ascii

```clojure
Set
```

### ascii/alpha

```clojure
Set
```

### ascii/alpha\_num

```clojure
Set
```

### ascii/lower

```clojure
Set
```

### ascii/numeric

```clojure
Set
```

### ascii/upper

```clojure
Set
```

### character

```clojure
Set
```

### composite

```clojure
(-> Set Set Set)
```

### end

```clojure
(-> Set library/lux/data/text.Char)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Set)
```

### full

```clojure
Set
```

### member?

```clojure
(-> Set library/lux/data/text.Char .Bit)
```

```clojure
(member? set character)
```

### non\_character

```clojure
Set
```

### set

```clojure
(-> [library/lux/data/text/unicode/block.Block (.List library/lux/data/text/unicode/block.Block)] Set)
```

```clojure
(set [head tail])
```

### start

```clojure
(-> Set library/lux/data/text.Char)
```

___

# library/lux/debug

## Definitions

### :hole

```clojure
.Macro
```

A typed 'hole'\.
Reveals the type expected of the expression that should go in the hole\.

```clojure
(: (-> Nat Text)
   (function (_ number)
     (:hole)))

... =>

.Text
```

### cannot\_represent\_value

```clojure
(library/lux/control/exception.Exception .Type)
```

### here

```clojure
.Macro
```

Shows the names and values of local bindings available around the call to 'here'\.

```clojure
(let [foo 123
      bar +456
      baz +789]
  (: Any
     (here)))

... =>

... foo: +123

... bar: +456

... baz: +789.0

[]

................................................................
................................................................

... Can optionally be given a list of definitions to focus on.

... These definitions to focus on can include custom format to represent the values.

(let [foo 123
      bar +456
      baz +789]
  (: Any
     (here [foo library/lux/data/text/format.nat]baz)))

... =>

... foo: 123

... baz: +789.0

[]
```

### inspection

```clojure
(library/lux/data/text/format.Format .Any)
```

A best\-effort attempt to generate a textual representation of a value, without knowing its type\.

```clojure
(inspection value)
```

### log\!

```clojure
(-> .Text .Any)
```

Prints/writes a message to standard output\.

```clojure
(log! message)
```

### private

```clojure
.Macro
```

Allows access to un\-exported definitions in other modules\.

```clojure
... Module A

(def: .private (secret_definition input)
  (-> ??? ???)
  (foo (bar (baz input))))

... Module B

((private   secret_definition) my_input)
```

### representation

```clojure
(-> .Type .Any (library/lux/control/try.Try .Text))
```

A best\-effort attempt to generate a textual representation of a value, while knowing its type\.

```clojure
(representation type value)
```

### type\_hole

```clojure
(library/lux/control/exception.Exception [.Location .Type])
```

### unknown\_local\_binding

```clojure
(library/lux/control/exception.Exception .Text)
```

___

# library/lux/documentation

## Definitions

### Definition

```clojure
... .Type
(Record
 [#definition .Text
  #documentation (library/lux/data/format/markdown.Markdown library/lux/data/format/markdown.Block)])
```

### Module

```clojure
... .Type
(Record
 [#module .Text
  #description .Text
  #expected (library/lux/data/collection/set.Set .Text)
  #definitions (.List Definition)])
```

### default

```clojure
.Macro
```

```clojure
(: Definition
   (default documentation/lux/documentation.definition))
```

### documentation

```clojure
(-> (.List Module) .Text)
```

### documentation:

```clojure
.Macro
```

```clojure
(documentation: default
  ""
  [(: Definition
      (default documentation/lux/documentation.definition))])
```

### module

```clojure
.Macro
```

```clojure
(: (.List Module)
   (module _
           ""
           [documentation/lux/documentation.default
            documentation/lux/documentation.documentation:
            documentation/lux/documentation.module
            (default   unqualified_symbol)
            (default   Definition)
            (default   Module)
            (default   documentation)]
           []))
```

### unqualified\_symbol

```clojure
(library/lux/control/exception.Exception .Symbol)
```

___

# library/lux/extension

## Definitions

### analysis:

```clojure
.Macro
```

Mechanism for defining extensions to Lux's analysis/type\-checking infrastructure\.

```clojure
(analysis: ("my analysis" self phase archive [pass_through library/lux/control/parser/code.any])
  (phase archive pass_through))
```

### directive:

```clojure
.Macro
```

```clojure
(directive: ("my directive" self phase archive [parameters (library/lux/control/parser.somelibrary/lux/control/parser/code.any)])
  (do library/lux/tool/compiler/phase.monad
    [.let [_ (library/lux/debug.log!(format "Successfully installed directive " (library/lux/data/text/format.textself) "!"))]]
    (in library/lux/tool/compiler/language/lux/phase/directive.no_requirements)))
```

### generation:

```clojure
.Macro
```

```clojure
(generation: ("my generation" self phase archive [pass_through <synthesis>.any])
  (for [library/lux/target.jvm
        (# library/lux/tool/compiler/phase.monadeach (|>> {library/lux/target/jvm.#Embedded}
                                 library/lux/data/collection/sequence.sequence)
           (phase archive pass_through))]
       (phase archive pass_through)))
```

### synthesis:

```clojure
.Macro
```

Mechanism for defining extensions to Lux's synthesis/optimization infrastructure\.

```clojure
(synthesis: ("my synthesis" self phase archive [pass_through <analysis>.any])
  (phase archive pass_through))
```

___

# library/lux/ffi

## Definitions

### Boolean

```clojure
... .Type
(Primitive "#Bit")
```

### Function

```clojure
... .Type
(Object Function')
```

### Null

```clojure
... .Type
(Object Null')
```

### Number

```clojure
... .Type
(Primitive "#Frac")
```

### \(Object brand\)

```clojure
... .Type
(Primitive "library/lux/ffi.Object" brand)
```

### String

```clojure
... .Type
(Primitive "#Text")
```

### Symbol

```clojure
... .Type
(Object Symbol')
```

### Undefined

```clojure
... .Type
(Object Undefined')
```

### closure

```clojure
.Macro
```

Allows defining closures/anonymous\-functions in the form that JavaScript expects\.
This is useful for adapting Lux functions for usage by JavaScript code\.

```clojure
(: Function
   (closure [left right]
            (do_something (:as Foo left) (:as Bar right))))
```

### constant

```clojure
.Macro
```

Allows using definitions from the JavaScript host platform\.

```clojure
(constant .Frac [Math PI])
```

### import:

```clojure
.Macro
```

Easily import types, methods, functions and constants\.

```clojure
(import: Uint8Array)

(import: TextEncoder
  ["[1]::[0]"
   (new [String])
   (encode [String]   Uint8Array)])

(import: TextDecoder
  ["[1]::[0]"
   (new [String])
   (decode [String]   String)])
```

### null

```clojure
(-> .Any .Nothing)
```

The null pointer\.

### null?

```clojure
(-> .Any .Bit)
```

### on\_browser?

```clojure
.Bit
```

### on\_nashorn?

```clojure
.Bit
```

### on\_node\_js?

```clojure
.Bit
```

### type\_of

```clojure
.Macro
```

The type of an object, as text\.

```clojure
(= "boolean"
   (type_of #1))

................................................................
................................................................

(= "number"
   (type_of +123.456))

................................................................
................................................................

(= "string"
   (type_of "789"))

................................................................
................................................................

(= "function"
   (type_of (function (_ value) value)))
```

___

# library/lux/locale

## Definitions

### Locale

```clojure
... .Type
(Primitive "library/lux/locale.Locale")
```

A description of a locale; with territory, \(optional\) language, and \(optional\) text\-encoding\.

### code

```clojure
(-> Locale .Text)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Locale)
```

### hash

```clojure
(library/lux/abstract/hash.Hash Locale)
```

### locale

```clojure
(-> library/lux/locale/language.Language (.Maybe library/lux/locale/territory.Territory) (.Maybe library/lux/data/text/encoding.Encoding) Locale)
```

```clojure
(locale language territory encoding)
```

___

# library/lux/locale/language

## Definitions

### Language

```clojure
... .Type
(Primitive "library/lux/locale/language.Language")
```

An ISO 639 language\.

### abkhazian

```clojure
Language
```

### achinese

```clojure
Language
```

### acoli

```clojure
Language
```

### adangme

```clojure
Language
```

### adyghe

```clojure
Language
```

### afar

```clojure
Language
```

### afrihili

```clojure
Language
```

### afrikaans

```clojure
Language
```

### afro\_asiatic

```clojure
Language
```

### ainu

```clojure
Language
```

### akan

```clojure
Language
```

### akkadian

```clojure
Language
```

### albanian

```clojure
Language
```

### alemannic

```clojure
Language
```

### aleut

```clojure
Language
```

### algonquian

```clojure
Language
```

### alsatian

```clojure
Language
```

### altaic

```clojure
Language
```

### amharic

```clojure
Language
```

### ancient\_greek

```clojure
Language
```

### angika

```clojure
Language
```

### apache

```clojure
Language
```

### arabic

```clojure
Language
```

### aragonese

```clojure
Language
```

### arapaho

```clojure
Language
```

### arawak

```clojure
Language
```

### armenian

```clojure
Language
```

### aromanian

```clojure
Language
```

### artificial

```clojure
Language
```

### arumanian

```clojure
Language
```

### assamese

```clojure
Language
```

### asturian

```clojure
Language
```

### asturleonese

```clojure
Language
```

### athapascan

```clojure
Language
```

### australian

```clojure
Language
```

### austronesian

```clojure
Language
```

### avaric

```clojure
Language
```

### avestan

```clojure
Language
```

### awadhi

```clojure
Language
```

### aymara

```clojure
Language
```

### azerbaijani

```clojure
Language
```

### bable

```clojure
Language
```

### balinese

```clojure
Language
```

### baltic

```clojure
Language
```

### baluchi

```clojure
Language
```

### bambara

```clojure
Language
```

### bamileke

```clojure
Language
```

### banda

```clojure
Language
```

### bantu

```clojure
Language
```

### basa

```clojure
Language
```

### bashkir

```clojure
Language
```

### basque

```clojure
Language
```

### batak

```clojure
Language
```

### beja

```clojure
Language
```

### belarusian

```clojure
Language
```

### bemba

```clojure
Language
```

### bengali

```clojure
Language
```

### berber

```clojure
Language
```

### bhojpuri

```clojure
Language
```

### bihari

```clojure
Language
```

### bikol

```clojure
Language
```

### bilin

```clojure
Language
```

### bini

```clojure
Language
```

### bislama

```clojure
Language
```

### blin

```clojure
Language
```

### blissymbols

```clojure
Language
```

### bokmal

```clojure
Language
```

### bosnian

```clojure
Language
```

### braj

```clojure
Language
```

### breton

```clojure
Language
```

### buginese

```clojure
Language
```

### bulgarian

```clojure
Language
```

### buriat

```clojure
Language
```

### burmese

```clojure
Language
```

### caddo

```clojure
Language
```

### castilian

```clojure
Language
```

### catalan

```clojure
Language
```

### caucasian

```clojure
Language
```

### cebuano

```clojure
Language
```

### celtic

```clojure
Language
```

### central\_american\_indian

```clojure
Language
```

### central\_khmer

```clojure
Language
```

### chagatai

```clojure
Language
```

### chamic

```clojure
Language
```

### chamorro

```clojure
Language
```

### chechen

```clojure
Language
```

### cherokee

```clojure
Language
```

### chewa

```clojure
Language
```

### cheyenne

```clojure
Language
```

### chibcha

```clojure
Language
```

### chichewa

```clojure
Language
```

### chinese

```clojure
Language
```

### chinook

```clojure
Language
```

### chipewyan

```clojure
Language
```

### choctaw

```clojure
Language
```

### church\_slavic

```clojure
Language
```

### church\_slavonic

```clojure
Language
```

### chuukese

```clojure
Language
```

### chuvash

```clojure
Language
```

### classical\_nepal\_bhasa

```clojure
Language
```

### classical\_newari

```clojure
Language
```

### classical\_syriac

```clojure
Language
```

### code

```clojure
(-> Language .Text)
```

### cook\_islands\_maori

```clojure
Language
```

### coptic

```clojure
Language
```

### cornish

```clojure
Language
```

### corsican

```clojure
Language
```

### cree

```clojure
Language
```

### creek

```clojure
Language
```

### creoles\_and\_pidgins

```clojure
Language
```

### creoles\_and\_pidgins/english

```clojure
Language
```

### creoles\_and\_pidgins/french

```clojure
Language
```

### creoles\_and\_pidgins/portuguese

```clojure
Language
```

### crimean

```clojure
Language
```

### croatian

```clojure
Language
```

### cushitic

```clojure
Language
```

### czech

```clojure
Language
```

### dakota

```clojure
Language
```

### danish

```clojure
Language
```

### dargwa

```clojure
Language
```

### delaware

```clojure
Language
```

### dhivehi

```clojure
Language
```

### dimili

```clojure
Language
```

### dimli

```clojure
Language
```

### dinka

```clojure
Language
```

### dogri

```clojure
Language
```

### dogrib

```clojure
Language
```

### dravidian

```clojure
Language
```

### duala

```clojure
Language
```

### dutch

```clojure
Language
```

### dyula

```clojure
Language
```

### dzongkha

```clojure
Language
```

### eastern\_frisian

```clojure
Language
```

### edo

```clojure
Language
```

### efik

```clojure
Language
```

### egyptian

```clojure
Language
```

### ekajuk

```clojure
Language
```

### elamite

```clojure
Language
```

### english

```clojure
Language
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Language)
```

### erzya

```clojure
Language
```

### esperanto

```clojure
Language
```

### estonian

```clojure
Language
```

### ewe

```clojure
Language
```

### ewondo

```clojure
Language
```

### fang

```clojure
Language
```

### fanti

```clojure
Language
```

### faroese

```clojure
Language
```

### fijian

```clojure
Language
```

### filipino

```clojure
Language
```

### finnish

```clojure
Language
```

### finno\_ugrian

```clojure
Language
```

### flemish

```clojure
Language
```

### fon

```clojure
Language
```

### french

```clojure
Language
```

### friulian

```clojure
Language
```

### fulah

```clojure
Language
```

### ga

```clojure
Language
```

### gaelic

```clojure
Language
```

### galibi\_carib

```clojure
Language
```

### galician

```clojure
Language
```

### ganda

```clojure
Language
```

### gayo

```clojure
Language
```

### gbaya

```clojure
Language
```

### geez

```clojure
Language
```

### georgian

```clojure
Language
```

### german

```clojure
Language
```

### germanic

```clojure
Language
```

### gikuyu

```clojure
Language
```

### gilbertese

```clojure
Language
```

### gondi

```clojure
Language
```

### gorontalo

```clojure
Language
```

### gothic

```clojure
Language
```

### grebo

```clojure
Language
```

### greek

```clojure
Language
```

### greenlandic

```clojure
Language
```

### guarani

```clojure
Language
```

### gujarati

```clojure
Language
```

### gwich'in

```clojure
Language
```

### haida

```clojure
Language
```

### haitian

```clojure
Language
```

### hash

```clojure
(library/lux/abstract/hash.Hash Language)
```

### hausa

```clojure
Language
```

### hawaiian

```clojure
Language
```

### hebrew

```clojure
Language
```

### herero

```clojure
Language
```

### hiligaynon

```clojure
Language
```

### himachali

```clojure
Language
```

### hindi

```clojure
Language
```

### hiri\_motu

```clojure
Language
```

### hittite

```clojure
Language
```

### hmong

```clojure
Language
```

### hungarian

```clojure
Language
```

### hupa

```clojure
Language
```

### iban

```clojure
Language
```

### icelandic

```clojure
Language
```

### ido

```clojure
Language
```

### igbo

```clojure
Language
```

### ijo

```clojure
Language
```

### iloko

```clojure
Language
```

### imperial\_aramaic

```clojure
Language
```

### inari

```clojure
Language
```

### indic

```clojure
Language
```

### indo\_european

```clojure
Language
```

### indonesian

```clojure
Language
```

### ingush

```clojure
Language
```

### interlingua

```clojure
Language
```

### interlingue

```clojure
Language
```

### inuktitut

```clojure
Language
```

### inupiaq

```clojure
Language
```

### iranian

```clojure
Language
```

### irish

```clojure
Language
```

### iroquoian

```clojure
Language
```

### italian

```clojure
Language
```

### japanese

```clojure
Language
```

### javanese

```clojure
Language
```

### jingpho

```clojure
Language
```

### judeo\_arabic

```clojure
Language
```

### judeo\_persian

```clojure
Language
```

### kabardian

```clojure
Language
```

### kabyle

```clojure
Language
```

### kachin

```clojure
Language
```

### kalaallisut

```clojure
Language
```

### kalmyk

```clojure
Language
```

### kamba

```clojure
Language
```

### kannada

```clojure
Language
```

### kanuri

```clojure
Language
```

### kapampangan

```clojure
Language
```

### kara\_kalpak

```clojure
Language
```

### karachay\_balkar

```clojure
Language
```

### karelian

```clojure
Language
```

### karen

```clojure
Language
```

### kashmiri

```clojure
Language
```

### kashubian

```clojure
Language
```

### kawi

```clojure
Language
```

### kazakh

```clojure
Language
```

### khasi

```clojure
Language
```

### khoisan

```clojure
Language
```

### khotanese

```clojure
Language
```

### kimbundu

```clojure
Language
```

### kinyarwanda

```clojure
Language
```

### kirdki

```clojure
Language
```

### kirmanjki

```clojure
Language
```

### klingon

```clojure
Language
```

### komi

```clojure
Language
```

### kongo

```clojure
Language
```

### konkani

```clojure
Language
```

### korean

```clojure
Language
```

### kosraean

```clojure
Language
```

### kpelle

```clojure
Language
```

### kru

```clojure
Language
```

### kumyk

```clojure
Language
```

### kurdish

```clojure
Language
```

### kurukh

```clojure
Language
```

### kutenai

```clojure
Language
```

### kwanyama

```clojure
Language
```

### kyrgyz

```clojure
Language
```

### ladino

```clojure
Language
```

### lahnda

```clojure
Language
```

### lamba

```clojure
Language
```

### land\_dayak

```clojure
Language
```

### lao

```clojure
Language
```

### latin

```clojure
Language
```

### latvian

```clojure
Language
```

### leonese

```clojure
Language
```

### lezghian

```clojure
Language
```

### limburgan

```clojure
Language
```

### lingala

```clojure
Language
```

### lithuanian

```clojure
Language
```

### lojban

```clojure
Language
```

### low\_german

```clojure
Language
```

### lower\_sorbian

```clojure
Language
```

### lozi

```clojure
Language
```

### luba\_katanga

```clojure
Language
```

### luba\_lulua

```clojure
Language
```

### luiseno

```clojure
Language
```

### lule

```clojure
Language
```

### lunda

```clojure
Language
```

### luo

```clojure
Language
```

### lushai

```clojure
Language
```

### luxembourgish

```clojure
Language
```

### macedo\_romanian

```clojure
Language
```

### macedonian

```clojure
Language
```

### madurese

```clojure
Language
```

### magahi

```clojure
Language
```

### maithili

```clojure
Language
```

### makasar

```clojure
Language
```

### malagasy

```clojure
Language
```

### malay

```clojure
Language
```

### malayalam

```clojure
Language
```

### maldivian

```clojure
Language
```

### maltese

```clojure
Language
```

### manchu

```clojure
Language
```

### mandar

```clojure
Language
```

### mandingo

```clojure
Language
```

### manipuri

```clojure
Language
```

### manobo

```clojure
Language
```

### manx

```clojure
Language
```

### maori

```clojure
Language
```

### mapudungun

```clojure
Language
```

### marathi

```clojure
Language
```

### mari

```clojure
Language
```

### marshallese

```clojure
Language
```

### marwari

```clojure
Language
```

### masai

```clojure
Language
```

### mayan

```clojure
Language
```

### mende

```clojure
Language
```

### mi'kmaq

```clojure
Language
```

### micmac

```clojure
Language
```

### middle\_dutch

```clojure
Language
```

### middle\_english

```clojure
Language
```

### middle\_french

```clojure
Language
```

### middle\_high\_german

```clojure
Language
```

### middle\_irish

```clojure
Language
```

### minangkabau

```clojure
Language
```

### mirandese

```clojure
Language
```

### mohawk

```clojure
Language
```

### moksha

```clojure
Language
```

### moldavian

```clojure
Language
```

### moldovan

```clojure
Language
```

### mon\_khmer

```clojure
Language
```

### mongo

```clojure
Language
```

### mongolian

```clojure
Language
```

### montenegrin

```clojure
Language
```

### mossi

```clojure
Language
```

### multiple

```clojure
Language
```

### munda

```clojure
Language
```

### n'ko

```clojure
Language
```

### nahuatl

```clojure
Language
```

### name

```clojure
(-> Language .Text)
```

### nauru

```clojure
Language
```

### navajo

```clojure
Language
```

### ndonga

```clojure
Language
```

### neapolitan

```clojure
Language
```

### nepal\_bhasa

```clojure
Language
```

### nepali

```clojure
Language
```

### newari

```clojure
Language
```

### nias

```clojure
Language
```

### niger\_kordofanian

```clojure
Language
```

### nilo\_saharan

```clojure
Language
```

### niuean

```clojure
Language
```

### nogai

```clojure
Language
```

### north\_american\_indian

```clojure
Language
```

### north\_ndebele

```clojure
Language
```

### northern\_frisian

```clojure
Language
```

### northern\_sami

```clojure
Language
```

### northern\_sotho

```clojure
Language
```

### norwegian

```clojure
Language
```

### not\_applicable

```clojure
Language
```

### nubian

```clojure
Language
```

### nuosu

```clojure
Language
```

### nyamwezi

```clojure
Language
```

### nyanja

```clojure
Language
```

### nyankole

```clojure
Language
```

### nynorsk

```clojure
Language
```

### nyoro

```clojure
Language
```

### nzima

```clojure
Language
```

### occitan

```clojure
Language
```

### official\_aramaic

```clojure
Language
```

### oirat

```clojure
Language
```

### ojibwa

```clojure
Language
```

### old\_bulgarian

```clojure
Language
```

### old\_church\_slavonic

```clojure
Language
```

### old\_english

```clojure
Language
```

### old\_french

```clojure
Language
```

### old\_high\_german

```clojure
Language
```

### old\_irish

```clojure
Language
```

### old\_newari

```clojure
Language
```

### old\_norse

```clojure
Language
```

### old\_persian

```clojure
Language
```

### old\_provencal

```clojure
Language
```

### old\_slavonic

```clojure
Language
```

### oriya

```clojure
Language
```

### oromo

```clojure
Language
```

### osage

```clojure
Language
```

### ossetic

```clojure
Language
```

### otomian

```clojure
Language
```

### ottoman\_turkish

```clojure
Language
```

### pahlavi

```clojure
Language
```

### palauan

```clojure
Language
```

### pali

```clojure
Language
```

### pampanga

```clojure
Language
```

### pangasinan

```clojure
Language
```

### papiamento

```clojure
Language
```

### papuan

```clojure
Language
```

### pashto

```clojure
Language
```

### pedi

```clojure
Language
```

### persian

```clojure
Language
```

### philippine

```clojure
Language
```

### phoenician

```clojure
Language
```

### pohnpeian

```clojure
Language
```

### polish

```clojure
Language
```

### portuguese

```clojure
Language
```

### prakrit

```clojure
Language
```

### provencal

```clojure
Language
```

### punjabi

```clojure
Language
```

### quechua

```clojure
Language
```

### rajasthani

```clojure
Language
```

### rapanui

```clojure
Language
```

### rarotongan

```clojure
Language
```

### romance

```clojure
Language
```

### romanian

```clojure
Language
```

### romansh

```clojure
Language
```

### romany

```clojure
Language
```

### rundi

```clojure
Language
```

### russian

```clojure
Language
```

### sakan

```clojure
Language
```

### salishan

```clojure
Language
```

### samaritan\_aramaic

```clojure
Language
```

### sami

```clojure
Language
```

### samoan

```clojure
Language
```

### sandawe

```clojure
Language
```

### sango

```clojure
Language
```

### sanskrit

```clojure
Language
```

### santali

```clojure
Language
```

### sardinian

```clojure
Language
```

### sasak

```clojure
Language
```

### scots

```clojure
Language
```

### selkup

```clojure
Language
```

### semitic

```clojure
Language
```

### sepedi

```clojure
Language
```

### serbian

```clojure
Language
```

### serer

```clojure
Language
```

### shan

```clojure
Language
```

### shona

```clojure
Language
```

### sichuan\_yi

```clojure
Language
```

### sicilian

```clojure
Language
```

### sidamo

```clojure
Language
```

### sign

```clojure
Language
```

### siksika

```clojure
Language
```

### sindhi

```clojure
Language
```

### sinhalese

```clojure
Language
```

### sino\_tibetan

```clojure
Language
```

### siouan

```clojure
Language
```

### skolt\_sami

```clojure
Language
```

### slavey

```clojure
Language
```

### slavic

```clojure
Language
```

### slovak

```clojure
Language
```

### slovenian

```clojure
Language
```

### sogdian

```clojure
Language
```

### somali

```clojure
Language
```

### songhai

```clojure
Language
```

### soninke

```clojure
Language
```

### sorbian

```clojure
Language
```

### south\_american\_indian

```clojure
Language
```

### south\_ndebele

```clojure
Language
```

### southern\_altai

```clojure
Language
```

### southern\_sami

```clojure
Language
```

### southern\_sotho

```clojure
Language
```

### spanish

```clojure
Language
```

### sranan\_tongo

```clojure
Language
```

### standard\_moroccan\_tamazight

```clojure
Language
```

### sukuma

```clojure
Language
```

### sumerian

```clojure
Language
```

### sundanese

```clojure
Language
```

### susu

```clojure
Language
```

### swahili

```clojure
Language
```

### swati

```clojure
Language
```

### swedish

```clojure
Language
```

### swiss\_german

```clojure
Language
```

### syriac

```clojure
Language
```

### tagalog

```clojure
Language
```

### tahitian

```clojure
Language
```

### tai

```clojure
Language
```

### tajik

```clojure
Language
```

### tamashek

```clojure
Language
```

### tamil

```clojure
Language
```

### tatar

```clojure
Language
```

### telugu

```clojure
Language
```

### tereno

```clojure
Language
```

### tetum

```clojure
Language
```

### thai

```clojure
Language
```

### tibetan

```clojure
Language
```

### tigre

```clojure
Language
```

### tigrinya

```clojure
Language
```

### timne

```clojure
Language
```

### tiv

```clojure
Language
```

### tlingit

```clojure
Language
```

### tok\_pisin

```clojure
Language
```

### tokelau

```clojure
Language
```

### tonga

```clojure
Language
```

### tongan

```clojure
Language
```

### tsimshian

```clojure
Language
```

### tsonga

```clojure
Language
```

### tswana

```clojure
Language
```

### tumbuka

```clojure
Language
```

### tupi

```clojure
Language
```

### turkish

```clojure
Language
```

### turkmen

```clojure
Language
```

### tuvalu

```clojure
Language
```

### tuvinian

```clojure
Language
```

### twi

```clojure
Language
```

### udmurt

```clojure
Language
```

### ugaritic

```clojure
Language
```

### ukrainian

```clojure
Language
```

### umbundu

```clojure
Language
```

### uncoded

```clojure
Language
```

### undetermined

```clojure
Language
```

### upper\_sorbian

```clojure
Language
```

### urdu

```clojure
Language
```

### uyghur

```clojure
Language
```

### uzbek

```clojure
Language
```

### vai

```clojure
Language
```

### valencian

```clojure
Language
```

### venda

```clojure
Language
```

### vietnamese

```clojure
Language
```

### volapük

```clojure
Language
```

### votic

```clojure
Language
```

### wakashan

```clojure
Language
```

### walamo

```clojure
Language
```

### walloon

```clojure
Language
```

### waray

```clojure
Language
```

### washo

```clojure
Language
```

### welsh

```clojure
Language
```

### western\_frisian

```clojure
Language
```

### wolof

```clojure
Language
```

### xhosa

```clojure
Language
```

### yakut

```clojure
Language
```

### yao

```clojure
Language
```

### yapese

```clojure
Language
```

### yiddish

```clojure
Language
```

### yoruba

```clojure
Language
```

### yupik

```clojure
Language
```

### zande

```clojure
Language
```

### zapotec

```clojure
Language
```

### zaza

```clojure
Language
```

### zazaki

```clojure
Language
```

### zenaga

```clojure
Language
```

### zhuang

```clojure
Language
```

### zulu

```clojure
Language
```

### zuni

```clojure
Language
```

___

# library/lux/locale/territory

## Definitions

### Territory

```clojure
... .Type
(Primitive "library/lux/locale/territory.Territory")
```

An ISO 3166 territory\.

### afghanistan

```clojure
Territory
```

### aland\_islands

```clojure
Territory
```

### albania

```clojure
Territory
```

### algeria

```clojure
Territory
```

### american\_samoa

```clojure
Territory
```

### andorra

```clojure
Territory
```

### angola

```clojure
Territory
```

### anguilla

```clojure
Territory
```

### antarctica

```clojure
Territory
```

### antigua

```clojure
Territory
```

### argentina

```clojure
Territory
```

### armenia

```clojure
Territory
```

### aruba

```clojure
Territory
```

### ascension

```clojure
Territory
```

### australia

```clojure
Territory
```

### austria

```clojure
Territory
```

### azerbaijan

```clojure
Territory
```

### bahrain

```clojure
Territory
```

### bangladesh

```clojure
Territory
```

### barbados

```clojure
Territory
```

### barbuda

```clojure
Territory
```

### belarus

```clojure
Territory
```

### belgium

```clojure
Territory
```

### belize

```clojure
Territory
```

### benin

```clojure
Territory
```

### bermuda

```clojure
Territory
```

### bhutan

```clojure
Territory
```

### bolivia

```clojure
Territory
```

### bonaire

```clojure
Territory
```

### bosnia

```clojure
Territory
```

### botswana

```clojure
Territory
```

### bouvet\_island

```clojure
Territory
```

### brazil

```clojure
Territory
```

### british\_indian\_ocean\_territory

```clojure
Territory
```

### british\_virgin\_islands

```clojure
Territory
```

### brunei\_darussalam

```clojure
Territory
```

### bulgaria

```clojure
Territory
```

### burkina\_faso

```clojure
Territory
```

### burundi

```clojure
Territory
```

### caicos\_islands

```clojure
Territory
```

### cambodia

```clojure
Territory
```

### cameroon

```clojure
Territory
```

### canada

```clojure
Territory
```

### cape\_verde

```clojure
Territory
```

### cayman\_islands

```clojure
Territory
```

### central\_african\_republic

```clojure
Territory
```

### chad

```clojure
Territory
```

### chile

```clojure
Territory
```

### china

```clojure
Territory
```

### christmas\_island

```clojure
Territory
```

### cocos\_islands

```clojure
Territory
```

### colombia

```clojure
Territory
```

### comoros

```clojure
Territory
```

### congo

```clojure
Territory
```

### cook\_islands

```clojure
Territory
```

### costa\_rica

```clojure
Territory
```

### croatia

```clojure
Territory
```

### cuba

```clojure
Territory
```

### curacao

```clojure
Territory
```

### cyprus

```clojure
Territory
```

### czech\_republic

```clojure
Territory
```

### democratic\_republic\_of\_the\_congo

```clojure
Territory
```

### denmark

```clojure
Territory
```

### djibouti

```clojure
Territory
```

### dominica

```clojure
Territory
```

### dominican\_republic

```clojure
Territory
```

### east\_timor

```clojure
Territory
```

### ecuador

```clojure
Territory
```

### egypt

```clojure
Territory
```

### el\_salvador

```clojure
Territory
```

### equatorial\_guinea

```clojure
Territory
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Territory)
```

### eritrea

```clojure
Territory
```

### estonia

```clojure
Territory
```

### eswatini

```clojure
Territory
```

### ethiopia

```clojure
Territory
```

### falkland\_islands

```clojure
Territory
```

### faroe\_islands

```clojure
Territory
```

### fiji

```clojure
Territory
```

### finland

```clojure
Territory
```

### france

```clojure
Territory
```

### french\_guiana

```clojure
Territory
```

### french\_polynesia

```clojure
Territory
```

### french\_southern\_territories

```clojure
Territory
```

### futuna

```clojure
Territory
```

### gabon

```clojure
Territory
```

### georgia

```clojure
Territory
```

### germany

```clojure
Territory
```

### ghana

```clojure
Territory
```

### gibraltar

```clojure
Territory
```

### greece

```clojure
Territory
```

### greenland

```clojure
Territory
```

### grenada

```clojure
Territory
```

### guadeloupe

```clojure
Territory
```

### guam

```clojure
Territory
```

### guatemala

```clojure
Territory
```

### guernsey

```clojure
Territory
```

### guinea

```clojure
Territory
```

### guinea\_bissau

```clojure
Territory
```

### guyana

```clojure
Territory
```

### haiti

```clojure
Territory
```

### hash

```clojure
(library/lux/abstract/hash.Hash Territory)
```

### heard\_island

```clojure
Territory
```

### herzegovina

```clojure
Territory
```

### honduras

```clojure
Territory
```

### hong\_kong

```clojure
Territory
```

### hungary

```clojure
Territory
```

### iceland

```clojure
Territory
```

### india

```clojure
Territory
```

### indonesia

```clojure
Territory
```

### iran

```clojure
Territory
```

### iraq

```clojure
Territory
```

### ireland

```clojure
Territory
```

### isle\_of\_man

```clojure
Territory
```

### israel

```clojure
Territory
```

### italy

```clojure
Territory
```

### ivory\_coast

```clojure
Territory
```

### jamaica

```clojure
Territory
```

### jan\_mayen

```clojure
Territory
```

### japan

```clojure
Territory
```

### jersey

```clojure
Territory
```

### jordan

```clojure
Territory
```

### kazakhstan

```clojure
Territory
```

### kenya

```clojure
Territory
```

### kiribati

```clojure
Territory
```

### kuwait

```clojure
Territory
```

### kyrgyzstan

```clojure
Territory
```

### laos

```clojure
Territory
```

### latvia

```clojure
Territory
```

### lebanon

```clojure
Territory
```

### lesotho

```clojure
Territory
```

### liberia

```clojure
Territory
```

### libya

```clojure
Territory
```

### liechtenstein

```clojure
Territory
```

### lithuania

```clojure
Territory
```

### long\_code

```clojure
(-> Territory .Text)
```

### luxembourg

```clojure
Territory
```

### macau

```clojure
Territory
```

### macedonia

```clojure
Territory
```

### madagascar

```clojure
Territory
```

### malawi

```clojure
Territory
```

### malaysia

```clojure
Territory
```

### maldives

```clojure
Territory
```

### mali

```clojure
Territory
```

### malta

```clojure
Territory
```

### marshall\_islands

```clojure
Territory
```

### martinique

```clojure
Territory
```

### mauritania

```clojure
Territory
```

### mauritius

```clojure
Territory
```

### mayotte

```clojure
Territory
```

### mcdonald\_islands

```clojure
Territory
```

### mexico

```clojure
Territory
```

### micronesia

```clojure
Territory
```

### miquelon

```clojure
Territory
```

### moldova

```clojure
Territory
```

### monaco

```clojure
Territory
```

### mongolia

```clojure
Territory
```

### montenegro

```clojure
Territory
```

### montserrat

```clojure
Territory
```

### morocco

```clojure
Territory
```

### mozambique

```clojure
Territory
```

### myanmar

```clojure
Territory
```

### name

```clojure
(-> Territory .Text)
```

### namibia

```clojure
Territory
```

### nauru

```clojure
Territory
```

### nepal

```clojure
Territory
```

### netherlands

```clojure
Territory
```

### nevis

```clojure
Territory
```

### new\_caledonia

```clojure
Territory
```

### new\_zealand

```clojure
Territory
```

### nicaragua

```clojure
Territory
```

### niger

```clojure
Territory
```

### nigeria

```clojure
Territory
```

### niue

```clojure
Territory
```

### norfolk\_island

```clojure
Territory
```

### north\_korea

```clojure
Territory
```

### northern\_ireland

```clojure
Territory
```

### northern\_mariana\_islands

```clojure
Territory
```

### norway

```clojure
Territory
```

### numeric\_code

```clojure
(-> Territory .Nat)
```

### oman

```clojure
Territory
```

### pakistan

```clojure
Territory
```

### palau

```clojure
Territory
```

### palestine

```clojure
Territory
```

### panama

```clojure
Territory
```

### papua\_new\_guinea

```clojure
Territory
```

### paraguay

```clojure
Territory
```

### peru

```clojure
Territory
```

### philippines

```clojure
Territory
```

### pitcairn\_islands

```clojure
Territory
```

### poland

```clojure
Territory
```

### portugal

```clojure
Territory
```

### principe

```clojure
Territory
```

### puerto\_rico

```clojure
Territory
```

### qatar

```clojure
Territory
```

### reunion

```clojure
Territory
```

### romania

```clojure
Territory
```

### russia

```clojure
Territory
```

### rwanda

```clojure
Territory
```

### saba

```clojure
Territory
```

### saint\_barthelemy

```clojure
Territory
```

### saint\_helena

```clojure
Territory
```

### saint\_kitts

```clojure
Territory
```

### saint\_lucia

```clojure
Territory
```

### saint\_martin

```clojure
Territory
```

### saint\_pierre

```clojure
Territory
```

### saint\_vincent

```clojure
Territory
```

### samoa

```clojure
Territory
```

### san\_marino

```clojure
Territory
```

### sao\_tome

```clojure
Territory
```

### saudi\_arabia

```clojure
Territory
```

### senegal

```clojure
Territory
```

### serbia

```clojure
Territory
```

### seychelles

```clojure
Territory
```

### short\_code

```clojure
(-> Territory .Text)
```

### sierra\_leone

```clojure
Territory
```

### singapore

```clojure
Territory
```

### sint\_eustatius

```clojure
Territory
```

### sint\_maarten

```clojure
Territory
```

### slovakia

```clojure
Territory
```

### slovenia

```clojure
Territory
```

### solomon\_islands

```clojure
Territory
```

### somalia

```clojure
Territory
```

### south\_africa

```clojure
Territory
```

### south\_georgia

```clojure
Territory
```

### south\_korea

```clojure
Territory
```

### south\_sandwich\_islands

```clojure
Territory
```

### south\_sudan

```clojure
Territory
```

### spain

```clojure
Territory
```

### sri\_lanka

```clojure
Territory
```

### sudan

```clojure
Territory
```

### suriname

```clojure
Territory
```

### svalbard

```clojure
Territory
```

### sweden

```clojure
Territory
```

### switzerland

```clojure
Territory
```

### syria

```clojure
Territory
```

### taiwan

```clojure
Territory
```

### tajikistan

```clojure
Territory
```

### tanzania

```clojure
Territory
```

### thailand

```clojure
Territory
```

### the\_bahamas

```clojure
Territory
```

### the\_gambia

```clojure
Territory
```

### the\_grenadines

```clojure
Territory
```

### tobago

```clojure
Territory
```

### togo

```clojure
Territory
```

### tokelau

```clojure
Territory
```

### tonga

```clojure
Territory
```

### trinidad

```clojure
Territory
```

### tristan\_da\_cunha

```clojure
Territory
```

### tunisia

```clojure
Territory
```

### turkey

```clojure
Territory
```

### turkmenistan

```clojure
Territory
```

### turks

```clojure
Territory
```

### tuvalu

```clojure
Territory
```

### uganda

```clojure
Territory
```

### ukraine

```clojure
Territory
```

### united\_arab\_emirates

```clojure
Territory
```

### united\_kingdom

```clojure
Territory
```

### united\_states\_minor\_outlying\_islands

```clojure
Territory
```

### united\_states\_of\_america

```clojure
Territory
```

### united\_states\_virgin\_islands

```clojure
Territory
```

### uruguay

```clojure
Territory
```

### uzbekistan

```clojure
Territory
```

### vanuatu

```clojure
Territory
```

### vatican\_city

```clojure
Territory
```

### venezuela

```clojure
Territory
```

### vietnam

```clojure
Territory
```

### wallis

```clojure
Territory
```

### western\_sahara

```clojure
Territory
```

### yemen

```clojure
Territory
```

### zambia

```clojure
Territory
```

### zimbabwe

```clojure
Territory
```

___

# library/lux/macro

## Definitions

### expansion

```clojure
(-> .Code (.Meta (.List .Code)))
```

Given code that requires applying a macro, expands repeatedly until no more direct macro\-calls are left\.
Otherwise, returns the code as\-is\.

```clojure
(expansion syntax)
```

### full\_expansion

```clojure
(-> .Code (.Meta (.List .Code)))
```

Expands all macro\-calls everywhere recursively, until only primitive/base code remains\.

```clojure
(full_expansion syntax)
```

### log\_expansion\!

```clojure
.Macro
```

Performs a macro\-expansion and logs the resulting code\.
You can either use the resulting code, or omit them\.
By omitting them, this macro produces nothing \(just like the lux\.comment macro\)\.

```clojure
(log_expansion!
 (def: (foo bar baz)
   (-> Int Int Int)
   (int.+ bar baz)))

(log_expansion! "omit"
                (def: (foo bar baz)
                  (-> Int Int Int)
                  (int.+ bar baz)))
```

### log\_full\_expansion\!

```clojure
.Macro
```

Performs a macro\-expansion and logs the resulting code\.
You can either use the resulting code, or omit them\.
By omitting them, this macro produces nothing \(just like the lux\.comment macro\)\.

```clojure
(log_full_expansion!
 (def: (foo bar baz)
   (-> Int Int Int)
   (int.+ bar baz)))

(log_full_expansion! "omit"
                     (def: (foo bar baz)
                       (-> Int Int Int)
                       (int.+ bar baz)))
```

### log\_single\_expansion\!

```clojure
.Macro
```

Performs a macro\-expansion and logs the resulting code\.
You can either use the resulting code, or omit them\.
By omitting them, this macro produces nothing \(just like the lux\.comment macro\)\.

```clojure
(log_single_expansion!
 (def: (foo bar baz)
   (-> Int Int Int)
   (int.+ bar baz)))

(log_single_expansion! "omit"
                       (def: (foo bar baz)
                         (-> Int Int Int)
                         (int.+ bar baz)))
```

### one\_expansion

```clojure
(-> .Code (.Meta .Code))
```

Works just like expand, except that it ensures that the output is a single Code token\.

```clojure
(one_expansion token)
```

### single\_expansion

```clojure
(-> .Code (.Meta (.List .Code)))
```

Given code that requires applying a macro, does it once and returns the result\.
Otherwise, returns the code as\-is\.

```clojure
(single_expansion syntax)
```

### symbol

```clojure
(-> .Text (.Meta .Code))
```

Generates a unique name as a Code node \(ready to be used in code templates\)\.
A prefix can be given \(or just be empty text\) to better identify the code for debugging purposes\.

```clojure
(symbol prefix)
```

### with\_symbols

```clojure
.Macro
```

Creates new symbols and offers them to the body expression\.

```clojure
(syntax: (synchronized [lock any
                        body any])
  (with_symbols [g!lock g!body g!_]
    (in (list (` (let [(~ g!lock) (~ lock)
                       (~ g!_) ("jvm monitorenter" (~ g!lock))
                       (~ g!body) (~ body)
                       (~ g!_) ("jvm monitorexit" (~ g!lock))]
                   (~ g!body)))))))
```

### wrong\_syntax\_error

```clojure
(-> .Symbol .Text)
```

A generic error message for macro syntax failures\.

___

# library/lux/macro/code

## Definitions

### bit

```clojure
(-> .Bit .Code)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Code)
```

### form

```clojure
(-> (.List .Code) .Code)
```

### format

```clojure
(-> .Code .Text)
```

### frac

```clojure
(-> .Frac .Code)
```

### int

```clojure
(-> .Int .Code)
```

### local\_symbol

```clojure
(-> .Text .Code)
```

Produces a local symbol \(an symbol with no module prefix\)\.

### nat

```clojure
(-> .Nat .Code)
```

### replaced

```clojure
(-> .Code .Code .Code .Code)
```

```clojure
(replaced original substitute ast)
```

### rev

```clojure
(-> .Rev .Code)
```

### symbol

```clojure
(-> .Symbol .Code)
```

### text

```clojure
(-> .Text .Code)
```

### tuple

```clojure
(-> (.List .Code) .Code)
```

### variant

```clojure
(-> (.List .Code) .Code)
```

___

# library/lux/macro/local

## Definitions

### cannot\_shadow\_definition

```clojure
(library/lux/control/exception.Exception [.Text .Text])
```

### push

```clojure
(-> (.List [.Symbol .Macro]) (.Meta .Code))
```

Installs macros in the compiler\-state, with the given names\.
Yields code that can be placed either as expression or as directives\.
This code un\-installs the macros\.
NOTE: Always use this code once to clean\-up\.\.

```clojure
(push macros)
```

### unknown\_definition

```clojure
(library/lux/control/exception.Exception [.Text .Text])
```

### unknown\_module

```clojure
(library/lux/control/exception.Exception .Text)
```

___

# library/lux/macro/syntax

## Definitions

### syntax:

```clojure
.Macro
```


A more advanced way to define macros than 'macro:'\.
The inputs to the macro can be parsed in complex ways through the use of syntax parsers\.
The macro body is also \(implicitly\) run in the Meta monad, to save some typing\.
Also, the compiler state can be accessed through the \*lux\* binding\.

```clojure
(syntax: .public (object [.let [imports (class_imports *lux*)]
                          .let [class_vars (list)]
                          super (opt (super_class_decl^ imports class_vars))
                          interfaces (tuple (some (super_class_decl^ imports class_vars)))
                          constructor_args (constructor_args^ imports class_vars)
                          methods (some (overriden_method_def^ imports))])
  (let [def_code ($_ text#composite "anon-class:"
                     (spaced (list (super_class_decl$ (maybe.else object_super_class super))
                                   (with_brackets (spaced (list#each super_class_decl$ interfaces)))
                                   (with_brackets (spaced (list#each constructor_arg$ constructor_args)))
                                   (with_brackets (spaced (list#each (method_def$ id) methods))))))]
    (in (list (` ((~ (code.text def_code))))))))
```

___

# library/lux/macro/syntax/check

## Definitions

### Check

```clojure
... .Type
(Record
 [#type .Code
  #value .Code])
```

A type annotation for an expression\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Check)
```

### format

```clojure
(-> Check .Code)
```

### parser

```clojure
(library/lux/control/parser/code.Parser Check)
```

___

# library/lux/macro/syntax/declaration

## Definitions

### Declaration

```clojure
... .Type
(Record
 [#name .Text
  #arguments (.List .Text)])
```

A declaration for either a constant or a function\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Declaration)
```

### format

```clojure
(-> Declaration .Code)
```

### parser

```clojure
(library/lux/control/parser/code.Parser Declaration)
```

A parser for declaration syntax\.

```clojure
... Such as:

quux

(foo bar baz)
```

___

# library/lux/macro/syntax/definition

## Definitions

### Definition

```clojure
... .Type
(Record
 [#name .Text
  #value (.Either library/lux/macro/syntax/check.Check .Code)
  #export? .Bit])
```

Syntax for a constant definition\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Definition)
```

### format

```clojure
(-> Definition .Code)
```

### lacks\_type

```clojure
(library/lux/control/exception.Exception Definition)
```

### parser

```clojure
(-> .Lux (library/lux/control/parser/code.Parser Definition))
```

A reader that first macro\-expands and then analyses the input Code, to ensure it is a definition\.

```clojure
(parser compiler)
```

### typed

```clojure
(-> .Lux (library/lux/control/parser/code.Parser Definition))
```

Only works for typed definitions\.

```clojure
(typed compiler)
```

___

# library/lux/macro/syntax/export

Syntax for marking a definition as an export\.

## Definitions

### default\_policy

```clojure
.Code
```

### parser

```clojure
(All (_ _0)
  (-> (library/lux/control/parser/code.Parser _0) (library/lux/control/parser/code.Parser [.Code _0])))
```

```clojure
(parser un_exported)
```

___

# library/lux/macro/syntax/input

## Definitions

### Input

```clojure
... .Type
(Record
 [#binding .Code
  #type .Code])
```

The common typed\-argument syntax used by many macros\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Input)
```

### format

```clojure
(-> (.List Input) .Code)
```

### parser

```clojure
(library/lux/control/parser/code.Parser (.List Input))
```

Parser for the common typed\-argument syntax used by many macros\.

___

# library/lux/macro/syntax/type/variable

## Definitions

### Variable

```clojure
... .Type
(Primitive "#Text")
```

A variable's name\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Variable)
```

### format

```clojure
(-> Variable .Code)
```

### parser

```clojure
(library/lux/control/parser/code.Parser Variable)
```

Parser for the common type variable/parameter used by many macros\.

___

# library/lux/macro/template

Utilities commonly used while templating\.

## Definitions

### amount

```clojure
.Macro
```

```clojure
(amount [a b c d])

... =>

4
```

### irregular\_arguments

```clojure
(library/lux/control/exception.Exception [.Nat .Nat])
```

### let

```clojure
.Macro
```

Lexically\-bound templates\.

```clojure
(let [(!square <root>)
      [(* <root> <root>)]]
  (def: (square root)
    (-> Nat Nat)
    (!square root)))
```

### spliced

```clojure
.Macro
```

```clojure
(spliced [a b c d])

... =>

a

b

c

d
```

### symbol

```clojure
.Macro
```

An symbol made by concatenating pieces of code\.
The \(optional\) module part and the short part are specified independently\.

```clojure
(symbol ["abc" .def documentation/lux/macro/template.ghi])

... =>

abcdefghi

................................................................
................................................................

(symbol [.def] ["abc" .def documentation/lux/macro/template.ghi])

... =>

.abcdefghi
```

### text

```clojure
.Macro
```

A text literal made by concatenating pieces of code\.

```clojure
(text [#0 123 +456 +789   "abc" .def documentation/lux/macro/template.ghi])

... =>

... #0123+456+789.0abcdefghi
```

### with\_locals

```clojure
.Macro
```

Creates names for local bindings aliased by the names you choose\.

```clojure
(with_locals [my_var]
  (let [my_var 123]
    (text [my_var])))

... =>

... __gensym__my_var506
```

___

# library/lux/math

Common mathematical constants and functions\.

## Definitions

### acos

```clojure
(-> .Frac .Frac)
```

### acosh

```clojure
(-> .Frac .Frac)
```

### acoth

```clojure
(-> .Frac .Frac)
```

### acsch

```clojure
(-> .Frac .Frac)
```

### asech

```clojure
(-> .Frac .Frac)
```

### asin

```clojure
(-> .Frac .Frac)
```

### asinh

```clojure
(-> .Frac .Frac)
```

### atan

```clojure
(-> .Frac .Frac)
```

### atan/2

```clojure
(-> .Frac .Frac .Frac)
```

```clojure
(atan/2 x y)
```

### atanh

```clojure
(-> .Frac .Frac)
```

### ceil

```clojure
(-> .Frac .Frac)
```

### cos

```clojure
(-> .Frac .Frac)
```

### cosh

```clojure
(-> .Frac .Frac)
```

### coth

```clojure
(-> .Frac .Frac)
```

### csch

```clojure
(-> .Frac .Frac)
```

### e

```clojure
.Frac
```

The base of the natural logarithm\.

### exp

```clojure
(-> .Frac .Frac)
```

### factorial

```clojure
(-> .Nat .Nat)
```

### floor

```clojure
(-> .Frac .Frac)
```

### hypotenuse

```clojure
(-> .Frac .Frac .Frac)
```

### log

```clojure
(-> .Frac .Frac)
```

### log\_by

```clojure
(-> .Frac .Frac .Frac)
```

```clojure
(log_by base it)
```

### pi

```clojure
.Frac
```

The ratio of a circle's circumference to its diameter\.

### pow

```clojure
(-> .Frac .Frac .Frac)
```

```clojure
(pow param subject)
```

### root/2

```clojure
(-> .Frac .Frac)
```

### root/3

```clojure
(-> .Frac .Frac)
```

### round

```clojure
(-> .Frac .Frac)
```

### sech

```clojure
(-> .Frac .Frac)
```

### sin

```clojure
(-> .Frac .Frac)
```

### sinh

```clojure
(-> .Frac .Frac)
```

### tan

```clojure
(-> .Frac .Frac)
```

### tanh

```clojure
(-> .Frac .Frac)
```

### tau

```clojure
.Frac
```

The ratio of a circle's circumference to its radius\.

___

# library/lux/math/infix

## Definitions

### infix

```clojure
.Macro
```

Infix math syntax\.
The rules for infix syntax are simple\.
If you want your binary function to work well with it\.
Then take the argument to the right \(y\) as your first argument,
and take the argument to the left \(x\) as your second argument\.

```clojure
... Binary functions

(infix [x * +10])

... =>

(* +10 x)

................................................................
................................................................

... Nested infix

(infix [[x + y] * [x - y]])

... =>

(* (- y x) (+ y x))

................................................................
................................................................

... Unary functions

(infix [sin [x + y]])

... =>

(sin (+ y x))

................................................................
................................................................

... Also works with logic

(infix [[x < y] and [y < z]])

... =>

(and (< z y)
     (< y x))

................................................................
................................................................

... Forms are left as-is

(infix [(* 3 9) gcd 450])

... =>

(gcd 450 (* 3 9))
```

___

# library/lux/math/logic/continuous

Continuous logic using Rev values\.
Continuous logic is logic in the interval \[0,1\] instead of just the binary \#0 and \#1 options\.
Because Rev is being used, the interval is actual \[0,1\)\.

## Definitions

### =

```clojure
(-> .Rev .Rev .Rev)
```

### and

```clojure
(-> .Rev .Rev .Rev)
```

### conjunction

```clojure
(library/lux/abstract/monoid.Monoid .Rev)
```

### disjunction

```clojure
(library/lux/abstract/monoid.Monoid .Rev)
```

### false

```clojure
.Rev
```

### implies

```clojure
(-> .Rev .Rev .Rev)
```

```clojure
(implies consequent antecedent)
```

### not

```clojure
(-> .Rev .Rev)
```

### or

```clojure
(-> .Rev .Rev .Rev)
```

### true

```clojure
.Rev
```

___

# library/lux/math/logic/fuzzy

Fuzzy logic, implemented on top of the Rev type\.

## Definitions

### \(Fuzzy it\)

```clojure
... .Type
(-> it .Rev)
```

A fuzzy set\.

### complement

```clojure
(All (_ _0)
  (-> (Fuzzy _0) (Fuzzy _0)))
```

### cut

```clojure
(All (_ _0)
  (-> .Rev (Fuzzy _0) (Fuzzy _0)))
```

```clojure
(cut treshold set)
```

### difference

```clojure
(All (_ _0)
  (-> (Fuzzy _0) (Fuzzy _0) (Fuzzy _0)))
```

```clojure
(difference sub base)
```

### empty

```clojure
Fuzzy
```

### full

```clojure
Fuzzy
```

### functor

```clojure
(library/lux/abstract/functor/contravariant.Functor Fuzzy)
```

### gradient

```clojure
(-> .Rev .Rev (Fuzzy .Rev))
```

```clojure
(gradient from to)
```

### intersection

```clojure
(All (_ _0)
  (-> (Fuzzy _0) (Fuzzy _0) (Fuzzy _0)))
```

### membership

```clojure
(All (_ _0)
  (-> (Fuzzy _0) _0 .Rev))
```

```clojure
(membership set elem)
```

### of\_predicate

```clojure
(All (_ _0)
  (-> (library/lux/abstract/predicate.Predicate _0) (Fuzzy _0)))
```

```clojure
(of_predicate predicate)
```

### of\_set

```clojure
(All (_ _0)
  (-> (library/lux/data/collection/set.Set _0) (Fuzzy _0)))
```

### predicate

```clojure
(All (_ _0)
  (-> .Rev (Fuzzy _0) (library/lux/abstract/predicate.Predicate _0)))
```

```clojure
(predicate treshold set)
```

### trapezoid

```clojure
(-> .Rev .Rev .Rev .Rev (Fuzzy .Rev))
```

```clojure
(trapezoid bottom middle_bottom middle_top top)
```

### triangle

```clojure
(-> .Rev .Rev .Rev (Fuzzy .Rev))
```

```clojure
(triangle bottom middle top)
```

### union

```clojure
(All (_ _0)
  (-> (Fuzzy _0) (Fuzzy _0) (Fuzzy _0)))
```

___

# library/lux/math/modular

## Definitions

### \*

```clojure
(All (_ _0)
  (-> (Mod _0) (Mod _0) (Mod _0)))
```

### \+

```clojure
(All (_ _0)
  (-> (Mod _0) (Mod _0) (Mod _0)))
```

### \-

```clojure
(All (_ _0)
  (-> (Mod _0) (Mod _0) (Mod _0)))
```

### <

```clojure
(All (_ _0)
  (-> (Mod _0) (Mod _0) .Bit))
```

### <=

```clojure
(All (_ _0)
  (-> (Mod _0) (Mod _0) .Bit))
```

### =

```clojure
(All (_ _0)
  (-> (Mod _0) (Mod _0) .Bit))
```

### >

```clojure
(All (_ _0)
  (-> (Mod _0) (Mod _0) .Bit))
```

### >=

```clojure
(All (_ _0)
  (-> (Mod _0) (Mod _0) .Bit))
```

### \(Mod %\)

```clojure
... .Type
(Primitive "library/lux/math/modular.Mod" %)
```

A number under a modulus\.

### adapter

```clojure
(All (_ _0 _1)
  (-> (library/lux/math/modulus.Modulus _0) (library/lux/math/modulus.Modulus _1) (library/lux/control/try.Try (-> (Mod _1) (Mod _0)))))
```

```clojure
(adapter reference subject)
```

### addition

```clojure
(All (_ _0)
  (-> (library/lux/math/modulus.Modulus _0) (library/lux/abstract/monoid.Monoid (Mod _0))))
```

### codec

```clojure
(All (_ _0)
  (-> (library/lux/math/modulus.Modulus _0) (library/lux/abstract/codec.Codec .Text (Mod _0))))
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Mod _0)))
```

### incorrect\_modulus

```clojure
(All (_ _0)
  (library/lux/control/exception.Exception [(library/lux/math/modulus.Modulus _0) .Int]))
```

### inverse

```clojure
(All (_ _0)
  (-> (Mod _0) (.Maybe (Mod _0))))
```

### modular

```clojure
(All (_ _0)
  (-> (library/lux/math/modulus.Modulus _0) .Int (Mod _0)))
```

```clojure
(modular modulus value)
```

### moduli\_are\_not\_equal

```clojure
(All (_ _0 _1)
  (library/lux/control/exception.Exception [(library/lux/math/modulus.Modulus _0) (library/lux/math/modulus.Modulus _1)]))
```

### modulus

```clojure
(All (_ _0)
  (-> (Mod _0) (library/lux/math/modulus.Modulus _0)))
```

### multiplication

```clojure
(All (_ _0)
  (-> (library/lux/math/modulus.Modulus _0) (library/lux/abstract/monoid.Monoid (Mod _0))))
```

### order

```clojure
(All (_ _0)
  (library/lux/abstract/order.Order (Mod _0)))
```

### value

```clojure
(All (_ _0)
  (-> (Mod _0) .Int))
```

___

# library/lux/math/modulus

## Definitions

### =

```clojure
(All (_ _0 _1)
  (-> (Modulus _0) (Modulus _1) .Bit))
```

### \(Modulus %\)

```clojure
... .Type
(Primitive "library/lux/math/modulus.Modulus" %)
```

A number used as a modulus in modular arithmetic\.
It cannot be 0\.

### congruent?

```clojure
(All (_ _0)
  (-> (Modulus _0) .Int .Int .Bit))
```

```clojure
(congruent? modulus reference subject)
```

### divisor

```clojure
(All (_ _0)
  (-> (Modulus _0) .Int))
```

### literal

```clojure
.Macro
```

```clojure
... Success!

(literal 123)

................................................................
................................................................

... Failure!

(literal 0)
```

### modulus

```clojure
(Ex (_ _0)
  (-> .Int (library/lux/control/try.Try (Modulus _0))))
```

### zero\_cannot\_be\_a\_modulus

```clojure
(library/lux/control/exception.Exception .Any)
```

___

# library/lux/math/number

## Definitions

### bin

```clojure
.Macro
```

Given syntax for a binary number, generates a Nat, an Int, a Rev or a Frac\.

```clojure
(bin "11001001")

................................................................
................................................................

... Allows for the presence of commas (,) among the digits.

(bin "11,00,10,01")
```

### hex

```clojure
.Macro
```

Given syntax for a hexadecimal number, generates a Nat, an Int, a Rev or a Frac\.

```clojure
(hex "deadBEEF")

................................................................
................................................................

... Allows for the presence of commas (,) among the digits.

(hex "dead,BEEF")
```

### oct

```clojure
.Macro
```

Given syntax for a octal number, generates a Nat, an Int, a Rev or a Frac\.

```clojure
(oct "615243")

................................................................
................................................................

... Allows for the presence of commas (,) among the digits.

(oct "615,243")
```

___

# library/lux/math/number/complex

Complex arithmetic\.

## Definitions

### %

```clojure
(-> Complex Complex Complex)
```

### \*

```clojure
(-> Complex Complex Complex)
```

### \*'

```clojure
(-> .Frac Complex Complex)
```

### \+

```clojure
(-> Complex Complex Complex)
```

### \+one

```clojure
Complex
```

### \-

```clojure
(-> Complex Complex Complex)
```

### \-one

```clojure
Complex
```

### /

```clojure
(-> Complex Complex Complex)
```

### /'

```clojure
(-> .Frac Complex Complex)
```

### =

```clojure
(-> Complex Complex .Bit)
```

### Complex

```clojure
... .Type
(Record
 [#real .Frac
  #imaginary .Frac])
```

A complex number\.

### abs

```clojure
(-> Complex .Frac)
```

### acos

```clojure
(-> Complex Complex)
```

### approximately?

```clojure
(-> .Frac Complex Complex .Bit)
```

### argument

```clojure
(-> Complex .Frac)
```

### asin

```clojure
(-> Complex Complex)
```

### atan

```clojure
(-> Complex Complex)
```

### complex

```clojure
.Macro
```

Complex literals\.

```clojure
(complex real imaginary)

................................................................
................................................................

... The imaginary part can be omitted if it's +0.0.

(complex real)
```

### conjugate

```clojure
(-> Complex Complex)
```

### cos

```clojure
(-> Complex Complex)
```

### cosh

```clojure
(-> Complex Complex)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Complex)
```

### exp

```clojure
(-> Complex Complex)
```

### i

```clojure
Complex
```

### log

```clojure
(-> Complex Complex)
```

### not\_a\_number?

```clojure
(-> Complex .Bit)
```

### opposite

```clojure
(-> Complex Complex)
```

### pow

```clojure
(-> Complex Complex Complex)
```

### pow'

```clojure
(-> .Frac Complex Complex)
```

### reciprocal

```clojure
(-> Complex Complex)
```

### root/2

```clojure
(-> Complex Complex)
```

### roots

```clojure
(-> .Nat Complex (.List Complex))
```

### signum

```clojure
(-> Complex Complex)
```

### sin

```clojure
(-> Complex Complex)
```

### sinh

```clojure
(-> Complex Complex)
```

### tan

```clojure
(-> Complex Complex)
```

### tanh

```clojure
(-> Complex Complex)
```

### zero

```clojure
Complex
```

___

# library/lux/math/number/frac

## Definitions

### %

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) remainder\.

### \*

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) multiplication\.

### \+

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) addition\.

### \-

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) substraction\.

### /

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) division\.

### /%

```clojure
(-> .Frac .Frac [.Frac .Frac])
```

```clojure
(/% param subject)
```

### <

```clojure
(-> .Frac .Frac .Bit)
```

Frac\(tion\) less\-than\.

```clojure
(< reference sample)
```

### <=

```clojure
(-> .Frac .Frac .Bit)
```

Frac\(tion\) less\-than or equal\.

```clojure
(<= reference sample)
```

### =

```clojure
(-> .Frac .Frac .Bit)
```

Frac\(tion\) equivalence\.

```clojure
(= reference sample)
```

### >

```clojure
(-> .Frac .Frac .Bit)
```

Frac\(tion\) greater\-than\.

```clojure
(> reference sample)
```

### >=

```clojure
(-> .Frac .Frac .Bit)
```

Frac\(tion\) greater\-than or equal\.

```clojure
(>= reference sample)
```

### abs

```clojure
(-> .Frac .Frac)
```

### addition

```clojure
(library/lux/abstract/monoid.Monoid .Frac)
```

### approximately?

```clojure
(-> .Frac .Frac .Frac .Bit)
```

```clojure
(approximately? margin_of_error standard value)
```

### biggest

```clojure
.Frac
```

### binary

```clojure
(library/lux/abstract/codec.Codec .Text .Frac)
```

### bits

```clojure
(-> .Frac .I64)
```

### decimal

```clojure
(library/lux/abstract/codec.Codec .Text .Frac)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Frac)
```

### hash

```clojure
(library/lux/abstract/hash.Hash .Frac)
```

### hex

```clojure
(library/lux/abstract/codec.Codec .Text .Frac)
```

### int

```clojure
(-> .Frac .Int)
```

### max

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) minimum\.

### maximum

```clojure
(library/lux/abstract/monoid.Monoid .Frac)
```

### min

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) minimum\.

### minimum

```clojure
(library/lux/abstract/monoid.Monoid .Frac)
```

### mod

```clojure
(All (_ _0)
  (-> .Frac .Frac .Frac))
```

```clojure
(mod divisor dividend)
```

### multiplication

```clojure
(library/lux/abstract/monoid.Monoid .Frac)
```

### nat

```clojure
(-> .Frac .Nat)
```

### negative?

```clojure
(library/lux/abstract/predicate.Predicate .Frac)
```

### negative\_infinity

```clojure
.Frac
```

Negative infinity\.

### not\_a\_number

```clojure
.Frac
```

Not a number\.

### not\_a\_number?

```clojure
(-> .Frac .Bit)
```

Tests whether a frac is actually not\-a\-number\.

```clojure
(not_a_number? it)
```

### number?

```clojure
(-> .Frac .Bit)
```

### octal

```clojure
(library/lux/abstract/codec.Codec .Text .Frac)
```

### of\_bits

```clojure
(-> .I64 .Frac)
```

### opposite

```clojure
(-> .Frac .Frac)
```

### order

```clojure
(library/lux/abstract/order.Order .Frac)
```

### positive?

```clojure
(library/lux/abstract/predicate.Predicate .Frac)
```

### positive\_infinity

```clojure
.Frac
```

Positive infinity\.

### rev

```clojure
(-> .Frac .Rev)
```

### signum

```clojure
(-> .Frac .Frac)
```

### smallest

```clojure
.Frac
```

### zero?

```clojure
(library/lux/abstract/predicate.Predicate .Frac)
```

___

# library/lux/math/number/i16

## Definitions

### I16

```clojure
... .Type
(.I64 (Primitive "{New Type @"library/lux/math/number/i16",13,17 0}"))
```

A 16\-bit integer\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence (.I64 (Primitive "{New Type @"library/lux/math/number/i16",13,17 0}")))
```

### i16

```clojure
(-> .I64 (.I64 (Primitive "{New Type @"library/lux/math/number/i16",13,17 0}")))
```

### i64

```clojure
(-> (.I64 (Primitive "{New Type @"library/lux/math/number/i16",13,17 0}")) .I64)
```

### width

```clojure
.Nat
```

___

# library/lux/math/number/i32

## Definitions

### I32

```clojure
... .Type
(.I64 (Primitive "{New Type @"library/lux/math/number/i32",13,17 0}"))
```

A 32\-bit integer\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence (.I64 (Primitive "{New Type @"library/lux/math/number/i32",13,17 0}")))
```

### i32

```clojure
(-> .I64 (.I64 (Primitive "{New Type @"library/lux/math/number/i32",13,17 0}")))
```

### i64

```clojure
(-> (.I64 (Primitive "{New Type @"library/lux/math/number/i32",13,17 0}")) .I64)
```

### width

```clojure
.Nat
```

___

# library/lux/math/number/i64

## Definitions

### Mask

```clojure
... .Type
(All (Mask _0)
  (Primitive "#I64" _0))
```

A pattern of bits that can be imposed on I64 values\.

### \(Sub width\)

```clojure
... .Type
(Record
 [&equivalence (library/lux/abstract/equivalence.Equivalence (.I64 width))
  bits .Nat
  narrow (-> .I64 (.I64 width))
  wide (-> (.I64 width) .I64)])
```

A sub\-space of I64 with a reduce amount of bits\.

### and

```clojure
(All (_ _0)
  (-> (.I64 .Any) (.I64 _0) (.I64 _0)))
```

Bitwise and\.

### bit

```clojure
(-> .Nat Mask)
```

A mask with only a specific bit set\.

```clojure
(bit position)
```

### bits\_per\_byte

```clojure
.Nat
```

### bytes\_per\_i64

```clojure
.Nat
```

### conjunction

```clojure
(All (_ _0)
  (library/lux/abstract/monoid.Monoid (.I64 _0)))
```

### disjunction

```clojure
(All (_ _0)
  (library/lux/abstract/monoid.Monoid (.I64 _0)))
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (.I64 _0)))
```

### false

```clojure
Mask
```

### flipped

```clojure
(All (_ _0)
  (-> .Nat (.I64 _0) (.I64 _0)))
```

Flip bit at given index\.

### hash

```clojure
(All (_ _0)
  (library/lux/abstract/hash.Hash (.I64 _0)))
```

### left\_rotated

```clojure
(All (_ _0)
  (-> .Nat (.I64 _0) (.I64 _0)))
```

### left\_shifted

```clojure
(All (_ _0)
  (-> .Nat (.I64 _0) (.I64 _0)))
```

Bitwise left\-shift\.

### mask

```clojure
(-> .Nat Mask)
```

Mask a block of bits of the specified size\.

```clojure
(mask amount_of_bits)
```

### not

```clojure
(All (_ _0)
  (-> (.I64 _0) (.I64 _0)))
```

Bitwise negation\.

### one

```clojure
(All (_ _0)
  (-> .Nat (.I64 _0) (.I64 _0)))
```

Set bit at given index\.

### one?

```clojure
(-> .Nat (.I64 .Any) .Bit)
```

```clojure
(one? index input)
```

### ones

```clojure
(-> (.I64 .Any) .Nat)
```

Count the number of 1s in a bit\-map\.

```clojure
(ones it)
```

### or

```clojure
(All (_ _0)
  (-> (.I64 .Any) (.I64 _0) (.I64 _0)))
```

Bitwise or\.

### region

```clojure
(-> .Nat .Nat Mask)
```

A mask for a block of bits of the given size, starting at the given offset\.

```clojure
(region offset size)
```

### reversed

```clojure
(All (_ _0)
  (-> (.I64 _0) (.I64 _0)))
```

### right\_rotated

```clojure
(All (_ _0)
  (-> .Nat (.I64 _0) (.I64 _0)))
```

### right\_shifted

```clojure
(All (_ _0)
  (-> .Nat (.I64 _0) (.I64 _0)))
```

Unsigned/logic bitwise right\-shift\.

### sign

```clojure
Mask
```

A mask for the sign bit of ints\.

### sub

```clojure
(Ex (_ _0)
  (-> .Nat (.Maybe (Sub _0))))
```

Given a width in the interval \(0,64\), yields an implementation for integers of that width\.

```clojure
(sub width)
```

### true

```clojure
Mask
```

### width

```clojure
.Nat
```

### xor

```clojure
(All (_ _0)
  (-> (.I64 .Any) (.I64 _0) (.I64 _0)))
```

Bitwise xor\.

### zero

```clojure
(All (_ _0)
  (-> .Nat (.I64 _0) (.I64 _0)))
```

Clear bit at the given index\.

### zero?

```clojure
(-> .Nat (.I64 .Any) .Bit)
```

```clojure
(zero? index input)
```

___

# library/lux/math/number/i8

## Definitions

### I8

```clojure
... .Type
(.I64 (Primitive "{New Type @"library/lux/math/number/i8",13,17 0}"))
```

A 8\-bit integer\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence (.I64 (Primitive "{New Type @"library/lux/math/number/i8",13,17 0}")))
```

### i64

```clojure
(-> (.I64 (Primitive "{New Type @"library/lux/math/number/i8",13,17 0}")) .I64)
```

### i8

```clojure
(-> .I64 (.I64 (Primitive "{New Type @"library/lux/math/number/i8",13,17 0}")))
```

### width

```clojure
.Nat
```

___

# library/lux/math/number/int

## Definitions

### %

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) remainder\.

### \*

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) multiplication\.

### \+

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) addition\.

### \-

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) substraction\.

### /

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) division\.

### /%

```clojure
(-> .Int .Int [.Int .Int])
```

Int\(eger\) \[division remainder\]\.

### <

```clojure
(-> .Int .Int .Bit)
```

Int\(eger\) less\-than\.

```clojure
(< reference sample)
```

### <=

```clojure
(-> .Int .Int .Bit)
```

Int\(eger\) less\-than or equal\.

```clojure
(<= reference sample)
```

### =

```clojure
(-> .Int .Int .Bit)
```

Int\(eger\) equivalence\.

```clojure
(= reference sample)
```

### >

```clojure
(-> .Int .Int .Bit)
```

Int\(eger\) greater\-than\.

```clojure
(> reference sample)
```

### >=

```clojure
(-> .Int .Int .Bit)
```

Int\(eger\) greater\-than or equal\.

```clojure
(>= reference sample)
```

### abs

```clojure
(-> .Int .Int)
```

A value of equal magnitude and positive sign\.

### addition

```clojure
(library/lux/abstract/monoid.Monoid .Int)
```

### binary

```clojure
(library/lux/abstract/codec.Codec .Text .Int)
```

### co\_prime?

```clojure
(-> .Int .Int .Bit)
```

### decimal

```clojure
(library/lux/abstract/codec.Codec .Text .Int)
```

### enum

```clojure
(library/lux/abstract/enum.Enum .Int)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Int)
```

### even?

```clojure
(-> .Int .Bit)
```

### extended\_gcd

```clojure
(-> .Int .Int [[.Int .Int] .Int])
```

Extended euclidean algorithm\.

### frac

```clojure
(-> .Int .Frac)
```

### gcd

```clojure
(-> .Int .Int .Int)
```

Greatest Common Divisor\.

### hash

```clojure
(library/lux/abstract/hash.Hash .Int)
```

### hex

```clojure
(library/lux/abstract/codec.Codec .Text .Int)
```

### interval

```clojure
(library/lux/abstract/interval.Interval .Int)
```

### lcm

```clojure
(-> .Int .Int .Int)
```

Least Common Multiple\.

### max

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) maximum\.

### maximum

```clojure
(library/lux/abstract/monoid.Monoid .Int)
```

### min

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) minimum\.

### minimum

```clojure
(library/lux/abstract/monoid.Monoid .Int)
```

### mod

```clojure
(All (_ _0)
  (-> .Int .Int .Int))
```

Integer modulo\.
Note: The modulo and the remainder are not the same\.

```clojure
(mod divisor dividend)
```

### multiplication

```clojure
(library/lux/abstract/monoid.Monoid .Int)
```

### negative?

```clojure
(library/lux/abstract/predicate.Predicate .Int)
```

### octal

```clojure
(library/lux/abstract/codec.Codec .Text .Int)
```

### odd?

```clojure
(-> .Int .Bit)
```

### opposite

```clojure
(-> .Int .Int)
```

A value of equal magnitude and opposite sign\.

### order

```clojure
(library/lux/abstract/order.Order .Int)
```

### positive?

```clojure
(library/lux/abstract/predicate.Predicate .Int)
```

### right\_shifted

```clojure
(-> .Nat .Int .Int)
```

Signed/arithmetic bitwise right\-shift\.

```clojure
(right_shifted parameter subject)
```

### signum

```clojure
(-> .Int .Int)
```

A value \(either \-1, 0 or \+0\) which represents the sign\.

### zero?

```clojure
(library/lux/abstract/predicate.Predicate .Int)
```

___

# library/lux/math/number/nat

## Definitions

### %

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) remainder\.

### \*

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) multiplication\.

### \+

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) addition\.

### \-

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) substraction\.

### /

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) division\.

### /%

```clojure
(-> .Nat .Nat [.Nat .Nat])
```

Nat\(ural\) \[division remainder\]\.

### <

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) less\-than\.

### <=

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) less\-than or equal\.

### =

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) equivalence\.

### >

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) greater\-than\.

### >=

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) greater\-than or equal\.

### addition

```clojure
(library/lux/abstract/monoid.Monoid .Nat)
```

### binary

```clojure
(library/lux/abstract/codec.Codec .Text .Nat)
```

### co\_prime?

```clojure
(-> .Nat .Nat .Bit)
```

### decimal

```clojure
(library/lux/abstract/codec.Codec .Text .Nat)
```

### enum

```clojure
(library/lux/abstract/enum.Enum .Nat)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Nat)
```

### even?

```clojure
(-> .Nat .Bit)
```

### frac

```clojure
(-> .Nat .Frac)
```

### gcd

```clojure
(-> .Nat .Nat .Nat)
```

Greatest Common Divisor\.

### hash

```clojure
(library/lux/abstract/hash.Hash .Nat)
```

### hex

```clojure
(library/lux/abstract/codec.Codec .Text .Nat)
```

### interval

```clojure
(library/lux/abstract/interval.Interval .Nat)
```

### lcm

```clojure
(-> .Nat .Nat .Nat)
```

Least Common Multiple\.

### max

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) maximum\.

### maximum

```clojure
(library/lux/abstract/monoid.Monoid .Nat)
```

### min

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) minimum\.

### minimum

```clojure
(library/lux/abstract/monoid.Monoid .Nat)
```

### multiplication

```clojure
(library/lux/abstract/monoid.Monoid .Nat)
```

### octal

```clojure
(library/lux/abstract/codec.Codec .Text .Nat)
```

### odd?

```clojure
(-> .Nat .Bit)
```

### order

```clojure
(library/lux/abstract/order.Order .Nat)
```

___

# library/lux/math/number/ratio

Rational numbers\.

## Definitions

### %

```clojure
(-> Ratio Ratio Ratio)
```

### \*

```clojure
(-> Ratio Ratio Ratio)
```

### \+

```clojure
(-> Ratio Ratio Ratio)
```

### \-

```clojure
(-> Ratio Ratio Ratio)
```

### /

```clojure
(-> Ratio Ratio Ratio)
```

### <

```clojure
(-> Ratio Ratio .Bit)
```

### <=

```clojure
(-> Ratio Ratio .Bit)
```

### =

```clojure
(-> Ratio Ratio .Bit)
```

### >

```clojure
(-> Ratio Ratio .Bit)
```

### >=

```clojure
(-> Ratio Ratio .Bit)
```

### Ratio

```clojure
... .Type
(Record
 [#numerator .Nat
  #denominator .Nat])
```

An unsigned ratio of numbers\.

### addition

```clojure
(library/lux/abstract/monoid.Monoid Ratio)
```

### codec

```clojure
(library/lux/abstract/codec.Codec .Text Ratio)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Ratio)
```

### multiplication

```clojure
(library/lux/abstract/monoid.Monoid Ratio)
```

### nat

```clojure
(-> Ratio (.Maybe .Nat))
```

### order

```clojure
(library/lux/abstract/order.Order Ratio)
```

### ratio

```clojure
.Macro
```

Rational literals\.

```clojure
(ratio numerator denominator)

................................................................
................................................................

... The denominator can be omitted if it is 1.

(ratio numerator)
```

### reciprocal

```clojure
(-> Ratio Ratio)
```

___

# library/lux/math/number/rev

## Definitions

### %

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) remainder\.

### \*

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) multiplication\.

### \+

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) addition\.

### \-

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) substraction\.

### /

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) division\.

### /%

```clojure
(-> .Rev .Rev [.Rev .Rev])
```

### /1

```clojure
.Rev
```

### /1024

```clojure
.Rev
```

### /128

```clojure
.Rev
```

### /16

```clojure
.Rev
```

### /2

```clojure
.Rev
```

### /2048

```clojure
.Rev
```

### /256

```clojure
.Rev
```

### /32

```clojure
.Rev
```

### /4

```clojure
.Rev
```

### /4096

```clojure
.Rev
```

### /512

```clojure
.Rev
```

### /64

```clojure
.Rev
```

### /8

```clojure
.Rev
```

### <

```clojure
(-> .Rev .Rev .Bit)
```

Rev\(olution\) less\-than\.

```clojure
(< reference sample)
```

### <=

```clojure
(-> .Rev .Rev .Bit)
```

Rev\(olution\) less\-than or equal\.

```clojure
(<= reference sample)
```

### =

```clojure
(-> .Rev .Rev .Bit)
```

Rev\(olution\) equivalence\.

```clojure
(= reference sample)
```

### >

```clojure
(-> .Rev .Rev .Bit)
```

Rev\(olution\) greater\-than\.

```clojure
(> reference sample)
```

### >=

```clojure
(-> .Rev .Rev .Bit)
```

Rev\(olution\) greater\-than or equal\.

```clojure
(>= reference sample)
```

### addition

```clojure
(library/lux/abstract/monoid.Monoid .Rev)
```

### binary

```clojure
(library/lux/abstract/codec.Codec .Text .Rev)
```

### decimal

```clojure
(library/lux/abstract/codec.Codec .Text .Rev)
```

### down

```clojure
(-> .Nat .Rev .Rev)
```

```clojure
(down scale subject)
```

### enum

```clojure
(library/lux/abstract/enum.Enum .Rev)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Rev)
```

### frac

```clojure
(-> .Rev .Frac)
```

### hash

```clojure
(library/lux/abstract/hash.Hash .Rev)
```

### hex

```clojure
(library/lux/abstract/codec.Codec .Text .Rev)
```

### interval

```clojure
(library/lux/abstract/interval.Interval .Rev)
```

### max

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) maximum\.

### maximum

```clojure
(library/lux/abstract/monoid.Monoid .Rev)
```

### min

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) minimum\.

### minimum

```clojure
(library/lux/abstract/monoid.Monoid .Rev)
```

### octal

```clojure
(library/lux/abstract/codec.Codec .Text .Rev)
```

### order

```clojure
(library/lux/abstract/order.Order .Rev)
```

### ratio

```clojure
(-> .Rev .Rev .Nat)
```

Ratio between two rev\(olution\)s\.

### reciprocal

```clojure
(-> .Nat .Rev)
```

Rev\(olution\) reciprocal of a Nat\(ural\)\.

```clojure
(reciprocal numerator)
```

### up

```clojure
(-> .Nat .Rev .Rev)
```

```clojure
(up scale subject)
```

___

# library/lux/math/random

Pseudo\-random number generation \(PRNG\) algorithms\.

## Definitions

### PRNG

```clojure
... .Type
(Rec PRNG
 (-> .Any [PRNG .I64]))
```

An abstract way to represent any PRNG\.

### \(Random it\)

```clojure
... .Type
(-> PRNG [PRNG it])
```

A producer of random values based on a PRNG\.

### and

```clojure
(All (_ _0 _1)
  (-> (Random _0) (Random _1) (Random [_0 _1])))
```

Sequencing combinator\.

```clojure
(and left right)
```

### apply

```clojure
(library/lux/abstract/apply.Apply Random)
```

### array

```clojure
(All (_ _0)
  (-> .Nat (Random _0) (Random (library/lux/data/collection/array.Array _0))))
```

### ascii

```clojure
(-> .Nat (Random .Text))
```

### ascii/alpha

```clojure
(-> .Nat (Random .Text))
```

### ascii/alpha\_num

```clojure
(-> .Nat (Random .Text))
```

### ascii/lower

```clojure
(-> .Nat (Random .Text))
```

### ascii/numeric

```clojure
(-> .Nat (Random .Text))
```

### ascii/upper

```clojure
(-> .Nat (Random .Text))
```

### bit

```clojure
(Random .Bit)
```

### char

```clojure
(-> library/lux/data/text/unicode/set.Set (Random library/lux/data/text.Char))
```

### complex

```clojure
(Random library/lux/math/number/complex.Complex)
```

### date

```clojure
(Random library/lux/time/date.Date)
```

### day

```clojure
(Random library/lux/time/day.Day)
```

### dictionary

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/hash.Hash _0) .Nat (Random _0) (Random _1) (Random (library/lux/data/collection/dictionary.Dictionary _0 _1))))
```

```clojure
(dictionary hash size key_gen value_gen)
```

### duration

```clojure
(Random library/lux/time/duration.Duration)
```

### either

```clojure
(All (_ _0)
  (-> (Random _0) (Random _0) (Random _0)))
```

Homogeneous alternative combinator\.

```clojure
(either left right)
```

### frac

```clojure
(Random .Frac)
```

### functor

```clojure
(library/lux/abstract/functor.Functor Random)
```

### i64

```clojure
(Random .I64)
```

### instant

```clojure
(Random library/lux/time/instant.Instant)
```

### int

```clojure
(Random .Int)
```

### list

```clojure
(All (_ _0)
  (-> .Nat (Random _0) (Random (.List _0))))
```

### maybe

```clojure
(All (_ _0)
  (-> (Random _0) (Random (.Maybe _0))))
```

### monad

```clojure
(library/lux/abstract/monad.Monad Random)
```

### month

```clojure
(Random library/lux/time/month.Month)
```

### nat

```clojure
(Random .Nat)
```

### one

```clojure
(All (_ _0 _1)
  (-> (-> _0 (.Maybe _1)) (Random _0) (Random _1)))
```

```clojure
(one check random)
```

### only

```clojure
(All (_ _0)
  (-> (-> _0 .Bit) (Random _0) (Random _0)))
```

Retries the generator until the output satisfies a predicate\.

```clojure
(only pred gen)
```

### or

```clojure
(All (_ _0 _1)
  (-> (Random _0) (Random _1) (Random (Or _0 _1))))
```

Heterogeneous alternative combinator\.

```clojure
(or left right)
```

### pcg\_32

```clojure
(-> [(.I64 .Any) (.I64 .Any)] PRNG)
```

An implementation of the PCG32 algorithm\.
For more information, please see: http://www\.pcg\-random\.org/

### prng

```clojure
(All (_ _0)
  (-> (-> _0 _0) (-> _0 .I64) _0 PRNG))
```

```clojure
(prng update return)
```

### queue

```clojure
(All (_ _0)
  (-> .Nat (Random _0) (Random (library/lux/data/collection/queue.Queue _0))))
```

### ratio

```clojure
(Random library/lux/math/number/ratio.Ratio)
```

### rec

```clojure
(All (_ _0)
  (-> (-> (Random _0) (Random _0)) (Random _0)))
```

A combinator for producing recursive random generators\.

```clojure
(rec gen)
```

### refined

```clojure
(All (_ _0 _1)
  (-> (library/lux/type/refinement.Refiner _0 _1) (Random _0) (Random (library/lux/type/refinement.Refined _0 _1))))
```

Retries the generator until the output can be refined\.

```clojure
(refined refiner gen)
```

### result

```clojure
(All (_ _0)
  (-> PRNG (Random _0) [PRNG _0]))
```

```clojure
(result prng calc)
```

### rev

```clojure
(Random .Rev)
```

### safe\_frac

```clojure
(Random .Frac)
```

A number in the interval \[0\.0,1\.0\]\.

### sequence

```clojure
(All (_ _0)
  (-> .Nat (Random _0) (Random (library/lux/data/collection/sequence.Sequence _0))))
```

### set

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) .Nat (Random _0) (Random (library/lux/data/collection/set.Set _0))))
```

```clojure
(set hash size value_gen)
```

### split\_mix\_64

```clojure
(-> .Nat PRNG)
```

An implementation of the SplitMix64 algorithm\.

### stack

```clojure
(All (_ _0)
  (-> .Nat (Random _0) (Random (library/lux/data/collection/stack.Stack _0))))
```

### text

```clojure
(-> (Random library/lux/data/text.Char) .Nat (Random .Text))
```

```clojure
(text char_gen size)
```

### time

```clojure
(Random library/lux/time.Time)
```

### unicode

```clojure
(-> .Nat (Random .Text))
```

### xoroshiro\_128\+

```clojure
(-> [(.I64 .Any) (.I64 .Any)] PRNG)
```

An implementation of the Xoroshiro128\+ algorithm\.
For more information, please see: http://xoroshiro\.di\.unimi\.it/

___

# library/lux/meta

Functions for extracting information from the state of the compiler\.

## Definitions

### apply

```clojure
(library/lux/abstract/apply.Apply .Meta)
```

### assertion

```clojure
(-> .Text .Bit (.Meta .Any))
```

Fails with the given message if the test is \#0\.

```clojure
(assertion message test)
```

### compiler\_state

```clojure
(.Meta .Lux)
```

Obtains the current state of the compiler\.

### current\_module

```clojure
(.Meta .Module)
```

The module currently being compiled, if any\.

### current\_module\_name

```clojure
(.Meta .Text)
```

The name of the module currently being compiled, if any\.

### de\_aliased

```clojure
(-> .Symbol (.Meta .Symbol))
```

Given an aliased definition's name, returns the original definition being referenced\.

```clojure
(de_aliased def_name)
```

### definition

```clojure
(-> .Symbol (.Meta .Global))
```

Looks\-up a definition's whole data in the available modules \(including the current one\)\.

```clojure
(definition name)
```

### definition\_type

```clojure
(-> .Symbol (.Meta .Type))
```

Looks\-up a definition's type in the available modules \(including the current one\)\.

```clojure
(definition_type name)
```

### definitions

```clojure
(-> .Text (.Meta (.List [.Text .Definition])))
```

The entire list of definitions in a module \(including the non\-exported/private ones\)\.

```clojure
(definitions module)
```

### either

```clojure
(All (_ _0)
  (-> (.Meta _0) (.Meta _0) (.Meta _0)))
```

Pick whichever computation succeeds\.

```clojure
(either left right)
```

### eval

```clojure
(-> .Type .Code (.Meta .Any))
```

```clojure
(eval type code)
```

### expected\_type

```clojure
(.Meta .Type)
```

The expected type of the current expression being analyzed\.

### export

```clojure
(-> .Symbol (.Meta .Definition))
```

Looks\-up a definition in the available modules \(including the current one\)\.
The look\-up only succeeds if the definition has been exported\.

```clojure
(export name)
```

### exports

```clojure
(-> .Text (.Meta (.List [.Text .Definition])))
```

All the exported definitions in a module\.

```clojure
(exports module_name)
```

### failure

```clojure
(All (_ _0)
  (-> .Text (.Meta _0)))
```

Fails with the given error message\.

```clojure
(failure error)
```

### functor

```clojure
(library/lux/abstract/functor.Functor .Meta)
```

### globals

```clojure
(-> .Text (.Meta (.List [.Text .Global])))
```

The entire list of globals in a module \(including the non\-exported/private ones\)\.

```clojure
(globals module)
```

### imported?

```clojure
(-> .Text (.Meta .Bit))
```

Checks if the given module has been imported by the current module\.

```clojure
(imported? import)
```

### imported\_by?

```clojure
(-> .Text .Text (.Meta .Bit))
```

```clojure
(imported_by? import module)
```

### imported\_modules

```clojure
(-> .Text (.Meta (.List .Text)))
```

All the modules imported by a specified module\.

```clojure
(imported_modules module_name)
```

### lifted

```clojure
(All (_ _0)
  (-> (library/lux/control/try.Try _0) (.Meta _0)))
```

### locals

```clojure
(.Meta (.List (.List [.Text .Type])))
```

All the local variables currently in scope, separated in different scopes\.

### location

```clojure
(.Meta .Location)
```

The location of the current expression being analyzed\.

### macro

```clojure
(-> .Symbol (.Meta (.Maybe .Macro)))
```

Looks\-up a macro known by the given name\.

```clojure
(macro full_name)
```

### module

```clojure
(-> .Text (.Meta .Module))
```

Looks\-up a module with the given name\.

```clojure
(module name)
```

### module\_exists?

```clojure
(-> .Text (.Meta .Bit))
```

```clojure
(module_exists? module)
```

### modules

```clojure
(.Meta (.List [.Text .Module]))
```

All the available modules \(including the current one\)\.

### monad

```clojure
(library/lux/abstract/monad.Monad .Meta)
```

### normal

```clojure
(-> .Symbol (.Meta .Symbol))
```

If given a name without a module prefix, gives it the current module's name as prefix\.
Otherwise, returns the name as\-is\.

```clojure
(normal name)
```

### result

```clojure
(All (_ _0)
  (-> .Lux (.Meta _0) (library/lux/control/try.Try _0)))
```

Evaluates a computation that depends on Lux's compiler state\.

```clojure
(result lux action)
```

### result'

```clojure
(All (_ _0)
  (-> .Lux (.Meta _0) (library/lux/control/try.Try [.Lux _0])))
```

Evaluates a computation that depends on Lux's compiler state\.
Also returns a \(potentially modified\) compiler state\.

```clojure
(result' lux action)
```

### seed

```clojure
(.Meta .Nat)
```

The current value of a number tracked by the compiler\.
Also increases the value, so it's different next time it is seen\.
This number can be used for generating data 'randomly' during compilation\.

### slot

```clojure
(-> .Symbol (.Meta [.Nat (.List .Symbol) .Type]))
```

Given a slot, finds out what is its index, its related slot\-list and its associated type\.

```clojure
(slot slot_name)
```

### tag

```clojure
(-> .Symbol (.Meta [.Nat (.List .Symbol) .Type]))
```

Given a tag, finds out what is its index, its related tag\-list and its associated type\.

```clojure
(tag tag_name)
```

### tag\_lists

```clojure
(-> .Text (.Meta (.List [(.List .Symbol) .Type])))
```

All the tag\-lists defined in a module, with their associated types\.

```clojure
(tag_lists module)
```

### tags\_of

```clojure
(-> .Symbol (.Meta (.Maybe (.List .Symbol))))
```

All the tags associated with a type definition\.

```clojure
(tags_of type_name)
```

### try

```clojure
(All (_ _0)
  (-> (.Meta _0) (.Meta (library/lux/control/try.Try _0))))
```

### type

```clojure
(-> .Symbol (.Meta .Type))
```

Looks\-up the type of either a local variable or a definition\.

```clojure
(type name)
```

### type\_context

```clojure
(.Meta .Type_Context)
```

The current type\-checking context\.

### type\_definition

```clojure
(-> .Symbol (.Meta .Type))
```

Finds the value of a type definition \(such as Int, Any or Lux\)\.

```clojure
(type_definition name)
```

### var\_type

```clojure
(-> .Text (.Meta .Type))
```

Looks\-up the type of a local variable somewhere in the environment\.

```clojure
(var_type name)
```

___

# library/lux/meta/location

## Definitions

### dummy

```clojure
.Location
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Location)
```

### format

```clojure
(-> .Location .Text)
```

### here

```clojure
.Macro
```

The Location of the current form\.

```clojure
(here)
```

### with

```clojure
(-> .Location .Text .Text)
```

```clojure
(with location error)
```

___

# library/lux/meta/symbol

## Definitions

### codec

```clojure
(library/lux/abstract/codec.Codec .Text .Symbol)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Symbol)
```

### hash

```clojure
(library/lux/abstract/hash.Hash .Symbol)
```

### module

```clojure
(-> .Symbol .Text)
```

The module part of a symbol\.

### order

```clojure
(library/lux/abstract/order.Order .Symbol)
```

### short

```clojure
(-> .Symbol .Text)
```

The short part of a symbol\.

___

# library/lux/program

## Definitions

### program:

```clojure
.Macro
```

Defines the entry\-point to a program \(similar to the 'main' function/method in other programming languages\)\.

```clojure
... Can take a list of all the input parameters to the program.

(program: all_arguments
  (do library/lux/control/io.monad
    [foo (initialize program)]
    (do_something_with all_arguments)))

................................................................
................................................................

... Can also parse them using CLI parsers from the library/lux/control/parser/cli module.

(program: [config configuration_parser]
  (do library/lux/control/io.monad
    [data (initialize program with config)]
    (do_something_with data)))
```

___

# library/lux/static

## Definitions

### frac

```clojure
.Macro
```

\(library/lux/static\.frac \(: library/lux\.Frac \(value generating expression\)\)\)

### int

```clojure
.Macro
```

\(library/lux/static\.int \(: library/lux\.Int \(value generating expression\)\)\)

### literal

```clojure
.Macro
```

```clojure
(literal
 (: (-> ??? Code)
    format)
 (: ???
    (value generating expression)))
```

### nat

```clojure
.Macro
```

\(library/lux/static\.nat \(: library/lux\.Nat \(value generating expression\)\)\)

### random

```clojure
.Macro
```

```clojure
(random
 (: (-> ??? Code)
    format)
 (: (Random ???)
    (random data generator)))
```

### random\_frac

```clojure
.Macro
```

\(: library/lux\.Frac \(library/lux/static\.random\_frac\)\)

### random\_int

```clojure
.Macro
```

\(: library/lux\.Int \(library/lux/static\.random\_int\)\)

### random\_nat

```clojure
.Macro
```

\(: library/lux\.Nat \(library/lux/static\.random\_nat\)\)

### random\_rev

```clojure
.Macro
```

\(: library/lux\.Rev \(library/lux/static\.random\_rev\)\)

### rev

```clojure
.Macro
```

\(library/lux/static\.rev \(: library/lux\.Rev \(value generating expression\)\)\)

### text

```clojure
.Macro
```

\(library/lux/static\.text \(: library/lux\.Text \(value generating expression\)\)\)

___

# library/lux/target

## Definitions

### Target

```clojure
... .Type
(Primitive "#Text")
```

The name/ID of a platform targetted by a Lux compiler\.
This information can be used to generate code targetting specific platforms, and to make programs cross\-platform\.

### common\_lisp

```clojure
Target
```

### js

```clojure
Target
```

### jvm

```clojure
Target
```

### lua

```clojure
Target
```

### old

```clojure
Target
```

### php

```clojure
Target
```

### python

```clojure
Target
```

### r

```clojure
Target
```

### ruby

```clojure
Target
```

### scheme

```clojure
Target
```

___

# library/lux/target/js

## Definitions

### %

```clojure
(-> Expression Expression Computation)
```

### \*

```clojure
(-> Expression Expression Computation)
```

### \+

```clojure
(-> Expression Expression Computation)
```

### \+\+

```clojure
(-> Location Expression)
```

### ,

```clojure
(-> Expression Expression Computation)
```

### \-

```clojure
(-> Expression Expression Computation)
```

### \-\-

```clojure
(-> Location Expression)
```

### /

```clojure
(-> Expression Expression Computation)
```

### <

```clojure
(-> Expression Expression Computation)
```

### <=

```clojure
(-> Expression Expression Computation)
```

### =

```clojure
(-> Expression Expression Computation)
```

### >

```clojure
(-> Expression Expression Computation)
```

### >=

```clojure
(-> Expression Expression Computation)
```

### ?

```clojure
(-> Expression Expression Expression Computation)
```

### Access

```clojure
... .Type
(Code (Expression' (Computation' (Location' Access'))))
```

### Code

```clojure
... .Type
(All (Code _0)
  (Primitive "library/lux/target/js.Code" _0))
```

### Computation

```clojure
... .Type
(Code (Expression' (Computation' .Any)))
```

### Expression

```clojure
... .Type
(Code (Expression' .Any))
```

### Label

```clojure
... .Type
(Code Label')
```

### Literal

```clojure
... .Type
(Code (Expression' (Computation' Literal')))
```

### Location

```clojure
... .Type
(Code (Expression' (Computation' (Location' .Any))))
```

### Loop

```clojure
... .Type
(Code (Statement' Loop'))
```

### Statement

```clojure
... .Type
(Code (Statement' .Any))
```

### Var

```clojure
... .Type
(Code (Expression' (Computation' (Location' Var'))))
```

### and

```clojure
(-> Expression Expression Computation)
```

### apply/\*

```clojure
(-> Expression (.List Expression) Computation)
```

### apply/1

```clojure
(-> Expression Expression Computation)
```

### apply/2

```clojure
(-> Expression Expression Expression Computation)
```

### apply/3

```clojure
(-> Expression Expression Expression Expression Computation)
```

### arithmetic\_right\_shift

```clojure
(-> Expression Expression Computation)
```

### array

```clojure
(-> (.List Expression) Computation)
```

### at

```clojure
(-> Expression Expression Access)
```

### bit\_and

```clojure
(-> Expression Expression Computation)
```

### bit\_not

```clojure
(-> Expression Computation)
```

### bit\_or

```clojure
(-> Expression Expression Computation)
```

### bit\_xor

```clojure
(-> Expression Expression Computation)
```

### boolean

```clojure
(-> .Bit Literal)
```

### break

```clojure
Statement
```

### break\_at

```clojure
(-> Label Statement)
```

### closure

```clojure
(-> (.List Var) Statement Computation)
```

### code

```clojure
(-> (Code .Any) .Text)
```

### comment

```clojure
(All (_ _0)
  (-> .Text (Code _0) (Code _0)))
```

### cond

```clojure
(-> (.List [Expression Statement]) Statement Statement)
```

### continue

```clojure
Statement
```

### continue\_at

```clojure
(-> Label Statement)
```

### declare

```clojure
(-> Var Statement)
```

### define

```clojure
(-> Var Expression Statement)
```

### delete

```clojure
(-> Location Statement)
```

### do

```clojure
(-> .Text (.List Expression) Expression Computation)
```

### do\_while

```clojure
(-> Expression Statement Loop)
```

### for

```clojure
(-> Var Expression Expression Expression Statement Loop)
```

### function

```clojure
(-> Var (.List Var) Statement Computation)
```

### function\!

```clojure
(-> Var (.List Var) Statement Statement)
```

### i32

```clojure
(-> .Int Computation)
```

### if

```clojure
(-> Expression Statement Statement Statement)
```

### int

```clojure
(-> .Int Literal)
```

### label

```clojure
(-> .Text Label)
```

### left\_shift

```clojure
(-> Expression Expression Computation)
```

### logic\_right\_shift

```clojure
(-> Expression Expression Computation)
```

### new

```clojure
(-> Expression (.List Expression) Computation)
```

### not

```clojure
(-> Expression Computation)
```

### not\_a\_number?

```clojure
(-> Expression Computation)
```

### null

```clojure
Literal
```

### number

```clojure
(-> .Frac Literal)
```

### object

```clojure
(-> (.List [.Text Expression]) Computation)
```

### opposite

```clojure
(-> Expression Computation)
```

### or

```clojure
(-> Expression Expression Computation)
```

### return

```clojure
(-> Expression Statement)
```

### set

```clojure
(-> Location Expression Statement)
```

### statement

```clojure
(-> Expression Statement)
```

### string

```clojure
(-> .Text Literal)
```

### switch

```clojure
(-> Expression (.List [(.List Literal) Statement]) (.Maybe Statement) Statement)
```

### the

```clojure
(-> .Text Expression Access)
```

### then

```clojure
(-> Statement Statement Statement)
```

### throw

```clojure
(-> Expression Statement)
```

### to\_i32

```clojure
(-> Expression Computation)
```

### try

```clojure
(-> Statement [Var Statement] Statement)
```

### type\_of

```clojure
(-> Expression Computation)
```

### undefined

```clojure
Literal
```

### use\_strict

```clojure
Statement
```

### var

```clojure
(-> .Text Var)
```

### when

```clojure
(-> Expression Statement Statement)
```

### while

```clojure
(-> Expression Statement Loop)
```

### with\_label

```clojure
(-> Label Loop Statement)
```

___

# library/lux/target/jvm

## Definitions

### Arithmetic

```clojure
... .Type
(Variant
 {#Int_Arithmetic Int_Arithmetic}
 {#Long_Arithmetic Long_Arithmetic}
 {#Float_Arithmetic Float_Arithmetic}
 {#Double_Arithmetic Double_Arithmetic})
```

### Array

```clojure
... .Type
(Variant
 {#ARRAYLENGTH .Any}
 {#NEWARRAY (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive)}
 {#ANEWARRAY (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Object)}
 {#BALOAD .Any}
 {#BASTORE .Any}
 {#SALOAD .Any}
 {#SASTORE .Any}
 {#IALOAD .Any}
 {#IASTORE .Any}
 {#LALOAD .Any}
 {#LASTORE .Any}
 {#FALOAD .Any}
 {#FASTORE .Any}
 {#DALOAD .Any}
 {#DASTORE .Any}
 {#CALOAD .Any}
 {#CASTORE .Any}
 {#AALOAD .Any}
 {#AASTORE .Any})
```

### Bitwise

```clojure
... .Type
(Variant
 {#Int_Bitwise Int_Bitwise}
 {#Long_Bitwise Long_Bitwise})
```

### Branching

```clojure
... .Type
(All (Branching _0)
  (Variant
   {#IF_ICMPEQ _0}
   {#IF_ICMPGE _0}
   {#IF_ICMPGT _0}
   {#IF_ICMPLE _0}
   {#IF_ICMPLT _0}
   {#IF_ICMPNE _0}
   {#IFEQ _0}
   {#IFNE _0}
   {#IFGE _0}
   {#IFGT _0}
   {#IFLE _0}
   {#IFLT _0}
   {#TABLESWITCH .Int .Int _0 (.List _0)}
   {#LOOKUPSWITCH _0 (.List [.Int _0])}
   {#IF_ACMPEQ _0}
   {#IF_ACMPNE _0}
   {#IFNONNULL _0}
   {#IFNULL _0}))
```

### Bytecode

```clojure
... .Type
(All (Bytecode _0 _1)
  (library/lux/data/collection/sequence.Sequence (Instruction _0 _1)))
```

### Comparison

```clojure
... .Type
(Variant
 {#LCMP .Any}
 {#FCMPG .Any}
 {#FCMPL .Any}
 {#DCMPG .Any}
 {#DCMPL .Any})
```

### Concurrency

```clojure
... .Type
(Variant
 {#MONITORENTER .Any}
 {#MONITOREXIT .Any})
```

### Constant

```clojure
... .Type
(Variant
 {#BIPUSH .Int}
 {#SIPUSH .Int}
 {#ICONST_M1 .Any}
 {#ICONST_0 .Any}
 {#ICONST_1 .Any}
 {#ICONST_2 .Any}
 {#ICONST_3 .Any}
 {#ICONST_4 .Any}
 {#ICONST_5 .Any}
 {#LCONST_0 .Any}
 {#LCONST_1 .Any}
 {#FCONST_0 .Any}
 {#FCONST_1 .Any}
 {#FCONST_2 .Any}
 {#DCONST_0 .Any}
 {#DCONST_1 .Any}
 {#ACONST_NULL .Any}
 {#LDC Literal})
```

### Control

```clojure
... .Type
(All (Control _0)
  (Variant
   {#GOTO _0}
   {#Branching (Branching _0)}
   {#Exception (Exception _0)}
   {#Concurrency Concurrency}
   {#Return Return}))
```

### Conversion

```clojure
... .Type
(Variant
 {#I2B .Any}
 {#I2S .Any}
 {#I2L .Any}
 {#I2F .Any}
 {#I2D .Any}
 {#I2C .Any}
 {#L2I .Any}
 {#L2F .Any}
 {#L2D .Any}
 {#F2I .Any}
 {#F2L .Any}
 {#F2D .Any}
 {#D2I .Any}
 {#D2L .Any}
 {#D2F .Any})
```

### Double\_Arithmetic

```clojure
... .Type
(Variant
 {#DADD .Any}
 {#DSUB .Any}
 {#DMUL .Any}
 {#DDIV .Any}
 {#DREM .Any}
 {#DNEG .Any})
```

### Exception

```clojure
... .Type
(All (Exception _0)
  (Variant
   {#Try _0 _0 _0 (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class)}
   {#ATHROW .Any}))
```

### Float\_Arithmetic

```clojure
... .Type
(Variant
 {#FADD .Any}
 {#FSUB .Any}
 {#FMUL .Any}
 {#FDIV .Any}
 {#FREM .Any}
 {#FNEG .Any})
```

### Instruction

```clojure
... .Type
(All (Instruction _0 _1)
  (Variant
   {#NOP .Any}
   {#Constant Constant}
   {#Arithmetic Arithmetic}
   {#Bitwise Bitwise}
   {#Conversion Conversion}
   {#Array Array}
   {#Object Object}
   {#Local Local}
   {#Stack Stack}
   {#Comparison Comparison}
   {#Control (Control _1)}
   {#Embedded _0}))
```

### Int\_Arithmetic

```clojure
... .Type
(Variant
 {#IADD .Any}
 {#ISUB .Any}
 {#IMUL .Any}
 {#IDIV .Any}
 {#IREM .Any}
 {#INEG .Any})
```

### Int\_Bitwise

```clojure
... .Type
(Variant
 {#IOR .Any}
 {#IXOR .Any}
 {#IAND .Any}
 {#ISHL .Any}
 {#ISHR .Any}
 {#IUSHR .Any})
```

### Label

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

### Literal

```clojure
... .Type
(Variant
 {#Boolean .Bit}
 {#Int .Int}
 {#Long .Int}
 {#Double .Frac}
 {#Char .Nat}
 {#String .Text})
```

### Local

```clojure
... .Type
(Variant
 {#Local_Int Local_Int}
 {#IINC Register}
 {#Local_Long Local_Long}
 {#Local_Float Local_Float}
 {#Local_Double Local_Double}
 {#Local_Object Local_Object})
```

### Local\_Double

```clojure
... .Type
(Variant
 {#DLOAD Register}
 {#DSTORE Register})
```

### Local\_Float

```clojure
... .Type
(Variant
 {#FLOAD Register}
 {#FSTORE Register})
```

### Local\_Int

```clojure
... .Type
(Variant
 {#ILOAD Register}
 {#ISTORE Register})
```

### Local\_Long

```clojure
... .Type
(Variant
 {#LLOAD Register}
 {#LSTORE Register})
```

### Local\_Object

```clojure
... .Type
(Variant
 {#ALOAD Register}
 {#ASTORE Register})
```

### Long\_Arithmetic

```clojure
... .Type
(Variant
 {#LADD .Any}
 {#LSUB .Any}
 {#LMUL .Any}
 {#LDIV .Any}
 {#LREM .Any}
 {#LNEG .Any})
```

### Long\_Bitwise

```clojure
... .Type
(Variant
 {#LOR .Any}
 {#LXOR .Any}
 {#LAND .Any}
 {#LSHL .Any}
 {#LSHR .Any}
 {#LUSHR .Any})
```

### Object

```clojure
... .Type
(Variant
 {#GETSTATIC (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class) .Text (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value)}
 {#PUTSTATIC (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class) .Text (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value)}
 {#NEW (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class)}
 {#INSTANCEOF (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class)}
 {#CHECKCAST (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Object)}
 {#GETFIELD (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class) .Text (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value)}
 {#PUTFIELD (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class) .Text (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value)}
 {#INVOKEINTERFACE (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class) .Text (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Method)}
 {#INVOKESPECIAL (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class) .Text (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Method)}
 {#INVOKESTATIC (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class) .Text (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Method)}
 {#INVOKEVIRTUAL (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class) .Text (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Method)})
```

### Register

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

### Return

```clojure
... .Type
(Variant
 {#RETURN .Any}
 {#IRETURN .Any}
 {#LRETURN .Any}
 {#FRETURN .Any}
 {#DRETURN .Any}
 {#ARETURN .Any})
```

### Stack

```clojure
... .Type
(Variant
 {#DUP .Any}
 {#DUP_X1 .Any}
 {#DUP_X2 .Any}
 {#DUP2 .Any}
 {#DUP2_X1 .Any}
 {#DUP2_X2 .Any}
 {#SWAP .Any}
 {#POP .Any}
 {#POP2 .Any})
```

___

# library/lux/target/jvm/type

## Definitions

### Argument

```clojure
... .Type
[.Text (Type library/lux/target/jvm/type/category.Value)]
```

### Constraint

```clojure
... .Type
(Record
 [#name .Text
  #super_class (Type library/lux/target/jvm/type/category.Class)
  #super_interfaces (.List (Type library/lux/target/jvm/type/category.Class))])
```

### Type

```clojure
... .Type
(All (Type _0)
  (Primitive "library/lux/target/jvm/type.Type" _0))
```

### Typed

```clojure
... .Type
(All (Typed _0)
  [(Type library/lux/target/jvm/type/category.Value) _0])
```

### array

```clojure
(-> (Type library/lux/target/jvm/type/category.Value) (Type library/lux/target/jvm/type/category.Array))
```

### as\_class

```clojure
(-> (Type library/lux/target/jvm/type/category.Declaration) (Type library/lux/target/jvm/type/category.Class))
```

### boolean

```clojure
(Type library/lux/target/jvm/type/category.Primitive)
```

### byte

```clojure
(Type library/lux/target/jvm/type/category.Primitive)
```

### char

```clojure
(Type library/lux/target/jvm/type/category.Primitive)
```

### class

```clojure
(-> library/lux/target/jvm/encoding/name.External (.List (Type library/lux/target/jvm/type/category.Parameter)) (Type library/lux/target/jvm/type/category.Class))
```

### class?

```clojure
(-> (Type library/lux/target/jvm/type/category.Value) (.Maybe library/lux/target/jvm/encoding/name.External))
```

### declaration

```clojure
(-> library/lux/target/jvm/encoding/name.External (.List (Type library/lux/target/jvm/type/category.Var)) (Type library/lux/target/jvm/type/category.Declaration))
```

### descriptor

```clojure
(All (_ _0)
  (-> (Type _0) (library/lux/target/jvm/type/descriptor.Descriptor _0)))
```

### double

```clojure
(Type library/lux/target/jvm/type/category.Primitive)
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Type _0)))
```

### float

```clojure
(Type library/lux/target/jvm/type/category.Primitive)
```

### format

```clojure
(All (_ _0)
  (library/lux/data/text/format.Format (Type _0)))
```

### hash

```clojure
(All (_ _0)
  (library/lux/abstract/hash.Hash (Type _0)))
```

### int

```clojure
(Type library/lux/target/jvm/type/category.Primitive)
```

### long

```clojure
(Type library/lux/target/jvm/type/category.Primitive)
```

### lower

```clojure
(-> (Type library/lux/target/jvm/type/category.Class) (Type library/lux/target/jvm/type/category.Parameter))
```

### method

```clojure
(-> [(.List (Type library/lux/target/jvm/type/category.Var)) (.List (Type library/lux/target/jvm/type/category.Value)) (Type library/lux/target/jvm/type/category.Return) (.List (Type library/lux/target/jvm/type/category.Class))] (Type library/lux/target/jvm/type/category.Method))
```

### primitive?

```clojure
(-> (Type library/lux/target/jvm/type/category.Value) (.Either (Type library/lux/target/jvm/type/category.Object) (Type library/lux/target/jvm/type/category.Primitive)))
```

### reflection

```clojure
(All (_ _0)
  (-> (Type (library/lux/target/jvm/type/category.Return' (library/lux/target/jvm/type/category.Value' _0))) (library/lux/target/jvm/type/reflection.Reflection (library/lux/target/jvm/type/category.Return' (library/lux/target/jvm/type/category.Value' _0)))))
```

### short

```clojure
(Type library/lux/target/jvm/type/category.Primitive)
```

### signature

```clojure
(All (_ _0)
  (-> (Type _0) (library/lux/target/jvm/type/signature.Signature _0)))
```

### upper

```clojure
(-> (Type library/lux/target/jvm/type/category.Class) (Type library/lux/target/jvm/type/category.Parameter))
```

### var

```clojure
(-> .Text (Type library/lux/target/jvm/type/category.Var))
```

### void

```clojure
(Type library/lux/target/jvm/type/category.Void)
```

### void?

```clojure
(-> (Type library/lux/target/jvm/type/category.Return) (.Either (Type library/lux/target/jvm/type/category.Value) (Type library/lux/target/jvm/type/category.Void)))
```

### wildcard

```clojure
(Type library/lux/target/jvm/type/category.Parameter)
```

___

# library/lux/target/jvm/type/alias

## Definitions

### Aliasing

```clojure
... .Type
(library/lux/data/collection/dictionary.Dictionary .Text .Text)
```

### fresh

```clojure
Aliasing
```

### method

```clojure
(-> Aliasing (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Method) (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Method))
```

___

# library/lux/target/jvm/type/box

## Definitions

### boolean

```clojure
library/lux/target/jvm/encoding/name.External
```

### byte

```clojure
library/lux/target/jvm/encoding/name.External
```

### char

```clojure
library/lux/target/jvm/encoding/name.External
```

### double

```clojure
library/lux/target/jvm/encoding/name.External
```

### float

```clojure
library/lux/target/jvm/encoding/name.External
```

### int

```clojure
library/lux/target/jvm/encoding/name.External
```

### long

```clojure
library/lux/target/jvm/encoding/name.External
```

### short

```clojure
library/lux/target/jvm/encoding/name.External
```

___

# library/lux/target/jvm/type/category

## Definitions

### Array

```clojure
... .Type
(Return' (Value' (Object' Array')))
```

### Class

```clojure
... .Type
(Return' (Value' (Object' (Parameter' Class'))))
```

### Declaration

```clojure
... .Type
(Primitive "library/lux/target/jvm/type/category.Declaration")
```

### Method

```clojure
... .Type
(Primitive "library/lux/target/jvm/type/category.Method")
```

### Object

```clojure
... .Type
(Return' (Value' (Object' .Any)))
```

### Parameter

```clojure
... .Type
(Return' (Value' (Object' (Parameter' .Any))))
```

### Primitive

```clojure
... .Type
(Return' (Value' Primitive'))
```

### Return

```clojure
... .Type
(Return' .Any)
```

### Return'

```clojure
... .Type
(All (Return' _0)
  (Primitive "library/lux/target/jvm/type/category.Return'" _0))
```

### Value

```clojure
... .Type
(Return' (Value' .Any))
```

### Value'

```clojure
... .Type
(All (Value' _0)
  (Primitive "library/lux/target/jvm/type/category.Value'" _0))
```

### Var

```clojure
... .Type
(Return' (Value' (Object' (Parameter' Var'))))
```

### Void

```clojure
... .Type
(Return' Void')
```

___

# library/lux/target/jvm/type/descriptor

## Definitions

### Descriptor

```clojure
... .Type
(All (Descriptor _0)
  (Primitive "library/lux/target/jvm/type/descriptor.Descriptor" _0))
```

### array

```clojure
(-> (Descriptor library/lux/target/jvm/type/category.Value) (Descriptor library/lux/target/jvm/type/category.Array))
```

### array\_prefix

```clojure
.Text
```

### as\_class

```clojure
(-> (Descriptor library/lux/target/jvm/type/category.Declaration) (Descriptor library/lux/target/jvm/type/category.Class))
```

### boolean

```clojure
(Descriptor library/lux/target/jvm/type/category.Primitive)
```

### byte

```clojure
(Descriptor library/lux/target/jvm/type/category.Primitive)
```

### char

```clojure
(Descriptor library/lux/target/jvm/type/category.Primitive)
```

### class

```clojure
(-> library/lux/target/jvm/encoding/name.External (Descriptor library/lux/target/jvm/type/category.Class))
```

### class\_name

```clojure
(-> (Descriptor library/lux/target/jvm/type/category.Object) library/lux/target/jvm/encoding/name.Internal)
```

### class\_prefix

```clojure
.Text
```

### class\_suffix

```clojure
.Text
```

### declaration

```clojure
(-> library/lux/target/jvm/encoding/name.External (Descriptor library/lux/target/jvm/type/category.Declaration))
```

### descriptor

```clojure
(-> (Descriptor .Any) .Text)
```

### double

```clojure
(Descriptor library/lux/target/jvm/type/category.Primitive)
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Descriptor _0)))
```

### float

```clojure
(Descriptor library/lux/target/jvm/type/category.Primitive)
```

### int

```clojure
(Descriptor library/lux/target/jvm/type/category.Primitive)
```

### long

```clojure
(Descriptor library/lux/target/jvm/type/category.Primitive)
```

### lower

```clojure
(-> (Descriptor library/lux/target/jvm/type/category.Class) (Descriptor library/lux/target/jvm/type/category.Parameter))
```

### method

```clojure
(-> [(.List (Descriptor library/lux/target/jvm/type/category.Value)) (Descriptor library/lux/target/jvm/type/category.Return)] (Descriptor library/lux/target/jvm/type/category.Method))
```

### short

```clojure
(Descriptor library/lux/target/jvm/type/category.Primitive)
```

### upper

```clojure
(-> (Descriptor library/lux/target/jvm/type/category.Class) (Descriptor library/lux/target/jvm/type/category.Parameter))
```

### var

```clojure
(Descriptor library/lux/target/jvm/type/category.Var)
```

### void

```clojure
(Descriptor library/lux/target/jvm/type/category.Void)
```

### wildcard

```clojure
(Descriptor library/lux/target/jvm/type/category.Parameter)
```

___

# library/lux/target/jvm/type/lux

## Definitions

### Lower

```clojure
... .Type
(All (Lower _0)
  (Primitive "library/lux/target/jvm/type/lux.Lower" _0))
```

### Mapping

```clojure
... .Type
(library/lux/data/collection/dictionary.Dictionary .Text .Type)
```

### Upper

```clojure
... .Type
(All (Upper _0)
  (Primitive "library/lux/target/jvm/type/lux.Upper" _0))
```

### boxed\_return

```clojure
(-> Mapping (library/lux/control/parser/text.Parser (library/lux/type/check.Check .Type)))
```

### boxed\_type

```clojure
(-> Mapping (library/lux/control/parser/text.Parser (library/lux/type/check.Check .Type)))
```

### check

```clojure
(All (_ _0)
  (-> (library/lux/control/parser/text.Parser (library/lux/type/check.Check _0)) .Text (library/lux/type/check.Check _0)))
```

### class

```clojure
(-> Mapping (library/lux/control/parser/text.Parser (library/lux/type/check.Check .Type)))
```

### fresh

```clojure
Mapping
```

### return

```clojure
(-> Mapping (library/lux/control/parser/text.Parser (library/lux/type/check.Check .Type)))
```

### type

```clojure
(-> Mapping (library/lux/control/parser/text.Parser (library/lux/type/check.Check .Type)))
```

### unknown\_var

```clojure
(library/lux/control/exception.Exception .Text)
```

___

# library/lux/target/jvm/type/parser

## Definitions

### array

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Array))
```

### array'

```clojure
(-> (library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value)) (library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Array)))
```

### array?

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value) (.Maybe (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value)))
```

### boolean

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive))
```

### byte

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive))
```

### char

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive))
```

### class

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class))
```

### class?

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value) (.Maybe [library/lux/target/jvm/encoding/name.External (.List (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Parameter))]))
```

### class\_name

```clojure
(library/lux/control/parser/text.Parser library/lux/target/jvm/encoding/name.External)
```

### declaration

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Declaration) [library/lux/target/jvm/encoding/name.External (.List (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Var))])
```

### declaration'

```clojure
(library/lux/control/parser/text.Parser [library/lux/target/jvm/encoding/name.External (.List (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Var))])
```

### double

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive))
```

### float

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive))
```

### int

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive))
```

### long

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive))
```

### lower?

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value) (.Maybe (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class)))
```

### method

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Method) [(.List (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Var)) (.List (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value)) (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Return) (.List (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class))])
```

### name

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Var) .Text)
```

### object

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Object))
```

### object?

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value) (.Maybe (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Object)))
```

### parameter

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Parameter))
```

### parameter?

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value) (.Maybe (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Parameter)))
```

### primitive

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive))
```

### primitive?

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value) (.Maybe (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive)))
```

### read\_class

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class) [library/lux/target/jvm/encoding/name.External (.List (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Parameter))])
```

### return

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Return))
```

### short

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Primitive))
```

### upper?

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value) (.Maybe (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Class)))
```

### value

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value))
```

### var

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Var))
```

### var'

```clojure
(library/lux/control/parser/text.Parser .Text)
```

### var?

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value) (.Maybe .Text))
```

### var\_name

```clojure
(library/lux/control/parser/text.Parser .Text)
```

### void

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Void))
```

### wildcard

```clojure
(library/lux/control/parser/text.Parser (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Parameter))
```

### wildcard?

```clojure
(-> (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value) (.Maybe (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Parameter)))
```

___

# library/lux/target/jvm/type/reflection

## Definitions

### Reflection

```clojure
... .Type
(All (Reflection _0)
  (Primitive "library/lux/target/jvm/type/reflection.Reflection" _0))
```

### array

```clojure
(-> (Reflection library/lux/target/jvm/type/category.Value) (Reflection library/lux/target/jvm/type/category.Array))
```

### as\_class

```clojure
(-> (Reflection library/lux/target/jvm/type/category.Declaration) (Reflection library/lux/target/jvm/type/category.Class))
```

### boolean

```clojure
(Reflection library/lux/target/jvm/type/category.Primitive)
```

### byte

```clojure
(Reflection library/lux/target/jvm/type/category.Primitive)
```

### char

```clojure
(Reflection library/lux/target/jvm/type/category.Primitive)
```

### class

```clojure
(-> library/lux/target/jvm/encoding/name.External (Reflection library/lux/target/jvm/type/category.Class))
```

### declaration

```clojure
(-> library/lux/target/jvm/encoding/name.External (Reflection library/lux/target/jvm/type/category.Declaration))
```

### double

```clojure
(Reflection library/lux/target/jvm/type/category.Primitive)
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Reflection _0)))
```

### float

```clojure
(Reflection library/lux/target/jvm/type/category.Primitive)
```

### int

```clojure
(Reflection library/lux/target/jvm/type/category.Primitive)
```

### long

```clojure
(Reflection library/lux/target/jvm/type/category.Primitive)
```

### lower

```clojure
(-> (Reflection library/lux/target/jvm/type/category.Class) (Reflection library/lux/target/jvm/type/category.Parameter))
```

### reflection

```clojure
(-> (Reflection .Any) .Text)
```

### short

```clojure
(Reflection library/lux/target/jvm/type/category.Primitive)
```

### upper

```clojure
(-> (Reflection library/lux/target/jvm/type/category.Class) (Reflection library/lux/target/jvm/type/category.Parameter))
```

### var

```clojure
(Reflection library/lux/target/jvm/type/category.Var)
```

### void

```clojure
(Reflection library/lux/target/jvm/type/category.Void)
```

### wildcard

```clojure
(Reflection library/lux/target/jvm/type/category.Parameter)
```

___

# library/lux/target/jvm/type/signature

## Definitions

### Signature

```clojure
... .Type
(All (Signature _0)
  (Primitive "library/lux/target/jvm/type/signature.Signature" _0))
```

### arguments\_end

```clojure
.Text
```

### arguments\_start

```clojure
.Text
```

### array

```clojure
(-> (Signature library/lux/target/jvm/type/category.Value) (Signature library/lux/target/jvm/type/category.Array))
```

### as\_class

```clojure
(-> (Signature library/lux/target/jvm/type/category.Declaration) (Signature library/lux/target/jvm/type/category.Class))
```

### boolean

```clojure
(Signature library/lux/target/jvm/type/category.Primitive)
```

### byte

```clojure
(Signature library/lux/target/jvm/type/category.Primitive)
```

### char

```clojure
(Signature library/lux/target/jvm/type/category.Primitive)
```

### class

```clojure
(-> library/lux/target/jvm/encoding/name.External (.List (Signature library/lux/target/jvm/type/category.Parameter)) (Signature library/lux/target/jvm/type/category.Class))
```

### declaration

```clojure
(-> library/lux/target/jvm/encoding/name.External (.List (Signature library/lux/target/jvm/type/category.Var)) (Signature library/lux/target/jvm/type/category.Declaration))
```

### double

```clojure
(Signature library/lux/target/jvm/type/category.Primitive)
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Signature _0)))
```

### exception\_prefix

```clojure
.Text
```

### float

```clojure
(Signature library/lux/target/jvm/type/category.Primitive)
```

### hash

```clojure
(All (_ _0)
  (library/lux/abstract/hash.Hash (Signature _0)))
```

### int

```clojure
(Signature library/lux/target/jvm/type/category.Primitive)
```

### long

```clojure
(Signature library/lux/target/jvm/type/category.Primitive)
```

### lower

```clojure
(-> (Signature library/lux/target/jvm/type/category.Class) (Signature library/lux/target/jvm/type/category.Parameter))
```

### lower\_prefix

```clojure
.Text
```

### method

```clojure
(-> [(.List (Signature library/lux/target/jvm/type/category.Var)) (.List (Signature library/lux/target/jvm/type/category.Value)) (Signature library/lux/target/jvm/type/category.Return) (.List (Signature library/lux/target/jvm/type/category.Class))] (Signature library/lux/target/jvm/type/category.Method))
```

### parameters\_end

```clojure
.Text
```

### parameters\_start

```clojure
.Text
```

### short

```clojure
(Signature library/lux/target/jvm/type/category.Primitive)
```

### signature

```clojure
(-> (Signature .Any) .Text)
```

### upper

```clojure
(-> (Signature library/lux/target/jvm/type/category.Class) (Signature library/lux/target/jvm/type/category.Parameter))
```

### upper\_prefix

```clojure
.Text
```

### var

```clojure
(-> .Text (Signature library/lux/target/jvm/type/category.Var))
```

### var\_name

```clojure
(-> (Signature library/lux/target/jvm/type/category.Var) .Text)
```

### var\_prefix

```clojure
.Text
```

### void

```clojure
(Signature library/lux/target/jvm/type/category.Void)
```

### wildcard

```clojure
(Signature library/lux/target/jvm/type/category.Parameter)
```

___

# library/lux/target/lua

## Definitions

### %

```clojure
(-> Expression Expression Expression)
```

### \*

```clojure
(-> Expression Expression Expression)
```

### \+

```clojure
(-> Expression Expression Expression)
```

### \-

```clojure
(-> Expression Expression Expression)
```

### /

```clojure
(-> Expression Expression Expression)
```

### //

```clojure
(-> Expression Expression Expression)
```

### <

```clojure
(-> Expression Expression Expression)
```

### <=

```clojure
(-> Expression Expression Expression)
```

### =

```clojure
(-> Expression Expression Expression)
```

### >

```clojure
(-> Expression Expression Expression)
```

### >=

```clojure
(-> Expression Expression Expression)
```

### Access

```clojure
... .Type
(Code (Expression' (Computation' (Location' Access'))))
```

### Code

```clojure
... .Type
(All (Code _0)
  (Primitive "library/lux/target/lua.Code" _0))
```

### Computation

```clojure
... .Type
(Code (Expression' (Computation' .Any)))
```

### Expression

```clojure
... .Type
(Code (Expression' .Any))
```

### Label

```clojure
... .Type
(Code Label')
```

### Literal

```clojure
... .Type
(Code (Expression' (Computation' Literal')))
```

### Location

```clojure
... .Type
(Code (Expression' (Computation' (Location' .Any))))
```

### Statement

```clojure
... .Type
(Code (Statement' .Any))
```

### Var

```clojure
... .Type
(Code (Expression' (Computation' (Location' Var'))))
```

### ^

```clojure
(-> Expression Expression Expression)
```

### and

```clojure
(-> Expression Expression Expression)
```

### apply/\*

```clojure
(-> (.List Expression) Expression Computation)
```

### apply/1

```clojure
(-> Expression Expression Computation)
```

### apply/2

```clojure
(-> Expression Expression Expression Computation)
```

### apply/3

```clojure
(-> Expression Expression Expression Expression Computation)
```

### apply/4

```clojure
(-> Expression Expression Expression Expression Expression Computation)
```

### apply/5

```clojure
(-> Expression Expression Expression Expression Expression Expression Computation)
```

### array

```clojure
(-> (.List Expression) Literal)
```

### bit\_and

```clojure
(-> Expression Expression Expression)
```

### bit\_or

```clojure
(-> Expression Expression Expression)
```

### bit\_shl

```clojure
(-> Expression Expression Expression)
```

### bit\_shr

```clojure
(-> Expression Expression Expression)
```

### bit\_xor

```clojure
(-> Expression Expression Expression)
```

### bool

```clojure
(-> .Bit Literal)
```

### break

```clojure
Statement
```

### closure

```clojure
(-> (.List Var) Statement Expression)
```

### code

```clojure
(-> (Code .Any) .Text)
```

### concat

```clojure
(-> Expression Expression Expression)
```

### cond

```clojure
(-> (.List [Expression Statement]) Statement Statement)
```

### do

```clojure
(-> .Text (.List Expression) Expression Computation)
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Code _0)))
```

### error/1

```clojure
(-> Expression Computation)
```

### error/2

```clojure
(-> Expression Expression Computation)
```

### float

```clojure
(-> .Frac Literal)
```

### for\_in

```clojure
(-> (.List Var) Expression Statement Statement)
```

### for\_step

```clojure
(-> Var Expression Expression Expression Statement Statement)
```

### function

```clojure
(-> Var (.List Var) Statement Statement)
```

### go\_to

```clojure
(-> Label Statement)
```

### hash

```clojure
(All (_ _0)
  (library/lux/abstract/hash.Hash (Code _0)))
```

### if

```clojure
(-> Expression Statement Statement Statement)
```

### int

```clojure
(-> .Int Literal)
```

### ipairs/1

```clojure
(-> Expression Computation)
```

### item

```clojure
(-> Expression Expression Access)
```

### label

```clojure
(-> .Text Label)
```

### length

```clojure
(-> Expression Computation)
```

### let

```clojure
(-> (.List Var) Expression Statement)
```

### local

```clojure
(-> (.List Var) Statement)
```

### local/1

```clojure
(-> Var Expression Statement)
```

### local\_function

```clojure
(-> Var (.List Var) Statement Statement)
```

### manual

```clojure
(-> .Text Code)
```

### multi

```clojure
(-> (.List Expression) Literal)
```

### nil

```clojure
Literal
```

### not

```clojure
(-> Expression Expression)
```

### opposite

```clojure
(-> Expression Expression)
```

### or

```clojure
(-> Expression Expression Expression)
```

### print/1

```clojure
(-> Expression Computation)
```

### print/2

```clojure
(-> Expression Expression Computation)
```

### print/3

```clojure
(-> Expression Expression Expression Computation)
```

### repeat

```clojure
(-> Expression Statement Statement)
```

### require/1

```clojure
(-> Expression Computation)
```

### return

```clojure
(-> Expression Statement)
```

### set

```clojure
(-> (.List Location) Expression Statement)
```

### set\_label

```clojure
(-> Label Statement)
```

### statement

```clojure
(-> Expression Statement)
```

### string

```clojure
(-> .Text Literal)
```

### table

```clojure
(-> (.List [.Text Expression]) Literal)
```

### the

```clojure
(-> .Text Expression Computation)
```

### then

```clojure
(-> Statement Statement Statement)
```

### type/1

```clojure
(-> Expression Computation)
```

### var

```clojure
(-> .Text Var)
```

### when

```clojure
(-> Expression Statement Statement)
```

### while

```clojure
(-> Expression Statement Statement)
```

___

# library/lux/target/python

## Definitions

### %

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### \*

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### \*\*

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### \+

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### \-

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### /

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### //

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### <

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### <=

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### =

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### >

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### >=

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### ?

```clojure
(-> (Expression .Any) (Expression .Any) (Expression .Any) (Computation .Any))
```

### Access

```clojure
... .Type
(Location Access')
```

### Code

```clojure
... .Type
(All (Code _0)
  (Primitive "library/lux/target/python.Code" _0))
```

### Computation

```clojure
... .Type
(All (Computation _0)
  (Expression (Computation' _0)))
```

### Except

```clojure
... .Type
(Record
 [#classes (.List SVar)
  #exception SVar
  #handler (Statement .Any)])
```

### Exception/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### Expression

```clojure
... .Type
(All (Expression _0)
  (Code (Expression' _0)))
```

### KVar

```clojure
... .Type
(Var Keyword)
```

### Keyword

```clojure
... .Type
(Primitive "library/lux/target/python.Keyword")
```

### Label

```clojure
... .Type
(Code Label')
```

### Literal

```clojure
... .Type
(Computation Literal')
```

### Location

```clojure
... .Type
(All (Location _0)
  (Computation (Location' _0)))
```

### Loop

```clojure
... .Type
(Statement Loop')
```

### PVar

```clojure
... .Type
(Var Poly)
```

### Poly

```clojure
... .Type
(Primitive "library/lux/target/python.Poly")
```

### SVar

```clojure
... .Type
(Var Single)
```

### Single

```clojure
... .Type
(Primitive "library/lux/target/python.Single")
```

### Statement

```clojure
... .Type
(All (Statement _0)
  (Code (Statement' _0)))
```

### Var

```clojure
... .Type
(All (Var _0)
  (Location (Var' _0)))
```

### \_\_import\_\_/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### and

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### apply/\*

```clojure
(-> (Expression .Any) (.List (Expression .Any)) (Computation .Any))
```

### apply/1

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### apply/2

```clojure
(-> (Expression .Any) (Expression .Any) (Expression .Any) (Computation .Any))
```

### apply/3

```clojure
(-> (Expression .Any) (Expression .Any) (Expression .Any) (Expression .Any) (Computation .Any))
```

### apply\_keyword

```clojure
(-> (.List (Expression .Any)) (Expression .Any) (Expression .Any) (Computation .Any))
```

### apply\_poly

```clojure
(-> (.List (Expression .Any)) (Expression .Any) (Expression .Any) (Computation .Any))
```

### bit\_and

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### bit\_or

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### bit\_shl

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### bit\_shr

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### bit\_xor

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### bool

```clojure
(-> .Bit Literal)
```

### break

```clojure
(Statement .Any)
```

### chr/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### code

```clojure
(-> (Code .Any) .Text)
```

### comment

```clojure
(All (_ _0)
  (-> .Text (Code _0) (Code _0)))
```

### cond

```clojure
(-> (.List [(Expression .Any) (Statement .Any)]) (Statement .Any) (Statement .Any))
```

### continue

```clojure
(Statement .Any)
```

### def

```clojure
(-> SVar (.List (Ex (_ _0) (Var _0))) (Statement .Any) (Statement .Any))
```

### delete

```clojure
(-> (Location .Any) (Statement .Any))
```

### dict

```clojure
(-> (.List [(Expression .Any) (Expression .Any)]) (Computation .Any))
```

### do

```clojure
(-> .Text (.List (Expression .Any)) (Expression .Any) (Computation .Any))
```

### do\_keyword

```clojure
(-> (.List (Expression .Any)) (Expression .Any) .Text (Expression .Any) (Computation .Any))
```

### do\_poly

```clojure
(-> (.List (Expression .Any)) (Expression .Any) .Text (Expression .Any) (Computation .Any))
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Code _0)))
```

### exec

```clojure
(-> (Expression .Any) (.Maybe (Expression .Any)) (Statement .Any))
```

### float

```clojure
(-> .Frac Literal)
```

### float/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### for\_in

```clojure
(-> SVar (Expression .Any) (Statement .Any) Loop)
```

### hash

```clojure
(All (_ _0)
  (library/lux/abstract/hash.Hash (Code _0)))
```

### if

```clojure
(-> (Expression .Any) (Statement .Any) (Statement .Any) (Statement .Any))
```

### import

```clojure
(-> .Text (Statement .Any))
```

### int

```clojure
(-> .Int Literal)
```

### int/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### is

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### item

```clojure
(-> (Expression .Any) (Expression .Any) Location)
```

### keyword

```clojure
(-> SVar (Var Keyword))
```

### lambda

```clojure
(-> (.List (Var .Any)) (Expression .Any) (Computation .Any))
```

### len/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### list

```clojure
(-> (.List (Expression .Any)) Literal)
```

### long

```clojure
(-> .Int Literal)
```

### manual

```clojure
(-> .Text Code)
```

### none

```clojure
Literal
```

### not

```clojure
(-> (Expression .Any) (Computation .Any))
```

### opposite

```clojure
(-> (Expression .Any) (Computation .Any))
```

### or

```clojure
(-> (Expression .Any) (Expression .Any) (Computation .Any))
```

### ord/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### pass

```clojure
(Statement .Any)
```

### poly

```clojure
(-> SVar (Var Poly))
```

### print

```clojure
(-> (Expression .Any) (Statement .Any))
```

### raise

```clojure
(-> (Expression .Any) (Statement .Any))
```

### repr/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### return

```clojure
(-> (Expression .Any) (Statement .Any))
```

### set

```clojure
(-> (.List (Location .Any)) (Expression .Any) (Statement .Any))
```

### slice

```clojure
(-> (Expression .Any) (Expression .Any) (Expression .Any) Access)
```

### slice\_from

```clojure
(-> (Expression .Any) (Expression .Any) Access)
```

### statement

```clojure
(-> (Expression .Any) (Statement .Any))
```

### str/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### string

```clojure
(-> .Text Literal)
```

### the

```clojure
(-> .Text (Expression .Any) (Computation .Any))
```

### then

```clojure
(-> (Statement .Any) (Statement .Any) (Statement .Any))
```

### try

```clojure
(-> (Statement .Any) (.List Except) (Statement .Any))
```

### tuple

```clojure
(-> (.List (Expression .Any)) Literal)
```

### unichr/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### unicode

```clojure
(-> .Text Literal)
```

### unicode/1

```clojure
(-> (Expression .Any) (Computation .Any))
```

### var

```clojure
(-> .Text SVar)
```

### when

```clojure
(-> (Expression .Any) (Statement .Any) (Statement .Any))
```

### while

```clojure
(-> (Expression .Any) (Statement .Any) (.Maybe (Statement .Any)) Loop)
```

___

# library/lux/target/ruby

## Definitions

### %

```clojure
(-> Expression Expression Computation)
```

### \*

```clojure
(-> Expression Expression Computation)
```

### \+

```clojure
(-> Expression Expression Computation)
```

### \-

```clojure
(-> Expression Expression Computation)
```

### /

```clojure
(-> Expression Expression Computation)
```

### <

```clojure
(-> Expression Expression Computation)
```

### <=

```clojure
(-> Expression Expression Computation)
```

### =

```clojure
(-> Expression Expression Computation)
```

### >

```clojure
(-> Expression Expression Computation)
```

### >=

```clojure
(-> Expression Expression Computation)
```

### ?

```clojure
(-> Expression Expression Expression Computation)
```

### Access

```clojure
... .Type
(Code (Expression' (Computation' (Location' Access'))))
```

### Code

```clojure
... .Type
(All (Code _0)
  (Primitive "library/lux/target/ruby.Code" _0))
```

### Computation

```clojure
... .Type
(Code (Expression' (Computation' .Any)))
```

### Expression

```clojure
... .Type
(Code (Expression' .Any))
```

### GVar

```clojure
... .Type
(Code (Expression' (Computation' (Location' (Var' GVar')))))
```

### IVar

```clojure
... .Type
(Code (Expression' (Computation' (Location' (Var' IVar')))))
```

### LVar

```clojure
... .Type
(Code (Expression' (Computation' (Location' (Var' (LVar' .Any))))))
```

### LVar\*

```clojure
... .Type
(Code (Expression' (Computation' (Location' (Var' (LVar' LVar*'))))))
```

### LVar\*\*

```clojure
... .Type
(Code (Expression' (Computation' (Location' (Var' (LVar' LVar**'))))))
```

### Literal

```clojure
... .Type
(Code (Expression' (Computation' Literal')))
```

### Location

```clojure
... .Type
(Code (Expression' (Computation' (Location' .Any))))
```

### Rescue

```clojure
... .Type
(Record
 [#classes (.List .Text)
  #exception LVar
  #rescue Statement])
```

### SVar

```clojure
... .Type
(Code (Expression' (Computation' (Location' (Var' SVar')))))
```

### Statement

```clojure
... .Type
(Code (Statement' .Any))
```

### Var

```clojure
... .Type
(Code (Expression' (Computation' (Location' (Var' .Any)))))
```

### and

```clojure
(-> Expression Expression Computation)
```

### apply/\*

```clojure
(-> (.List Expression) Expression Computation)
```

### apply/1

```clojure
(-> Expression Expression Computation)
```

### apply/2

```clojure
(-> Expression Expression Expression Computation)
```

### apply/3

```clojure
(-> Expression Expression Expression Expression Computation)
```

### apply\_lambda/\*

```clojure
(-> (.List Expression) Expression Computation)
```

### array

```clojure
(-> (.List Expression) Literal)
```

### array\_range

```clojure
(-> Expression Expression Expression Computation)
```

### begin

```clojure
(-> Statement (.List Rescue) Statement)
```

### bit\_and

```clojure
(-> Expression Expression Computation)
```

### bit\_or

```clojure
(-> Expression Expression Computation)
```

### bit\_shl

```clojure
(-> Expression Expression Computation)
```

### bit\_shr

```clojure
(-> Expression Expression Computation)
```

### bit\_xor

```clojure
(-> Expression Expression Computation)
```

### bool

```clojure
(-> .Bit Literal)
```

### break

```clojure
Statement
```

### case\_insensitivity\_flag

```clojure
GVar
```

### catch

```clojure
(-> Expression Statement Statement)
```

### code

```clojure
(-> (Code .Any) .Text)
```

### code\_equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Code _0)))
```

### code\_hash

```clojure
(All (_ _0)
  (library/lux/abstract/hash.Hash (Code _0)))
```

### command\_line\_arguments

```clojure
LVar
```

### comment

```clojure
(All (_ _0)
  (-> .Text (Code _0) (Code _0)))
```

### cond

```clojure
(-> (.List [Expression Statement]) Statement Statement)
```

### do

```clojure
(-> .Text (.List Expression) Expression Computation)
```

### double\_splat

```clojure
(-> Expression Computation)
```

### exit\_status

```clojure
GVar
```

### float

```clojure
(-> .Frac Literal)
```

### for\_in

```clojure
(-> LVar Expression Statement Statement)
```

### function

```clojure
(-> LVar (.List LVar) Statement Statement)
```

### global

```clojure
(-> .Text GVar)
```

### hash

```clojure
(-> (.List [Expression Expression]) Literal)
```

### if

```clojure
(-> Expression Statement Statement Statement)
```

### input\_record\_separator

```clojure
GVar
```

### instance

```clojure
(-> .Text IVar)
```

### int

```clojure
(-> .Int Literal)
```

### item

```clojure
(-> Expression Expression Access)
```

### lambda

```clojure
(-> (.Maybe LVar) (.List Var) Statement Literal)
```

### last\_line\_number\_read

```clojure
GVar
```

### last\_regexp\_match

```clojure
GVar
```

### last\_string\_matched

```clojure
GVar
```

### last\_string\_read

```clojure
GVar
```

### latest\_error

```clojure
GVar
```

### local

```clojure
(-> .Text LVar)
```

### manual

```clojure
(-> .Text Code)
```

### next

```clojure
Statement
```

### nil

```clojure
Literal
```

### not

```clojure
(-> Expression Computation)
```

### opposite

```clojure
(-> Expression Computation)
```

### or

```clojure
(-> Expression Expression Computation)
```

### output\_record\_separator

```clojure
GVar
```

### pow

```clojure
(-> Expression Expression Computation)
```

### print/1

```clojure
(-> Expression Computation)
```

### print/2

```clojure
(-> Expression Expression Computation)
```

### print/3

```clojure
(-> Expression Expression Expression Computation)
```

### process\_id

```clojure
GVar
```

### raise

```clojure
(-> Expression Computation)
```

### redo

```clojure
Statement
```

### require/1

```clojure
(-> Expression Computation)
```

### return

```clojure
(-> Expression Statement)
```

### script\_name

```clojure
GVar
```

### set

```clojure
(-> (.List Location) Expression Statement)
```

### splat

```clojure
(-> Expression Computation)
```

### statement

```clojure
(-> Expression Statement)
```

### static

```clojure
(-> .Text SVar)
```

### string

```clojure
(-> .Text Literal)
```

### symbol

```clojure
(-> .Text Literal)
```

### the

```clojure
(-> .Text Expression Access)
```

### then

```clojure
(-> Statement Statement Statement)
```

### throw/1

```clojure
(-> Expression Statement)
```

### variadic

```clojure
(-> LVar LVar*)
```

### variadic\_kv

```clojure
(-> LVar LVar**)
```

### when

```clojure
(-> Expression Statement Statement)
```

### while

```clojure
(-> Expression Statement Statement)
```

___

# library/lux/test

Tools for unit & property\-based/generative testing\.

## Definitions

### Assertion

```clojure
... .Type
(library/lux/control/concurrency/async.Async [Tally .Text])
```

An asynchronous operation that yields test results\.

### Seed

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

The seed value used for random testing \(if that feature is used\)\.

### Tally

```clojure
... .Type
(Record
 [#successes .Nat
  #failures .Nat
  #expected_coverage (library/lux/data/collection/set.Set .Symbol)
  #actual_coverage (library/lux/data/collection/set.Set .Symbol)])
```

A record of successes and failures while executing tests\.

### Test

```clojure
... .Type
(library/lux/math/random.Random Assertion)
```

A test that relies on random data generation to thoroughly cover different scenarios\.

### and

```clojure
(-> Test Test Test)
```

Sequencing combinator\.

```clojure
(and left right)
```

### and'

```clojure
(-> Assertion Assertion Assertion)
```

Sequencing combinator \(for assertions\)\.

```clojure
(and' left right)
```

### assertion

```clojure
(-> .Text .Bit Assertion)
```

Check that a condition is \#1, and fail with the given message otherwise\.

```clojure
(assertion message condition)
```

### context

```clojure
(-> .Text Test Test)
```

Adds a contextual description to a test's documentation\.

```clojure
(context description)
```

### cover

```clojure
.Macro
```

Specifies a test as covering one or more definitions\.
Adds to the test tally information to track which definitions have been tested\.

```clojure
(cover [definition/0 definition/1 ,,, definition/N]
       (: Bit
          (some "computation")))
```

### cover'

```clojure
.Macro
```

Specifies a test as covering one or more definitions\.
Adds to the test tally information to track which definitions have been tested\.

```clojure
(cover' [definition/0 definition/1 ,,, definition/N]
        (: Bit
           (some "computation")))
```

### covering

```clojure
.Macro
```

Specifies the module being covered by a test\.
Adds tracking information to the tally to know which exported definitions in the module need to be covered\.

```clojure
(covering documentation/lux/test._
          (: Test
             some_test))
```

### error\_during\_execution

```clojure
(library/lux/control/exception.Exception .Text)
```

### failure

```clojure
(-> .Text Test)
```

A failing test, with a given error message\.

### for

```clojure
.Macro
```

Specifies a context for tests as covering one or more definitions\.
Adds to the test tally information to track which definitions have been tested\.

```clojure
(for [definition/0 definition/1 ,,, definition/N]
     (: Test
        some_test))
```

### in\_parallel

```clojure
(-> (.List Test) Test)
```

Executes multiple tests in parallel \(if the host platform supports it\) to take advantage of multiple cores\.

```clojure
(in_parallel tests)
```

### lifted

```clojure
(-> .Text (library/lux/math/random.Random .Bit) Test)
```

```clojure
(lifted message random)
```

### must\_try\_test\_at\_least\_once

```clojure
(library/lux/control/exception.Exception .Any)
```

### run\!

```clojure
(-> Test (library/lux/control/concurrency/async.Async .Nothing))
```

Executes a test, and exits the program with either a successful or a failing exit code\.
WARNING: This procedure is only meant to be used in \(program: \.\.\.\) forms\.

```clojure
(run! test)
```

### seed

```clojure
(-> Seed Test Test)
```

Execute the given test with a specific seed value\.
This allows you to reproduce a failing test case as many times as you want while debugging\.

```clojure
(seed value test)
```

### test

```clojure
(-> .Text .Bit Test)
```

Check that a condition is \#1, and fail with the given message otherwise\.

```clojure
(test message condition)
```

### times

```clojure
(-> .Nat Test Test)
```

Allows executing a test several times\.
By doing this, it's possible to thoroughly test code with many different scenarios\.
This assumes that random data generation is being used in tests instead of fixed/constant inputs\.

```clojure
(times amount test)
```

___

# library/lux/time

## Definitions

### Clock

```clojure
... .Type
(Record
 [#hour .Nat
  #minute .Nat
  #second .Nat
  #milli_second .Nat])
```

A clock marking the specific hour, minute, second, and milli\-second in a day\.

### Time

```clojure
... .Type
(Primitive "library/lux/time.Time")
```

Time is defined as milliseconds since the start of the day \(00:00:00\.000\)\.

### clock

```clojure
(-> Time Clock)
```

```clojure
(clock time)
```

### codec

```clojure
(library/lux/abstract/codec.Codec .Text Time)
```

Based on ISO 8601\.
For example: 21:14:51\.827

### enum

```clojure
(library/lux/abstract/enum.Enum Time)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Time)
```

### hours

```clojure
.Nat
```

Number of hours in an day\.

### invalid\_hour

```clojure
(library/lux/control/exception.Exception .Nat)
```

### invalid\_minute

```clojure
(library/lux/control/exception.Exception .Nat)
```

### invalid\_second

```clojure
(library/lux/control/exception.Exception .Nat)
```

### midnight

```clojure
Time
```

The instant corresponding to the start of the day: 00:00:00\.000

### milli\_seconds

```clojure
.Nat
```

Number of milli\-seconds in a second\.

### millis

```clojure
(-> Time .Nat)
```

### minutes

```clojure
.Nat
```

Number of minutes in an hour\.

### of\_millis

```clojure
(-> .Nat (library/lux/control/try.Try Time))
```

```clojure
(of_millis milli_seconds)
```

### order

```clojure
(library/lux/abstract/order.Order Time)
```

### parser

```clojure
(library/lux/control/parser/text.Parser Time)
```

### seconds

```clojure
.Nat
```

Number of seconds in a minute\.

### time

```clojure
(-> Clock (library/lux/control/try.Try Time))
```

```clojure
(time clock)
```

### time\_exceeds\_a\_day

```clojure
(library/lux/control/exception.Exception .Nat)
```

___

# library/lux/time/date

## Definitions

### Date

```clojure
... .Type
(Primitive "library/lux/time/date.Date")
```

A date specified as a year/month/day triplet\.

### codec

```clojure
(library/lux/abstract/codec.Codec .Text Date)
```

Based on ISO 8601\.
For example: 2017\-01\-15

### date

```clojure
(-> library/lux/time/year.Year library/lux/time/month.Month .Nat (library/lux/control/try.Try Date))
```

A date, within the allowed limits\.

```clojure
(date year month day_of_month)
```

### day\_of\_month

```clojure
(-> Date .Nat)
```

### days

```clojure
(-> Date .Int)
```

### enum

```clojure
(library/lux/abstract/enum.Enum Date)
```

### epoch

```clojure
Date
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Date)
```

### invalid\_day

```clojure
(library/lux/control/exception.Exception [library/lux/time/year.Year library/lux/time/month.Month .Nat])
```

### invalid\_month

```clojure
(library/lux/control/exception.Exception .Nat)
```

### month

```clojure
(-> Date library/lux/time/month.Month)
```

### of\_days

```clojure
(-> .Int Date)
```

### order

```clojure
(library/lux/abstract/order.Order Date)
```

### parser

```clojure
(library/lux/control/parser/text.Parser Date)
```

### year

```clojure
(-> Date library/lux/time/year.Year)
```

___

# library/lux/time/day

## Definitions

### Day

```clojure
... .Type
(Variant
 {#Sunday .Any}
 {#Monday .Any}
 {#Tuesday .Any}
 {#Wednesday .Any}
 {#Thursday .Any}
 {#Friday .Any}
 {#Saturday .Any})
```

A day of the week\.

### by\_number

```clojure
(-> .Nat (library/lux/control/try.Try Day))
```

### codec

```clojure
(library/lux/abstract/codec.Codec .Text Day)
```

### enum

```clojure
(library/lux/abstract/enum.Enum Day)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Day)
```

### hash

```clojure
(library/lux/abstract/hash.Hash Day)
```

### invalid\_day

```clojure
(library/lux/control/exception.Exception .Nat)
```

### not\_a\_day\_of\_the\_week

```clojure
(library/lux/control/exception.Exception .Text)
```

### number

```clojure
(-> Day .Nat)
```

### order

```clojure
(library/lux/abstract/order.Order Day)
```

### week

```clojure
(.List Day)
```

All the days, ordered by when they come in a week\.

___

# library/lux/time/duration

## Definitions

### Duration

```clojure
... .Type
(Primitive "library/lux/time/duration.Duration")
```

Durations have a resolution of milli\-seconds\.

### codec

```clojure
(library/lux/abstract/codec.Codec .Text Duration)
```

### day

```clojure
Duration
```

### difference

```clojure
(-> Duration Duration Duration)
```

```clojure
(difference from to)
```

### down

```clojure
(-> .Nat Duration Duration)
```

### empty

```clojure
Duration
```

### enum

```clojure
(library/lux/abstract/enum.Enum Duration)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Duration)
```

### framed

```clojure
(-> Duration Duration Duration)
```

### hour

```clojure
Duration
```

### inverse

```clojure
(-> Duration Duration)
```

### leap\_year

```clojure
Duration
```

### merged

```clojure
(-> Duration Duration Duration)
```

### milli\_second

```clojure
Duration
```

### millis

```clojure
(-> Duration .Int)
```

### minute

```clojure
Duration
```

### monoid

```clojure
(library/lux/abstract/monoid.Monoid Duration)
```

### negative?

```clojure
(-> Duration .Bit)
```

### neutral?

```clojure
(-> Duration .Bit)
```

### normal\_year

```clojure
Duration
```

### of\_millis

```clojure
(-> .Int Duration)
```

### order

```clojure
(library/lux/abstract/order.Order Duration)
```

### positive?

```clojure
(-> Duration .Bit)
```

### second

```clojure
Duration
```

### ticks

```clojure
(-> Duration Duration .Int)
```

### up

```clojure
(-> .Nat Duration Duration)
```

### week

```clojure
Duration
```

___

# library/lux/time/instant

## Definitions

### Instant

```clojure
... .Type
(Primitive "library/lux/time/instant.Instant")
```

Instant is defined as milli\-seconds since the epoch\.

### absolute

```clojure
(-> library/lux/time/duration.Duration Instant)
```

```clojure
(absolute offset)
```

### after

```clojure
(-> library/lux/time/duration.Duration Instant Instant)
```

```clojure
(after duration instant)
```

### codec

```clojure
(library/lux/abstract/codec.Codec .Text Instant)
```

Based on ISO 8601\.
For example: 2017\-01\-15T21:14:51\.827Z

### date

```clojure
(-> Instant library/lux/time/date.Date)
```

### day\_of\_week

```clojure
(-> Instant library/lux/time/day.Day)
```

### enum

```clojure
(library/lux/abstract/enum.Enum Instant)
```

### epoch

```clojure
Instant
```

The instant corresponding to 1970\-01\-01T00:00:00Z\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Instant)
```

### millis

```clojure
(-> Instant .Int)
```

### now

```clojure
(library/lux/control/io.IO Instant)
```

Yields the current instant, as measured from the operating\-system's clock\.

### of\_date\_time

```clojure
(-> library/lux/time/date.Date library/lux/time.Time Instant)
```

```clojure
(of_date_time date time)
```

### of\_millis

```clojure
(-> .Int Instant)
```

### order

```clojure
(library/lux/abstract/order.Order Instant)
```

### relative

```clojure
(-> Instant library/lux/time/duration.Duration)
```

```clojure
(relative instant)
```

### span

```clojure
(-> Instant Instant library/lux/time/duration.Duration)
```

```clojure
(span from to)
```

### time

```clojure
(-> Instant library/lux/time.Time)
```

___

# library/lux/time/month

## Definitions

### Month

```clojure
... .Type
(Variant
 {#January .Any}
 {#February .Any}
 {#March .Any}
 {#April .Any}
 {#May .Any}
 {#June .Any}
 {#July .Any}
 {#August .Any}
 {#September .Any}
 {#October .Any}
 {#November .Any}
 {#December .Any})
```

A month of the year\.

### by\_number

```clojure
(-> .Nat (library/lux/control/try.Try Month))
```

### codec

```clojure
(library/lux/abstract/codec.Codec .Text Month)
```

### days

```clojure
(-> Month .Nat)
```

The amount of days of a month\.

```clojure
(days month)
```

### enum

```clojure
(library/lux/abstract/enum.Enum Month)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Month)
```

### hash

```clojure
(library/lux/abstract/hash.Hash Month)
```

### invalid\_month

```clojure
(library/lux/control/exception.Exception .Nat)
```

### leap\_year\_days

```clojure
(-> Month .Nat)
```

The amount of days of a month \(in a leap year\)\.

```clojure
(leap_year_days month)
```

### not\_a\_month\_of\_the\_year

```clojure
(library/lux/control/exception.Exception .Text)
```

### number

```clojure
(-> Month .Nat)
```

### order

```clojure
(library/lux/abstract/order.Order Month)
```

### year

```clojure
(.List Month)
```

All the months, ordered by when they come in a year\.

___

# library/lux/time/year

## Definitions

### Period

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

An amount of years\.

### Year

```clojure
... .Type
(Primitive "library/lux/time/year.Year")
```

A year in the gregorian calendar\.
Both negative \(< 0\) and positive \(> 0\) values are valid, but not 0\.
This is because the first year of the gregorian calendar was year 1\.

### century

```clojure
Period
```

### codec

```clojure
(library/lux/abstract/codec.Codec .Text Year)
```

Based on ISO 8601\.
For example: 2017

### days

```clojure
.Nat
```

The amount of days in a typical year\.

### epoch

```clojure
Year
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Year)
```

### era

```clojure
Period
```

### leap

```clojure
Period
```

### leap?

```clojure
(-> Year .Bit)
```

### leaps

```clojure
(-> Year .Int)
```

The number of leap years in a period of years\.

```clojure
(leaps year)
```

### order

```clojure
(library/lux/abstract/order.Order Year)
```

### parser

```clojure
(library/lux/control/parser/text.Parser Year)
```

### there\_is\_no\_year\_0

```clojure
(library/lux/control/exception.Exception .Any)
```

### value

```clojure
(-> Year .Int)
```

### year

```clojure
(-> .Int (library/lux/control/try.Try Year))
```

A valid year in the gregorian calendar, if possible\.

```clojure
(year value)
```

___

# library/lux/tool/compiler/language/lux/analysis

## Definitions

### %analysis

```clojure
(library/lux/data/text/format.Format Analysis)
```

### Abstraction

```clojure
... .Type
(All (Abstraction _0)
  [(Environment _0) library/lux/tool/compiler/arity.Arity _0])
```

### Analysis

```clojure
... .Type
(Rec Analysis
 (Variant
  {#Primitive Primitive}
  {#Structure (Composite Analysis)}
  {#Reference library/lux/tool/compiler/reference.Reference}
  {#Case Analysis (Match' Analysis)}
  {#Function (Environment Analysis) Analysis}
  {#Apply Analysis Analysis}
  {#Extension (library/lux/tool/compiler/language/lux/phase/extension.Extension Analysis)}))
```

### Application

```clojure
... .Type
(All (Application _0)
  [_0 (.List _0)])
```

### Branch

```clojure
... .Type
(Branch' Analysis)
```

### Branch'

```clojure
... .Type
(All (Branch' _0)
  (Record
   [#when Pattern
    #then _0]))
```

### Bundle

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.Bundle .Lux .Code Analysis)
```

### Composite

```clojure
... .Type
(All (Composite _0)
  (Variant
   {#Variant (Variant _0)}
   {#Tuple (Tuple _0)}))
```

### Environment

```clojure
... .Type
(All (Environment _0)
  (.List _0))
```

### Handler

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.Handler .Lux .Code Analysis)
```

### Match

```clojure
... .Type
(Match' Analysis)
```

### Match'

```clojure
... .Type
(All (Match' _0)
  [(Branch' _0) (.List (Branch' _0))])
```

### Operation

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.Operation .Lux .Code Analysis)
```

### Pattern

```clojure
... .Type
(Rec Pattern
 (Variant
  {#Simple Primitive}
  {#Complex (Composite Pattern)}
  {#Bind library/lux/tool/compiler/reference/variable.Register}))
```

### Phase

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.Phase .Lux .Code Analysis)
```

### Primitive

```clojure
... .Type
(Variant
 {#Unit .Any}
 {#Bit .Bit}
 {#Nat .Nat}
 {#Int .Int}
 {#Rev .Rev}
 {#Frac .Frac}
 {#Text .Text})
```

### State\+

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.State .Lux .Code Analysis)
```

### Tag

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

### Tuple

```clojure
... .Type
(All (Tuple _0)
  (.List _0))
```

### Variant

```clojure
... .Type
(All (Variant _0)
  (Record
   [#lefts .Nat
    #right? .Bit
    #value _0]))
```

### application

```clojure
(-> Analysis (Application Analysis))
```

### apply

```clojure
(-> (Application Analysis) Analysis)
```

### assertion

```clojure
(All (_ _0)
  (-> (library/lux/control/exception.Exception _0) _0 .Bit (Operation .Any)))
```

### bit

```clojure
.Macro
```

### choice

```clojure
(-> .Nat .Nat [.Nat .Bit])
```

### composite\_equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Composite _0))))
```

### composite\_hash

```clojure
(All (_ _0)
  (-> (library/lux/abstract/hash.Hash _0) (library/lux/abstract/hash.Hash (Composite _0))))
```

### constant

```clojure
.Macro
```

### control/case

```clojure
.Macro
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Analysis)
```

### except

```clojure
(All (_ _0)
  (-> (library/lux/control/exception.Exception _0) _0 Operation))
```

### except'

```clojure
(All (_ _0)
  (-> (library/lux/control/exception.Exception _0) _0 (library/lux/tool/compiler/phase.Operation .Lux)))
```

### failure

```clojure
(-> .Text Operation)
```

### failure'

```clojure
(-> .Text (library/lux/tool/compiler/phase.Operation .Lux))
```

### frac

```clojure
.Macro
```

### info

```clojure
(-> library/lux/tool/compiler/version.Version .Text .Info)
```

### install

```clojure
(-> .Lux (Operation .Any))
```

### int

```clojure
.Macro
```

### location

```clojure
(-> .Text .Location)
```

### nat

```clojure
.Macro
```

### no\_op

```clojure
.Macro
```

### pattern/bind

```clojure
.Macro
```

### pattern/bit

```clojure
.Macro
```

### pattern/frac

```clojure
.Macro
```

### pattern/int

```clojure
.Macro
```

### pattern/nat

```clojure
.Macro
```

### pattern/rev

```clojure
.Macro
```

### pattern/text

```clojure
.Macro
```

### pattern/tuple

```clojure
.Macro
```

### pattern/unit

```clojure
.Macro
```

### pattern/variant

```clojure
.Macro
```

### rev

```clojure
.Macro
```

### set\_current\_module

```clojure
(-> .Text (Operation .Any))
```

### set\_location

```clojure
(-> .Location (Operation .Any))
```

### set\_source\_code

```clojure
(-> .Source (Operation .Any))
```

### source

```clojure
(-> .Text .Text .Source)
```

### state

```clojure
(-> .Info .Lux)
```

### tag

```clojure
(-> .Nat .Bit .Nat)
```

### text

```clojure
.Macro
```

### tuple

```clojure
.Macro
```

### unit

```clojure
.Macro
```

### variable

```clojure
.Macro
```

### variable/foreign

```clojure
.Macro
```

### variable/local

```clojure
.Macro
```

### variant

```clojure
.Macro
```

### with\_current\_module

```clojure
(All (_ _0)
  (-> .Text (Operation _0) (Operation _0)))
```

### with\_location

```clojure
(All (_ _0)
  (-> .Location (Operation _0) (Operation _0)))
```

### with\_scope

```clojure
(All (_ _0)
  (-> (Operation _0) (Operation [.Scope _0])))
```

### with\_source\_code

```clojure
(All (_ _0)
  (-> .Source (Operation _0) (Operation _0)))
```

### with\_stack

```clojure
(All (_ _0 _1)
  (-> (library/lux/control/exception.Exception _0) _0 (Operation _1) (Operation _1)))
```

### without\_scopes

```clojure
(All (_ _0)
  (-> (Operation _0) (Operation _0)))
```

___

# library/lux/tool/compiler/language/lux/directive

## Definitions

### Bundle

```clojure
... .Type
(All (Bundle _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.Bundle (State _0 _1 _2) .Code Requirements))
```

### Component

```clojure
... .Type
(All (Component _0 _1)
  (Record
   [#state _0
    #phase _1]))
```

### Handler

```clojure
... .Type
(All (Handler _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.Handler (State _0 _1 _2) .Code Requirements))
```

### Import

```clojure
... .Type
(Record
 [#module library/lux/tool/compiler/meta/archive/descriptor.Module
  #alias .Text])
```

### Operation

```clojure
... .Type
(All (Operation _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.Operation (State _0 _1 _2) .Code Requirements))
```

### Phase

```clojure
... .Type
(All (Phase _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.Phase (State _0 _1 _2) .Code Requirements))
```

### Requirements

```clojure
... .Type
(Record
 [#imports (.List Import)
  #referrals (.List .Code)])
```

### State

```clojure
... .Type
(All (State _0 _1 _2)
  (Record
   [#analysis (Component library/lux/tool/compiler/language/lux/analysis.State+ library/lux/tool/compiler/language/lux/analysis.Phase)
    #synthesis (Component library/lux/tool/compiler/language/lux/synthesis.State+ library/lux/tool/compiler/language/lux/synthesis.Phase)
    #generation (Component (library/lux/tool/compiler/language/lux/generation.State+ _0 _1 _2) (library/lux/tool/compiler/language/lux/generation.Phase _0 _1 _2))]))
```

### State\+

```clojure
... .Type
(All (State+ _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.State (State _0 _1 _2) .Code Requirements))
```

### analysis

```clojure
(All (_ _0 _1 _2)
  (Operation _0 _1 _2 library/lux/tool/compiler/language/lux/analysis.Phase))
```

### generation

```clojure
(All (_ _0 _1 _2)
  (Operation _0 _1 _2 (library/lux/tool/compiler/language/lux/generation.Phase _0 _1 _2)))
```

### lifted\_analysis

```clojure
(All (_ _0 _1 _2 _3)
  (-> (library/lux/tool/compiler/language/lux/analysis.Operation _3) (Operation _0 _1 _2 _3)))
```

### lifted\_generation

```clojure
(All (_ _0 _1 _2 _3)
  (-> (library/lux/tool/compiler/language/lux/generation.Operation _0 _1 _2 _3) (Operation _0 _1 _2 _3)))
```

### lifted\_synthesis

```clojure
(All (_ _0 _1 _2 _3)
  (-> (library/lux/tool/compiler/language/lux/synthesis.Operation _3) (Operation _0 _1 _2 _3)))
```

### merge\_requirements

```clojure
(-> Requirements Requirements Requirements)
```

### no\_requirements

```clojure
Requirements
```

### set\_current\_module

```clojure
(All (_ _0 _1 _2)
  (-> library/lux/tool/compiler/meta/archive/descriptor.Module (Operation _0 _1 _2 .Any)))
```

### synthesis

```clojure
(All (_ _0 _1 _2)
  (Operation _0 _1 _2 library/lux/tool/compiler/language/lux/synthesis.Phase))
```

___

# library/lux/tool/compiler/language/lux/generation

## Definitions

### Buffer

```clojure
... .Type
(All (Buffer _0)
  (library/lux/data/collection/sequence.Sequence [library/lux/tool/compiler/meta/archive/artifact.ID (.Maybe .Text) _0]))
```

### Bundle

```clojure
... .Type
(All (Bundle _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.Bundle (State _0 _1 _2) library/lux/tool/compiler/language/lux/synthesis.Synthesis _1))
```

### Context

```clojure
... .Type
[library/lux/tool/compiler/meta/archive.ID library/lux/tool/compiler/meta/archive/artifact.ID]
```

### Extender

```clojure
... .Type
(All (Extender _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.Extender (State _0 _1 _2) library/lux/tool/compiler/language/lux/synthesis.Synthesis _1))
```

### Handler

```clojure
... .Type
(All (Handler _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.Handler (State _0 _1 _2) library/lux/tool/compiler/language/lux/synthesis.Synthesis _1))
```

### Host

```clojure
... .Type
(All (Host _0 _1)
  (Record
   [evaluate (-> Context _0 (library/lux/control/try.Try .Any))
    execute (-> _1 (library/lux/control/try.Try .Any))
    define (-> Context (.Maybe .Text) _0 (library/lux/control/try.Try [.Text .Any _1]))
    ingest (-> Context library/lux/data/binary.Binary _1)
    re_learn (-> Context (.Maybe .Text) _1 (library/lux/control/try.Try .Any))
    re_load (-> Context (.Maybe .Text) _1 (library/lux/control/try.Try .Any))]))
```

### Operation

```clojure
... .Type
(All (Operation _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.Operation (State _0 _1 _2) library/lux/tool/compiler/language/lux/synthesis.Synthesis _1))
```

### Phase

```clojure
... .Type
(All (Phase _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.Phase (State _0 _1 _2) library/lux/tool/compiler/language/lux/synthesis.Synthesis _1))
```

### State

```clojure
... .Type
(All (State _0 _1 _2)
  (Record
   [#module library/lux/tool/compiler/meta/archive/descriptor.Module
    #anchor (.Maybe _0)
    #host (Host _1 _2)
    #buffer (.Maybe (Buffer _2))
    #registry library/lux/tool/compiler/meta/archive/artifact.Registry
    #counter .Nat
    #context (.Maybe library/lux/tool/compiler/meta/archive/artifact.ID)
    #log (library/lux/data/collection/sequence.Sequence .Text)]))
```

### State\+

```clojure
... .Type
(All (State+ _0 _1 _2)
  (library/lux/tool/compiler/language/lux/phase/extension.State (State _0 _1 _2) library/lux/tool/compiler/language/lux/synthesis.Synthesis _1))
```

### anchor

```clojure
(All (_ _0 _1 _2)
  (Operation _0 _1 _2 _0))
```

### buffer

```clojure
(All (_ _0 _1 _2)
  (Operation _0 _1 _2 (Buffer _2)))
```

### cannot\_interpret

```clojure
(library/lux/control/exception.Exception .Text)
```

### cannot\_overwrite\_output

```clojure
(library/lux/control/exception.Exception library/lux/tool/compiler/meta/archive/artifact.ID)
```

### context

```clojure
(All (_ _0 _1 _2)
  (-> library/lux/tool/compiler/meta/archive.Archive (Operation _0 _1 _2 Context)))
```

### define\!

```clojure
(All (_ _0 _1 _2)
  (-> Context (.Maybe .Text) _1 (Operation _0 _1 _2 [.Text .Any _2])))
```

### empty\_buffer

```clojure
Buffer
```

### enter\_module

```clojure
(All (_ _0 _1 _2)
  (-> library/lux/tool/compiler/meta/archive/descriptor.Module (Operation _0 _1 _2 .Any)))
```

### evaluate\!

```clojure
(All (_ _0 _1 _2)
  (-> Context _1 (Operation _0 _1 _2 .Any)))
```

### execute\!

```clojure
(All (_ _0 _1 _2)
  (-> _2 (Operation _0 _1 _2 .Any)))
```

### get\_registry

```clojure
(All (_ _0 _1 _2)
  (Operation _0 _1 _2 library/lux/tool/compiler/meta/archive/artifact.Registry))
```

### learn

```clojure
(All (_ _0 _1 _2)
  (-> .Text (Operation _0 _1 _2 library/lux/tool/compiler/meta/archive/artifact.ID)))
```

### learn\_analyser

```clojure
(All (_ _0 _1 _2)
  (-> .Text (Operation _0 _1 _2 library/lux/tool/compiler/meta/archive/artifact.ID)))
```

### learn\_custom

```clojure
(All (_ _0 _1 _2)
  (-> .Text (Operation _0 _1 _2 library/lux/tool/compiler/meta/archive/artifact.ID)))
```

### learn\_directive

```clojure
(All (_ _0 _1 _2)
  (-> .Text (Operation _0 _1 _2 library/lux/tool/compiler/meta/archive/artifact.ID)))
```

### learn\_generator

```clojure
(All (_ _0 _1 _2)
  (-> .Text (Operation _0 _1 _2 library/lux/tool/compiler/meta/archive/artifact.ID)))
```

### learn\_synthesizer

```clojure
(All (_ _0 _1 _2)
  (-> .Text (Operation _0 _1 _2 library/lux/tool/compiler/meta/archive/artifact.ID)))
```

### log\!

```clojure
(All (_ _0 _1 _2 _3)
  (-> .Text (Operation _0 _1 _2 .Any)))
```

### module

```clojure
(All (_ _0 _1 _2)
  (Operation _0 _1 _2 library/lux/tool/compiler/meta/archive/descriptor.Module))
```

### module\_id

```clojure
(All (_ _0 _1 _2)
  (-> library/lux/tool/compiler/meta/archive/descriptor.Module library/lux/tool/compiler/meta/archive.Archive (Operation _0 _1 _2 library/lux/tool/compiler/meta/archive.ID)))
```

### next

```clojure
(All (_ _0 _1 _2)
  (Operation _0 _1 _2 .Nat))
```

### no\_active\_buffer

```clojure
(library/lux/control/exception.Exception .Any)
```

### no\_anchor

```clojure
(library/lux/control/exception.Exception .Any)
```

### no\_buffer\_for\_saving\_code

```clojure
(library/lux/control/exception.Exception library/lux/tool/compiler/meta/archive/artifact.ID)
```

### no\_context

```clojure
(library/lux/control/exception.Exception .Any)
```

### remember

```clojure
(All (_ _0 _1 _2)
  (-> library/lux/tool/compiler/meta/archive.Archive .Symbol (Operation _0 _1 _2 Context)))
```

### save\!

```clojure
(All (_ _0 _1 _2)
  (-> library/lux/tool/compiler/meta/archive/artifact.ID (.Maybe .Text) _2 (Operation _0 _1 _2 .Any)))
```

### set\_anchor

```clojure
(All (_ _0 _1 _2)
  (-> _0 (Operation _0 _1 _2 .Any)))
```

### set\_buffer

```clojure
(All (_ _0 _1 _2)
  (-> (Buffer _2) (Operation _0 _1 _2 .Any)))
```

### set\_registry

```clojure
(All (_ _0 _1 _2)
  (-> library/lux/tool/compiler/meta/archive/artifact.Registry (Operation _0 _1 _2 .Any)))
```

### state

```clojure
(All (_ _0 _1 _2)
  (-> (Host _1 _2) library/lux/tool/compiler/meta/archive/descriptor.Module (State _0 _1 _2)))
```

### symbol

```clojure
(All (_ _0 _1 _2)
  (-> .Text (Operation _0 _1 _2 .Text)))
```

### unknown\_definition

```clojure
(library/lux/control/exception.Exception [.Symbol (.List .Text)])
```

### with\_anchor

```clojure
(All (_ _0 _1 _2 _3)
  (-> _0 (Operation _0 _1 _2 _3) (Operation _0 _1 _2 _3)))
```

### with\_buffer

```clojure
(All (_ _0 _1 _2 _3)
  (-> (Operation _0 _1 _2 _3) (Operation _0 _1 _2 _3)))
```

### with\_context

```clojure
(All (_ _0 _1 _2 _3)
  (-> library/lux/tool/compiler/meta/archive/artifact.ID (Operation _0 _1 _2 _3) (Operation _0 _1 _2 _3)))
```

### with\_new\_context

```clojure
(All (_ _0 _1 _2 _3)
  (-> library/lux/tool/compiler/meta/archive.Archive (Operation _0 _1 _2 _3) (Operation _0 _1 _2 [Context _3])))
```

___

# library/lux/tool/compiler/language/lux/synthesis

## Definitions

### \!bind\_top

```clojure
.Macro
```

### \!multi\_pop

```clojure
.Macro
```

### %path

```clojure
(library/lux/data/text/format.Format Path)
```

### %path'

```clojure
(All (_ _0)
  (-> (library/lux/data/text/format.Format _0) (library/lux/data/text/format.Format (Path' _0))))
```

### %synthesis

```clojure
(library/lux/data/text/format.Format Synthesis)
```

### Abstraction

```clojure
... .Type
(Abstraction' Synthesis)
```

### Abstraction'

```clojure
... .Type
(All (Abstraction' _0)
  (Record
   [#environment (library/lux/tool/compiler/language/lux/analysis.Environment _0)
    #arity library/lux/tool/compiler/arity.Arity
    #body _0]))
```

### Access

```clojure
... .Type
(Variant
 {#Side Side}
 {#Member Member})
```

### Apply

```clojure
... .Type
(Apply' Synthesis)
```

### Apply'

```clojure
... .Type
(All (Apply' _0)
  (Record
   [#function _0
    #arguments (.List _0)]))
```

### Branch

```clojure
... .Type
(All (Branch _0)
  (Variant
   {#Let _0 library/lux/tool/compiler/reference/variable.Register _0}
   {#If _0 _0 _0}
   {#Get (.List Member) _0}
   {#Case _0 (Path' _0)}))
```

### Bundle

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.Bundle State library/lux/tool/compiler/language/lux/analysis.Analysis Synthesis)
```

### Control

```clojure
... .Type
(All (Control _0)
  (Variant
   {#Branch (Branch _0)}
   {#Loop (Loop _0)}
   {#Function (Function _0)}))
```

### Fork

```clojure
... .Type
(All (Fork _0 _1)
  [[_0 _1] (.List [_0 _1])])
```

### Function

```clojure
... .Type
(All (Function _0)
  (Variant
   {#Abstraction (Abstraction' _0)}
   {#Apply _0 (.List _0)}))
```

### Handler

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.Handler State library/lux/tool/compiler/language/lux/analysis.Analysis Synthesis)
```

### Loop

```clojure
... .Type
(All (Loop _0)
  (Variant
   {#Scope (Scope _0)}
   {#Again (.List _0)}))
```

### Member

```clojure
... .Type
(.Either .Nat .Nat)
```

### Operation

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.Operation State library/lux/tool/compiler/language/lux/analysis.Analysis Synthesis)
```

### Path

```clojure
... .Type
(Path' Synthesis)
```

### Path'

```clojure
... .Type
(All (Path' _0)
  (Variant
   {#Pop .Any}
   {#Access Access}
   {#Bind library/lux/tool/compiler/reference/variable.Register}
   {#Bit_Fork .Bit (Path' _0) (.Maybe (Path' _0))}
   {#I64_Fork (Fork (.I64 .Any) (Path' _0))}
   {#F64_Fork (Fork .Frac (Path' _0))}
   {#Text_Fork (Fork .Text (Path' _0))}
   {#Alt (Path' _0) (Path' _0)}
   {#Seq (Path' _0) (Path' _0)}
   {#Then _0}))
```

### Phase

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.Phase State library/lux/tool/compiler/language/lux/analysis.Analysis Synthesis)
```

### Primitive

```clojure
... .Type
(Variant
 {#Bit .Bit}
 {#I64 (.I64 .Any)}
 {#F64 .Frac}
 {#Text .Text})
```

### Resolver

```clojure
... .Type
(library/lux/data/collection/dictionary.Dictionary library/lux/tool/compiler/reference/variable.Variable library/lux/tool/compiler/reference/variable.Variable)
```

### Scope

```clojure
... .Type
(All (Scope _0)
  (Record
   [#start library/lux/tool/compiler/reference/variable.Register
    #inits (.List _0)
    #iteration _0]))
```

### Side

```clojure
... .Type
(.Either .Nat .Nat)
```

### State

```clojure
... .Type
(Record
 [#locals .Nat
  #currying? .Bit])
```

### State\+

```clojure
... .Type
(library/lux/tool/compiler/language/lux/phase/extension.State State library/lux/tool/compiler/language/lux/analysis.Analysis Synthesis)
```

### Synthesis

```clojure
... .Type
(Rec Synthesis
 (Variant
  {#Primitive Primitive}
  {#Structure (library/lux/tool/compiler/language/lux/analysis.Composite Synthesis)}
  {#Reference library/lux/tool/compiler/reference.Reference}
  {#Control (Control Synthesis)}
  {#Extension (library/lux/tool/compiler/language/lux/phase/extension.Extension Synthesis)}))
```

### access\_equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Access)
```

### bit

```clojure
.Macro
```

### branch/case

```clojure
.Macro
```

### branch/get

```clojure
.Macro
```

### branch/if

```clojure
.Macro
```

### branch/let

```clojure
.Macro
```

### constant

```clojure
.Macro
```

### currying?

```clojure
(Operation .Bit)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Synthesis)
```

### f64

```clojure
.Macro
```

### fresh\_resolver

```clojure
Resolver
```

### function/abstraction

```clojure
.Macro
```

### function/apply

```clojure
.Macro
```

### hash

```clojure
(library/lux/abstract/hash.Hash Synthesis)
```

### i64

```clojure
.Macro
```

### init

```clojure
State
```

### locals

```clojure
(Operation .Nat)
```

### loop/again

```clojure
.Macro
```

### loop/scope

```clojure
.Macro
```

### member/left

```clojure
.Macro
```

### member/right

```clojure
.Macro
```

### path'\_equivalence

```clojure
(All (_ _0)
  (-> (library/lux/abstract/equivalence.Equivalence _0) (library/lux/abstract/equivalence.Equivalence (Path' _0))))
```

### path/alt

```clojure
.Macro
```

### path/bind

```clojure
.Macro
```

### path/member

```clojure
.Macro
```

### path/pop

```clojure
Path
```

### path/seq

```clojure
.Macro
```

### path/side

```clojure
.Macro
```

### path/then

```clojure
.Macro
```

### path\_equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Path)
```

### primitive\_equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Primitive)
```

### side/left

```clojure
.Macro
```

### side/right

```clojure
.Macro
```

### simple\_left\_side

```clojure
.Macro
```

### simple\_right\_side

```clojure
.Macro
```

### text

```clojure
.Macro
```

### tuple

```clojure
.Macro
```

### unit

```clojure
.Text
```

### variable

```clojure
.Macro
```

### variable/foreign

```clojure
.Macro
```

### variable/local

```clojure
.Macro
```

### variant

```clojure
.Macro
```

### with\_currying?

```clojure
(-> .Bit (All (_ _0) (-> (Operation _0) (Operation _0))))
```

### with\_locals

```clojure
(-> .Nat (All (_ _0) (-> (Operation _0) (Operation _0))))
```

### with\_new\_local

```clojure
(All (_ _0)
  (-> (Operation _0) (Operation _0)))
```

___

# library/lux/tool/compiler/phase

## Definitions

### Operation

```clojure
... .Type
(All (Operation _0 _1)
  (library/lux/control/state.+State library/lux/control/try.Try _0 _1))
```

### Phase

```clojure
... .Type
(All (Phase _0 _1 _2)
  (-> library/lux/tool/compiler/meta/archive.Archive _1 (Operation _0 _2)))
```

### Wrapper

```clojure
... .Type
(All (Wrapper _0 _1 _2)
  (-> (Phase _0 _1 _2) .Any))
```

### assertion

```clojure
.Macro
```

### composite

```clojure
(All (_ _0 _1 _2 _3 _4)
  (-> (Phase _0 _2 _3) (Phase _1 _3 _4) (Phase [_0 _1] _2 _4)))
```

### except

```clojure
(All (_ _0)
  (-> (library/lux/control/exception.Exception _0) _0 Operation))
```

### failure

```clojure
(-> .Text Operation)
```

### get\_state

```clojure
(All (_ _0 _1)
  (Operation _0 _0))
```

### identity

```clojure
(All (_ _0 _1)
  (Phase _0 _1 _1))
```

### lifted

```clojure
(All (_ _0 _1)
  (-> (library/lux/control/try.Try _1) (Operation _0 _1)))
```

### monad

```clojure
(All (_ _0)
  (library/lux/abstract/monad.Monad (Operation _0)))
```

### result

```clojure
(All (_ _0 _1)
  (-> _0 (Operation _0 _1) (library/lux/control/try.Try _1)))
```

### result'

```clojure
(All (_ _0 _1)
  (-> _0 (Operation _0 _1) (library/lux/control/try.Try [_0 _1])))
```

### set\_state

```clojure
(All (_ _0 _1)
  (-> _0 (Operation _0 .Any)))
```

### sub

```clojure
(All (_ _0 _1 _2)
  (-> [(-> _0 _1) (-> _1 _0 _0)] (Operation _1 _2) (Operation _0 _2)))
```

### timed

```clojure
(All (_ _0 _1)
  (-> .Symbol .Text (Operation _0 _1) (Operation _0 _1)))
```

___

# library/lux/type

Basic functionality for working with types\.

## Definitions

### :as

```clojure
.Macro
```

Casts a value to a specific type\.
The specified type can depend on type variables of the original type of the value\.
NOTE: Careless use of type\-casts is an easy way to introduce bugs\. USE WITH CAUTION\.

```clojure
(: (Bar Bit Nat Text)
   (:as [a b c]
        (Foo a [b c])
        (Bar a b c)
        (: (Foo Bit [Nat Text])
           (foo expression))))
```

### :by\_example

```clojure
.Macro
```

Constructs a type that shares type\-variables with an expression of some other type\.

```clojure
(: Type
   (:by_example [a b c]
                (Foo a [b c])
                (: (Foo Bit [Nat Text])
                   (foo expression))

                (Bar a b c)))

... =>

(.type (Bar Bit Nat Text))
```

### :log\!

```clojure
.Macro
```

Logs to the console/terminal the type of an expression\.

```clojure
(:log! (: Foo (foo expression)))

... =>

... Expression: (foo expression)

...       Type: Foo

(foo expression)
```

### :sharing

```clojure
.Macro
```

Allows specifing the type of an expression as sharing type\-variables with the type of another expression\.

```clojure
(: (Bar Bit Nat Text)
   (:sharing [a b c]
             (Foo a [b c])
             (: (Foo Bit [Nat Text])
                (foo expression))

             (Bar a b c)
             (bar expression)))
```

### anonymous

```clojure
(-> .Type .Type)
```

A type without any names covering it\.

```clojure
(anonymous type)
```

### application

```clojure
(-> (.List .Type) .Type .Type)
```

An un\-evaluated type application, with the given quantified type, and parameters\.

```clojure
(application params quant)
```

### applied

```clojure
(-> (.List .Type) .Type (.Maybe .Type))
```

To the extend possible, applies a quantified type to the given parameters\.

```clojure
(applied params func)
```

### array

```clojure
(-> .Nat .Type .Type)
```

An array type, with the given level of nesting/depth, and the given element type\.

```clojure
(array depth element_type)
```

### array?

```clojure
(-> .Type .Bit)
```

Is a type an array type?

### code

```clojure
(-> .Type .Code)
```

A representation of a type as code\.
The code is such that evaluating it would yield the type value\.

```clojure
(code type)
```

### de\_aliased

```clojure
(-> .Type .Type)
```

A \(potentially named\) type that does not have its name shadowed by other names\.

```clojure
(de_aliased type)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Type)
```

### ex\_q

```clojure
(-> .Nat .Type .Type)
```

A quantified type, with the given number of parameters, and body\.

### flat\_application

```clojure
(-> .Type [.Type (.List .Type)])
```

The quantified type, and its parameters, for a type\-application\.

```clojure
(flat_application type)
```

### flat\_array

```clojure
(-> .Type [.Nat .Type])
```

The level of nesting/depth and element type for an array type\.

```clojure
(flat_array type)
```

### flat\_ex\_q

```clojure
(-> .Type [.Nat .Type])
```

The number of parameters, and the body, of a quantified type\.

### flat\_function

```clojure
(-> .Type [(.List .Type) .Type])
```

The input, and the output of a function type\.

```clojure
(flat_function type)
```

### flat\_tuple

```clojure
(-> .Type (.List .Type))
```

The members of a composite type\.

### flat\_univ\_q

```clojure
(-> .Type [.Nat .Type])
```

The number of parameters, and the body, of a quantified type\.

### flat\_variant

```clojure
(-> .Type (.List .Type))
```

The members of a composite type\.

### format

```clojure
(-> .Type .Text)
```

A \(readable\) textual representable of a type\.

```clojure
(format type)
```

### function

```clojure
(-> (.List .Type) .Type .Type)
```

A function type, with the given inputs and output\.

```clojure
(function inputs output)
```

### quantified?

```clojure
(-> .Type .Bit)
```

Only yields \#1 for universally or existentially quantified types\.

```clojure
(quantified? type)
```

### tuple

```clojure
(-> (.List .Type) .Type)
```

A composite type, constituted by the given member types\.

### univ\_q

```clojure
(-> .Nat .Type .Type)
```

A quantified type, with the given number of parameters, and body\.

### variant

```clojure
(-> (.List .Type) .Type)
```

A composite type, constituted by the given member types\.

___

# library/lux/type/abstract

## Definitions

### :abstraction

```clojure
.Macro
```

Type\-casting macro for abstract/nominal types\.

```clojure
(|> value
    (: Representation)
    :abstraction
    (: Abstraction)
    :representation
    (: Representation))
```

### :representation

```clojure
.Macro
```

Type\-casting macro for abstract/nominal types\.

```clojure
(|> value
    (: Representation)
    :abstraction
    (: Abstraction)
    :representation
    (: Representation))
```

### :transmutation

```clojure
.Macro
```

Transmutes an abstract/nominal type's phantom types\.

```clojure
(abstract: (JavaScript a)
  Text

  (abstract: Expression Any)
  (abstract: Statement Any)

  (def: (statement expression)
    (-> (JavaScript Expression) (JavaScript Statement))
    (:transmutation expression))

  (def: (statement' expression)
    (-> (JavaScript Expression) (JavaScript Statement))
    (:transmutation JavaScript expression)))
```

### Frame

```clojure
... .Type
(Record
 [#name .Text
  #type_vars (.List .Code)
  #abstraction .Code
  #representation .Code])
```

Meta\-data about an abstract/nominal type in a stack of them\.

### ^:representation

```clojure
.Macro
```

Pattern\-matching macro to easily extract a representation\.

```clojure
(def: (computation abstraction)
  (All (_ a) (-> (Abstract a) ???))
  (let [(^:representation value) abstraction]
    (foo (bar (baz value)))))
```

### abstract:

```clojure
.Macro
```

Define abstract/nominal types which hide their representation details\.
You can convert between the abstraction and its representation selectively to access the value, while hiding it from others\.

```clojure
(abstract: String
  Text

  (def: (string value)
    (-> Text String)
    (:abstraction value))

  (def: (text value)
    (-> String Text)
    (:representation value)))

................................................................
................................................................

... Type-parameters are optional.

(abstract: (Duplicate a)
  [a a]

  (def: (duplicate value)
    (All (_ a) (-> a (Duplicate a)))
    (:abstraction [value value])))

................................................................
................................................................

... Definitions can be nested.

(abstract: (Single a)
  a

  (def: (single value)
    (All (_ a) (-> a (Single a)))
    (:abstraction value))

  (abstract: (Double a)
    [a a]

    (def: (double value)
      (All (_ a) (-> a (Double a)))
      (:abstraction [value value]))

    (def: (single' value)
      (All (_ a) (-> a (Single a)))
      (:abstraction Single [value value]))

    (let [value 123]
      (same? value
             (|> value
                 single'
                 (:representation Single)
                 double
                 :representation)))))

................................................................
................................................................

... Type-parameters do not necessarily have to be used in the representation type.

... If they are not used, they become phantom types and can be used to customize types without changing the representation.

(abstract: (JavaScript a)
  Text

  (abstract: Expression Any)
  (abstract: Statement Any)

  (def: (+ x y)
    (-> (JavaScript Expression) (JavaScript Expression) (JavaScript Expression))
    (:abstraction
     (format "(" (:representation x) "+" (:representation y) ")")))

  (def: (while test body)
    (-> (JavaScript Expression) (JavaScript Statement) (JavaScript Statement))
    (:abstraction
     (format "while(" (:representation test) ") {"
             (:representation body)
             "}"))))
```

### current

```clojure
(.Meta Frame)
```

The currently\-being\-defined abstract/nominal type\.

### no\_active\_frames

```clojure
(library/lux/control/exception.Exception .Any)
```

### specific

```clojure
(-> .Text (.Meta Frame))
```

A specific abstract/nominal type still being defined somewhere in the scope\.

```clojure
(specific name)
```

___

# library/lux/type/check

Type\-checking functionality\.

## Definitions

### \(Check it\)

```clojure
... .Type
(-> .Type_Context (library/lux/control/try.Try [.Type_Context it]))
```

A type\-checking computation which may fail or yield a value\.

### Var

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

The ID for a type\-variable in a type\-checking context\.

### apply

```clojure
(library/lux/abstract/apply.Apply Check)
```

### assertion

```clojure
(-> .Text .Bit (Check .Any))
```

```clojure
(assertion message test)
```

### bind

```clojure
(-> .Type Var (Check .Any))
```

Attemmpts to buy a type\-variable\.
Fails if the variable has been bound already\.

```clojure
(bind type id)
```

### bound?

```clojure
(-> Var (Check .Bit))
```

### cannot\_rebind\_var

```clojure
(library/lux/control/exception.Exception [.Nat .Type .Type])
```

### check

```clojure
(-> .Type .Type (Check .Any))
```

Type\-check to ensure that the 'expected' type subsumes the 'actual' type\.

```clojure
(check expected actual)
```

### clean

```clojure
(-> .Type (Check .Type))
```

Resolves every bound type\-variable to yield a new type that is as resolved as possible\.

```clojure
(clean inputT)
```

### context

```clojure
(Check .Type_Context)
```

The current state of the type\-checking context\.

### except

```clojure
(All (_ _0 _1)
  (-> (library/lux/control/exception.Exception _0) _0 (Check _1)))
```

```clojure
(except exception message)
```

### existential

```clojure
(Check [.Nat .Type])
```

A brand\-new existential type\.

### failure

```clojure
(All (_ _0)
  (-> .Text (Check _0)))
```

```clojure
(failure message)
```

### fresh\_context

```clojure
.Type_Context
```

An empty/un\-used type\-checking context\.

### functor

```clojure
(library/lux/abstract/functor.Functor Check)
```

### invalid\_type\_application

```clojure
(library/lux/control/exception.Exception [.Type .Type])
```

### monad

```clojure
(library/lux/abstract/monad.Monad Check)
```

### peek

```clojure
(-> Var (Check (.Maybe .Type)))
```

### read

```clojure
(-> Var (Check .Type))
```

### result

```clojure
(All (_ _0)
  (-> .Type_Context (Check _0) (library/lux/control/try.Try _0)))
```

```clojure
(result context proc)
```

### subsumes?

```clojure
(-> .Type .Type .Bit)
```

A simple type\-checking function that just returns a yes/no answer\.

```clojure
(subsumes? expected actual)
```

### type\_check\_failed

```clojure
(library/lux/control/exception.Exception [.Type .Type])
```

### unbound\_type\_var

```clojure
(library/lux/control/exception.Exception .Nat)
```

### unknown\_type\_var

```clojure
(library/lux/control/exception.Exception .Nat)
```

### var

```clojure
(Check [Var .Type])
```

A brand\-new \(unbound\) type\-variable\.

___

# library/lux/type/dynamic

## Definitions

### :dynamic

```clojure
.Macro
```

```clojure
(: Dynamic
   (:dynamic 123))
```

### :static

```clojure
.Macro
```

```clojure
(: (try.Try Nat)
   (:static Nat (:dynamic 123)))
```

### Dynamic

```clojure
... .Type
(Primitive "library/lux/type/dynamic.Dynamic")
```

A value coupled with its type, so it can be checked later\.

### format

```clojure
(-> Dynamic (library/lux/control/try.Try .Text))
```

### wrong\_type

```clojure
(library/lux/control/exception.Exception [.Type .Type])
```

___

# library/lux/type/implicit

## Definitions

### \#\#

```clojure
.Macro
```

Automatic implementation selection \(for type\-class style polymorphism\)\.
This feature layers type\-class style polymorphism on top of Lux's signatures and implementations\.
When calling a polymorphic function, or using a polymorphic constant,
this macro will check the types of the arguments, and the expected type for the whole expression
and it will search in the local scope, the module's scope and the imports' scope
in order to find suitable implementations to satisfy those requirements\.
If a single alternative is found, that one will be used automatically\.
If no alternative is found, or if more than one alternative is found \(ambiguity\)
a compile\-time error will be raised, to alert the user\.

Caveat emptor: You need to make sure to import the module of any implementation you want to use\.
Otherwise, this macro will not find it\.

```clojure
... Nat equivalence

(# number.equivalence = x y)

(## = x y)

................................................................
................................................................

... Can optionally add the prefix of the module where the signature was defined.

(## equivalence.= x y)

................................................................
................................................................

... (List Nat) equivalence

(## =
    (list.indices 10)
    (list.indices 10))

................................................................
................................................................

... (Functor List) each

(## each ++ (list.indices 10))
```

### implicit:

```clojure
.Macro
```

Establish local definitions for implementations that will be prioritized over foreign definitions\.

```clojure
(implicit: [n.multiplication])

(n.= (# n.multiplication composite left right)
     (## composite left right))
```

### with

```clojure
.Macro
```

Establish lexical bindings for implementations that will be prioritized over non\-lexically\-bound implementations\.

```clojure
(with [n.addition]
  (n.= (# n.addition composite left right)
       (## composite left right)))
```

## Missing documentation

1. `` compatible_type? ``

___

# library/lux/type/poly

## Definitions

### code

```clojure
(-> library/lux/control/parser/type.Env .Type .Code)
```

```clojure
(code env type)
```

### poly:

```clojure
.Macro
```

___

# library/lux/type/quotient

## Definitions

### \(Class value label\)

```clojure
... .Type
(All (_ _0)
  (Primitive "library/lux/type/quotient.Class" value label _0))
```

The class knows how to classify/label values that are meant to be equivalent to one another\.

### \(Quotient value label\)

```clojure
... .Type
(All (_ _0)
  (Primitive "library/lux/type/quotient.Quotient" value label _0))
```

A quotient value has been labeled with a class\.
All equivalent values will belong to the same class\.
This means all equivalent values possess the same label\.

### class

```clojure
(All (_ _0 _1)
  (Ex (_ _2)
    (-> (-> _0 _1) (Class _0 _1 _2))))
```

### equivalence

```clojure
(All (_ _0 _1 _2)
  (-> (library/lux/abstract/equivalence.Equivalence _1) (library/lux/abstract/equivalence.Equivalence (Quotient _0 _1 _2))))
```

### label

```clojure
(All (_ _0 _1 _2)
  (-> (Quotient _0 _1 _2) _1))
```

### quotient

```clojure
(All (_ _0 _1 _2)
  (-> (Class _0 _1 _2) _0 (Quotient _0 _1 _2)))
```

```clojure
(quotient class value)
```

### type

```clojure
.Macro
```

The Quotient type associated with a Class type\.

```clojure
(def: even
  (class even?))

(def: Even
  Type
  (type even))

(: Even
   (quotient even 123))
```

### value

```clojure
(All (_ _0 _1 _2)
  (-> (Quotient _0 _1 _2) _0))
```

___

# library/lux/type/refinement

## Definitions

### \(Refined it\)

```clojure
... .Type
(All (_ _0)
  (Primitive "library/lux/type/refinement.Refined" it _0))
```

A refined version of another type, using a predicate to select valid instances\.

### \(Refiner it\)

```clojure
... .Type
(All (_ _0)
  (-> it (.Maybe (Refined it _0))))
```

A selection mechanism for refined instances of a type\.

### lifted

```clojure
(All (_ _0 _1)
  (-> (-> _0 _0) (Refined _0 _1) (.Maybe (Refined _0 _1))))
```

Yields a function that can work on refined values\.
Respects the constraints of the refinement\.

```clojure
(lifted transform)
```

### only

```clojure
(All (_ _0 _1)
  (-> (Refiner _0 _1) (.List _0) (.List (Refined _0 _1))))
```

```clojure
(only refiner values)
```

### partition

```clojure
(All (_ _0 _1)
  (-> (Refiner _0 _1) (.List _0) [(.List (Refined _0 _1)) (.List _0)]))
```

Separates refined values from the un\-refined ones\.

```clojure
(partition refiner values)
```

### predicate

```clojure
(All (_ _0 _1)
  (-> (Refined _0 _1) (library/lux/abstract/predicate.Predicate _0)))
```

### refiner

```clojure
(All (_ _0)
  (Ex (_ _1)
    (-> (library/lux/abstract/predicate.Predicate _0) (Refiner _0 _1))))
```

```clojure
(refiner predicate)
```

### type

```clojure
.Macro
```

The Refined type associated with a Refiner type\.

```clojure
(def: even
  (refiner even?))

(def: Even
  Type
  (type even))

(: (Maybe Even)
   (even 123))
```

### value

```clojure
(All (_ _0 _1)
  (-> (Refined _0 _1) _0))
```

___

# library/lux/type/resource

## Definitions

### \(Affine monad permissions value\)

```clojure
... .Type
(All (_ _0)
  (Procedure monad _0 [permissions _0] value))
```

A procedure which expands the number of available resources\.

### Commutative

```clojure
... .Type
(Primitive "library/lux/type/resource.Commutative")
```

The mode of keys which CAN be swapped, and for whom order of release/consumption DOES NOT matters\.

### \(Key mode key\)

```clojure
... .Type
(Primitive "library/lux/type/resource.Key" mode key)
```

The access right for a resource\.
Without the key for a resource existing somewhere among the available ambient rights, one cannot use a resource\.

### \(Linear monad value\)

```clojure
... .Type
(All (_ _0)
  (Procedure monad _0 _0 value))
```

A procedure that is constant with regards to resource access rights\.
This means no additional resources will be available after the computation is over\.
This also means no previously available resources will have been consumed\.

### Ordered

```clojure
... .Type
(Primitive "library/lux/type/resource.Ordered")
```

The mode of keys which CANNOT be swapped, and for whom order of release/consumption matters\.

### \(Procedure monad input output value\)

```clojure
... .Type
(-> input (monad [output value]))
```

A computation that takes a sequence of resource access rights as inputs and yields a different sequence as outputs\.
A procedure yields a result value\.
A procedure can make use of monadic effects\.

### \(Relevant monad permissions value\)

```clojure
... .Type
(All (_ _0)
  (Procedure monad [permissions _0] _0 value))
```

A procedure which reduces the number of available resources\.

### \(Res key value\)

```clojure
... .Type
(Primitive "library/lux/type/resource.Res" key value)
```

A resource locked by a key\.
The 'key' represents the right to access/consume a resource\.

### amount\_cannot\_be\_zero

```clojure
(library/lux/control/exception.Exception .Any)
```

### commutative

```clojure
(All (_ _0 _1)
  (Ex (_ _2)
    (-> (library/lux/abstract/monad.Monad _0) _1 (Affine _0 (Key Commutative _2) (Res _2 _1)))))
```

Makes a value into a resource and adds the key/access\-right to it to the ambient keyring for future use\.

### exchange

```clojure
.Macro
```

A function that can exchange the keys for resource, so long as they are commutative\.
This keys will be placed at the front of the keyring in the order they are specified\.
The specific keys must be specified based of their index into the current keyring\.

```clojure
(do (monad !)
  [res|left (commutative ! pre)
   res|right (commutative ! post)
   _ ((exchange [1 0]) !)
   left (read ! res|left)
   right (read ! res|right)]
  (in (format left right)))
```

### group

```clojure
.Macro
```

Group/un\-group keys in the keyring into/out\-of tuples\.

```clojure
(do (monad !)
  [res|left (commutative ! pre)
   res|right (commutative ! post)
   _ ((group 2) !)
   _ ((un_group 2) !)
   right (read ! res|right)
   left (read ! res|left)]
  (in (format left right)))
```

### index\_cannot\_be\_repeated

```clojure
(library/lux/control/exception.Exception .Nat)
```

### lifted

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (_0 _1) (Linear _0 _1)))
```

```clojure
(lifted monad procedure)
```

### monad

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (library/lux/abstract/monad/indexed.IxMonad (Procedure _0))))
```

### ordered

```clojure
(All (_ _0 _1)
  (Ex (_ _2)
    (-> (library/lux/abstract/monad.Monad _0) _1 (Affine _0 (Key Ordered _2) (Res _2 _1)))))
```

Makes a value into a resource and adds the key/access\-right to it to the ambient keyring for future use\.

### read

```clojure
(All (_ _0 _1 _2 _3)
  (-> (library/lux/abstract/monad.Monad _0) (Res _2 _1) (Relevant _0 (Key _3 _2) _1)))
```

Access the value of a resource, so long as its key is available\.

```clojure
(read monad resource)
```

### run\!

```clojure
(All (_ _0 _1)
  (-> (library/lux/abstract/monad.Monad _0) (Linear _0 _1) (_0 _1)))
```

```clojure
(run! monad procedure)
```

### un\_group

```clojure
.Macro
```

Group/un\-group keys in the keyring into/out\-of tuples\.

```clojure
(do (monad !)
  [res|left (commutative ! pre)
   res|right (commutative ! post)
   _ ((group 2) !)
   _ ((un_group 2) !)
   right (read ! res|right)
   left (read ! res|left)]
  (in (format left right)))
```

___

# library/lux/type/unit

## Definitions

### \*

```clojure
(All (_ _0 _1)
  (-> (Qty _0) (Qty _1) (Qty [_0 _1])))
```

### \+

```clojure
(All (_ _0)
  (-> (Qty _0) (Qty _0) (Qty _0)))
```

### \-

```clojure
(All (_ _0)
  (-> (Qty _0) (Qty _0) (Qty _0)))
```

### /

```clojure
(All (_ _0 _1)
  (-> (Qty _0) (Qty [_0 _1]) (Qty _1)))
```

### Giga

```clojure
... .Type
(All (Giga _0)
  (Primitive "library/lux/type/unit.Giga" _0))
```

### Gram

```clojure
... .Type
(Primitive "library/lux/type/unit.Gram")
```

### Kilo

```clojure
... .Type
(All (Kilo _0)
  (Primitive "library/lux/type/unit.Kilo" _0))
```

### Litre

```clojure
... .Type
(Primitive "library/lux/type/unit.Litre")
```

### Mega

```clojure
... .Type
(All (Mega _0)
  (Primitive "library/lux/type/unit.Mega" _0))
```

### Meter

```clojure
... .Type
(Primitive "library/lux/type/unit.Meter")
```

### Micro

```clojure
... .Type
(All (Micro _0)
  (Primitive "library/lux/type/unit.Micro" _0))
```

### Milli

```clojure
... .Type
(All (Milli _0)
  (Primitive "library/lux/type/unit.Milli" _0))
```

### Nano

```clojure
... .Type
(All (Nano _0)
  (Primitive "library/lux/type/unit.Nano" _0))
```

### Pure

```clojure
... .Type
(Qty .Any)
```

A pure, unit\-less quantity\.

### \(Qty unit\)

```clojure
... .Type
(Primitive "library/lux/type/unit.Qty" unit)
```

A quantity with an associated unit of measurement\.

### \(Scale scale\)

```clojure
... .Type
(Record
 [scale (All (_ _0) (-> (Qty _0) (Qty (scale _0))))
  de_scale (All (_ _0) (-> (Qty (scale _0)) (Qty _0)))
  ratio library/lux/math/number/ratio.Ratio])
```

A scale of magnitude\.

### Second

```clojure
... .Type
(Primitive "library/lux/type/unit.Second")
```

### \(Unit unit\)

```clojure
... .Type
(Record
 [in (-> .Int (Qty unit))
  out (-> (Qty unit) .Int)])
```

A unit of measurement, to qualify numbers with\.

### enum

```clojure
(All (_ _0)
  (library/lux/abstract/enum.Enum (Qty _0)))
```

### equivalence

```clojure
(All (_ _0)
  (library/lux/abstract/equivalence.Equivalence (Qty _0)))
```

### giga

```clojure
(Scale Giga)
```

The 'giga' scale, from 1 to 1000000000\.

### gram

```clojure
(Unit Gram)
```

The 'gram' unit of meaurement\.

### kilo

```clojure
(Scale Kilo)
```

The 'kilo' scale, from 1 to 1000\.

### litre

```clojure
(Unit Litre)
```

The 'litre' unit of meaurement\.

### mega

```clojure
(Scale Mega)
```

The 'mega' scale, from 1 to 1000000\.

### meter

```clojure
(Unit Meter)
```

The 'meter' unit of meaurement\.

### micro

```clojure
(Scale Micro)
```

The 'micro' scale, from 1000000 to 1\.

### milli

```clojure
(Scale Milli)
```

The 'milli' scale, from 1000 to 1\.

### nano

```clojure
(Scale Nano)
```

The 'nano' scale, from 1000000000 to 1\.

### number

```clojure
(-> Pure .Int)
```

### order

```clojure
(All (_ _0)
  (library/lux/abstract/order.Order (Qty _0)))
```

### pure

```clojure
(-> .Int Pure)
```

### re\_scaled

```clojure
(All (_ _0 _1 _2)
  (-> (Scale _0) (Scale _1) (Qty (_0 _2)) (Qty (_1 _2))))
```

```clojure
(re_scaled from to quantity)
```

### scale:

```clojure
.Macro
```

Define a scale of magnitude\.

```clojure
(scale: .public Bajillion bajillion
  [1 1234567890])
```

### second

```clojure
(Unit Second)
```

The 'second' unit of meaurement\.

### unit:

```clojure
.Macro
```

Define a unit of measurement\.
Both the name of the type, and the name of the Unit implementation must be specified\.

```clojure
(unit: .public Feet feet)
```

___

# library/lux/type/variance

## Definitions

### \(Co it\)

```clojure
... .Type
(-> .Any it)
```

A constraint for covariant types\.

### \(Contra it\)

```clojure
... .Type
(-> it .Any)
```

A constraint for contravariant types\.

### \(In it\)

```clojure
... .Type
(-> it it)
```

A constraint for invariant types\.

___

# library/lux/world/console

## Definitions

### \(Console \!\)

```clojure
... .Type
(Record
 [read (-> .Any (! (library/lux/control/try.Try library/lux/data/text.Char)))
  read_line (-> .Any (! (library/lux/control/try.Try .Text)))
  write (-> .Text (! (library/lux/control/try.Try .Any)))
  close (-> .Any (! (library/lux/control/try.Try .Any)))])
```

An interface to console/terminal I/O\.

### \(Mock s\)

```clojure
... .Type
(Record
 [on_read (-> s (library/lux/control/try.Try [s library/lux/data/text.Char]))
  on_read_line (-> s (library/lux/control/try.Try [s .Text]))
  on_write (-> .Text s (library/lux/control/try.Try s))
  on_close (-> s (library/lux/control/try.Try s))])
```

A mock/simulation of a console\.
Useful for testing\.

### async

```clojure
(-> (Console library/lux/control/io.IO) (Console library/lux/control/concurrency/async.Async))
```

### mock

```clojure
(All (_ _0)
  (-> (Mock _0) _0 (Console library/lux/control/io.IO)))
```

```clojure
(mock mock init)
```

### write\_line

```clojure
(All (_ _0)
  (-> .Text (Console _0) (_0 (library/lux/control/try.Try .Any))))
```

Writes the message on the console and appends a new\-line/line\-feed at the end\.

```clojure
(write_line message console)
```

___

# library/lux/world/file

## Definitions

### Path

```clojure
... .Type
(Primitive "#Text")
```

A path to a file or a directory in a file\-system\.

### \(System \!\)

```clojure
... .Type
(Record
 [separator .Text
  file? (-> Path (! .Bit))
  directory? (-> Path (! .Bit))
  make_directory (-> Path (! (library/lux/control/try.Try .Any)))
  directory_files (-> Path (! (library/lux/control/try.Try (.List Path))))
  sub_directories (-> Path (! (library/lux/control/try.Try (.List Path))))
  file_size (-> Path (! (library/lux/control/try.Try .Nat)))
  last_modified (-> Path (! (library/lux/control/try.Try library/lux/time/instant.Instant)))
  can_execute? (-> Path (! (library/lux/control/try.Try .Bit)))
  read (-> Path (! (library/lux/control/try.Try library/lux/data/binary.Binary)))
  delete (-> Path (! (library/lux/control/try.Try .Any)))
  modify (-> library/lux/time/instant.Instant Path (! (library/lux/control/try.Try .Any)))
  write (-> library/lux/data/binary.Binary Path (! (library/lux/control/try.Try .Any)))
  append (-> library/lux/data/binary.Binary Path (! (library/lux/control/try.Try .Any)))
  move (-> Path Path (! (library/lux/control/try.Try .Any)))])
```

An interface to a file\-system\.

### async

```clojure
(-> (System library/lux/control/io.IO) (System library/lux/control/concurrency/async.Async))
```

### cannot\_delete

```clojure
(library/lux/control/exception.Exception Path)
```

### cannot\_find\_directory

```clojure
(library/lux/control/exception.Exception Path)
```

### cannot\_find\_file

```clojure
(library/lux/control/exception.Exception Path)
```

### cannot\_make\_directory

```clojure
(library/lux/control/exception.Exception Path)
```

### cannot\_make\_file

```clojure
(library/lux/control/exception.Exception Path)
```

### cannot\_read\_all\_data

```clojure
(library/lux/control/exception.Exception Path)
```

### default

```clojure
(System library/lux/control/concurrency/async.Async)
```

### exists?

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (System _0) Path (_0 .Bit)))
```

Checks if either a file or a directory exists at the given path\.

```clojure
(exists? monad fs path)
```

### make\_directories

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (System _0) Path (_0 (library/lux/control/try.Try .Any))))
```

Creates the directory specified by the given path\.
Also, creates every super\-directory necessary to make the given path valid\.

```clojure
(make_directories monad fs path)
```

### make\_file

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (System _0) library/lux/data/binary.Binary Path (_0 (library/lux/control/try.Try .Any))))
```

Creates a new file with the given content if\-and\-only\-if the file does not already exist\.

```clojure
(make_file monad fs content path)
```

### mock

```clojure
(-> .Text (System library/lux/control/concurrency/async.Async))
```

A purely in\-memory simulation of a file\-system\.
Useful for testing\.

```clojure
(mock separator)
```

### name

```clojure
(All (_ _0)
  (-> (System _0) Path .Text))
```

The un\-nested name of a file/directory\.

```clojure
(name fs path)
```

### parent

```clojure
(All (_ _0)
  (-> (System _0) Path (.Maybe Path)))
```

If a path represents a nested file/directory, extracts its parent directory\.

```clojure
(parent fs path)
```

### rooted

```clojure
(All (_ _0)
  (-> (System _0) Path .Text Path))
```

A nested path for a file/directory, given a root/parent path and a file/directory name within it\.

```clojure
(rooted fs parent child)
```

___

# library/lux/world/file/watch

## Definitions

### Concern

```clojure
... .Type
(Primitive "library/lux/world/file/watch.Concern")
```

A particular concern to watch\-out for\.

### \(Watcher \!\)

```clojure
... .Type
(Record
 [start (-> Concern library/lux/world/file.Path (! (library/lux/control/try.Try .Any)))
  concern (-> library/lux/world/file.Path (! (library/lux/control/try.Try Concern)))
  stop (-> library/lux/world/file.Path (! (library/lux/control/try.Try Concern)))
  poll (-> .Any (! (library/lux/control/try.Try (.List [Concern library/lux/world/file.Path]))))])
```

Machinery for watching a file\-system for changes to files and directories\.

### all

```clojure
Concern
```

### also

```clojure
(-> Concern Concern Concern)
```

```clojure
(also left right)
```

### cannot\_poll\_a\_non\_existent\_directory

```clojure
(library/lux/control/exception.Exception library/lux/world/file.Path)
```

### creation

```clojure
Concern
```

### creation?

```clojure
(library/lux/abstract/predicate.Predicate Concern)
```

### deletion

```clojure
Concern
```

### deletion?

```clojure
(library/lux/abstract/predicate.Predicate Concern)
```

### mock

```clojure
(-> .Text [(library/lux/world/file.System library/lux/control/concurrency/async.Async) (Watcher library/lux/control/concurrency/async.Async)])
```

A fake/emulated watcher\.
Must be given a path separator for the file\-system\.

```clojure
(mock separator)
```

### modification

```clojure
Concern
```

### modification?

```clojure
(library/lux/abstract/predicate.Predicate Concern)
```

### not\_being\_watched

```clojure
(library/lux/control/exception.Exception library/lux/world/file.Path)
```

### polling

```clojure
(-> (library/lux/world/file.System library/lux/control/concurrency/async.Async) (Watcher library/lux/control/concurrency/async.Async))
```

A simple watcher that works for any file\-system\.Polls files and directories to detect changes\.

```clojure
(polling fs)
```

___

# library/lux/world/input/keyboard

## Definitions

### Key

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

A key from a keyboard, identify by a numeric ID\.

### Press

```clojure
... .Type
(Record
 [#pressed? .Bit
  #input Key])
```

A key\-press for a key\.

### a

```clojure
Key
```

### alt

```clojure
Key
```

### b

```clojure
Key
```

### back\_space

```clojure
Key
```

### c

```clojure
Key
```

### caps\_lock

```clojure
Key
```

### control

```clojure
Key
```

### d

```clojure
Key
```

### delete

```clojure
Key
```

### down

```clojure
Key
```

### e

```clojure
Key
```

### end

```clojure
Key
```

### enter

```clojure
Key
```

### escape

```clojure
Key
```

### f

```clojure
Key
```

### f1

```clojure
Key
```

### f10

```clojure
Key
```

### f11

```clojure
Key
```

### f12

```clojure
Key
```

### f13

```clojure
Key
```

### f14

```clojure
Key
```

### f15

```clojure
Key
```

### f16

```clojure
Key
```

### f17

```clojure
Key
```

### f18

```clojure
Key
```

### f19

```clojure
Key
```

### f2

```clojure
Key
```

### f20

```clojure
Key
```

### f21

```clojure
Key
```

### f22

```clojure
Key
```

### f23

```clojure
Key
```

### f24

```clojure
Key
```

### f3

```clojure
Key
```

### f4

```clojure
Key
```

### f5

```clojure
Key
```

### f6

```clojure
Key
```

### f7

```clojure
Key
```

### f8

```clojure
Key
```

### f9

```clojure
Key
```

### g

```clojure
Key
```

### h

```clojure
Key
```

### home

```clojure
Key
```

### i

```clojure
Key
```

### insert

```clojure
Key
```

### j

```clojure
Key
```

### k

```clojure
Key
```

### l

```clojure
Key
```

### left

```clojure
Key
```

### m

```clojure
Key
```

### n

```clojure
Key
```

### num\_lock

```clojure
Key
```

### num\_pad\_0

```clojure
Key
```

### num\_pad\_1

```clojure
Key
```

### num\_pad\_2

```clojure
Key
```

### num\_pad\_3

```clojure
Key
```

### num\_pad\_4

```clojure
Key
```

### num\_pad\_5

```clojure
Key
```

### num\_pad\_6

```clojure
Key
```

### num\_pad\_7

```clojure
Key
```

### num\_pad\_8

```clojure
Key
```

### num\_pad\_9

```clojure
Key
```

### o

```clojure
Key
```

### p

```clojure
Key
```

### page\_down

```clojure
Key
```

### page\_up

```clojure
Key
```

### press

```clojure
(-> Key Press)
```

### print\_screen

```clojure
Key
```

### q

```clojure
Key
```

### r

```clojure
Key
```

### release

```clojure
(-> Key Press)
```

### right

```clojure
Key
```

### s

```clojure
Key
```

### scroll\_lock

```clojure
Key
```

### shift

```clojure
Key
```

### space

```clojure
Key
```

### t

```clojure
Key
```

### u

```clojure
Key
```

### up

```clojure
Key
```

### v

```clojure
Key
```

### w

```clojure
Key
```

### windows

```clojure
Key
```

### x

```clojure
Key
```

### y

```clojure
Key
```

### z

```clojure
Key
```

___

# library/lux/world/net

## Definitions

### Address

```clojure
... .Type
(Primitive "#Text")
```

A TCP/IP address\.

### Location

```clojure
... .Type
(Record
 [#address Address
  #port Port])
```

### Port

```clojure
... .Type
(Primitive "#I64" (Primitive "#Nat"))
```

A TCP/IP port\.

### URL

```clojure
... .Type
(Primitive "#Text")
```

A Uniform Resource Locator\.

___

# library/lux/world/net/http/client

## Definitions

### \(Client \!\)

```clojure
... .Type
(Record
 [#request (-> library/lux/world/net/http.Method library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (! (library/lux/control/try.Try (library/lux/world/net/http.Response !))))])
```

A HTTP client capable of issuing requests to a HTTP server\.

### async

```clojure
(-> (Client library/lux/control/io.IO) (Client library/lux/control/concurrency/async.Async))
```

### connect

```clojure
(All (_ _0)
  (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client _0) (_0 (library/lux/control/try.Try (library/lux/world/net/http.Response _0)))))
```

A CONNECT request\.

### delete

```clojure
(All (_ _0)
  (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client _0) (_0 (library/lux/control/try.Try (library/lux/world/net/http.Response _0)))))
```

A DELETE request\.

### get

```clojure
(All (_ _0)
  (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client _0) (_0 (library/lux/control/try.Try (library/lux/world/net/http.Response _0)))))
```

A GET request\.

### head

```clojure
(All (_ _0)
  (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client _0) (_0 (library/lux/control/try.Try (library/lux/world/net/http.Response _0)))))
```

A HEAD request\.

### headers

```clojure
(-> (.List [.Text .Text]) library/lux/world/net/http.Headers)
```

### options

```clojure
(All (_ _0)
  (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client _0) (_0 (library/lux/control/try.Try (library/lux/world/net/http.Response _0)))))
```

A OPTIONS request\.

### patch

```clojure
(All (_ _0)
  (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client _0) (_0 (library/lux/control/try.Try (library/lux/world/net/http.Response _0)))))
```

A PATCH request\.

### post

```clojure
(All (_ _0)
  (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client _0) (_0 (library/lux/control/try.Try (library/lux/world/net/http.Response _0)))))
```

A POST request\.

### put

```clojure
(All (_ _0)
  (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client _0) (_0 (library/lux/control/try.Try (library/lux/world/net/http.Response _0)))))
```

A PUT request\.

### trace

```clojure
(All (_ _0)
  (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client _0) (_0 (library/lux/control/try.Try (library/lux/world/net/http.Response _0)))))
```

A TRACE request\.

___

# library/lux/world/net/http/status

## Definitions

### accepted

```clojure
library/lux/world/net/http.Status
```

202: ACCEPTED

### already\_reported

```clojure
library/lux/world/net/http.Status
```

208: ALREADY REPORTED

### bad\_gateway

```clojure
library/lux/world/net/http.Status
```

502: BAD GATEWAY

### bad\_request

```clojure
library/lux/world/net/http.Status
```

400: BAD REQUEST

### conflict

```clojure
library/lux/world/net/http.Status
```

409: CONFLICT

### continue

```clojure
library/lux/world/net/http.Status
```

100: CONTINUE

### created

```clojure
library/lux/world/net/http.Status
```

201: CREATED

### early\_hints

```clojure
library/lux/world/net/http.Status
```

103: EARLY HINTS

### expectation\_failed

```clojure
library/lux/world/net/http.Status
```

417: EXPECTATION FAILED

### failed\_dependency

```clojure
library/lux/world/net/http.Status
```

424: FAILED DEPENDENCY

### forbidden

```clojure
library/lux/world/net/http.Status
```

403: FORBIDDEN

### found

```clojure
library/lux/world/net/http.Status
```

302: FOUND

### gateway\_timeout

```clojure
library/lux/world/net/http.Status
```

504: GATEWAY TIMEOUT

### gone

```clojure
library/lux/world/net/http.Status
```

410: GONE

### http\_version\_not\_supported

```clojure
library/lux/world/net/http.Status
```

505: HTTP VERSION NOT SUPPORTED

### im\_a\_teapot

```clojure
library/lux/world/net/http.Status
```

418: IM A TEAPOT

### im\_used

```clojure
library/lux/world/net/http.Status
```

226: IM USED

### insufficient\_storage

```clojure
library/lux/world/net/http.Status
```

507: INSUFFICIENT STORAGE

### internal\_server\_error

```clojure
library/lux/world/net/http.Status
```

500: INTERNAL SERVER ERROR

### length\_required

```clojure
library/lux/world/net/http.Status
```

411: LENGTH REQUIRED

### locked

```clojure
library/lux/world/net/http.Status
```

423: LOCKED

### loop\_detected

```clojure
library/lux/world/net/http.Status
```

508: LOOP DETECTED

### method\_not\_allowed

```clojure
library/lux/world/net/http.Status
```

405: METHOD NOT ALLOWED

### misdirected\_request

```clojure
library/lux/world/net/http.Status
```

421: MISDIRECTED REQUEST

### moved\_permanently

```clojure
library/lux/world/net/http.Status
```

301: MOVED PERMANENTLY

### multi\_status

```clojure
library/lux/world/net/http.Status
```

207: MULTI STATUS

### multiple\_choices

```clojure
library/lux/world/net/http.Status
```

300: MULTIPLE CHOICES

### network\_authentication\_required

```clojure
library/lux/world/net/http.Status
```

511: NETWORK AUTHENTICATION REQUIRED

### no\_content

```clojure
library/lux/world/net/http.Status
```

204: NO CONTENT

### non\_authoritative\_information

```clojure
library/lux/world/net/http.Status
```

203: NON AUTHORITATIVE INFORMATION

### not\_acceptable

```clojure
library/lux/world/net/http.Status
```

406: NOT ACCEPTABLE

### not\_extended

```clojure
library/lux/world/net/http.Status
```

510: NOT EXTENDED

### not\_found

```clojure
library/lux/world/net/http.Status
```

404: NOT FOUND

### not\_implemented

```clojure
library/lux/world/net/http.Status
```

501: NOT IMPLEMENTED

### not\_modified

```clojure
library/lux/world/net/http.Status
```

304: NOT MODIFIED

### ok

```clojure
library/lux/world/net/http.Status
```

200: OK

### partial\_content

```clojure
library/lux/world/net/http.Status
```

206: PARTIAL CONTENT

### payload\_too\_large

```clojure
library/lux/world/net/http.Status
```

413: PAYLOAD TOO LARGE

### payment\_required

```clojure
library/lux/world/net/http.Status
```

402: PAYMENT REQUIRED

### permanent\_redirect

```clojure
library/lux/world/net/http.Status
```

308: PERMANENT REDIRECT

### precondition\_failed

```clojure
library/lux/world/net/http.Status
```

412: PRECONDITION FAILED

### precondition\_required

```clojure
library/lux/world/net/http.Status
```

428: PRECONDITION REQUIRED

### processing

```clojure
library/lux/world/net/http.Status
```

102: PROCESSING

### proxy\_authentication\_required

```clojure
library/lux/world/net/http.Status
```

407: PROXY AUTHENTICATION REQUIRED

### range\_not\_satisfiable

```clojure
library/lux/world/net/http.Status
```

416: RANGE NOT SATISFIABLE

### request\_header\_fields\_too\_large

```clojure
library/lux/world/net/http.Status
```

431: REQUEST HEADER FIELDS TOO LARGE

### request\_timeout

```clojure
library/lux/world/net/http.Status
```

408: REQUEST TIMEOUT

### reset\_content

```clojure
library/lux/world/net/http.Status
```

205: RESET CONTENT

### see\_other

```clojure
library/lux/world/net/http.Status
```

303: SEE OTHER

### service\_unavailable

```clojure
library/lux/world/net/http.Status
```

503: SERVICE UNAVAILABLE

### switch\_proxy

```clojure
library/lux/world/net/http.Status
```

306: SWITCH PROXY

### switching\_protocols

```clojure
library/lux/world/net/http.Status
```

101: SWITCHING PROTOCOLS

### temporary\_redirect

```clojure
library/lux/world/net/http.Status
```

307: TEMPORARY REDIRECT

### too\_many\_requests

```clojure
library/lux/world/net/http.Status
```

429: TOO MANY REQUESTS

### unauthorized

```clojure
library/lux/world/net/http.Status
```

401: UNAUTHORIZED

### unavailable\_for\_legal\_reasons

```clojure
library/lux/world/net/http.Status
```

451: UNAVAILABLE FOR LEGAL REASONS

### unprocessable\_entity

```clojure
library/lux/world/net/http.Status
```

422: UNPROCESSABLE ENTITY

### unsupported\_media\_type

```clojure
library/lux/world/net/http.Status
```

415: UNSUPPORTED MEDIA TYPE

### upgrade\_required

```clojure
library/lux/world/net/http.Status
```

426: UPGRADE REQUIRED

### uri\_too\_long

```clojure
library/lux/world/net/http.Status
```

414: URI TOO LONG

### use\_proxy

```clojure
library/lux/world/net/http.Status
```

305: USE PROXY

### variant\_also\_negotiates

```clojure
library/lux/world/net/http.Status
```

506: VARIANT ALSO NEGOTIATES

___

# library/lux/world/net/uri

## Definitions

### URI

```clojure
... .Type
(Primitive "#Text")
```

A Uniform Resource Identifier\.

### separator

```clojure
.Text
```

A separator for the pieces of a URI\.

___

# library/lux/world/output/video/resolution

## Definitions

### Resolution

```clojure
... .Type
(Record
 [#width .Nat
  #height .Nat])
```

A screen resolution\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Resolution)
```

### fhd

```clojure
Resolution
```

FHD resolution: 1920x1080\.

### hash

```clojure
(library/lux/abstract/hash.Hash Resolution)
```

### hd\+

```clojure
Resolution
```

HD\+ resolution: 1600x900\.

### svga

```clojure
Resolution
```

SVGA resolution: 800x600\.

### sxga

```clojure
Resolution
```

SXGA resolution: 1280x1024\.

### uhd\_4k

```clojure
Resolution
```

UHD 4K resolution: 3840x2160\.

### wqhd

```clojure
Resolution
```

WQHD resolution: 2560x1440\.

### wsvga

```clojure
Resolution
```

WSVGA resolution: 1024x600\.

### wsxga\+

```clojure
Resolution
```

WSXGA\+ resolution: 1680x1050\.

### wuxga

```clojure
Resolution
```

WUXGA resolution: 1920x1200\.

### wxga\+

```clojure
Resolution
```

WXGA\+ resolution: 1440x900\.

### wxga/16:10

```clojure
Resolution
```

WXGA 16:10 resolution: 1280x800\.

### wxga/16:9

```clojure
Resolution
```

WXGA 16:9 resolution: 1280x720\.

### wxga/5:3

```clojure
Resolution
```

WXGA 5:3 resolution: 1280x768\.

### xga

```clojure
Resolution
```

XGA resolution: 1024x768\.

### xga\+

```clojure
Resolution
```

XGA\+ resolution: 1152x864\.

___

# library/lux/world/program

## Definitions

### \(Program \!\)

```clojure
... .Type
(Record
 [available_variables (-> .Any (! (.List .Text)))
  variable (-> .Text (! (library/lux/control/try.Try .Text)))
  home library/lux/world/file.Path
  directory library/lux/world/file.Path
  exit (-> library/lux/world/shell.Exit (! .Nothing))])
```

Access to ambient program data and the capacity to exit the program\.

### async

```clojure
(-> (Program library/lux/control/io.IO) (Program library/lux/control/concurrency/async.Async))
```

### default

```clojure
(Program library/lux/control/io.IO)
```

### environment

```clojure
(All (_ _0)
  (-> (library/lux/abstract/monad.Monad _0) (Program _0) (_0 library/lux/control/parser/environment.Environment)))
```

Assembles the environment variables available to the program\.

```clojure
(environment monad program)
```

### mock

```clojure
(-> library/lux/control/parser/environment.Environment library/lux/world/file.Path library/lux/world/file.Path (Program library/lux/control/io.IO))
```

```clojure
(mock environment home directory)
```

### unknown\_environment\_variable

```clojure
(library/lux/control/exception.Exception .Text)
```

___

# library/lux/world/shell

## Definitions

### Argument

```clojure
... .Type
(Primitive "#Text")
```

A parameter for a command\.

### Command

```clojure
... .Type
(Primitive "#Text")
```

A command that can be executed by the operating system\.

### Exit

```clojure
... .Type
(Primitive "#I64" (Primitive "#Int"))
```

A program exit code\.

### \(Mock s\)

```clojure
... .Type
(Record
 [on_read (-> s (library/lux/control/try.Try [s .Text]))
  on_fail (-> s (library/lux/control/try.Try [s .Text]))
  on_write (-> .Text s (library/lux/control/try.Try s))
  on_destroy (-> s (library/lux/control/try.Try s))
  on_await (-> s (library/lux/control/try.Try [s Exit]))])
```

A simulated process\.

### \(Process \!\)

```clojure
... .Type
(Record
 [read (-> .Any (! (library/lux/control/try.Try .Text)))
  fail (-> .Any (! (library/lux/control/try.Try .Text)))
  write (-> .Text (! (library/lux/control/try.Try .Any)))
  destroy (-> .Any (! (library/lux/control/try.Try .Any)))
  await (-> .Any (! (library/lux/control/try.Try Exit)))])
```

The means for communicating with a program/process being executed by the operating system\.

### \(Shell \!\)

```clojure
... .Type
(Record
 [#execute (-> [library/lux/control/parser/environment.Environment library/lux/world/file.Path Command (.List Argument)] (! (library/lux/control/try.Try (Process !))))])
```

The means for issuing commands to the operating system\.

### async

```clojure
(-> (Shell library/lux/control/io.IO) (Shell library/lux/control/concurrency/async.Async))
```

### error

```clojure
Exit
```

### mock

```clojure
(All (_ _0)
  (-> (-> [library/lux/control/parser/environment.Environment library/lux/world/file.Path Command (.List Argument)] (library/lux/control/try.Try (Mock _0))) _0 (Shell library/lux/control/io.IO)))
```

```clojure
(mock mock init)
```

### normal

```clojure
Exit
```


