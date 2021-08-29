# library/lux

## Definitions

### $\_

```clojure
Macro
```

Right\-association for the application of binary functions over variadic arguments\.

```clojure
($_ text\composite "Hello, " name ". How are you?")

... =>

(text\composite "Hello, " (text\composite name ". How are you?"))
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
(All (_ a) (-> (I64 a) (I64 a)))
```

Increment function\.

### \-\-

```clojure
(All (_ a) (-> (I64 a) (I64 a)))
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

(def: public (of_list list)
  (All (_ a) (-> (List a) (Row a)))
  (list\mix add
            (: (Row (:parameter 0))
               empty)
            list))
```

### <<|

```clojure
Macro
```

Similar to the reverse piping macro, but rather than taking an initial object to work on, creates a function for taking it\.

```clojure
(<<| (mix text\composite "")
     (interposed " ")
     (list\each int\encoded))

... =>

(function (_ <it>)
  (mix text\composite ""
       (interposed " "
                   (list\each int\encoded
                              <it>))))
```

### <|

```clojure
Macro
```

Reverse piping macro\.

```clojure
(<| (mix text\composite "")
    (interposed " ")
    (list\each int\encoded)
    elems)

... =>

(mix text\composite ""
     (interposed " "
                 (list\each int\encoded
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
[meta_data datum]
```

The type of things that can be annotated with meta\-data of arbitrary types\.

### Any

```clojure
... Type
(Ex (Any a) a)
```

The type of things whose type is irrelevant\.
It can be used to write functions or data\-structures that can take, or return, anything\.

### \(Bindings key value\)

```clojure
... Type
[Nat (List [key value])]
```

### Bit

```clojure
... Type
(primitive "#Bit")
```

Your standard, run\-of\-the\-mill boolean values \(as \#0 or \#1 bits\)\.

### Code

```clojure
(Or [Text (List ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing))] [((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing) ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing)] [((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing) ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing)] [((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing) ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing)] Nat Nat Nat [(List ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing)) ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing)] [(List ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing)) ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing)] [((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing) ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing)] [Name ((All (_ a) (Or [Text (List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] Nat Nat Nat [(List (? a)) (? a)] [(List (? a)) (? a)] [(? a) (? a)] [Name (? a)])) Nothing)])
```

The type of Code nodes for Lux syntax\.

### \(Code' w\)

```clojure
... Type
(Or Bit Nat Int Rev Frac Text Name Name (List (w (Code' w))) (List (w (Code' w))) (List [(w (Code' w)) (w (Code' w))]))
```

### Definition

```clojure
... Type
[Bit Type Code Any]
```

Represents all the data associated with a definition: its type, its annotations, and its value\.

### \(Either left right\)

```clojure
... Type
(Or left right)
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
(primitive "#Frac")
```

Your standard, run\-of\-the\-mill floating\-point \(fractional\) numbers\.

### Global

```clojure
... Type
(Or Definition [Bit Type (Or [Text (List Text)] [Text (List Text)])] Label Label Alias)
```

Represents all the data associated with a global constant\.

### \(I64 kind\)

```clojure
... Type
(primitive "#I64" kind)
```

64\-bit integers without any semantics\.

### Info

```clojure
... Type
[Text Text Mode]
```

Information about the current version and type of compiler that is running\.

### Int

```clojure
... Type
(primitive "#I64" (primitive "#Int"))
```

Your standard, run\-of\-the\-mill integer numbers\.

### Interface

```clojure
Macro
```

Interface definition\.

```clojure
(type: public (Order a)
  (Interface
   (: (Equivalence a)
      &equivalence)
   (: (-> a a Bit)
      <)))
```

### \(List item\)

```clojure
... Type
(Or Any [item (List item)])
```

A potentially empty list of values\.

### Location

```clojure
... Type
[Text Nat Nat]
```

Locations are for specifying the location of Code nodes in Lux files during compilation\.

### Lux

```clojure
... Type
((All (Lux a) [Info Source Location (Maybe Text) (List [Text Module]) (List Scope) Type_Context (Maybe Type) Nat (List Nat) Any (-> Type Code (Lux a) (Or Text [(Lux a) Any])) Any]) Nothing)
```

Represents the state of the Lux compiler during a run\.
It is provided to macros during their invocation, so they can access compiler data\.
Caveat emptor: Avoid fiddling with it, unless you know what you're doing\.

### Macro

```clojure
... Type
(primitive "#Macro")
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
(Or Any value)
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
(Or Any Any Any)
```

A sign that shows the conditions under which the compiler is running\.

### Module

```clojure
... Type
[Nat (List [Text Text]) (List [Text Global]) (List Text) (Maybe Code) Module_State]
```

All the information contained within a Lux module\.

### Module\_State

```clojure
... Type
(Or Any Any Any)
```

### Name

```clojure
... Type
[Text Text]
```

A name\. It is used as part of Lux syntax to represent identifiers and tags\.

### Nat

```clojure
... Type
(primitive "#I64" (primitive "#Nat"))
```

Natural numbers \(unsigned integers\)\.
They start at zero \(0\) and extend in the positive direction\.

### Nothing

```clojure
... Type
(All (Nothing a) a)
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
     (#Primitive Text (List @))
     (#Sum @ @)
     (#Product @ @)
     (#Function @ @)
     (#Parameter Nat)
     (#Var Nat)
     (#Ex Nat)
     (#UnivQ (List @) @)
     (#ExQ (List @) @)
     (#Apply @ @)
     (#Named Name @))))
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
   {#refer_defs Referrals
    #refer_open (List Openings)}))
```

### Ref

```clojure
... Type
(Or Nat Nat)
```

### Rev

```clojure
... Type
(primitive "#I64" (primitive "#Rev"))
```

Fractional numbers that live in the interval \[0,1\)\.
Useful for probability, and other domains that work within that interval\.

### Scope

```clojure
... Type
[(List Text) Nat (Bindings Text [Type Nat]) (Bindings Text [Type Ref])]
```

### Source

```clojure
... Type
[Location Nat Text]
```

### Text

```clojure
... Type
(primitive "#Text")
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
((All (Type a) (Or [Text (List (Type a))] [(Type a) (Type a)] [(Type a) (Type a)] [(Type a) (Type a)] Nat Nat Nat [(List (Type a)) (Type a)] [(List (Type a)) (Type a)] [(Type a) (Type a)] [Name (Type a)])) Nothing)
```

This type represents the data\-structures that are used to specify types themselves\.

### Type\_Context

```clojure
... Type
[Nat Nat (List [Nat (Maybe Type)])]
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
   #All
   (#Only (List Text))
   (#Exclude (List Text))
   #Ignore
   #Nothing))
```

### \\

```clojure
Macro
```

Allows accessing the value of a implementation's member\.

```clojure
(\ codec encoded)

................................................................
................................................................

... Also allows using that value as a function.

(\ codec encoded +123)
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
  (#Some ($_ * x y z))

  _
  #None)
```

### ^@

```clojure
Macro
```

Allows you to simultaneously bind and de\-structure a value\.

```clojure
(def: (hash (^@ set [member_hash _]))
  (list\mix (function (_ elem acc)
              (+ acc
                 (\ member_hash hash elem)))
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
     (^code (#0 (~ [_ (#library/lux.Natnumber)]) +456.789))
     (#library/lux.Somenumber)

     _
     #library/lux.None))
```

### ^multi

```clojure
Macro
```

Multi\-level pattern matching\.
Useful in situations where the result of a branch depends on further refinements on the values being matched\.

```clojure
(case (split (size static) uri)
  (^multi (#Some [chunk uri'])
          {(text\= static chunk) #1})
  (match_uri endpoint? parts' uri')

  _
  (#Left (format "Static part " (%t static) " does not match URI: " uri)))

................................................................
................................................................

... Short-cuts can be taken when using bit tests.

... The example above can be rewritten as...

(case (split (size static) uri)
  (^multi (#Some [chunk uri'])
          (text\= static chunk))
  (match_uri endpoint? parts' uri')

  _
  (#Left (format "Static part " (%t static) " does not match URI: " uri)))
```

### ^open

```clojure
Macro
```

Same as the 'open' macro, but meant to be used as a pattern\-matching macro for generating local bindings\.
Takes an 'alias' text for the generated local bindings\.

```clojure
(def: public (range enum from to)
  (All (_ a) (-> (Enum a) a a (List a)))
  (let [(^open ".") enum]
    (loop [end to
           output #library/lux.End]
      (cond (< end from)
            (recur (pred end) (#library/lux.Itemend output))

            (< from end)
            (recur (succ end) (#library/lux.Itemend output))


            (#library/lux.Itemend output)))))
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
   #Monday
   #Tuesday
   #Wednesday
   #Thursday
   #Friday
   #Saturday
   #Sunday))

(def: (weekend? day)
  (-> Weekday Bit)
  (case day
    (^or #Saturday #Sunday)
    #1

    _
    #0))
```

### ^slots

```clojure
Macro
```

Allows you to extract record members as local variables with the same names\.

```clojure
(let [(^slots [#foo #bar #baz]) quux]
  (f foo bar baz))
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
    (#library/lux.Primitivename params)
    (#library/lux.Primitivename (list\each (reduced env) params))

    (^template [<tag>]
      [(<tag> left right)
       (<tag> (reduced env left) (reduced env right))])
    ([#library/lux.Sum][#library/lux.Product])

    (^template [<tag>]
      [(<tag> left right)
       (<tag> (reduced env left) (reduced env right))])
    ([#library/lux.Function][#library/lux.Apply])

    (^template [<tag>]
      [(<tag> old_env def)
       (case old_env
         #library/lux.End
         (<tag> env def)

         _
         type)])
    ([#library/lux.UnivQ][#library/lux.ExQ])

    (#library/lux.Parameteridx)
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
(_$ text\composite "Hello, " name ". How are you?")

... =>

(text\composite (text\composite "Hello, " name) ". How are you?")
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
  (#Item x (#Item y (#Item z #End)))
  (#Some ($_ * x y z))

  _
  #None)
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
(def: js "JavaScript")

(for {"JVM" (do jvm stuff)
      documentation/lux.js(do js stuff)}
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
(implementation: public order
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

Allows arbitrary looping, using the 'recur' form to re\-start the loop\.
Can be used in monadic code to create monadic loops\.

```clojure
(loop [count +0
       x init]
  (if (< +10 count)
    (recur (++ count) (f x))
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

### macro:

```clojure
Macro
```

Macro\-definition macro\.

```clojure
(macro: public (name_of tokens)
  (case tokens
    (^template [<tag>]
      [(^ (list [_ (<tag> [module name])]))
       (in (list (` [(~ (text$ module)) (~ (text$ name))])))])
    ([#Identifier] [#Tag])

    _
    (failure "Wrong syntax for name_of")))
```

### module:

```clojure
Macro
```

Module\-definition macro\.

```clojure
(module:
  [lux #*
   [control
    ["M" monad #*]]
   [data
    maybe
    ["." name ("#/." codec)]]
   [macro
    code]]
  [//
   [type ("." equivalence)]])
```

### module\_separator

```clojure
Text
```

Character used to separate the parts of module names\.
Value: "/"

### name\_of

```clojure
Macro
```

Given an identifier or a tag, gives back a 2 tuple with the module and name parts, both as Text\.

```clojure
(name_of #library/lux.doc)

... =>

["library/lux" "doc"]
```

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
(open: "i:." order)

... =>

(def: i:= (\ order =))

(def: i:< (\ order <))
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

### primitive

```clojure
Macro
```

Macro to treat define new primitive types\.

```clojure
(primitive "java.lang.Object")

................................................................
................................................................

(primitive "java.util.List" [(primitive "java.lang.Long")])
```

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
(All (_ a) (-> a a Bit))
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

### template

```clojure
Macro
```

```clojure
... By specifying a pattern (with holes), and the input data to fill those holes, repeats the pattern as many times as necessary.

(template [<name> <diff>]
  [(def: public <name>
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
     (#library/lux.Rightsuccess)
     (: Foo
        (do something after success))

     (#library/lux.Lefterror)
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
  #End
  (#Item a (List a)))
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
                                (\ codec encoded <function> <parameter>)))]

               [bit #1                              "#1"]
               [int +123                            "+123"]
               [frac +123.0                         "+123.0"]
               [text "123"                           "'123'"]
               [tag ["yolo" "lol"]                  "#yolo.lol"]
               [identifier ["yolo" "lol"]           "yolo.lol"]
               [form (list (bit #1))                "(#1)"]
               [tuple (list (bit #1))               "[#1]"]
               [record (list [(bit #1) (int +123)]) "{#1 +123}"])]

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
    (list\each int\encoded)
    (interposed " ")
    (mix text\composite ""))

... =>

(mix text\composite ""
     (interposed " "
                 (list\each int\encoded
                            elems)))
```

### |>>

```clojure
Macro
```

Similar to the piping macro, but rather than taking an initial object to work on, creates a function for taking it\.

```clojure
(|>> (list\each int\encoded)
     (interposed " ")
     (mix text\composite ""))

... =>

(function (_ <it>)
  (mix text\composite ""
       (interposed " "
                   (list\each int\encoded <it>))))
```

## Missing documentation

1. `` Label ``
1. `` __adjusted_quantified_type__ ``
1. `` macro ``

___

# library/lux/abstract/apply

## Definitions

### \(Apply f\)

```clojure
... .Type
[(library/lux/abstract/functor.Functor f) (All (_ b c) (-> (f b) (f (-> b c)) (f c)))]
```

Applicative functors\.

### composite

```clojure
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (Apply a) (Apply b) (Apply (All (_ c) (a (b c))))))
```

Applicative functor composition\.

___

# library/lux/abstract/codec

## Definitions

### \(Codec medium value\)

```clojure
... .Type
[(-> value medium) (-> medium (library/lux/control/try.Try value))]
```

A way to move back\-and\-forth between a type and an alternative representation for it\.

### composite

```clojure
(All (_ a b c) (-> (Codec c b) (Codec b a) (Codec c a)))
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
[(library/lux/abstract/functor.Functor !) (All (_ b) (-> (! b) b)) (All (_ b) (-> (! b) (! (! b))))]
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
(All (_ a) (-> (library/lux/abstract/functor.Functor a) (library/lux/abstract/comonad.CoMonad (CoFree a))))
```

### functor

```clojure
(All (_ a) (-> (library/lux/abstract/functor.Functor a) (library/lux/abstract/functor.Functor (CoFree a))))
```

___

# library/lux/abstract/enum

## Definitions

### \(Enum it\)

```clojure
... .Type
[(library/lux/abstract/order.Order it) (-> it it) (-> it it)]
```

Enumerable types, with a notion of moving forward and backwards through a type's instances\.

### range

```clojure
(All (_ a) (-> (Enum a) a a (.List a)))
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
(-> it it .Bit)
```

Equivalence for a type's instances\.

### functor

```clojure
(library/lux/abstract/functor/contravariant.Functor Equivalence)
```

### rec

```clojure
(All (_ a) (-> (-> (Equivalence a) (Equivalence a)) (Equivalence a)))
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
(All (_ c) [(left c) (right c)])
```

### \(Fix \!\)

```clojure
... .Type
(! (Fix !))
```

### \(Functor \!\)

```clojure
... .Type
(All (_ b c) (-> (-> b c) (! b) (! c)))
```

### \(Or left right\)

```clojure
... .Type
(All (_ c) (Or (left c) (right c)))
```

### \(Then outer inner\)

```clojure
... .Type
(All (_ c) (outer (inner c)))
```

### composite

```clojure
(All (_ a b) (-> (Functor a) (Functor b) (Functor (Then a b))))
```

Functor composition\.

### product

```clojure
(All (_ a b) (-> (Functor a) (Functor b) (Functor (And a b))))
```

Product composition for functors\.

### sum

```clojure
(All (_ a b) (-> (Functor a) (Functor b) (Functor (Or a b))))
```

Co\-product \(sum\) composition for functors\.

___

# library/lux/abstract/functor/contravariant

## Definitions

### \(Functor \!\)

```clojure
... .Type
(All (_ b c) (-> (-> c b) (! b) (! c)))
```

The contravariant functor\.

___

# library/lux/abstract/hash

## Definitions

### \(Hash it\)

```clojure
... .Type
[(library/lux/abstract/equivalence.Equivalence it) (-> it .Nat)]
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
[(library/lux/abstract/enum.Enum it) it it]
```

A representation of top and bottom boundaries for an ordered type\.

### after?

```clojure
(All (_ a) (-> a (Interval a) .Bit))
```

### before?

```clojure
(All (_ a) (-> a (Interval a) .Bit))
```

### between

```clojure
(All (_ a) (-> (library/lux/abstract/enum.Enum a) a a (Interval a)))
```

### borders?

```clojure
(All (_ a) (-> (Interval a) a .Bit))
```

Where a value is at the border of an interval\.

### complement

```clojure
(All (_ a) (-> (Interval a) (Interval a)))
```

The inverse of an interval\.

### ends\_with?

```clojure
(All (_ a) (-> a (Interval a) .Bit))
```

### equivalence

```clojure
(All (_ a) (library/lux/abstract/equivalence.Equivalence (Interval a)))
```

### finishes?

```clojure
(All (_ a) (-> (Interval a) (Interval a) .Bit))
```

### inner?

```clojure
(All (_ a) (-> (Interval a) .Bit))
```

### intersection

```clojure
(All (_ a) (-> (Interval a) (Interval a) (Interval a)))
```

An interval spanned by both predecessors\.

### meets?

```clojure
(All (_ a) (-> (Interval a) (Interval a) .Bit))
```

Whether an interval meets another one on its bottom/lower side\.

### nested?

```clojure
(All (_ a) (-> (Interval a) (Interval a) .Bit))
```

### outer?

```clojure
(All (_ a) (-> (Interval a) .Bit))
```

### overlaps?

```clojure
(All (_ a) (-> (Interval a) (Interval a) .Bit))
```

### precedes?

```clojure
(All (_ a) (-> (Interval a) (Interval a) .Bit))
```

### singleton

```clojure
(All (_ a) (-> (library/lux/abstract/enum.Enum a) a (Interval a)))
```

An interval where both top and bottom are the same value\.

```clojure
(singleton enum elem)
```

### singleton?

```clojure
(All (_ a) (-> (Interval a) .Bit))
```

### starts?

```clojure
(All (_ a) (-> (Interval a) (Interval a) .Bit))
```

### starts\_with?

```clojure
(All (_ a) (-> a (Interval a) .Bit))
```

### succeeds?

```clojure
(All (_ a) (-> (Interval a) (Interval a) .Bit))
```

### touches?

```clojure
(All (_ a) (-> (Interval a) (Interval a) .Bit))
```

### union

```clojure
(All (_ a) (-> (Interval a) (Interval a) (Interval a)))
```

An interval that spans both predecessors\.

### within?

```clojure
(All (_ a) (-> (Interval a) a .Bit))
```

___

# library/lux/abstract/mix

## Definitions

### \(Mix structure\)

```clojure
... .Type
(All (_ b c) (-> (-> c b b) b (structure c) b))
```

Iterate over a structure's values to build a summary value\.

### with\_monoid

```clojure
(All (_ a b) (-> (library/lux/abstract/monoid.Monoid b) (Mix a) (a b) b))
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
[(library/lux/abstract/functor.Functor it) (All (_ b) (-> b (it b))) (All (_ b) (-> (it (it b)) (it b)))]
```

A monad is a monoid in the category of endofunctors\.
What's the problem?

### all

```clojure
(All (_ a b) (-> (Monad a) (.List (a b)) (a (.List b))))
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
(All (_ a b c) (-> (Monad a) (-> b (a c)) (.List b) (a (.List c))))
```

Apply a monadic function to all values in a list\.

```clojure
(each monad function items)
```

### mix

```clojure
(All (_ a b c) (-> (Monad a) (-> c b (a b)) b (.List c) (a b)))
```

Mix a list with a monadic function\.

```clojure
(mix monad function initial_value items)
```

### only

```clojure
(All (_ a b c) (-> (Monad a) (-> b (a .Bit)) (.List b) (a (.List b))))
```

Filter the values in a list with a monadic function\.

```clojure
(only monad predicate items)
```

### then

```clojure
(All (_ a b c) (-> (Monad a) (-> b (a c)) (a b) (a c)))
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
(Or it (! (Free ! it)))
```

The Free Monad\.

### apply

```clojure
(All (_ a) (-> (library/lux/abstract/functor.Functor a) (library/lux/abstract/apply.Apply (Free a))))
```

### functor

```clojure
(All (_ a) (-> (library/lux/abstract/functor.Functor a) (library/lux/abstract/functor.Functor (Free a))))
```

### monad

```clojure
(All (_ a) (-> (library/lux/abstract/functor.Functor a) (library/lux/abstract/monad.Monad (Free a))))
```

___

# library/lux/abstract/monoid

## Definitions

### \(Monoid it\)

```clojure
... .Type
[it (-> it it it)]
```

A way to compose values\.
Includes an identity value which does not alter any other value when combined with\.

### and

```clojure
(All (_ a b) (-> (Monoid a) (Monoid b) (Monoid [a b])))
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
[(library/lux/abstract/equivalence.Equivalence it) (-> it it .Bit)]
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
(All (_ a) (-> (Predicate a) (Predicate a) (Predicate a)))
```

A predicate that meets both predecessors\.

### complement

```clojure
(All (_ a) (-> (Predicate a) (Predicate a)))
```

The opposite of a predicate\.

### difference

```clojure
(All (_ a) (-> (Predicate a) (Predicate a) (Predicate a)))
```

A predicate that meeds 'base', but not 'sub'\.

### functor

```clojure
(library/lux/abstract/functor/contravariant.Functor Predicate)
```

### intersection

```clojure
(All (_ a) (library/lux/abstract/monoid.Monoid (Predicate a)))
```

### none

```clojure
Predicate
```

A predicate that always fails\.

### or

```clojure
(All (_ a) (-> (Predicate a) (Predicate a) (Predicate a)))
```

A predicate that meets either predecessor\.

### rec

```clojure
(All (_ a) (-> (-> (Predicate a) (Predicate a)) (Predicate a)))
```

Ties the knot for a recursive predicate\.

### union

```clojure
(All (_ a) (library/lux/abstract/monoid.Monoid (Predicate a)))
```

___

# library/lux/control/concatenative

## Definitions

### &&

```clojure
(All (_ a b c) (-> [[c a] b] [c a b]))
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

(All (_ ___a ___z)
  (=> {then (=> ___a ___z)
       else (=> ___a ___z)}
      ___a [Bit then else] ___z))
```

### ?

```clojure
(All (_ a b) (-> [[[b .Bit] a] a] [b a]))
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
(All (_ a b) (-> (-> a b) (All (_ c) (-> [c a] [c b]))))
```

Lift a function of arity 1 into a concatenative function of arity 1\.

```clojure
        (

   1function_of_arity_1)
```

### apply/2

```clojure
(All (_ a b c) (-> (-> a b c) (All (_ d) (-> [[d a] b] [d c]))))
```

Lift a function of arity 2 into a concatenative function of arity 2\.

```clojure
        (

       2function_of_arity_2)
```

### apply/3

```clojure
(All (_ a b c d) (-> (-> a b c d) (All (_ e) (-> [[[e a] b] c] [e d]))))
```

Lift a function of arity 3 into a concatenative function of arity 3\.

```clojure
        (

           3function_of_arity_3)
```

### apply/4

```clojure
(All (_ a b c d e) (-> (-> a b c d e) (All (_ f) (-> [[[[f a] b] c] d] [f e]))))
```

Lift a function of arity 4 into a concatenative function of arity 4\.

```clojure
        (

               4function_of_arity_4)
```

### apply/5

```clojure
(All (_ a b c d e f) (-> (-> a b c d e f) (All (_ g) (-> [[[[[g a] b] c] d] e] [g f]))))
```

Lift a function of arity 5 into a concatenative function of arity 5\.

```clojure
        (


   5function_of_arity_5)
```

### apply/6

```clojure
(All (_ a b c d e f g) (-> (-> a b c d e f g) (All (_ h) (-> [[[[[[h a] b] c] d] e] f] [h g]))))
```

Lift a function of arity 6 into a concatenative function of arity 6\.

```clojure
        (


       6function_of_arity_6)
```

### apply/7

```clojure
(All (_ a b c d e f g h) (-> (-> a b c d e f g h) (All (_ i) (-> [[[[[[[i a] b] c] d] e] f] g] [i h]))))
```

Lift a function of arity 7 into a concatenative function of arity 7\.

```clojure
        (


           7function_of_arity_7)
```

### apply/8

```clojure
(All (_ a b c d e f g h i) (-> (-> a b c d e f g h i) (All (_ j) (-> [[[[[[[[j a] b] c] d] e] f] g] h] [j i]))))
```

Lift a function of arity 8 into a concatenative function of arity 8\.

```clojure
        (


               8function_of_arity_8)
```

### call

```clojure
(All (_ a b) (-> [a (-> a b)] b))
```

Executes an anonymous block on the stack\.

### compose

```clojure
(All (_ a b c d) (-> [[d (-> a b)] (-> b c)] [d (-> a c)]))
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
(All (_ a b) (-> [[a b] (-> a a)] [a b]))
```

Executes a block on the stack, save for the topmost value\.

### dip/2

```clojure
(All (_ a b c) (-> [[[a b] c] (-> a a)] [[a b] c]))
```

Executes a block on the stack, save for the 2 topmost values\.

### do

```clojure
(All (_ a b) (-> [[a (-> b [a .Bit])] (-> a b)] [[b (-> b [a .Bit])] (-> a b)]))
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
(All (_ a b) (-> [b a] b))
```

Drop/pop a value from the top of the stack\.

### dup

```clojure
(All (_ a b) (-> [b a] [[b a] a]))
```

Duplicate the top of the stack\.

### f/%

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Frac]))
```

% for Frac arithmetic\.

### f/\*

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Frac]))
```

\* for Frac arithmetic\.

### f/\+

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Frac]))
```

\+ for Frac arithmetic\.

### f/\-

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Frac]))
```

\- for Frac arithmetic\.

### f//

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Frac]))
```

/ for Frac arithmetic\.

### f/<

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Bit]))
```

< for Frac arithmetic\.

### f/<=

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Bit]))
```

<= for Frac arithmetic\.

### f/=

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Bit]))
```

= for Frac arithmetic\.

### f/>

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Bit]))
```

> for Frac arithmetic\.

### f/>=

```clojure
(All (_ a) (-> [[a .Frac] .Frac] [a .Bit]))
```

>= for Frac arithmetic\.

### i/%

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Int]))
```

% for Int arithmetic\.

### i/\*

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Int]))
```

\* for Int arithmetic\.

### i/\+

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Int]))
```

\+ for Int arithmetic\.

### i/\-

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Int]))
```

\- for Int arithmetic\.

### i//

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Int]))
```

/ for Int arithmetic\.

### i/<

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Bit]))
```

< for Int arithmetic\.

### i/<=

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Bit]))
```

<= for Int arithmetic\.

### i/=

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Bit]))
```

= for Int arithmetic\.

### i/>

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Bit]))
```

> for Int arithmetic\.

### i/>=

```clojure
(All (_ a) (-> [[a .Int] .Int] [a .Bit]))
```

>= for Int arithmetic\.

### if

```clojure
(All (_ a b) (-> [[[a .Bit] (-> a b)] (-> a b)] b))
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
(All (_ a) (-> [a (-> a [a .Bit])] a))
```

Executes a block as a loop until it yields \#0 to stop\.

### n/%

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Nat]))
```

% for Nat arithmetic\.

### n/\*

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Nat]))
```

\* for Nat arithmetic\.

### n/\+

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Nat]))
```

\+ for Nat arithmetic\.

### n/\-

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Nat]))
```

\- for Nat arithmetic\.

### n//

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Nat]))
```

/ for Nat arithmetic\.

### n/<

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Bit]))
```

< for Nat arithmetic\.

### n/<=

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Bit]))
```

<= for Nat arithmetic\.

### n/=

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Bit]))
```

= for Nat arithmetic\.

### n/>

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Bit]))
```

> for Nat arithmetic\.

### n/>=

```clojure
(All (_ a) (-> [[a .Nat] .Nat] [a .Bit]))
```

>= for Nat arithmetic\.

### nip

```clojure
(All (_ a b c) (-> [[c a] b] [c b]))
```

Drop the second\-to\-last value from the top of the stack\.

### partial

```clojure
(All (_ a b c) (-> [[a c] (-> [a c] b)] [a (-> a b)]))
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
(All (_ a) (-> a (All (_ b) (-> b [b a]))))
```

Push a value onto the stack\.

### r/%

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Rev]))
```

% for Rev arithmetic\.

### r/\*

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Rev]))
```

\* for Rev arithmetic\.

### r/\+

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Rev]))
```

\+ for Rev arithmetic\.

### r/\-

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Rev]))
```

\- for Rev arithmetic\.

### r//

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Rev]))
```

/ for Rev arithmetic\.

### r/<

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Bit]))
```

< for Rev arithmetic\.

### r/<=

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Bit]))
```

<= for Rev arithmetic\.

### r/=

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Bit]))
```

= for Rev arithmetic\.

### r/>

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Bit]))
```

> for Rev arithmetic\.

### r/>=

```clojure
(All (_ a) (-> [[a .Rev] .Rev] [a .Bit]))
```

>= for Rev arithmetic\.

### rotL

```clojure
(All (_ a b c d) (-> [[[d a] b] c] [[[d b] c] a]))
```

Rotes the 3 topmost stack values to the left\.

### rotR

```clojure
(All (_ a b c d) (-> [[[d a] b] c] [[[d c] a] b]))
```

Rotes the 3 topmost stack values to the right\.

### swap

```clojure
(All (_ a b c) (-> [[c a] b] [[c b] a]))
```

Swaps the 2 topmost stack values\.

### when

```clojure
(All (_ a) (-> [[a .Bit] (-> a a)] a))
```

Only execute the block when \#1\.

### while

```clojure
(All (_ a b) (-> [[a (-> a [b .Bit])] (-> b a)] b))
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
(All (_ a b c) (-> [c a] [c (Or a b)]))
```

Left\-injects the top into sum\.

### ||R

```clojure
(All (_ a b c) (-> [c b] [c (Or a b)]))
```

Right\-injects the top into sum\.

___

# library/lux/control/concurrency/actor

The actor model of concurrency\.

## Definitions

### \(Actor state\)

```clojure
... .Type
(primitive "library/lux/control/concurrency/actor.Actor" state)
```

An entity that can react to messages \(mail\) sent to it concurrently\.

### \(Behavior input state\)

```clojure
... .Type
[(-> input state) (-> (Mail state) state (Actor state) (library/lux/control/concurrency/async.Async (library/lux/control/try.Try state)))]
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
(actor {Nat
        123}
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
(actor: .public(stack a)
  {}

  (List a)

  ((on_mail mail state self)
   (do (try.with async.monad)
     [.let[_ (debug.log! "BEFORE")]
      output (mail state self)
      .let[_ (debug.log! "AFTER")]]
     (in output)))

  (message: .public(push {value a} state self)
    (List a)
    (let [state' (#library/lux.Itemvalue state)]
      (async.resolved (#try.Success [state' state'])))))

(actor: .publiccounter
  {}

  Nat

  (message: .public(count! {increment Nat} state self)
    Any
    (let [state' (n.+ increment state)]
      (async.resolved (#try.Success [state' state']))))

  (message: .public(read! state self)
    Nat
    (async.resolved (#try.Success [state state]))))
```

### alive?

```clojure
(All (_ a) (-> (Actor a) (library/lux/control/io.IO .Bit)))
```

### dead

```clojure
(library/lux/control/exception.Exception .Any)
```

### default

```clojure
(All (_ a) (Behavior a a))
```

Default actor behavior\.

### mail\!

```clojure
(All (_ a) (-> (Mail a) (Actor a) (library/lux/control/io.IO (library/lux/control/try.Try .Any))))
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
(actor: .public(stack a)
  {}

  (List a)

  ((on_mail mail state self)
   (do (try.with async.monad)
     [.let[_ (debug.log! "BEFORE")]
      output (mail state self)
      .let[_ (debug.log! "AFTER")]]
     (in output)))

  (message: .public(push {value a} state self)
    (List a)
    (let [state' (#library/lux.Itemvalue state)]
      (async.resolved (#try.Success [state' state'])))))

(actor: .publiccounter
  {}

  Nat

  (message: .public(count! {increment Nat} state self)
    Any
    (let [state' (n.+ increment state)]
      (async.resolved (#try.Success [state' state']))))

  (message: .public(read! state self)
    Nat
    (async.resolved (#try.Success [state state]))))
```

### obituary

```clojure
(All (_ a) (-> (Actor a) (library/lux/control/concurrency/async.Async (Obituary a))))
```

Await for an actor to stop working\.

### obituary'

```clojure
(All (_ a) (-> (Actor a) (library/lux/control/io.IO (.Maybe (Obituary a)))))
```

### observe\!

```clojure
(All (_ a b) (-> (-> a Stop (Mail b)) (library/lux/control/concurrency/frp.Channel a) (Actor b) (library/lux/control/io.IO .Any)))
```

Use an actor to observe a channel by transforming each datum
flowing through the channel into mail the actor can process\.
Can stop observing the channel by executing the Stop value\.

### poison\!

```clojure
(All (_ a) (-> (Actor a) (library/lux/control/io.IO (library/lux/control/try.Try .Any))))
```

Kills the actor by sending mail that will kill it upon processing,
but allows the actor to handle previous mail\.

### poisoned

```clojure
(library/lux/control/exception.Exception .Any)
```

### spawn\!

```clojure
(All (_ a b) (-> (Behavior a b) a (library/lux/control/io.IO (Actor b))))
```

Given a behavior and initial state, spawns an actor and returns it\.

### tell\!

```clojure
(All (_ a b) (-> (Message a b) (Actor a) (library/lux/control/concurrency/async.Async (library/lux/control/try.Try b))))
```

Communicate with an actor through message\-passing\.

___

# library/lux/control/concurrency/async

## Definitions

### \(Async it\)

```clojure
... .Type
(primitive "library/lux/control/concurrency/async.Async" it)
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
(All (_ a) (-> .Nat a (Async a)))
```

Delivers a value after a certain period has passed\.

```clojure
(after milli_seconds value)
```

### and

```clojure
(All (_ a b) (-> (Async a) (Async b) (Async [a b])))
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
(All (_ a) (-> .Any [(Async a) (Resolver a)]))
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
(All (_ a) (-> (Async a) (Async a) (Async a)))
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
(All (_ a) (-> (library/lux/control/io.IO a) (Async a)))
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
(All (_ a b) (-> (Async a) (Async b) (Async (Or a b))))
```

Yields the results of whichever async gets resolved first\.
You can tell which one was resolved first through pattern\-matching\.

```clojure
(or left right)
```

### resolved

```clojure
(All (_ a) (-> a (Async a)))
```

Produces an async that has already been resolved to the given value\.

```clojure
(resolved value)
```

### resolved?

```clojure
(All (_ a) (-> (Async a) (library/lux/control/io.IO .Bit)))
```

Checks whether an async's value has already been resolved\.

### schedule\!

```clojure
(All (_ a) (-> .Nat (library/lux/control/io.IO a) (Async a)))
```

Runs an I/O computation on its own thread \(after a specified delay\)\.
Returns an async that will eventually host its result\.

```clojure
(schedule! milli_seconds computation)
```

### upon\!

```clojure
(All (_ a) (-> (-> a (library/lux/control/io.IO .Any)) (Async a) (library/lux/control/io.IO .Any)))
```

Executes the given function as soon as the async has been resolved\.

```clojure
(upon! function async)
```

### value

```clojure
(All (_ a) (-> (Async a) (library/lux/control/io.IO (.Maybe a))))
```

Polls an async for its value\.

### within

```clojure
(All (_ a) (-> .Nat (Async a) (Async (.Maybe a))))
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
(primitive "library/lux/control/concurrency/atom.Atom" it)
```

Atomic references that are safe to mutate concurrently\.

### atom

```clojure
(All (_ a) (-> a (Atom a)))
```

### compare\_and\_swap\!

```clojure
(All (_ a) (-> a a (Atom a) (library/lux/control/io.IO .Bit)))
```

Only mutates an atom if you can present its current value\.
That guarantees that atom was not updated since you last read from it\.

### read\!

```clojure
(All (_ a) (-> (Atom a) (library/lux/control/io.IO a)))
```

### update\!

```clojure
(All (_ a) (-> (-> a a) (Atom a) (library/lux/control/io.IO [a a])))
```

Updates an atom by applying a function to its current value\.
If it fails to update it \(because some other process wrote to it first\), it will retry until it succeeds\.
The retries will be done with the new values of the atom, as they show up\.

### write\!

```clojure
(All (_ a) (-> a (Atom a) (library/lux/control/io.IO a)))
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
[(library/lux/control/io.IO (library/lux/control/try.Try .Any)) (-> it (library/lux/control/io.IO (library/lux/control/try.Try .Any)))]
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
(All (_ a) (-> .Any [(Channel a) (Sink a)]))
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
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (Channel a) (Channel a)))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Channel)
```

### iterations

```clojure
(All (_ a b) (-> (-> a (library/lux/control/concurrency/async.Async (.Maybe [a b]))) a (Channel b)))
```

### list

```clojure
(All (_ a) (-> (Channel a) (library/lux/control/concurrency/async.Async (.List a))))
```

### mix

```clojure
(All (_ a b) (-> (-> b a (library/lux/control/concurrency/async.Async a)) a (Channel b) (library/lux/control/concurrency/async.Async a)))
```

Asynchronous mix over channels\.

```clojure
(mix f init channel)
```

### mixes

```clojure
(All (_ a b) (-> (-> b a (library/lux/control/concurrency/async.Async a)) a (Channel b) (Channel a)))
```

### monad

```clojure
(library/lux/abstract/monad.Monad Channel)
```

### of\_async

```clojure
(All (_ a) (-> (library/lux/control/concurrency/async.Async a) (Channel a)))
```

A one\-element channel containing the output from an async\.

```clojure
(of_async async)
```

### only

```clojure
(All (_ a) (-> (-> a .Bit) (Channel a) (Channel a)))
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
(All (_ a) (-> .Nat (library/lux/control/io.IO a) [(Channel a) (Sink a)]))
```

### sequential

```clojure
(All (_ a) (-> .Nat (.List a) (Channel a)))
```

Transforms the given list into a channel with the same elements\.

```clojure
(sequential milli_seconds values)
```

### subscribe\!

```clojure
(All (_ a) (-> (Subscriber a) (Channel a) (library/lux/control/io.IO .Any)))
```

___

# library/lux/control/concurrency/semaphore

## Definitions

### Barrier

```clojure
... .Type
(primitive "library/lux/control/concurrency/semaphore.Barrier")
```

A barrier that blocks all processes from proceeding until a given number of processes are parked at the barrier\.

### Limit

```clojure
... .Type
(library/lux/type/refinement.Refined .Nat (primitive "{New Type @"library/lux/control/concurrency/semaphore",126,2 0}"))
```

A limit for barriers\.

### Mutex

```clojure
... .Type
(primitive "library/lux/control/concurrency/semaphore.Mutex")
```

A mutual\-exclusion lock that can only be acquired by one process at a time\.

### Semaphore

```clojure
... .Type
(primitive "library/lux/control/concurrency/semaphore.Semaphore")
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
(library/lux/type/refinement.Refiner .Nat (primitive "{New Type @"library/lux/control/concurrency/semaphore",126,2 0}"))
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
(Ex (_ a) (-> Semaphore (library/lux/control/concurrency/async.Async (library/lux/control/try.Try .Int))))
```

Signal to a semaphore that you're done with your work, and that there is a new open position\.

```clojure
(signal! semaphore)
```

### synchronize\!

```clojure
(All (_ a) (-> Mutex (library/lux/control/io.IO (library/lux/control/concurrency/async.Async a)) (library/lux/control/concurrency/async.Async a)))
```

Runs the procedure with exclusive control of the mutex\.

```clojure
(synchronize! mutex procedure)
```

### wait\!

```clojure
(Ex (_ a) (-> Semaphore (library/lux/control/concurrency/async.Async .Any)))
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
(primitive "library/lux/control/concurrency/stm.Var" it)
```

A mutable cell containing a value, and observers that will be alerted of any change to it\.

### apply

```clojure
(library/lux/abstract/apply.Apply STM)
```

### commit\!

```clojure
(All (_ a) (-> (STM a) (library/lux/control/concurrency/async.Async a)))
```

Commits a transaction and returns its result \(asynchronously\)\.
Note that a transaction may be re\-run an indeterminate number of times if other transactions involving the same variables successfully commit first\.
For this reason, it's important to note that transactions must be free from side\-effects, such as I/O\.

```clojure
(commit! procedure)
```

### follow\!

```clojure
(All (_ a) (-> (Var a) (library/lux/control/io.IO [(library/lux/control/concurrency/frp.Channel a) (library/lux/control/concurrency/frp.Sink a)])))
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
(All (_ a) (-> (Var a) (STM a)))
```

### update

```clojure
(All (_ a) (-> (-> a a) (Var a) (STM [a a])))
```

Update a var's value, and return a tuple with the old and the new values\.

```clojure
(update function var)
```

### var

```clojure
(All (_ a) (-> a (Var a)))
```

Creates a new STM var, with a default value\.

```clojure
(var value)
```

### write

```clojure
(All (_ a) (-> a (Var a) (STM .Any)))
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
(All (_ a) (library/lux/abstract/apply.Apply (All (_ b) (Cont b a))))
```

### continued

```clojure
(All (_ a b) (-> (-> a b) (Cont a b) b))
```

Continues a continuation thunk\.

```clojure
(continued next cont)
```

### functor

```clojure
(All (_ a) (library/lux/abstract/functor.Functor (All (_ b) (Cont b a))))
```

### monad

```clojure
(All (_ a) (library/lux/abstract/monad.Monad (All (_ b) (Cont b a))))
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
(All (_ a b c) (-> a (Cont [(-> a (Cont b c)) a] c)))
```

### reset

```clojure
(All (_ a b) (-> (Cont a a) (Cont a b)))
```

### result

```clojure
(All (_ a) (-> (Cont a a) a))
```

Forces a continuation thunk to be evaluated\.

```clojure
(result cont)
```

### shift

```clojure
(All (_ a) (-> (-> (-> a (Cont a a)) (Cont a a)) (Cont a a)))
```

### with\_current

```clojure
(All (_ a b c) (-> (-> (-> a (Cont b c)) (Cont a c)) (Cont a c)))
```

Call with current continuation\.

```clojure
(with_current
  (function (_ go)
    (do monad
      [.let[nexus (function (nexus val)
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
[.Text (-> it .Text)]
```

An exception provides a way to decorate error messages\.

### assertion

```clojure
(All (_ a) (-> (Exception a) a .Bit (library/lux/control/try.Try .Any)))
```

### error

```clojure
(All (_ a) (-> (Exception a) a .Text))
```

Constructs an error message from an exception\.

```clojure
(error exception message)
```

### except

```clojure
(All (_ a b) (-> (Exception a) a (library/lux/control/try.Try b)))
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

(exception: .publicsome_exception)

................................................................
................................................................

... Complex case:

(exception: .public[arbitrary type variables] (some_exception {optional Text} {arguments Int})
  optional_body)
```

### listing

```clojure
(All (_ a) (-> (-> a .Text) (.List a) .Text))
```

A numbered report of the entries on a list\.
NOTE: 0\-based numbering\.

```clojure
(listing format entries)
```

### match?

```clojure
(All (_ a) (-> (Exception a) .Text .Bit))
```

Is this exception the cause of the error message?

```clojure
(match? exception error)
```

### otherwise

```clojure
(All (_ a) (-> (-> .Text a) (library/lux/control/try.Try a) a))
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
(All (_ a b) (-> (Exception a) (-> .Text b) (library/lux/control/try.Try b) (library/lux/control/try.Try b)))
```

If a particular exception is detected on a possibly\-erroneous value, handle it\.
If no exception was detected, or a different one from the one being checked, then pass along the original value\.

```clojure
(when exception then try)
```

### with

```clojure
(All (_ a b) (-> (Exception a) a (library/lux/control/try.Try b) (library/lux/control/try.Try b)))
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
(All (_ a b c) (-> (-> b c) (-> a b) a c))
```

Function composition\.

```clojure
(= ((composite f g) "foo")
   (f (g "foo")))
```

### constant

```clojure
(All (_ a) (-> a (All (_ b) (-> b a))))
```

Create constant functions\.

```clojure
(= ((constant "foo") "bar")
   "foo")
```

### flipped

```clojure
(All (_ a b c) (-> (-> a b c) b a c))
```

Flips the order of the arguments of a function\.

```clojure
(= ((flipped f) "foo" "bar")
   (f "bar" "foo"))
```

### identity

```clojure
(All (_ a) (-> a a))
```

Identity function\.
Does nothing to its argument and just returns it\.

```clojure
(same? (identity value)
       value)
```

### monoid

```clojure
(All (_ a) (library/lux/abstract/monoid.Monoid (-> a a)))
```

### on

```clojure
(All (_ a b) (-> a (-> a b) b))
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
(All (_ a b) (-> (library/lux/abstract/hash.Hash a) (Memo a b) a b))
```

Memoization confined to a single invocation to the function \(not counting any subsequent recursive invocations\)\.
Memoized results will be re\-used during recursive invocations, but cannot be accessed after the main invocation has ended\.

```clojure
(closed hash memo)
```

### memoization

```clojure
(All (_ a b) (library/lux/control/function/mixin.Mixin a (library/lux/control/state.State (library/lux/data/collection/dictionary.Dictionary a b) b)))
```

### none

```clojure
(All (_ a b) (-> (library/lux/abstract/hash.Hash a) (Memo a b) a b))
```

No memoization at all\.
This is useful as a test control when measuring the effect of using memoization\.

```clojure
(none hash memo)
```

### open

```clojure
(All (_ a b) (-> (Memo a b) [(library/lux/data/collection/dictionary.Dictionary a b) a] [(library/lux/data/collection/dictionary.Dictionary a b) b]))
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
(All (_ a b) (-> (library/lux/abstract/predicate.Predicate a) (Mixin a b) (Mixin a b)))
```

Only apply then mixin when the input meets some criterion\.

```clojure
(advice when then)
```

### after

```clojure
(All (_ a b c) (-> (library/lux/abstract/monad.Monad a) (-> b c (a .Any)) (Mixin b (a c))))
```

Executes an action after doing the main work\.

```clojure
(after monad action)
```

### before

```clojure
(All (_ a b c) (-> (library/lux/abstract/monad.Monad a) (-> b (a .Any)) (Mixin b (a c))))
```

Executes an action before doing the main work\.

```clojure
(before monad action)
```

### fixed

```clojure
(All (_ a b) (-> (Mixin a b) a b))
```

Given a mixin, produces a normal function\.

```clojure
(fixed f)
```

### mixed

```clojure
(All (_ a b) (-> (Mixin a b) (Mixin a b) (Mixin a b)))
```

Produces a new mixin, where the behavior of the child can make use of the behavior of the parent\.

```clojure
(mixed parent child)
```

### monoid

```clojure
(All (_ a b) (library/lux/abstract/monoid.Monoid (Mixin a b)))
```

### nothing

```clojure
Mixin
```

A mixin that does nothing and just delegates work to the next mixin\.

### of\_recursive

```clojure
(All (_ a b) (-> (Recursive a b) (Mixin a b)))
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
  [.public(even? number)
   (-> Nat Bit)
   (case number
     0 true
     _ (odd? (-- number)))]

  [.public(odd? number)
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
(primitive "library/lux/control/io.IO" it)
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
(All (_ a b) (-> (IO b) b))
```

A way to execute IO computations and perform their side\-effects\.

___

# library/lux/control/lazy

## Definitions

### \(Lazy it\)

```clojure
... .Type
(primitive "library/lux/control/lazy.Lazy" it)
```

A value specified by an expression that is calculated only at the last moment possible\.
Afterwards, the value is cached for future reference\.

### apply

```clojure
(library/lux/abstract/apply.Apply Lazy)
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (Lazy a))))
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
(All (_ a) (-> (Lazy a) a))
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
if a \(Maybe x\) value turns out to be \#\.None\.
Note: the expression for the default value will not be computed if the base computation succeeds\.

```clojure
(else +20 (#library/lux.Some+10))

... =>

+10

................................................................
................................................................

(else +20 #library/lux.None)

... =>

+20
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (.Maybe a))))
```

### functor

```clojure
(library/lux/abstract/functor.Functor .Maybe)
```

### hash

```clojure
(All (_ a) (-> (library/lux/abstract/hash.Hash a) (library/lux/abstract/hash.Hash (.Maybe a))))
```

### lifted

```clojure
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (a b) (a (.Maybe b))))
```

Wraps a monadic value with Maybe machinery\.

```clojure
(lifted monad)
```

### list

```clojure
(All (_ a) (-> (.Maybe a) (.List a)))
```

### monad

```clojure
(library/lux/abstract/monad.Monad .Maybe)
```

### monoid

```clojure
(All (_ a) (library/lux/abstract/monoid.Monoid (.Maybe a)))
```

### trusted

```clojure
(All (_ a) (-> (.Maybe a) a))
```

Assumes that a Maybe value is a \#\.Some and yields its value\.
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
   when   (passes_test? value)]
  (do_something_else 4 5 6))
```

### with

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (library/lux/abstract/monad.Monad (All (_ b) (a (.Maybe b))))))
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
(All (_ a b c) (-> (Parser a b) (Parser a c) (Parser a c)))
```

Run the parser after another one \(whose output is ignored\)\.

```clojure
(after param subject)
```

### and

```clojure
(All (_ a b c) (-> (Parser a b) (Parser a c) (Parser a [b c])))
```

Sequencing combinator\.

```clojure
(and first second)
```

### apply

```clojure
(All (_ a) (library/lux/abstract/apply.Apply (Parser a)))
```

### assertion

```clojure
(All (_ a) (-> .Text .Bit (Parser a .Any)))
```

Fails with the given message if the test is \#0\.

```clojure
(assertion message test)
```

### at\_least

```clojure
(All (_ a b) (-> .Nat (Parser a b) (Parser a (.List b))))
```

Parse at least N times\.

```clojure
(at_least amount parser)
```

### at\_most

```clojure
(All (_ a b) (-> .Nat (Parser a b) (Parser a (.List b))))
```

Parse at most N times\.

```clojure
(at_most amount parser)
```

### before

```clojure
(All (_ a b c) (-> (Parser a b) (Parser a c) (Parser a c)))
```

Run the parser before another one \(whose output is ignored\)\.

```clojure
(before param subject)
```

### between

```clojure
(All (_ a b) (-> .Nat .Nat (Parser a b) (Parser a (.List b))))
```

```clojure
(between minimum additional parser)
```

### codec

```clojure
(All (_ a b c) (-> (library/lux/abstract/codec.Codec b c) (Parser a b) (Parser a c)))
```

Decode the output of a parser using a codec\.

```clojure
(codec codec parser)
```

### either

```clojure
(All (_ a b) (-> (Parser a b) (Parser a b) (Parser a b)))
```

Homogeneous alternative combinator\.

```clojure
(either this that)
```

### else

```clojure
(All (_ a b) (-> b (Parser a b) (Parser a b)))
```

If the given parser fails, returns the default value\.

```clojure
(else value parser)
```

### exactly

```clojure
(All (_ a b) (-> .Nat (Parser a b) (Parser a (.List b))))
```

Parse exactly N times\.

```clojure
(exactly amount parser)
```

### failure

```clojure
(All (_ a b) (-> .Text (Parser a b)))
```

Always fail with this 'message'\.

```clojure
(failure message)
```

### functor

```clojure
(All (_ a) (library/lux/abstract/functor.Functor (Parser a)))
```

### lifted

```clojure
(All (_ a b) (-> (library/lux/control/try.Try b) (Parser a b)))
```

Lift a potentially failed computation into a parser\.

```clojure
(lifted operation)
```

### many

```clojure
(All (_ a b) (-> (Parser a b) (Parser a (.List b))))
```

1\-or\-more combinator\.

```clojure
(many parser)
```

### maybe

```clojure
(All (_ a b) (-> (Parser a b) (Parser a (.Maybe b))))
```

Optionality combinator\.

```clojure
(maybe parser)
```

### monad

```clojure
(All (_ a) (library/lux/abstract/monad.Monad (Parser a)))
```

### not

```clojure
(All (_ a b) (-> (Parser a b) (Parser a .Any)))
```

Only succeeds when the underlying parser fails\.

```clojure
(not parser)
```

### only

```clojure
(All (_ a b) (-> (-> b .Bit) (Parser a b) (Parser a b)))
```

Only succeed when the parser's output passes a test\.

```clojure
(only test parser)
```

### or

```clojure
(All (_ a b c) (-> (Parser a b) (Parser a c) (Parser a (Or b c))))
```

Heterogeneous alternative combinator\.

```clojure
(or left right)
```

### parses

```clojure
(All (_ a b) (-> (Parser a b) (Parser a .Any)))
```

Ignore a parser's output and just execute it\.

```clojure
(parses parser)
```

### parses?

```clojure
(All (_ a b) (-> (Parser a b) (Parser a .Bit)))
```

Ignore a parser's output and just verify that it succeeds\.

```clojure
(parses? parser)
```

### rec

```clojure
(All (_ a b) (-> (-> (Parser a b) (Parser a b)) (Parser a b)))
```

Combinator for recursive parsers\.

```clojure
(rec parser)
```

### remaining

```clojure
(All (_ a) (Parser a a))
```

Yield the remaining input \(without consuming it\)\.

### result

```clojure
(All (_ a b) (-> (Parser a b) a (library/lux/control/try.Try [a b])))
```

Executes the parser on the input\.
Does not verify that all of the input has been consumed by the parser\.
Returns both the parser's output, and a value that represents the remaining input\.

```clojure
(result parser input)
```

### separated\_by

```clojure
(All (_ a b c) (-> (Parser a c) (Parser a b) (Parser a (.List b))))
```

Parses instances of 'parser' that are separated by instances of 'separator'\.

```clojure
(separated_by separator parser)
```

### some

```clojure
(All (_ a b) (-> (Parser a b) (Parser a (.List b))))
```

0\-or\-more combinator\.

```clojure
(some parser)
```

### speculative

```clojure
(All (_ a b) (-> (Parser a b) (Parser a b)))
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
(Parser .Name)
```

Queries for a constant value\.

### constant\!

```clojure
(-> .Name (Parser .Any))
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
(All (_ a) (-> (Parser a) (.List library/lux/tool/compiler/language/lux/analysis.Analysis) (library/lux/control/try.Try a)))
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
(All (_ a) (-> (Parser a) (Parser a)))
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
(primitive "#I64" (primitive "#Nat"))
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
(primitive "#I64" (primitive "#Nat"))
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
(All (_ a) (-> (Parser a) (Parser (.List a))))
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
(All (_ a) (-> (Parser a) (Parser (.Maybe a))))
```

### name

```clojure
(Parser .Name)
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
(All (_ a b) (-> (Parser a) (Parser b) (Parser (Or a b))))
```

### rec

```clojure
(All (_ a) (-> (-> (Parser a) (Parser a)) (Parser a)))
```

Tie the knot for a recursive parser\.

### remaining

```clojure
(Parser .Nat)
```

How much of the data remains to be read\.

### result

```clojure
(All (_ a) (-> (Parser a) library/lux/data/binary.Binary (library/lux/control/try.Try a)))
```

Runs a parser and checks that all the binary data was read by it\.

```clojure
(result parser input)
```

### rev

```clojure
(Parser .Rev)
```

### row/16

```clojure
(All (_ a) (-> (Parser a) (Parser (library/lux/data/collection/row.Row a))))
```

Parses a row of values prefixed with a size that is 16 bytes long\.

### row/32

```clojure
(All (_ a) (-> (Parser a) (Parser (library/lux/data/collection/row.Row a))))
```

Parses a row of values prefixed with a size that is 32 bytes long\.

### row/64

```clojure
(All (_ a) (-> (Parser a) (Parser (library/lux/data/collection/row.Row a))))
```

Parses a row of values prefixed with a size that is 64 bytes long\.

### row/8

```clojure
(All (_ a) (-> (Parser a) (Parser (library/lux/data/collection/row.Row a))))
```

Parses a row of values prefixed with a size that is 8 bytes long\.

### segment

```clojure
(-> .Nat (Parser library/lux/data/binary.Binary))
```

Parses a chunk of data of a given size\.

```clojure
(segment size)
```

### set

```clojure
(All (_ a) (-> (library/lux/abstract/hash.Hash a) (Parser a) (Parser (library/lux/data/collection/set.Set a))))
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

## Missing documentation

1. `` or/5 ``

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
(All (_ a) (-> .Text (Parser a) (Parser a)))
```

Parses a named parameter and yields its value\.

```clojure
(named name value)
```

### parameter

```clojure
(All (_ a) (-> [.Text .Text] (Parser a) (Parser a)))
```

Parses a parameter that can have either a short or a long name\.

```clojure
(parameter [short long] value)
```

### parse

```clojure
(All (_ a) (-> (-> .Text (library/lux/control/try.Try a)) (Parser a)))
```

Parses the next input with a parsing function\.

```clojure
(parse parser)
```

### result

```clojure
(All (_ a) (-> (Parser a) (.List .Text) (library/lux/control/try.Try a)))
```

Executes the parser and verifies that all inputs are processed\.

```clojure
(result parser inputs)
```

### somewhere

```clojure
(All (_ a) (-> (Parser a) (Parser a)))
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
(All (_ a) (-> (Parser a) (Parser a)))
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

### identifier

```clojure
(Parser .Name)
```

Parses the next identifier input\.

### identifier\!

```clojure
(-> .Name (Parser .Any))
```

Checks for a specific identifier input\.

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
(All (_ a) (-> (.List .Code) (Parser a) (Parser a)))
```

Runs parser against the given list of inputs\.

```clojure
(local inputs parser)
```

### local\_identifier

```clojure
(Parser .Text)
```

Parse a local local identifier \(a local identifier that has no module prefix\)\.

### local\_identifier\!

```clojure
(-> .Text (Parser .Any))
```

Checks for a specific local local identifier \(a local identifier that has no module prefix\)\.

### local\_tag

```clojure
(Parser .Text)
```

Parse a local local tag \(a local tag that has no module prefix\)\.

### local\_tag\!

```clojure
(-> .Text (Parser .Any))
```

Checks for a specific local local tag \(a local tag that has no module prefix\)\.

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

### record

```clojure
(All (_ a) (-> (Parser a) (Parser a)))
```

Parses the contents of a record\.

### result

```clojure
(All (_ a) (-> (Parser a) (.List .Code) (library/lux/control/try.Try a)))
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

### tag

```clojure
(Parser .Name)
```

Parses the next tag input\.

### tag\!

```clojure
(-> .Name (Parser .Any))
```

Checks for a specific tag input\.

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
(All (_ a) (-> (Parser a) (Parser a)))
```

Parses the contents of a tuple\.

## Missing documentation

1. `` next ``
1. `` not ``

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
(primitive "#Text")
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
(All (_ a) (-> (Parser a) Environment (library/lux/control/try.Try a)))
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
(All (_ a) (-> (Parser a) (Parser a)))
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

```clojure
(

    boolean!         test)
```

### boolean?

```clojure
(-> library/lux/data/format/json.Boolean (Parser .Bit))
```

Asks whether a JSON value is a boolean\.

```clojure
       (




boolean?               test)
```

### dictionary

```clojure
(All (_ a) (-> (Parser a) (Parser (library/lux/data/collection/dictionary.Dictionary .Text a))))
```

Parses a dictionary\-like JSON object\.

### empty\_input

```clojure
(library/lux/control/exception.Exception .Any)
```

### field

```clojure
(All (_ a) (-> .Text (Parser a) (Parser a)))
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
(All (_ a) (-> (Parser a) (Parser (.Maybe a))))
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

```clojure
(


    number!         test)
```

### number?

```clojure
(-> library/lux/data/format/json.Number (Parser .Bit))
```

Asks whether a JSON value is a number\.

```clojure
       (





number?               test)
```

### object

```clojure
(All (_ a) (-> (Parser a) (Parser a)))
```

Parses the contents of a JSON object\.
Use this with the 'field' combinator\.

```clojure
(object parser)
```

### result

```clojure
(All (_ a) (-> (Parser a) library/lux/data/format/json.JSON (library/lux/control/try.Try a)))
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

```clojure
(



    string!         test)
```

### string?

```clojure
(-> library/lux/data/format/json.String (Parser .Bit))
```

Asks whether a JSON value is a string\.

```clojure
       (






string?               test)
```

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
(All (_ a) (library/lux/control/exception.Exception [library/lux/data/format/json.JSON library/lux/data/format/json.JSON]))
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

```clojure
 (

bit!              expected)
```

### cannot\_parse

```clojure
(library/lux/control/exception.Exception (.List library/lux/tool/compiler/language/lux/synthesis.Synthesis))
```

### constant

```clojure
(Parser .Name)
```

Queries for a constant synthesis node\.

### constant\!

```clojure
(-> .Name (Parser .Any))
```

Checks for a specific constant synthesis node\.

```clojure
(







    constant!             expected)
```

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

```clojure
 (



f64!              expected)
```

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

```clojure
(






   foreign!             expected)
```

### function

```clojure
(All (_ a) (-> library/lux/tool/compiler/arity.Arity (Parser a) (Parser [(library/lux/tool/compiler/language/lux/analysis.Environment library/lux/tool/compiler/language/lux/synthesis.Synthesis) a])))
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

```clojure
 (


i64!              expected)
```

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

```clojure
(





 local!             expected)
```

### loop

```clojure
(All (_ a b) (-> (Parser a) (Parser b) (Parser [library/lux/tool/compiler/reference/variable.Register a b])))
```

Parses the initial values and the body of a loop\.

```clojure
(loop init_parsers iteration_parser)
```

### result

```clojure
(All (_ a) (-> (Parser a) (.List library/lux/tool/compiler/language/lux/synthesis.Synthesis) (library/lux/control/try.Try a)))
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

```clojure
(




text!             expected)
```

### tuple

```clojure
(All (_ a) (-> (Parser a) (Parser a)))
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
(primitive "#I64" (primitive "#Nat"))
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
[Offset Offset]
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

```clojure
       (






at_least               amount parser)
```

### at\_least\!

```clojure
(-> .Nat (Parser Slice) (Parser Slice))
```

Yields at least N characters \(as a slice\)\.

```clojure
(



    at_least!         amount parser)
```

### at\_most

```clojure
(-> .Nat (Parser .Text) (Parser .Text))
```

Yields at most N characters\.

```clojure
       (





at_most               amount parser)
```

### at\_most\!

```clojure
(-> .Nat (Parser Slice) (Parser Slice))
```

Yields at most N characters \(as a slice\)\.

```clojure
(


    at_most!         amount parser)
```

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
(All (_ a) (-> [.Text .Text] (Parser a) (Parser a)))
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

```clojure
       (




exactly               amount parser)
```

### exactly\!

```clojure
(-> .Nat (Parser Slice) (Parser Slice))
```

Yields exactly N characters \(as a slice\)\.

```clojure
(

    exactly!         amount parser)
```

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
(All (_ a) (-> .Text (Parser a) (Parser a)))
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

```clojure
       (





many               parser)
```

### many\!

```clojure
(-> (Parser Slice) (Parser Slice))
```

Yields <name> characters as a single continuous text \(as a slice\)\.

```clojure
(


many!         parser)
```

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

```clojure
   (


none_of           options)
```

### none\_of\!

```clojure
(-> .Text (Parser Slice))
```

Yields characters \(as a slice\) that are not part of a piece of text\.

```clojure
   (


none_of!           options)
```

### not

```clojure
(All (_ a) (-> (Parser a) (Parser .Text)))
```

Produce a character if the parser fails\.

```clojure
       (

not               parser)
```

### not\!

```clojure
(All (_ a) (-> (Parser a) (Parser Slice)))
```

Produce a character \(as a slice\) if the parser fails\.

```clojure
       (


not!               parser)
```

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

```clojure
   (

one_of           options)
```

### one\_of\!

```clojure
(-> .Text (Parser Slice))
```

Yields characters \(as a slice\) that are part of a piece of text\.

```clojure
   (

one_of!           options)
```

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
(All (_ a) (-> (Parser a) .Text (library/lux/control/try.Try a)))
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

```clojure
       (




some               parser)
```

### some\!

```clojure
(-> (Parser Slice) (Parser Slice))
```

Yields <name> characters as a single continuous text \(as a slice\)\.

```clojure
(

some!         parser)
```

### space

```clojure
(Parser .Text)
```

Yields white\-space\.

### then

```clojure
(All (_ a b) (-> (Parser b) (library/lux/control/parser.Parser a .Text) (library/lux/control/parser.Parser a b)))
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
(All (_ b) (library/lux/control/parser.Parser (library/lux/data/collection/tree/zipper.Zipper it) b))
```

A parser of arbitrary trees\.

### cannot\_move\_further

```clojure
(library/lux/control/exception.Exception .Any)
```

### down

```clojure
(All (_ a) (Parser a .Any))
```

Move down\.

### end

```clojure
(All (_ a) (Parser a .Any))
```

Move to the last node\.

### left

```clojure
(All (_ a) (Parser a .Any))
```

Move to the left\.

### leftmost

```clojure
(All (_ a) (Parser a .Any))
```

Move to the leftmost node\.

### next

```clojure
(All (_ a) (Parser a .Any))
```

Move to the next node\.

### previous

```clojure
(All (_ a) (Parser a .Any))
```

Move to the previous node\.

### result

```clojure
(All (_ a b) (-> (Parser a b) (library/lux/data/collection/tree.Tree a) (library/lux/control/try.Try b)))
```

Applies the parser against a tree\.

```clojure
(result parser tree)
```

### result'

```clojure
(All (_ a b) (-> (Parser a b) (library/lux/data/collection/tree/zipper.Zipper a) (library/lux/control/try.Try b)))
```

Applies the parser against a tree zipper\.

```clojure
(result' parser zipper)
```

### right

```clojure
(All (_ a) (Parser a .Any))
```

Move to the right\.

### rightmost

```clojure
(All (_ a) (Parser a .Any))
```

Move to the rightmost node\.

### start

```clojure
(All (_ a) (Parser a .Any))
```

Move to the root node\.

### up

```clojure
(All (_ a) (Parser a .Any))
```

Move up\.

### value

```clojure
(All (_ a) (Parser a a))
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
(All (_ a) (-> (Parser a) (Parser a)))
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

```clojure
   (

exactly           expected)
```

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
(All (_ a b) (-> (Parser a) (Parser b) (Parser [a b])))
```

Parses a function's inputs and output\.

```clojure
(function in_poly out_poly)
```

### local

```clojure
(All (_ a) (-> (.List .Type) (Parser a) (Parser a)))
```

Apply a parser to the given inputs\.

```clojure
(local types poly)
```

### named

```clojure
(Parser [.Name .Type])
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
(All (_ a) (-> (Parser a) (Parser [.Code (.List .Code) a])))
```

```clojure
(polymorphic poly)
```

### recursive

```clojure
(All (_ a) (-> (Parser a) (Parser [.Code a])))
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
(All (_ a) (-> (Parser a) .Type (library/lux/control/try.Try a)))
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

```clojure
   (


sub           expected)
```

### super

```clojure
(-> .Type (Parser .Any))
```

Parses a super type\.

```clojure
   (



super           expected)
```

### tuple

```clojure
(All (_ a) (-> (Parser a) (Parser a)))
```

Parses the contents of a tuple type\.

```clojure
       (


tuple               poly)
```

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
(All (_ a) (-> (Parser a) (Parser a)))
```

Parses the contents of a variant type\.

```clojure
       (

variant               poly)
```

### with\_extension

```clojure
(All (_ a) (-> .Type (Parser a) (Parser [.Code a])))
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
(All (_ a) (-> library/lux/data/format/xml.Tag (Parser a) (Parser a)))
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
(All (_ a) (-> (Parser a) (.List library/lux/data/format/xml.XML) (library/lux/control/try.Try a)))
```

Applies a parser against a stream of XML documents\.
Verifies that all of the inputs are consumed by the parser\.

```clojure
(result parser documents)
```

### somewhere

```clojure
(All (_ a) (-> (Parser a) (Parser a)))
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
    (exec> [.nat%n log!])
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
            [i\encoded]))

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
(All (_ a) (library/lux/abstract/apply.Apply (Reader a)))
```

### functor

```clojure
(All (_ a) (library/lux/abstract/functor.Functor (Reader a)))
```

### lifted

```clojure
(All (_ a b c) (-> (a c) (Reader b (a c))))
```

Lift monadic values to the Reader wrapper\.

### local

```clojure
(All (_ a b) (-> (-> a a) (Reader a b) (Reader a b)))
```

Run computation with a locally\-modified environment\.

```clojure
(local change proc)
```

### monad

```clojure
(All (_ a) (library/lux/abstract/monad.Monad (Reader a)))
```

### read

```clojure
(All (_ a) (Reader a a))
```

Get the environment\.

### result

```clojure
(All (_ a b) (-> a (Reader a b) b))
```

Executes the reader against the given environment\.

```clojure
(result env proc)
```

### with

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (All (_ b) (library/lux/abstract/monad.Monad (All (_ c) (Reader b (a c)))))))
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
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (-> b (a (library/lux/control/try.Try .Any))) b (All (_ c) (Region c a b))))
```

Acquire a resource while pairing it a function that knows how to reclaim it\.

```clojure
(acquire! monad cleaner value)
```

### apply

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (All (_ b) (library/lux/abstract/apply.Apply (Region b a)))))
```

### clean\_up\_error

```clojure
(All (_ a) (library/lux/control/exception.Exception [.Text (library/lux/control/try.Try a)]))
```

### except

```clojure
(All (_ a b c) (-> (library/lux/abstract/monad.Monad a) (library/lux/control/exception.Exception b) b (All (_ d) (Region d a c))))
```

Fail by throwing/raising an exception\.

```clojure
(except monad exception message)
```

### failure

```clojure
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) .Text (All (_ c) (Region c a b))))
```

Immediately fail with this 'message'\.

```clojure
(failure monad error)
```

### functor

```clojure
(All (_ a) (-> (library/lux/abstract/functor.Functor a) (All (_ b) (library/lux/abstract/functor.Functor (Region b a)))))
```

### lifted

```clojure
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (a b) (All (_ c) (Region c a b))))
```

Lift an effectful computation into a region\-based computation\.

```clojure
(lifted monad operation)
```

### monad

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (All (_ b) (library/lux/abstract/monad.Monad (Region b a)))))
```

### run\!

```clojure
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (All (_ c) (Region c a b)) (a (library/lux/control/try.Try b))))
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
          (fix_me

















           "2022-04-01"
           "Do this, that and the other.")

................................................................
................................................................

          (fix_me




















           "2022-04-01"
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
          (remember

















           "2022-04-01"
           "Do this, that and the other.")

................................................................
................................................................

          (remember




















           "2022-04-01"
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
          (to_do

















           "2022-04-01"
           "Do this, that and the other.")

................................................................
................................................................

          (to_do




















           "2022-04-01"
           "Improve the performace."
           (some (complicated (computation 123))))
```

___

# library/lux/control/security/capability

## Definitions

### \(Capability brand input output\)

```clojure
... .Type
(primitive "library/lux/control/security/capability.Capability" brand input output)
```

Represents the capability to perform an operation\.
This operation is assumed to have security implications\.

### async

```clojure
(All (_ a b c) (-> (Capability a b (library/lux/control/io.IO c)) (Capability a b (library/lux/control/concurrency/async.Async c))))
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
(All (_ a b c) (-> (Capability a b c) b c))
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
(All (_ d) (-> (Policy brand d %from) (Policy brand d %to)))
```

Represents the act of delegating policy capacities\.

### \(Policy brand value %\)

```clojure
... .Type
(primitive "library/lux/control/security/policy.Policy" brand value %)
```

A security policy encoded as the means to 'upgrade' or 'downgrade' in a secure context\.

### Privacy

```clojure
... .Type
(primitive "library/lux/control/security/policy.Privacy")
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
[(Can_Upgrade brand %) (Can_Downgrade brand %)]
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
(primitive "library/lux/control/security/policy.Safety")
```

A security context for safety\.
Safe data is data coming from outside the program which can be trusted to be properly formatted and lacking injections\.

### apply

```clojure
(All (_ a b) (library/lux/abstract/apply.Apply (All (_ c) (Policy a c b))))
```

### delegation

```clojure
(All (_ a b c) (-> (Can_Downgrade a b) (Can_Upgrade a c) (Delegation a b c)))
```

Delegating policy capacities\.

```clojure
(delegation downgrade upgrade)
```

### functor

```clojure
(All (_ a b) (library/lux/abstract/functor.Functor (All (_ c) (Policy a c b))))
```

### monad

```clojure
(All (_ a b) (library/lux/abstract/monad.Monad (All (_ c) (Policy a c b))))
```

### with\_policy

```clojure
(All (_ a b) (Ex (_ c) (-> (Context a b c) (b c))))
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
(All (+State a b c) (-> b (a [b c])))
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
(All (_ a) (library/lux/abstract/apply.Apply (State a)))
```

### do\_while

```clojure
(All (_ a) (-> (State a .Bit) (State a .Any) (State a .Any)))
```

A stateful do\-while loop\.

```clojure
(do_while condition body)
```

### functor

```clojure
(All (_ a) (library/lux/abstract/functor.Functor (State a)))
```

### get

```clojure
(All (_ a) (State a a))
```

Read the current state\.

### lifted

```clojure
(All (_ a b c) (-> (library/lux/abstract/monad.Monad a) (a c) (+State a b c)))
```

Lift monadic values to the \+State wrapper\.

```clojure
(lifted monad ma)
```

### local

```clojure
(All (_ a b) (-> (-> a a) (State a b) (State a b)))
```

Run the computation with a locally\-modified state\.

```clojure
(local change action)
```

### monad

```clojure
(All (_ a) (library/lux/abstract/monad.Monad (State a)))
```

### put

```clojure
(All (_ a) (-> a (State a .Any)))
```

Set the new state\.

```clojure
(put new_state)
```

### result

```clojure
(All (_ a b) (-> a (State a b) [a b]))
```

Run a stateful computation\.

```clojure
(result state action)
```

### result'

```clojure
(All (_ a b c) (-> b (+State a b c) (a [b c])))
```

Execute a stateful computation decorated by a monad\.

```clojure
(result' state action)
```

### update

```clojure
(All (_ a) (-> (-> a a) (State a .Any)))
```

Compute the new state\.

```clojure
(update change)
```

### use

```clojure
(All (_ a b) (-> (-> a b) (State a b)))
```

Run a function on the current state\.

```clojure
(use user)
```

### while

```clojure
(All (_ a) (-> (State a .Bit) (State a .Any) (State a .Any)))
```

A stateful while loop\.

```clojure
(while condition body)
```

### with

```clojure
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (library/lux/abstract/monad.Monad (+State a b))))
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
(primitive "library/lux/control/thread.Box" ! it)
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
(All (_ a) (library/lux/abstract/apply.Apply (Thread a)))
```

### box

```clojure
(All (_ a) (-> a (All (_ b) (Thread b (Box b a)))))
```

A brand\-new box initialized to the given value\.

```clojure
(box init)
```

### functor

```clojure
(All (_ a) (library/lux/abstract/functor.Functor (Thread a)))
```

### io

```clojure
(All (_ a) (-> (All (_ b) (Thread b a)) (library/lux/control/io.IO a)))
```

Transforms the imperative thread into an I/O computation\.

### monad

```clojure
(All (_ a) (library/lux/abstract/monad.Monad (Thread a)))
```

### read\!

```clojure
(All (_ a b) (-> (Box a b) (Thread a b)))
```

Reads the current value in the box\.

```clojure
(read! box)
```

### result

```clojure
(All (_ a) (-> (All (_ b) (Thread b a)) a))
```

Executes the imperative thread in a self\-contained way\.

```clojure
(result thread)
```

### update\!

```clojure
(All (_ a b) (-> (-> a a) (Box b a) (Thread b a)))
```

Update a box's value by applying a function to it\.

```clojure
(update! f box)
```

### write\!

```clojure
(All (_ a) (-> a (All (_ b) (-> (Box b a) (Thread b .Any)))))
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
(Or .Text it)
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
   (else "foo" (#documentation/lux/control/try.Success"bar")))

................................................................
................................................................

(= "foo"
   (else "foo" (#documentation/lux/control/try.Failure"KABOOM!")))
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (Try a))))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Try)
```

### lifted

```clojure
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (a b) (a (Try b))))
```

Wraps a monadic value with error\-handling machinery\.

```clojure
(lifted monad)
```

### maybe

```clojure
(All (_ a) (-> (Try a) (.Maybe a)))
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
(All (_ a) (-> (.Maybe a) (Try a)))
```

```clojure
(of_maybe maybe)
```

### trusted

```clojure
(All (_ a) (-> (Try a) a))
```

Assumes a Try value succeeded, and yields its value\.
If it didn't, raises the error as a runtime error\.
WARNING: Use with caution\.

```clojure
(trusted try)
```

### with

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (library/lux/abstract/monad.Monad (All (_ b) (a (Try b))))))
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
[log value]
```

Represents a value with an associated 'log' to record arbitrary information\.

### apply

```clojure
(All (_ a) (-> (library/lux/abstract/monoid.Monoid a) (library/lux/abstract/apply.Apply (Writer a))))
```

### functor

```clojure
(All (_ a) (library/lux/abstract/functor.Functor (Writer a)))
```

### lifted

```clojure
(All (_ a b c) (-> (library/lux/abstract/monoid.Monoid a) (library/lux/abstract/monad.Monad b) (b c) (b (Writer a c))))
```

Wraps a monadic value with Writer machinery\.

```clojure
(lifted monoid monad)
```

### monad

```clojure
(All (_ a) (-> (library/lux/abstract/monoid.Monoid a) (library/lux/abstract/monad.Monad (Writer a))))
```

### with

```clojure
(All (_ a b) (-> (library/lux/abstract/monoid.Monoid a) (library/lux/abstract/monad.Monad b) (library/lux/abstract/monad.Monad (All (_ c) (b (Writer a c))))))
```

Enhances a monad with Writer functionality\.

```clojure
(with monoid monad)
```

### write

```clojure
(All (_ a) (-> a (Writer a .Any)))
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
(primitive "[B")
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
(All (_ a) (-> (-> .I64 a a) a Binary a))
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

___

# library/lux/data/bit

## Definitions

### codec

```clojure
(library/lux/abstract/codec.Codec .Text .Bit)
```

### complement

```clojure
(All (_ a) (-> (-> a .Bit) a .Bit))
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
(primitive "#Array" it)
```

Mutable arrays\.

### any?

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (library/lux/abstract/predicate.Predicate (Array a))))
```

### clone

```clojure
(All (_ a) (-> (Array a) (Array a)))
```

Yields a shallow clone of the array\.

```clojure
(clone xs)
```

### contains?

```clojure
(All (_ a) (-> .Nat (Array a) .Bit))
```

```clojure
(contains? index array)
```

### copy\!

```clojure
(All (_ a) (-> .Nat .Nat (Array a) .Nat (Array a) (Array a)))
```

Writes the contents of one array into the other\.

```clojure
(copy! length src_start src_array dest_start dest_array)
```

### delete\!

```clojure
(All (_ a) (-> .Nat (Array a) (Array a)))
```

Mutate the array by deleting the value at the specified index\.

```clojure
(delete! index array)
```

### empty

```clojure
(All (_ a) (-> .Nat (Array a)))
```

An empty array of the specified size\.

```clojure
(empty size)
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (Array a))))
```

### every?

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (library/lux/abstract/predicate.Predicate (Array a))))
```

### example

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (Array a) (.Maybe a)))
```

Yields the first item in the array that satisfies the predicate\.

```clojure
(example p xs)
```

### example\+

```clojure
(All (_ a) (-> (-> .Nat a .Bit) (Array a) (.Maybe [.Nat a])))
```

Just like 'example', but with access to the index of each value\.

```clojure
(example+ p xs)
```

### filter\!

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (Array a) (Array a)))
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
(All (_ a) (-> (.Maybe a) (Array a) (.List a)))
```

Yields a list with every non\-empty item in the array\.
Can use the optional default value when encountering an empty cell in the array\.

```clojure
(list #library/lux.Nonearray)

(list (#library/lux.Somedefault) array)
```

### mix

```clojure
(library/lux/abstract/mix.Mix Array)
```

### monoid

```clojure
(All (_ a) (library/lux/abstract/monoid.Monoid (Array a)))
```

### occupancy

```clojure
(All (_ a) (-> (Array a) .Nat))
```

Finds out how many cells in an array are occupied\.

```clojure
(occupancy array)
```

### of\_list

```clojure
(All (_ a) (-> (.List a) (Array a)))
```

```clojure
(of_list xs)
```

### read\!

```clojure
(All (_ a) (-> .Nat (Array a) (.Maybe a)))
```

```clojure
(read! index array)
```

### size

```clojure
(All (_ a) (-> (Array a) .Nat))
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
(All (_ a) (-> .Nat (-> a a) (Array a) (Array a)))
```

Mutate the array by updating the value at the specified index\.

```clojure
(update! index transform array)
```

### upsert\!

```clojure
(All (_ a) (-> .Nat a (-> a a) (Array a) (Array a)))
```

Mutate the array by updating the value at the specified index\.
If there is no value, update and write the default value given\.

```clojure
(upsert! index default transform array)
```

### vacancy

```clojure
(All (_ a) (-> (Array a) .Nat))
```

Finds out how many cells in an array are vacant\.

```clojure
(vacancy array)
```

### write\!

```clojure
(All (_ a) (-> .Nat a (Array a) (Array a)))
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
(All (Chunk a) (primitive "#I64" a))
```

### and

```clojure
(-> Bits Bits Bits)
```

```clojure
   (

and           param subject)
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

```clojure
   (



flipped           index input)
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

```clojure
   (

one           index input)
```

### or

```clojure
(-> Bits Bits Bits)
```

```clojure
   (


or           param subject)
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

```clojure
   (



xor           param subject)
```

### zero

```clojure
(-> .Nat Bits Bits)
```

```clojure
   (


zero           index input)
```

___

# library/lux/data/collection/dictionary

## Definitions

### \(Dictionary key value\)

```clojure
... .Type
[(library/lux/abstract/hash.Hash key) (Node key value)]
```

A dictionary implemented as a Hash\-Array Mapped Trie \(HAMT\)\.

### empty

```clojure
(All (_ a b) (-> (library/lux/abstract/hash.Hash a) (Dictionary a b)))
```

An empty dictionary\.

```clojure
(empty key_hash)
```

### empty?

```clojure
(All (_ a b) (-> (Dictionary a b) .Bit))
```

### entries

```clojure
(All (_ a b) (-> (Dictionary a b) (.List [a b])))
```

### equivalence

```clojure
(All (_ a b) (-> (library/lux/abstract/equivalence.Equivalence b) (library/lux/abstract/equivalence.Equivalence (Dictionary a b))))
```

### functor

```clojure
(All (_ a) (library/lux/abstract/functor.Functor (Dictionary a)))
```

### has

```clojure
(All (_ a b) (-> a b (Dictionary a b) (Dictionary a b)))
```

```clojure
(has key val dict)
```

### has'

```clojure
(All (_ a b) (-> a b (Dictionary a b) (library/lux/control/try.Try (Dictionary a b))))
```

Only puts the KV\-pair if the key is not already present\.

```clojure
(has' key val dict)
```

### key?

```clojure
(All (_ a b) (-> (Dictionary a b) a .Bit))
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
(All (_ a b) (-> (Dictionary a b) (library/lux/abstract/hash.Hash a)))
```

### keys

```clojure
(All (_ a b) (-> (Dictionary a b) (.List a)))
```

### lacks

```clojure
(All (_ a b) (-> a (Dictionary a b) (Dictionary a b)))
```

```clojure
(lacks key dict)
```

### merged

```clojure
(All (_ a b) (-> (Dictionary a b) (Dictionary a b) (Dictionary a b)))
```

Merges 2 dictionaries\.
If any collisions with keys occur, the values of dict2 will overwrite those of dict1\.

```clojure
(merged dict2 dict1)
```

### merged\_with

```clojure
(All (_ a b) (-> (-> b b b) (Dictionary a b) (Dictionary a b) (Dictionary a b)))
```

Merges 2 dictionaries\.
If any collisions with keys occur, a new value will be computed by applying 'f' to the values of dict2 and dict1\.

```clojure
(merged_with f dict2 dict1)
```

### of\_list

```clojure
(All (_ a b) (-> (library/lux/abstract/hash.Hash a) (.List [a b]) (Dictionary a b)))
```

```clojure
(of_list key_hash kvs)
```

### re\_bound

```clojure
(All (_ a b) (-> a a (Dictionary a b) (Dictionary a b)))
```

If there is a value under 'from\_key', remove 'from\_key' and store the value under 'to\_key'\.

```clojure
(re_bound from_key to_key dict)
```

### revised

```clojure
(All (_ a b) (-> a (-> b b) (Dictionary a b) (Dictionary a b)))
```

Transforms the value located at key \(if available\), using the given function\.

```clojure
(revised key f dict)
```

### revised'

```clojure
(All (_ a b) (-> a b (-> b b) (Dictionary a b) (Dictionary a b)))
```

Updates the value at the key; if it exists\.
Otherwise, puts a value by applying the function to a default\.

```clojure
(revised' key default f dict)
```

### size

```clojure
(All (_ a b) (-> (Dictionary a b) .Nat))
```

### sub

```clojure
(All (_ a b) (-> (.List a) (Dictionary a b) (Dictionary a b)))
```

A sub\-dictionary, with only the specified keys\.

```clojure
(sub keys dict)
```

### value

```clojure
(All (_ a b) (-> a (Dictionary a b) (.Maybe b)))
```

```clojure
(value key dict)
```

### values

```clojure
(All (_ a b) (-> (Dictionary a b) (.List b)))
```

___

# library/lux/data/collection/dictionary/ordered

## Definitions

### \(Dictionary key value\)

```clojure
... .Type
[(library/lux/abstract/order.Order key) (.Maybe (Node key value))]
```

A dictionary data\-structure with ordered entries\.

### empty

```clojure
(All (_ a b) (-> (library/lux/abstract/order.Order a) (Dictionary a b)))
```

An empty dictionary, employing the given order\.

```clojure
(empty order)
```

### empty?

```clojure
(All (_ a b) (-> (Dictionary a b) .Bit))
```

### entries

```clojure
(All (_ a b) (-> (Dictionary a b) (.List [a b])))
```

### equivalence

```clojure
(All (_ a b) (-> (library/lux/abstract/equivalence.Equivalence b) (library/lux/abstract/equivalence.Equivalence (Dictionary a b))))
```

### has

```clojure
(All (_ a b) (-> a b (Dictionary a b) (Dictionary a b)))
```

```clojure
(has key value dict)
```

### key?

```clojure
(All (_ a b) (-> (Dictionary a b) a .Bit))
```

```clojure
(key? dict key)
```

### keys

```clojure
(All (_ a b) (-> (Dictionary a b) (.List a)))
```

### lacks

```clojure
(All (_ a b) (-> a (Dictionary a b) (Dictionary a b)))
```

```clojure
(lacks key dict)
```

### max

```clojure
(All (_ a b) (-> (Dictionary a b) (.Maybe b)))
```

Yields value under the maximum key\.

```clojure
       (


max               dict)
```

### min

```clojure
(All (_ a b) (-> (Dictionary a b) (.Maybe b)))
```

Yields value under the minimum key\.

```clojure
       (

min               dict)
```

### of\_list

```clojure
(All (_ a b) (-> (library/lux/abstract/order.Order a) (.List [a b]) (Dictionary a b)))
```

```clojure
(of_list order list)
```

### revised

```clojure
(All (_ a b) (-> a (-> b b) (Dictionary a b) (Dictionary a b)))
```

```clojure
(revised key transform dict)
```

### size

```clojure
(All (_ a b) (-> (Dictionary a b) .Nat))
```

```clojure
(size dict)
```

### value

```clojure
(All (_ a b) (-> a (Dictionary a b) (.Maybe b)))
```

```clojure
(value key dict)
```

### values

```clojure
(All (_ a b) (-> (Dictionary a b) (.List b)))
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
(All (_ a) (-> .Text (PList a) .Bit))
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
(All (_ a) (-> (PList a) .Bit))
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (PList a))))
```

### has

```clojure
(All (_ a) (-> .Text a (PList a) (PList a)))
```

```clojure
(has key val properties)
```

### keys

```clojure
(All (_ a) (-> (PList a) (.List .Text)))
```

### lacks

```clojure
(All (_ a) (-> .Text (PList a) (PList a)))
```

```clojure
(lacks key properties)
```

### revised

```clojure
(All (_ a) (-> .Text (-> a a) (PList a) (PList a)))
```

```clojure
(revised key f properties)
```

### size

```clojure
(All (_ a) (-> (PList a) .Nat))
```

### value

```clojure
(All (_ a) (-> .Text (PList a) (.Maybe a)))
```

```clojure
(value key properties)
```

### values

```clojure
(All (_ a) (-> (PList a) (.List a)))
```

___

# library/lux/data/collection/list

## Definitions

### after

```clojure
(All (_ a) (-> .Nat (.List a) (.List a)))
```

```clojure
   (


after           n xs)
```

### all

```clojure
(All (_ a b) (-> (-> a (.Maybe b)) (.List a) (.List b)))
```

```clojure
(all check xs)
```

### any?

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (.List a) .Bit))
```

```clojure
   (


any?           predicate items)
```

### apply

```clojure
(library/lux/abstract/apply.Apply .List)
```

### empty?

```clojure
(All (_ a) (library/lux/abstract/predicate.Predicate (.List a)))
```

```clojure
(empty? xs)
```

### enumeration

```clojure
(All (_ a) (-> (.List a) (.List [.Nat a])))
```

Pairs every element in the list with its index, starting at 0\.

```clojure
(enumeration xs)
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (.List a))))
```

### every?

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (.List a) .Bit))
```

```clojure
   (

every?           predicate items)
```

### example

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (.List a) (.Maybe a)))
```

Yields the first value in the list that satisfies the predicate\.

```clojure
(example predicate xs)
```

### first

```clojure
(All (_ a) (-> .Nat (.List a) (.List a)))
```

```clojure
   (

first           n xs)
```

### functor

```clojure
(library/lux/abstract/functor.Functor .List)
```

### hash

```clojure
(All (_ a) (-> (library/lux/abstract/hash.Hash a) (library/lux/abstract/hash.Hash (.List a))))
```

### head

```clojure
(All (_ a) (-> (.List a) (.Maybe a)))
```

Yields the first element of a list\.

```clojure
   (

head           xs)
```

### indices

```clojure
(All (_ a) (-> .Nat (.List .Nat)))
```

Produces all the valid indices for a given size\.

```clojure
(indices size)
```

### inits

```clojure
(All (_ a) (-> (.List a) (.Maybe (.List a))))
```

For a list of size N, yields the first N\-1 elements\.
Will yield a \#\.None for empty lists\.

```clojure
(inits xs)
```

### interposed

```clojure
(All (_ a) (-> a (.List a) (.List a)))
```

Puts a value between every two elements in the list\.

```clojure
(interposed sep xs)
```

### item

```clojure
(All (_ a) (-> .Nat (.List a) (.Maybe a)))
```

Fetches the element at the specified index\.

```clojure
(item i xs)
```

### iterations

```clojure
(All (_ a) (-> (-> a (.Maybe a)) a (.List a)))
```

Generates a list element by element until the function returns \#\.None\.

```clojure
(iterations f x)
```

### last

```clojure
(All (_ a) (-> (.List a) (.Maybe a)))
```

```clojure
(last xs)
```

### lifted

```clojure
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (a b) (a (.List b))))
```

Wraps a monadic value with List machinery\.

```clojure
(lifted monad)
```

### member?

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (.List a) a .Bit))
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
(All (_ a b) (-> (-> a b b) b (.List a) (.List b)))
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
(All (_ a) (library/lux/abstract/monoid.Monoid (.List a)))
```

### one

```clojure
(All (_ a b) (-> (-> a (.Maybe b)) (.List a) (.Maybe b)))
```

```clojure
(one check xs)
```

### only

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (.List a) (.List a)))
```

A list with only values that satisfy the predicate\.

```clojure
(only keep? xs)
```

### pairs

```clojure
(All (_ a) (-> (.List a) (.List [a a])))
```

Cut the list into pairs of 2\.
Caveat emptor: If the list has an un\-even number of elements, the last one will be skipped\.

```clojure
(pairs xs)
```

### partition

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (.List a) [(.List a) (.List a)]))
```

Divide the list into all elements that satisfy a predicate, and all elements that do not\.

```clojure
(partition satisfies? list)
```

### repeated

```clojure
(All (_ a) (-> .Nat a (.List a)))
```

A list of the value x, repeated n times\.

```clojure
(repeated n x)
```

### reversed

```clojure
(All (_ a) (-> (.List a) (.List a)))
```

```clojure
(reversed xs)
```

### size

```clojure
(All (_ a) (-> (.List a) .Nat))
```

```clojure
(size list)
```

### sorted

```clojure
(All (_ a) (-> (-> a a .Bit) (.List a) (.List a)))
```

A list ordered by a comparison function\.

```clojure
(sorted < xs)
```

### split\_at

```clojure
(All (_ a) (-> .Nat (.List a) [(.List a) (.List a)]))
```

```clojure
(split_at n xs)
```

### split\_when

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (.List a) [(.List a) (.List a)]))
```

Segment the list by using a predicate to tell when to cut\.

```clojure
(split_when predicate xs)
```

### sub

```clojure
(All (_ a) (-> .Nat (.List a) (.List (.List a))))
```

Segment the list into sub\-lists of \(at most\) the given size\.

```clojure
(sub size list)
```

### tail

```clojure
(All (_ a) (-> (.List a) (.Maybe (.List a))))
```

For a list of size N, yields the N\-1 elements after the first one\.

```clojure
   (


tail           xs)
```

### together

```clojure
(All (_ a) (-> (.List (.List a)) (.List a)))
```

The sequential combination of all the lists\.

### until

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (.List a) (.List a)))
```

```clojure
   (


until           predicate xs)
```

### when

```clojure
.Macro
```

Can be used as a guard in \(co\)monadic be/do expressions\.

```clojure
(do monad
  [value (do_something 1 2 3)
   when   (passes_test? value)]
  (do_something_else 4 5 6))
```

### while

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (.List a) (.List a)))
```

```clojure
   (

while           predicate xs)
```

### with

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (library/lux/abstract/monad.Monad (All (_ b) (a (.List b))))))
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
(All (_ a b) (-> (.List a) (.List b) (.List [a b])))
```

### zipped/3

```clojure
(All (_ a b c) (-> (.List a) (.List b) (.List c) (.List [a b c])))
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
(All (_ a b c) (-> (-> a b c) (.List a) (.List b) (.List c)))
```

### zipped\_with/3

```clojure
(All (_ a b c d) (-> (-> a b c d) (.List a) (.List b) (.List c) (.List d)))
```

___

# library/lux/data/collection/queue

## Definitions

### \(Queue it\)

```clojure
... .Type
[(.List it) (.List it)]
```

A first\-in, first\-out sequential data\-structure\.

### empty

```clojure
Queue
```

### empty?

```clojure
(All (_ a) (-> (Queue a) .Bit))
```

### end

```clojure
(All (_ a) (-> a (Queue a) (Queue a)))
```

```clojure
(end val queue)
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (Queue a))))
```

### front

```clojure
(All (_ a) (-> (Queue a) (.Maybe a)))
```

Yields the first value in the queue, if any\.

### functor

```clojure
(library/lux/abstract/functor.Functor Queue)
```

### list

```clojure
(All (_ a) (-> (Queue a) (.List a)))
```

```clojure
(list queue)
```

### member?

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (Queue a) a .Bit))
```

```clojure
(member? equivalence queue member)
```

### next

```clojure
(All (_ a) (-> (Queue a) (Queue a)))
```

```clojure
(next queue)
```

### of\_list

```clojure
(All (_ a) (-> (.List a) (Queue a)))
```

```clojure
(of_list entries)
```

### size

```clojure
(All (_ a) (-> (Queue a) .Nat))
```

___

# library/lux/data/collection/queue/priority

## Definitions

### Priority

```clojure
... .Type
(primitive "#I64" (primitive "#Nat"))
```

### \(Queue it\)

```clojure
... .Type
(primitive "library/lux/data/collection/queue/priority.Queue" it)
```

### empty

```clojure
Queue
```

### empty?

```clojure
(All (_ a) (-> (Queue a) .Bit))
```

### end

```clojure
(All (_ a) (-> Priority a (Queue a) (Queue a)))
```

```clojure
(end priority value queue)
```

### front

```clojure
(All (_ a) (-> (Queue a) (.Maybe a)))
```

### max

```clojure
Priority
```

### member?

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (Queue a) a .Bit))
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
(All (_ a) (-> (Queue a) (Queue a)))
```

### size

```clojure
(All (_ a) (-> (Queue a) .Nat))
```

___

# library/lux/data/collection/row

## Definitions

### \(Row it\)

```clojure
... .Type
[Level .Nat (Hierarchy it) (Base it)]
```

A sequential data\-structure with fast random access\.

### any?

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (Row a) .Bit))
```

### apply

```clojure
(library/lux/abstract/apply.Apply Row)
```

### empty

```clojure
Row
```

### empty?

```clojure
(All (_ a) (-> (Row a) .Bit))
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (Row a))))
```

### every?

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (Row a) .Bit))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Row)
```

### has

```clojure
(All (_ a) (-> .Nat a (Row a) (library/lux/control/try.Try (Row a))))
```

```clojure
(has idx val row)
```

### index\_out\_of\_bounds

```clojure
(All (_ a) (library/lux/control/exception.Exception [(Row a) .Nat]))
```

### item

```clojure
(All (_ a) (-> .Nat (Row a) (library/lux/control/try.Try a)))
```

```clojure
(item idx row)
```

### list

```clojure
(All (_ a) (-> (Row a) (.List a)))
```

```clojure
(list row)
```

### member?

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (Row a) a .Bit))
```

```clojure
(member? equivalence row val)
```

### mix

```clojure
(library/lux/abstract/mix.Mix Row)
```

### monad

```clojure
(library/lux/abstract/monad.Monad Row)
```

### monoid

```clojure
(All (_ a) (library/lux/abstract/monoid.Monoid (Row a)))
```

### of\_list

```clojure
(All (_ a) (-> (.List a) (Row a)))
```

### prefix

```clojure
(All (_ a) (-> (Row a) (Row a)))
```

```clojure
(prefix row)
```

### reversed

```clojure
(All (_ a) (-> (Row a) (Row a)))
```

### revised

```clojure
(All (_ a) (-> .Nat (-> a a) (Row a) (library/lux/control/try.Try (Row a))))
```

```clojure
(revised idx f row)
```

### row

```clojure
.Macro
```

Row literals\.

```clojure
(: (Row Nat)
   (row 12 34 56 78 90))
```

### size

```clojure
(All (_ a) (-> (Row a) .Nat))
```

### suffix

```clojure
(All (_ a) (-> a (Row a) (Row a)))
```

```clojure
(suffix val row)
```

### within\_bounds?

```clojure
(All (_ a) (-> (Row a) .Nat .Bit))
```

Determines whether the index is within the bounds of the row\.

```clojure
(within_bounds? row idx)
```

___

# library/lux/data/collection/sequence

## Definitions

### \(Sequence it\)

```clojure
... .Type
(library/lux/control/continuation.Cont [it (Sequence it)])
```

An infinite sequence of values\.

### ^sequence&

```clojure
.Macro
```

Allows destructuring of sequences in pattern\-matching expressions\.
Caveat emptor: Only use it for destructuring, and not for testing values within the sequences\.

```clojure
(let [(^sequence& x y z _tail) (some_sequence_func +1 +2 +3)]
  (func x y z))
```

### after

```clojure
(All (_ a) (-> .Nat (Sequence a) (Sequence a)))
```

```clojure
(


     after           pred xs)
```

### comonad

```clojure
(library/lux/abstract/comonad.CoMonad Sequence)
```

### cycle

```clojure
(All (_ a) (-> [a (.List a)] (Sequence a)))
```

Go over the elements of a list forever\.
The list should not be empty\.

```clojure
(cycle [start next])
```

### first

```clojure
(All (_ a) (-> .Nat (Sequence a) (.List a)))
```

```clojure
   (





first            pred xs)
```

### functor

```clojure
(library/lux/abstract/functor.Functor Sequence)
```

### head

```clojure
(All (_ a) (-> (Sequence a) a))
```

### item

```clojure
(All (_ a) (-> .Nat (Sequence a) a))
```

```clojure
(item idx sequence)
```

### iterations

```clojure
(All (_ a b) (-> (-> a [a b]) a (Sequence b)))
```

A stateful way of infinitely calculating the values of a sequence\.

```clojure
(iterations step init)
```

### only

```clojure
(All (_ a) (-> (-> a .Bit) (Sequence a) (Sequence a)))
```

A new sequence only with items that satisfy the predicate\.

```clojure
(only predicate sequence)
```

### partition

```clojure
(All (_ a) (-> (-> a .Bit) (Sequence a) [(Sequence a) (Sequence a)]))
```

Split a sequence in two based on a predicate\.
The left side contains all entries for which the predicate is \#1\.
The right side contains all entries for which the predicate is \#0\.

```clojure
(partition left? xs)
```

### repeated

```clojure
(All (_ a) (-> a (Sequence a)))
```

Repeat a value forever\.

```clojure
(repeated x)
```

### split\_at

```clojure
(All (_ a) (-> .Nat (Sequence a) [(.List a) (Sequence a)]))
```

```clojure
   (


split_at               pred xs)
```

### split\_when

```clojure
(All (_ a) (-> (-> a .Bit) (Sequence a) [(.List a) (Sequence a)]))
```

```clojure
   (

split_when               pred xs)
```

### tail

```clojure
(All (_ a) (-> (Sequence a) (Sequence a)))
```

### until

```clojure
(All (_ a) (-> (-> a .Bit) (Sequence a) (Sequence a)))
```

```clojure
(

     until           pred xs)
```

### while

```clojure
(All (_ a) (-> (-> a .Bit) (Sequence a) (.List a)))
```

```clojure
   (




while            pred xs)
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
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

```clojure
(difference sub base)
```

### empty

```clojure
(All (_ a) (-> (library/lux/abstract/hash.Hash a) (Set a)))
```

### empty?

```clojure
(All (_ a) (-> (Set a) .Bit))
```

### equivalence

```clojure
(All (_ a) (library/lux/abstract/equivalence.Equivalence (Set a)))
```

### has

```clojure
(All (_ a) (-> a (Set a) (Set a)))
```

```clojure
(has elem set)
```

### hash

```clojure
(All (_ a) (library/lux/abstract/hash.Hash (Set a)))
```

### intersection

```clojure
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

```clojure
(intersection filter base)
```

### lacks

```clojure
(All (_ a) (-> a (Set a) (Set a)))
```

### list

```clojure
(All (_ a) (-> (Set a) (.List a)))
```

### member?

```clojure
(All (_ a) (-> (Set a) a .Bit))
```

### member\_hash

```clojure
(All (_ a) (-> (Set a) (library/lux/abstract/hash.Hash a)))
```

### monoid

```clojure
(All (_ a) (-> (library/lux/abstract/hash.Hash a) (library/lux/abstract/monoid.Monoid (Set a))))
```

### of\_list

```clojure
(All (_ a) (-> (library/lux/abstract/hash.Hash a) (.List a) (Set a)))
```

### predicate

```clojure
(All (_ a) (-> (Set a) (library/lux/abstract/predicate.Predicate a)))
```

### size

```clojure
(All (_ a) (-> (Set a) .Nat))
```

### sub?

```clojure
(All (_ a) (-> (Set a) (Set a) .Bit))
```

```clojure
(sub? super sub)
```

### super?

```clojure
(All (_ a) (-> (Set a) (Set a) .Bit))
```

```clojure
(super? sub super)
```

### union

```clojure
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

___

# library/lux/data/collection/set/multi

## Definitions

### \(Set it\)

```clojure
... .Type
(primitive "library/lux/data/collection/set/multi.Set" it)
```

A set that keeps track of repetition in its entries\.

### difference

```clojure
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

```clojure
   (




difference           parameter subject)
```

### empty

```clojure
(All (_ a) (-> (library/lux/abstract/hash.Hash a) (Set a)))
```

### empty?

```clojure
(All (_ a) (-> (Set a) .Bit))
```

### equivalence

```clojure
(All (_ a) (library/lux/abstract/equivalence.Equivalence (Set a)))
```

### has

```clojure
(All (_ a) (-> .Nat a (Set a) (Set a)))
```

```clojure
(has multiplicity elem set)
```

### hash

```clojure
(All (_ a) (library/lux/abstract/hash.Hash (Set a)))
```

### intersection

```clojure
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

```clojure
   (



intersection           parameter subject)
```

### lacks

```clojure
(All (_ a) (-> .Nat a (Set a) (Set a)))
```

```clojure
(lacks multiplicity elem set)
```

### list

```clojure
(All (_ a) (-> (Set a) (.List a)))
```

### member?

```clojure
(All (_ a) (-> (Set a) a .Bit))
```

```clojure
(member? set elem)
```

### multiplicity

```clojure
(All (_ a) (-> (Set a) a .Nat))
```

```clojure
(multiplicity set elem)
```

### of\_list

```clojure
(All (_ a) (-> (library/lux/abstract/hash.Hash a) (.List a) (Set a)))
```

### of\_set

```clojure
(All (_ a) (-> (library/lux/data/collection/set.Set a) (Set a)))
```

### size

```clojure
(All (_ a) (-> (Set a) .Nat))
```

### sub?

```clojure
(All (_ a) (-> (Set a) (Set a) .Bit))
```

Is 'subject' a sub\-set of 'reference'?

```clojure
(sub? reference subject)
```

### sum

```clojure
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

```clojure
   (


sum           parameter subject)
```

### super?

```clojure
(All (_ a) (-> (Set a) (Set a) .Bit))
```

Is 'subject' a super\-set of 'reference'?

### support

```clojure
(All (_ a) (-> (Set a) (library/lux/data/collection/set.Set a)))
```

A set of the unique \(non repeated\) members\.

```clojure
(support set)
```

### union

```clojure
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

```clojure
   (

union           parameter subject)
```

___

# library/lux/data/collection/set/ordered

## Definitions

### \(Set it\)

```clojure
... .Type
(primitive "library/lux/data/collection/set/ordered.Set" it)
```

A set with ordered entries\.

### difference

```clojure
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

```clojure
(difference param subject)
```

### empty

```clojure
(All (_ a) (-> (library/lux/abstract/order.Order a) (Set a)))
```

### empty?

```clojure
(All (_ a) (-> (Set a) .Bit))
```

### equivalence

```clojure
(All (_ a) (library/lux/abstract/equivalence.Equivalence (Set a)))
```

### has

```clojure
(All (_ a) (-> a (Set a) (Set a)))
```

```clojure
(has elem set)
```

### intersection

```clojure
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

### lacks

```clojure
(All (_ a) (-> a (Set a) (Set a)))
```

```clojure
(lacks elem set)
```

### list

```clojure
(All (_ a) (-> (Set a) (.List a)))
```

### max

```clojure
(All (_ a) (-> (Set a) (.Maybe a)))
```

### member?

```clojure
(All (_ a) (-> (Set a) a .Bit))
```

```clojure
(member? set elem)
```

### min

```clojure
(All (_ a) (-> (Set a) (.Maybe a)))
```

### of\_list

```clojure
(All (_ a) (-> (library/lux/abstract/order.Order a) (.List a) (Set a)))
```

### size

```clojure
(All (_ a) (-> (Set a) .Nat))
```

### sub?

```clojure
(All (_ a) (-> (Set a) (Set a) .Bit))
```

Is 'sub' a sub\-set of 'super'?

```clojure
(sub? super sub)
```

### super?

```clojure
(All (_ a) (-> (Set a) (Set a) .Bit))
```

Is 'super' a super\-set of 'sub'?

```clojure
(super? sub super)
```

### union

```clojure
(All (_ a) (-> (Set a) (Set a) (Set a)))
```

___

# library/lux/data/collection/stack

## Definitions

### \(Stack it\)

```clojure
... .Type
(primitive "library/lux/data/collection/stack.Stack" it)
```

A first\-in, last\-out sequential data\-structure\.

### empty

```clojure
Stack
```

### empty?

```clojure
(All (_ a) (-> (Stack a) .Bit))
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (Stack a))))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Stack)
```

### next

```clojure
(All (_ a) (-> (Stack a) (.Maybe [a (Stack a)])))
```

```clojure
(next stack)
```

### size

```clojure
(All (_ a) (-> (Stack a) .Nat))
```

### top

```clojure
(All (_ a) (-> a (Stack a) (Stack a)))
```

```clojure
(top value stack)
```

### value

```clojure
(All (_ a) (-> (Stack a) (.Maybe a)))
```

Yields the top value in the stack, if any\.

```clojure
(value stack)
```

___

# library/lux/data/collection/tree

## Definitions

### \(Tree it\)

```clojure
... .Type
[it (.List (Tree it))]
```

A generic tree data\-structure\.

### branch

```clojure
(All (_ a) (-> a (.List (Tree a)) (Tree a)))
```

```clojure
(branch value children)
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (Tree a))))
```

### flat

```clojure
(All (_ a) (-> (Tree a) (.List a)))
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
(All (_ a) (-> a (Tree a)))
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
[(All (_ c) (-> tag c (Tree @ tag c))) (All (_ c) (-> (Tree @ tag c) (Tree @ tag c) (Tree @ tag c)))]
```

A builder for finter tree structures\.

### \(Tree @ tag value\)

```clojure
... .Type
(primitive "library/lux/data/collection/tree/finger.Tree" @ tag value)
```

A finger tree\.

### builder

```clojure
(All (_ a) (Ex (_ b) (-> (library/lux/abstract/monoid.Monoid a) (Builder b a))))
```

A new builder using the given monoid\.

```clojure
(builder monoid)
```

### exists?

```clojure
(All (_ a b c) (-> (library/lux/abstract/predicate.Predicate b) (Tree a b c) .Bit))
```

Verifies that a value exists which meets the predicate\.

```clojure
(exists? predicate tree)
```

### one

```clojure
(All (_ a b c) (-> (library/lux/abstract/predicate.Predicate b) (Tree a b c) (.Maybe c)))
```

Finds one value that meets the predicate\.

```clojure
(one predicate tree)
```

### root

```clojure
(All (_ a b c) (-> (Tree a b c) (.Either c [(Tree a b c) (Tree a b c)])))
```

### tag

```clojure
(All (_ a b c) (-> (Tree a b c) b))
```

### tags

```clojure
(All (_ a b c) (-> (Tree a b c) (.List b)))
```

```clojure
(tags tree)
```

### value

```clojure
(All (_ a b c) (-> (Tree a b c) c))
```

```clojure
(value tree)
```

### values

```clojure
(All (_ a b c) (-> (Tree a b c) (.List c)))
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
[(.Maybe (Family Zipper it)) (library/lux/data/collection/tree.Tree it)]
```

Tree zippers, for easy navigation and editing of trees\.

### adopt

```clojure
(All (_ a) (-> a (Zipper a) (Zipper a)))
```

```clojure
(adopt value zipper)
```

### branch?

```clojure
(All (_ a) (-> (Zipper a) .Bit))
```

### comonad

```clojure
(library/lux/abstract/comonad.CoMonad Zipper)
```

### down

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### end

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### end?

```clojure
(All (_ a) (-> (Zipper a) .Bit))
```

### equivalence

```clojure
(All (_ a) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence (Zipper a))))
```

### functor

```clojure
(library/lux/abstract/functor.Functor Zipper)
```

### insert\_left

```clojure
(All (_ a) (-> a (Zipper a) (.Maybe (Zipper a))))
```

### insert\_right

```clojure
(All (_ a) (-> a (Zipper a) (.Maybe (Zipper a))))
```

### interpose

```clojure
(All (_ a) (-> a (Zipper a) (Zipper a)))
```

```clojure
(interpose value zipper)
```

### leaf?

```clojure
(All (_ a) (-> (Zipper a) .Bit))
```

### left

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### leftmost

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### next

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### previous

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### remove

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### right

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### rightmost

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### set

```clojure
(All (_ a) (-> a (Zipper a) (Zipper a)))
```

```clojure
(set value zipper)
```

### start

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### start?

```clojure
(All (_ a) (-> (Zipper a) .Bit))
```

### tree

```clojure
(All (_ a) (-> (Zipper a) (library/lux/data/collection/tree.Tree a)))
```

### up

```clojure
(All (_ a) (-> (Zipper a) (.Maybe (Zipper a))))
```

### update

```clojure
(All (_ a) (-> (-> a a) (Zipper a) (Zipper a)))
```

```clojure
(update transform zipper)
```

### value

```clojure
(All (_ a) (-> (Zipper a) a))
```

### zipper

```clojure
(All (_ a) (-> (library/lux/data/collection/tree.Tree a) (Zipper a)))
```

___

# library/lux/data/color

## Definitions

### Alpha

```clojure
... .Type
(primitive "#I64" (primitive "#Rev"))
```

The degree of transparency of a pigment\.

### CMYK

```clojure
... .Type
[.Frac .Frac .Frac .Frac]
```

Cyan\-Magenta\-Yellow\-Key color format\.

### Color

```clojure
... .Type
(primitive "library/lux/data/color.Color")
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
[Color Alpha]
```

A color with some degree of transparency\.

### RGB

```clojure
... .Type
[.Nat .Nat .Nat]
```

Red\-Green\-Blue color format\.

### Spread

```clojure
... .Type
(primitive "#Frac")
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

```clojure
   (


brighter           ratio color)
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

```clojure
   (

darker           ratio color)
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

```clojure
   (



saturated           ratio color)
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

```clojure
   (




un_saturated           ratio color)
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
(All (_ a b) (-> (Writer a) (Writer b) (Writer [a b])))
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
(All (_ a) (-> (Writer a) (Writer (.List a))))
```

### location

```clojure
(Writer .Location)
```

### maybe

```clojure
(All (_ a) (-> (Writer a) (Writer (.Maybe a))))
```

### monoid

```clojure
(library/lux/abstract/monoid.Monoid Specification)
```

### name

```clojure
(Writer .Name)
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
(All (_ a b) (-> (Writer a) (Writer b) (Writer (Or a b))))
```

```clojure
(or left right)
```

### rec

```clojure
(All (_ a) (-> (-> (Writer a) (Writer a)) (Writer a)))
```

A combinator for recursive writers\.

```clojure
(rec body)
```

### result

```clojure
(All (_ a) (-> (Writer a) a library/lux/data/binary.Binary))
```

Yields a binary blob with all the information written to it\.

```clojure
(result writer value)
```

### rev

```clojure
(Writer .Rev)
```

### row/16

```clojure
(All (_ a) (-> (Writer a) (Writer (library/lux/data/collection/row.Row a))))
```

### row/32

```clojure
(All (_ a) (-> (Writer a) (Writer (library/lux/data/collection/row.Row a))))
```

### row/64

```clojure
(All (_ a) (-> (Writer a) (Writer (library/lux/data/collection/row.Row a))))
```

### row/8

```clojure
(All (_ a) (-> (Writer a) (Writer (library/lux/data/collection/row.Row a))))
```

### segment

```clojure
(-> .Nat (Writer library/lux/data/binary.Binary))
```

Writes at most 'size' bytes of an input binary blob\.

```clojure
(segment size)
```

### set

```clojure
(All (_ a) (-> (Writer a) (Writer (library/lux/data/collection/set.Set a))))
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

## Missing documentation

1. `` or/5 ``

___

# library/lux/data/format/json

Functionality for reading and writing values in the JSON format\.
For more information, please see: http://www\.json\.org/

## Definitions

### Array

```clojure
... .Type
(library/lux/data/collection/row.Row JSON)
```

### Boolean

```clojure
... .Type
(primitive "#Bit")
```

### JSON

```clojure
... .Type
((All (JSON a) (Or Null Boolean Number String (library/lux/data/collection/row.Row (JSON .Nothing)) (library/lux/data/collection/dictionary.Dictionary String (JSON .Nothing)))) .Nothing)
```

### Null

```clojure
... .Type
(Ex (Null a) a)
```

### Number

```clojure
... .Type
(primitive "#Frac")
```

### Object

```clojure
... .Type
(library/lux/data/collection/dictionary.Dictionary String JSON)
```

### String

```clojure
... .Type
(primitive "#Text")
```

### array\_field

```clojure
(-> .Text JSON (library/lux/control/try.Try Array))
```

A JSON object field getter for arrays\.

```clojure
   (




array_field           key json)
```

### boolean\_field

```clojure
(-> .Text JSON (library/lux/control/try.Try Boolean))
```

A JSON object field getter for booleans\.

```clojure
   (

boolean_field           key json)
```

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
(json #null)

................................................................
................................................................

(json #1)

................................................................
................................................................

(json +123.456)

................................................................
................................................................

(json "this is a string")

................................................................
................................................................

(json ["this" "is" "an" "array"])

................................................................
................................................................

(json {"this" "is"
       "an" "object"})
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

```clojure
   (


number_field           key json)
```

### object

```clojure
(-> (.List [String JSON]) JSON)
```

### object\_field

```clojure
(-> .Text JSON (library/lux/control/try.Try Object))
```

A JSON object field getter for objects\.

```clojure
   (





object_field           key json)
```

### string\_field

```clojure
(-> .Text JSON (library/lux/control/try.Try String))
```

A JSON object field getter for strings\.

```clojure
   (



string_field           key json)
```

___

# library/lux/data/format/tar

## Definitions

### Big

```clojure
... .Type
(primitive "library/lux/data/format/tar.Big")
```

### Content

```clojure
... .Type
(primitive "library/lux/data/format/tar.Content")
```

### Contiguous

```clojure
... .Type
[Path library/lux/time/instant.Instant Mode Ownership Content]
```

### Directory

```clojure
... .Type
(primitive "library/lux/data/format/tar.Path")
```

### Entry

```clojure
... .Type
(Or Normal Symbolic_Link Directory Contiguous)
```

### File

```clojure
... .Type
[Path library/lux/time/instant.Instant Mode Ownership Content]
```

### ID

```clojure
... .Type
(primitive "library/lux/data/format/tar.Small")
```

### Mode

```clojure
... .Type
(primitive "library/lux/data/format/tar.Mode")
```

### Name

```clojure
... .Type
(primitive "library/lux/data/format/tar.Name")
```

### Normal

```clojure
... .Type
[Path library/lux/time/instant.Instant Mode Ownership Content]
```

### Owner

```clojure
... .Type
[Name ID]
```

### Ownership

```clojure
... .Type
[Owner Owner]
```

### Path

```clojure
... .Type
(primitive "library/lux/data/format/tar.Path")
```

### Small

```clojure
... .Type
(primitive "library/lux/data/format/tar.Small")
```

### Symbolic\_Link

```clojure
... .Type
(primitive "library/lux/data/format/tar.Path")
```

### Tar

```clojure
... .Type
(library/lux/data/collection/row.Row Entry)
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
((All (XML a) (Or .Text [Tag Attrs (.List (XML .Nothing))])) .Nothing)
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

# library/lux/data/name

## Definitions

### codec

```clojure
(library/lux/abstract/codec.Codec .Text .Name)
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence .Name)
```

### hash

```clojure
(library/lux/abstract/hash.Hash .Name)
```

### module

```clojure
(-> .Name .Text)
```

The module part of a name\.

### order

```clojure
(library/lux/abstract/order.Order .Name)
```

### short

```clojure
(-> .Name .Text)
```

The short part of a name\.

___

# library/lux/data/product

Functionality for working with tuples \(particularly 2\-tuples/pairs\)\.

## Definitions

### curried

```clojure
(All (_ a b c) (-> (-> [a b] c) a b c))
```

Converts a 2\-argument function into nested single\-argument functions\.

```clojure
(curried f)
```

### equivalence

```clojure
(All (_ a b) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence b) (library/lux/abstract/equivalence.Equivalence [a b])))
```

### forked

```clojure
(All (_ a b c) (-> (-> a b) (-> a c) a [b c]))
```

Yields a pair by applying both functions to a single value\.

```clojure
(forked f g)
```

### hash

```clojure
(All (_ a b) (-> (library/lux/abstract/hash.Hash a) (library/lux/abstract/hash.Hash b) (library/lux/abstract/hash.Hash [a b])))
```

### left

```clojure
(All (_ a b) (-> [a b] a))
```

The left side of a pair\.

### right

```clojure
(All (_ a b) (-> [a b] b))
```

The right side of a pair\.

### swapped

```clojure
(All (_ a b) (-> [a b] [b a]))
```

```clojure
(swapped [left right])
```

### then

```clojure
(All (_ a b c d) (-> (-> a c) (-> b d) [a b] [c d]))
```

Apply functions to both sides of a pair\.

```clojure
(then f g)
```

### uncurried

```clojure
(All (_ a b c) (-> (-> a b c) [a b] c))
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
(All (_ a b c) (-> (-> a c) (-> b c) (Or a b) c))
```

Applies a function to either side of a 2\-variant\.

```clojure
(either on_left on_right)
```

### equivalence

```clojure
(All (_ a b) (-> (library/lux/abstract/equivalence.Equivalence a) (library/lux/abstract/equivalence.Equivalence b) (library/lux/abstract/equivalence.Equivalence (Or a b))))
```

### hash

```clojure
(All (_ a b) (-> (library/lux/abstract/hash.Hash a) (library/lux/abstract/hash.Hash b) (library/lux/abstract/hash.Hash (Or a b))))
```

### left

```clojure
(All (_ a b) (-> a (Or a b)))
```

Lifts value to the left side of a 2\-variant\.

### lefts

```clojure
(All (_ a b) (-> (.List (Or a b)) (.List a)))
```

### partition

```clojure
(All (_ a b) (-> (.List (Or a b)) [(.List a) (.List b)]))
```

### right

```clojure
(All (_ a b) (-> b (Or a b)))
```

Lifts value to the right side of a 2\-variant\.

### rights

```clojure
(All (_ a b) (-> (.List (Or a b)) (.List b)))
```

### then

```clojure
(All (_ a b c d) (-> (-> a b) (-> c d) (Or a c) (Or b d)))
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
(primitive "#I64" (primitive "#Nat"))
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
(primitive "library/lux/data/text/buffer.Buffer")
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
(primitive "library/lux/data/text/encoding.Encoding")
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
(All (_ a) (-> (Format a) (Format (.List a))))
```

### location

```clojure
(Format .Location)
```

### maybe

```clojure
(All (_ a) (-> (Format a) (Format (.Maybe a))))
```

### mod

```clojure
(All (_ a) (Format (library/lux/math/modular.Mod a)))
```

### month

```clojure
(Format library/lux/time/month.Month)
```

### name

```clojure
(Format .Name)
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
(primitive "library/lux/data/text/unicode/block.Block")
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
(All (_ a) (-> Block library/lux/data/text.Char .Bit))
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
(primitive "library/lux/data/text/unicode/set.Set")
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
      baz +789.0]
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
      baz +789.0]
  (: Any
     (here {foo library/lux/data/text/format.nat}baz)))

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

(def: .private(secret_definition input)
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
[.Text (library/lux/data/format/markdown.Markdown library/lux/data/format/markdown.Block)]
```

### Module

```clojure
... .Type
[.Text .Text (library/lux/data/collection/set.Set .Text) (.List Definition)]
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
(: (.ListModule)
   (module _
           ""
           [documentation/lux/documentation.default
            documentation/lux/documentation.documentation:
            documentation/lux/documentation.module
            (default   unqualified_identifier)
            (default   Definition)
            (default   Module)
            (default   documentation)]
           []))
```

### unqualified\_identifier

```clojure
(library/lux/control/exception.Exception .Name)
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
    [.let[_ (library/lux/debug.log!(format "Successfully installed directive " (library/lux/data/text/format.textself) "!"))]]
    (in library/lux/tool/compiler/language/lux/phase/directive.no_requirements)))
```

### generation:

```clojure
.Macro
```

```clojure
(generation: ("my generation" self phase archive [pass_through <synthesis>.any])
  (for {library/lux/target.jvm
        (\ library/lux/tool/compiler/phase.monadeach (|>> #library/lux/target/jvm.Embedded
                                 library/lux/data/collection/row.row)
           (phase archive pass_through))}
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

### \!\!\!

```clojure
.Macro
```

Takes a \(Maybe ObjectType\) and returns a ObjectType\.
A \#\.None would get translated into a \(null\)\.

```clojure
(= (null)
   (!!! (??? (: java/lang/Thread (null)))))

................................................................
................................................................

(= "foo"
   (!!! (??? "foo")))
```

### :as

```clojure
.Macro
```

```clojure
(:as java/lang/Object
     (: java/lang/String
        ???))
```

### ???

```clojure
.Macro
```

Takes a \(potentially null\) ObjectType reference and creates a \(Maybe ObjectType\) for it\.

```clojure
(= (??? (: java/lang/String (null)))
   #library/lux.None)

................................................................
................................................................

(= (??? "YOLO")
   (#library/lux.Some"YOLO"))
```

### Boolean

```clojure
... .Type
(primitive "java.lang.Boolean")
```

The type of a \(boxed\) Boolean object\.

### Byte

```clojure
... .Type
(primitive "java.lang.Byte")
```

The type of a \(boxed\) Byte object\.

### Character

```clojure
... .Type
(primitive "java.lang.Character")
```

The type of a \(boxed\) Character object\.

### Double

```clojure
... .Type
(primitive "java.lang.Double")
```

The type of a \(boxed\) Double object\.

### Float

```clojure
... .Type
(primitive "java.lang.Float")
```

The type of a \(boxed\) Float object\.

### Inheritance

```clojure
... .Type
(Or .Any .Any .Any)
```

### Integer

```clojure
... .Type
(primitive "java.lang.Integer")
```

The type of a \(boxed\) Integer object\.

### Long

```clojure
... .Type
(primitive "java.lang.Long")
```

The type of a \(boxed\) Long object\.

### Privacy

```clojure
... .Type
(Or .Any .Any .Any .Any)
```

### Short

```clojure
... .Type
(primitive "java.lang.Short")
```

The type of a \(boxed\) Short object\.

### State

```clojure
... .Type
(Or .Any .Any .Any)
```

### array

```clojure
.Macro
```

Create an array of the given type, with the given size\.

```clojure
(array java/lang/Object 10)
```

### boolean

```clojure
... .Type
(primitive "boolean")
```

The type of an \(unboxed\) boolean value\.

### byte

```clojure
... .Type
(primitive "byte")
```

The type of an \(unboxed\) byte value\.

### byte\_to\_char

```clojure
.Macro
```

Type converter\.

```clojure
       (:



































                            Character          (


































byte_to_char                  (:


































                  Byte                            foo)))
```

### byte\_to\_int

```clojure
.Macro
```

Type converter\.

```clojure
       (:

































                            Integer          (
































byte_to_int                  (:
































                  Byte                            foo)))
```

### byte\_to\_long

```clojure
.Macro
```

Type converter\.

```clojure
       (:


                            Long          (

byte_to_long                  (:

                  Byte                            foo)))
```

### cannot\_cast\_to\_non\_object

```clojure
(library/lux/control/exception.Exception (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Value))
```

### cannot\_convert\_to\_jvm\_type

```clojure
(library/lux/control/exception.Exception .Type)
```

### char

```clojure
... .Type
(primitive "char")
```

The type of an \(unboxed\) char value\.

### char\_to\_byte

```clojure
.Macro
```

Type converter\.

```clojure
       (:



























                            Byte          (


























char_to_byte                  (:


























                  Character                            foo)))
```

### char\_to\_int

```clojure
.Macro
```

Type converter\.

```clojure
       (:





























                            Integer          (




























char_to_int                  (:




























                  Character                            foo)))
```

### char\_to\_long

```clojure
.Macro
```

Type converter\.

```clojure
       (:






























                            Long          (





























char_to_long                  (:





























                  Character                            foo)))
```

### char\_to\_short

```clojure
.Macro
```

Type converter\.

```clojure
       (:




























                            Short          (



























char_to_short                  (:



























                  Character                            foo)))
```

### check

```clojure
.Macro
```

Checks whether an object is an instance of a particular class\.
Caveat emptor: Cannot check for polymorphism, so avoid using parameterized classes\.

```clojure
(case (check String "YOLO")
  (#library/lux.Somevalue_as_string)
  #library/lux.None)
```

### class:

```clojure
.Macro
```

Allows defining JVM classes in Lux code\.

```clojure
(class: #final (TestClass A) [Runnable]

  (#private foo boolean)
  (#private bar A)
  (#private baz java/lang/Object)

  (#public [] (new [value A]) []
           (exec
             (:= ::foo #1)
             (:= ::bar value)
             (:= ::baz "")
             []))
  (#public (virtual) java/lang/Object
           "")
  (#public #static (static) java/lang/Object
           "")
  (Runnable [] (run) void
            []))

... The tuple corresponds to parent interfaces.

... An optional super-class can be specified before the tuple. If not specified, java.lang.Object will be assumed.

... Fields and methods defined in the class can be used with special syntax.

... For example:

... ::resolved, for accessing the 'resolved' field.

... (:= ::resolved #1) for modifying it.

... (::new! []) for calling the class's constructor.

... (::resolve! container [value]) for calling the 'resolve' method.
```

### class\_for

```clojure
.Macro
```

Loads the class as a java\.lang\.Class object\.

```clojure
(class_for java/lang/String)
```

### class\_name\_cannot\_be\_a\_type\_variable

```clojure
(library/lux/control/exception.Exception [.Text (.List (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Var))])
```

### class\_names\_cannot\_contain\_periods

```clojure
(library/lux/control/exception.Exception .Text)
```

### do\_to

```clojure
.Macro
```

Call a variety of methods on an object\. Then, return the object\.

```clojure
(do_to object
  (ClassName::method1 arg0 arg1 arg2)
  (ClassName::method2 arg3 arg4 arg5))
```

### double

```clojure
... .Type
(primitive "double")
```

The type of an \(unboxed\) double value\.

### double\_to\_float

```clojure
.Macro
```

Type converter\.

```clojure
       (:








                            Float          (







double_to_float                  (:







                  Double                            foo)))
```

### double\_to\_int

```clojure
.Macro
```

Type converter\.

```clojure
       (:






                            Integer          (





double_to_int                  (:





                  Double                            foo)))
```

### double\_to\_long

```clojure
.Macro
```

Type converter\.

```clojure
       (:







                            Long          (






double_to_long                  (:






                  Double                            foo)))
```

### float

```clojure
... .Type
(primitive "float")
```

The type of an \(unboxed\) float value\.

### float\_to\_double

```clojure
.Macro
```

Type converter\.

```clojure
       (:












                            Double          (











float_to_double                  (:











                  Float                            foo)))
```

### float\_to\_int

```clojure
.Macro
```

Type converter\.

```clojure
       (:










                            Integer          (









float_to_int                  (:









                  Float                            foo)))
```

### float\_to\_long

```clojure
.Macro
```

Type converter\.

```clojure
       (:











                            Long          (










float_to_long                  (:










                  Float                            foo)))
```

### import:

```clojure
.Macro
```

Allows importing JVM classes, and using them as types\.

```clojure
... Their methods, fields and enum options can also be imported.

(import: java/lang/Object
  ["#::."
   (new [])
   (equals [java/lang/Object] boolean)
   (wait [int] #io #try void)])

................................................................
................................................................

... Special options can also be given for the return values.

... #? means that the values will be returned inside a Maybe type. That way, null becomes #.None.

... #try means that the computation might throw an exception, and the return value will be wrapped by the Try type.

... #io means the computation has side effects, and will be wrapped by the IO type.

... These options must show up in the following order [#io #try #?] (although, each option can be used independently).

(import: java/lang/String
  ["#::."
   (new [[byte]])
   (#static valueOf [char] java/lang/String)
   (#static valueOf #as int_valueOf [int] java/lang/String)])

(import: (java/util/List e)
  ["#::."
   (size [] int)
   (get [int] e)])

(import: (java/util/ArrayList a)
  ["#::."
   ([T] toArray [[T]] [T])])

................................................................
................................................................

... The class-type that is generated is of the fully-qualified name.

... This avoids a clash between the java.util.List type, and Lux's own List type.

... All enum options to be imported must be specified.

(import: java/lang/Character$UnicodeScript
  ["#::."
   (#enum ARABIC CYRILLIC LATIN)])

................................................................
................................................................

... It should also be noted, the only types that may show up in method arguments or return values may be Java classes, arrays, primitives, void or type-vars.

... Lux types, such as Maybe cannot be named (otherwise, they'd be confused for Java classes).

(import: (lux/concurrency/async/JvmAsync A)
  ["#::."
   (resolve [A] boolean)
   (poll [] A)
   (wasResolved [] boolean)
   (waitOn [lux/Function] void)
   (#static [A] make [A] (lux/concurrency/async/JvmAsync A))])

................................................................
................................................................

... Also, the names of the imported members will look like Class::member

(java/lang/Object::new [])

(java/lang/Object::equals [other_object] my_object)

(java/util/List::size [] my_list)

java/lang/Character$UnicodeScript::LATIN
```

### int

```clojure
... .Type
(primitive "int")
```

The type of an \(unboxed\) int value\.

### int\_to\_byte

```clojure
.Macro
```

Type converter\.

```clojure
       (:














                            Byte          (













int_to_byte                  (:













                  Integer                            foo)))
```

### int\_to\_char

```clojure
.Macro
```

Type converter\.

```clojure
       (:



















                            Character          (


















int_to_char                  (:


















                  Integer                            foo)))
```

### int\_to\_double

```clojure
.Macro
```

Type converter\.

```clojure
       (:


















                            Double          (

















int_to_double                  (:

















                  Integer                            foo)))
```

### int\_to\_float

```clojure
.Macro
```

Type converter\.

```clojure
       (:

















                            Float          (
















int_to_float                  (:
















                  Integer                            foo)))
```

### int\_to\_long

```clojure
.Macro
```

Type converter\.

```clojure
       (:
















                            Long          (















int_to_long                  (:















                  Integer                            foo)))
```

### int\_to\_short

```clojure
.Macro
```

Type converter\.

```clojure
       (:















                            Short          (














int_to_short                  (:














                  Integer                            foo)))
```

### interface:

```clojure
.Macro
```

Allows defining JVM interfaces\.

```clojure
(interface: TestInterface
  ([] foo [boolean String] void #throws [Exception]))
```

### length

```clojure
.Macro
```

Gives the length of an array\.

```clojure
(length my_array)
```

### long

```clojure
... .Type
(primitive "long")
```

The type of an \(unboxed\) long value\.

### long\_to\_byte

```clojure
.Macro
```

Type converter\.

```clojure
       (:





















                            Byte          (




















long_to_byte                  (:




















                  Long                            foo)))
```

### long\_to\_char

```clojure
.Macro
```

Type converter\.

```clojure
       (:
































                            Character          (































long_to_char                  (:































                  Long                            foo)))
```

### long\_to\_double

```clojure
.Macro
```

Type converter\.

```clojure
       (:

























                            Double          (
























long_to_double                  (:
























                  Long                            foo)))
```

### long\_to\_float

```clojure
.Macro
```

Type converter\.

```clojure
       (:
























                            Float          (























long_to_float                  (:























                  Long                            foo)))
```

### long\_to\_int

```clojure
.Macro
```

Type converter\.

```clojure
       (:























                            Integer          (






















long_to_int                  (:






















                  Long                            foo)))
```

### long\_to\_short

```clojure
.Macro
```

Type converter\.

```clojure
       (:






















                            Short          (





















long_to_short                  (:





















                  Long                            foo)))
```

### null

```clojure
.Macro
```

The null pointer\.

```clojure
(null)
```

### null?

```clojure
(-> (primitive "java.lang.Object") .Bit)
```

Test for the null pointer\.

```clojure
(= true
   (null? (null)))

................................................................
................................................................

(= false
   (null? "YOLO"))
```

### object

```clojure
.Macro
```

Allows defining anonymous classes\.

```clojure
... The 1st tuple corresponds to class-level type-variables.

... The 2nd tuple corresponds to parent interfaces.

... The 3rd tuple corresponds to arguments to the super class constructor.

... An optional super-class can be specified before the 1st tuple. If not specified, java.lang.Object will be assumed.

(object [] [Runnable]
  []
  (Runnable [] (run self) void
            (exec
              (do_something some_value)
              [])))
```

### read\!

```clojure
.Macro
```

Loads an element from an array\.

```clojure
(read! 10 my_array)
```

### short

```clojure
... .Type
(primitive "short")
```

The type of an \(unboxed\) short value\.

### short\_to\_char

```clojure
.Macro
```

Type converter\.

```clojure
       (:




































                            Character          (



































short_to_char                  (:



































                  Short                            foo)))
```

### short\_to\_int

```clojure
.Macro
```

Type converter\.

```clojure
       (:


































                            Integer          (

































short_to_int                  (:

































                  Short                            foo)))
```

### short\_to\_long

```clojure
.Macro
```

Type converter\.

```clojure
       (:




                            Long          (



short_to_long                  (:



                  Short                            foo)))
```

### synchronized

```clojure
.Macro
```

Evaluates body, while holding a lock on a given object\.

```clojure
(synchronized object_to_be_locked
  (exec
    (do something)
    (dosomething else)
    (finish the computation)))
```

### type

```clojure
.Macro
```

```clojure
(: Type
   (type java/lang/String))
```

### unexpected\_type\_variable

```clojure
(library/lux/control/exception.Exception [.Text (.List (library/lux/target/jvm/type.Type library/lux/target/jvm/type/category.Var))])
```

### write\!

```clojure
.Macro
```

Stores an element into an array\.

```clojure
(write! 10 my_object my_array)
```

___

# library/lux/locale

## Definitions

### Locale

```clojure
... .Type
(primitive "library/lux/locale.Locale")
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
(primitive "library/lux/locale/language.Language")
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
(primitive "library/lux/locale/territory.Territory")
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

### identifier

```clojure
(-> .Text (.Meta .Code))
```

Generates a unique name as an Code node \(ready to be used in code templates\)\.
A prefix can be given \(or just be empty text\) to better identify the code for debugging purposes\.

```clojure
(identifier prefix)
```

### log\_expansion\!

```clojure
.Macro
```

Performs a macro\-expansion and logs the resulting code\.
You can either use the resulting code, or omit them\.
By omitting them, this macro produces nothing \(just like the lux\.comment macro\)\.

```clojure
   (





log_expansion!            #omit
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
   (






log_full_expansion!            #omit
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
   (




log_single_expansion!            #omit
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

### with\_identifiers

```clojure
.Macro
```

Creates new identifiers and offers them to the body expression\.

```clojure
(syntax: (synchronized [lock any
                        body any])
  (with_identifiers [g!lock g!body g!_]
    (in (list (` (let [(~ g!lock) (~ lock)
                       (~ g!_) ("jvm monitorenter" (~ g!lock))
                       (~ g!body) (~ body)
                       (~ g!_) ("jvm monitorexit" (~ g!lock))]
                   (~ g!body)))))))
```

### wrong\_syntax\_error

```clojure
(-> .Name .Text)
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

### identifier

```clojure
(-> .Name .Code)
```

### int

```clojure
(-> .Int .Code)
```

### local\_identifier

```clojure
(-> .Text .Code)
```

Produces a local identifier \(an identifier with no module prefix\)\.

### local\_tag

```clojure
(-> .Text .Code)
```

Produces a local tag \(a tag with no module prefix\)\.

### nat

```clojure
(-> .Nat .Code)
```

### record

```clojure
(-> (.List [.Code .Code]) .Code)
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

### tag

```clojure
(-> .Name .Code)
```

### text

```clojure
(-> .Text .Code)
```

### tuple

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
(-> (.List [.Name .Macro]) (.Meta .Code))
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
(syntax: .public(object [.let[imports (class_imports *lux*)]
                          .let[class_vars (list)]
                          super (opt (super_class_decl^ imports class_vars))
                          interfaces (tuple (some (super_class_decl^ imports class_vars)))
                          constructor_args (constructor_args^ imports class_vars)
                          methods (some (overriden_method_def^ imports))])
  (let [def_code ($_ text\composite "anon-class:"
                     (spaced (list (super_class_decl$ (maybe.else object_super_class super))
                                   (with_brackets (spaced (list\each super_class_decl$ interfaces)))
                                   (with_brackets (spaced (list\each constructor_arg$ constructor_args)))
                                   (with_brackets (spaced (list\each (method_def$ id) methods))))))]
    (in (list (` ((~ (code.text def_code))))))))
```

___

# library/lux/macro/syntax/annotations

## Definitions

### Annotations

```clojure
... .Type
(.List [.Name .Code])
```

Definition/module annotations\.

### empty

```clojure
Annotations
```

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Annotations)
```

### format

```clojure
(-> Annotations .Code)
```

### parser

```clojure
(library/lux/control/parser/code.Parser Annotations)
```

___

# library/lux/macro/syntax/check

## Definitions

### Check

```clojure
... .Type
[.Code .Code]
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
[.Text (.List .Text)]
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
[.Text (.Either library/lux/macro/syntax/check.Check .Code) library/lux/macro/syntax/annotations.Annotations .Bit]
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
(All (_ a) (-> (library/lux/control/parser/code.Parser a) (library/lux/control/parser/code.Parser [.Code a])))
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
[.Code .Code]
```

The common typed\-argument syntax used by many macros\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence Input)
```

### format

```clojure
(-> Input .Code)
```

### parser

```clojure
(library/lux/control/parser/code.Parser Input)
```

Parser for the common typed\-argument syntax used by many macros\.

___

# library/lux/macro/syntax/type/variable

## Definitions

### Variable

```clojure
... .Type
(primitive "#Text")
```

A variable'S name\.

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

### identifier

```clojure
.Macro
```

An identifier made by concatenating pieces of code\.
The \(optional\) module part and the short part are specified independently\.

```clojure
(






  identifier        ["abc" .defdocumentation/lux/macro/template.ghi])

... =>

abcdefghi

................................................................
................................................................

(



  identifier        [.def]["abc" .defdocumentation/lux/macro/template.ghi])

... =>

.abcdefghi
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

### tag

```clojure
.Macro
```

A tag made by concatenating pieces of code\.
The \(optional\) module part and the short part are specified independently\.

```clojure
(







  tag        ["abc" .defdocumentation/lux/macro/template.ghi])

... =>

#abcdefghi

................................................................
................................................................

(




  tag        [.def]["abc" .defdocumentation/lux/macro/template.ghi])

... =>

#library/lux.abcdefghi
```

### text

```clojure
.Macro
```

A text literal made by concatenating pieces of code\.

```clojure
(text [#0 123 +456 +789.0 "abc" .defdocumentation/lux/macro/template.ghi])

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
(infix [x * +10])

................................................................
................................................................

(infix [[x + y] * [x - y]])

................................................................
................................................................

(infix [sin [x + y]])

................................................................
................................................................

(infix [[x < y] and [y < z]])

................................................................
................................................................

(infix [#and x < y < z])

................................................................
................................................................

(infix [(* 3 9) gcd 450])
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
(All (_ a) (-> (Fuzzy a) (Fuzzy a)))
```

### cut

```clojure
(All (_ a) (-> .Rev (Fuzzy a) (Fuzzy a)))
```

```clojure
(cut treshold set)
```

### difference

```clojure
(All (_ a) (-> (Fuzzy a) (Fuzzy a) (Fuzzy a)))
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
(All (_ a) (-> (Fuzzy a) (Fuzzy a) (Fuzzy a)))
```

### membership

```clojure
(All (_ a) (-> (Fuzzy a) a .Rev))
```

```clojure
(membership set elem)
```

### of\_predicate

```clojure
(All (_ a) (-> (library/lux/abstract/predicate.Predicate a) (Fuzzy a)))
```

```clojure
(of_predicate predicate)
```

### of\_set

```clojure
(All (_ a) (-> (library/lux/data/collection/set.Set a) (Fuzzy a)))
```

### predicate

```clojure
(All (_ a) (-> .Rev (Fuzzy a) (library/lux/abstract/predicate.Predicate a)))
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
(All (_ a) (-> (Fuzzy a) (Fuzzy a) (Fuzzy a)))
```

___

# library/lux/math/modular

## Definitions

### \*

```clojure
(All (_ a) (-> (Mod a) (Mod a) (Mod a)))
```

### \+

```clojure
(All (_ a) (-> (Mod a) (Mod a) (Mod a)))
```

### \-

```clojure
(All (_ a) (-> (Mod a) (Mod a) (Mod a)))
```

### <

```clojure
(All (_ a) (-> (Mod a) (Mod a) .Bit))
```

### <=

```clojure
(All (_ a) (-> (Mod a) (Mod a) .Bit))
```

### =

```clojure
(All (_ a) (-> (Mod a) (Mod a) .Bit))
```

### >

```clojure
(All (_ a) (-> (Mod a) (Mod a) .Bit))
```

### >=

```clojure
(All (_ a) (-> (Mod a) (Mod a) .Bit))
```

### \(Mod %\)

```clojure
... .Type
(primitive "library/lux/math/modular.Mod" %)
```

A number under a modulus\.

### adapter

```clojure
(All (_ a b) (-> (library/lux/math/modulus.Modulus a) (library/lux/math/modulus.Modulus b) (library/lux/control/try.Try (-> (Mod b) (Mod a)))))
```

```clojure
(adapter reference subject)
```

### addition

```clojure
(All (_ a) (-> (library/lux/math/modulus.Modulus a) (library/lux/abstract/monoid.Monoid (Mod a))))
```

### codec

```clojure
(All (_ a) (-> (library/lux/math/modulus.Modulus a) (library/lux/abstract/codec.Codec .Text (Mod a))))
```

### equivalence

```clojure
(All (_ a) (library/lux/abstract/equivalence.Equivalence (Mod a)))
```

### incorrect\_modulus

```clojure
(All (_ a) (library/lux/control/exception.Exception [(library/lux/math/modulus.Modulus a) .Int]))
```

### inverse

```clojure
(All (_ a) (-> (Mod a) (.Maybe (Mod a))))
```

### modular

```clojure
(All (_ a) (-> (library/lux/math/modulus.Modulus a) .Int (Mod a)))
```

```clojure
(modular modulus value)
```

### moduli\_are\_not\_equal

```clojure
(All (_ a b) (library/lux/control/exception.Exception [(library/lux/math/modulus.Modulus a) (library/lux/math/modulus.Modulus b)]))
```

### modulus

```clojure
(All (_ a) (-> (Mod a) (library/lux/math/modulus.Modulus a)))
```

### multiplication

```clojure
(All (_ a) (-> (library/lux/math/modulus.Modulus a) (library/lux/abstract/monoid.Monoid (Mod a))))
```

### order

```clojure
(All (_ a) (library/lux/abstract/order.Order (Mod a)))
```

### value

```clojure
(All (_ a) (-> (Mod a) .Int))
```

___

# library/lux/math/modulus

## Definitions

### =

```clojure
(All (_ a b) (-> (Modulus a) (Modulus b) .Bit))
```

### \(Modulus %\)

```clojure
... .Type
(primitive "library/lux/math/modulus.Modulus" %)
```

A number used as a modulus in modular arithmetic\.
It cannot be 0\.

### congruent?

```clojure
(All (_ a) (-> (Modulus a) .Int .Int .Bit))
```

```clojure
(congruent? modulus reference subject)
```

### divisor

```clojure
(All (_ a) (-> (Modulus a) .Int))
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
(Ex (_ a) (-> .Int (library/lux/control/try.Try (Modulus a))))
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
[.Frac .Frac]
```

### \-

```clojure
(-> Complex Complex Complex)
```

### \-one

```clojure
[.Frac .Frac]
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
[.Frac .Frac]
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
[.Frac .Frac]
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
[.Frac .Frac]
```

___

# library/lux/math/number/frac

## Definitions

### %

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) remainder\.

```clojure
   (





%           param subject)
```

### \*

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) multiplication\.

```clojure
   (



*           param subject)
```

### \+

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) addition\.

```clojure
   (

+           param subject)
```

### \-

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) substraction\.

```clojure
   (


-           param subject)
```

### /

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) division\.

```clojure
   (




/           param subject)
```

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

```clojure
   (


max           left right)
```

### maximum

```clojure
(library/lux/abstract/monoid.Monoid .Frac)
```

### min

```clojure
(-> .Frac .Frac .Frac)
```

Frac\(tion\) minimum\.

```clojure
   (

min           left right)
```

### minimum

```clojure
(library/lux/abstract/monoid.Monoid .Frac)
```

### mod

```clojure
(All (_ a) (-> .Frac .Frac .Frac))
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
(Or [.Text (.List ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing))] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] .Nat .Nat .Nat [(.List ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [(.List ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [.Name ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)])
```

A 16\-bit integer\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence (.I64 (primitive "{New Type @"library/lux/math/number/i16",13,17 0}")))
```

### i16

```clojure
(-> .I64 (.I64 (primitive "{New Type @"library/lux/math/number/i16",13,17 0}")))
```

### i64

```clojure
(-> (.I64 (primitive "{New Type @"library/lux/math/number/i16",13,17 0}")) .I64)
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
(Or [.Text (.List ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing))] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] .Nat .Nat .Nat [(.List ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [(.List ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [.Name ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)])
```

A 32\-bit integer\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence (.I64 (primitive "{New Type @"library/lux/math/number/i32",13,17 0}")))
```

### i32

```clojure
(-> .I64 (.I64 (primitive "{New Type @"library/lux/math/number/i32",13,17 0}")))
```

### i64

```clojure
(-> (.I64 (primitive "{New Type @"library/lux/math/number/i32",13,17 0}")) .I64)
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
(All (Mask a) (primitive "#I64" a))
```

A pattern of bits that can be imposed on I64 values\.

### \(Sub width\)

```clojure
... .Type
[(library/lux/abstract/equivalence.Equivalence (.I64 width)) .Nat (-> .I64 (.I64 width)) (-> (.I64 width) .I64)]
```

A sub\-space of I64 with a reduce amount of bits\.

### and

```clojure
(All (_ a) (-> (.I64 .Any) (.I64 a) (.I64 a)))
```

Bitwise and\.

```clojure
   (



and           parameter subject)
```

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
(All (_ a) (library/lux/abstract/monoid.Monoid (.I64 a)))
```

### disjunction

```clojure
(All (_ a) (library/lux/abstract/monoid.Monoid (.I64 a)))
```

### equivalence

```clojure
(All (_ a) (library/lux/abstract/equivalence.Equivalence (.I64 a)))
```

### false

```clojure
Mask
```

### flipped

```clojure
(All (_ a) (-> .Nat (.I64 a) (.I64 a)))
```

Flip bit at given index\.

```clojure
   (


flipped           index input)
```

### hash

```clojure
(All (_ a) (library/lux/abstract/hash.Hash (.I64 a)))
```

### left\_rotated

```clojure
(All (_ a) (-> .Nat (.I64 a) (.I64 a)))
```

```clojure
   (

left_rotated           distance input)
```

### left\_shifted

```clojure
(All (_ a) (-> .Nat (.I64 a) (.I64 a)))
```

Bitwise left\-shift\.

```clojure
   (





left_shifted           parameter subject)
```

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
(All (_ a) (-> (.I64 a) (.I64 a)))
```

Bitwise negation\.

### one

```clojure
(All (_ a) (-> .Nat (.I64 a) (.I64 a)))
```

Set bit at given index\.

```clojure
   (

one           index input)
```

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
(All (_ a) (-> (.I64 .Any) (.I64 a) (.I64 a)))
```

Bitwise or\.

```clojure
   (

or           parameter subject)
```

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
(All (_ a) (-> (.I64 a) (.I64 a)))
```

### right\_rotated

```clojure
(All (_ a) (-> .Nat (.I64 a) (.I64 a)))
```

```clojure
   (


right_rotated           distance input)
```

### right\_shifted

```clojure
(All (_ a) (-> .Nat (.I64 a) (.I64 a)))
```

Unsigned/logic bitwise right\-shift\.

```clojure
   (






right_shifted           parameter subject)
```

### sign

```clojure
Mask
```

A mask for the sign bit of ints\.

### sub

```clojure
(Ex (_ a) (-> .Nat (.Maybe (Sub a))))
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
(All (_ a) (-> (.I64 .Any) (.I64 a) (.I64 a)))
```

Bitwise xor\.

```clojure
   (


xor           parameter subject)
```

### zero

```clojure
(All (_ a) (-> .Nat (.I64 a) (.I64 a)))
```

Clear bit at the given index\.

```clojure
(zero index input)
```

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
(Or [.Text (.List ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing))] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] .Nat .Nat .Nat [(.List ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [(.List ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing) ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)] [.Name ((All (_ a) (Or [.Text (.List (? a))] [(? a) (? a)] [(? a) (? a)] [(? a) (? a)] .Nat .Nat .Nat [(.List (? a)) (? a)] [(.List (? a)) (? a)] [(? a) (? a)] [.Name (? a)])) .Nothing)])
```

A 8\-bit integer\.

### equivalence

```clojure
(library/lux/abstract/equivalence.Equivalence (.I64 (primitive "{New Type @"library/lux/math/number/i8",13,17 0}")))
```

### i64

```clojure
(-> (.I64 (primitive "{New Type @"library/lux/math/number/i8",13,17 0}")) .I64)
```

### i8

```clojure
(-> .I64 (.I64 (primitive "{New Type @"library/lux/math/number/i8",13,17 0}")))
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

```clojure
   (





%           param subject)
```

### \*

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) multiplication\.

```clojure
   (



*           param subject)
```

### \+

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) addition\.

```clojure
   (

+           param subject)
```

### \-

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) substraction\.

```clojure
   (


-           param subject)
```

### /

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) division\.

```clojure
   (




/           param subject)
```

### /%

```clojure
(-> .Int .Int [.Int .Int])
```

Int\(eger\) \[division remainder\]\.

```clojure
   (






/%           param subject)
```

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

```clojure
   (


max           left right)
```

### maximum

```clojure
(library/lux/abstract/monoid.Monoid .Int)
```

### min

```clojure
(-> .Int .Int .Int)
```

Int\(eger\) minimum\.

```clojure
   (

min           left right)
```

### minimum

```clojure
(library/lux/abstract/monoid.Monoid .Int)
```

### mod

```clojure
(All (_ a) (-> .Int .Int .Int))
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

```clojure
   (











%           parameter subject)
```

### \*

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) multiplication\.

```clojure
   (








*           parameter subject)
```

### \+

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) addition\.

```clojure
   (


+           parameter subject)
```

### \-

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) substraction\.

```clojure
   (



-           parameter subject)
```

### /

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) division\.

```clojure
   (









/           parameter subject)
```

### /%

```clojure
(-> .Nat .Nat [.Nat .Nat])
```

Nat\(ural\) \[division remainder\]\.

```clojure
   (










/%           parameter subject)
```

### <

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) less\-than\.

```clojure
   (




<           parameter subject)
```

### <=

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) less\-than or equal\.

```clojure
   (





<=           parameter subject)
```

### =

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) equivalence\.

```clojure
   (

=           parameter subject)
```

### >

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) greater\-than\.

```clojure
   (






>           parameter subject)
```

### >=

```clojure
(-> .Nat .Nat .Bit)
```

Nat\(ural\) greater\-than or equal\.

```clojure
   (







>=           parameter subject)
```

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

```clojure
   (


max           left right)
```

### maximum

```clojure
(library/lux/abstract/monoid.Monoid .Nat)
```

### min

```clojure
(-> .Nat .Nat .Nat)
```

Nat\(ural\) minimum\.

```clojure
   (

min           left right)
```

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
[.Nat .Nat]
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

```clojure
   (





%           param subject)
```

### \*

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) multiplication\.

```clojure
   (



*           param subject)
```

### \+

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) addition\.

```clojure
   (

+           param subject)
```

### \-

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) substraction\.

```clojure
   (


-           param subject)
```

### /

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) division\.

```clojure
   (




/           param subject)
```

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
   (


down           scale subject)
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

```clojure
   (


max           left right)
```

### maximum

```clojure
(library/lux/abstract/monoid.Monoid .Rev)
```

### min

```clojure
(-> .Rev .Rev .Rev)
```

Rev\(olution\) minimum\.

```clojure
   (

min           left right)
```

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

```clojure
   (






ratio           param subject)
```

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
   (

up           scale subject)
```

___

# library/lux/math/random

Pseudo\-random number generation \(PRNG\) algorithms\.

## Definitions

### PRNG

```clojure
... .Type
((All (PRNG a) (-> .Any [(PRNG .Nothing) .I64])) .Nothing)
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
(All (_ a b) (-> (Random a) (Random b) (Random [a b])))
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
(All (_ a) (-> .Nat (Random a) (Random (library/lux/data/collection/array.Array a))))
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
(All (_ a b) (-> (library/lux/abstract/hash.Hash a) .Nat (Random a) (Random b) (Random (library/lux/data/collection/dictionary.Dictionary a b))))
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
(All (_ a) (-> (Random a) (Random a) (Random a)))
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
(All (_ a) (-> .Nat (Random a) (Random (.List a))))
```

### maybe

```clojure
(All (_ a) (-> (Random a) (Random (.Maybe a))))
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
(All (_ a b) (-> (-> a (.Maybe b)) (Random a) (Random b)))
```

```clojure
(one check random)
```

### only

```clojure
(All (_ a) (-> (-> a .Bit) (Random a) (Random a)))
```

Retries the generator until the output satisfies a predicate\.

```clojure
(only pred gen)
```

### or

```clojure
(All (_ a b) (-> (Random a) (Random b) (Random (Or a b))))
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
(All (_ a) (-> (-> a a) (-> a .I64) a PRNG))
```

```clojure
(prng update return)
```

### queue

```clojure
(All (_ a) (-> .Nat (Random a) (Random (library/lux/data/collection/queue.Queue a))))
```

### ratio

```clojure
(Random library/lux/math/number/ratio.Ratio)
```

### rec

```clojure
(All (_ a) (-> (-> (Random a) (Random a)) (Random a)))
```

A combinator for producing recursive random generators\.

```clojure
(rec gen)
```

### refined

```clojure
(All (_ a b) (-> (library/lux/type/refinement.Refiner a b) (Random a) (Random (library/lux/type/refinement.Refined a b))))
```

Retries the generator until the output can be refined\.

```clojure
(refined refiner gen)
```

### result

```clojure
(All (_ a) (-> PRNG (Random a) [PRNG a]))
```

```clojure
(result prng calc)
```

### rev

```clojure
(Random .Rev)
```

### row

```clojure
(All (_ a) (-> .Nat (Random a) (Random (library/lux/data/collection/row.Row a))))
```

### safe\_frac

```clojure
(Random .Frac)
```

A number in the interval \[0\.0,1\.0\]\.

### set

```clojure
(All (_ a) (-> (library/lux/abstract/hash.Hash a) .Nat (Random a) (Random (library/lux/data/collection/set.Set a))))
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
(All (_ a) (-> .Nat (Random a) (Random (library/lux/data/collection/stack.Stack a))))
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
(-> .Name (.Meta .Name))
```

Given an aliased definition's name, returns the original definition being referenced\.

```clojure
(de_aliased def_name)
```

### definition

```clojure
(-> .Name (.Meta .Global))
```

Looks\-up a definition's whole data in the available modules \(including the current one\)\.

```clojure
(definition name)
```

### definition\_type

```clojure
(-> .Name (.Meta .Type))
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
(All (_ a) (-> (.Meta a) (.Meta a) (.Meta a)))
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
(-> .Name (.Meta .Definition))
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
(All (_ a) (-> .Text (.Meta a)))
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
(All (_ a) (-> (library/lux/control/try.Try a) (.Meta a)))
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
(-> .Name (.Meta (.Maybe .Macro)))
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
(-> .Name (.Meta .Name))
```

If given a name without a module prefix, gives it the current module's name as prefix\.
Otherwise, returns the name as\-is\.

```clojure
(normal name)
```

### result

```clojure
(All (_ a) (-> .Lux (.Meta a) (library/lux/control/try.Try a)))
```

Evaluates a computation that depends on Lux's compiler state\.

```clojure
(result lux action)
```

### result'

```clojure
(All (_ a) (-> .Lux (.Meta a) (library/lux/control/try.Try [.Lux a])))
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

### tag

```clojure
(-> .Name (.Meta [.Nat (.List .Name) .Type]))
```

Given a tag, finds out what is its index, its related tag\-list and its associated type\.

```clojure
(tag tag_name)
```

### tag\_lists

```clojure
(-> .Text (.Meta (.List [(.List .Name) .Type])))
```

All the tag\-lists defined in a module, with their associated types\.

```clojure
(tag_lists module)
```

### tags\_of

```clojure
(-> .Name (.Meta (.Maybe (.List .Name))))
```

All the tags associated with a type definition\.

```clojure
(tags_of type_name)
```

### type

```clojure
(-> .Name (.Meta .Type))
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
(-> .Name (.Meta .Type))
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

# library/lux/meta/annotation

Machinary for querying annotations on modules and definitions\.

## Definitions

### Annotation

```clojure
... .Type
(.Ann .Location (.Code' (.Ann .Location)))
```

### bit

```clojure
(-> .Name Annotation (.Maybe .Bit))
```

```clojure
   (


bit           tag ann)
```

### documentation

```clojure
(-> Annotation (.Maybe .Text))
```

### flagged?

```clojure
(-> .Name Annotation .Bit)
```

### form

```clojure
(-> .Name Annotation (.Maybe (.List .Code)))
```

```clojure
   (










form           tag ann)
```

### frac

```clojure
(-> .Name Annotation (.Maybe .Frac))
```

```clojure
   (






frac           tag ann)
```

### function\_arguments

```clojure
(-> Annotation (.List .Text))
```

### identifier

```clojure
(-> .Name Annotation (.Maybe .Name))
```

```clojure
   (








identifier           tag ann)
```

### implementation?

```clojure
(-> Annotation .Bit)
```

### int

```clojure
(-> .Name Annotation (.Maybe .Int))
```

```clojure
   (




int           tag ann)
```

### nat

```clojure
(-> .Name Annotation (.Maybe .Nat))
```

```clojure
   (



nat           tag ann)
```

### record

```clojure
(-> .Name Annotation (.Maybe (.List [.Code .Code])))
```

```clojure
   (












record           tag ann)
```

### rev

```clojure
(-> .Name Annotation (.Maybe .Rev))
```

```clojure
   (





rev           tag ann)
```

### tag

```clojure
(-> .Name Annotation (.Maybe .Name))
```

```clojure
   (









tag           tag ann)
```

### text

```clojure
(-> .Name Annotation (.Maybe .Text))
```

```clojure
   (







text           tag ann)
```

### tuple

```clojure
(-> .Name Annotation (.Maybe (.List .Code)))
```

```clojure
   (











tuple           tag ann)
```

### type\_arguments

```clojure
(-> Annotation (.List .Text))
```

### value

```clojure
(-> .Name Annotation (.Maybe .Code))
```

```clojure
   (

value           tag ann)
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

```clojure
   (






frac    (:





       .Frac       (value generating expression)))
```

### int

```clojure
.Macro
```

```clojure
   (




int    (:



      .Int       (value generating expression)))
```

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

```clojure
   (



nat    (:


      .Nat       (value generating expression)))
```

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

```clojure
   (:





              .Frac      (




random_frac))
```

### random\_int

```clojure
.Macro
```

```clojure
   (:



             .Int      (


random_int))
```

### random\_nat

```clojure
.Macro
```

```clojure
   (:


             .Nat      (

random_nat))
```

### random\_rev

```clojure
.Macro
```

```clojure
   (:




             .Rev      (



random_rev))
```

### rev

```clojure
.Macro
```

```clojure
   (





rev    (:




      .Rev       (value generating expression)))
```

### text

```clojure
.Macro
```

```clojure
   (







text    (:






       .Text       (value generating expression)))
```

___

# library/lux/target

## Definitions

### Target

```clojure
... .Type
(primitive "#Text")
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
(primitive "#I64" (primitive "#Nat"))
```

The seed value used for random testing \(if that feature is used\)\.

### Tally

```clojure
... .Type
[.Nat .Nat (library/lux/data/collection/set.Set .Name) (library/lux/data/collection/set.Set .Name)]
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
   (




cover            [definition/0 definition/1 ,,, definition/N]
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
   (



cover'            [definition/0 definition/1 ,,, definition/N]
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
[.Nat .Nat .Nat .Nat]
```

A clock marking the specific hour, minute, second, and milli\-second in a day\.

### Time

```clojure
... .Type
(primitive "library/lux/time.Time")
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
(primitive "library/lux/time/date.Date")
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
(Or .Any .Any .Any .Any .Any .Any .Any)
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
(primitive "library/lux/time/duration.Duration")
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
(primitive "library/lux/time/instant.Instant")
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
(Or .Any .Any .Any .Any .Any .Any .Any .Any .Any .Any .Any .Any)
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
(primitive "#I64" (primitive "#Nat"))
```

An amount of years\.

### Year

```clojure
... .Type
(primitive "library/lux/time/year.Year")
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

(.type(Bar Bit Nat Text))
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

```clojure
   (


ex_q           size body)
```

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

```clojure
   (


flat_ex_q           type)
```

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

```clojure
   (


flat_tuple           type)
```

### flat\_univ\_q

```clojure
(-> .Type [.Nat .Type])
```

The number of parameters, and the body, of a quantified type\.

```clojure
   (

flat_univ_q           type)
```

### flat\_variant

```clojure
(-> .Type (.List .Type))
```

The members of a composite type\.

```clojure
   (

flat_variant           type)
```

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

```clojure
   (


tuple           types)
```

### univ\_q

```clojure
(-> .Nat .Type .Type)
```

A quantified type, with the given number of parameters, and body\.

```clojure
   (

univ_q           size body)
```

### variant

```clojure
(-> (.List .Type) .Type)
```

A composite type, constituted by the given member types\.

```clojure
   (

variant           types)
```

___

# library/lux/type/abstract

## Definitions

### :abstraction

```clojure
.Macro
```

Type\-casting macro for abstract/nominal types\.

```clojure
   (:


                              abstraction      (

:abstraction              (:

               representation                        value)))
```

### :representation

```clojure
.Macro
```

Type\-casting macro for abstract/nominal types\.

```clojure
   (:



                              representation      (


:representation              (:


                  abstraction                        value)))
```

### :transmutation

```clojure
.Macro
```

Transmutes an abstract/nominal type's phantom types\.

```clojure
(abstract: (JavaScript a)
  {}

  Text

  (abstract: Expression {} Any)
  (abstract: Statement {} Any)

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
[.Text (.List .Code) .Code .Code]
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
  {}

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
  {}

  [a a]

  (def: (duplicate value)
    (All (_ a) (-> a (Duplicate a)))
    (:abstraction [value value])))

................................................................
................................................................

... Definitions can be nested.

(abstract: (Single a)
  {}

  a

  (def: (single value)
    (All (_ a) (-> a (Single a)))
    (:abstraction value))

  (abstract: (Double a)
    {}

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
  {}

  Text

  (abstract: Expression {} Any)
  (abstract: Statement {} Any)

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
(primitive "#I64" (primitive "#Nat"))
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
(All (_ a b) (-> (library/lux/control/exception.Exception a) a (Check b)))
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
(All (_ a) (-> .Text (Check a)))
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
(All (_ a) (-> .Type_Context (Check a) (library/lux/control/try.Try a)))
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
(primitive "library/lux/type/dynamic.Dynamic")
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

### \\\\

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

(\ number.equivalence = x y)

(\\ = x y)

................................................................
................................................................

... Can optionally add the prefix of the module where the signature was defined.

(\\ equivalence.= x y)

................................................................
................................................................

... (List Nat) equivalence

(\\ =
    (list.indices 10)
    (list.indices 10))

................................................................
................................................................

... (Functor List) each

(\\ each ++ (list.indices 10))
```

### implicit:

```clojure
.Macro
```

Establish local definitions for implementations that will be prioritized over foreign definitions\.

```clojure
(implicit: [n.multiplication])

(n.= (\ n.multiplication composite left right)
     (\\ composite left right))
```

### with

```clojure
.Macro
```

Establish lexical bindings for implementations that will be prioritized over non\-lexically\-bound implementations\.

```clojure
(with [n.addition]
  (n.= (\ n.addition composite left right)
       (\\ composite left right)))
```

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

### derived:

```clojure
.Macro
```

```clojure
(type: Variant
  (.Variant
   (#Bit Bit)
   (#Text Text)
   (#Frac Frac)))

(type: Recursive
  (Rec Recursive
    (.Variant
     (#Number Frac)
     (#Addition Frac Recursive))))

(type: Record
  (.Record
   {#bit Bit
    #frac Frac
    #text Text
    #maybe (Maybe Frac)
    #list (List Frac)
    #dictionary (Dictionary Text Frac)
    #variant Variant
    #tuple [Bit Text Frac]
    #recursive Recursive
    #date Date
    #grams (Qty Gram)}))

(derived: equivalence
  (specification/lux/abstract/equivalence.equivalence
   Record))

(: (Equivalence Record)
   equivalence)

(derived: codec
  (specification/lux/abstract/codec.codec
   Record))

(: (Codec Json Record)
   codec)
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
(All (_ c) (primitive "library/lux/type/quotient.Class" value label c))
```

The class knows how to classify/label values that are meant to be equivalent to one another\.

### \(Quotient value label\)

```clojure
... .Type
(All (_ c) (primitive "library/lux/type/quotient.Quotient" value label c))
```

A quotient value has been labeled with a class\.
All equivalent values will belong to the same class\.
This means all equivalent values possess the same label\.

### class

```clojure
(All (_ a b) (Ex (_ c) (-> (-> a b) (Class a b c))))
```

### equivalence

```clojure
(All (_ a b c) (-> (library/lux/abstract/equivalence.Equivalence b) (library/lux/abstract/equivalence.Equivalence (Quotient a b c))))
```

### label

```clojure
(All (_ a b c) (-> (Quotient a b c) b))
```

### quotient

```clojure
(All (_ a b c) (-> (Class a b c) a (Quotient a b c)))
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
(All (_ a b c) (-> (Quotient a b c) a))
```

___

# library/lux/type/refinement

## Definitions

### \(Refined it\)

```clojure
... .Type
(All (_ b) (primitive "library/lux/type/refinement.Refined" it b))
```

A refined version of another type, using a predicate to select valid instances\.

### \(Refiner it\)

```clojure
... .Type
(All (_ b) (-> it (.Maybe (Refined it b))))
```

A selection mechanism for refined instances of a type\.

### lifted

```clojure
(All (_ a b) (-> (-> a a) (Refined a b) (.Maybe (Refined a b))))
```

Yields a function that can work on refined values\.
Respects the constraints of the refinement\.

```clojure
(lifted transform)
```

### only

```clojure
(All (_ a b) (-> (Refiner a b) (.List a) (.List (Refined a b))))
```

```clojure
(only refiner values)
```

### partition

```clojure
(All (_ a b) (-> (Refiner a b) (.List a) [(.List (Refined a b)) (.List a)]))
```

Separates refined values from the un\-refined ones\.

```clojure
(partition refiner values)
```

### predicate

```clojure
(All (_ a b) (-> (Refined a b) (library/lux/abstract/predicate.Predicate a)))
```

### refiner

```clojure
(All (_ a) (Ex (_ b) (-> (library/lux/abstract/predicate.Predicate a) (Refiner a b))))
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
(All (_ a b) (-> (Refined a b) a))
```

___

# library/lux/type/resource

## Definitions

### \(Affine monad permissions value\)

```clojure
... .Type
(All (_ d) (Procedure monad d [permissions d] value))
```

A procedure which expands the number of available resources\.

### Commutative

```clojure
... .Type
(primitive "library/lux/type/resource.Commutative")
```

The mode of keys which CAN be swapped, and for whom order of release/consumption DOES NOT matters\.

### \(Key mode key\)

```clojure
... .Type
(primitive "library/lux/type/resource.Key" mode key)
```

The access right for a resource\.
Without the key for a resource existing somewhere among the available ambient rights, one cannot use a resource\.

### \(Linear monad value\)

```clojure
... .Type
(All (_ c) (Procedure monad c c value))
```

A procedure that is constant with regards to resource access rights\.
This means no additional resources will be available after the computation is over\.
This also means no previously available resources will have been consumed\.

### Ordered

```clojure
... .Type
(primitive "library/lux/type/resource.Ordered")
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
(All (_ d) (Procedure monad [permissions d] d value))
```

A procedure which reduces the number of available resources\.

### \(Res key value\)

```clojure
... .Type
(primitive "library/lux/type/resource.Res" key value)
```

A resource locked by a key\.
The 'key' represents the right to access/consume a resource\.

### amount\_cannot\_be\_zero

```clojure
(library/lux/control/exception.Exception .Any)
```

### commutative

```clojure
(All (_ a b) (Ex (_ c) (-> (library/lux/abstract/monad.Monad a) b (Affine a (Key Commutative c) (Res c b)))))
```

Makes a value into a resource and adds the key/access\-right to it to the ambient keyring for future use\.

```clojure
   (


commutative           monad value)
```

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
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (a b) (Linear a b)))
```

```clojure
(lifted monad procedure)
```

### monad

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (library/lux/abstract/monad/indexed.IxMonad (Procedure a))))
```

### ordered

```clojure
(All (_ a b) (Ex (_ c) (-> (library/lux/abstract/monad.Monad a) b (Affine a (Key Ordered c) (Res c b)))))
```

Makes a value into a resource and adds the key/access\-right to it to the ambient keyring for future use\.

```clojure
   (

ordered           monad value)
```

### read

```clojure
(All (_ a b c d) (-> (library/lux/abstract/monad.Monad a) (Res c b) (Relevant a (Key d c) b)))
```

Access the value of a resource, so long as its key is available\.

```clojure
(read monad resource)
```

### run\!

```clojure
(All (_ a b) (-> (library/lux/abstract/monad.Monad a) (Linear a b) (a b)))
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
(All (_ a b) (-> (Qty a) (Qty b) (Qty [a b])))
```

```clojure
   (



*           param subject)
```

### \+

```clojure
(All (_ a) (-> (Qty a) (Qty a) (Qty a)))
```

```clojure
   (

+           param subject)
```

### \-

```clojure
(All (_ a) (-> (Qty a) (Qty a) (Qty a)))
```

```clojure
   (


-           param subject)
```

### /

```clojure
(All (_ a b) (-> (Qty a) (Qty [a b]) (Qty b)))
```

```clojure
   (




/           param subject)
```

### Giga

```clojure
... .Type
(All (Giga a) (primitive "library/lux/type/unit.Giga" a))
```

### Gram

```clojure
... .Type
(primitive "library/lux/type/unit.Gram")
```

### Kilo

```clojure
... .Type
(All (Kilo a) (primitive "library/lux/type/unit.Kilo" a))
```

### Litre

```clojure
... .Type
(primitive "library/lux/type/unit.Litre")
```

### Mega

```clojure
... .Type
(All (Mega a) (primitive "library/lux/type/unit.Mega" a))
```

### Meter

```clojure
... .Type
(primitive "library/lux/type/unit.Meter")
```

### Micro

```clojure
... .Type
(All (Micro a) (primitive "library/lux/type/unit.Micro" a))
```

### Milli

```clojure
... .Type
(All (Milli a) (primitive "library/lux/type/unit.Milli" a))
```

### Nano

```clojure
... .Type
(All (Nano a) (primitive "library/lux/type/unit.Nano" a))
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
(primitive "library/lux/type/unit.Qty" unit)
```

A quantity with an associated unit of measurement\.

### \(Scale scale\)

```clojure
... .Type
[(All (_ b) (-> (Qty b) (Qty (scale b)))) (All (_ b) (-> (Qty (scale b)) (Qty b))) library/lux/math/number/ratio.Ratio]
```

A scale of magnitude\.

### Second

```clojure
... .Type
(primitive "library/lux/type/unit.Second")
```

### \(Unit unit\)

```clojure
... .Type
[(-> .Int (Qty unit)) (-> (Qty unit) .Int)]
```

A unit of measurement, to qualify numbers with\.

### enum

```clojure
(All (_ a) (library/lux/abstract/enum.Enum (Qty a)))
```

### equivalence

```clojure
(All (_ a) (library/lux/abstract/equivalence.Equivalence (Qty a)))
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
(All (_ a) (library/lux/abstract/order.Order (Qty a)))
```

### pure

```clojure
(-> .Int Pure)
```

### re\_scaled

```clojure
(All (_ a b c) (-> (Scale a) (Scale b) (Qty (a c)) (Qty (b c))))
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
(scale: .publicBajillion bajillion
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
(unit: .publicFeet feet)
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
[(-> .Any (! (library/lux/control/try.Try library/lux/data/text.Char))) (-> .Any (! (library/lux/control/try.Try .Text))) (-> .Text (! (library/lux/control/try.Try .Any))) (-> .Any (! (library/lux/control/try.Try .Any)))]
```

An interface to console/terminal I/O\.

### \(Mock s\)

```clojure
... .Type
[(-> s (library/lux/control/try.Try [s library/lux/data/text.Char])) (-> s (library/lux/control/try.Try [s .Text])) (-> .Text s (library/lux/control/try.Try s)) (-> s (library/lux/control/try.Try s))]
```

A mock/simulation of a console\.
Useful for testing\.

### async

```clojure
(-> (Console library/lux/control/io.IO) (Console library/lux/control/concurrency/async.Async))
```

### cannot\_close

```clojure
(library/lux/control/exception.Exception .Any)
```

### cannot\_open

```clojure
(library/lux/control/exception.Exception .Any)
```

### default

```clojure
(library/lux/control/io.IO (library/lux/control/try.Try (Console library/lux/control/io.IO)))
```

### mock

```clojure
(All (_ a) (-> (Mock a) a (Console library/lux/control/io.IO)))
```

```clojure
(mock mock init)
```

### write\_line

```clojure
(All (_ a) (-> .Text (Console a) (a (library/lux/control/try.Try .Any))))
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
(primitive "#Text")
```

A path to a file or a directory in a file\-system\.

### \(System \!\)

```clojure
... .Type
[.Text (-> Path (! .Bit)) (-> Path (! .Bit)) (-> Path (! (library/lux/control/try.Try .Any))) (-> Path (! (library/lux/control/try.Try (.List Path)))) (-> Path (! (library/lux/control/try.Try (.List Path)))) (-> Path (! (library/lux/control/try.Try .Nat))) (-> Path (! (library/lux/control/try.Try library/lux/time/instant.Instant))) (-> Path (! (library/lux/control/try.Try .Bit))) (-> Path (! (library/lux/control/try.Try library/lux/data/binary.Binary))) (-> Path (! (library/lux/control/try.Try .Any))) (-> library/lux/time/instant.Instant Path (! (library/lux/control/try.Try .Any))) (-> library/lux/data/binary.Binary Path (! (library/lux/control/try.Try .Any))) (-> library/lux/data/binary.Binary Path (! (library/lux/control/try.Try .Any))) (-> Path Path (! (library/lux/control/try.Try .Any)))]
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

### cannot\_modify\_file

```clojure
(library/lux/control/exception.Exception [library/lux/time/instant.Instant Path])
```

### cannot\_read\_all\_data

```clojure
(library/lux/control/exception.Exception Path)
```

### default

```clojure
(System library/lux/control/io.IO)
```

### exists?

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (System a) Path (a .Bit)))
```

Checks if either a file or a directory exists at the given path\.

```clojure
(exists? monad fs path)
```

### make\_directories

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (System a) Path (a (library/lux/control/try.Try .Any))))
```

Creates the directory specified by the given path\.
Also, creates every super\-directory necessary to make the given path valid\.

```clojure
(make_directories monad fs path)
```

### make\_file

```clojure
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (System a) library/lux/data/binary.Binary Path (a (library/lux/control/try.Try .Any))))
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
(All (_ a) (-> (System a) Path .Text))
```

The un\-nested name of a file/directory\.

```clojure
(name fs path)
```

### parent

```clojure
(All (_ a) (-> (System a) Path (.Maybe Path)))
```

If a path represents a nested file/directory, extracts its parent directory\.

```clojure
(parent fs path)
```

### rooted

```clojure
(All (_ a) (-> (System a) Path .Text Path))
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
(primitive "library/lux/world/file/watch.Concern")
```

A particular concern to watch\-out for\.

### \(Watcher \!\)

```clojure
... .Type
[(-> Concern library/lux/world/file.Path (! (library/lux/control/try.Try .Any))) (-> library/lux/world/file.Path (! (library/lux/control/try.Try Concern))) (-> library/lux/world/file.Path (! (library/lux/control/try.Try Concern))) (-> .Any (! (library/lux/control/try.Try (.List [Concern library/lux/world/file.Path]))))]
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

### default

```clojure
(library/lux/control/io.IO (library/lux/control/try.Try (Watcher library/lux/control/concurrency/async.Async)))
```

The default watcher for the default file\-system\.

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
(primitive "#I64" (primitive "#Nat"))
```

A key from a keyboard, identify by a numeric ID\.

### Press

```clojure
... .Type
[.Bit Key]
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
(primitive "#Text")
```

A TCP/IP address\.

### Location

```clojure
... .Type
[Address Port]
```

### Port

```clojure
... .Type
(primitive "#I64" (primitive "#Nat"))
```

A TCP/IP port\.

### URL

```clojure
... .Type
(primitive "#Text")
```

A Uniform Resource Locator\.

___

# library/lux/world/net/http/client

## Definitions

### \(Client \!\)

```clojure
... .Type
(-> library/lux/world/net/http.Method library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (! (library/lux/control/try.Try (library/lux/world/net/http.Response !))))
```

A HTTP client capable of issuing requests to a HTTP server\.

### async

```clojure
(-> (Client library/lux/control/io.IO) (Client library/lux/control/concurrency/async.Async))
```

A ASYNC request\.

### connect

```clojure
(All (_ a) (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client a) (a (library/lux/control/try.Try (library/lux/world/net/http.Response a)))))
```

A CONNECT request\.

### default

```clojure
(Client library/lux/control/io.IO)
```

A DEFAULT request\.

### delete

```clojure
(All (_ a) (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client a) (a (library/lux/control/try.Try (library/lux/world/net/http.Response a)))))
```

A DELETE request\.

### get

```clojure
(All (_ a) (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client a) (a (library/lux/control/try.Try (library/lux/world/net/http.Response a)))))
```

A GET request\.

### head

```clojure
(All (_ a) (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client a) (a (library/lux/control/try.Try (library/lux/world/net/http.Response a)))))
```

A HEAD request\.

### headers

```clojure
(-> (.List [.Text .Text]) library/lux/world/net/http.Headers)
```

A HEADERS request\.

### options

```clojure
(All (_ a) (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client a) (a (library/lux/control/try.Try (library/lux/world/net/http.Response a)))))
```

A OPTIONS request\.

### patch

```clojure
(All (_ a) (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client a) (a (library/lux/control/try.Try (library/lux/world/net/http.Response a)))))
```

A PATCH request\.

### post

```clojure
(All (_ a) (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client a) (a (library/lux/control/try.Try (library/lux/world/net/http.Response a)))))
```

A POST request\.

### put

```clojure
(All (_ a) (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client a) (a (library/lux/control/try.Try (library/lux/world/net/http.Response a)))))
```

A PUT request\.

### trace

```clojure
(All (_ a) (-> library/lux/world/net.URL library/lux/world/net/http.Headers (.Maybe library/lux/data/binary.Binary) (Client a) (a (library/lux/control/try.Try (library/lux/world/net/http.Response a)))))
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
(primitive "#Text")
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
[.Nat .Nat]
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
[(-> .Any (! (.List .Text))) (-> .Text (! (library/lux/control/try.Try .Text))) library/lux/world/file.Path library/lux/world/file.Path (-> library/lux/world/shell.Exit (! .Nothing))]
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
(All (_ a) (-> (library/lux/abstract/monad.Monad a) (Program a) (a library/lux/control/parser/environment.Environment)))
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
(primitive "#Text")
```

A parameter for a command\.

### Command

```clojure
... .Type
(primitive "#Text")
```

A command that can be executed by the operating system\.

### Exit

```clojure
... .Type
(primitive "#I64" (primitive "#Int"))
```

A program exit code\.

### \(Mock s\)

```clojure
... .Type
[(-> s (library/lux/control/try.Try [s .Text])) (-> s (library/lux/control/try.Try [s .Text])) (-> .Text s (library/lux/control/try.Try s)) (-> s (library/lux/control/try.Try s)) (-> s (library/lux/control/try.Try [s Exit]))]
```

A simulated process\.

### \(Process \!\)

```clojure
... .Type
[(-> .Any (! (library/lux/control/try.Try .Text))) (-> .Any (! (library/lux/control/try.Try .Text))) (-> .Text (! (library/lux/control/try.Try .Any))) (-> .Any (! (library/lux/control/try.Try .Any))) (-> .Any (! (library/lux/control/try.Try Exit)))]
```

The means for communicating with a program/process being executed by the operating system\.

### \(Shell \!\)

```clojure
... .Type
(-> [library/lux/control/parser/environment.Environment library/lux/world/file.Path Command (.List Argument)] (! (library/lux/control/try.Try (Process !))))
```

The means for issuing commands to the operating system\.

### async

```clojure
(-> (Shell library/lux/control/io.IO) (Shell library/lux/control/concurrency/async.Async))
```

### default

```clojure
(Shell library/lux/control/io.IO)
```

### error

```clojure
Exit
```

### mock

```clojure
(All (_ a) (-> (-> [library/lux/control/parser/environment.Environment library/lux/world/file.Path Command (.List Argument)] (library/lux/control/try.Try (Mock a))) a (Shell library/lux/control/io.IO)))
```

```clojure
(mock mock init)
```

### no\_more\_output

```clojure
(library/lux/control/exception.Exception .Any)
```

### normal

```clojure
Exit
```


