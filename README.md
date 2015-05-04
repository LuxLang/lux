## What's available?

### Types
	(deftype Bool (^ java.lang.Boolean))

	(deftype Int (^ java.lang.Long))

	(deftype Real (^ java.lang.Double))

	(deftype Char (^ java.lang.Character))

	(deftype Text (^ java.lang.String))

	(deftype Void (|))

	(deftype Ident (, Text Text))

	(deftype (List a)
	  (| #Nil
		 (#Cons [a (List a)])))

	(deftype (Maybe a)
	  (| #None
		 (#Some a)))

	(deftype Type ...)

	(deftype (Meta m d)
	  (| (#Meta [m d])))

	(deftype Syntax ...)

	(deftype (Either l r)
	  (| (#Left l)
		 (#Right r)))

	(deftype Reader ...)

	(deftype LuxVar ...)

	(deftype CompilerState ...)

	(deftype (Lux a)
	  (-> CompilerState (Either Text (, CompilerState a))))

	(deftype Macro
	  (-> (List Syntax) (Lux (List Syntax))))

	(deftype (IO a)
	  (-> (,) a))

### Macros
defmacro
e.g.
	(defmacro #export (and tokens)
	  (case (reverse tokens)
		(\ (list& last init))
		(return (: (List Syntax)
		           (list (fold (: (-> Syntax Syntax Syntax)
		                          (lambda [post pre]
		                            (` (if (~ pre)
		                                 true
		                                 (~ post)))))
		                       last
		                       init))))
		
		_
		(fail "and requires >=1 clauses.")))

comment
e.g.
	(comment 1 2 3 4) ## Same as not writing anything...

list
e.g.
	(list 1 2 3)
	=> (#Cons [1 (#Cons [2 (#Cons [3 #Nil])])])

list&
e.g.
	(list& 0 (list 1 2 3))
	=> (#Cons [0 (list 1 2 3)])

lambda
e.g.
	(def const
	  (lambda [x y] x))

	(def const
	  (lambda const [x y] x))

let
e.g.
	(let [x (foo bar)
		  y (baz quux)]
	  ...)

$
e.g.
	($ text:++ "Hello, " name ".\nHow are you?")
	=> (text:++ "Hello, " (text:++ name ".\nHow are you?"))

|>
e.g.
	(|> elems (map ->text) (interpose " ") (fold text:++ ""))
	=>
	(fold text:++ ""
		  (interpose " "
		             (map ->text elems)))

if
e.g.
	(if true
	  "Oh, yeah"
	  "Aw hell naw!")

^
e.g.
	(^ java.lang.Object)

,
e.g.
	(, Text Int Bool)

|
e.g.
	(| #Yes #No)

&
e.g.
	(& #name Text
	   #age Int
	   #alive? Bool)

->
e.g.
	(-> Int Int Int)

All
e.g.
	(All List [a]
		 (| #Nil
		    (#Cons [a (List a)])))

	(All [a]
	  (-> a a))

type`
	## This macro is not mean to be used directly. It's used by :, :!, deftype, struct, sig

io
	## Just makes sure whatever computation you do returns an IO type. It's here mostly for host-interop.

:
e.g.
	(: (List Int) (list 1 2 3))

:!
e.g.
	(:! Dinosaur (list 1 2 3))

deftype
e.g.
	(deftype (List a)
	  (| #Nil
		 (#Cons [a (List a)])))

exec
e.g.
	(exec
	  (println "#1")
	  (println "#2")
	  (println "#3")
	  "YOLO")

def
e.g.
	(def (rejoin-pair pair)
	  (-> (, Syntax Syntax) (List Syntax))
	  (let [[left right] pair]
		(list left right)))

case
e.g.
	(case (: (List Int) (list 1 2 3))
	  (#Cons [x (#Cons [y (#Cons [z #Nil])])])
	  (#Some ($ int:* x y z))

	  _
	  #None)

	(case (: (List Int) (list 1 2 3))
	  (\ (list 1 2 3))
	  (#Some ($ int:* x y z))

	  _
	  #None)

	(deftype Weekday
	  (| #Monday
		 #Tuesday
		 #Wednesday
		 #Thursday
		 #Friday
		 #Saturday
		 #Sunday))

	(def (weekend? day)
	  (-> Weekday Bool)
	  (case day
		(\or #Saturday #Sunday)
		true

		_
		false))

\
	## It's a special macro meant to be used with case

\or
	## It's a special macro meant to be used with case

`
	## Quasi-quotation as a macro. Unquote (~) and unquote-splice (~@) must also be used as forms
	e.g.
	(` (def (~ name)
		 (lambda [(~@ args)]
		   (~ body))))

sig
	## Not mean to be used directly. Prefer defsig

struct
	## Not mean to be used directly. Prefer defstruct

defsig
e.g.
	(defsig #export (Ord a)
	  (: (-> a a Bool)
		 <)
	  (: (-> a a Bool)
		 <=)
	  (: (-> a a Bool)
		 >)
	  (: (-> a a Bool)
		 >=))

defstruct
e.g.
	(defstruct #export Int:Ord (Ord Int)
	  (def (< x y)
		(jvm-llt x y))
	  (def (<= x y)
		(or (jvm-llt x y)
		    (jvm-leq x y)))
	  (def (> x y)
		(jvm-lgt x y))
	  (def (>= x y)
		(or (jvm-lgt x y)
		    (jvm-leq x y))))

and
e.g.
	(and true false true) ## => false

or
e.g.
	(or true false true) ## => true

alias-lux
	## Just creates local aliases of everything defined & exported in lux.lux
	e.g.
	(;alias-lux)

using
e.g.
	(using Int:Ord
	  (< 5 10))

### Functions
fold
	(All [a b]
	  (-> (-> a b a) a (List b) a))

reverse
	(All [a]
	  (-> (List a) (List a)))

map
	(All [a b]
	  (-> (-> a b) (List a) (List b)))

any?
	(All [a]
	  (-> (-> a Bool) (List a) Bool))

.
	(All [a b c]
	  (-> (-> b c) (-> a b) (-> a c)))

int:+
	(-> Int Int Int)

int:-
	(-> Int Int Int)

int:*
	(-> Int Int Int)

int:/
	(-> Int Int Int)

int:%
	(-> Int Int Int)

int:=
	(-> Int Int Bool)

int:>
	(-> Int Int Bool)

int:<
	(-> Int Int Bool)

real:+
	(-> Real Real Real)

real:-
	(-> Real Real Real)

real:*
	(-> Real Real Real)

real:/
	(-> Real Real Real)

real:%
	(-> Real Real Real)

real:=
	(-> Real Real Bool)

real:>
	(-> Real Real Bool)

real:<
	(-> Real Real Bool)

length
	(All [a]
	  (-> (List a) Int))

not
	(-> Bool Bool)

text:++
	(-> Text Text Text)

get-module-name
	(Lux Text)

find-macro
	(-> Ident (Lux (Maybe Macro)))

normalize
	(-> Ident (Lux Ident))

->text
	(-> (^ java.lang.Object) Text)

interpose
	(All [a]
	  (-> a (List a) (List a)))

syntax:show
	(-> Syntax Text)

macro-expand
	(-> Syntax (Lux (List Syntax)))

gensym
	(-> Text (Lux Syntax))

macro-expand-1
	(-> Syntax (Lux Syntax))

id
	(All [a] (-> a a))

print
	(-> Text (,))

println
	(-> Text (,))

some
	(All [a b]
	  (-> (-> a (Maybe b)) (List a) (Maybe b)))

### Signatures
Eq
	(defsig #export (Eq a)
	  (: (-> a a Bool)
		 =))

Show
	(defsig #export (Show a)
	  (: (-> a Text)
		 show))

Ord
	(defsig #export (Ord a)
	  (: (-> a a Bool)
		 <)
	  (: (-> a a Bool)
		 <=)
	  (: (-> a a Bool)
		 >)
	  (: (-> a a Bool)
		 >=))

### Structures
Int:Eq
Real:Eq

Bool:Show
Int:Show
Real:Show
Char:Show

Int:Ord
Real:Ord

