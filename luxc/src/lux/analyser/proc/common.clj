(ns lux.analyser.proc.common
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case assert!]]
                 [type :as &type])
            (lux.analyser [base :as &&])))

(defn ^:private analyse-lux-is [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons left (&/$Cons right (&/$Nil))) ?values]
            =left (&&/analyse-1 analyse $var left)
            =right (&&/analyse-1 analyse $var right)
            _ (&type/check exo-type &type/Bool)
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["lux" "is"]) (&/|list =left =right) (&/|list)))))))))

(do-template [<name> <proc> <input-type> <output-type>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons x (&/$Cons y (&/$Nil))) ?values]
          =x (&&/analyse-1 analyse <input-type> x)
          =y (&&/analyse-1 analyse <input-type> y)
          _ (&type/check exo-type <output-type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T <proc>) (&/|list =x =y) (&/|list)))))))

  ^:private analyse-text-eq     ["text" "="]      &type/Text &type/Bool
  ^:private analyse-text-lt     ["text" "<"]      &type/Text &type/Bool
  ^:private analyse-text-append ["text" "append"] &type/Text &type/Text
  )

(do-template [<name> <proc-name> <output-type>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons text (&/$Cons part (&/$Cons start (&/$Nil)))) ?values]
          =text (&&/analyse-1 analyse &type/Text text)
          =part (&&/analyse-1 analyse &type/Text part)
          =start (&&/analyse-1 analyse &type/Nat start)
          _ (&type/check exo-type <output-type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["text" <proc-name>])
                                           (&/|list =text =part =start)
                                           (&/|list)))))))

  ^:private analyse-text-index      "index"      (&/$AppT &type/Maybe &type/Nat)
  ^:private analyse-text-last-index "last-index" (&/$AppT &type/Maybe &type/Nat)
  )

(defn ^:private analyse-text-contains? [analyse exo-type ?values]
  (|do [:let [(&/$Cons text (&/$Cons part (&/$Nil))) ?values]
        =text (&&/analyse-1 analyse &type/Text text)
        =part (&&/analyse-1 analyse &type/Text part)
        _ (&type/check exo-type &type/Bool)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["text" "contains?"])
                                         (&/|list =text =part)
                                         (&/|list)))))))

(defn ^:private analyse-text-clip [analyse exo-type ?values]
  (|do [:let [(&/$Cons text (&/$Cons from (&/$Cons to (&/$Nil)))) ?values]
        =text (&&/analyse-1 analyse &type/Text text)
        =from (&&/analyse-1 analyse &type/Nat from)
        =to (&&/analyse-1 analyse &type/Nat to)
        _ (&type/check exo-type (&/$AppT &type/Maybe &type/Text))
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["text" "clip"])
                                         (&/|list =text =from =to)
                                         (&/|list)))))))

(defn ^:private analyse-text-replace-all [analyse exo-type ?values]
  (|do [:let [(&/$Cons text (&/$Cons to-find (&/$Cons replace-with (&/$Nil)))) ?values]
        =text (&&/analyse-1 analyse &type/Text text)
        =to-find (&&/analyse-1 analyse &type/Text to-find)
        =replace-with (&&/analyse-1 analyse &type/Text replace-with)
        _ (&type/check exo-type &type/Text)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["text" "replace-all"])
                                         (&/|list =text =to-find =replace-with)
                                         (&/|list)))))))

(do-template [<name> <proc>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons text (&/$Nil)) ?values]
          =text (&&/analyse-1 analyse &type/Text text)
          _ (&type/check exo-type &type/Nat)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["text" <proc>])
                                           (&/|list =text)
                                           (&/|list)))))))

  ^:private analyse-text-size "size"
  ^:private analyse-text-hash "hash"
  )

(do-template [<name> <proc>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons text (&/$Nil)) ?values]
          =text (&&/analyse-1 analyse &type/Text text)
          _ (&type/check exo-type &type/Text)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["text" <proc>])
                                           (&/|list =text)
                                           (&/|list)))))))

  ^:private analyse-text-trim "trim"
  ^:private analyse-text-upper-case "upper-case"
  ^:private analyse-text-lower-case "lower-case"
  )

(defn ^:private analyse-text-char [analyse exo-type ?values]
  (|do [:let [(&/$Cons text (&/$Cons idx (&/$Nil))) ?values]
        =text (&&/analyse-1 analyse &type/Text text)
        =idx (&&/analyse-1 analyse &type/Nat idx)
        _ (&type/check exo-type (&/$AppT &type/Maybe &type/Char))
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["text" "char"])
                                         (&/|list =text =idx)
                                         (&/|list)))))))

(do-template [<name> <op>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons input (&/$Cons mask (&/$Nil))) ?values]
          =mask (&&/analyse-1 analyse &type/Nat mask)
          =input (&&/analyse-1 analyse &type/Nat input)
          _ (&type/check exo-type &type/Nat)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["bit" <op>]) (&/|list =input =mask) (&/|list)))))))

  ^:private analyse-bit-and "and"
  ^:private analyse-bit-or  "or"
  ^:private analyse-bit-xor "xor"
  )

(defn ^:private analyse-bit-count [analyse exo-type ?values]
  (|do [:let [(&/$Cons input (&/$Nil)) ?values]
        =input (&&/analyse-1 analyse &type/Nat input)
        _ (&type/check exo-type &type/Nat)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["bit" "count"]) (&/|list =input) (&/|list)))))))

(do-template [<name> <op> <type>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons input (&/$Cons shift (&/$Nil))) ?values]
          =shift (&&/analyse-1 analyse &type/Nat shift)
          =input (&&/analyse-1 analyse <type> input)
          _ (&type/check exo-type <type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["bit" <op>]) (&/|list =input =shift) (&/|list)))))))

  ^:private analyse-bit-shift-left           "shift-left"           &type/Nat
  ^:private analyse-bit-shift-right          "shift-right"          &type/Int
  ^:private analyse-bit-unsigned-shift-right "unsigned-shift-right" &type/Nat
  )

(do-template [<name> <proc> <input-type> <output-type>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons x (&/$Cons y (&/$Nil))) ?values]
          =x (&&/analyse-1 analyse <input-type> x)
          =y (&&/analyse-1 analyse <input-type> y)
          _ (&type/check exo-type <output-type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T <proc>) (&/|list =x =y) (&/|list)))))))

  ^:private analyse-nat-add  ["nat" "+"]  &type/Nat  &type/Nat
  ^:private analyse-nat-sub  ["nat" "-"]  &type/Nat  &type/Nat
  ^:private analyse-nat-mul  ["nat" "*"]  &type/Nat  &type/Nat
  ^:private analyse-nat-div  ["nat" "/"]  &type/Nat  &type/Nat
  ^:private analyse-nat-rem  ["nat" "%"]  &type/Nat  &type/Nat
  ^:private analyse-nat-eq   ["nat" "="]  &type/Nat  &type/Bool
  ^:private analyse-nat-lt   ["nat" "<"]  &type/Nat  &type/Bool

  ^:private analyse-int-add  ["int" "+"]  &type/Int  &type/Int
  ^:private analyse-int-sub  ["int" "-"]  &type/Int  &type/Int
  ^:private analyse-int-mul  ["int" "*"]  &type/Int  &type/Int
  ^:private analyse-int-div  ["int" "/"]  &type/Int  &type/Int
  ^:private analyse-int-rem  ["int" "%"]  &type/Int  &type/Int
  ^:private analyse-int-eq   ["int" "="]  &type/Int  &type/Bool
  ^:private analyse-int-lt   ["int" "<"]  &type/Int  &type/Bool

  ^:private analyse-deg-add  ["deg" "+"]  &type/Deg  &type/Deg
  ^:private analyse-deg-sub  ["deg" "-"]  &type/Deg  &type/Deg
  ^:private analyse-deg-mul  ["deg" "*"]  &type/Deg  &type/Deg
  ^:private analyse-deg-div  ["deg" "/"]  &type/Deg  &type/Deg
  ^:private analyse-deg-rem  ["deg" "%"]  &type/Deg  &type/Deg
  ^:private analyse-deg-eq   ["deg" "="]  &type/Deg  &type/Bool
  ^:private analyse-deg-lt   ["deg" "<"]  &type/Deg  &type/Bool

  ^:private analyse-real-add ["real" "+"] &type/Real &type/Real
  ^:private analyse-real-sub ["real" "-"] &type/Real &type/Real
  ^:private analyse-real-mul ["real" "*"] &type/Real &type/Real
  ^:private analyse-real-div ["real" "/"] &type/Real &type/Real
  ^:private analyse-real-rem ["real" "%"] &type/Real &type/Real
  ^:private analyse-real-eq  ["real" "="] &type/Real &type/Bool
  ^:private analyse-real-lt  ["real" "<"] &type/Real &type/Bool

  ^:private analyse-char-eq  ["char" "="] &type/Char &type/Bool
  ^:private analyse-char-lt  ["char" "<"] &type/Char &type/Bool
  )

(defn ^:private analyse-deg-scale [analyse exo-type ?values]
  (|do [:let [(&/$Cons x (&/$Cons y (&/$Nil))) ?values]
        =x (&&/analyse-1 analyse &type/Deg x)
        =y (&&/analyse-1 analyse &type/Nat y)
        _ (&type/check exo-type &type/Deg)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["deg" "scale"]) (&/|list =x =y) (&/|list)))))))

(do-template [<encode> <encode-op> <decode> <decode-op> <type>]
  (do (defn <encode> [analyse exo-type ?values]
        (|do [:let [(&/$Cons x (&/$Nil)) ?values]
              =x (&&/analyse-1 analyse <type> x)
              _ (&type/check exo-type &type/Text)
              _cursor &/cursor]
          (return (&/|list (&&/|meta exo-type _cursor
                                     (&&/$proc (&/T <encode-op>) (&/|list =x) (&/|list)))))))

    (let [decode-type (&/$AppT &type/Maybe <type>)]
      (defn <decode> [analyse exo-type ?values]
        (|do [:let [(&/$Cons x (&/$Nil)) ?values]
              =x (&&/analyse-1 analyse &type/Text x)
              _ (&type/check exo-type decode-type)
              _cursor &/cursor]
          (return (&/|list (&&/|meta exo-type _cursor
                                     (&&/$proc (&/T <decode-op>) (&/|list =x) (&/|list)))))))))

  ^:private analyse-nat-encode ["nat" "encode"] ^:private analyse-nat-decode ["nat" "decode"] &type/Nat
  ^:private analyse-int-encode ["int" "encode"] ^:private analyse-int-decode ["int" "decode"] &type/Int
  ^:private analyse-deg-encode ["deg" "encode"] ^:private analyse-deg-decode ["deg" "decode"] &type/Deg
  ^:private analyse-real-encode ["real" "encode"] ^:private analyse-real-decode ["real" "decode"] &type/Real
  )

(do-template [<name> <type> <op>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Nil) ?values]
          _ (&type/check exo-type <type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T <op>) (&/|list) (&/|list)))))))

  ^:private analyse-nat-min-value            &type/Nat  ["nat"  "min-value"]
  ^:private analyse-nat-max-value            &type/Nat  ["nat"  "max-value"]

  ^:private analyse-int-min-value            &type/Int  ["int"  "min-value"]
  ^:private analyse-int-max-value            &type/Int  ["int"  "max-value"]

  ^:private analyse-deg-min-value           &type/Deg ["deg" "min-value"]
  ^:private analyse-deg-max-value           &type/Deg ["deg" "max-value"]

  ^:private analyse-real-min-value          &type/Real  ["real"  "min-value"]
  ^:private analyse-real-max-value          &type/Real  ["real"  "max-value"]
  ^:private analyse-real-not-a-number       &type/Real  ["real"  "not-a-number"]
  ^:private analyse-real-positive-infinity  &type/Real  ["real"  "positive-infinity"]
  ^:private analyse-real-negative-infinity  &type/Real  ["real"  "negative-infinity"]
  )

(do-template [<name> <from-type> <to-type> <op>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons x (&/$Nil)) ?values]
          =x (&&/analyse-1 analyse <from-type> x)
          _ (&type/check exo-type <to-type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T <op>) (&/|list =x) (&/|list)))))))

  ^:private analyse-nat-to-int   &type/Nat  &type/Int    ["nat" "to-int"]
  ^:private analyse-int-to-nat   &type/Int  &type/Nat    ["int" "to-nat"]
  
  ^:private analyse-nat-to-char  &type/Nat  &type/Char   ["nat" "to-char"]
  ^:private analyse-char-to-nat  &type/Char &type/Nat    ["char" "to-nat"]
  
  ^:private analyse-int-to-real  &type/Int  &type/Real   ["int" "to-real"]
  ^:private analyse-real-to-int  &type/Real &type/Int    ["real" "to-int"]
  ^:private analyse-real-hash    &type/Real &type/Nat    ["real" "hash"]
  
  ^:private analyse-char-to-text &type/Char &type/Text   ["char" "to-text"]

  ^:private analyse-deg-to-real  &type/Deg  &type/Real   ["deg" "to-real"]
  ^:private analyse-real-to-deg  &type/Real &type/Deg    ["real" "to-deg"]
  
  ^:private analyse-io-log       &type/Text &/$UnitT     ["io" "log"]
  ^:private analyse-io-error     &type/Text &type/Bottom ["io" "error"]
  ^:private analyse-io-exit      &type/Int  &type/Bottom ["io" "exit"]
  )

(defn ^:private analyse-io-current-time [analyse exo-type ?values]
  (|do [:let [(&/$Nil) ?values]
        _ (&type/check exo-type &type/Int)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["io" "current-time"]) (&/|list) (&/|list)))))))

(defn ^:private analyse-array-new [analyse exo-type ?values]
  (|do [:let [(&/$Cons length (&/$Nil)) ?values]
        =length (&&/analyse-1 analyse &type/Nat length)
        _ (&type/check exo-type (&/$UnivQ (&/|list) (&type/Array (&/$BoundT 1))))
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["array" "new"]) (&/|list =length) (&/|list)))))))

(defn ^:private analyse-array-get [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons array (&/$Cons idx (&/$Nil))) ?values]
            =array (&&/analyse-1 analyse (&type/Array $var) array)
            =idx (&&/analyse-1 analyse &type/Nat idx)
            _ (&type/check exo-type (&/$AppT &type/Maybe $var))
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["array" "get"]) (&/|list =array =idx) (&/|list)))))))))

(defn ^:private analyse-array-put [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons array (&/$Cons idx (&/$Cons elem (&/$Nil)))) ?values]
            :let [array-type (&type/Array $var)]
            =array (&&/analyse-1 analyse array-type array)
            =idx (&&/analyse-1 analyse &type/Nat idx)
            =elem (&&/analyse-1 analyse $var elem)
            _ (&type/check exo-type array-type)
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["array" "put"]) (&/|list =array =idx =elem) (&/|list)))))))))

(defn ^:private analyse-array-remove [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons array (&/$Cons idx (&/$Nil))) ?values]
            :let [array-type (&type/Array $var)]
            =array (&&/analyse-1 analyse array-type array)
            =idx (&&/analyse-1 analyse &type/Nat idx)
            _ (&type/check exo-type array-type)
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["array" "remove"]) (&/|list =array =idx) (&/|list)))))))))

(defn ^:private analyse-array-size [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons array (&/$Nil)) ?values]
            =array (&&/analyse-1 analyse (&type/Array $var) array)
            _ (&type/check exo-type &type/Nat)
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["array" "size"]) (&/|list =array) (&/|list)))))))))

(do-template [<name> <proc>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Nil) ?values]
          _ (&type/check exo-type &type/Real)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["math" <proc>]) (&/|list) (&/|list)))))))

  ^:private analyse-math-e  "e"
  ^:private analyse-math-pi "pi"
  )

(do-template [<name> <proc>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
          =input (&&/analyse-1 analyse &type/Real ?input)
          _ (&type/check exo-type &type/Real)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["math" <proc>]) (&/|list =input) (&/|list)))))))

  ^:private analyse-math-cos "cos"
  ^:private analyse-math-sin "sin"
  ^:private analyse-math-tan "tan"
  ^:private analyse-math-acos "acos"
  ^:private analyse-math-asin "asin"
  ^:private analyse-math-atan "atan"
  ^:private analyse-math-cosh "cosh"
  ^:private analyse-math-sinh "sinh"
  ^:private analyse-math-tanh "tanh"
  ^:private analyse-math-exp "exp"
  ^:private analyse-math-log "log"
  ^:private analyse-math-root2 "root2"
  ^:private analyse-math-root3 "root3"
  ^:private analyse-math-ceil "ceil"
  ^:private analyse-math-floor "floor"
  ^:private analyse-math-round "round"
  )

(do-template [<name> <proc>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons ?input (&/$Cons ?param (&/$Nil))) ?values]
          =input (&&/analyse-1 analyse &type/Real ?input)
          =param (&&/analyse-1 analyse &type/Real ?param)
          _ (&type/check exo-type &type/Real)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["math" <proc>]) (&/|list =input =param) (&/|list)))))))

  ^:private analyse-math-atan2 "atan2"
  ^:private analyse-math-pow "pow"
  )

(defn ^:private analyse-atom-new [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons ?init (&/$Nil)) ?values]
            =init (&&/analyse-1 analyse $var ?init)
            _ (&type/check exo-type (&type/Atom $var))
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["atom" "new"]) (&/|list =init) (&/|list)))))))))

(defn ^:private analyse-atom-get [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons ?atom (&/$Nil)) ?values]
            =atom (&&/analyse-1 analyse (&type/Atom $var) ?atom)
            _ (&type/check exo-type $var)
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["atom" "get"]) (&/|list =atom) (&/|list)))))))))

(defn ^:private analyse-atom-compare-and-swap [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons ?atom (&/$Cons ?old (&/$Cons ?new (&/$Nil)))) ?values]
            =atom (&&/analyse-1 analyse (&type/Atom $var) ?atom)
            =old (&&/analyse-1 analyse $var ?old)
            =new (&&/analyse-1 analyse $var ?new)
            _ (&type/check exo-type &type/Bool)
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["atom" "compare-and-swap"]) (&/|list =atom =old =new) (&/|list)))))))))

(defn ^:private analyse-process-concurrency-level [analyse exo-type ?values]
  (|do [:let [(&/$Nil) ?values]
        _ (&type/check exo-type &type/Nat)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["process" "concurrency-level"]) (&/|list) (&/|list)))))))

(defn ^:private analyse-process-future [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?procedure (&/$Nil)) ?values]
        =procedure (&&/analyse-1 analyse (&/$AppT &type/IO &type/Top) ?procedure)
        _ (&type/check exo-type &/$UnitT)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["process" "future"]) (&/|list =procedure) (&/|list)))))))

(defn ^:private analyse-process-schedule [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?milliseconds (&/$Cons ?procedure (&/$Nil))) ?values]
        =milliseconds (&&/analyse-1 analyse &type/Nat ?milliseconds)
        =procedure (&&/analyse-1 analyse (&/$AppT &type/IO &type/Top) ?procedure)
        _ (&type/check exo-type &/$UnitT)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["process" "schedule"]) (&/|list =milliseconds =procedure) (&/|list)))))))

(defn analyse-proc [analyse exo-type category proc ?values]
  (case category
    "lux"
    (case proc
      "is"                   (analyse-lux-is analyse exo-type ?values))

    "io"
    (case proc
      "log"                  (analyse-io-log analyse exo-type ?values)
      "error"                (analyse-io-error analyse exo-type ?values)
      "exit"                 (analyse-io-exit analyse exo-type ?values)
      "current-time"         (analyse-io-current-time analyse exo-type ?values)
      )

    "text"
    (case proc
      "="                    (analyse-text-eq analyse exo-type ?values)
      "<"                    (analyse-text-lt analyse exo-type ?values)
      "append"               (analyse-text-append analyse exo-type ?values)
      "clip"                 (analyse-text-clip analyse exo-type ?values)
      "index"                (analyse-text-index analyse exo-type ?values)
      "last-index"           (analyse-text-last-index analyse exo-type ?values)
      "size"                 (analyse-text-size analyse exo-type ?values)
      "hash"                 (analyse-text-hash analyse exo-type ?values)
      "replace-all"          (analyse-text-replace-all analyse exo-type ?values)
      "trim"                 (analyse-text-trim analyse exo-type ?values)
      "char"                 (analyse-text-char analyse exo-type ?values)
      "upper-case"           (analyse-text-upper-case analyse exo-type ?values)
      "lower-case"           (analyse-text-lower-case analyse exo-type ?values)
      "contains?"            (analyse-text-contains? analyse exo-type ?values)
      )

    "bit"
    (case proc
      "count"                (analyse-bit-count analyse exo-type ?values)
      "and"                  (analyse-bit-and analyse exo-type ?values)
      "or"                   (analyse-bit-or analyse exo-type ?values)
      "xor"                  (analyse-bit-xor analyse exo-type ?values)
      "shift-left"           (analyse-bit-shift-left analyse exo-type ?values)
      "shift-right"          (analyse-bit-shift-right analyse exo-type ?values)
      "unsigned-shift-right" (analyse-bit-unsigned-shift-right analyse exo-type ?values))
    
    "array"
    (case proc
      "new"    (analyse-array-new analyse exo-type ?values)
      "get"    (analyse-array-get analyse exo-type ?values)
      "put"    (analyse-array-put analyse exo-type ?values)
      "remove" (analyse-array-remove analyse exo-type ?values)
      "size"   (analyse-array-size analyse exo-type ?values))

    "nat"
    (case proc
      "+" (analyse-nat-add analyse exo-type ?values)
      "-" (analyse-nat-sub analyse exo-type ?values)
      "*" (analyse-nat-mul analyse exo-type ?values)
      "/" (analyse-nat-div analyse exo-type ?values)
      "%" (analyse-nat-rem analyse exo-type ?values)
      "=" (analyse-nat-eq analyse exo-type ?values)
      "<" (analyse-nat-lt analyse exo-type ?values)
      "encode" (analyse-nat-encode analyse exo-type ?values)
      "decode" (analyse-nat-decode analyse exo-type ?values)
      "min-value" (analyse-nat-min-value analyse exo-type ?values)
      "max-value" (analyse-nat-max-value analyse exo-type ?values)
      "to-int" (analyse-nat-to-int analyse exo-type ?values)
      "to-char" (analyse-nat-to-char analyse exo-type ?values)
      )

    "int"
    (case proc
      "+" (analyse-int-add analyse exo-type ?values)
      "-" (analyse-int-sub analyse exo-type ?values)
      "*" (analyse-int-mul analyse exo-type ?values)
      "/" (analyse-int-div analyse exo-type ?values)
      "%" (analyse-int-rem analyse exo-type ?values)
      "=" (analyse-int-eq analyse exo-type ?values)
      "<" (analyse-int-lt analyse exo-type ?values)
      "encode" (analyse-int-encode analyse exo-type ?values)
      "decode" (analyse-int-decode analyse exo-type ?values)
      "min-value" (analyse-int-min-value analyse exo-type ?values)
      "max-value" (analyse-int-max-value analyse exo-type ?values)
      "to-nat" (analyse-int-to-nat analyse exo-type ?values)
      "to-real" (analyse-int-to-real analyse exo-type ?values)
      )

    "deg"
    (case proc
      "+" (analyse-deg-add analyse exo-type ?values)
      "-" (analyse-deg-sub analyse exo-type ?values)
      "*" (analyse-deg-mul analyse exo-type ?values)
      "/" (analyse-deg-div analyse exo-type ?values)
      "%" (analyse-deg-rem analyse exo-type ?values)
      "=" (analyse-deg-eq analyse exo-type ?values)
      "<" (analyse-deg-lt analyse exo-type ?values)
      "encode" (analyse-deg-encode analyse exo-type ?values)
      "decode" (analyse-deg-decode analyse exo-type ?values)
      "min-value" (analyse-deg-min-value analyse exo-type ?values)
      "max-value" (analyse-deg-max-value analyse exo-type ?values)
      "to-real" (analyse-deg-to-real analyse exo-type ?values)
      "scale" (analyse-deg-scale analyse exo-type ?values)
      )

    "real"
    (case proc
      "+" (analyse-real-add analyse exo-type ?values)
      "-" (analyse-real-sub analyse exo-type ?values)
      "*" (analyse-real-mul analyse exo-type ?values)
      "/" (analyse-real-div analyse exo-type ?values)
      "%" (analyse-real-rem analyse exo-type ?values)
      "=" (analyse-real-eq analyse exo-type ?values)
      "<" (analyse-real-lt analyse exo-type ?values)
      "encode" (analyse-real-encode analyse exo-type ?values)
      "decode" (analyse-real-decode analyse exo-type ?values)
      "min-value" (analyse-real-min-value analyse exo-type ?values)
      "max-value" (analyse-real-max-value analyse exo-type ?values)
      "not-a-number" (analyse-real-not-a-number analyse exo-type ?values)
      "positive-infinity" (analyse-real-positive-infinity analyse exo-type ?values)
      "negative-infinity" (analyse-real-negative-infinity analyse exo-type ?values)
      "to-deg" (analyse-real-to-deg analyse exo-type ?values)
      "to-int" (analyse-real-to-int analyse exo-type ?values)
      "hash" (analyse-real-hash analyse exo-type ?values)
      )

    "char"
    (case proc
      "=" (analyse-char-eq analyse exo-type ?values)
      "<" (analyse-char-lt analyse exo-type ?values)
      "to-text" (analyse-char-to-text analyse exo-type ?values)
      "to-nat" (analyse-char-to-nat analyse exo-type ?values)
      )

    "math"
    (case proc
      "e" (analyse-math-e analyse exo-type ?values)
      "pi" (analyse-math-pi analyse exo-type ?values)
      "cos" (analyse-math-cos analyse exo-type ?values)
      "sin" (analyse-math-sin analyse exo-type ?values)
      "tan" (analyse-math-tan analyse exo-type ?values)
      "acos" (analyse-math-acos analyse exo-type ?values)
      "asin" (analyse-math-asin analyse exo-type ?values)
      "atan" (analyse-math-atan analyse exo-type ?values)
      "cosh" (analyse-math-cosh analyse exo-type ?values)
      "sinh" (analyse-math-sinh analyse exo-type ?values)
      "tanh" (analyse-math-tanh analyse exo-type ?values)
      "exp" (analyse-math-exp analyse exo-type ?values)
      "log" (analyse-math-log analyse exo-type ?values)
      "root2" (analyse-math-root2 analyse exo-type ?values)
      "root3" (analyse-math-root3 analyse exo-type ?values)
      "ceil" (analyse-math-ceil analyse exo-type ?values)
      "floor" (analyse-math-floor analyse exo-type ?values)
      "round" (analyse-math-round analyse exo-type ?values)
      "atan2" (analyse-math-atan2 analyse exo-type ?values)
      "pow" (analyse-math-pow analyse exo-type ?values)
      )

    "atom"
    (case proc
      "new" (analyse-atom-new analyse exo-type ?values)
      "get" (analyse-atom-get analyse exo-type ?values)
      "compare-and-swap" (analyse-atom-compare-and-swap analyse exo-type ?values)
      )

    "process"
    (case proc
      "concurrency-level" (analyse-process-concurrency-level analyse exo-type ?values)
      "future" (analyse-process-future analyse exo-type ?values)
      "schedule" (analyse-process-schedule analyse exo-type ?values)
      )
    
    ;; else
    (&/fail-with-loc (str "[Analyser Error] Unknown host procedure: " [category proc]))))
