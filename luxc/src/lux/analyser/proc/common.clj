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

(defn ^:private analyse-lux-try [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons op (&/$Nil)) ?values]
            =op (&&/analyse-1 analyse (&/$Apply $var &type/IO) op)
            _ (&type/check exo-type (&/$Sum &type/Text ;; lux;Left
                                            $var ;; lux;Right
                                            ))
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["lux" "try"]) (&/|list =op) (&/|list)))))))))

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
  ^:private analyse-text-concat ["text" "concat"] &type/Text &type/Text
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

  ^:private analyse-text-index      "index"      (&/$Apply &type/Nat &type/Maybe)
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
        _ (&type/check exo-type (&/$Apply &type/Text &type/Maybe))
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

  ^:private analyse-text-upper-case "upper-case"
  ^:private analyse-text-lower-case "lower-case"
  )

(defn ^:private analyse-text-char [analyse exo-type ?values]
  (|do [:let [(&/$Cons text (&/$Cons idx (&/$Nil))) ?values]
        =text (&&/analyse-1 analyse &type/Text text)
        =idx (&&/analyse-1 analyse &type/Nat idx)
        _ (&type/check exo-type (&/$Apply &type/Nat &type/Maybe))
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

  ^:private analyse-frac-add ["frac" "+"] &type/Frac &type/Frac
  ^:private analyse-frac-sub ["frac" "-"] &type/Frac &type/Frac
  ^:private analyse-frac-mul ["frac" "*"] &type/Frac &type/Frac
  ^:private analyse-frac-div ["frac" "/"] &type/Frac &type/Frac
  ^:private analyse-frac-rem ["frac" "%"] &type/Frac &type/Frac
  ^:private analyse-frac-eq  ["frac" "="] &type/Frac &type/Bool
  ^:private analyse-frac-lt  ["frac" "<"] &type/Frac &type/Bool
  )

(do-template [<name> <proc>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons x (&/$Cons y (&/$Nil))) ?values]
          =x (&&/analyse-1 analyse &type/Deg x)
          =y (&&/analyse-1 analyse &type/Nat y)
          _ (&type/check exo-type &type/Deg)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T <proc>) (&/|list =x =y) (&/|list)))))))

  ^:private analyse-deg-scale      ["deg" "scale"]
  ^:private analyse-deg-reciprocal ["deg" "reciprocal"]
  )

(do-template [<encode> <encode-op> <decode> <decode-op> <type>]
  (do (defn <encode> [analyse exo-type ?values]
        (|do [:let [(&/$Cons x (&/$Nil)) ?values]
              =x (&&/analyse-1 analyse <type> x)
              _ (&type/check exo-type &type/Text)
              _cursor &/cursor]
          (return (&/|list (&&/|meta exo-type _cursor
                                     (&&/$proc (&/T <encode-op>) (&/|list =x) (&/|list)))))))

    (let [decode-type (&/$Apply <type> &type/Maybe)]
      (defn <decode> [analyse exo-type ?values]
        (|do [:let [(&/$Cons x (&/$Nil)) ?values]
              =x (&&/analyse-1 analyse &type/Text x)
              _ (&type/check exo-type decode-type)
              _cursor &/cursor]
          (return (&/|list (&&/|meta exo-type _cursor
                                     (&&/$proc (&/T <decode-op>) (&/|list =x) (&/|list)))))))))

  ^:private analyse-frac-encode ["frac" "encode"] ^:private analyse-frac-decode ["frac" "decode"] &type/Frac
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

  ^:private analyse-frac-smallest-value     &type/Frac  ["frac"  "smallest-value"]
  ^:private analyse-frac-min-value          &type/Frac  ["frac"  "min-value"]
  ^:private analyse-frac-max-value          &type/Frac  ["frac"  "max-value"]
  ^:private analyse-frac-not-a-number       &type/Frac  ["frac"  "not-a-number"]
  ^:private analyse-frac-positive-infinity  &type/Frac  ["frac"  "positive-infinity"]
  ^:private analyse-frac-negative-infinity  &type/Frac  ["frac"  "negative-infinity"]
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
  
  ^:private analyse-nat-to-char  &type/Nat  &type/Text   ["nat" "to-char"]
  
  ^:private analyse-int-to-frac  &type/Int  &type/Frac   ["int" "to-frac"]
  ^:private analyse-frac-to-int  &type/Frac &type/Int    ["frac" "to-int"]

  ^:private analyse-deg-to-frac  &type/Deg  &type/Frac   ["deg" "to-frac"]
  ^:private analyse-frac-to-deg  &type/Frac &type/Deg    ["frac" "to-deg"]
  
  ^:private analyse-io-log       &type/Text &/$Unit     ["io" "log"]
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
        _ (&type/check exo-type (&/$UnivQ (&/|list) (&type/Array (&/$Bound 1))))
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["array" "new"]) (&/|list =length) (&/|list)))))))

(defn ^:private analyse-array-get [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons array (&/$Cons idx (&/$Nil))) ?values]
            =array (&&/analyse-1 analyse (&type/Array $var) array)
            =idx (&&/analyse-1 analyse &type/Nat idx)
            _ (&type/check exo-type (&/$Apply $var &type/Maybe))
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
          _ (&type/check exo-type &type/Frac)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["math" <proc>]) (&/|list) (&/|list)))))))

  ^:private analyse-math-e  "e"
  ^:private analyse-math-pi "pi"
  )

(do-template [<name> <proc>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
          =input (&&/analyse-1 analyse &type/Frac ?input)
          _ (&type/check exo-type &type/Frac)
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
          =input (&&/analyse-1 analyse &type/Frac ?input)
          =param (&&/analyse-1 analyse &type/Frac ?param)
          _ (&type/check exo-type &type/Frac)
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
        =procedure (&&/analyse-1 analyse (&/$Apply &type/Top &type/IO) ?procedure)
        _ (&type/check exo-type &/$Unit)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["process" "future"]) (&/|list =procedure) (&/|list)))))))

(defn ^:private analyse-process-schedule [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?milliseconds (&/$Cons ?procedure (&/$Nil))) ?values]
        =milliseconds (&&/analyse-1 analyse &type/Nat ?milliseconds)
        =procedure (&&/analyse-1 analyse (&/$Apply &type/Top &type/IO) ?procedure)
        _ (&type/check exo-type &/$Unit)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["process" "schedule"]) (&/|list =milliseconds =procedure) (&/|list)))))))

(defn analyse-proc [analyse exo-type proc ?values]
  (try (case proc
         "lux is"                   (analyse-lux-is analyse exo-type ?values)
         "lux try"                  (analyse-lux-try analyse exo-type ?values)

         "lux io log"                  (analyse-io-log analyse exo-type ?values)
         "lux io error"                (analyse-io-error analyse exo-type ?values)
         "lux io exit"                 (analyse-io-exit analyse exo-type ?values)
         "lux io current-time"         (analyse-io-current-time analyse exo-type ?values)
         
         "lux text ="                    (analyse-text-eq analyse exo-type ?values)
         "lux text <"                    (analyse-text-lt analyse exo-type ?values)
         "lux text concat"               (analyse-text-concat analyse exo-type ?values)
         "lux text clip"                 (analyse-text-clip analyse exo-type ?values)
         "lux text index"                (analyse-text-index analyse exo-type ?values)
         "lux text size"                 (analyse-text-size analyse exo-type ?values)
         "lux text hash"                 (analyse-text-hash analyse exo-type ?values)
         "lux text replace-all"          (analyse-text-replace-all analyse exo-type ?values)
         "lux text char"                 (analyse-text-char analyse exo-type ?values)
         "lux text upper-case"           (analyse-text-upper-case analyse exo-type ?values)
         "lux text lower-case"           (analyse-text-lower-case analyse exo-type ?values)
         "lux text contains?"            (analyse-text-contains? analyse exo-type ?values)
         
         "lux bit count"                (analyse-bit-count analyse exo-type ?values)
         "lux bit and"                  (analyse-bit-and analyse exo-type ?values)
         "lux bit or"                   (analyse-bit-or analyse exo-type ?values)
         "lux bit xor"                  (analyse-bit-xor analyse exo-type ?values)
         "lux bit shift-left"           (analyse-bit-shift-left analyse exo-type ?values)
         "lux bit shift-right"          (analyse-bit-shift-right analyse exo-type ?values)
         "lux bit unsigned-shift-right" (analyse-bit-unsigned-shift-right analyse exo-type ?values)
         
         "lux array new"    (analyse-array-new analyse exo-type ?values)
         "lux array get"    (analyse-array-get analyse exo-type ?values)
         "lux array put"    (analyse-array-put analyse exo-type ?values)
         "lux array remove" (analyse-array-remove analyse exo-type ?values)
         "lux array size"   (analyse-array-size analyse exo-type ?values)

         "lux nat +" (analyse-nat-add analyse exo-type ?values)
         "lux nat -" (analyse-nat-sub analyse exo-type ?values)
         "lux nat *" (analyse-nat-mul analyse exo-type ?values)
         "lux nat /" (analyse-nat-div analyse exo-type ?values)
         "lux nat %" (analyse-nat-rem analyse exo-type ?values)
         "lux nat =" (analyse-nat-eq analyse exo-type ?values)
         "lux nat <" (analyse-nat-lt analyse exo-type ?values)
         "lux nat min-value" (analyse-nat-min-value analyse exo-type ?values)
         "lux nat max-value" (analyse-nat-max-value analyse exo-type ?values)
         "lux nat to-int" (analyse-nat-to-int analyse exo-type ?values)
         "lux nat to-char" (analyse-nat-to-char analyse exo-type ?values)
         
         "lux int +" (analyse-int-add analyse exo-type ?values)
         "lux int -" (analyse-int-sub analyse exo-type ?values)
         "lux int *" (analyse-int-mul analyse exo-type ?values)
         "lux int /" (analyse-int-div analyse exo-type ?values)
         "lux int %" (analyse-int-rem analyse exo-type ?values)
         "lux int =" (analyse-int-eq analyse exo-type ?values)
         "lux int <" (analyse-int-lt analyse exo-type ?values)
         "lux int min-value" (analyse-int-min-value analyse exo-type ?values)
         "lux int max-value" (analyse-int-max-value analyse exo-type ?values)
         "lux int to-nat" (analyse-int-to-nat analyse exo-type ?values)
         "lux int to-frac" (analyse-int-to-frac analyse exo-type ?values)
         
         "lux deg +" (analyse-deg-add analyse exo-type ?values)
         "lux deg -" (analyse-deg-sub analyse exo-type ?values)
         "lux deg *" (analyse-deg-mul analyse exo-type ?values)
         "lux deg /" (analyse-deg-div analyse exo-type ?values)
         "lux deg %" (analyse-deg-rem analyse exo-type ?values)
         "lux deg =" (analyse-deg-eq analyse exo-type ?values)
         "lux deg <" (analyse-deg-lt analyse exo-type ?values)
         "lux deg min-value" (analyse-deg-min-value analyse exo-type ?values)
         "lux deg max-value" (analyse-deg-max-value analyse exo-type ?values)
         "lux deg to-frac" (analyse-deg-to-frac analyse exo-type ?values)
         "lux deg scale" (analyse-deg-scale analyse exo-type ?values)
         "lux deg reciprocal" (analyse-deg-reciprocal analyse exo-type ?values)
         
         "lux frac +" (analyse-frac-add analyse exo-type ?values)
         "lux frac -" (analyse-frac-sub analyse exo-type ?values)
         "lux frac *" (analyse-frac-mul analyse exo-type ?values)
         "lux frac /" (analyse-frac-div analyse exo-type ?values)
         "lux frac %" (analyse-frac-rem analyse exo-type ?values)
         "lux frac =" (analyse-frac-eq analyse exo-type ?values)
         "lux frac <" (analyse-frac-lt analyse exo-type ?values)
         "lux frac encode" (analyse-frac-encode analyse exo-type ?values)
         "lux frac decode" (analyse-frac-decode analyse exo-type ?values)
         "lux frac smallest-value" (analyse-frac-smallest-value analyse exo-type ?values)
         "lux frac min-value" (analyse-frac-min-value analyse exo-type ?values)
         "lux frac max-value" (analyse-frac-max-value analyse exo-type ?values)
         "lux frac not-a-number" (analyse-frac-not-a-number analyse exo-type ?values)
         "lux frac positive-infinity" (analyse-frac-positive-infinity analyse exo-type ?values)
         "lux frac negative-infinity" (analyse-frac-negative-infinity analyse exo-type ?values)
         "lux frac to-deg" (analyse-frac-to-deg analyse exo-type ?values)
         "lux frac to-int" (analyse-frac-to-int analyse exo-type ?values)
         
         "lux math e" (analyse-math-e analyse exo-type ?values)
         "lux math pi" (analyse-math-pi analyse exo-type ?values)
         "lux math cos" (analyse-math-cos analyse exo-type ?values)
         "lux math sin" (analyse-math-sin analyse exo-type ?values)
         "lux math tan" (analyse-math-tan analyse exo-type ?values)
         "lux math acos" (analyse-math-acos analyse exo-type ?values)
         "lux math asin" (analyse-math-asin analyse exo-type ?values)
         "lux math atan" (analyse-math-atan analyse exo-type ?values)
         "lux math cosh" (analyse-math-cosh analyse exo-type ?values)
         "lux math sinh" (analyse-math-sinh analyse exo-type ?values)
         "lux math tanh" (analyse-math-tanh analyse exo-type ?values)
         "lux math exp" (analyse-math-exp analyse exo-type ?values)
         "lux math log" (analyse-math-log analyse exo-type ?values)
         "lux math root2" (analyse-math-root2 analyse exo-type ?values)
         "lux math root3" (analyse-math-root3 analyse exo-type ?values)
         "lux math ceil" (analyse-math-ceil analyse exo-type ?values)
         "lux math floor" (analyse-math-floor analyse exo-type ?values)
         "lux math round" (analyse-math-round analyse exo-type ?values)
         "lux math atan2" (analyse-math-atan2 analyse exo-type ?values)
         "lux math pow" (analyse-math-pow analyse exo-type ?values)
         
         "lux atom new" (analyse-atom-new analyse exo-type ?values)
         "lux atom get" (analyse-atom-get analyse exo-type ?values)
         "lux atom compare-and-swap" (analyse-atom-compare-and-swap analyse exo-type ?values)
         
         "lux process concurrency-level" (analyse-process-concurrency-level analyse exo-type ?values)
         "lux process future" (analyse-process-future analyse exo-type ?values)
         "lux process schedule" (analyse-process-schedule analyse exo-type ?values)
         
         ;; else
         (&/fail-with-loc (str "[Analyser Error] Unknown host procedure: " proc)))
    (catch Exception ex
      (&/fail-with-loc (str "[Analyser Error] Invalid syntax for procedure: " proc)))))
