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

  ^:private analyse-bit-left-shift             "left-shift"             &type/Nat
  ^:private analyse-bit-arithmetic-right-shift "arithmetic-right-shift" &type/Int
  ^:private analyse-bit-logical-right-shift    "logical-right-shift"    &type/Nat
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

  ^:private analyse-int-add  ["int" "+"]  &type/Int  &type/Int
  ^:private analyse-int-sub  ["int" "-"]  &type/Int  &type/Int
  ^:private analyse-int-mul  ["int" "*"]  &type/Int  &type/Int
  ^:private analyse-int-div  ["int" "/"]  &type/Int  &type/Int
  ^:private analyse-int-rem  ["int" "%"]  &type/Int  &type/Int
  ^:private analyse-int-eq   ["int" "="]  &type/Int  &type/Bool
  ^:private analyse-int-lt   ["int" "<"]  &type/Int  &type/Bool

  ^:private analyse-frac-add ["frac" "+"] &type/Frac &type/Frac
  ^:private analyse-frac-sub ["frac" "-"] &type/Frac &type/Frac
  ^:private analyse-frac-mul ["frac" "*"] &type/Frac &type/Frac
  ^:private analyse-frac-div ["frac" "/"] &type/Frac &type/Frac
  ^:private analyse-frac-rem ["frac" "%"] &type/Frac &type/Frac
  ^:private analyse-frac-eq  ["frac" "="] &type/Frac &type/Bool
  ^:private analyse-frac-lt  ["frac" "<"] &type/Frac &type/Bool
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

  ^:private analyse-int-min                 &type/Int  ["int"  "min"]
  ^:private analyse-int-max                 &type/Int  ["int"  "max"]

  ^:private analyse-frac-smallest           &type/Frac ["frac"  "smallest"]
  ^:private analyse-frac-min                &type/Frac ["frac"  "min"]
  ^:private analyse-frac-max                &type/Frac ["frac"  "max"]
  ^:private analyse-frac-not-a-number       &type/Frac ["frac"  "not-a-number"]
  ^:private analyse-frac-positive-infinity  &type/Frac ["frac"  "positive-infinity"]
  ^:private analyse-frac-negative-infinity  &type/Frac ["frac"  "negative-infinity"]
  )

(do-template [<name> <from-type> <to-type> <op>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons x (&/$Nil)) ?values]
          =x (&&/analyse-1 analyse <from-type> x)
          _ (&type/check exo-type <to-type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T <op>) (&/|list =x) (&/|list)))))))

  ^:private analyse-int-char     &type/Int  &type/Text   ["int" "char"]
  
  ^:private analyse-int-to-frac  &type/Int  &type/Frac   ["int" "to-frac"]
  ^:private analyse-frac-to-int  &type/Frac &type/Int    ["frac" "to-int"]

  ^:private analyse-io-log       &type/Text &type/Top    ["io" "log"]
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
  ^:private analyse-math-exp "exp"
  ^:private analyse-math-log "log"
  ^:private analyse-math-ceil "ceil"
  ^:private analyse-math-floor "floor"
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

(defn ^:private analyse-atom-read [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons ?atom (&/$Nil)) ?values]
            =atom (&&/analyse-1 analyse (&type/Atom $var) ?atom)
            _ (&type/check exo-type $var)
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["atom" "read"]) (&/|list =atom) (&/|list)))))))))

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

(defn ^:private analyse-box-new [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons ?init (&/$Nil)) ?values]
            =init (&&/analyse-1 analyse $var ?init)
            _ (&type/check exo-type (&/$UnivQ (&/|list) (&type/Box (&/$Bound 1) $var)))
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["box" "new"]) (&/|list =init) (&/|list)))))))))

(defn ^:private analyse-box-read [analyse exo-type ?values]
  (&type/with-var
    (fn [threadT]
      (&type/with-var
        (fn [valueT]
          (|do [:let [(&/$Cons boxC (&/$Nil)) ?values]
                boxA (&&/analyse-1 analyse (&type/Box threadT valueT) boxC)
                _ (&type/check exo-type valueT)
                _cursor &/cursor]
            (return (&/|list (&&/|meta exo-type _cursor
                                       (&&/$proc (&/T ["box" "read"]) (&/|list boxA) (&/|list)))))))))))

(defn ^:private analyse-box-write [analyse exo-type ?values]
  (&type/with-var
    (fn [threadT]
      (&type/with-var
        (fn [valueT]
          (|do [:let [(&/$Cons valueC (&/$Cons boxC (&/$Nil))) ?values]
                boxA (&&/analyse-1 analyse (&type/Box threadT valueT) boxC)
                valueA (&&/analyse-1 analyse valueT valueC)
                _ (&type/check exo-type &type/Top)
                _cursor &/cursor]
            (return (&/|list (&&/|meta exo-type _cursor
                                       (&&/$proc (&/T ["box" "write"]) (&/|list valueA boxA) (&/|list)))))))))))

(defn ^:private analyse-process-concurrency-level [analyse exo-type ?values]
  (|do [:let [(&/$Nil) ?values]
        _ (&type/check exo-type &type/Nat)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["process" "concurrency-level"]) (&/|list) (&/|list)))))))

(defn ^:private analyse-process-future [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?procedure (&/$Nil)) ?values]
        =procedure (&&/analyse-1 analyse (&/$Apply &type/Top &type/IO) ?procedure)
        _ (&type/check exo-type &type/Top)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["process" "future"]) (&/|list =procedure) (&/|list)))))))

(defn ^:private analyse-process-schedule [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?milliseconds (&/$Cons ?procedure (&/$Nil))) ?values]
        =milliseconds (&&/analyse-1 analyse &type/Nat ?milliseconds)
        =procedure (&&/analyse-1 analyse (&/$Apply &type/Top &type/IO) ?procedure)
        _ (&type/check exo-type &type/Top)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["process" "schedule"]) (&/|list =milliseconds =procedure) (&/|list)))))))

(defn analyse-proc [analyse exo-type proc ?values]
  (try (case proc
         "lux is"                   (analyse-lux-is analyse exo-type ?values)
         "lux try"                  (analyse-lux-try analyse exo-type ?values)

         "lux box new"                 (analyse-box-new analyse exo-type ?values)
         "lux box read"                (analyse-box-read analyse exo-type ?values)
         "lux box write"               (analyse-box-write analyse exo-type ?values)
         
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
         "lux text contains?"            (analyse-text-contains? analyse exo-type ?values)
         
         "lux bit count"                (analyse-bit-count analyse exo-type ?values)
         "lux bit and"                  (analyse-bit-and analyse exo-type ?values)
         "lux bit or"                   (analyse-bit-or analyse exo-type ?values)
         "lux bit xor"                  (analyse-bit-xor analyse exo-type ?values)
         "lux bit left-shift"           (analyse-bit-left-shift analyse exo-type ?values)
         "lux bit arithmetic-right-shift" (analyse-bit-arithmetic-right-shift analyse exo-type ?values)
         "lux bit logical-right-shift" (analyse-bit-logical-right-shift analyse exo-type ?values)
         
         "lux array new"    (analyse-array-new analyse exo-type ?values)
         "lux array get"    (analyse-array-get analyse exo-type ?values)
         "lux array put"    (analyse-array-put analyse exo-type ?values)
         "lux array remove" (analyse-array-remove analyse exo-type ?values)
         "lux array size"   (analyse-array-size analyse exo-type ?values)

         
         "lux int +" (analyse-int-add analyse exo-type ?values)
         "lux int -" (analyse-int-sub analyse exo-type ?values)
         "lux int *" (analyse-int-mul analyse exo-type ?values)
         "lux int /" (analyse-int-div analyse exo-type ?values)
         "lux int %" (analyse-int-rem analyse exo-type ?values)
         "lux int =" (analyse-int-eq analyse exo-type ?values)
         "lux int <" (analyse-int-lt analyse exo-type ?values)
         "lux int min" (analyse-int-min analyse exo-type ?values)
         "lux int max" (analyse-int-max analyse exo-type ?values)
         "lux int to-frac" (analyse-int-to-frac analyse exo-type ?values)
         "lux int char" (analyse-int-char analyse exo-type ?values)
         
         "lux frac +" (analyse-frac-add analyse exo-type ?values)
         "lux frac -" (analyse-frac-sub analyse exo-type ?values)
         "lux frac *" (analyse-frac-mul analyse exo-type ?values)
         "lux frac /" (analyse-frac-div analyse exo-type ?values)
         "lux frac %" (analyse-frac-rem analyse exo-type ?values)
         "lux frac =" (analyse-frac-eq analyse exo-type ?values)
         "lux frac <" (analyse-frac-lt analyse exo-type ?values)
         "lux frac encode" (analyse-frac-encode analyse exo-type ?values)
         "lux frac decode" (analyse-frac-decode analyse exo-type ?values)
         "lux frac smallest" (analyse-frac-smallest analyse exo-type ?values)
         "lux frac min" (analyse-frac-min analyse exo-type ?values)
         "lux frac max" (analyse-frac-max analyse exo-type ?values)
         "lux frac not-a-number" (analyse-frac-not-a-number analyse exo-type ?values)
         "lux frac positive-infinity" (analyse-frac-positive-infinity analyse exo-type ?values)
         "lux frac negative-infinity" (analyse-frac-negative-infinity analyse exo-type ?values)
         "lux frac to-int" (analyse-frac-to-int analyse exo-type ?values)
         
         "lux math cos" (analyse-math-cos analyse exo-type ?values)
         "lux math sin" (analyse-math-sin analyse exo-type ?values)
         "lux math tan" (analyse-math-tan analyse exo-type ?values)
         "lux math acos" (analyse-math-acos analyse exo-type ?values)
         "lux math asin" (analyse-math-asin analyse exo-type ?values)
         "lux math atan" (analyse-math-atan analyse exo-type ?values)
         "lux math exp" (analyse-math-exp analyse exo-type ?values)
         "lux math log" (analyse-math-log analyse exo-type ?values)
         "lux math ceil" (analyse-math-ceil analyse exo-type ?values)
         "lux math floor" (analyse-math-floor analyse exo-type ?values)
         "lux math pow" (analyse-math-pow analyse exo-type ?values)
         
         "lux atom new" (analyse-atom-new analyse exo-type ?values)
         "lux atom read" (analyse-atom-read analyse exo-type ?values)
         "lux atom compare-and-swap" (analyse-atom-compare-and-swap analyse exo-type ?values)
         
         "lux process concurrency-level" (analyse-process-concurrency-level analyse exo-type ?values)
         "lux process future" (analyse-process-future analyse exo-type ?values)
         "lux process schedule" (analyse-process-schedule analyse exo-type ?values)
         
         ;; else
         (&/fail-with-loc (str "[Analyser Error] Unknown host procedure: " proc)))
    (catch Exception ex
      (&/fail-with-loc (str "[Analyser Error] Invalid syntax for procedure: " proc)))))
