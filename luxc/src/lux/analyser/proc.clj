(ns lux.analyser.proc
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case assert!]]
                 [type :as &type])
            (lux.analyser [base :as &&])))

(defn ^:private analyse-lux-== [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Cons left (&/$Cons right (&/$Nil))) ?values]
            =left (&&/analyse-1 analyse $var left)
            =right (&&/analyse-1 analyse $var right)
            _ (&type/check exo-type &type/Bool)
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["lux" "=="]) (&/|list =left =right) (&/|list)))))))))

(do-template [<name> <proc> <input-type> <output-type>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons x (&/$Cons y (&/$Nil))) ?values]
          =x (&&/analyse-1 analyse <input-type> x)
          =y (&&/analyse-1 analyse <input-type> y)
          _ (&type/check exo-type <output-type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta <output-type> _cursor
                                 (&&/$proc (&/T <proc>) (&/|list =x =y) (&/|list)))))))

  ^:private analyse-text-eq     ["text" "="]      &type/Text &type/Bool
  ^:private analyse-text-append ["text" "append"] &type/Text &type/Text
  )

;; (do-template [<name> <op>]
;;   (defn <name> [analyse exo-type ?values]
;;     (|do [:let [(&/$Cons input (&/$Cons mask (&/$Nil))) ?values]
;;           =mask (&&/analyse-1 analyse &type/Nat mask)
;;           =input (&&/analyse-1 analyse &type/Nat input)
;;           _ (&type/check exo-type &type/Nat)
;;           _cursor &/cursor]
;;       (return (&/|list (&&/|meta exo-type _cursor
;;                                  (&&/$proc (&/T ["bit" <op>]) (&/|list =input =mask) (&/|list)))))))

;;   ^:private analyse-bit-and "and"
;;   ^:private analyse-bit-or  "or"
;;   ^:private analyse-bit-xor "xor"
;;   )

;; (defn ^:private analyse-bit-count [analyse exo-type ?values]
;;   (|do [:let [(&/$Cons input (&/$Nil)) ?values]
;;         =input (&&/analyse-1 analyse &type/Nat input)
;;         _ (&type/check exo-type &type/Nat)
;;         _cursor &/cursor]
;;     (return (&/|list (&&/|meta exo-type _cursor
;;                                (&&/$proc (&/T ["bit" "count"]) (&/|list =input) (&/|list)))))))

;; (do-template [<name> <op> <type>]
;;   (defn <name> [analyse exo-type ?values]
;;     (|do [:let [(&/$Cons input (&/$Cons shift (&/$Nil))) ?values]
;;           =shift (&&/analyse-1 analyse &type/Nat shift)
;;           =input (&&/analyse-1 analyse <type> input)
;;           _ (&type/check exo-type <type>)
;;           _cursor &/cursor]
;;       (return (&/|list (&&/|meta exo-type _cursor
;;                                  (&&/$proc (&/T ["bit" <op>]) (&/|list =input =shift) (&/|list)))))))

;;   ^:private analyse-bit-shift-left           "shift-left"           &type/Nat
;;   ^:private analyse-bit-shift-right          "shift-right"          &type/Int
;;   ^:private analyse-bit-unsigned-shift-right "unsigned-shift-right" &type/Nat
;;   )

(do-template [<name> <proc> <input-type> <output-type>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons x (&/$Cons y (&/$Nil))) ?values]
          =x (&&/analyse-1 analyse <input-type> x)
          =y (&&/analyse-1 analyse <input-type> y)
          _ (&type/check exo-type <output-type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta <output-type> _cursor
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
  )

(defn ^:private analyse-deg-scale [analyse exo-type ?values]
  (|do [:let [(&/$Cons x (&/$Cons y (&/$Nil))) ?values]
        =x (&&/analyse-1 analyse &type/Deg x)
        =y (&&/analyse-1 analyse &type/Nat y)
        _ (&type/check exo-type &type/Deg)
        _cursor &/cursor]
    (return (&/|list (&&/|meta &type/Deg _cursor
                               (&&/$proc (&/T ["deg" "scale"]) (&/|list =x =y) (&/|list)))))))

(do-template [<encode> <encode-op> <decode> <decode-op> <type>]
  (do (defn <encode> [analyse exo-type ?values]
        (|do [:let [(&/$Cons x (&/$Nil)) ?values]
              =x (&&/analyse-1 analyse <type> x)
              _ (&type/check exo-type &type/Text)
              _cursor &/cursor]
          (return (&/|list (&&/|meta &type/Text _cursor
                                     (&&/$proc (&/T <encode-op>) (&/|list =x) (&/|list)))))))

    (let [decode-type (&/$AppT &type/Maybe <type>)]
      (defn <decode> [analyse exo-type ?values]
        (|do [:let [(&/$Cons x (&/$Nil)) ?values]
              =x (&&/analyse-1 analyse &type/Text x)
              _ (&type/check exo-type decode-type)
              _cursor &/cursor]
          (return (&/|list (&&/|meta decode-type _cursor
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
      (return (&/|list (&&/|meta <type> _cursor
                                 (&&/$proc (&/T <op>) (&/|list) (&/|list)))))))

  ^:private analyse-nat-min-value  &type/Nat  ["nat"  "min-value"]
  ^:private analyse-nat-max-value  &type/Nat  ["nat"  "max-value"]

  ^:private analyse-int-min-value  &type/Int  ["int"  "min-value"]
  ^:private analyse-int-max-value  &type/Int  ["int"  "max-value"]

  ^:private analyse-deg-min-value &type/Deg ["deg" "min-value"]
  ^:private analyse-deg-max-value &type/Deg ["deg" "max-value"]
  )

(do-template [<name> <from-type> <to-type> <op>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons x (&/$Nil)) ?values]
          =x (&&/analyse-1 analyse <from-type> x)
          _ (&type/check exo-type <to-type>)
          _cursor &/cursor]
      (return (&/|list (&&/|meta <to-type> _cursor
                                 (&&/$proc (&/T <op>) (&/|list =x) (&/|list)))))))

  ^:private analyse-nat-to-int   &type/Nat  &type/Int  ["nat" "to-int"]
  ^:private analyse-nat-to-char  &type/Nat  &type/Char ["nat" "to-char"]
  ^:private analyse-int-to-nat   &type/Int  &type/Nat  ["int" "to-nat"]
  ^:private analyse-char-to-nat  &type/Char &type/Nat  ["char" "to-nat"]

  ^:private analyse-deg-to-real &type/Deg &type/Real ["deg" "to-real"]
  ^:private analyse-real-to-deg &type/Real &type/Deg ["real" "to-deg"]
  )

(defn analyse-proc [analyse exo-type category proc ?values]
  (case category
    ;; "lux"
    ;; (case proc
    ;;   "=="                   (analyse-lux-== analyse exo-type ?values))

    "text"
    (case proc
      "="                    (analyse-text-eq analyse exo-type ?values)
      "append"               (analyse-text-append analyse exo-type ?values))

    ;; "bit"
    ;; (case proc
    ;;   "count"                (analyse-bit-count analyse exo-type ?values)
    ;;   "and"                  (analyse-bit-and analyse exo-type ?values)
    ;;   "or"                   (analyse-bit-or analyse exo-type ?values)
    ;;   "xor"                  (analyse-bit-xor analyse exo-type ?values)
    ;;   "shift-left"           (analyse-bit-shift-left analyse exo-type ?values)
    ;;   "shift-right"          (analyse-bit-shift-right analyse exo-type ?values)
    ;;   "unsigned-shift-right" (analyse-bit-unsigned-shift-right analyse exo-type ?values))
    
    ;; "array"
    ;; (case proc
    ;;   "new"    (analyse-array-new analyse exo-type ?values)
    ;;   "get"    (analyse-array-get analyse exo-type ?values)
    ;;   "put"    (analyse-jvm-aastore analyse exo-type ?values)
    ;;   "remove" (analyse-array-remove analyse exo-type ?values)
    ;;   "size"   (analyse-jvm-arraylength analyse exo-type ?values))

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
      ;; "encode" (analyse-deg-encode analyse exo-type ?values)
      ;; "decode" (analyse-deg-decode analyse exo-type ?values)
      ;; "min-value" (analyse-deg-min-value analyse exo-type ?values)
      ;; "max-value" (analyse-deg-max-value analyse exo-type ?values)
      ;; "to-real" (analyse-deg-to-real analyse exo-type ?values)
      ;; "scale" (analyse-deg-scale analyse exo-type ?values)
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
      ;; "decode" (analyse-real-decode analyse exo-type ?values)
      ;; "min-value" (analyse-real-min-value analyse exo-type ?values)
      ;; "max-value" (analyse-real-max-value analyse exo-type ?values)
      ;; "to-deg" (analyse-real-to-real analyse exo-type ?values)
      )

    ;; "char"
    ;; (case proc
    ;;   "to-nat" (analyse-char-to-nat analyse exo-type ?values)
    ;;   )
    
    ;; else
    (&/fail-with-loc (str "[Analyser Error] Unknown host procedure: " [category proc]))))
