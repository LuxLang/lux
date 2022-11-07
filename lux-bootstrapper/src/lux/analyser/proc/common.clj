(ns lux.analyser.proc.common
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case assert!]]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [module :as &&module])))

(defn- analyse-lux-is [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Item reference (&/$Item sample (&/$End))) ?values]
            =reference (&&/analyse-1 analyse $var reference)
            =sample (&&/analyse-1 analyse $var sample)
            _ (&type/check exo-type &type/Bit)
            _location &/location]
        (return (&/|list (&&/|meta exo-type _location
                                   (&&/$proc (&/T ["lux" "is"]) (&/|list =sample =reference) (&/|list)))))))))

(defn- analyse-lux-try [analyse exo-type ?values]
  (&type/with-var
    (fn [$var]
      (|do [:let [(&/$Item op (&/$End)) ?values]
            =op (&&/analyse-1 analyse (&/$Function &type/Any $var) op)
            _ (&type/check exo-type (&/$Sum &type/Text ;; lux.Left
                                            $var ;; lux.Right
                                            ))
            _location &/location]
        (return (&/|list (&&/|meta exo-type _location
                                   (&&/$proc (&/T ["lux" "try"]) (&/|list =op) (&/|list)))))))))

(do-template [<name> <proc> <input-type> <output-type>]
  (defn- <name> [analyse exo-type ?values]
    (|do [:let [(&/$Item reference (&/$Item sample (&/$End))) ?values]
          =reference (&&/analyse-1 analyse <input-type> reference)
          =sample (&&/analyse-1 analyse <input-type> sample)
          _ (&type/check exo-type <output-type>)
          _location &/location]
      (return (&/|list (&&/|meta exo-type _location
                                 (&&/$proc (&/T <proc>) (&/|list =sample =reference) (&/|list)))))))

  analyse-text-eq     ["text" "="]      &type/Text &type/Bit
  analyse-text-lt     ["text" "<"]      &type/Text &type/Bit
  )

(defn- analyse-text-concat [analyse exo-type ?values]
  (|do [=values (&/map% (fn [it] (&&/analyse-1 analyse &type/Text it))
                        ?values)
        _ (&type/check exo-type &type/Text)
        _location &/location]
    (return (&/|list (&&/|meta exo-type _location
                               (&&/$proc (&/T ["text" "concat"]) =values (&/|list)))))))

(defn- analyse-text-index [analyse exo-type ?values]
  (|do [:let [(&/$Item start (&/$Item part (&/$Item text (&/$End)))) ?values]
        =start (&&/analyse-1 analyse &type/Nat start)
        =part (&&/analyse-1 analyse &type/Text part)
        =text (&&/analyse-1 analyse &type/Text text)
        _ (&type/check exo-type (&/$Apply &type/Nat &type/Maybe))
        _location &/location]
    (return (&/|list (&&/|meta exo-type _location
                               (&&/$proc (&/T ["text" "index"])
                                         (&/|list =text =part =start)
                                         (&/|list)))))))

(defn- analyse-text-clip [analyse exo-type ?values]
  (|do [:let [(&/$Item from (&/$Item to (&/$Item text (&/$End)))) ?values]
        =from (&&/analyse-1 analyse &type/Nat from)
        =to (&&/analyse-1 analyse &type/Nat to)
        =text (&&/analyse-1 analyse &type/Text text)
        _ (&type/check exo-type &type/Text)
        _location &/location]
    (return (&/|list (&&/|meta exo-type _location
                               (&&/$proc (&/T ["text" "clip"])
                                         (&/|list =text =from =to)
                                         (&/|list)))))))

(do-template [<name> <proc>]
  (defn- <name> [analyse exo-type ?values]
    (|do [:let [(&/$Item text (&/$End)) ?values]
          =text (&&/analyse-1 analyse &type/Text text)
          _ (&type/check exo-type &type/Nat)
          _location &/location]
      (return (&/|list (&&/|meta exo-type _location
                                 (&&/$proc (&/T ["text" <proc>])
                                           (&/|list =text)
                                           (&/|list)))))))

  analyse-text-size "size"
  )

(defn- analyse-text-char [analyse exo-type ?values]
  (|do [:let [(&/$Item idx (&/$Item text (&/$End))) ?values]
        =idx (&&/analyse-1 analyse &type/Nat idx)
        =text (&&/analyse-1 analyse &type/Text text)
        _ (&type/check exo-type &type/Nat)
        _location &/location]
    (return (&/|list (&&/|meta exo-type _location
                               (&&/$proc (&/T ["text" "char"])
                                         (&/|list =text =idx)
                                         (&/|list)))))))

(do-template [<name> <op>]
  (let [inputT (&/$Apply &type/Any &type/I64)
        outputT &type/I64]
    (defn- <name> [analyse exo-type ?values]
      (|do [:let [(&/$Item mask (&/$Item input (&/$End))) ?values]
            =mask (&&/analyse-1 analyse inputT mask)
            =input (&&/analyse-1 analyse inputT input)
            _ (&type/check exo-type outputT)
            _location &/location]
        (return (&/|list (&&/|meta exo-type _location
                                   (&&/$proc (&/T ["i64" <op>]) (&/|list =input =mask) (&/|list))))))))

  analyse-i64-and "and"
  analyse-i64-or  "or"
  analyse-i64-xor "xor"
  )

(do-template [<name> <op>]
  (let [inputT (&/$Apply &type/Any &type/I64)
        outputT &type/I64]
    (defn- <name> [analyse exo-type ?values]
      (|do [:let [(&/$Item shift (&/$Item input (&/$End))) ?values]
            =shift (&&/analyse-1 analyse &type/Nat shift)
            =input (&&/analyse-1 analyse inputT input)
            _ (&type/check exo-type outputT)
            _location &/location]
        (return (&/|list (&&/|meta exo-type _location
                                   (&&/$proc (&/T ["i64" <op>]) (&/|list =input =shift) (&/|list))))))))

  analyse-i64-left-shift  "left-shift"
  analyse-i64-right-shift "right-shift"
  )

(do-template [<proc> <name> <input-type> <output-type>]
  (let [inputT <input-type>
        outputT <output-type>]
    (defn- <name> [analyse exo-type ?values]
      (|do [:let [(&/$Item parameterC (&/$Item subjectC (&/$End))) ?values]
            parameterA (&&/analyse-1 analyse <input-type> parameterC)
            subjectA (&&/analyse-1 analyse <input-type> subjectC)
            _ (&type/check exo-type <output-type>)
            _location &/location]
        (return (&/|list (&&/|meta exo-type _location
                                   (&&/$proc (&/T <proc>) (&/|list subjectA parameterA) (&/|list))))))))

  ["i64" "="] analyse-i64-eq   (&/$Apply &type/Any &type/I64) &type/Bit
  ["i64" "+"] analyse-i64-add  (&/$Apply &type/Any &type/I64) &type/I64
  ["i64" "-"] analyse-i64-sub  (&/$Apply &type/Any &type/I64) &type/I64
  
  ["i64" "*"] analyse-int-mul  &type/Int  &type/Int
  ["i64" "/"] analyse-int-div  &type/Int  &type/Int
  ["i64" "%"] analyse-int-rem  &type/Int  &type/Int
  ["i64" "<"] analyse-int-lt   &type/Int  &type/Bit

  ["f64" "+"] analyse-f64-add &type/Frac &type/Frac
  ["f64" "-"] analyse-f64-sub &type/Frac &type/Frac
  ["f64" "*"] analyse-f64-mul &type/Frac &type/Frac
  ["f64" "/"] analyse-f64-div &type/Frac &type/Frac
  ["f64" "%"] analyse-f64-rem &type/Frac &type/Frac
  ["f64" "="] analyse-f64-eq  &type/Frac &type/Bit
  ["f64" "<"] analyse-f64-lt  &type/Frac &type/Bit
  )

(do-template [<encode> <encode-op> <decode> <decode-op> <type>]
  (do (defn- <encode> [analyse exo-type ?values]
        (|do [:let [(&/$Item x (&/$End)) ?values]
              =x (&&/analyse-1 analyse <type> x)
              _ (&type/check exo-type &type/Text)
              _location &/location]
          (return (&/|list (&&/|meta exo-type _location
                                     (&&/$proc (&/T <encode-op>) (&/|list =x) (&/|list)))))))

    (let [decode-type (&/$Apply <type> &type/Maybe)]
      (defn- <decode> [analyse exo-type ?values]
        (|do [:let [(&/$Item x (&/$End)) ?values]
              =x (&&/analyse-1 analyse &type/Text x)
              _ (&type/check exo-type decode-type)
              _location &/location]
          (return (&/|list (&&/|meta exo-type _location
                                     (&&/$proc (&/T <decode-op>) (&/|list =x) (&/|list)))))))))

  analyse-f64-encode ["f64" "encode"] analyse-f64-decode ["f64" "decode"] &type/Frac
  )

(do-template [<name> <from-type> <to-type> <op>]
  (defn- <name> [analyse exo-type ?values]
    (|do [:let [(&/$Item x (&/$End)) ?values]
          =x (&&/analyse-1 analyse <from-type> x)
          _ (&type/check exo-type <to-type>)
          _location &/location]
      (return (&/|list (&&/|meta exo-type _location
                                 (&&/$proc (&/T <op>) (&/|list =x) (&/|list)))))))

  analyse-int-char  &type/Int  &type/Text   ["i64" "char"]
  analyse-int-frac  &type/Int  &type/Frac   ["i64" "f64"]
  analyse-f64-int  &type/Frac &type/Int    ["f64" "i64"]

  analyse-io-log    &type/Text &type/Any    ["io" "log"]
  analyse-io-error  &type/Text &type/Nothing ["io" "error"]
  )

(defn- analyse-syntax-char-case! [analyse exo-type ?values]
  (|do [:let [(&/$Item ?input (&/$Item [_ (&/$Tuple ?pairs)] (&/$Item ?else (&/$End)))) ?values]
        _location &/location
        =input (&&/analyse-1 analyse &type/Nat ?input)
        _ (assert! (even? (&/|length ?pairs)) "The number of matches must be even!")
        =pairs (&/map% (fn [?pair]
                         (|let [[[_ (&/$Tuple ?patterns)] ?match] ?pair]
                           (|do [=match (&&/analyse-1 analyse exo-type ?match)]
                             (return (&/T [(&/|map (fn [?pattern]
                                                     (|let [[_ (&/$Text ^String ?pattern-char)] ?pattern]
                                                       (int (.charAt ?pattern-char 0))))
                                                   ?patterns)
                                           =match])))))
                       (&/|as-pairs ?pairs))
        =else (&&/analyse-1 analyse exo-type ?else)]
    (return (&/|list (&&/|meta exo-type _location
                               (&&/$proc (&/T ["lux" "syntax char case!"])
                                         (&/$Item =input (&/$Item =else (&/|map &/|second =pairs)))
                                         (&/|map &/|first =pairs)))))))

(let [extensions #{"is?#" "try#" "when_char#"

                   "log!#" "error#"

                   "text_=#" "text_<#" "text_composite#"
                   "text_clip#" "text_index#" "text_size#" "text_char#"

                   "i64_and#" "i64_or#" "i64_xor#" "i64_left#" "i64_right#"
                   "i64_+#" "i64_-#" "i64_=#"

                   "int_*#" "int_/#" "int_%#" "int_<#"
                   "int_f64#" "int_char#"

                   "f64_+#" "f64_-#" "f64_*#" "f64_/#" "f64_%#"
                   "f64_=#" "f64_<#"
                   "f64_int#" "f64_encoded#" "f64_decoded#"}]
  (defn uses_new_format? [extension]
    (if (extensions extension)
      true
      false)))

(defn analyse-proc [analyse exo-type proc ?values]
  (try (case proc
         "is?#" (analyse-lux-is analyse exo-type ?values)
         "try#" (analyse-lux-try analyse exo-type ?values)
         ;; Special extensions for performance reasons
         ;; Will be replaced by custom extensions in the future.
         "when_char#" (analyse-syntax-char-case! analyse exo-type ?values)

         "log!#" (analyse-io-log analyse exo-type ?values)
         "error#" (analyse-io-error analyse exo-type ?values)
         
         "text_=#" (analyse-text-eq analyse exo-type ?values)
         "text_<#" (analyse-text-lt analyse exo-type ?values)
         "text_composite#" (analyse-text-concat analyse exo-type ?values)
         "text_clip#" (analyse-text-clip analyse exo-type ?values)
         "text_index#" (analyse-text-index analyse exo-type ?values)
         "text_size#" (analyse-text-size analyse exo-type ?values)
         "text_char#" (analyse-text-char analyse exo-type ?values)
         
         "i64_and#" (analyse-i64-and analyse exo-type ?values)
         "i64_or#" (analyse-i64-or analyse exo-type ?values)
         "i64_xor#" (analyse-i64-xor analyse exo-type ?values)
         "i64_left#"  (analyse-i64-left-shift analyse exo-type ?values)
         "i64_right#" (analyse-i64-right-shift analyse exo-type ?values)
         
         "i64_+#" (analyse-i64-add analyse exo-type ?values)
         "i64_-#" (analyse-i64-sub analyse exo-type ?values)
         "i64_=#" (analyse-i64-eq analyse exo-type ?values)
         
         "int_*#" (analyse-int-mul analyse exo-type ?values)
         "int_/#" (analyse-int-div analyse exo-type ?values)
         "int_%#" (analyse-int-rem analyse exo-type ?values)
         "int_<#" (analyse-int-lt analyse exo-type ?values)

         "int_f64#" (analyse-int-frac analyse exo-type ?values)
         "int_char#" (analyse-int-char analyse exo-type ?values)
         
         "f64_+#" (analyse-f64-add analyse exo-type ?values)
         "f64_-#" (analyse-f64-sub analyse exo-type ?values)
         "f64_*#" (analyse-f64-mul analyse exo-type ?values)
         "f64_/#" (analyse-f64-div analyse exo-type ?values)
         "f64_%#" (analyse-f64-rem analyse exo-type ?values)
         "f64_=#" (analyse-f64-eq analyse exo-type ?values)
         "f64_<#" (analyse-f64-lt analyse exo-type ?values)

         "f64_int#" (analyse-f64-int analyse exo-type ?values)
         "f64_encoded#" (analyse-f64-encode analyse exo-type ?values)
         "f64_decoded#" (analyse-f64-decode analyse exo-type ?values)

         ;; else
         (&/fail-with-loc (str "[Analyser Error] Unknown host procedure: " proc)))
    (catch Exception ex
      (&/fail-with-loc (str "[Analyser Error] Invalid syntax for procedure: " proc)))))
