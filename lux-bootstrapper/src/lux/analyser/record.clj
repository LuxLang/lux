(ns lux.analyser.record
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return |case]]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [module :as &&module])))

;; [Exports]
(defn order-record [pairs]
  "(-> (List (, Syntax Syntax)) (Lux (List Syntax)))"
  (|do [[tag-group tag-type] (|case pairs
                               (&/$Nil)
                               (return (&/T [&/$Nil &type/Any]))
                               
                               (&/$Cons [[_ (&/$Tag tag1)] _] _)
                               (|do [[module name] (&&/resolved-ident tag1)
                                     tags (&&module/tag-group module name)
                                     type (&&module/tag-type module name)]
                                 (return (&/T [tags type])))

                               _
                               (&/fail-with-loc "[Analyser Error] Wrong syntax for records. Odd elements must be tags."))
        =pairs (&/map% (fn [kv]
                         (|case kv
                           [[_ (&/$Tag k)] v]
                           (|do [=k (&&/resolved-ident k)]
                             (return (&/T [(&/ident->text =k) v])))

                           _
                           (&/fail-with-loc "[Analyser Error] Wrong syntax for records. Odd elements must be tags.")))
                       pairs)
        _ (let [num-expected (&/|length tag-group)
                num-got (&/|length =pairs)]
            (&/assert! (= num-expected num-got)
                       (str "[Analyser Error] Wrong number of record members. Expected " num-expected ", but got " num-got ".")))
        =members (&/map% (fn [tag]
                           (if-let [member (&/|get tag =pairs)]
                             (return member)
                             (&/fail-with-loc (str "[Analyser Error] Missing tag: " tag))))
                         (&/|map &/ident->text tag-group))]
    (return (&/T [=members tag-type]))))
