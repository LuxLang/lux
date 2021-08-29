(ns lux.analyser.record
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return |case]]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [module :as &&module])))

;; [Exports]
(defn order-record
  "(-> (List (, Syntax Syntax)) (Lux (List Syntax)))"
  [pairs]
  (|do [[module slot-group slot-type] (|case pairs
                                        (&/$End)
                                        (|do [module &/get-module-name]
                                          (return (&/T [module &/$End &type/Any])))
                                        
                                        (&/$Item [[_ (&/$Tag slot1)] _] _)
                                        (|do [[module name] (&&/resolved-ident slot1)
                                              [_exported? type slots _index] (&&module/find-slot module (str "#" name))]
                                          (return (&/T [module slots type])))

                                        _
                                        (&/fail-with-loc "[Analyser Error] Wrong syntax for records. Odd elements must be slots."))
        =pairs (&/map% (fn [kv]
                         (|case kv
                           [[_ (&/$Tag k)] v]
                           (|do [=k (&&/resolved-ident k)]
                             (return (&/T [(&/ident->text =k) v])))

                           _
                           (&/fail-with-loc "[Analyser Error] Wrong syntax for records. Odd elements must be slots.")))
                       pairs)
        _ (let [num-expected (&/|length slot-group)
                num-got (&/|length =pairs)]
            (&/assert! (= num-expected num-got)
                       (str "[Analyser Error] Wrong number of record members. Expected " num-expected ", but got " num-got ".")))
        =members (&/map% (fn [slot]
                           (let [slot (&/ident->text (&/T [module slot]))]
                             (if-let [member (&/|get slot =pairs)]
                               (return member)
                               (&/fail-with-loc (str "[Analyser Error] Missing slot: " slot)))))
                         slot-group)]
    (return (&/T [=members slot-type]))))
