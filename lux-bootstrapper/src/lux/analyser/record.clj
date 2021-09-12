(ns lux.analyser.record
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return |case]]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [module :as &&module])))

;; [Exports]
(defn order-record
  "(-> (List Syntax) (Lux (Maybe (List Syntax))))"
  [pairs]
  (if (even? (&/|length pairs))
    (let [pairs (&/|as-pairs pairs)]
      (|do [module&slot-group&slot-type (|case pairs
                                          (&/$End)
                                          (|do [module &/get-module-name]
                                            (return (&/$Some (&/T [module &/$End &type/Any]))))
                                          
                                          (&/$Item [[_ (&/$Identifier slot0)] _] _)
                                          (|do [[module name] (&&/resolved-ident slot0)
                                                _exported?&type&slots&_index (fn [lux]
                                                                               (|case ((&&module/find-slot module name) lux)
                                                                                 (&/$Left error)
                                                                                 (&/$Right (&/T [lux &/$None]))
                                                                                 
                                                                                 (&/$Right [lux* output])
                                                                                 (&/$Right (&/T [lux* (&/$Some output)]))))]
                                            (|case _exported?&type&slots&_index
                                              (&/$Some [_exported? type slots _index])
                                              (return (&/$Some (&/T [module slots type])))

                                              (&/$None)
                                              (return &/$None)))

                                          _
                                          (return &/$None))]
        (|case module&slot-group&slot-type
          (&/$Some [module slot-group slot-type])
          (|do [=pairs (&/map% (fn [kv]
                                 (|case kv
                                   [[_ (&/$Identifier k)] v]
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
            (return (&/$Some (&/T [=members slot-type]))))

          (&/$None)
          (return &/$None))))
    (return &/$None)))
