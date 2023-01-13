;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns lux.analyser.record
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return |case]]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [module :as &&module])))

(defn ^:private record_slot [slot0]
  (|do [[module name] (&&/resolved-ident slot0)
        exported?&label (fn [lux]
                          (|case ((&&module/find-slot module name) lux)
                            (&/$Left error)
                            (&/$Right (&/T [lux &/$None]))
                            
                            (&/$Right [lux* output])
                            (&/$Right (&/T [lux* (&/$Some output)]))))]
    (return (|case exported?&label
              (&/$Some [exported? [label* type]])
              (&/$Some (&/T [label* type]))

              (&/$None)
              &/$None))))

(defn ^:private slot_type
  "(-> [Label Code] Type)"
  [it]
  (|let [[[label* type] value] it]
    type))

(defn ^:private same_record?
  "(-> (List [Label Code]) Bit)"
  [it]
  (|case it
    (&/$Item head tail)
    (|let [expected (slot_type head)]
      (&/|every? (fn [it] (->> it slot_type (&type/type= expected)))
                 tail))

    (&/$End)
    false))

(defn ^:private complete_record?
  "(-> (List [Label Code]) Bit)"
  [it]
  (loop [expected_lefts 0
         remaining it]
    (|case remaining
      (&/$Item [[label* type] value] (&/$End))
      (|case label*
        (&/$Some [lefts true family])
        (= (dec expected_lefts) lefts)

        (&/$None)
        (= 0 expected_lefts))
      
      (&/$Item [[(&/$Some [lefts false family]) type] value] tail)
      (and (= expected_lefts lefts)
           (recur (inc expected_lefts) tail))

      _
      false)))

;; [Exports]
(defn order-record
  "(-> (List Syntax) (Lux (Maybe (List Syntax))))"
  [pattern_matching? pairs]
  (let [arity (&/|length pairs)]
    (cond (= 0 arity)
          (return &/$None)

          (even? arity)
          (let [pairs (&/|as-pairs pairs)]
            (|do [resolved_slots* (&/map% (fn [pair]
                                            (|case pair
                                              [[_ (&/$Identifier slot0)] value]
                                              (|case slot0
                                                ["" short0]
                                                (if pattern_matching?
                                                  (return &/$None)
                                                  (|do [local? (&&module/find_local short0)]
                                                    (|case local?
                                                      (&/$None)
                                                      (|do [slot (record_slot slot0)]
                                                        (return (|case slot
                                                                  (&/$Some slot*)
                                                                  (&/$Some (&/T [slot* value]))

                                                                  (&/$None)
                                                                  &/$None)))

                                                      (&/$Some [local _inner _outer])
                                                      (return &/$None))))
                                                
                                                [module0 short0]
                                                (|do [slot (record_slot slot0)]
                                                  (return (|case slot
                                                            (&/$Some slot*)
                                                            (&/$Some (&/T [slot* value]))

                                                            (&/$None)
                                                            &/$None))))

                                              _
                                              (return &/$None)))
                                          pairs)]
              (|case (&/all_maybe resolved_slots*)
                (&/$Some resolved_slots)
                (|do [:let [sorted_slots (->> resolved_slots
                                              &/->seq
                                              (sort (fn [left right]
                                                      (|let [[[(&/$Some [leftsL right?L familyL]) typeL] valueL] left
                                                             [[(&/$Some [leftsR right?R familyR]) typeR] valueR] right]
                                                        (if (= leftsL leftsR)
                                                          (not right?L)
                                                          (< leftsL leftsR)))))
                                              &/->list)]
                      _ (&/assert! (same_record? sorted_slots)
                                   "[Analyser Error] Slots correspond to different record types.")
                      _ (&/assert! (complete_record? sorted_slots)
                                   "[Analyser Error] Missing record slots.")]
                  (return (&/$Some (&/T [(&/|map &/|second sorted_slots)
                                         (slot_type (&/|head sorted_slots))]))))

                (&/$None)
                (return &/$None))))

          true
          (return &/$None))))
