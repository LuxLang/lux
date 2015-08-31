;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.case
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftags |do return fail |let |case]]
                 [parser :as &parser]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [env :as &env]
                          [module :as &module]
                          [record :as &&record])))

;; [Tags]
(deftags ""
  "DefaultTotal"
  "BoolTotal"
  "IntTotal"
  "RealTotal"
  "CharTotal"
  "TextTotal"
  "TupleTotal"
  "VariantTotal"
  )

(deftags ""
  "StoreTestAC"
  "BoolTestAC"
  "IntTestAC"
  "RealTestAC"
  "CharTestAC"
  "TextTestAC"
  "TupleTestAC"
  "VariantTestAC"
  )

;; [Utils]
(def ^:private unit
  (&/T (&/T "" -1 -1) (&/V &/$TupleS (&/|list))))

(defn ^:private resolve-type [type]
  (|case type
    (&/$VarT ?id)
    (|do [type* (&/try-all% (&/|list (&type/deref ?id)
                                     (fail "##9##")))]
      (resolve-type type*))

    (&/$UnivQ _)
    ;; (&type/actual-type _abody)
    (|do [$var &type/existential
          =type (&type/apply-type type $var)]
      (&type/actual-type =type))
    ;; (&type/with-var
    ;;   (fn [$var]
    ;;     (|do [=type (&type/apply-type type $var)]
    ;;       (&type/actual-type =type))))

    _
    (&type/actual-type type)))

(defn update-up-frame [frame]
  (|let [[_env _idx _var] frame]
    (&/T _env (+ 2 _idx) _var)))

(defn adjust-type* [up type]
  "(-> (List (, (Maybe (List Type)) Int Type)) Type (Lux Type))"
  ;; (prn 'adjust-type* (&type/show-type type))
  (|case type
    (&/$UnivQ _aenv _abody)
    (&type/with-var
      (fn [$var]
        (|do [=type (&type/apply-type type $var)]
          (adjust-type* (&/Cons$ (&/T _aenv 1 $var) (&/|map update-up-frame up)) =type))))

    (&/$TupleT ?members)
    (|do [(&/$TupleT ?members*) (&/fold% (fn [_abody ena]
                                           (|let [[_aenv _aidx (&/$VarT _avar)] ena]
                                             (|do [_ (&type/set-var _avar (&/V &/$BoundT _aidx))]
                                               (&type/clean* _avar _abody))))
                                         type
                                         up)]
      (return (&type/Tuple$ (&/|map (fn [v]
                                      (&/fold (fn [_abody ena]
                                                (|let [[_aenv _aidx _avar] ena]
                                                  (&/V &/$UnivQ (&/T _aenv _abody))))
                                              v
                                              up))
                                    ?members*))))

    (&/$VariantT ?members)
    (|do [(&/$VariantT ?members*) (&/fold% (fn [_abody ena]
                                             (|let [[_aenv _aidx (&/$VarT _avar)] ena]
                                               (|do [_ (&type/set-var _avar (&/V &/$BoundT _aidx))]
                                                 (&type/clean* _avar _abody))))
                                           type
                                           up)]
      (return (&/V &/$VariantT (&/|map (fn [v]
                                         (&/fold (fn [_abody ena]
                                                   (|let [[_aenv _aidx _avar] ena]
                                                     (&/V &/$UnivQ (&/T _aenv _abody))))
                                                 v
                                                 up))
                                       ?members*))))

    (&/$AppT ?tfun ?targ)
    (|do [=type (&type/apply-type ?tfun ?targ)]
      (adjust-type* up =type))

    (&/$VarT ?id)
    (|do [type* (&/try-all% (&/|list (&type/deref ?id)
                                     (fail "##9##")))]
      (adjust-type* up type*))

    (&/$NamedT ?name ?type)
    (adjust-type* up ?type)

    _
    (assert false (prn 'adjust-type* (&type/show-type type)))
    ))

(defn adjust-type [type]
  "(-> Type (Lux Type))"
  (adjust-type* (&/|list) type))

(defn ^:private analyse-pattern [value-type pattern kont]
  (|let [[_ pattern*] pattern]
    (|case pattern*
      (&/$SymbolS "" name)
      (|do [=kont (&env/with-local name value-type
                    kont)
            idx &env/next-local-idx]
        (return (&/T (&/V $StoreTestAC idx) =kont)))

      (&/$SymbolS ident)
      (fail (str "[Pattern-matching Error] Symbols must be unqualified: " (&/ident->text ident)))

      (&/$BoolS ?value)
      (|do [_ (&type/check value-type &type/Bool)
            =kont kont]
        (return (&/T (&/V $BoolTestAC ?value) =kont)))

      (&/$IntS ?value)
      (|do [_ (&type/check value-type &type/Int)
            =kont kont]
        (return (&/T (&/V $IntTestAC ?value) =kont)))

      (&/$RealS ?value)
      (|do [_ (&type/check value-type &type/Real)
            =kont kont]
        (return (&/T (&/V $RealTestAC ?value) =kont)))

      (&/$CharS ?value)
      (|do [_ (&type/check value-type &type/Char)
            =kont kont]
        (return (&/T (&/V $CharTestAC ?value) =kont)))

      (&/$TextS ?value)
      (|do [_ (&type/check value-type &type/Text)
            =kont kont]
        (return (&/T (&/V $TextTestAC ?value) =kont)))

      (&/$TupleS ?members)
      (|do [value-type* (adjust-type value-type)]
        (do ;; (prn 'PM/TUPLE-1 (&type/show-type value-type*))
            (|case value-type*
              (&/$TupleT ?member-types)
              (do ;; (prn 'PM/TUPLE-2 (&/|length ?member-types) (&/|length ?members))
                  (if (not (.equals ^Object (&/|length ?member-types) (&/|length ?members)))
                    (fail (str "[Pattern-matching Error] Pattern-matching mismatch. Require tuple[" (&/|length ?member-types) "]. Given tuple [" (&/|length ?members) "]"))
                    (|do [[=tests =kont] (&/fold (fn [kont* vm]
                                                   (|let [[v m] vm]
                                                     (|do [[=test [=tests =kont]] (analyse-pattern v m kont*)]
                                                       (return (&/T (&/Cons$ =test =tests) =kont)))))
                                                 (|do [=kont kont]
                                                   (return (&/T (&/|list) =kont)))
                                                 (&/|reverse (&/zip2 ?member-types ?members)))]
                      (return (&/T (&/V $TupleTestAC =tests) =kont)))))

              _
              (fail (str "[Pattern-matching Error] Tuples require tuple-types: " (&type/show-type value-type*))))))
      
      (&/$RecordS pairs)
      (|do [?members (&&record/order-record pairs)
            ;; :let [_ (prn 'PRE (&type/show-type value-type))]
            value-type* (adjust-type value-type)
            ;; :let [_ (prn 'POST (&type/show-type value-type*))]
            ;; value-type* (resolve-type value-type)
            ]
        (|case value-type*
          (&/$TupleT ?member-types)
          (if (not (.equals ^Object (&/|length ?member-types) (&/|length ?members)))
            (fail (str "[Pattern-matching Error] Pattern-matching mismatch. Require record[" (&/|length ?member-types) "]. Given record[" (&/|length ?members) "]"))
            (|do [[=tests =kont] (&/fold (fn [kont* vm]
                                           (|let [[v m] vm]
                                             (|do [[=test [=tests =kont]] (analyse-pattern v m kont*)]
                                               (return (&/T (&/Cons$ =test =tests) =kont)))))
                                         (|do [=kont kont]
                                           (return (&/T (&/|list) =kont)))
                                         (&/|reverse (&/zip2 ?member-types ?members)))]
              (return (&/T (&/V $TupleTestAC =tests) =kont))))

          _
          (fail "[Pattern-matching Error] Record requires record-type.")))

      (&/$TagS ?ident)
      (|do [;; :let [_ (println "#00" (&/ident->text ?ident))]
            [=module =name] (&&/resolved-ident ?ident)
            ;; :let [_ (println "#01")]
            value-type* (adjust-type value-type)
            ;; :let [_ (println "#02")]
            idx (&module/tag-index =module =name)
            group (&module/tag-group =module =name)
            ;; :let [_ (println "#03")]
            case-type (&type/variant-case idx value-type*)
            ;; :let [_ (println "#04")]
            [=test =kont] (analyse-pattern case-type unit kont)
            ;; :let [_ (println "#05")]
            ]
        (return (&/T (&/V $VariantTestAC (&/T idx (&/|length group) =test)) =kont)))

      (&/$FormS (&/$Cons [_ (&/$TagS ?ident)]
                         ?values))
      (|do [;; :let [_ (println "#10" (&/ident->text ?ident))]
            [=module =name] (&&/resolved-ident ?ident)
            ;; :let [_ (println "#11")]
            value-type* (adjust-type value-type)
            ;; :let [_ (println "#12" (&type/show-type value-type*))]
            idx (&module/tag-index =module =name)
            group (&module/tag-group =module =name)
            ;; :let [_ (println "#13")]
            case-type (&type/variant-case idx value-type*)
            ;; :let [_ (println "#14" (&type/show-type case-type))]
            [=test =kont] (case (int (&/|length ?values))
                            0 (analyse-pattern case-type unit kont)
                            1 (analyse-pattern case-type (&/|head ?values) kont)
                            ;; 1+
                            (analyse-pattern case-type (&/T (&/T "" -1 -1) (&/V &/$TupleS ?values)) kont))
            ;; :let [_ (println "#15")]
            ]
        (return (&/T (&/V $VariantTestAC (&/T idx (&/|length group) =test)) =kont)))
      )))

(defn ^:private analyse-branch [analyse exo-type value-type pattern body patterns]
  (|do [pattern+body (analyse-pattern value-type pattern
                                      (&&/analyse-1 analyse exo-type body))]
    (return (&/Cons$ pattern+body patterns))))

(let [compare-kv #(.compareTo ^String (aget ^objects %1 0) ^String (aget ^objects %2 0))]
  (defn ^:private merge-total [struct test+body]
    (|let [[test ?body] test+body]
      (|case [struct test]
        [($DefaultTotal total?) ($StoreTestAC ?idx)]
        (return (&/V $DefaultTotal true))

        [[?tag [total? ?values]] ($StoreTestAC ?idx)]
        (return (&/V ?tag (&/T true ?values)))
        
        [($DefaultTotal total?) ($BoolTestAC ?value)]
        (return (&/V $BoolTotal (&/T total? (&/|list ?value))))

        [($BoolTotal total? ?values) ($BoolTestAC ?value)]
        (return (&/V $BoolTotal (&/T total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($IntTestAC ?value)]
        (return (&/V $IntTotal (&/T total? (&/|list ?value))))

        [($IntTotal total? ?values) ($IntTestAC ?value)]
        (return (&/V $IntTotal (&/T total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($RealTestAC ?value)]
        (return (&/V $RealTotal (&/T total? (&/|list ?value))))

        [($RealTotal total? ?values) ($RealTestAC ?value)]
        (return (&/V $RealTotal (&/T total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($CharTestAC ?value)]
        (return (&/V $CharTotal (&/T total? (&/|list ?value))))

        [($CharTotal total? ?values) ($CharTestAC ?value)]
        (return (&/V $CharTotal (&/T total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($TextTestAC ?value)]
        (return (&/V $TextTotal (&/T total? (&/|list ?value))))

        [($TextTotal total? ?values) ($TextTestAC ?value)]
        (return (&/V $TextTotal (&/T total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($TupleTestAC ?tests)]
        (|do [structs (&/map% (fn [t]
                                (merge-total (&/V $DefaultTotal total?) (&/T t ?body)))
                              ?tests)]
          (return (&/V $TupleTotal (&/T total? structs))))

        [($TupleTotal total? ?values) ($TupleTestAC ?tests)]
        (if (.equals ^Object (&/|length ?values) (&/|length ?tests))
          (|do [structs (&/map2% (fn [v t]
                                   (merge-total v (&/T t ?body)))
                                 ?values ?tests)]
            (return (&/V $TupleTotal (&/T total? structs))))
          (fail "[Pattern-matching Error] Inconsistent tuple-size."))

        [($DefaultTotal total?) ($VariantTestAC ?tag ?count ?test)]
        (|do [sub-struct (merge-total (&/V $DefaultTotal total?)
                                      (&/T ?test ?body))
              structs (|case (&/|list-put ?tag sub-struct (&/|repeat ?count (&/V $DefaultTotal total?)))
                        (&/$Some list)
                        (return list)

                        (&/$None)
                        (fail "[Pattern-matching Error] YOLO"))]
          (return (&/V $VariantTotal (&/T total? structs))))

        [($VariantTotal total? ?branches) ($VariantTestAC ?tag ?count ?test)]
        (|do [sub-struct (merge-total (|case (&/|at ?tag ?branches)
                                        (&/$Some sub)
                                        sub
                                        
                                        (&/$None)
                                        (&/V $DefaultTotal total?))
                                      (&/T ?test ?body))
              structs (|case (&/|list-put ?tag sub-struct ?branches)
                        (&/$Some list)
                        (return list)

                        (&/$None)
                        (fail "[Pattern-matching Error] YOLO"))]
          (return (&/V $VariantTotal (&/T total? structs))))
        ))))

(defn ^:private check-totality [value-type struct]
  ;; (prn 'check-totality (&type/show-type value-type) (&/adt->text struct))
  (|case struct
    ($DefaultTotal ?total)
    (return ?total)

    ($BoolTotal ?total ?values)
    (return (or ?total
                (= #{true false} (set (&/->seq ?values)))))

    ($IntTotal ?total _)
    (return ?total)

    ($RealTotal ?total _)
    (return ?total)

    ($CharTotal ?total _)
    (return ?total)

    ($TextTotal ?total _)
    (return ?total)

    ($TupleTotal ?total ?structs)
    (if ?total
      (return true)
      (|do [value-type* (resolve-type value-type)]
        (|case value-type*
          (&/$TupleT ?members)
          (|do [totals (&/map2% (fn [sub-struct ?member]
                                  (check-totality ?member sub-struct))
                                ?structs ?members)]
            (return (&/fold #(and %1 %2) true totals)))

          _
          (fail "[Pattern-maching Error] Tuple is not total."))))

    ($VariantTotal ?total ?structs)
    (if ?total
      (return true)
      (|do [value-type* (resolve-type value-type)]
        (|case value-type*
          (&/$VariantT ?members)
          (|do [totals (&/map2% (fn [sub-struct ?member]
                                  ;; (prn '$VariantTotal
                                  ;;      (&/adt->text sub-struct)
                                  ;;      (&type/show-type ?member))
                                  (check-totality ?member sub-struct))
                                ?structs ?members)]
            (return (&/fold #(and %1 %2) true totals)))

          _
          (fail "[Pattern-maching Error] Variant is not total."))))
    
    ;; _
    ;; (assert false (prn-str 'check-totality (&type/show-type value-type)
    ;;                        (&/adt->text struct)))
    ))

;; [Exports]
(defn analyse-branches [analyse exo-type value-type branches]
  (|do [patterns (&/fold% (fn [patterns branch]
                            (|let [[pattern body] branch]
                              (analyse-branch analyse exo-type value-type pattern body patterns)))
                          (&/|list)
                          branches)
        struct (&/fold% merge-total (&/V $DefaultTotal false) patterns)
        ? (check-totality value-type struct)]
    (if ?
      (return patterns)
      (fail "[Pattern-maching Error] Pattern-matching is non-total."))))
