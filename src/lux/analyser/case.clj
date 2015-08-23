;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.analyser.case
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftags |do return fail |let |case $$]]
                 [parser :as &parser]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [env :as &env]
                          [module :as &module]
                          [record :as &&record])))

;; [Tags]
(deftags
  ["DefaultTotal"
   "BoolTotal"
   "IntTotal"
   "RealTotal"
   "CharTotal"
   "TextTotal"
   "ProdTotal"
   "SumTotal"]
  )

(deftags
  ["StoreTestAC"
   "BoolTestAC"
   "IntTestAC"
   "RealTestAC"
   "CharTestAC"
   "TextTestAC"
   "ProdTestAC"
   "SumTestAC"]
  )

;; [Utils]
(def ^:private unit
  (&/P (&/cursor$ "" -1 -1) (&/S &/$TupleS (&/|list))))

(defn ^:private resolve-type [type]
  (|case type
    (&/$VarT ?id)
    (|do [type* (&/try-all% (&/|list (&type/deref ?id)
                                     (fail "##9##")))]
      (resolve-type type*))

    (&/$AllT _aenv _aname _aarg _abody)
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

(let [cleaner (fn [_abody ena]
                (|let [[_aenv _aname _aarg (&/$VarT _avar)] ena]
                  (|do [_ (&type/set-var _avar (&/S &/$BoundT _aarg))]
                    (&type/clean* _avar _abody))))]
  (defn adjust-type* [up type]
    "(-> (List (, (Maybe (Env Text Type)) Text Text Type)) Type (Lux Type))"
    ;; (prn 'adjust-type* (&type/show-type type))
    (|case type
      (&/$AllT _aenv _aname _aarg _abody)
      (&type/with-var
        (fn [$var]
          (|do [=type (&type/apply-type type $var)]
            (adjust-type* (&/Cons$ ($$ &/P _aenv _aname _aarg $var) up) =type))))

      (&/$SumT ?left ?right)
      (|do [=left (&/fold% cleaner ?left up)
            =right (&/fold% cleaner ?right up)]
        (return (&type/Sum$ =left =right)))

      (&/$ProdT ?left ?right)
      (|do [=left (&/fold% cleaner ?left up)
            =right (&/fold% cleaner ?right up)]
        (return (&type/Prod$ =left =right)))

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
      )))

(defn adjust-type [type]
  "(-> Type (Lux Type))"
  (adjust-type* (&/|list) type))

(defn ^:private resolve-tag [tag type]
  (|do [[=module =name] (&&/resolved-ident tag)
        type* (adjust-type type)
        idx (&module/tag-index =module =name)
        group (&module/tag-group =module =name)
        case-type (&type/variant-case idx type*)]
    (return ($$ &/P idx (&/|length group) case-type))))

(defn ^:private analyse-pattern [value-type pattern kont]
  (|let [[_ pattern*] pattern]
    (|case pattern*
      (&/$SymbolS "" name)
      (|do [=kont (&env/with-local name value-type
                    kont)
            idx &env/next-local-idx]
        (return (&/P (&/S $StoreTestAC idx) =kont)))

      (&/$SymbolS ident)
      (fail (str "[Pattern-matching Error] Symbols must be unqualified: " (&/ident->text ident)))

      (&/$BoolS ?value)
      (|do [_ (&type/check value-type &type/Bool)
            =kont kont]
        (return (&/P (&/S $BoolTestAC ?value) =kont)))

      (&/$IntS ?value)
      (|do [_ (&type/check value-type &type/Int)
            =kont kont]
        (return (&/P (&/S $IntTestAC ?value) =kont)))

      (&/$RealS ?value)
      (|do [_ (&type/check value-type &type/Real)
            =kont kont]
        (return (&/P (&/S $RealTestAC ?value) =kont)))

      (&/$CharS ?value)
      (|do [_ (&type/check value-type &type/Char)
            =kont kont]
        (return (&/P (&/S $CharTestAC ?value) =kont)))

      (&/$TextS ?value)
      (|do [_ (&type/check value-type &type/Text)
            =kont kont]
        (return (&/P (&/S $TextTestAC ?value) =kont)))

      (&/$TupleS (&/$Cons ?_left ?tail))
      (|do [value-type* (adjust-type value-type)]
        (|case value-type*
          (&/$ProdT ?left ?right)
          (|do [[=left [=right =kont]] (analyse-pattern ?left ?_left
                                                        (|do [[=right =kont] (|case ?tail
                                                                               (&/$Cons ?_right (&/$Nil))
                                                                               (analyse-pattern ?right ?_right kont)

                                                                               (&/$Nil)
                                                                               (fail "[Pattern-matching Error] Pattern-matching mismatch. Tuple has wrong size.")

                                                                               _
                                                                               (analyse-pattern ?right (&/S &/$TupleS ?tail) kont))]
                                                          (return (&/P =right =kont))))]
            (return (&/P (&/S $ProdTestAC =left =right) =kont)))

          _
          (fail (str "[Pattern-matching Error] Tuples require product-types: " (&type/show-type value-type*)))))
      
      (&/$RecordS pairs)
      (|do [?members (&&record/order-record pairs)]
        (analyse-pattern value-type (&/S &/$TupleS ?members) kont))

      (&/$TagS ?ident)
      (|do [[idx group-count case-type] (resolve-tag ?ident value-type)
            [=test =kont] (analyse-pattern case-type unit kont)]
        (return (&/P (&/S $SumTestAC ($$ &/P idx group-count =test)) =kont)))

      (&/$FormS (&/$Cons [_ (&/$TagS ?ident)]
                         ?values))
      (|do [[idx group-count case-type] (resolve-tag ?ident value-type)
            [=test =kont] (case (&/|length ?values)
                            0 (analyse-pattern case-type unit kont)
                            1 (analyse-pattern case-type (&/|head ?values) kont)
                            ;; 1+
                            (analyse-pattern case-type (&/P (&/cursor$ "" -1 -1) (&/S &/$TupleS ?values)) kont))
            ;; :let [_ (println "#15")]
            ]
        (return (&/P (&/S $SumTestAC ($$ &/P idx group-count =test)) =kont)))
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
        (return (&/S $DefaultTotal true))

        [[?tag [total? ?values]] ($StoreTestAC ?idx)]
        (return (&/S ?tag (&/P true ?values)))
        
        [($DefaultTotal total?) ($BoolTestAC ?value)]
        (return (&/S $BoolTotal (&/P total? (&/|list ?value))))

        [($BoolTotal total? ?values) ($BoolTestAC ?value)]
        (return (&/S $BoolTotal (&/P total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($IntTestAC ?value)]
        (return (&/S $IntTotal (&/P total? (&/|list ?value))))

        [($IntTotal total? ?values) ($IntTestAC ?value)]
        (return (&/S $IntTotal (&/P total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($RealTestAC ?value)]
        (return (&/S $RealTotal (&/P total? (&/|list ?value))))

        [($RealTotal total? ?values) ($RealTestAC ?value)]
        (return (&/S $RealTotal (&/P total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($CharTestAC ?value)]
        (return (&/S $CharTotal (&/P total? (&/|list ?value))))

        [($CharTotal total? ?values) ($CharTestAC ?value)]
        (return (&/S $CharTotal (&/P total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($TextTestAC ?value)]
        (return (&/S $TextTotal (&/P total? (&/|list ?value))))

        [($TextTotal total? ?values) ($TextTestAC ?value)]
        (return (&/S $TextTotal (&/P total? (&/Cons$ ?value ?values))))

        [($DefaultTotal total?) ($ProdTestAC ?left ?right)]
        (|do [:let [_default (&/S $DefaultTotal total?)]
              =left (merge-total _default (&/P ?left ?body))
              =right (merge-total _default (&/P ?right ?body))]
          (return (&/S $ProdTotal ($$ &/P total? =left =right))))

        [($ProdTotal total? ?_left ?_right) ($ProdTestAC ?left ?right)]
        (|do [=left (merge-total ?_left (&/P ?left ?body))
              =right (merge-total ?_right (&/P ?right ?body))]
          (return (&/S $ProdTotal ($$ &/P total? =left =right))))

        [($DefaultTotal total?) ($SumTestAC ?tag ?count ?test)]
        (|do [sub-struct (merge-total (&/S $DefaultTotal total?)
                                      (&/P ?test ?body))
              structs (|case (&/|list-put ?tag sub-struct (&/|repeat ?count (&/S $DefaultTotal total?)))
                        (&/$Some list)
                        (return list)

                        (&/$None)
                        (fail "[Pattern-matching Error] YOLO"))]
          (return (&/S $SumTotal (&/P total? structs))))

        [($SumTotal total? ?branches) ($SumTestAC ?tag ?count ?test)]
        (|do [sub-struct (merge-total (|case (&/|at ?tag ?branches)
                                        (&/$Some sub)
                                        sub
                                        
                                        (&/$None)
                                        (&/S $DefaultTotal total?))
                                      (&/P ?test ?body))
              structs (|case (&/|list-put ?tag sub-struct ?branches)
                        (&/$Some list)
                        (return list)

                        (&/$None)
                        (fail "[Pattern-matching Error] YOLO"))]
          (return (&/S $SumTotal (&/P total? structs))))
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

    ($ProdTotal ?total ?_left ?_right)
    (if ?total
      (return true)
      (|do [value-type* (resolve-type value-type)]
        (|case value-type*
          (&/$ProdT ?left ?right)
          (|do [=left (check-totality ?left ?_left)
                =right (check-totality ?right ?_right)]
            (return (and =left =right)))

          _
          (fail "[Pattern-maching Error] Tuple is not total."))))

    ($SumTotal ?total ?structs)
    (if ?total
      (return true)
      (|do [value-type* (resolve-type value-type)]
        (|case [value-type* ?structs]
          [(&/$SumT ?left ?right) (&/$Cons ?_left ?tail)]
          (|do [=left (check-totality ?left ?_left)
                =right (|case ?tail
                         (&/$Cons ?_right (&/$Nil))
                         (check-totality ?right ?_right)

                         (&/$Nil)
                         (fail "[Pattern-matching Error] Pattern-matching mismatch. Variant has wrong size.")

                         _
                         (check-totality ?right ($SumTotal ?total ?tail)))]
            (return (and =left =right)))

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
        struct (&/fold% merge-total (&/S $DefaultTotal false) patterns)
        ? (check-totality value-type struct)]
    (if ?
      (return patterns)
      (fail "[Pattern-maching Error] Pattern-matching is non-total."))))
