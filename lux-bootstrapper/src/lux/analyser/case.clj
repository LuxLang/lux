(ns lux.analyser.case
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [defvariant |do return |let |case]]
                 [parser :as &parser]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [env :as &env]
                          [module :as &module]
                          [record :as &&record])))

;; [Tags]
(defvariant
  ("DefaultTotal" 1)
  ("BitTotal" 2)
  ("NatTotal" 2)
  ("IntTotal" 2)
  ("RevTotal" 2)
  ("FracTotal" 2)
  ("TextTotal" 2)
  ("TupleTotal" 2)
  ("VariantTotal" 2))

(defvariant
  ("NoTestAC" 0)
  ("StoreTestAC" 1)
  ("BitTestAC" 1)
  ("NatTestAC" 1)
  ("IntTestAC" 1)
  ("RevTestAC" 1)
  ("FracTestAC" 1)
  ("TextTestAC" 1)
  ("TupleTestAC" 1)
  ("VariantTestAC" 1))

;; [Utils]
(def ^:private unit-tuple
  (&/T [(&/T ["" -1 -1]) (&/$Tuple &/$End)]))

(defn ^:private resolve-type [type]
  (if (&type/type= &type/Any type)
    (return type)
    (|case type
      (&/$Var ?id)
      (|do [type* (&/try-all% (&/|list (&type/deref ?id)
                                       (&/fail-with-loc "##1##")))]
        (resolve-type type*))

      (&/$UnivQ _)
      (|do [$var &type/existential
            =type (&type/apply-type type $var)]
        (&type/actual-type =type))

      (&/$ExQ _ _)
      (|do [$var &type/existential
            =type (&type/apply-type type $var)]
        (&type/actual-type =type))

      _
      (&type/actual-type type))))

(defn update-up-frame [frame]
  (|let [[_env _idx _var] frame]
    (&/T [_env (+ 2 _idx) _var])))

(defn clean! [level ?tid parameter-idx type]
  (|case type
    (&/$Var ?id)
    (if (= ?tid ?id)
      (&/$Parameter (+ (* 2 level) parameter-idx))
      type)

    (&/$Primitive ?name ?params)
    (&/$Primitive ?name (&/|map (partial clean! level ?tid parameter-idx)
                                ?params))
    
    (&/$Function ?arg ?return)
    (&/$Function (clean! level ?tid parameter-idx ?arg)
                 (clean! level ?tid parameter-idx ?return))

    (&/$Apply ?param ?lambda)
    (&/$Apply (clean! level ?tid parameter-idx ?param)
              (clean! level ?tid parameter-idx ?lambda))

    (&/$Product ?left ?right)
    (&/$Product (clean! level ?tid parameter-idx ?left)
                (clean! level ?tid parameter-idx ?right))
    
    (&/$Sum ?left ?right)
    (&/$Sum (clean! level ?tid parameter-idx ?left)
            (clean! level ?tid parameter-idx ?right))

    (&/$UnivQ ?env ?body)
    (&/$UnivQ (&/|map (partial clean! level ?tid parameter-idx) ?env)
              (clean! (inc level) ?tid parameter-idx ?body))

    (&/$ExQ ?env ?body)
    (&/$ExQ (&/|map (partial clean! level ?tid parameter-idx) ?env)
            (clean! (inc level) ?tid parameter-idx ?body))

    _
    type
    ))

(defn beta-reduce! [level env type]
  (|case type
    (&/$Primitive ?name ?params)
    (&/$Primitive ?name (&/|map (partial beta-reduce! level env) ?params))

    (&/$Sum ?left ?right)
    (&/$Sum (beta-reduce! level env ?left)
            (beta-reduce! level env ?right))

    (&/$Product ?left ?right)
    (&/$Product (beta-reduce! level env ?left)
                (beta-reduce! level env ?right))

    (&/$Apply ?type-arg ?type-fn)
    (&/$Apply (beta-reduce! level env ?type-arg)
              (beta-reduce! level env ?type-fn))
    
    (&/$UnivQ ?local-env ?local-def)
    (|case ?local-env
      (&/$End)
      (&/$UnivQ ?local-env (beta-reduce! (inc level) env ?local-def))

      _
      type)

    (&/$ExQ ?local-env ?local-def)
    (|case ?local-env
      (&/$End)
      (&/$ExQ ?local-env (beta-reduce! (inc level) env ?local-def))

      _
      type)

    (&/$Function ?input ?output)
    (&/$Function (beta-reduce! level env ?input)
                 (beta-reduce! level env ?output))

    (&/$Parameter ?idx)
    (|case (&/|at (- ?idx (* 2 level)) env)
      (&/$Some parameter)
      (beta-reduce! level env parameter)

      _
      type)

    _
    type
    ))

(defn apply-type! [type-fn param]
  (|case type-fn
    (&/$UnivQ local-env local-def)
    (return (beta-reduce! 0 (->> local-env
                                 (&/$Item param)
                                 (&/$Item type-fn))
                          local-def))

    (&/$ExQ local-env local-def)
    (return (beta-reduce! 0 (->> local-env
                                 (&/$Item param)
                                 (&/$Item type-fn))
                          local-def))

    (&/$Apply A F)
    (|do [type-fn* (apply-type! F A)]
      (apply-type! type-fn* param))

    (&/$Named ?name ?type)
    (apply-type! ?type param)

    (&/$Ex id)
    (return (&/$Apply param type-fn))

    (&/$Var id)
    (|do [=type-fun (deref id)]
      (apply-type! =type-fun param))
    
    _
    (&/fail-with-loc (str "[Type System] Not a type-function:\n" (&type/show-type type-fn) "\n"))))

(defn adjust-type*
  "(-> (List (, (Maybe (List Type)) Int Type)) Type (Lux Type))"
  [up type]
  (|case type
    (&/$UnivQ _aenv _abody)
    (&type/with-var
      (fn [$var]
        (|do [=type (apply-type! type $var)
              ==type (adjust-type* (&/$Item (&/T [_aenv 1 $var])
                                            (&/|map update-up-frame up))
                                   =type)]
          (&type/clean $var ==type))))

    (&/$ExQ _aenv _abody)
    (|do [$var &type/existential
          =type (apply-type! type $var)]
      (adjust-type* up =type))

    (&/$Product ?left ?right)
    (let [=type (&/fold (fn [_abody ena]
                          (|let [[_aenv _aidx (&/$Var _avar)] ena]
                            (clean! 0 _avar _aidx _abody)))
                        type
                        up)
          distributor (fn [v]
                        (&/fold (fn [_abody ena]
                                  (|let [[_aenv _aidx _avar] ena]
                                    (&/$UnivQ _aenv _abody)))
                                v
                                up))]
      (return (&type/Tuple$ (&/|map distributor
                                    (&type/flatten-prod =type)))))

    (&/$Sum ?left ?right)
    (let [=type (&/fold (fn [_abody ena]
                          (|let [[_aenv _aidx (&/$Var _avar)] ena]
                            (clean! 0 _avar _aidx _abody)))
                        type
                        up)
          distributor (fn [v]
                        (&/fold (fn [_abody ena]
                                  (|let [[_aenv _aidx _avar] ena]
                                    (&/$UnivQ _aenv _abody)))
                                v
                                up))]
      (return (&type/Variant$ (&/|map distributor
                                      (&type/flatten-sum =type)))))

    (&/$Apply ?targ ?tfun)
    (|do [=type (apply-type! ?tfun ?targ)]
      (adjust-type* up =type))

    (&/$Var ?id)
    (|do [type* (&/try-all% (&/|list (&type/deref ?id)
                                     (&/fail-with-loc (str "##2##: " ?id))))]
      (adjust-type* up type*))

    (&/$Named ?name ?type)
    (adjust-type* up ?type)

    _
    (&/fail-with-loc (str "[Pattern-matching Error] Cannot pattern-match against type: " (&type/show-type type)))
    ))

(defn adjust-type
  "(-> Type (Lux Type))"
  [type]
  (adjust-type* &/$End type))

(defn analyse-tuple-pattern [analyse-pattern pattern value-type ?members kont]
  (|do [must-infer? (&type/unknown? value-type)
        value-type* (if must-infer?
                      (|do [member-types (&/map% (fn [_] &type/create-var+) (&/|range (&/|length ?members)))]
                        (return (&type/fold-prod member-types)))
                      (adjust-type value-type))]
    (|case value-type*
      (&/$Product _)
      (|let [num-elems (&/|length ?members)
             [_shorter _tuple-types] (&type/tuple-types-for (&/|length ?members) value-type*)]
        (if (= num-elems _shorter)
          (|do [[=tests =kont] (&/fold (fn [kont* vm]
                                         (|let [[v m] vm]
                                           (|do [[=test [=tests =kont]] (analyse-pattern &/$None v m kont*)]
                                             (return (&/T [(&/$Item =test =tests) =kont])))))
                                       (|do [=kont kont]
                                         (return (&/T [&/$End =kont])))
                                       (&/|reverse (&/zip2 _tuple-types ?members)))]
            (return (&/T [($TupleTestAC =tests) =kont])))
          (&/fail-with-loc (str "[Pattern-matching Error] Pattern-matching mismatch. Requires tuple[" (&/|length (&type/flatten-prod value-type*)) "]. Given tuple [" (&/|length ?members) "].\n"
                                "           At: " (&/show-ast pattern) "\n"
                                "Expected type: " (&type/show-type value-type*) "\n"
                                "  Actual type: " (&type/show-type value-type)))))

      _
      (&/fail-with-loc (str "[Pattern-matching Error] Tuples require tuple-types: " (&type/show-type value-type))))))

(defn ^:private analyse-pattern [var?? value-type pattern kont]
  (|let [[meta pattern*] pattern]
    (|case pattern*
      (&/$Identifier "" name)
      (|case var??
        (&/$Some var-analysis)
        (|do [=kont (&env/with-alias name var-analysis
                      kont)]
          (return (&/T [$NoTestAC =kont])))

        _
        (|do [=kont (&env/with-local name value-type
                      kont)
              idx &env/next-local-idx]
          (return (&/T [($StoreTestAC idx) =kont]))))
      
      (&/$Identifier ident)
      (&/fail-with-loc (str "[Pattern-matching Error] Identifiers must be unqualified: " (&/ident->text ident)))

      (&/$Bit ?value)
      (|do [_ (&type/check value-type &type/Bit)
            =kont kont]
        (return (&/T [($BitTestAC ?value) =kont])))

      (&/$Nat ?value)
      (|do [_ (&type/check value-type &type/Nat)
            =kont kont]
        (return (&/T [($NatTestAC ?value) =kont])))

      (&/$Int ?value)
      (|do [_ (&type/check value-type &type/Int)
            =kont kont]
        (return (&/T [($IntTestAC ?value) =kont])))

      (&/$Rev ?value)
      (|do [_ (&type/check value-type &type/Rev)
            =kont kont]
        (return (&/T [($RevTestAC ?value) =kont])))
      
      (&/$Frac ?value)
      (|do [_ (&type/check value-type &type/Frac)
            =kont kont]
        (return (&/T [($FracTestAC ?value) =kont])))

      (&/$Text ?value)
      (|do [_ (&type/check value-type &type/Text)
            =kont kont]
        (return (&/T [($TextTestAC ?value) =kont])))

      (&/$Tuple (&/$End))
      (|do [_ (&type/check value-type &type/Any)
            =kont kont]
        (return (&/T [($TupleTestAC (&/|list)) =kont])))

      (&/$Tuple (&/$Item ?member (&/$End)))
      (analyse-pattern var?? value-type ?member kont)

      (&/$Tuple ?members)
      (|do [rec-members&rec-type (&&record/order-record true ?members)]
        (|case rec-members&rec-type
          (&/$Some [rec-members rec-type])
          (|do [must-infer? (&type/unknown? value-type)
                rec-type* (if must-infer?
                            (&type/instantiate-inference rec-type)
                            (return value-type))
                _ (&type/check value-type rec-type*)]
            (|case rec-members
              (&/$Item singleton (&/$End))
              (analyse-pattern &/$None rec-type* singleton kont)

              _
              (analyse-tuple-pattern analyse-pattern pattern rec-type* rec-members kont)))

          (&/$None)
          (analyse-tuple-pattern analyse-pattern pattern value-type ?members kont)))

      (&/$Variant (&/$Item [_ (&/$Nat idx)] (&/$Item [_ (&/$Bit right?)] ?values)))
      (let [idx (if right? (inc idx) idx)]
        (|do [value-type* (adjust-type value-type)
              case-type (&type/sum-at idx value-type*)
              [=test =kont] (case (int (&/|length ?values))
                              0 (analyse-pattern &/$None case-type unit-tuple kont)
                              1 (analyse-pattern &/$None case-type (&/|head ?values) kont)
                              ;; 1+
                              (analyse-pattern &/$None case-type (&/T [(&/T ["" -1 -1]) (&/$Tuple ?values)]) kont))]
          (return (&/T [($VariantTestAC (&/T [idx (&/|length (&type/flatten-sum value-type*)) =test])) =kont]))))

      (&/$Variant (&/$Item [_ (&/$Identifier ?ident)] ?values))
      (|do [[=module =name] (&&/resolved-ident ?ident)
            must-infer? (&type/unknown? value-type)
            [_exported? variant-type** group idx] (&module/find-tag =module =name)
            variant-type (if must-infer?
                           (|do [variant-type* (&type/instantiate-inference variant-type**)
                                 _ (&type/check value-type variant-type*)]
                             (return variant-type*))
                           (return value-type))
            value-type* (adjust-type variant-type)
            case-type (&type/sum-at idx value-type*)
            [=test =kont] (case (int (&/|length ?values))
                            0 (analyse-pattern &/$None case-type unit-tuple kont)
                            1 (analyse-pattern &/$None case-type (&/|head ?values) kont)
                            ;; 1+
                            (analyse-pattern &/$None case-type (&/T [(&/T ["" -1 -1]) (&/$Tuple ?values)]) kont))]
        (return (&/T [($VariantTestAC (&/T [idx (&/|length group) =test])) =kont])))

      _
      (&/fail-with-loc (str "[Pattern-matching Error] Unrecognized pattern syntax: " (&/show-ast pattern)))
      )))

(defn ^:private analyse-branch [analyse exo-type var?? value-type pattern body patterns]
  (|do [pattern+body (analyse-pattern var?? value-type pattern
                                      (&&/analyse-1 analyse exo-type body))]
    (return (&/$Item pattern+body patterns))))

(defn ^:private merge-total [struct test+body]
  (|let [[test ?body] test+body]
    (|case [struct test]
      [($DefaultTotal total?) ($NoTestAC)]
      (return ($DefaultTotal true))

      [($BitTotal total? ?values) ($NoTestAC)]
      (return ($BitTotal true ?values))

      [($NatTotal total? ?values) ($NoTestAC)]
      (return ($NatTotal true ?values))

      [($IntTotal total? ?values) ($NoTestAC)]
      (return ($IntTotal true ?values))

      [($RevTotal total? ?values) ($NoTestAC)]
      (return ($RevTotal true ?values))

      [($FracTotal total? ?values) ($NoTestAC)]
      (return ($FracTotal true ?values))

      [($TextTotal total? ?values) ($NoTestAC)]
      (return ($TextTotal true ?values))

      [($TupleTotal total? ?values) ($NoTestAC)]
      (return ($TupleTotal true ?values))

      [($VariantTotal total? ?values) ($NoTestAC)]
      (return ($VariantTotal true ?values))

      [($DefaultTotal total?) ($StoreTestAC ?idx)]
      (return ($DefaultTotal true))

      [($BitTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($BitTotal true ?values))

      [($NatTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($NatTotal true ?values))

      [($IntTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($IntTotal true ?values))

      [($RevTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($RevTotal true ?values))

      [($FracTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($FracTotal true ?values))

      [($TextTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($TextTotal true ?values))

      [($TupleTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($TupleTotal true ?values))

      [($VariantTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($VariantTotal true ?values))

      [($DefaultTotal total?) ($BitTestAC ?value)]
      (return ($BitTotal total? (&/|list ?value)))

      [($BitTotal total? ?values) ($BitTestAC ?value)]
      (return ($BitTotal total? (&/$Item ?value ?values)))

      [($DefaultTotal total?) ($NatTestAC ?value)]
      (return ($NatTotal total? (&/|list ?value)))

      [($NatTotal total? ?values) ($NatTestAC ?value)]
      (return ($NatTotal total? (&/$Item ?value ?values)))

      [($DefaultTotal total?) ($IntTestAC ?value)]
      (return ($IntTotal total? (&/|list ?value)))

      [($IntTotal total? ?values) ($IntTestAC ?value)]
      (return ($IntTotal total? (&/$Item ?value ?values)))

      [($DefaultTotal total?) ($RevTestAC ?value)]
      (return ($RevTotal total? (&/|list ?value)))

      [($RevTotal total? ?values) ($RevTestAC ?value)]
      (return ($RevTotal total? (&/$Item ?value ?values)))

      [($DefaultTotal total?) ($FracTestAC ?value)]
      (return ($FracTotal total? (&/|list ?value)))

      [($FracTotal total? ?values) ($FracTestAC ?value)]
      (return ($FracTotal total? (&/$Item ?value ?values)))

      [($DefaultTotal total?) ($TextTestAC ?value)]
      (return ($TextTotal total? (&/|list ?value)))

      [($TextTotal total? ?values) ($TextTestAC ?value)]
      (return ($TextTotal total? (&/$Item ?value ?values)))

      [($DefaultTotal total?) ($TupleTestAC ?tests)]
      (|do [structs (&/map% (fn [t]
                              (merge-total ($DefaultTotal total?) (&/T [t ?body])))
                            ?tests)]
        (return ($TupleTotal total? structs)))

      [($TupleTotal total? ?values) ($TupleTestAC ?tests)]
      (if (.equals ^Object (&/|length ?values) (&/|length ?tests))
        (|do [structs (&/map2% (fn [v t]
                                 (merge-total v (&/T [t ?body])))
                               ?values ?tests)]
          (return ($TupleTotal total? structs)))
        (&/fail-with-loc (str "[Pattern-matching Error] Inconsistent tuple-size.\n"
                              "Expected: " (&/|length ?values) "\n"
                              "  Actual: " (&/|length ?tests))))

      [($DefaultTotal total?) ($VariantTestAC ?tag ?count ?test)]
      (|do [sub-struct (merge-total ($DefaultTotal total?)
                                    (&/T [?test ?body]))
            structs (|case (&/|list-put ?tag sub-struct (&/|repeat ?count ($DefaultTotal total?)))
                      (&/$Some list)
                      (return list)

                      (&/$None)
                      (assert false))]
        (return ($VariantTotal total? structs)))

      [($VariantTotal total? ?branches) ($VariantTestAC ?tag ?count ?test)]
      (|do [sub-struct (merge-total (|case (&/|at ?tag ?branches)
                                      (&/$Some sub)
                                      sub
                                      
                                      (&/$None)
                                      ($DefaultTotal total?))
                                    (&/T [?test ?body]))
            structs (|case (&/|list-put ?tag sub-struct ?branches)
                      (&/$Some list)
                      (return list)

                      (&/$None)
                      (assert false))]
        (return ($VariantTotal total? structs)))
      )))

(defn check-totality+ [check-totality]
  (fn [?token]
    (&type/with-var
      (fn [$var]
        (|do [=output (check-totality $var ?token)
              ?type (&type/deref+ $var)
              =type (&type/clean $var ?type)]
          (return (&/T [=output =type])))))))

(defn ^:private check-totality [value-type struct]
  (|case struct
    ($DefaultTotal ?total)
    (return ?total)

    ($BitTotal ?total ?values)
    (|do [_ (&type/check value-type &type/Bit)]
      (return (or ?total
                  (= #{true false} (set (&/->seq ?values))))))

    ($NatTotal ?total _)
    (|do [_ (&type/check value-type &type/Nat)]
      (return ?total))

    ($IntTotal ?total _)
    (|do [_ (&type/check value-type &type/Int)]
      (return ?total))

    ($RevTotal ?total _)
    (|do [_ (&type/check value-type &type/Rev)]
      (return ?total))
    
    ($FracTotal ?total _)
    (|do [_ (&type/check value-type &type/Frac)]
      (return ?total))

    ($TextTotal ?total _)
    (|do [_ (&type/check value-type &type/Text)]
      (return ?total))

    ($TupleTotal ?total ?structs)
    (|case ?structs
      (&/$End)
      (|do [value-type* (resolve-type value-type)]
        (if (&type/type= &type/Any value-type*)
          (return true)
          (&/fail-with-loc "[Pattern-maching Error] Unit is not total.")))
      
      _
      (|do [unknown? (&type/unknown? value-type)]
        (if unknown?
          (|do [=structs (&/map% (check-totality+ check-totality) ?structs)
                _ (&type/check value-type (|case (->> (&/|map &/|second =structs) (&/|reverse))
                                            (&/$Item last prevs)
                                            (&/fold (fn [right left] (&/$Product left right))
                                                    last prevs)))]
            (return (or ?total
                        (&/fold #(and %1 %2) true (&/|map &/|first =structs)))))
          (if ?total
            (return true)
            (|do [value-type* (resolve-type value-type)]
              (|case value-type*
                (&/$Product _)
                (|let [num-elems (&/|length ?structs)
                       [_shorter _tuple-types] (&type/tuple-types-for (&/|length ?structs) value-type*)
                       _ (&/assert! (= num-elems _shorter)
                                    (&/fail-with-loc (str "[Pattern-maching Error] Tuple-mismatch. Require tuple[" (&/|length (&type/flatten-prod value-type*)) "]. Given tuple [" (&/|length ?structs) "]")))]
                  (|do [totals (&/map2% check-totality _tuple-types ?structs)]
                    (return (&/fold #(and %1 %2) true totals))))
                
                _
                (&/fail-with-loc (str "[Pattern-maching Error] Tuple is not total." " - " (&type/show-type value-type*)))))))))

    ($VariantTotal ?total ?structs)
    (if ?total
      (return true)
      (|do [value-type* (resolve-type value-type)]
        (|case value-type*
          (&/$Sum _)
          (|do [totals (&/map2% check-totality
                                (&type/flatten-sum value-type*)
                                ?structs)]
            (return (&/fold #(and %1 %2) true totals)))

          _
          (&/fail-with-loc "[Pattern-maching Error] Variant is not total."))))
    ))

;; [Exports]
(defn analyse-branches [analyse exo-type var?? value-type branches]
  (|do [patterns (&/fold% (fn [patterns branch]
                            (|let [[pattern body] branch]
                              (analyse-branch analyse exo-type var?? value-type pattern body patterns)))
                          &/$End
                          branches)
        struct (&/fold% merge-total ($DefaultTotal false) patterns)
        ? (check-totality value-type struct)
        _ (&/assert! ? "[Pattern-maching Error] Pattern-matching is not total.")]
    (return patterns)))
