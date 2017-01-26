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
  ("BoolTotal" 2)
  ("NatTotal" 2)
  ("IntTotal" 2)
  ("DegTotal" 2)
  ("RealTotal" 2)
  ("CharTotal" 2)
  ("TextTotal" 2)
  ("TupleTotal" 2)
  ("VariantTotal" 2))

(defvariant
  ("NoTestAC" 0)
  ("StoreTestAC" 1)
  ("BoolTestAC" 1)
  ("NatTestAC" 1)
  ("IntTestAC" 1)
  ("DegTestAC" 1)
  ("RealTestAC" 1)
  ("CharTestAC" 1)
  ("TextTestAC" 1)
  ("TupleTestAC" 1)
  ("VariantTestAC" 1))

;; [Utils]
(def ^:private unit-tuple
  (&/T [(&/T ["" -1 -1]) (&/$TupleS &/$Nil)]))

(defn ^:private resolve-type [type]
  (|case type
    (&/$VarT ?id)
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
    (&type/actual-type type)))

(defn update-up-frame [frame]
  (|let [[_env _idx _var] frame]
    (&/T [_env (+ 2 _idx) _var])))

(defn clean! [level ?tid bound-idx type]
  (|case type
    (&/$VarT ?id)
    (if (= ?tid ?id)
      (&/$BoundT (+ (* 2 level) bound-idx))
      type)

    (&/$HostT ?name ?params)
    (&/$HostT ?name (&/|map (partial clean! level ?tid bound-idx)
                            ?params))
    
    (&/$LambdaT ?arg ?return)
    (&/$LambdaT (clean! level ?tid bound-idx ?arg)
                (clean! level ?tid bound-idx ?return))

    (&/$AppT ?lambda ?param)
    (&/$AppT (clean! level ?tid bound-idx ?lambda)
             (clean! level ?tid bound-idx ?param))

    (&/$ProdT ?left ?right)
    (&/$ProdT (clean! level ?tid bound-idx ?left)
              (clean! level ?tid bound-idx ?right))
    
    (&/$SumT ?left ?right)
    (&/$SumT (clean! level ?tid bound-idx ?left)
             (clean! level ?tid bound-idx ?right))

    (&/$UnivQ ?env ?body)
    (&/$UnivQ (&/|map (partial clean! level ?tid bound-idx) ?env)
              (clean! (inc level) ?tid bound-idx ?body))

    (&/$ExQ ?env ?body)
    (&/$ExQ (&/|map (partial clean! level ?tid bound-idx) ?env)
            (clean! (inc level) ?tid bound-idx ?body))

    _
    type
    ))

(defn beta-reduce! [level env type]
  (|case type
    (&/$HostT ?name ?params)
    (&/$HostT ?name (&/|map (partial beta-reduce! level env) ?params))

    (&/$SumT ?left ?right)
    (&/$SumT (beta-reduce! level env ?left)
             (beta-reduce! level env ?right))

    (&/$ProdT ?left ?right)
    (&/$ProdT (beta-reduce! level env ?left)
              (beta-reduce! level env ?right))

    (&/$AppT ?type-fn ?type-arg)
    (&/$AppT (beta-reduce! level env ?type-fn)
             (beta-reduce! level env ?type-arg))
    
    (&/$UnivQ ?local-env ?local-def)
    (|case ?local-env
      (&/$Nil)
      (&/$UnivQ ?local-env (beta-reduce! (inc level) env ?local-def))

      _
      type)

    (&/$ExQ ?local-env ?local-def)
    (|case ?local-env
      (&/$Nil)
      (&/$ExQ ?local-env (beta-reduce! (inc level) env ?local-def))

      _
      type)

    (&/$LambdaT ?input ?output)
    (&/$LambdaT (beta-reduce! level env ?input)
                (beta-reduce! level env ?output))

    (&/$BoundT ?idx)
    (|case (&/|at (- ?idx (* 2 level)) env)
      (&/$Some bound)
      (beta-reduce! level env bound)

      _
      type)

    _
    type
    ))

(defn apply-type! [type-fn param]
  (|case type-fn
    (&/$UnivQ local-env local-def)
    (return (beta-reduce! 0 (->> local-env
                                 (&/$Cons param)
                                 (&/$Cons type-fn))
                          local-def))

    (&/$ExQ local-env local-def)
    (return (beta-reduce! 0 (->> local-env
                                 (&/$Cons param)
                                 (&/$Cons type-fn))
                          local-def))

    (&/$AppT F A)
    (|do [type-fn* (apply-type! F A)]
      (apply-type! type-fn* param))

    (&/$NamedT ?name ?type)
    (apply-type! ?type param)

    (&/$ExT id)
    (return (&/$AppT type-fn param))

    (&/$VarT id)
    (|do [=type-fun (deref id)]
      (apply-type! =type-fun param))
    
    _
    (&/fail-with-loc (str "[Type System] Not a type function:\n" (&type/show-type type-fn) "\n"))))

(defn adjust-type* [up type]
  "(-> (List (, (Maybe (List Type)) Int Type)) Type (Lux Type))"
  (|case type
    (&/$UnivQ _aenv _abody)
    (&type/with-var
      (fn [$var]
        (|do [=type (apply-type! type $var)
              ==type (adjust-type* (&/$Cons (&/T [_aenv 1 $var])
                                            (&/|map update-up-frame up))
                                   =type)]
          (&type/clean $var ==type))))

    (&/$ExQ _aenv _abody)
    (|do [$var &type/existential
          =type (apply-type! type $var)]
      (adjust-type* up =type))

    (&/$ProdT ?left ?right)
    (let [=type (&/fold (fn [_abody ena]
                          (|let [[_aenv _aidx (&/$VarT _avar)] ena]
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

    (&/$SumT ?left ?right)
    (let [=type (&/fold (fn [_abody ena]
                          (|let [[_aenv _aidx (&/$VarT _avar)] ena]
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

    (&/$AppT ?tfun ?targ)
    (|do [=type (apply-type! ?tfun ?targ)]
      (adjust-type* up =type))

    (&/$VarT ?id)
    (|do [type* (&/try-all% (&/|list (&type/deref ?id)
                                     (&/fail-with-loc (str "##2##: " ?id))))]
      (adjust-type* up type*))

    (&/$NamedT ?name ?type)
    (adjust-type* up ?type)

    (&/$UnitT)
    (return type)

    _
    (&/fail-with-loc (str "[Pattern-matching Error] Can't adjust type: " (&type/show-type type)))
    ))

(defn adjust-type [type]
  "(-> Type (Lux Type))"
  (adjust-type* &/$Nil type))

(defn ^:private analyse-pattern [var?? value-type pattern kont]
  (|let [[meta pattern*] pattern]
    (|case pattern*
      (&/$SymbolS "" name)
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
      
      (&/$SymbolS ident)
      (&/fail-with-loc (str "[Pattern-matching Error] Symbols must be unqualified: " (&/ident->text ident)))

      (&/$BoolS ?value)
      (|do [_ (&type/check value-type &type/Bool)
            =kont kont]
        (return (&/T [($BoolTestAC ?value) =kont])))

      (&/$NatS ?value)
      (|do [_ (&type/check value-type &type/Nat)
            =kont kont]
        (return (&/T [($NatTestAC ?value) =kont])))

      (&/$IntS ?value)
      (|do [_ (&type/check value-type &type/Int)
            =kont kont]
        (return (&/T [($IntTestAC ?value) =kont])))

      (&/$DegS ?value)
      (|do [_ (&type/check value-type &type/Deg)
            =kont kont]
        (return (&/T [($DegTestAC ?value) =kont])))
      
      (&/$RealS ?value)
      (|do [_ (&type/check value-type &type/Real)
            =kont kont]
        (return (&/T [($RealTestAC ?value) =kont])))

      (&/$CharS ?value)
      (|do [_ (&type/check value-type &type/Char)
            =kont kont]
        (return (&/T [($CharTestAC ?value) =kont])))

      (&/$TextS ?value)
      (|do [_ (&type/check value-type &type/Text)
            =kont kont]
        (return (&/T [($TextTestAC ?value) =kont])))

      (&/$TupleS ?members)
      (|case ?members
        (&/$Nil)
        (|do [_ (&type/check value-type &/$UnitT)
              =kont kont]
          (return (&/T [($TupleTestAC (&/|list)) =kont])))

        (&/$Cons ?member (&/$Nil))
        (analyse-pattern var?? value-type ?member kont)

        _
        (|do [must-infer? (&type/unknown? value-type)
              value-type* (if must-infer?
                            (|do [member-types (&/map% (fn [_] &type/create-var+) (&/|range (&/|length ?members)))]
                              (return (&type/fold-prod member-types)))
                            (adjust-type value-type))]
          (|case value-type*
            (&/$ProdT _)
            (|let [num-elems (&/|length ?members)
                   [_shorter _tuple-types] (&type/tuple-types-for (&/|length ?members) value-type*)]
              (if (= num-elems _shorter)
                (|do [[=tests =kont] (&/fold (fn [kont* vm]
                                               (|let [[v m] vm]
                                                 (|do [[=test [=tests =kont]] (analyse-pattern &/$None v m kont*)]
                                                   (return (&/T [(&/$Cons =test =tests) =kont])))))
                                             (|do [=kont kont]
                                               (return (&/T [&/$Nil =kont])))
                                             (&/|reverse (&/zip2 _tuple-types ?members)))]
                  (return (&/T [($TupleTestAC =tests) =kont])))
                (&/fail-with-loc (str "[Pattern-matching Error] Pattern-matching mismatch. Require tuple[" (&/|length (&type/flatten-prod value-type*)) "]. Given tuple [" (&/|length ?members) "]"
                                      " -- " (&/show-ast pattern)
                                      " " (&type/show-type value-type*) " " (&type/show-type value-type)))))

            _
            (&/fail-with-loc (str "[Pattern-matching Error] Tuples require tuple-types: " (&type/show-type value-type))))))

      (&/$RecordS pairs)
      (|do [[rec-members rec-type] (&&record/order-record pairs)
            must-infer? (&type/unknown? value-type)
            rec-type* (if must-infer?
                        (&type/instantiate-inference rec-type)
                        (return value-type))
            _ (&type/check value-type rec-type*)]
        (analyse-pattern &/$None rec-type* (&/T [meta (&/$TupleS rec-members)]) kont))

      (&/$TagS ?ident)
      (|do [[=module =name] (&&/resolved-ident ?ident)
            must-infer? (&type/unknown? value-type)
            variant-type (if must-infer?
                           (|do [variant-type (&module/tag-type =module =name)
                                 variant-type* (&type/instantiate-inference variant-type)
                                 _ (&type/check value-type variant-type*)]
                             (return variant-type*))
                           (return value-type))
            value-type* (adjust-type variant-type)
            idx (&module/tag-index =module =name)
            group (&module/tag-group =module =name)
            case-type (&type/sum-at idx value-type*)
            [=test =kont] (analyse-pattern &/$None case-type unit-tuple kont)]
        (return (&/T [($VariantTestAC (&/T [idx (&/|length group) =test])) =kont])))

      (&/$FormS (&/$Cons [_ (&/$NatS idx)] ?values))
      (|do [value-type* (adjust-type value-type)
            case-type (&type/sum-at idx value-type*)
            [=test =kont] (case (int (&/|length ?values))
                            0 (analyse-pattern &/$None case-type unit-tuple kont)
                            1 (analyse-pattern &/$None case-type (&/|head ?values) kont)
                            ;; 1+
                            (analyse-pattern &/$None case-type (&/T [(&/T ["" -1 -1]) (&/$TupleS ?values)]) kont))]
        (return (&/T [($VariantTestAC (&/T [idx (&/|length (&type/flatten-sum value-type*)) =test])) =kont])))

      (&/$FormS (&/$Cons [_ (&/$TagS ?ident)] ?values))
      (|do [[=module =name] (&&/resolved-ident ?ident)
            must-infer? (&type/unknown? value-type)
            variant-type (if must-infer?
                           (|do [variant-type (&module/tag-type =module =name)
                                 variant-type* (&type/instantiate-inference variant-type)
                                 _ (&type/check value-type variant-type*)]
                             (return variant-type*))
                           (return value-type))
            value-type* (adjust-type variant-type)
            idx (&module/tag-index =module =name)
            group (&module/tag-group =module =name)
            case-type (&type/sum-at idx value-type*)
            [=test =kont] (case (int (&/|length ?values))
                            0 (analyse-pattern &/$None case-type unit-tuple kont)
                            1 (analyse-pattern &/$None case-type (&/|head ?values) kont)
                            ;; 1+
                            (analyse-pattern &/$None case-type (&/T [(&/T ["" -1 -1]) (&/$TupleS ?values)]) kont))]
        (return (&/T [($VariantTestAC (&/T [idx (&/|length group) =test])) =kont])))

      _
      (&/fail-with-loc (str "[Pattern-matching Error] Unrecognized pattern syntax: " (&/show-ast pattern)))
      )))

(defn ^:private analyse-branch [analyse exo-type var?? value-type pattern body patterns]
  (|do [pattern+body (analyse-pattern var?? value-type pattern
                                      (&&/analyse-1 analyse exo-type body))]
    (return (&/$Cons pattern+body patterns))))

(defn ^:private merge-total [struct test+body]
  (|let [[test ?body] test+body]
    (|case [struct test]
      [($DefaultTotal total?) ($NoTestAC)]
      (return ($DefaultTotal true))

      [($BoolTotal total? ?values) ($NoTestAC)]
      (return ($BoolTotal true ?values))

      [($NatTotal total? ?values) ($NoTestAC)]
      (return ($NatTotal true ?values))

      [($IntTotal total? ?values) ($NoTestAC)]
      (return ($IntTotal true ?values))

      [($DegTotal total? ?values) ($NoTestAC)]
      (return ($DegTotal true ?values))

      [($RealTotal total? ?values) ($NoTestAC)]
      (return ($RealTotal true ?values))

      [($CharTotal total? ?values) ($NoTestAC)]
      (return ($CharTotal true ?values))

      [($TextTotal total? ?values) ($NoTestAC)]
      (return ($TextTotal true ?values))

      [($TupleTotal total? ?values) ($NoTestAC)]
      (return ($TupleTotal true ?values))

      [($VariantTotal total? ?values) ($NoTestAC)]
      (return ($VariantTotal true ?values))

      [($DefaultTotal total?) ($StoreTestAC ?idx)]
      (return ($DefaultTotal true))

      [($BoolTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($BoolTotal true ?values))

      [($NatTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($NatTotal true ?values))

      [($IntTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($IntTotal true ?values))

      [($DegTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($DegTotal true ?values))

      [($RealTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($RealTotal true ?values))

      [($CharTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($CharTotal true ?values))

      [($TextTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($TextTotal true ?values))

      [($TupleTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($TupleTotal true ?values))

      [($VariantTotal total? ?values) ($StoreTestAC ?idx)]
      (return ($VariantTotal true ?values))

      [($DefaultTotal total?) ($BoolTestAC ?value)]
      (return ($BoolTotal total? (&/|list ?value)))

      [($BoolTotal total? ?values) ($BoolTestAC ?value)]
      (return ($BoolTotal total? (&/$Cons ?value ?values)))

      [($DefaultTotal total?) ($NatTestAC ?value)]
      (return ($NatTotal total? (&/|list ?value)))

      [($NatTotal total? ?values) ($NatTestAC ?value)]
      (return ($NatTotal total? (&/$Cons ?value ?values)))

      [($DefaultTotal total?) ($IntTestAC ?value)]
      (return ($IntTotal total? (&/|list ?value)))

      [($IntTotal total? ?values) ($IntTestAC ?value)]
      (return ($IntTotal total? (&/$Cons ?value ?values)))

      [($DefaultTotal total?) ($DegTestAC ?value)]
      (return ($DegTotal total? (&/|list ?value)))

      [($DegTotal total? ?values) ($DegTestAC ?value)]
      (return ($DegTotal total? (&/$Cons ?value ?values)))

      [($DefaultTotal total?) ($RealTestAC ?value)]
      (return ($RealTotal total? (&/|list ?value)))

      [($RealTotal total? ?values) ($RealTestAC ?value)]
      (return ($RealTotal total? (&/$Cons ?value ?values)))

      [($DefaultTotal total?) ($CharTestAC ?value)]
      (return ($CharTotal total? (&/|list ?value)))

      [($CharTotal total? ?values) ($CharTestAC ?value)]
      (return ($CharTotal total? (&/$Cons ?value ?values)))

      [($DefaultTotal total?) ($TextTestAC ?value)]
      (return ($TextTotal total? (&/|list ?value)))

      [($TextTotal total? ?values) ($TextTestAC ?value)]
      (return ($TextTotal total? (&/$Cons ?value ?values)))

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
        (&/fail-with-loc "[Pattern-matching Error] Inconsistent tuple-size."))

      [($DefaultTotal total?) ($VariantTestAC ?tag ?count ?test)]
      (|do [sub-struct (merge-total ($DefaultTotal total?)
                                    (&/T [?test ?body]))
            structs (|case (&/|list-put ?tag sub-struct (&/|repeat ?count ($DefaultTotal total?)))
                      (&/$Some list)
                      (return list)

                      (&/$None)
                      (&/fail-with-loc "[Pattern-matching Error] YOLO"))]
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
                      (&/fail-with-loc "[Pattern-matching Error] YOLO"))]
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

    ($BoolTotal ?total ?values)
    (|do [_ (&type/check value-type &type/Bool)]
      (return (or ?total
                  (= #{true false} (set (&/->seq ?values))))))

    ($NatTotal ?total _)
    (|do [_ (&type/check value-type &type/Nat)]
      (return ?total))

    ($IntTotal ?total _)
    (|do [_ (&type/check value-type &type/Int)]
      (return ?total))

    ($DegTotal ?total _)
    (|do [_ (&type/check value-type &type/Deg)]
      (return ?total))
    
    ($RealTotal ?total _)
    (|do [_ (&type/check value-type &type/Real)]
      (return ?total))

    ($CharTotal ?total _)
    (|do [_ (&type/check value-type &type/Char)]
      (return ?total))

    ($TextTotal ?total _)
    (|do [_ (&type/check value-type &type/Text)]
      (return ?total))

    ($TupleTotal ?total ?structs)
    (|case ?structs
      (&/$Nil)
      (|do [value-type* (resolve-type value-type)]
        (|case value-type*
          (&/$UnitT)
          (return true)

          _
          (&/fail-with-loc "[Pattern-maching Error] Unit is not total.")))
      
      _
      (|do [unknown? (&type/unknown? value-type)]
        (if unknown?
          (|do [=structs (&/map% (check-totality+ check-totality) ?structs)
                _ (&type/check value-type (|case (->> (&/|map &/|second =structs) (&/|reverse))
                                            (&/$Cons last prevs)
                                            (&/fold (fn [right left] (&/$ProdT left right))
                                                    last prevs)))]
            (return (or ?total
                        (&/fold #(and %1 %2) true (&/|map &/|first =structs)))))
          (if ?total
            (return true)
            (|do [value-type* (resolve-type value-type)]
              (|case value-type*
                (&/$ProdT _)
                (|let [num-elems (&/|length ?structs)
                       [_shorter _tuple-types] (&type/tuple-types-for (&/|length ?structs) value-type*)]
                  (if (= num-elems _shorter)
                    (|do [totals (&/map2% check-totality _tuple-types ?structs)]
                      (return (&/fold #(and %1 %2) true totals)))
                    (&/fail-with-loc (str "[Pattern-maching Error] Tuple-mismatch. Require tuple[" (&/|length (&type/flatten-prod value-type*)) "]. Given tuple [" (&/|length ?structs) "]"))))
                
                _
                (&/fail-with-loc (str "[Pattern-maching Error] Tuple is not total." " - " (&type/show-type value-type*)))))))))

    ($VariantTotal ?total ?structs)
    (if ?total
      (return true)
      (|do [value-type* (resolve-type value-type)]
        (|case value-type*
          (&/$SumT _)
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
                          &/$Nil
                          branches)
        struct (&/fold% merge-total ($DefaultTotal false) patterns)
        ? (check-totality value-type struct)]
    (if ?
      (return patterns)
      (&/fail-with-loc "[Pattern-maching Error] Pattern-matching is non-total."))))
