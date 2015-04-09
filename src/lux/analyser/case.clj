(ns lux.analyser.case
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return fail |let]]
                 [parser :as &parser]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [env :as &env])))

;; [Utils]
(defn ^:private analyse-variant [analyse-pattern idx value-type tag value]
  (|do [=var &type/fresh-var
        _ (&type/check value-type (&/V "lux;VariantT" (&/|list (&/T tag =var))))
        [idx* test] (analyse-pattern idx =var value)]
    (return (&/T idx* (&/V "VariantTestAC" (&/T tag test))))))

(defn ^:private analyse-pattern [idx value-type pattern]
  (prn 'analyse-pattern/pattern (aget pattern 0) (aget pattern 1) (alength (aget pattern 1)))
  (matchv ::M/objects [pattern]
    [["lux;Meta" [_ pattern*]]]
    ;; (assert false)
    (do (prn 'analyse-pattern/pattern* (aget pattern* 0))
      (when (= "lux;Form" (aget pattern* 0))
        (prn 'analyse-pattern/_2 (aget pattern* 1 0)) ;; "lux;Cons"
        (prn 'analyse-pattern/_2 (aget pattern* 1 1 0 0)) ;; "lux;Meta"
        (prn 'analyse-pattern/_2 (alength (aget pattern* 1 1 0 1)))
        (prn 'analyse-pattern/_2 (aget pattern* 1 1 0 1 1 0)) ;; "lux;Tag"
        (prn 'analyse-pattern/_2 [(aget pattern* 1 1 0 1 1 1 0) (aget pattern* 1 1 0 1 1 1 1)]) ;; ["" "Cons"]
        (prn 'analyse-pattern/_2 (aget pattern* 1 1 1 0)) ;; "lux;Cons"
        (prn 'analyse-pattern/_2 (aget pattern* 1 1 1 1 0)) ;; #<Object[] [Ljava.lang.Object;@63c7c38b>
        (prn 'analyse-pattern/_2 (aget pattern* 1 1 1 1 1 0)) ;; "lux;Nil"
        )
      ;; ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Tag" [?module ?name]]]]
      ;;                          ["lux;Cons" [?value
      ;;                                       ["lux;Nil" _]]]]]]
      (matchv ::M/objects [pattern*]
        [["lux;Symbol" [?module ?name]]]
        (return (&/T (inc idx) (&/V "StoreTestAC" (&/T idx (str ?module ";" ?name) value-type))))

        [["lux;Bool" ?value]]
        (|do [_ (&type/check value-type &type/Bool)]
          (return (&/T idx (&/V "BoolTestAC" ?value))))

        [["lux;Int" ?value]]
        (|do [_ (&type/check value-type &type/Int)]
          (return (&/T idx (&/V "IntTestAC" ?value))))

        [["lux;Real" ?value]]
        (|do [_ (&type/check value-type &type/Real)]
          (return (&/T idx (&/V "RealTestAC" ?value))))

        [["lux;Char" ?value]]
        (|do [_ (&type/check value-type &type/Char)]
          (return (&/T idx (&/V "CharTestAC" ?value))))

        [["lux;Text" ?value]]
        (|do [_ (&type/check value-type &type/Text)]
          (return (&/T idx (&/V "TextTestAC" ?value))))

        [["lux;Tuple" ?members]]
        (|do [=vars (&/map% (fn [_] &type/fresh-var)
                            (&/|range (&/|length ?members)))
              _ (&type/check value-type (&/V "lux;TupleT" =vars))
              [idx* tests] (&/fold% (fn [idx+subs mv]
                                      (|let [[_idx subs] idx+subs
                                             [?member ?var] mv]
                                        (|do [[idx* test] (analyse-pattern _idx ?var ?member)]
                                          (return (&/T idx* (&/|cons test subs))))))
                                    (&/T idx (&/|list))
                                    (&/zip2 ?members =vars))]
          (return (&/T idx* (&/V "TupleTestAC" (&/|reverse tests)))))
        
        [["lux;Record" ?fields]]
        (|do [=vars (&/map% (fn [_] &type/fresh-var)
                            (&/|range (&/|length ?fields)))
              _ (&type/check value-type (&/V "lux;RecordT" (&/zip2 (&/|keys ?fields) =vars)))
              tests (&/fold% (fn [idx+subs mv]
                               (|let [[_idx subs] idx+subs
                                      [[slot value] ?var] mv]
                                 (|do [[idx* test] (analyse-pattern _idx ?var value)]
                                   (return (&/T idx* (&/|cons (&/T slot test) subs))))))
                             (&/T idx (&/|list)) (&/zip2 ?fields =vars))]
          (return (&/V "RecordTestAC" tests)))

        [["lux;Tag" [?module ?name]]]
        (|do [module* (if (= "" ?module)
                        &/get-module-name
                        (return ?module))]
          (analyse-variant analyse-pattern idx value-type (str module* ";" ?name) (&/V "lux;Meta" (&/T (&/T "" -1 -1)
                                                                                                       (&/V "lux;Tuple" (&/|list))))))

        [["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Tag" [?module ?name]]]]
                                  ["lux;Cons" [?value
                                               ["lux;Nil" _]]]]]]]
        (|do [module* (if (= "" ?module)
                        &/get-module-name
                        (return ?module))]
          (analyse-variant analyse-pattern idx value-type (str module* ";" ?name) ?value))
        ))
    ))

(defn ^:private with-test [test body]
  (matchv ::M/objects [test]
    [["StoreTestAC" [?idx ?name ?type]]]
    (&env/with-local ?name ?type
      body)

    [["TupleTestAC" ?tests]]
    (&/fold #(with-test %2 %1) body (&/|reverse ?tests))

    [["RecordTestAC" ?tests]]
    (&/fold #(with-test %2 %1) body (&/|reverse (&/|vals ?tests)))

    [["VariantTestAC" [?tag ?value]]]
    (with-test ?value body)
    
    [_]
    body
    ))

(defn ^:private analyse-branch [analyse exo-type value-type pattern body match]
  (|do [idx &env/next-local-idx
        [idx* =test] (analyse-pattern idx value-type pattern)
        =body (with-test =test
                (&&/analyse-1 analyse exo-type body))]
    (matchv ::M/objects [match]
      [["MatchAC" ?patterns]]
      (return (&/V "MatchAC" (&/|cons (&/T =test =body) ?patterns))))))

(let [compare-kv #(compare (aget %1 0) (aget %2 0))]
  (defn ^:private merge-total [struct test+body]
    (prn 'merge-total (aget struct 0) (class test+body))
    (prn 'merge-total (aget struct 0) (aget test+body 0))
    (prn 'merge-total (aget struct 0) (aget test+body 0 0))
    (matchv ::M/objects [test+body]
      [[test ?body]]
      (matchv ::M/objects [struct test]
        [["DefaultTotal" total?] ["StoreTestAC" [?idx ?name type]]]
        (return (&/V "DefaultTotal" true))

        [[?tag [total? ?values]] ["StoreTestAC" [?idx ?name type]]]
        (return (&/V ?tag (&/T true ?values)))
        
        [["DefaultTotal" total?] ["BoolTestAC" ?value]]
        (return (&/V "BoolTotal" (&/T total? (&/|list ?value))))

        [["BoolTotal" [total? ?values]] ["BoolTestAC" ?value]]
        (return (&/V "BoolTotal" (&/T total? (&/|cons ?value ?values))))

        [["DefaultTotal" total?] ["IntTestAC" ?value]]
        (return (&/V "IntTotal" (&/T total? (&/|list ?value))))

        [["IntTotal" [total? ?values]] ["IntTestAC" ?value]]
        (return (&/V "IntTotal" (&/T total? (&/|cons ?value ?values))))

        [["DefaultTotal" total?] ["RealTestAC" ?value]]
        (return (&/V "RealTotal" (&/T total? (&/|list ?value))))

        [["RealTotal" [total? ?values]] ["RealTestAC" ?value]]
        (return (&/V "RealTotal" (&/T total? (&/|cons ?value ?values))))

        [["DefaultTotal" total?] ["CharTestAC" ?value]]
        (return (&/V "CharTotal" (&/T total? (&/|list ?value))))

        [["CharTotal" [total? ?values]] ["CharTestAC" ?value]]
        (return (&/V "CharTotal" (&/T total? (&/|cons ?value ?values))))

        [["DefaultTotal" total?] ["TextTestAC" ?value]]
        (return (&/V "TextTotal" (&/T total? (&/|list ?value))))

        [["TextTotal" [total? ?values]] ["TextTestAC" ?value]]
        (return (&/V "TextTotal" (&/T total? (&/|cons ?value ?values))))

        [["DefaultTotal" total?] ["TupleTestAC" ?tests]]
        (|do [structs (&/map% (fn [t]
                                (merge-total (&/V "DefaultTotal" total?) (&/T t ?body)))
                              ?tests)]
          (return (&/V "TupleTotal" (&/T total? structs))))

        [["TupleTotal" [total? ?values]] ["TupleTestAC" ?tests]]
        (if (= (&/|length ?values) (&/|length ?tests))
          (|do [structs (&/map% (fn [vt]
                                  (|let [[v t] vt]
                                    (merge-total v (&/T t ?body))))
                                (&/zip2 ?values ?tests))]
            (return (&/V "TupleTotal" (&/T total? structs))))
          (fail "[Pattern-matching error] Inconsistent tuple-size."))

        [["DefaultTotal" total?] ["RecordTestAC" ?tests]]
        (|do [structs (&/map% (fn [t]
                                (|let [[slot value] t]
                                  (|do [struct (merge-total (&/V "DefaultTotal" total?) (&/T value ?body))]
                                    (return (&/T slot struct)))))
                              (sort compare-kv ?tests))]
          (return (&/V "RecordTotal" (&/T total? structs))))

        [["RecordTotal" [total? ?values]] ["RecordTestAC" ?tests]]
        (if (= (&/|length ?values) (&/|length ?tests))
          (|do [structs (&/map% (fn [lr]
                                  (|let [[[lslot struct] [rslot value]] lr]
                                    (if (= lslot rslot)
                                      (|do [struct (merge-total (&/V "DefaultTotal" total?) (&/T value ?body))]
                                        (return (&/T lslot struct)))
                                      (fail "[Pattern-matching error] Record slots mismatch."))))
                                (&/zip2 ?values
                                        (sort compare-kv ?tests)))]
            (return (&/V "RecordTotal" (&/T total? structs))))
          (fail "[Pattern-matching error] Inconsistent record-size."))

        [["DefaultTotal" total?] ["VariantTestAC" [?tag ?test]]]
        (|do [struct (merge-total (&/V "DefaultTotal" total?) (&/T ?test ?body))]
          (return (&/V "VariantTotal" (&/T total? (&/|put ?tag struct (&/|table))))))

        [["VariantTotal" [total? ?branches]] ["VariantTestAC" [?tag ?test]]]
        (|do [struct (merge-total (or (&/|get ?tag ?branches)
                                      (&/V "DefaultTotal" total?))
                                  (&/T ?test ?body))]
          (return (&/V "VariantTotal" (&/T total? (&/|put ?tag struct ?branches)))))
        ))))

(defn ^:private totality-struct [owner-total? match]
  (let [msg "Pattern matching is non-total"]
    (matchv ::M/objects [match]
      [["MatchAC" ?tests]]
      (&/fold% merge-total (&/V "DefaultTotal" false) ?tests))))

(defn ^:private resolve-type [type]
  (matchv ::M/objects [type]
    [["lux;VarT" ?idx]]
    (&type/deref ?idx)

    [_]
    (return type)))

(defn ^:private check-totality [value-type struct]
  (prn 'check-totality (aget value-type 0) (aget struct 0) (&type/show-type value-type))
  (matchv ::M/objects [value-type struct]
    [_ ["BoolTotal" [?total _]]]
    (|do [_ (&type/check value-type &type/Bool)]
      (return ?total))

    [_ ["IntTotal" [?total _]]]
    (|do [_ (&type/check value-type &type/Int)]
      (return ?total))

    [_ ["RealTotal" [?total _]]]
    (|do [_ (&type/check value-type &type/Real)]
      (return ?total))

    [_ ["CharTotal" [?total _]]]
    (|do [_ (&type/check value-type &type/Char)]
      (return ?total))

    [_ ["TextTotal" [?total _]]]
    (|do [_ (&type/check value-type &type/Text)]
      (return ?total))

    [_ ["TupleTotal" [?total ?structs]]]
    (|do [elems-vars (&/map% (constantly &type/fresh-var) (&/|range (&/|length ?structs)))
          _ (&type/check value-type (&/V "lux;TupleT" elems-vars))
          totals (&/map% (fn [sv]
                           (|let [[struct tvar] sv]
                             (check-totality tvar struct)))
                         (&/zip2 ?structs elems-vars))]
      (return (or ?total
                  (every? true? totals))))

    [_ ["RecordTotal" [?total ?structs]]]
    (|do [elems-vars (&/map% (constantly &type/fresh-var) (&/|range (&/|length ?structs)))
          :let [structs+vars (&/zip2 ?structs elems-vars)
                record-type (&/V "lux;RecordT" (&/|map (fn [sv]
                                                         (|let [[[k v] tvar] sv]
                                                           (&/T k tvar)))
                                                       structs+vars))]
          _ (&type/check value-type record-type)
          totals (&/map% (fn [sv]
                           (|let [[[k v] tvar] sv]
                             (check-totality tvar v)))
                         structs+vars)]
      (return (or ?total
                  (every? true? totals))))

    [_ ["VariantTotal" [?total ?structs]]]
    (|do [real-type (resolve-type value-type)]
      (assert false))
    
    [_ ["DefaultTotal" true]]
    (return true)
    ))

;; [Exports]
(defn analyse-branches [analyse exo-type value-type branches]
  (|do [=match (&/fold% (fn [match branch]
                          (|let [[pattern body] branch]
                            (analyse-branch analyse exo-type value-type pattern body match)))
                        (&/V "MatchAC" (&/|list))
                        branches)
        struct (totality-struct false =match)
        ? (check-totality value-type struct)]
    (matchv ::M/objects [=match]
      [["MatchAC" ?tests]]
      (if ?
        (return (&/V "MatchAC" (&/|reverse ?tests)))
        (fail "[Pattern-maching error] Pattern-matching is non-total.")))))
