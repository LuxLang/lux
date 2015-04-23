(ns lux.analyser.lux
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail fail* |let |list]]
                 [parser :as &parser]
                 [type :as &type]
                 [macro :as &macro]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lambda :as &&lambda]
                          [case :as &&case]
                          [env :as &&env]
                          [def :as &&def])))

(defn ^:private analyse-1+ [analyse ?token]
  (&type/with-var
    (fn [$var]
      ;; (prn 'analyse-1+ (aget $var 1) (&/show-ast ?token))
      (|do [=expr (&&/analyse-1 analyse $var ?token)]
        (matchv ::M/objects [=expr]
          [["Expression" [?item ?type]]]
          (|do [=type (&type/clean $var ?type)]
            (return (&/V "Expression" (&/T ?item =type))))
          )))))

;; [Exports]
(defn analyse-tuple [analyse exo-type ?elems]
  ;; (prn "^^ analyse-tuple ^^")
  ;; (prn 'analyse-tuple (str "[" (->> ?elems (&/|map &/show-ast) (&/|interpose " ") (&/fold str "")) "]")
  ;;      (&type/show-type exo-type))
  (matchv ::M/objects [exo-type]
    [["lux;TupleT" ?members]]
    (|do [=elems (&/map% (fn [ve]
                           (|let [[elem-t elem] ve]
                             (&&/analyse-1 analyse elem-t elem)))
                         (&/zip2 ?members ?elems))]
      (return (&/|list (&/V "Expression" (&/T (&/V "tuple" =elems)
                                              exo-type)))))

    [_]
    (fail "[Analyser Error] Tuples require tuple-types.")))

(defn analyse-variant [analyse exo-type ident ?value]
  ;; (prn "^^ analyse-variant ^^")
  (|do [;; :let [_ (prn 'analyse-variant/exo-type (&type/show-type exo-type))]
        ?tag (&&/resolved-ident ident)
        exo-type* (matchv ::M/objects [exo-type]
                    [["lux;VarT" ?id]]
                    (&/try-all% (&/|list (|do [exo-type* (&type/deref ?id)]
                                           (&type/actual-type exo-type*))
                                         (|do [_ (&type/set-var ?id &type/Type)]
                                           (&type/actual-type &type/Type))))

                    [_]
                    (&type/actual-type exo-type))
        ;; :let [_ (prn 'analyse-variant/exo-type* (&type/show-type exo-type*))]
        ]
    (matchv ::M/objects [exo-type*]
      [["lux;VariantT" ?cases]]
      (if-let [vtype (&/|get ?tag ?cases)]
        (|do [;; :let [_ (prn 'VARIANT_BODY ?tag (&/show-ast ?value) (&type/show-type vtype))]
              =value (&&/analyse-1 analyse vtype ?value)
              ;; :let [_ (prn 'GOT_VALUE =value)]
              ]
          (return (&/|list (&/V "Expression" (&/T (&/V "variant" (&/T ?tag =value))
                                                  exo-type)))))
        (fail (str "[Analyser Error] There is no case " ?tag " for variant type " (&type/show-type exo-type*))))

      [_]
      (fail (str "[Analyser Error] Can't create a variant if the expected type is " (&type/show-type exo-type*))))))

(defn analyse-record [analyse exo-type ?elems]
  (|do [=elems (&/map% (fn [kv]
                         (matchv ::M/objects [kv]
                           [[k v]]
                           (|do [=v (&&/analyse-1 analyse v)]
                             (return (to-array [k =v])))))
                       ?elems)
        =elems-types (&/map% (fn [kv]
                               (matchv ::M/objects [kv]
                                 [[k v]]
                                 (|do [module (if (= "" k)
                                                &/get-module-name
                                                (return k))
                                       =v (&&/expr-type v)]
                                   (return (to-array [module =v])))))
                             =elems)
        ;; :let [_ (prn 'analyse-tuple =elems)]
        ]
    (return (&/|list (&/V "Expression" (&/T (&/V "lux;record" =elems) (&/V "lux;RecordT" =elems-types)))))))

(defn ^:private show-frame [frame]
  (str "{{" (->> frame (&/get$ "lux;locals") (&/get$ "lux;mappings")
                 &/|keys &/->seq (interpose " ") (reduce str ""))
       "}}"))

(defn ^:private type-test [exo-type binding]
  (|do [btype (&&/expr-type binding)
        o?? (&type/is-Type? exo-type)]
    (if o??
      (|do [i?? (&type/is-Type? btype)]
        (if i??
          (do ;; (println "FOUND TWO TYPES!")
            (return (&/|list binding)))
          (fail "[Type Error] Types don't match.")))
      (|do [_ (&type/check exo-type btype)]
        (return (&/|list binding))))))

(defn analyse-symbol [analyse exo-type ident]
  (|do [module-name &/get-module-name]
    (fn [state]
      (|let [[?module ?name] ident
             local-ident (str ?module ";" ?name)
             global-ident (str (if (= "" ?module) module-name ?module) ";" ?name)
             stack (&/get$ "lux;local-envs" state)
             no-binding? #(and (->> % (&/get$ "lux;locals")  (&/get$ "lux;mappings") (&/|contains? local-ident) not)
                               (->> % (&/get$ "lux;closure") (&/get$ "lux;mappings") (&/|contains? local-ident) not))
             [inner outer] (&/|split-with no-binding? stack)]
        (matchv ::M/objects [outer]
          [["lux;Nil" _]]
          (if-let [global (->> state (&/get$ "lux;global-env") &/from-some (&/get$ "lux;locals") (&/get$ "lux;mappings") (&/|get global-ident))]
            (&/run-state (type-test exo-type global)
                         ;; (|do [btype (&&/expr-type global)
                         ;;       _ (&type/check exo-type btype)]
                         ;;   (return (&/|list global)))
                         state)
            (do ;; (prn (str "((" (->> stack (&/|map show-frame) &/->seq (interpose " ") (reduce str "")) "))"))
              (fail* (str "[Analyser Error] Unrecognized identifier: " local-ident))))

          [["lux;Cons" [top-outer _]]]
          (|let [scopes (&/|tail (&/folds #(&/|cons (&/get$ "lux;name" %2) %1)
                                          (&/|map #(&/get$ "lux;name" %) outer)
                                          (&/|reverse inner)))
                 [=local inner*] (&/fold (fn [register+new-inner frame+in-scope]
                                           (|let [[register new-inner] register+new-inner
                                                  [frame in-scope] frame+in-scope
                                                  [register* frame*] (&&lambda/close-over (&/|cons module-name (&/|reverse in-scope)) ident register frame)]
                                             (&/T register* (&/|cons frame* new-inner))))
                                         (&/T (or (->> top-outer (&/get$ "lux;locals")  (&/get$ "lux;mappings") (&/|get local-ident))
                                                  (->> top-outer (&/get$ "lux;closure") (&/get$ "lux;mappings") (&/|get local-ident)))
                                              (&/|list))
                                         (&/zip2 (&/|reverse inner) scopes))]
            (&/run-state (type-test exo-type =local)
                         ;; (|do [btype (&&/expr-type =local)
                         ;;       _ (&type/check exo-type btype)]
                         ;;   (return (&/|list =local)))
                         (&/set$ "lux;local-envs" (&/|++ inner* outer) state)))
          )))
    ))

(defn ^:private analyse-apply* [analyse exo-type =fn ?args]
  ;; (prn 'analyse-apply* (&/->seq (&/|map &/show-ast ?args)))
  ;; (prn 'analyse-apply*/exo-type (&type/show-type exo-type))
  (matchv ::M/objects [=fn]
    [["Statement" _]]
    (fail "[Analyser Error] Can't apply a statement!")

    [["Expression" [?fun-expr ?fun-type]]]
    (matchv ::M/objects [?args]
      [["lux;Nil" _]]
      (|do [_ (&type/check exo-type ?fun-type)]
        (return (&/|list =fn)))
      
      [["lux;Cons" [?arg ?args*]]]
      (do ;; (prn 'analyse-apply*/=fn (&type/show-type ?fun-type))
        (matchv ::M/objects [?fun-type]
          [["lux;AllT" _]]
          (&type/with-var
            (fn [$var]
              (|do [type* (&type/apply-type ?fun-type $var)
                    output (analyse-apply* analyse exo-type (&/V "Expression" (&/T ?fun-expr type*)) ?args)]
                (matchv ::M/objects [output]
                  [["lux;Cons" [["Expression" [?expr* ?type*]] ["lux;Nil" _]]]]
                  (|do [type** (&type/clean $var ?type*)]
                    (return (&/|list (&/V "Expression" (&/T ?expr* type**)))))

                  [_]
                  (assert false (prn-str 'analyse-apply*/output (aget output 0)))))))

          [["lux;LambdaT" [?input-t ?output-t]]]
          ;; (|do [=arg (&&/analyse-1 analyse ?input-t ?arg)]
          ;;   (return (&/|list (&/V "Expression" (&/T (&/V "apply" (&/T =fn =arg))
          ;;                                           ?output-t)))))
          (|do [=arg (&&/analyse-1 analyse ?input-t ?arg)]
            (analyse-apply* analyse exo-type (&/V "Expression" (&/T (&/V "apply" (&/T =fn =arg))
                                                                    ?output-t))
                            ?args*))

          [_]
          (fail "[Analyser Error] Can't apply a non-function.")))
      )))

(defn analyse-apply [analyse exo-type =fn ?args]
  ;; (prn 'analyse-apply1 (aget =fn 0))
  (|do [loader &/loader]
    (matchv ::M/objects [=fn]
      [["Expression" [=fn-form =fn-type]]]
      (do ;; (prn 'analyse-apply2 (aget =fn-form 0))
          (matchv ::M/objects [=fn-form]
            [["global" [?module ?name]]]
            (|do [macro? (&&def/macro? ?module ?name)]
              (if macro?
                (let [macro-class (&host/location (&/|list ?module ?name))]
                  (|do [macro-expansion (&macro/expand loader macro-class ?args)
                        ;; :let [_ (when (and (= "lux" ?module)
                        ;;                    (= "`" ?name))
                        ;;           (prn 'macro-expansion (->> macro-expansion (&/|map &/show-ast) (&/|interpose " ") (&/fold str ""))))]
                        ;; :let [_ (prn 'EXPANDING (&type/show-type exo-type))]
                        output (&/flat-map% (partial analyse exo-type) macro-expansion)]
                    (return output)))
                (analyse-apply* analyse exo-type =fn ?args)))
            
            [_]
            (analyse-apply* analyse exo-type =fn ?args)))

      [_]
      (fail "[Analyser Error] Can't call a statement!"))
    ))

(defn analyse-case [analyse exo-type ?value ?branches]
  ;; (prn 'analyse-case 'exo-type (&type/show-type exo-type) (&/show-ast ?value))
  (|do [:let [num-branches (&/|length ?branches)]
        _ (&/assert! (> num-branches 0) "[Analyser Error] Can't have empty branches in \"case'\" expression.")
        _ (&/assert! (even? num-branches) "[Analyser Error] Unbalanced branches in \"case'\" expression.")
        =value (analyse-1+ analyse ?value)
        =value-type (&&/expr-type =value)
        ;; :let [_ (prn 'analyse-case/GOT_VALUE (&type/show-type =value-type))]
        =match (&&case/analyse-branches analyse exo-type =value-type (&/|as-pairs ?branches))
        ;; :let [_ (prn 'analyse-case/GOT_MATCH)]
        ]
    (return (&/|list (&/V "Expression" (&/T (&/V "case" (&/T =value =match))
                                            exo-type))))))

(defn analyse-lambda* [analyse exo-type ?self ?arg ?body]
  ;; (prn 'analyse-lambda ?self ?arg ?body)
  (matchv ::M/objects [exo-type]
    [["lux;LambdaT" [?arg-t ?return-t]]]
    (|do [[=scope =captured =body] (&&lambda/with-lambda ?self exo-type
                                     ?arg ?arg-t
                                     (&&/analyse-1 analyse ?return-t ?body))]
      (return (&/V "Expression" (&/T (&/V "lambda" (&/T =scope =captured =body)) exo-type))))
    
    [_]
    (fail (str "[Analyser Error] Functions require function types: " (&type/show-type exo-type)))))

(defn analyse-lambda** [analyse exo-type ?self ?arg ?body]
  ;; (prn 'analyse-lambda**/&& (aget exo-type 0))
  (matchv ::M/objects [exo-type]
    [["lux;AllT" _]]
    (&type/with-var
      (fn [$var]
        (|do [exo-type* (&type/apply-type exo-type $var)
              output (analyse-lambda** analyse exo-type* ?self ?arg ?body)]
          (matchv ::M/objects [$var]
            [["lux;VarT" ?id]]
            (|do [? (&type/bound? ?id)]
              (if ?
                (fail "[Analyser Error] Can't use type-var in any type-specific way inside polymorphic functions.")
                (return output)))))))
    
    [_]
    (analyse-lambda* analyse exo-type ?self ?arg ?body)))

(defn analyse-lambda [analyse exo-type ?self ?arg ?body]
  (|do [output (analyse-lambda** analyse exo-type ?self ?arg ?body)]
    (return (&/|list output))))

(defn analyse-def [analyse ?name ?value]
  (prn 'analyse-def/CODE ?name (&/show-ast ?value))
  (|do [module-name &/get-module-name
        ? (&&def/defined? module-name ?name)]
    (if ?
      (fail (str "[Analyser Error] Can't redefine " ?name))
      (|do [;; :let [_ (prn 'analyse-def/_0)]
            =value (&/with-scope ?name
                     (analyse-1+ analyse ?value))
            ;; :let [_ (prn 'analyse-def/_1)]
            =value-type (&&/expr-type =value)
            ;; :let [_ (prn 'analyse-def/_2)]
            :let [_ (prn 'analyse-def/TYPE ?name (&type/show-type =value-type))
                  _ (println)]
            _ (&&def/define module-name ?name =value-type)
            ;; :let [_ (prn 'analyse-def/_3)]
            ]
        (return (&/|list (&/V "Statement" (&/V "def" (&/T ?name =value)))))))))

(defn analyse-declare-macro [ident]
  (|do [current-module &/get-module-name
        ;; :let [_ (prn 'analyse-declare-macro/current-module current-module)]
        [?module ?name] (&&/resolved-ident* ident)
        ;; :let [_ (prn 'analyse-declare-macro '[?module ?name] [?module ?name])]
        ]
    (if (= ?module current-module)
      (|do [_ (&&def/declare-macro ?module ?name)]
        (return (&/|list)))
      (fail "Can't declare macros from foreign modules."))))

(defn analyse-import [analyse exo-type ?path]
  (assert false)
  (return (&/|list)))

(defn analyse-check [analyse eval! exo-type ?type ?value]
  ;; (println "analyse-check#0")
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ;; =type (analyse-1+ analyse ?type)
        ;; :let [_ (println "analyse-check#1")]
        ==type (eval! =type)
        _ (&type/check exo-type ==type)
        ;; :let [_ (println "analyse-check#4" (&type/show-type ==type))]
        =value (&&/analyse-1 analyse ==type ?value)
        ;; :let [_ (println "analyse-check#5")]
        ]
    (matchv ::M/objects [=value]
      [["Expression" [?expr ?expr-type]]]
      (return (&/|list (&/V "Expression" (&/T ?expr ==type)))))))

(defn analyse-coerce [analyse eval! exo-type ?type ?value]
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ==type (eval! =type)
        =value (&&/analyse-1 analyse ==type ?value)]
    (matchv ::M/objects [=value]
      [["Expression" [?expr ?expr-type]]]
      (return (&/|list (&/V "Expression" (&/T ?expr ==type)))))))
