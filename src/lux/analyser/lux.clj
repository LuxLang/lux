(ns lux.analyser.lux
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return return* fail fail* |let |list]]
                 [parser :as &parser]
                 [type :as &type]
                 [macro :as &macro]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lambda :as &&lambda]
                          [case :as &&case]
                          [env :as &&env]
                          [def :as &&def])))

(defn ^:private analyse-1+ [analyse]
  (fn [?token]
    (&&/with-var #(&&/analyse-1 analyse % ?token))))

;; [Exports]
(defn analyse-tuple [analyse exo-type ?elems]
  (exec [=elems (&/map% (analyse-1+ analyse) ?elems)
         =elems-types (&/map% &&/expr-type =elems)
         ;; :let [_ (prn 'analyse-tuple =elems)]
         :let [endo-type (&/V "lux;TupleT" =elems-types)]
         _ (&type/solve exo-type endo-type)
         ;; :let [_ (prn 'analyse-tuple 'DONE)]
         ]
    (return (&/|list (&/V "Expression" (&/T (&/V "tuple" =elems)
                                            exo-type))))))

(defn analyse-variant [analyse exo-type ident ?value]
  (|let [[?module ?name] ident
         ?tag (str ?module ";" ?name)]
    (exec [=value ((analyse-1+ analyse) ?value)
           =value-type (&&/expr-type =value)
           :let [endo-type (&/V "lux;VariantT" (|list (&/T ?tag =value-type)))]
           _ (&type/solve exo-type endo-type)
           ;; :let [_ (prn 'analyse-variant 'DONE)]
           ]
      (return (&/|list (&/V "Expression" (&/T (&/V "variant" (&/T ?tag =value))
                                              exo-type)))))))

(defn analyse-record [analyse exo-type ?elems]
  (exec [=elems (&/map% (fn [kv]
                          (matchv ::M/objects [kv]
                            [[k v]]
                            (exec [=v (&&/analyse-1 analyse v)]
                              (return (to-array [k =v])))))
                        ?elems)
         =elems-types (&/map% (fn [kv]
                                (matchv ::M/objects [kv]
                                  [[k v]]
                                  (exec [=v (&&/expr-type v)]
                                    (return (to-array [k =v])))))
                              =elems)
         ;; :let [_ (prn 'analyse-tuple =elems)]
         ]
    (return (&/|list (&/V "Expression" (&/T (&/V "lux;record" =elems) (&/V "lux;RecordT" =elems-types)))))))

(defn analyse-symbol [analyse exo-type ident]
  (|let [[?module ?name] ident]
    (do ;; (prn 'analyse-ident ?module ?name)
        (exec [module-name &/get-module-name]
          (fn [state]
            ;; (when (and (= "lux" ?module)
            ;;            (= "output" ?name))
            ;;   (prn (&/show-state state)))
            ;; (prn 'module-name module-name)
            ;; (prn '(&/get$ "local-envs" state) (&/get$ "local-envs" state))
            ;; (prn '(&/->seq (&/get$ "local-envs" state)) (&/->seq (&/get$ "local-envs" state)))
            ;; (println (&/show-state state))
            (|let [stack (&/get$ "lux;local-envs" state)
                   no-binding? #(and (->> % (&/get$ "lux;locals")  (&/get$ "lux;mappings") (&/|contains? ?name) not)
                                     (->> % (&/get$ "lux;closure") (&/get$ "lux;mappings") (&/|contains? ?name) not))
                   [inner outer] (&/|split-with no-binding? stack)]
              (matchv ::M/objects [outer]
                [["lux;Nil" _]]
                (|let [[?module ?name] ident
                       ident* (str ?module ";" ?name)]
                  (if-let [global (->> state (&/get$ "lux;global-env") &/from-some (&/get$ "lux;locals") (&/get$ "lux;mappings") (&/|get ident*))]
                    (&/run-state (exec [=global-type (&&/expr-type global)
                                        _ (&type/solve exo-type =global-type)]
                                   (return (&/|list global)))
                                 state)
                    (fail* (str "[Analyser Error] Unresolved identifier: " ident*))))

                [["lux;Cons" [top-outer _]]]
                (|let [scopes (&/|tail (&/folds #(&/|cons (&/get$ "lux;name" %2) %1)
                                                (&/|map #(&/get$ "lux;name" %) outer)
                                                (&/|reverse inner)))
                       ;; _ (prn 'inner module-name ident (&/->seq (&/|map #(&/get$ "name" %) inner)) scopes)
                       [=local inner*] (&/fold (fn [register+new-inner frame+in-scope]
                                                 (|let [[register new-inner] register+new-inner
                                                        [frame in-scope] frame+in-scope
                                                        [register* frame*] (&&lambda/close-over (&/|cons module-name (&/|reverse in-scope)) ?name register frame)]
                                                   (&/T register* (&/|cons frame* new-inner))))
                                               (&/T (or (->> top-outer (&/get$ "lux;locals")  (&/get$ "lux;mappings") (&/|get ?name))
                                                        (->> top-outer (&/get$ "lux;closure") (&/get$ "lux;mappings") (&/|get ?name)))
                                                    (&/|list))
                                               (&/zip2 (&/|reverse inner) scopes))]
                  (return* (&/set$ "lux;local-envs" (&/|++ inner* outer) state) (&/|list =local)))
                )))
          ))))

(defn ^:private analyse-apply* [analyse exo-type =fn ?args]
  (exec [=args (&/flat-map% analyse ?args)
         =fn-type (&&/expr-type =fn)
         [=apply _] (&/fold% (fn [[=fn =fn-type] =input]
                               (exec [;; :let [_ (prn "#2")]
                                      =input-type (&&/expr-type =input)
                                      ;; :let [_ (prn "#3")]
                                      =output-type (&type/apply-lambda =fn-type =input-type)
                                      ;; :let [_ (prn "#4")]
                                      ]
                                 (return [(&/V "Expression" (&/T (&/V "apply" (&/T =fn =input))
                                                                 =output-type))
                                          =output-type])))
                             [=fn =fn-type]
                             =args)]
    (return (&/|list =apply))))

(defn analyse-apply [analyse exo-type =fn ?args]
  ;; (prn 'analyse-apply1 (aget =fn 0))
  (exec [loader &/loader]
    (matchv ::M/objects [=fn]
      [["Expression" [=fn-form =fn-type]]]
      (do ;; (prn 'analyse-apply2 (aget =fn-form 0))
        (matchv ::M/objects [=fn-form]
          [["global" [?module ?name]]]
          (exec [macro? (&&def/macro? ?module ?name)]
            (if macro?
              (let [macro-class (&host/location (&/|list ?module ?name))]
                (exec [macro-expansion (&macro/expand loader macro-class ?args)
                       output (&/flat-map% analyse macro-expansion)]
                  (return output)))
              (analyse-apply* analyse =fn ?args)))
          
          [_]
          (analyse-apply* analyse =fn ?args)))

      [_]
      (fail "[Analyser Error] Can't call a statement!"))
    ))

(defn analyse-case [analyse exo-type ?value ?branches]
  ;; (prn 'analyse-case (aget ?branches 0) (aget ?branches 1 1 0)
  ;;      (&/->seq ?branches))
  ;; (prn 'analyse-case (&/show-ast ?value))
  (exec [:let [num-branches (&/|length ?branches)
               ;; _ (prn 'analyse-case ?value (&/|length ?branches)
               ;;        (and (> num-branches 0) (even? num-branches)))
               ]
         _ (&/assert! (and (> num-branches 0) (even? num-branches))
                      "[Analyser Error] Unbalanced branches in \"case'\" expression.")
         :let [branches (&/|as-pairs ?branches)
               ;; _ (prn '(&/|length branches) (&/|length branches))
               locals-per-branch (&/|map (comp &&case/locals &/|first) branches)
               max-locals (&/fold max 0 (&/|map &/|length locals-per-branch))]
         ;; :let [_ (prn '[branches locals-per-branch max-locals] [branches locals-per-branch max-locals])]
         base-register &&env/next-local-idx
         ;; :let [_ (prn 'base-register base-register)]
         =value (&&/analyse-1 analyse ?value)
         ;; :let [_ (prn '=value =value)]
         =bodies (&/map% (partial &&case/analyse-branch analyse max-locals)
                         (&/zip2 locals-per-branch (&/|map &/|second branches)))
         ;; :let [_ (prn '=bodies =bodies)]
         ;; :let [_ (prn 'analyse-case/=bodies =bodies)]
         =body-types (&/map% &&/expr-type =bodies)
         :let [_ (prn 'analyse-case (->> =body-types (&/|map &type/show-type) (&/|interpose " ") (&/fold str "")))]
         =case-type (&/fold% &type/merge (&/V "lux;NothingT" nil) =body-types)
         :let [=branches (&/zip2 (&/|map &/|first branches) =bodies)]]
    (return (&/|list (&/V "Expression" (&/T (&/V "case" (&/T =value base-register max-locals =branches))
                                            =case-type))))))

(defn analyse-lambda [analyse exo-type ?self ?arg ?body]
  ;; (prn 'analyse-lambda ?self ?arg ?body)
  (exec [=lambda-type* &type/fresh-lambda]
    (matchv ::M/objects [=lambda-type*]
      [["lux;LambdaT" [=arg =return]]]
      (exec [[=scope =captured =body] (&&lambda/with-lambda ?self =lambda-type*
                                        ?arg =arg
                                        (&&/analyse-1 analyse ?body))
             =body-type (&&/expr-type =body)
             ;; _ =body-type
             =lambda-type (exec [_ (&type/solve &type/init-fixpoints =return =body-type)]
                            (&type/clean =return =lambda-type*))
             =bound-arg (&type/lookup =arg)
             =lambda-type (matchv ::M/objects [=arg =bound-arg]
                            [["lux;VarT" id] ["lux;Some" bound]]
                            (&type/clean =arg =lambda-type)

                            [["lux;VarT" id] ["lux;None" _]]
                            (let [var-name (str (gensym ""))
                                  bound (&/V "lux;BoundT" var-name)]
                              (exec [_ (&type/reset id bound)
                                     lambda-type (&type/clean =arg =lambda-type)]
                                (return (&/V "lux;AllT" (&/T (&/|list) "" var-name lambda-type))))))
             ;; :let [_ (prn '=lambda-type =lambda-type)]
             ]
        (return (&/|list (&/V "Expression" (&/T (&/V "lambda" (&/T =scope =captured ?arg =body)) =lambda-type))))))))

(defn analyse-def [analyse exo-type ?name ?value]
  ;; (prn 'analyse-def ?name ?value)
  (exec [_ (&type/solve &type/Nothing exo-type)
         module-name &/get-module-name]
    (&/if% (&&def/defined? module-name ?name)
           (fail (str "[Analyser Error] Can't redefine " ?name))
           (exec [=value (&/with-scope ?name
                           (&&/with-var
                             #(&&/analyse-1 analyse % ?value)))
                  =value-type (&&/expr-type =value)
                  :let [_ (prn 'analyse-def ?name (&type/show-type =value-type))]
                  _ (&&def/define module-name ?name =value-type)]
             (return (&/|list (&/V "Statement" (&/V "def" (&/T ?name =value)))))))))

(defn analyse-declare-macro [exo-type ident]
  (|let [[?module ?name] ident]
    (exec [module-name &/get-module-name]
      (if (= ?module module-name)
        (exec [_ (&&def/declare-macro ?module ?name)]
          (return (&/|list)))
        (fail "Can't declare macros from foreign modules.")))))

(defn analyse-import [analyse exo-type ?path]
  (assert false)
  (return (&/|list)))

(defn analyse-check [analyse eval! exo-type ?type ?value]
  (println "analyse-check#0")
  (exec [=type (&&/analyse-1 analyse &type/Type ?type)
         :let [_ (println "analyse-check#1")]
         ==type (eval! =type)
         _ (&type/solve &type/init-fixpoints exo-type ==type)
         :let [_ (println "analyse-check#4" (&type/show-type ==type))]
         =value (&&/analyse-1 analyse ==type ?value)
         :let [_ (println "analyse-check#5")]]
    (matchv ::M/objects [=value]
      [["Expression" [?expr ?expr-type]]]
      (exec [:let [_ (println "analyse-check#6" (&type/show-type ?expr-type))]
             _ (&type/solve &type/init-fixpoints ==type ?expr-type)
             :let [_ (println "analyse-check#7")]]
        (return (&/|list (&/V "Expression" (&/T ?expr ==type))))))))

(defn analyse-coerce [analyse eval! exo-type ?type ?value]
  (exec [=type (&&/analyse-1 analyse ?type)
         =type-type (&&/expr-type =type)
         _ (&type/solve &type/init-fixpoints &type/Type =type-type)
         ==type (eval! =type)
         =value (&&/analyse-1 analyse ?value)]
    (matchv ::M/objects [=value]
      [["Expression" [?expr ?expr-type]]]
      (return (&/|list (&/V "Expression" (&/T ?expr ==type)))))))
