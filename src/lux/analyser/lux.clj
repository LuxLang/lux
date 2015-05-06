(ns lux.analyser.lux
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail fail* |let |list]]
                 [parser :as &parser]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lambda :as &&lambda]
                          [case :as &&case]
                          [env :as &&env]
                          [module :as &&module])))

(defn ^:private analyse-1+ [analyse ?token]
  (&type/with-var
    (fn [$var]
      (|do [=expr (&&/analyse-1 analyse $var ?token)]
        (matchv ::M/objects [=expr]
          [[?item ?type]]
          (|do [=type (&type/clean $var ?type)]
            (return (&/T ?item =type)))
          )))))

;; [Exports]
(defn analyse-tuple [analyse exo-type ?elems]
  (|do [exo-type* (&type/actual-type exo-type)]
    (matchv ::M/objects [exo-type*]
      [["lux;TupleT" ?members]]
      (|do [=elems (&/map% (fn [ve]
                             (|let [[elem-t elem] ve]
                               (&&/analyse-1 analyse elem-t elem)))
                           (&/zip2 ?members ?elems))]
        (return (&/|list (&/T (&/V "tuple" =elems)
                              exo-type))))

      [["lux;AllT" _]]
      (&type/with-var
        (fn [$var]
          (|do [exo-type** (&type/apply-type exo-type* $var)]
            (analyse-tuple analyse exo-type** ?elems))))

      [_]
      (fail (str "[Analyser Error] Tuples require tuple-types: " (&type/show-type exo-type*))))))

(defn analyse-variant [analyse exo-type ident ?value]
  (|do [exo-type* (matchv ::M/objects [exo-type]
                    [["lux;VarT" ?id]]
                    (&/try-all% (&/|list (|do [exo-type* (&/try-all% (&/|list (&type/deref ?id)
                                                                              (fail "##8##")))]
                                           (&type/actual-type exo-type*))
                                         (|do [_ (&type/set-var ?id &type/Type)]
                                           (&type/actual-type &type/Type))))

                    [_]
                    (&type/actual-type exo-type))]
    (matchv ::M/objects [exo-type*]
      [["lux;VariantT" ?cases]]
      (|do [?tag (&&/resolved-ident ident)]
        (if-let [vtype (&/|get ?tag ?cases)]
          (|do [=value (&&/analyse-1 analyse vtype ?value)]
            (return (&/|list (&/T (&/V "variant" (&/T ?tag =value))
                                  exo-type))))
          (fail (str "[Analyser Error] There is no case " ?tag " for variant type " (&type/show-type exo-type*)))))

      [["lux;AllT" _]]
      (&type/with-var
        (fn [$var]
          (|do [exo-type** (&type/apply-type exo-type* $var)]
            (analyse-variant analyse exo-type** ident ?value))))
      
      [_]
      (fail (str "[Analyser Error] Can't create a variant if the expected type is " (&type/show-type exo-type*))))))

(defn analyse-record [analyse exo-type ?elems]
  (|do [exo-type* (matchv ::M/objects [exo-type]
                    [["lux;VarT" ?id]]
                    (|do [exo-type* (&/try-all% (&/|list (&type/deref ?id)
                                                         (fail "##7##")))]
                      (&type/actual-type exo-type*))

                    [_]
                    (&type/actual-type exo-type))
        types (matchv ::M/objects [exo-type*]
                [["lux;RecordT" ?table]]
                (return ?table)

                [_]
                (fail "[Analyser Error] The type of a record must be a record type."))
        =slots (&/map% (fn [kv]
                         (matchv ::M/objects [kv]
                           [[["lux;Meta" [_ ["lux;Tag" ?ident]]] ?value]]
                           (|do [?tag (&&/resolved-ident ?ident)
                                 slot-type (if-let [slot-type (&/|get ?tag types)]
                                             (return slot-type)
                                             (fail (str "[Analyser Error] Record type does not have slot: " ?tag)))
                                 =value (&&/analyse-1 analyse slot-type ?value)]
                             (return (&/T ?tag =value)))

                           [_]
                           (fail "[Analyser Error] Wrong syntax for records. Odd elements must be tags.")))
                       ?elems)]
    (return (&/|list (&/T (&/V "record" =slots) (&/V "lux;RecordT" exo-type))))))

(defn analyse-symbol [analyse exo-type ident]
  (|do [module-name &/get-module-name]
    (fn [state]
      (|let [[?module ?name] ident
             local-ident (str ?module ";" ?name)
             stack (&/get$ &/$ENVS state)
             no-binding? #(and (->> % (&/get$ &/$LOCALS)  (&/get$ &/$MAPPINGS) (&/|contains? local-ident) not)
                               (->> % (&/get$ &/$CLOSURE) (&/get$ &/$MAPPINGS) (&/|contains? local-ident) not))
             [inner outer] (&/|split-with no-binding? stack)]
        (matchv ::M/objects [outer]
          [["lux;Nil" _]]
          (&/run-state (|do [[[r-module r-name] $def] (&&module/find-def (if (= "" ?module) module-name ?module)
                                                                         ?name)
                             endo-type (matchv ::M/objects [$def]
                                         [["lux;ValueD" ?type]]
                                         (return ?type)

                                         [["lux;MacroD" _]]
                                         (return &type/Macro)

                                         [["lux;TypeD" _]]
                                         (return &type/Type))
                             _ (if (and (= &type/Type endo-type) (= &type/Type exo-type))
                                 (return nil)
                                 (&type/check exo-type endo-type))]
                         (return (&/|list (&/T (&/V "lux;Global" (&/T r-module r-name))
                                               endo-type))))
                       state)

          [["lux;Cons" [?genv ["lux;Nil" _]]]]
          (if-let [global (->> ?genv (&/get$ &/$LOCALS) (&/get$ &/$MAPPINGS) (&/|get local-ident))]
            (matchv ::M/objects [global]
              [[["lux;Global" [?module* ?name*]] _]]
              (&/run-state (|do [[[r-module r-name] $def] (&&module/find-def ?module* ?name*)
                                 endo-type (matchv ::M/objects [$def]
                                             [["lux;ValueD" ?type]]
                                             (return ?type)

                                             [["lux;MacroD" _]]
                                             (return &type/Macro)

                                             [["lux;TypeD" _]]
                                             (return &type/Type))
                                 _ (if (and (= &type/Type endo-type) (= &type/Type exo-type))
                                     (return nil)
                                     (&type/check exo-type endo-type))]
                             (return (&/|list (&/T (&/V "lux;Global" (&/T r-module r-name))
                                                   endo-type))))
                           state)

              [_]
              (fail* "[Analyser Error] Can't have anything other than a global def in the global environment."))
            (fail* ""))

          [["lux;Cons" [top-outer _]]]
          (|let [scopes (&/|tail (&/folds #(&/|cons (&/get$ &/$NAME %2) %1)
                                          (&/|map #(&/get$ &/$NAME %) outer)
                                          (&/|reverse inner)))
                 [=local inner*] (&/fold (fn [register+new-inner frame+in-scope]
                                           (|let [[register new-inner] register+new-inner
                                                  [frame in-scope] frame+in-scope
                                                  [register* frame*] (&&lambda/close-over (&/|cons module-name (&/|reverse in-scope)) ident register frame)]
                                             (&/T register* (&/|cons frame* new-inner))))
                                         (&/T (or (->> top-outer (&/get$ &/$LOCALS)  (&/get$ &/$MAPPINGS) (&/|get local-ident))
                                                  (->> top-outer (&/get$ &/$CLOSURE) (&/get$ &/$MAPPINGS) (&/|get local-ident)))
                                              (&/|list))
                                         (&/zip2 (&/|reverse inner) scopes))]
            (&/run-state (|do [btype (&&/expr-type =local)
                               _ (&type/check exo-type btype)]
                           (return (&/|list =local)))
                         (&/set$ &/$ENVS (&/|++ inner* outer) state)))
          )))
    ))

(defn ^:private analyse-apply* [analyse exo-type =fn ?args]
  (matchv ::M/objects [=fn]
    [[?fun-expr ?fun-type]]
    (matchv ::M/objects [?args]
      [["lux;Nil" _]]
      (|do [_ (&type/check exo-type ?fun-type)]
        (return =fn))
      
      [["lux;Cons" [?arg ?args*]]]
      (|do [?fun-type* (&type/actual-type ?fun-type)]
        (matchv ::M/objects [?fun-type*]
          [["lux;AllT" _]]
          (&type/with-var
            (fn [$var]
              (|do [type* (&type/apply-type ?fun-type* $var)
                    output (analyse-apply* analyse exo-type (&/T ?fun-expr type*) ?args)]
                (matchv ::M/objects [output $var]
                  [[?expr* ?type*] ["lux;VarT" ?id]]
                  (|do [? (&type/bound? ?id)
                        _ (if ?
                            (return nil)
                            (|do [ex &type/existential]
                              (&type/set-var ?id ex)))
                        type** (&type/clean $var ?type*)]
                    (return (&/T ?expr* type**)))
                  ))))

          [["lux;LambdaT" [?input-t ?output-t]]]
          (|do [=arg (&&/analyse-1 analyse ?input-t ?arg)]
            (analyse-apply* analyse exo-type (&/T (&/V "apply" (&/T =fn =arg))
                                                  ?output-t)
                            ?args*))

          [_]
          (fail (str "[Analyser Error] Can't apply a non-function: " (&type/show-type ?fun-type*)))))
      )))

(defn analyse-apply [analyse exo-type =fn ?args]
  (|do [loader &/loader]
    (matchv ::M/objects [=fn]
      [[=fn-form =fn-type]]
      (matchv ::M/objects [=fn-form]
        [["lux;Global" [?module ?name]]]
        (|do [[[r-module r-name] $def] (&&module/find-def ?module ?name)]
          (matchv ::M/objects [$def]
            [["lux;MacroD" macro]]
            (|do [macro-expansion #(-> macro (.apply ?args) (.apply %))]
              (&/flat-map% (partial analyse exo-type) macro-expansion))

            [_]
            (|do [output (analyse-apply* analyse exo-type =fn ?args)]
              (return (&/|list output)))))
        
        [_]
        (|do [output (analyse-apply* analyse exo-type =fn ?args)]
          (return (&/|list output))))
      )))

(defn analyse-case [analyse exo-type ?value ?branches]
  (|do [:let [num-branches (&/|length ?branches)]
        _ (&/assert! (> num-branches 0) "[Analyser Error] Can't have empty branches in \"case'\" expression.")
        _ (&/assert! (even? num-branches) "[Analyser Error] Unbalanced branches in \"case'\" expression.")
        =value (analyse-1+ analyse ?value)
        =value-type (&&/expr-type =value)
        =match (&&case/analyse-branches analyse exo-type =value-type (&/|as-pairs ?branches))]
    (return (&/|list (&/T (&/V "case" (&/T =value =match))
                          exo-type)))))

(defn analyse-lambda* [analyse exo-type ?self ?arg ?body]
  (matchv ::M/objects [exo-type]
    [["lux;LambdaT" [?arg-t ?return-t]]]
    (|do [[=scope =captured =body] (&&lambda/with-lambda ?self exo-type
                                     ?arg ?arg-t
                                     (&&/analyse-1 analyse ?return-t ?body))]
      (return (&/T (&/V "lambda" (&/T =scope =captured =body)) exo-type)))
    
    [_]
    (fail (str "[Analyser Error] Functions require function types: "
               (&type/show-type exo-type)))))

(defn analyse-lambda** [analyse exo-type ?self ?arg ?body]
  (matchv ::M/objects [exo-type]
    [["lux;AllT" [_env _self _arg _body]]]
    (&type/with-var
      (fn [$var]
        (|do [exo-type* (&type/apply-type exo-type $var)
              [_expr _] (analyse-lambda** analyse exo-type* ?self ?arg ?body)]
          (matchv ::M/objects [$var]
            [["lux;VarT" ?id]]
            (|do [? (&type/bound? ?id)]
              (if ?
                (|do [dtype (&/try-all% (&/|list (&type/deref ?id)
                                                 (fail "##6##")))]
                  (matchv ::M/objects [dtype]
                    [["lux;ExT" _]]
                    (return (&/T _expr exo-type))

                    [_]
                    (fail (str "[Analyser Error] Can't use type-var in any type-specific way inside polymorphic functions: " ?id ":" _arg " " (&type/show-type dtype)))))
                (return (&/T _expr exo-type))))))))
    
    [_]
    (|do [exo-type* (&type/actual-type exo-type)]
      (analyse-lambda* analyse exo-type* ?self ?arg ?body))
    ))

(defn analyse-lambda [analyse exo-type ?self ?arg ?body]
  (|do [output (analyse-lambda** analyse exo-type ?self ?arg ?body)]
    (return (&/|list output))))

(defn analyse-def [analyse ?name ?value]
  (prn 'analyse-def/BEGIN ?name)
  (|do [module-name &/get-module-name
        ? (&&module/defined? module-name ?name)]
    (if ?
      (fail (str "[Analyser Error] Can't redefine " ?name))
      (|do [=value (&/with-scope ?name
                     (analyse-1+ analyse ?value))
            =value-type (&&/expr-type =value)]
        (matchv ::M/objects [=value]
          [[["lux;Global" [?r-module ?r-name]] _]]
          (|do [_ (&&module/def-alias module-name ?name ?r-module ?r-name =value-type)
                :let [_ (println 'analyse-def/ALIAS (str module-name ";" ?name) '=> (str ?r-module ";" ?r-name))
                      _ (println)]]
            (return (&/|list)))

          [_]
          (|do [=value-type (&&/expr-type =value)
                :let [_ (prn 'analyse-def/END ?name)
                      _ (println)
                      def-data (cond (&type/type= &type/Type =value-type)
                                     (&/V "lux;TypeD" nil)
                                     
                                     :else
                                     (&/V "lux;ValueD" =value-type))]
                _ (&&module/define module-name ?name def-data =value-type)]
            (return (&/|list (&/V "def" (&/T ?name =value def-data))))))
        ))))

(defn analyse-declare-macro [analyse ?name]
  (|do [module-name &/get-module-name]
    (return (&/|list (&/V "declare-macro" (&/T module-name ?name))))))

(defn analyse-import [analyse exo-type ?path]
  (return (&/|list)))

(defn analyse-export [analyse name]
  (|do [module-name &/get-module-name
        _ (&&module/export module-name name)]
    (return (&/|list))))

(defn analyse-check [analyse eval! exo-type ?type ?value]
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ==type (eval! =type)
        _ (&type/check exo-type ==type)
        =value (&&/analyse-1 analyse ==type ?value)]
    (matchv ::M/objects [=value]
      [[?expr ?expr-type]]
      (return (&/|list (&/T ?expr ==type))))))

(defn analyse-coerce [analyse eval! exo-type ?type ?value]
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ==type (eval! =type)
        =value (&&/analyse-1 analyse ==type ?value)]
    (matchv ::M/objects [=value]
      [[?expr ?expr-type]]
      (return (&/|list (&/T ?expr ==type))))))
