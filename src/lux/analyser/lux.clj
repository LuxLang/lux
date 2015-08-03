;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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

(defn ^:private with-cursor [cursor form]
  (matchv ::M/objects [form]
    [["lux;Meta" [_ syntax]]]
    (&/V "lux;Meta" (&/T cursor syntax))))

;; [Exports]
(defn analyse-tuple [analyse exo-type ?elems]
  (|do [exo-type* (&type/actual-type exo-type)]
    (matchv ::M/objects [exo-type*]
      [["lux;TupleT" ?members]]
      (|do [=elems (&/map2% (fn [elem-t elem]
                              (&&/analyse-1 analyse elem-t elem))
                            ?members ?elems)]
        (return (&/|list (&/T (&/V "tuple" =elems)
                              exo-type))))

      [["lux;AllT" _]]
      (&type/with-var
        (fn [$var]
          (|do [exo-type** (&type/apply-type exo-type* $var)]
            (analyse-tuple analyse exo-type** ?elems))))

      [_]
      (fail (str "[Analyser Error] Tuples require tuple-types: " (&type/show-type exo-type*))))))

(defn ^:private analyse-variant-body [analyse exo-type ?values]
  (|do [output (matchv ::M/objects [?values]
                 [["lux;Nil" _]]
                 (analyse-tuple analyse exo-type (&/|list))

                 [["lux;Cons" [?value ["lux;Nil" _]]]]
                 (analyse exo-type ?value)

                 [_]
                 (analyse-tuple analyse exo-type ?values)
                 )]
    (matchv ::M/objects [output]
      [["lux;Cons" [x ["lux;Nil" _]]]]
      (return x)

      [_]
      (fail "[Analyser Error] Can't expand to other than 1 element."))))

(defn analyse-variant [analyse exo-type ident ?values]
  (|do [exo-type* (matchv ::M/objects [exo-type]
                    [["lux;VarT" ?id]]
                    (&/try-all% (&/|list (|do [exo-type* (&type/deref ?id)]
                                           (&type/actual-type exo-type*))
                                         (|do [_ (&type/set-var ?id &type/Type)]
                                           (&type/actual-type &type/Type))))

                    [_]
                    (&type/actual-type exo-type))]
    (matchv ::M/objects [exo-type*]
      [["lux;VariantT" ?cases]]
      (|do [?tag (&&/resolved-ident ident)]
        (if-let [vtype (&/|get ?tag ?cases)]
          (|do [=value (analyse-variant-body analyse vtype ?values)]
            (return (&/|list (&/T (&/V "variant" (&/T ?tag =value))
                                  exo-type))))
          (fail (str "[Analyser Error] There is no case " ?tag " for variant type " (&type/show-type exo-type*)))))

      [["lux;AllT" _]]
      (&type/with-var
        (fn [$var]
          (|do [exo-type** (&type/apply-type exo-type* $var)]
            (analyse-variant analyse exo-type** ident ?values))))
      
      [_]
      (fail (str "[Analyser Error] Can't create a variant if the expected type is " (&type/show-type exo-type*))))))

(defn analyse-record [analyse exo-type ?elems]
  (|do [exo-type* (matchv ::M/objects [exo-type]
                    [["lux;VarT" ?id]]
                    (|do [exo-type* (&type/deref ?id)]
                      (&type/actual-type exo-type*))

                    [["lux;AllT" _]]
                    (|do [$var &type/existential
                          =type (&type/apply-type exo-type $var)]
                      (&type/actual-type =type))
                    ;; (&type/with-var
                    ;;   (fn [$var]
                    ;;     (|do [=type (&type/apply-type exo-type $var)]
                    ;;       (&type/actual-type =type))))

                    [_]
                    (&type/actual-type exo-type))
        types (matchv ::M/objects [exo-type*]
                [["lux;RecordT" ?table]]
                (return ?table)

                [_]
                (fail (str "[Analyser Error] The type of a record must be a record type:\n"
                           (&type/show-type exo-type*)
                           "\n")))
        _ (&/assert! (= (&/|length types) (&/|length ?elems))
                     (str "[Analyser Error] Record length mismatch. Expected: " (&/|length types) "; actual: " (&/|length ?elems)))
        =slots (&/map% (fn [kv]
                         (matchv ::M/objects [kv]
                           [[["lux;Meta" [_ ["lux;TagS" ?ident]]] ?value]]
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
             ;; _ (prn 'analyse-symbol/_0 ?module ?name)
             local-ident (str ?module ";" ?name)
             stack (&/get$ &/$ENVS state)
             no-binding? #(and (->> % (&/get$ &/$LOCALS)  (&/get$ &/$MAPPINGS) (&/|contains? local-ident) not)
                               (->> % (&/get$ &/$CLOSURE) (&/get$ &/$MAPPINGS) (&/|contains? local-ident) not))
             [inner outer] (&/|split-with no-binding? stack)]
        (matchv ::M/objects [outer]
          [["lux;Nil" _]]
          (do ;; (prn 'analyse-symbol/_1
              ;;      [?module ?name]
              ;;      [(if (.equals "" ?module) module-name ?module)
              ;;       ?name])
              ((|do [[[r-module r-name] $def] (&&module/find-def (if (.equals "" ?module) module-name ?module)
                                                                 ?name)
                     ;; :let [_ (prn 'analyse-symbol/_1.1 r-module r-name)]
                     endo-type (matchv ::M/objects [$def]
                                 [["lux;ValueD" ?type]]
                                 (return ?type)

                                 [["lux;MacroD" _]]
                                 (return &type/Macro)

                                 [["lux;TypeD" _]]
                                 (return &type/Type))
                     _ (if (and (clojure.lang.Util/identical &type/Type endo-type)
                                (clojure.lang.Util/identical &type/Type exo-type))
                         (return nil)
                         (&type/check exo-type endo-type))]
                 (return (&/|list (&/T (&/V "lux;Global" (&/T r-module r-name))
                                       endo-type))))
               state))

          [["lux;Cons" [?genv ["lux;Nil" _]]]]
          (do ;; (prn 'analyse-symbol/_2 ?module ?name local-ident (->> ?genv (&/get$ &/$LOCALS) (&/get$ &/$MAPPINGS) &/|keys &/->seq))
              (if-let [global (->> ?genv (&/get$ &/$LOCALS) (&/get$ &/$MAPPINGS) (&/|get local-ident))]
                (do ;; (prn 'analyse-symbol/_2.1 ?module ?name local-ident (aget global 0))
                    (matchv ::M/objects [global]
                      [[["lux;Global" [?module* ?name*]] _]]
                      ((|do [[[r-module r-name] $def] (&&module/find-def ?module* ?name*)
                             ;; :let [_ (prn 'analyse-symbol/_2.1.1 r-module r-name)]
                             endo-type (matchv ::M/objects [$def]
                                         [["lux;ValueD" ?type]]
                                         (return ?type)

                                         [["lux;MacroD" _]]
                                         (return &type/Macro)

                                         [["lux;TypeD" _]]
                                         (return &type/Type))
                             _ (if (and (clojure.lang.Util/identical &type/Type endo-type)
                                        (clojure.lang.Util/identical &type/Type exo-type))
                                 (return nil)
                                 (&type/check exo-type endo-type))]
                         (return (&/|list (&/T (&/V "lux;Global" (&/T r-module r-name))
                                               endo-type))))
                       state)

                      [_]
                      (do ;; (prn 'analyse-symbol/_2.1.2 ?module ?name local-ident)
                          (fail* "[Analyser Error] Can't have anything other than a global def in the global environment."))))
                (fail* "_{_ analyse-symbol _}_")))
          
          [["lux;Cons" [top-outer _]]]
          (do ;; (prn 'analyse-symbol/_3 ?module ?name)
              (|let [scopes (&/|tail (&/folds #(&/|cons (&/get$ &/$NAME %2) %1)
                                              (&/|map #(&/get$ &/$NAME %) outer)
                                              (&/|reverse inner)))
                     [=local inner*] (&/fold2 (fn [register+new-inner frame in-scope]
                                                (|let [[register new-inner] register+new-inner
                                                       [register* frame*] (&&lambda/close-over (&/|reverse in-scope) ident register frame)]
                                                  (&/T register* (&/|cons frame* new-inner))))
                                              (&/T (or (->> top-outer (&/get$ &/$LOCALS)  (&/get$ &/$MAPPINGS) (&/|get local-ident))
                                                       (->> top-outer (&/get$ &/$CLOSURE) (&/get$ &/$MAPPINGS) (&/|get local-ident)))
                                                   (&/|list))
                                              (&/|reverse inner) scopes)]
                ((|do [btype (&&/expr-type =local)
                       _ (&type/check exo-type btype)]
                   (return (&/|list =local)))
                 (&/set$ &/$ENVS (&/|++ inner* outer) state))))
          )))
    ))

(defn ^:private analyse-apply* [analyse exo-type fun-type ?args]
  ;; (prn 'analyse-apply* (aget fun-type 0))
  (matchv ::M/objects [?args]
    [["lux;Nil" _]]
    (|do [_ (&type/check exo-type fun-type)]
      (return (&/T fun-type (&/|list))))
    
    [["lux;Cons" [?arg ?args*]]]
    (|do [?fun-type* (&type/actual-type fun-type)]
      (matchv ::M/objects [?fun-type*]
        [["lux;AllT" [_aenv _aname _aarg _abody]]]
        ;; (|do [$var &type/existential
        ;;       type* (&type/apply-type ?fun-type* $var)]
        ;;   (analyse-apply* analyse exo-type type* ?args))
        (&type/with-var
          (fn [$var]
            (|do [type* (&type/apply-type ?fun-type* $var)
                  [=output-t =args] (analyse-apply* analyse exo-type type* ?args)]
              (matchv ::M/objects [$var]
                [["lux;VarT" ?id]]
                (|do [? (&type/bound? ?id)
                      type** (if ?
                               (&type/clean $var =output-t)
                               (|do [_ (&type/set-var ?id (&/V "lux;BoundT" _aarg))]
                                 (&type/clean $var =output-t)))]
                  (return (&/T type** =args)))
                ))))

        [["lux;LambdaT" [?input-t ?output-t]]]
        (|do [[=output-t =args] (analyse-apply* analyse exo-type ?output-t ?args*)
              =arg (&&/analyse-1 analyse ?input-t ?arg)]
          (return (&/T =output-t (&/|cons =arg =args))))

        ;; [["lux;VarT" ?id-t]]
        ;; (|do [ (&type/deref ?id-t)])
        
        [_]
        (fail (str "[Analyser Error] Can't apply a non-function: " (&type/show-type ?fun-type*)))))
    ))

(defn analyse-apply [analyse exo-type form-cursor =fn ?args]
  (|do [loader &/loader]
    (matchv ::M/objects [=fn]
      [[=fn-form =fn-type]]
      (matchv ::M/objects [=fn-form]
        [["lux;Global" [?module ?name]]]
        (|do [[[r-module r-name] $def] (&&module/find-def ?module ?name)]
          (matchv ::M/objects [$def]
            [["lux;MacroD" macro]]
            (|do [;; :let [_ (prn 'MACRO-EXPAND|PRE (str r-module ";" r-name))]
                  macro-expansion #(-> macro (.apply ?args) (.apply %))
                  ;; :let [_ (prn 'MACRO-EXPAND|POST (str r-module ";" r-name))]
                  :let [macro-expansion* (&/|map (partial with-cursor form-cursor) macro-expansion)]
                  ;; :let [_ (when (or (= "loop" r-name)
                  ;;                   ;; (= "struct" r-name)
                  ;;                   )
                  ;;           (->> (&/|map &/show-ast macro-expansion*)
                  ;;                (&/|interpose "\n")
                  ;;                (&/fold str "")
                  ;;                (prn (str r-module ";" r-name))))]
                  ]
              (&/flat-map% (partial analyse exo-type) macro-expansion*))

            [_]
            (|do [[=output-t =args] (analyse-apply* analyse exo-type =fn-type ?args)]
              (return (&/|list (&/T (&/V "apply" (&/T =fn =args))
                                    =output-t))))))
        
        [_]
        (|do [[=output-t =args] (analyse-apply* analyse exo-type =fn-type ?args)]
          (return (&/|list (&/T (&/V "apply" (&/T =fn =args))
                                =output-t)))))
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
  (|do [exo-type* (&type/actual-type exo-type)]
    (matchv ::M/objects [exo-type]
      [["lux;AllT" _]]
      (&type/with-var
        (fn [$var]
          (|do [exo-type** (&type/apply-type exo-type* $var)]
            (analyse-lambda* analyse exo-type** ?self ?arg ?body))))
      ;; (|do [$var &type/existential
      ;;       exo-type** (&type/apply-type exo-type* $var)]
      ;;   (analyse-lambda* analyse exo-type** ?self ?arg ?body))
      
      [["lux;LambdaT" [?arg-t ?return-t]]]
      (|do [[=scope =captured =body] (&&lambda/with-lambda ?self exo-type*
                                       ?arg ?arg-t
                                       (&&/analyse-1 analyse ?return-t ?body))]
        (return (&/T (&/V "lambda" (&/T =scope =captured =body)) exo-type*)))
      
      [_]
      (fail (str "[Analyser Error] Functions require function types: "
                 (&type/show-type exo-type*))))))

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
                (|do [dtype (&type/deref ?id)
                      ;; dtype* (&type/actual-type dtype)
                      ]
                  (matchv ::M/objects [dtype]
                    [["lux;BoundT" ?vname]]
                    (return (&/T _expr exo-type))
                    
                    [["lux;ExT" _]]
                    (return (&/T _expr exo-type))

                    [["lux;VarT" ?_id]]
                    (|do [?? (&type/bound? ?_id)]
                      ;; (return (&/T _expr exo-type))
                      (if ??
                        (fail (str "[Analyser Error] Can't use type-var in any type-specific way inside polymorphic functions: " ?id ":" _arg " " (&type/show-type dtype)))
                        (return (&/T _expr exo-type)))
                      )

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
  ;; (prn 'analyse-def/BEGIN ?name)
  ;; (when (= "PList/Dict" ?name)
  ;;   (prn 'DEF ?name (&/show-ast ?value)))
  (|do [module-name &/get-module-name
        ? (&&module/defined? module-name ?name)]
    (if ?
      (fail (str "[Analyser Error] Can't redefine " (str module-name ";" ?name)))
      (|do [=value (&/with-scope ?name
                     (analyse-1+ analyse ?value))
            =value-type (&&/expr-type =value)]
        (matchv ::M/objects [=value]
          [[["lux;Global" [?r-module ?r-name]] _]]
          (|do [_ (&&module/def-alias module-name ?name ?r-module ?r-name =value-type)
                ;; :let [_ (println 'analyse-def/ALIAS (str module-name ";" ?name) '=> (str ?r-module ";" ?r-name))
                ;;       _ (println)]
                ]
            (return (&/|list)))

          [_]
          (|do [=value-type (&&/expr-type =value)
                :let [;; _ (prn 'analyse-def/END ?name)
                      _ (println 'DEF (str module-name ";" ?name))
                      ;; _ (println)
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

(defn analyse-import [analyse compile-module ?path]
  (|do [module-name &/get-module-name
        _ (if (= module-name ?path)
            (fail (str "[Analyser Error] Module can't import itself: " ?path))
            (return nil))]
    (&/save-module
     (|do [already-compiled? (&&module/exists? ?path)
           ;; :let [_ (prn 'analyse-import module-name ?path already-compiled?)]
           _ (&&module/add-import ?path)
           _ (&/when% (not already-compiled?) (compile-module ?path))]
       (return (&/|list))))))

(defn analyse-export [analyse name]
  (|do [module-name &/get-module-name
        _ (&&module/export module-name name)]
    (return (&/|list))))

(defn analyse-alias [analyse ex-alias ex-module]
  (|do [module-name &/get-module-name
        _ (&&module/alias module-name ex-alias ex-module)]
    (return (&/|list))))

(defn analyse-check [analyse eval! exo-type ?type ?value]
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ==type (eval! =type)
        _ (&type/check exo-type ==type)
        =value (&&/analyse-1 analyse ==type ?value)]
    (return (&/|list (&/T (&/V "ann" (&/T =value =type))
                          ==type)))))

(defn analyse-coerce [analyse eval! exo-type ?type ?value]
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ==type (eval! =type)
        _ (&type/check exo-type ==type)
        =value (&&/analyse-1 analyse ==type ?value)]
    (return (&/|list (&/T (&/V "ann" (&/T =value =type))
                          ==type)))))
