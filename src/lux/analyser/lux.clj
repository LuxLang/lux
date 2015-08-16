;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.analyser.lux
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail fail* |let |list |case]]
                 [parser :as &parser]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lambda :as &&lambda]
                          [case :as &&case]
                          [env :as &&env]
                          [module :as &&module]
                          [record :as &&record])))

(defn ^:private analyse-1+ [analyse ?token]
  (&type/with-var
    (fn [$var]
      (|do [=expr (&&/analyse-1 analyse $var ?token)
            :let [[?item ?type] =expr]
            =type (&type/clean $var ?type)]
        (return (&/T ?item =type))))))

(defn ^:private with-cursor [cursor form]
  (|case form
    (&/$Meta _ syntax)
    (&/V &/$Meta (&/T cursor syntax))))

;; [Exports]
(defn analyse-tuple [analyse exo-type ?elems]
  (|do [exo-type* (&type/actual-type exo-type)]
    (|case exo-type*
      (&/$TupleT ?members)
      (|do [=elems (&/map2% (fn [elem-t elem]
                              (&&/analyse-1 analyse elem-t elem))
                            ?members ?elems)]
        (return (&/|list (&/T (&/V &&/$tuple =elems)
                              exo-type))))

      (&/$AllT _)
      (&type/with-var
        (fn [$var]
          (|do [exo-type** (&type/apply-type exo-type* $var)]
            (analyse-tuple analyse exo-type** ?elems))))

      _
      (fail (str "[Analyser Error] Tuples require tuple-types: " (&type/show-type exo-type*))))))

(defn ^:private analyse-variant-body [analyse exo-type ?values]
  (|do [output (|case ?values
                 (&/$Nil)
                 (analyse-tuple analyse exo-type (&/|list))

                 (&/$Cons ?value (&/$Nil))
                 (analyse exo-type ?value)

                 _
                 (analyse-tuple analyse exo-type ?values)
                 )]
    (|case output
      (&/$Cons x (&/$Nil))
      (return x)

      _
      (fail "[Analyser Error] Can't expand to other than 1 element."))))

(defn analyse-variant [analyse exo-type idx ?values]
  (|do [exo-type* (|case exo-type
                    (&/$VarT ?id)
                    (&/try-all% (&/|list (|do [exo-type* (&type/deref ?id)]
                                           (&type/actual-type exo-type*))
                                         (|do [_ (&type/set-var ?id &type/Type)]
                                           (&type/actual-type &type/Type))))

                    _
                    (&type/actual-type exo-type))]
    (|case exo-type*
      (&/$VariantT ?cases)
      (|case (&/|at idx ?cases)
        (&/$Some vtype)
        (|do [=value (analyse-variant-body analyse vtype ?values)]
          (return (&/|list (&/T (&/V &&/$variant (&/T idx =value))
                                exo-type))))

        (&/$None)
        (fail (str "[Analyser Error] There is no case " idx " for variant type " (&type/show-type exo-type*))))

      (&/$AllT _)
      (&type/with-var
        (fn [$var]
          (|do [exo-type** (&type/apply-type exo-type* $var)]
            (analyse-variant analyse exo-type** idx ?values))))
      
      _
      (fail (str "[Analyser Error] Can't create a variant if the expected type is " (&type/show-type exo-type*))))))
;; (defn analyse-variant [analyse exo-type ident ?values]
;;   (|do [exo-type* (|case exo-type
;;                     (&/$VarT ?id)
;;                     (&/try-all% (&/|list (|do [exo-type* (&type/deref ?id)]
;;                                            (&type/actual-type exo-type*))
;;                                          (|do [_ (&type/set-var ?id &type/Type)]
;;                                            (&type/actual-type &type/Type))))

;;                     _
;;                     (&type/actual-type exo-type))]
;;     (|case exo-type*
;;       (&/$VariantT ?cases)
;;       (|do [?tag (&&/resolved-ident ident)]
;;         (if-let [vtype (&/|get ?tag ?cases)]
;;           (|do [=value (analyse-variant-body analyse vtype ?values)]
;;             (return (&/|list (&/T (&/V &&/$variant (&/T ?tag =value))
;;                                   exo-type))))
;;           (fail (str "[Analyser Error] There is no case " ?tag " for variant type " (&type/show-type exo-type*)))))

;;       (&/$AllT _)
;;       (&type/with-var
;;         (fn [$var]
;;           (|do [exo-type** (&type/apply-type exo-type* $var)]
;;             (analyse-variant analyse exo-type** ident ?values))))

;;       _
;;       (fail (str "[Analyser Error] Can't create a variant if the expected type is " (&type/show-type exo-type*))))))

(defn analyse-record [analyse exo-type ?elems]
  (|do [exo-type* (|case exo-type
                    (&/$VarT ?id)
                    (|do [exo-type* (&type/deref ?id)]
                      (&type/actual-type exo-type*))

                    (&/$AllT _)
                    (|do [$var &type/existential
                          =type (&type/apply-type exo-type $var)]
                      (&type/actual-type =type))
                    ;; (&type/with-var
                    ;;   (fn [$var]
                    ;;     (|do [=type (&type/apply-type exo-type $var)]
                    ;;       (&type/actual-type =type))))

                    _
                    (&type/actual-type exo-type))
        types (|case exo-type*
                (&/$TupleT ?table)
                (return ?table)

                _
                (fail (str "[Analyser Error] The type of a record must be a record-type:\n" (&type/show-type exo-type*))))
        _ (&/assert! (= (&/|length types) (&/|length ?elems))
                     (str "[Analyser Error] Record length mismatch. Expected: " (&/|length types) "; actual: " (&/|length ?elems)))
        members (&&record/order-record ?elems)
        =members (&/map2% (fn [elem-t elem]
                            (&&/analyse-1 analyse elem-t elem))
                          types members)]
    (return (&/|list (&/T (&/V &&/$tuple =members) exo-type)))))

(defn ^:private analyse-global [analyse exo-type module name]
  (|do [[[r-module r-name] $def] (&&module/find-def module name)
        ;; :let [_ (prn 'analyse-symbol/_1.1 r-module r-name)]
        ;; :let [_ (prn 'analyse-global/$def (aget $def 0))]
        endo-type (|case $def
                    (&/$ValueD ?type _)
                    (return ?type)

                    (&/$MacroD _)
                    (return &type/Macro)

                    (&/$TypeD _)
                    (return &type/Type))
        _ (if (and (clojure.lang.Util/identical &type/Type endo-type)
                   (clojure.lang.Util/identical &type/Type exo-type))
            (return nil)
            (&type/check exo-type endo-type))]
    (return (&/|list (&/T (&/V &&/$var (&/V &/$Global (&/T r-module r-name)))
                          endo-type)))))

(defn ^:private analyse-local [analyse exo-type name]
  (fn [state]
    (|let [stack (&/get$ &/$envs state)
           no-binding? #(and (->> % (&/get$ &/$locals)  (&/get$ &/$mappings) (&/|contains? name) not)
                             (->> % (&/get$ &/$closure) (&/get$ &/$mappings) (&/|contains? name) not))
           [inner outer] (&/|split-with no-binding? stack)]
      (|case outer
        (&/$Nil)
        (&/run-state (|do [module-name &/get-module-name]
                       (analyse-global analyse exo-type module-name name))
                     state)

        (&/$Cons ?genv (&/$Nil))
        (do ;; (prn 'analyse-symbol/_2 ?module name name (->> ?genv (&/get$ &/$locals) (&/get$ &/$mappings) &/|keys &/->seq))
            (if-let [global (->> ?genv (&/get$ &/$locals) (&/get$ &/$mappings) (&/|get name))]
              (do ;; (prn 'analyse-symbol/_2.1 ?module name name (aget global 0))
                  (|case global
                    [(&/$Global ?module* name*) _]
                    ((|do [[[r-module r-name] $def] (&&module/find-def ?module* name*)
                           ;; :let [_ (prn 'analyse-symbol/_2.1.1 r-module r-name)]
                           endo-type (|case $def
                                       (&/$ValueD ?type _)
                                       (return ?type)

                                       (&/$MacroD _)
                                       (return &type/Macro)

                                       (&/$TypeD _)
                                       (return &type/Type))
                           _ (if (and (clojure.lang.Util/identical &type/Type endo-type)
                                      (clojure.lang.Util/identical &type/Type exo-type))
                               (return nil)
                               (&type/check exo-type endo-type))]
                       (return (&/|list (&/T (&/V &&/$var (&/V &/$Global (&/T r-module r-name)))
                                             endo-type))))
                     state)

                    [_]
                    (do ;; (prn 'analyse-symbol/_2.1.2 ?module name name)
                        (fail* "[Analyser Error] Can't have anything other than a global def in the global environment."))))
              (fail* "_{_ analyse-symbol _}_")))
        
        (&/$Cons top-outer _)
        (do ;; (prn 'analyse-symbol/_3 ?module name)
            (|let [scopes (&/|tail (&/folds #(&/|cons (&/get$ &/$name %2) %1)
                                            (&/|map #(&/get$ &/$name %) outer)
                                            (&/|reverse inner)))
                   [=local inner*] (&/fold2 (fn [register+new-inner frame in-scope]
                                              (|let [[register new-inner] register+new-inner
                                                     [register* frame*] (&&lambda/close-over (&/|reverse in-scope) name register frame)]
                                                (&/T register* (&/|cons frame* new-inner))))
                                            (&/T (or (->> top-outer (&/get$ &/$locals)  (&/get$ &/$mappings) (&/|get name))
                                                     (->> top-outer (&/get$ &/$closure) (&/get$ &/$mappings) (&/|get name)))
                                                 (&/|list))
                                            (&/|reverse inner) scopes)]
              ((|do [btype (&&/expr-type =local)
                     _ (&type/check exo-type btype)]
                 (return (&/|list =local)))
               (&/set$ &/$envs (&/|++ inner* outer) state))))
        ))))

(defn analyse-symbol [analyse exo-type ident]
  (|do [:let [[?module ?name] ident]]
    (if (= "" ?module)
      (analyse-local analyse exo-type ?name)
      (analyse-global analyse exo-type ?module ?name))
    ))

(defn ^:private analyse-apply* [analyse exo-type fun-type ?args]
  ;; (prn 'analyse-apply* (aget fun-type 0))
  (|case ?args
    (&/$Nil)
    (|do [_ (&type/check exo-type fun-type)]
      (return (&/T fun-type (&/|list))))
    
    (&/$Cons ?arg ?args*)
    (|do [?fun-type* (&type/actual-type fun-type)]
      (|case ?fun-type*
        (&/$AllT _aenv _aname _aarg _abody)
        ;; (|do [$var &type/existential
        ;;       type* (&type/apply-type ?fun-type* $var)]
        ;;   (analyse-apply* analyse exo-type type* ?args))
        (&type/with-var
          (fn [$var]
            (|do [type* (&type/apply-type ?fun-type* $var)
                  [=output-t =args] (analyse-apply* analyse exo-type type* ?args)]
              (|case $var
                (&/$VarT ?id)
                (|do [? (&type/bound? ?id)
                      type** (if ?
                               (&type/clean $var =output-t)
                               (|do [_ (&type/set-var ?id (&/V &/$BoundT _aarg))]
                                 (&type/clean $var =output-t)))]
                  (return (&/T type** =args)))
                ))))

        (&/$LambdaT ?input-t ?output-t)
        (|do [[=output-t =args] (analyse-apply* analyse exo-type ?output-t ?args*)
              =arg (&&/analyse-1 analyse ?input-t ?arg)]
          (return (&/T =output-t (&/|cons =arg =args))))

        ;; [[&/$VarT ?id-t]]
        ;; (|do [ (&type/deref ?id-t)])
        
        _
        (fail (str "[Analyser Error] Can't apply a non-function: " (&type/show-type ?fun-type*)))))
    ))

(defn analyse-apply [analyse exo-type form-cursor =fn ?args]
  (|do [loader &/loader]
    (|let [[=fn-form =fn-type] =fn]
      (|case =fn-form
        (&&/$var (&/$Global ?module ?name))
        (|do [[real-name $def] (&&module/find-def ?module ?name)]
          (|case $def
            (&/$MacroD macro)
            (|do [;; :let [_ (prn 'MACRO-EXPAND|PRE (&/ident->text real-name))]
                  macro-expansion #(-> macro (.apply ?args) (.apply %))
                  ;; :let [_ (prn 'MACRO-EXPAND|POST (&/ident->text real-name))]
                  ;; :let [macro-expansion* (&/|map (partial with-cursor form-cursor) macro-expansion)]
                  ;; :let [_ (when (or (= ":" (aget real-name 1))
                  ;;                   (= "type" (aget real-name 1))
                  ;;                   ;; (= &&/$struct r-name)
                  ;;                   )
                  ;;           (->> (&/|map &/show-ast macro-expansion)
                  ;;                (&/|interpose "\n")
                  ;;                (&/fold str "")
                  ;;                (prn (&/ident->text real-name))))]
                  ]
              (&/flat-map% (partial analyse exo-type) macro-expansion))

            _
            (|do [[=output-t =args] (analyse-apply* analyse exo-type =fn-type ?args)]
              (return (&/|list (&/T (&/V &&/$apply (&/T =fn =args))
                                    =output-t))))))
        
        _
        (|do [[=output-t =args] (analyse-apply* analyse exo-type =fn-type ?args)]
          (return (&/|list (&/T (&/V &&/$apply (&/T =fn =args))
                                =output-t)))))
      )))

(defn analyse-case [analyse exo-type ?value ?branches]
  (|do [:let [num-branches (&/|length ?branches)]
        _ (&/assert! (> num-branches 0) "[Analyser Error] Can't have empty branches in \"case'\" expression.")
        _ (&/assert! (even? num-branches) "[Analyser Error] Unbalanced branches in \"case'\" expression.")
        =value (analyse-1+ analyse ?value)
        =value-type (&&/expr-type =value)
        =match (&&case/analyse-branches analyse exo-type =value-type (&/|as-pairs ?branches))]
    (return (&/|list (&/T (&/V &&/$case (&/T =value =match))
                          exo-type)))))

(defn analyse-lambda* [analyse exo-type ?self ?arg ?body]
  (|do [exo-type* (&type/actual-type exo-type)]
    (|case exo-type
      (&/$AllT _)
      (&type/with-var
        (fn [$var]
          (|do [exo-type** (&type/apply-type exo-type* $var)]
            (analyse-lambda* analyse exo-type** ?self ?arg ?body))))
      ;; (|do [$var &type/existential
      ;;       exo-type** (&type/apply-type exo-type* $var)]
      ;;   (analyse-lambda* analyse exo-type** ?self ?arg ?body))
      
      (&/$LambdaT ?arg-t ?return-t)
      (|do [[=scope =captured =body] (&&lambda/with-lambda ?self exo-type*
                                       ?arg ?arg-t
                                       (&&/analyse-1 analyse ?return-t ?body))]
        (return (&/T (&/V &&/$lambda (&/T =scope =captured =body)) exo-type*)))
      
      _
      (fail (str "[Analyser Error] Functions require function types: "
                 (&type/show-type exo-type*))))))

(defn analyse-lambda** [analyse exo-type ?self ?arg ?body]
  (|case exo-type
    (&/$AllT _env _self _arg _body)
    (&type/with-var
      (fn [$var]
        (|do [exo-type* (&type/apply-type exo-type $var)
              [_expr _] (analyse-lambda** analyse exo-type* ?self ?arg ?body)]
          (|case $var
            (&/$VarT ?id)
            (|do [? (&type/bound? ?id)]
              (if ?
                (|do [dtype (&type/deref ?id)
                      ;; dtype* (&type/actual-type dtype)
                      ]
                  (|case dtype
                    (&/$BoundT ?vname)
                    (return (&/T _expr exo-type))
                    
                    (&/$ExT _)
                    (return (&/T _expr exo-type))

                    (&/$VarT ?_id)
                    (|do [?? (&type/bound? ?_id)]
                      ;; (return (&/T _expr exo-type))
                      (if ??
                        (fail (str "[Analyser Error] Can't use type-var in any type-specific way inside polymorphic functions: " ?id ":" _arg " " (&type/show-type dtype)))
                        (return (&/T _expr exo-type)))
                      )

                    _
                    (fail (str "[Analyser Error] Can't use type-var in any type-specific way inside polymorphic functions: " ?id ":" _arg " " (&type/show-type dtype)))))
                (return (&/T _expr exo-type))))))))
    
    _
    (|do [exo-type* (&type/actual-type exo-type)]
      (analyse-lambda* analyse exo-type* ?self ?arg ?body))
    ))

(defn analyse-lambda [analyse exo-type ?self ?arg ?body]
  (|do [output (analyse-lambda** analyse exo-type ?self ?arg ?body)]
    (return (&/|list output))))

(defn analyse-def [analyse compile-token ?name ?value]
  ;; (prn 'analyse-def/BEGIN ?name)
  ;; (when (= "PList/Dict" ?name)
  ;;   (prn 'DEF ?name (&/show-ast ?value)))
  (|do [module-name &/get-module-name
        ;; :let [_ (println 'DEF/PRE (str module-name ";" ?name))]
        ? (&&module/defined? module-name ?name)]
    (if ?
      (fail (str "[Analyser Error] Can't redefine " (str module-name ";" ?name)))
      (|do [=value (&/with-scope ?name
                     (analyse-1+ analyse ?value))
            =value-type (&&/expr-type =value)]
        (|case =value
          [(&/$Global ?r-module ?r-name) _]
          (|do [_ (&&module/def-alias module-name ?name ?r-module ?r-name =value-type)
                ;; :let [_ (println 'analyse-def/ALIAS (str module-name ";" ?name) '=> (str ?r-module ";" ?r-name))
                ;;       _ (println)]
                ]
            (return (&/|list)))

          _
          (do ;; (println 'DEF (str module-name ";" ?name))
            (|do [_ (compile-token (&/V &&/$def (&/T ?name =value)))
                  :let [;; _ (println 'DEF/COMPILED (str module-name ";" ?name))
                        _ (println 'DEF (str module-name ";" ?name))]]
              (return (&/|list)))))
        ))))

(defn analyse-declare-macro [analyse compile-token ?name]
  (|do [;; :let [_ (prn 'analyse-declare-macro ?name "0")]
        module-name &/get-module-name
        ;; :let [_ (prn 'analyse-declare-macro ?name "1")]
        _ (compile-token (&/V &&/$declare-macro (&/T module-name ?name)))
        ;; :let [_ (prn 'analyse-declare-macro ?name "2")]
        ]
    (return (&/|list))))

(defn ensure-undeclared-tags [module tags]
  (|do [;; :let [_ (prn 'ensure-undeclared-tags/_0)]
        tags-table (&&module/tags-by-module module)
        ;; :let [_ (prn 'ensure-undeclared-tags/_1)]
        _ (&/map% (fn [tag]
                    (if (&/|get tag tags-table)
                      (fail (str "[Analyser Error] Can't re-declare tag: " (&/ident->text (&/T module tag))))
                      (return nil)))
                  tags)
        ;; :let [_ (prn 'ensure-undeclared-tags/_2)]
        ]
    (return nil)))

(defn analyse-declare-tags [tags]
  (|do [;; :let [_ (prn 'analyse-declare-tags/_0)]
        module-name &/get-module-name
        ;; :let [_ (prn 'analyse-declare-tags/_1)]
        _ (ensure-undeclared-tags module-name tags)
        ;; :let [_ (prn 'analyse-declare-tags/_2)]
        _ (&&module/declare-tags module-name tags)
        ;; :let [_ (prn 'analyse-declare-tags/_3)]
        ]
    (return (&/|list))))

(defn analyse-import [analyse compile-module compile-token ?path]
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

(defn analyse-export [analyse compile-token name]
  (|do [module-name &/get-module-name
        _ (&&module/export module-name name)]
    (return (&/|list))))

(defn analyse-alias [analyse compile-token ex-alias ex-module]
  (|do [module-name &/get-module-name
        _ (&&module/alias module-name ex-alias ex-module)]
    (return (&/|list))))

(defn analyse-check [analyse eval! exo-type ?type ?value]
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ==type (eval! =type)
        _ (&type/check exo-type ==type)
        =value (&&/analyse-1 analyse ==type ?value)]
    (return (&/|list (&/T (&/V &&/$ann (&/T =value =type))
                          ==type)))))

(defn analyse-coerce [analyse eval! exo-type ?type ?value]
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ==type (eval! =type)
        _ (&type/check exo-type ==type)
        =value (analyse-1+ analyse ?value)]
    (return (&/|list (&/T (&/V &&/$ann (&/T =value =type))
                          ==type)))))
