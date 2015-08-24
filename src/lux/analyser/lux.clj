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
            (lux [base :as & :refer [|do return return* fail fail* |let |list |case $$]]
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
        (return (&/P ?item =type))))))

(defn ^:private with-cursor [cursor form]
  (|case form
    [_ syntax]
    (&/P cursor syntax)))

;; [Exports]
(defn analyse-tuple [analyse exo-type ?elems]
  ;; (prn 'analyse-tuple/_0 (&type/show-type exo-type) (->> ?elems (&/|map &/show-ast) (&/->seq)))
  (|case ?elems
    (&/$Nil)
    (|do [_ (&type/check exo-type &type/Unit)]
      (return (&/|list (&/P (&/S &&/$unit nil)
                            exo-type))))

    (&/$Cons single (&/$Nil))
    (fail (str "Tuples can't have only 1 element: " (&/show-ast single)))

    (&/$Cons head tail)
    (|do [exo-type* (&type/actual-type exo-type)
          ;; :let [_ (prn 'analyse-tuple/_0.25_0 (&/show-ast head) (&/adt->text exo-type*))
          ;;       _ (prn 'analyse-tuple/_0.25_1 (&/show-ast head) (&type/show-type exo-type*))]
          ]
      (|case exo-type*
        (&/$ProdT ?left ?right)
        (|do [;; :let [_ (prn 'analyse-tuple/_0.5 (&/show-ast head) (&type/show-type ?left))]
              =left (&&/analyse-1 analyse ?left head)
              ;; :let [_ (prn 'analyse-tuple/_1 =left (&type/show-type ?left))]
              =right (|case tail
                       (&/$Nil)
                       (fail "Tuples has wrong size.")

                       (&/$Cons single (&/$Nil))
                       (&&/analyse-1 analyse ?right single)

                       _
                       (&/ensure-1 (analyse-tuple analyse ?right tail)))
              ;; :let [_ (prn 'analyse-tuple/_2 =right (&type/show-type ?right))]
              ]
          (return (&/|list (&/P (&/S &&/$prod (&/P =left =right))
                                exo-type))))

        (&/$AllT _)
        (&type/with-var
          (fn [$var]
            (|do [exo-type** (&type/apply-type exo-type* $var)]
              (analyse-tuple analyse exo-type** ?elems))))

        _
        (fail (str "[Analyser Error] Tuples require tuple-types: " (&type/show-type exo-type*)))))
    ))

(defn analyse-variant [analyse exo-type idx ?values]
  ;; (prn 'analyse-variant/_0
  ;;      (&type/show-type exo-type)
  ;;      idx
  ;;      (->> ?values (&/|map &/show-ast) (&/->seq)))
  (|do [exo-type* (|case exo-type
                    (&/$VarT ?id)
                    (&/try-all% (&/|list (|do [exo-type* (&type/deref ?id)]
                                           (&type/actual-type exo-type*))
                                         (|do [_ (&type/set-var ?id &type/Type)]
                                           (&type/actual-type &type/Type))))

                    _
                    (&type/actual-type exo-type))]
    (|case exo-type*
      (&/$AllT _)
      (&type/with-var
        (fn [$var]
          (|do [exo-type** (&type/apply-type exo-type* $var)]
            (analyse-variant analyse exo-type** idx ?values))))

      ?variant
      (|do [;; :let [_ (prn 'analyse-variant/_1
            ;;              (&type/show-type ?variant)
            ;;              idx
            ;;              (->> ?values (&/|map &/show-ast) (&/->seq)))]
            vtype (&type/variant-case idx ?variant)
            ;; :let [_ (prn 'analyse-variant/_2
            ;;              idx
            ;;              (&type/show-type vtype))]
            =value (&/ensure-1 (|case ?values
                                 (&/$Nil)
                                 (analyse-tuple analyse vtype (&/|list))

                                 (&/$Cons ?value (&/$Nil))
                                 (analyse vtype ?value)

                                 _
                                 (analyse-tuple analyse vtype ?values)))
            ;; :let [_ (prn 'analyse-variant/_3
            ;;              idx
            ;;              =value)]
            ]
        (return (&/|list (&/P (&/S &&/$sum (&/P idx =value))
                              exo-type))))
      )))

(defn analyse-record [analyse exo-type ?elems]
  (|do [members (&&record/order-record ?elems)]
    (analyse-tuple analyse exo-type members)))

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
    (return (&/|list (&/P (&/S &&/$var (&/S &/$Global (&/P r-module r-name)))
                          endo-type)))))

(defn ^:private analyse-local [analyse exo-type name]
  (fn [state]
    (|let [stack (&/$get-envs state)
           no-binding? #(do ;; (prn 'analyse-local/_ (->> % &/adt->text))
                          ;; (prn 'analyse-local/_1 (->> % (&/$get-locals) &/adt->text))
                          ;; (prn 'analyse-local/_2 (->> % (&/$get-closure) &/adt->text))
                          (and (->> % (&/$get-locals)  (&/$get-mappings) (&/|contains? name) not)
                               (->> % (&/$get-closure) (&/$get-mappings) (&/|contains? name) not)))
           [inner outer] (&/|split-with no-binding? stack)]
      (|case outer
        (&/$Nil)
        (&/run-state (|do [module-name &/get-module-name]
                       (analyse-global analyse exo-type module-name name))
                     state)

        (&/$Cons ?genv (&/$Nil))
        (do ;; (prn 'analyse-symbol/_2 ?module name name (->> ?genv (&/$get-locals) (&/$get-mappings) &/|keys &/->seq))
            (if-let [global (->> ?genv (&/$get-locals) (&/$get-mappings) (&/|get name))]
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
                       (return (&/|list (&/P (&/S &&/$var (&/S &/$Global (&/P r-module r-name)))
                                             endo-type))))
                     state)

                    _
                    (fail* "[Analyser Error] Can't have anything other than a global def in the global environment.")))
              (fail* "_{_ analyse-symbol _}_")))
        
        (&/$Cons top-outer _)
        (do ;; (prn 'analyse-symbol/_3 ?module name)
            (|let [scopes (&/|tail (&/folds #(&/Cons$ (&/$get-name %2) %1)
                                            (&/|map #(&/$get-name %) outer)
                                            (&/|reverse inner)))
                   [=local inner*] (&/fold2 (fn [register+new-inner frame in-scope]
                                              (|let [[register new-inner] register+new-inner
                                                     [register* frame*] (&&lambda/close-over (&/|reverse in-scope) name register frame)]
                                                (&/P register* (&/Cons$ frame* new-inner))))
                                            (&/P (or (->> top-outer (&/$get-locals)  (&/$get-mappings) (&/|get name))
                                                     (->> top-outer (&/$get-closure) (&/$get-mappings) (&/|get name)))
                                                 (&/|list))
                                            (&/|reverse inner) scopes)]
              ((|do [btype (&&/expr-type =local)
                     ;; :let [_ (prn 'analyse-local/_0 name)
                     ;;       _ (prn 'analyse-local/_1 name (&type/show-type exo-type) (&type/show-type btype))]
                     _ (&type/check exo-type btype)
                     ;; :let [_ (prn 'analyse-local/_2 name 'CHECKED)]
                     ]
                 (return (&/|list =local)))
               (&/$set-envs (&/|++ inner* outer) state))))
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
      (return (&/P fun-type (&/|list))))
    
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
                               (|do [_ (&type/set-var ?id (&/S &/$BoundT _aarg))]
                                 (&type/clean $var =output-t)))]
                  (return (&/P type** =args)))
                ))))

        (&/$LambdaT ?input-t ?output-t)
        (|do [[=output-t =args] (analyse-apply* analyse exo-type ?output-t ?args*)
              =arg (&&/analyse-1 analyse ?input-t ?arg)]
          (return (&/P =output-t (&/Cons$ =arg =args))))

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
                  :let [_ (when (or (= "using" (aget real-name 1))
                                    ;; (= "type" (aget real-name 1))
                                    ;; (= &&/$struct r-name)
                                    )
                            (->> (&/|map &/show-ast macro-expansion)
                                 (&/|interpose "\n")
                                 (&/fold str "")
                                 (prn (&/ident->text real-name))))]
                  ]
              (&/flat-map% (partial analyse exo-type) macro-expansion))

            _
            (|do [[=output-t =args] (analyse-apply* analyse exo-type =fn-type ?args)]
              (return (&/|list (&/P (&/S &&/$apply (&/P =fn =args))
                                    =output-t))))))
        
        _
        (|do [[=output-t =args] (analyse-apply* analyse exo-type =fn-type ?args)]
          (return (&/|list (&/P (&/S &&/$apply (&/P =fn =args))
                                =output-t)))))
      )))

(defn analyse-case [analyse exo-type ?value ?branches]
  (|do [:let [num-branches (&/|length ?branches)]
        _ (&/assert! (> num-branches 0) "[Analyser Error] Can't have empty branches in \"case'\" expression.")
        _ (&/assert! (even? num-branches) "[Analyser Error] Unbalanced branches in \"case'\" expression.")
        =value (analyse-1+ analyse ?value)
        =value-type (&&/expr-type =value)
        =match (&&case/analyse-branches analyse exo-type =value-type (&/|as-pairs ?branches))]
    (return (&/|list (&/P (&/S &&/$case (&/P =value =match))
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
        (return (&/P (&/S &&/$lambda ($$ &/P =scope =captured =body)) exo-type*)))
      
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
                    (return (&/P _expr exo-type))
                    
                    (&/$ExT _)
                    (return (&/P _expr exo-type))

                    (&/$VarT ?_id)
                    (|do [?? (&type/bound? ?_id)]
                      ;; (return (&/P _expr exo-type))
                      (if ??
                        (fail (str "[Analyser Error] Can't use type-var in any type-specific way inside polymorphic functions: " ?id ":" _arg " " (&type/show-type dtype)))
                        (return (&/P _expr exo-type)))
                      )

                    _
                    (fail (str "[Analyser Error] Can't use type-var in any type-specific way inside polymorphic functions: " ?id ":" _arg " " (&type/show-type dtype)))))
                (return (&/P _expr exo-type))))))))
    
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
          [(&&/$var (&/$Global ?r-module ?r-name)) _]
          (|do [_ (&&module/def-alias module-name ?name ?r-module ?r-name =value-type)
                ;; :let [_ (println 'analyse-def/ALIAS (str module-name ";" ?name) '=> (str ?r-module ";" ?r-name))
                ;;       _ (println)]
                ]
            (return (&/|list)))

          _
          (do ;; (println 'DEF (str module-name ";" ?name))
              (|do [_ (compile-token (&/S &&/$def (&/P ?name =value)))
                    :let [;; _ (println 'DEF/COMPILED (str module-name ";" ?name))
                          _ (println 'DEF (str module-name ";" ?name))]]
                (return (&/|list)))))
        ))))

(defn analyse-declare-macro [analyse compile-token ?name]
  (|do [;; :let [_ (prn 'analyse-declare-macro ?name "0")]
        module-name &/get-module-name
        ;; :let [_ (prn 'analyse-declare-macro ?name "1")]
        _ (compile-token (&/S &&/$declare-macro (&/P module-name ?name)))
        ;; :let [_ (prn 'analyse-declare-macro ?name "2")]
        ]
    (return (&/|list))))

(defn analyse-declare-tags [tags type-name]
  (|do [module-name &/get-module-name
        ;; :let [_ (prn 'analyse-declare-tags (&/ident->text (&/P module-name type-name)) (&/->seq tags))]
        [_ def-data] (&&module/find-def module-name type-name)
        ;; :let [_ (prn 'analyse-declare-tags (&/ident->text (&/P module-name type-name)) (&/->seq tags) (&/adt->text def-data))]
        def-type (&&module/ensure-type-def def-data)
        _ (&&module/declare-tags module-name tags def-type)]
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
    (return (&/|list (&/P (&/S &&/$ann (&/P =value =type))
                          ==type)))))

(defn analyse-coerce [analyse eval! exo-type ?type ?value]
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ==type (eval! =type)
        _ (&type/check exo-type ==type)
        =value (analyse-1+ analyse ?value)]
    (return (&/|list (&/P (&/S &&/$ann (&/P =value =type))
                          ==type)))))
