;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

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

;; [Utils]
(defn ^:private count-univq [type]
  "(-> Type Int)"
  (|case type
    (&/$UnivQ env type*)
    (inc (count-univq type*))

    _
    0))

(defn ^:private next-bound-type [type]
  "(-> Type Type)"
  (&type/Bound$ (->> (count-univq type) (* 2) (+ 1))))

(defn ^:private embed-inferred-input [input output]
  "(-> Type Type Type)"
  (|case output
    (&/$UnivQ env output*)
    (&type/Univ$ env (embed-inferred-input input output*))

    _
    (&type/Lambda$ input output)))

;; [Exports]
(defn analyse-tuple [analyse ?exo-type ?elems]
  (|case ?exo-type
    (&/$Left exo-type)
    (|do [;; :let [_ (println 'analyse-tuple/$Left (&type/show-type exo-type))]
          exo-type* (&type/actual-type exo-type)]
      (|case exo-type*
        (&/$UnivQ _)
        (&type/with-var
          (fn [$var]
            (|do [exo-type** (&type/apply-type exo-type* $var)
                  [tuple-analysis tuple-type] (&&/cap-1 (analyse-tuple analyse (&/V &/$Left exo-type**) ?elems))
                  =var (&type/resolve-type $var)
                  inferred-type (|case =var
                                  (&/$VarT iid)
                                  (|do [:let [=var* (next-bound-type tuple-type)]
                                        _ (&type/set-var iid =var*)
                                        tuple-type* (&type/clean $var tuple-type)]
                                    (return (&type/Univ$ &/Nil$ tuple-type*)))

                                  _
                                  (&type/clean $var tuple-type))]
              (return (&/|list (&/T tuple-analysis inferred-type))))))

        _
        (analyse-tuple analyse (&/V &/$Right exo-type*) ?elems)))

    (&/$Right exo-type)
    (|do [unknown? (&type/unknown? exo-type)]
      (if unknown?
        (|do [=elems (&/map% #(|do [=analysis (&&/analyse-1+ analyse %)]
                                (return =analysis))
                             ?elems)
              _ (&type/check exo-type (&/V &/$TupleT (&/|map &&/expr-type* =elems)))]
          (return (&/|list (&/T (&/V &&/$tuple =elems)
                                exo-type))))
        (|do [exo-type* (&type/actual-type exo-type)]
          (|case exo-type*
            (&/$TupleT ?members)
            (|do [=elems (&/map2% (fn [elem-t elem]
                                    (&&/analyse-1 analyse elem-t elem))
                                  ?members ?elems)]
              (return (&/|list (&/T (&/V &&/$tuple =elems)
                                    exo-type))))

            (&/$UnivQ _)
            (|do [$var &type/existential
                  exo-type** (&type/apply-type exo-type* $var)
                  [tuple-analysis tuple-type] (&&/cap-1 (analyse-tuple analyse (&/V &/$Right exo-type**) ?elems))]
              (return (&/|list (&/T tuple-analysis exo-type))))

            _
            (fail (str "[Analyser Error] Tuples require tuple-types: " (&type/show-type exo-type*) " " (&type/show-type exo-type) " " "[" (->> ?elems (&/|map &/show-ast) (&/|interpose " ") (&/fold str "")) "]"))
            ;; (assert false (str "[Analyser Error] Tuples require tuple-types: " (&type/show-type exo-type*) " " (&type/show-type exo-type) " " "[" (->> ?elems (&/|map &/show-ast) (&/|interpose " ") (&/fold str "")) "]"))
            ))))))

(defn with-attempt [m-value on-error]
  (fn [state]
    (|case (m-value state)
      (&/$Left msg)
      ((on-error msg) state)
      
      output
      output)))

(defn ^:private analyse-variant-body [analyse exo-type ?values]
  (|do [output (with-attempt
                 (|case ?values
                   (&/$Nil)
                   (analyse-tuple analyse (&/V &/$Right exo-type) &/Nil$)

                   (&/$Cons ?value (&/$Nil))
                   (analyse exo-type ?value)

                   _
                   (analyse-tuple analyse (&/V &/$Right exo-type) ?values))
                 (fn [err]
                   (fail (str err "\n"
                              'analyse-variant-body " " (&type/show-type exo-type)
                              " " (->> ?values (&/|map &/show-ast) (&/|interpose " ") (&/fold str ""))))
                   ;; (assert false
                   ;;         (str err "\n"
                   ;;              'analyse-variant-body " " (&type/show-type exo-type)
                   ;;              " " (->> ?values (&/|map &/show-ast) (&/|interpose " ") (&/fold str ""))))
                   ))]
    (|case output
      (&/$Cons x (&/$Nil))
      (return x)

      _
      (fail "[Analyser Error] Can't expand to other than 1 element."))))

(defn analyse-variant [analyse ?exo-type idx ?values]
  (|case ?exo-type
    (&/$Left exo-type)
    (|do [;; :let [_ (println 'analyse-variant/Left 0 (&type/show-type exo-type))]
          exo-type* (&type/actual-type exo-type)
          ;; :let [_ (println 'analyse-variant/Left 1 (&type/show-type exo-type*))]
          ]
      (|case exo-type*
        (&/$UnivQ _)
        (&type/with-var
          (fn [$var]
            (|do [exo-type** (&type/apply-type exo-type* $var)
                  ;; :let [_ (println 'analyse-variant/Left 2 (&type/show-type exo-type**))]
                  [variant-analysis variant-type] (&&/cap-1 (analyse-variant analyse (&/V &/$Left exo-type**) idx ?values))
                  ;; :let [_ (println 'analyse-variant/Left 3 (&type/show-type variant-type))]
                  =var (&type/resolve-type $var)
                  ;; :let [_ (println 'analyse-variant/Left 4 (&type/show-type =var))]
                  inferred-type (|case =var
                                  (&/$VarT iid)
                                  (|do [:let [=var* (next-bound-type variant-type)]
                                        _ (&type/set-var iid =var*)
                                        variant-type* (&type/clean $var variant-type)]
                                    (return (&type/Univ$ &/Nil$ variant-type*)))

                                  _
                                  (&type/clean $var variant-type))
                  ;; :let [_ (println 'analyse-variant/Left 5 (&type/show-type inferred-type))]
                  ]
              (return (&/|list (&/T variant-analysis inferred-type))))))

        _
        (analyse-variant analyse (&/V &/$Right exo-type*) idx ?values)))

    (&/$Right exo-type)
    ;; [_ exo-type]
    (|do [;; :let [_ (println 'analyse-variant/Right 0 (&type/show-type exo-type))]
          exo-type* (|case exo-type
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
          (|do [=value (with-attempt
                         (analyse-variant-body analyse vtype ?values)
                         (fn [err]
                           (|do [_exo-type (&type/deref+ exo-type)]
                             (fail (str err "\n"
                                        'analyse-variant " " idx " " (&type/show-type exo-type) " " (&type/show-type _exo-type)
                                        " " (->> ?values (&/|map &/show-ast) (&/|interpose " ") (&/fold str "")))))))]
            (return (&/|list (&/T (&/V &&/$variant (&/T idx =value))
                                  exo-type))))

          (&/$None)
          (fail (str "[Analyser Error] There is no case " idx " for variant type " (&type/show-type exo-type*))))

        (&/$UnivQ _)
        (|do [$var &type/existential
              exo-type** (&type/apply-type exo-type* $var)]
          (analyse-variant analyse (&/V &/$Right exo-type**) idx ?values))
        
        _
        (fail (str "[Analyser Error] Can't create a variant if the expected type is " (&type/show-type exo-type*)))))))

(defn analyse-record [analyse exo-type ?elems]
  (|do [[rec-members rec-type] (&&record/order-record ?elems)]
    (|case exo-type
      (&/$VarT id)
      (|do [? (&type/bound? id)]
        (if ?
          (analyse-tuple analyse (&/V &/$Right exo-type) rec-members)
          (|do [[tuple-analysis tuple-type] (&&/cap-1 (analyse-tuple analyse (&/V &/$Left rec-type) rec-members))
                _ (&type/check exo-type tuple-type)]
            (return (&/|list (&/T tuple-analysis exo-type))))))

      _
      (analyse-tuple analyse (&/V &/$Right exo-type) rec-members)
      )))

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
            (|let [scopes (&/|tail (&/folds #(&/Cons$ (&/get$ &/$name %2) %1)
                                            (&/|map #(&/get$ &/$name %) outer)
                                            (&/|reverse inner)))
                   [=local inner*] (&/fold2 (fn [register+new-inner frame in-scope]
                                              (|let [[register new-inner] register+new-inner
                                                     [register* frame*] (&&lambda/close-over (&/|reverse in-scope) name register frame)]
                                                (&/T register* (&/Cons$ frame* new-inner))))
                                            (&/T (or (->> top-outer (&/get$ &/$locals)  (&/get$ &/$mappings) (&/|get name))
                                                     (->> top-outer (&/get$ &/$closure) (&/get$ &/$mappings) (&/|get name)))
                                                 &/Nil$)
                                            (&/|reverse inner) scopes)]
              ((|do [_ (&type/check exo-type (&&/expr-type* =local))]
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
    (|do [;; :let [_ (prn 'analyse-apply*/_0 (&type/show-type exo-type) (&type/show-type fun-type))]
          _ (&type/check exo-type fun-type)
          ;; :let [_ (prn 'analyse-apply*/_1 'SUCCESS (str "(_ " (->> ?args (&/|map &/show-ast) (&/|interpose " ") (&/fold str "")) ")"))]
          ]
      (return (&/T fun-type &/Nil$)))
    
    (&/$Cons ?arg ?args*)
    (|do [?fun-type* (&type/actual-type fun-type)]
      (|case ?fun-type*
        (&/$UnivQ _)
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
                               (|do [_ (&type/set-var ?id (&/V &/$BoundT 1))]
                                 (&type/clean $var =output-t)))]
                  (return (&/T type** =args)))
                ))))

        (&/$LambdaT ?input-t ?output-t)
        (|do [[=output-t =args] (analyse-apply* analyse exo-type ?output-t ?args*)
              =arg (with-attempt
                     (&&/analyse-1 analyse ?input-t ?arg)
                     (fn [err]
                       (fail (str err "\n"
                                  'analyse-apply* " " (&type/show-type exo-type) " " (&type/show-type ?fun-type*)
                                  " " "(_ " (->> ?args (&/|map &/show-ast) (&/|interpose " ") (&/fold str "")) ")"))))]
          (return (&/T =output-t (&/Cons$ =arg =args))))

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
                  ;; :let [_ (when (or (= "zip" (aget real-name 1))
                  ;;                   ;; (= "..?" (aget real-name 1))
                  ;;                   ;; (= "try$" (aget real-name 1))
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
        =value (&&/analyse-1+ analyse ?value)
        =match (&&case/analyse-branches analyse exo-type (&&/expr-type* =value) (&/|as-pairs ?branches))]
    (return (&/|list (&/T (&/V &&/$case (&/T =value =match))
                          exo-type)))))

(defn analyse-lambda* [analyse exo-type ?self ?arg ?body]
  (|case exo-type
    (&/$VarT id)
    (|do [? (&type/bound? id)]
      (if ?
        (|do [exo-type* (&type/deref id)]
          (analyse-lambda* analyse exo-type* ?self ?arg ?body))
        ;; Inference
        (&type/with-var
          (fn [$input]
            (&type/with-var
              (fn [$output]
                (|do [[lambda-analysis lambda-type] (analyse-lambda* analyse (&type/Lambda$ $input $output) ?self ?arg ?body)
                      =input (&type/resolve-type $input)
                      =output (&type/resolve-type $output)
                      inferred-type (|case =input
                                      (&/$VarT iid)
                                      (|do [:let [=input* (next-bound-type =output)]
                                            _ (&type/set-var iid =input*)
                                            =output* (&type/clean $input =output)
                                            =output** (&type/clean $output =output*)]
                                        (return (&type/Univ$ &/Nil$ (embed-inferred-input =input* =output**))))

                                      _
                                      (|do [=output* (&type/clean $input =output)
                                            =output** (&type/clean $output =output*)]
                                        (return (embed-inferred-input =input =output**))))
                      _ (&type/check exo-type inferred-type)
                      ]
                  (return (&/T lambda-analysis inferred-type)))
                ))))))

    _
    (|do [exo-type* (&type/actual-type exo-type)]
      (|case exo-type
        (&/$UnivQ _)
        (|do [$var &type/existential
              exo-type** (&type/apply-type exo-type* $var)]
          (analyse-lambda* analyse exo-type** ?self ?arg ?body))
        
        (&/$LambdaT ?arg-t ?return-t)
        (|do [[=scope =captured =body] (&&lambda/with-lambda ?self exo-type*
                                         ?arg ?arg-t
                                         (&&/analyse-1 analyse ?return-t ?body))]
          (return (&/T (&/V &&/$lambda (&/T =scope =captured =body)) exo-type*)))

        
        
        _
        (fail (str "[Analyser Error] Functions require function types: "
                   (&type/show-type exo-type*)))))
    ))

(defn analyse-lambda** [analyse exo-type ?self ?arg ?body]
  (|case exo-type
    (&/$UnivQ _)
    (|do [$var &type/existential
          exo-type* (&type/apply-type exo-type $var)
          [_expr _] (analyse-lambda** analyse exo-type* ?self ?arg ?body)]
      (return (&/T _expr exo-type)))

    (&/$VarT id)
    (|do [? (&type/bound? id)]
      (if ?
        (|do [exo-type* (&type/actual-type exo-type)]
          (analyse-lambda* analyse exo-type* ?self ?arg ?body))
        ;; Inference
        (analyse-lambda* analyse exo-type ?self ?arg ?body)))
    
    _
    (|do [exo-type* (&type/actual-type exo-type)]
      (analyse-lambda* analyse exo-type* ?self ?arg ?body))
    ))

(defn analyse-lambda [analyse exo-type ?self ?arg ?body]
  (|do [output (analyse-lambda** analyse exo-type ?self ?arg ?body)]
    (return (&/|list output))))

(defn analyse-def [analyse compile-token ?name ?value]
  ;; (prn 'analyse-def/BEGIN ?name)
  ;; (when (= "monoid$" ?name)
  ;;   (reset! &type/!flag true))
  (|do [module-name &/get-module-name
        ;; :let [_ (println 'DEF/PRE (str module-name ";" ?name))]
        ? (&&module/defined? module-name ?name)]
    (if ?
      (fail (str "[Analyser Error] Can't redefine " (str module-name ";" ?name)))
      (|do [=value (&/with-scope ?name
                     (&&/analyse-1+ analyse ?value))]
        (|case =value
          [(&&/$var (&/$Global ?r-module ?r-name)) _]
          (|do [_ (&&module/def-alias module-name ?name ?r-module ?r-name (&&/expr-type* =value))
                ;; :let [_ (println 'analyse-def/ALIAS (str module-name ";" ?name) '=> (str ?r-module ";" ?r-name))
                ;;       _ (println)]
                ]
            (return &/Nil$))

          _
          (do ;; (println 'DEF (str module-name ";" ?name))
              (|do [_ (compile-token (&/V &&/$def (&/T ?name =value)))
                    ;; _ (if (and (= "lux" module-name)
                    ;;            (= "Type" ?name))
                    ;;     (|do [newly-defined-Type 
                    ;;           :let [_ (&type/redefine-type! newly-defined-Type)]]
                    ;;       (return nil))
                    ;;     (return nil))
                    :let [;; _ (println 'DEF/COMPILED (str module-name ";" ?name))
                          [def-analysis def-type] =value
                          _ (println 'DEF (str module-name ";" ?name) ;; (&type/show-type def-type)
                                     )]]
                (return &/Nil$))))
        ))))

(defn analyse-declare-macro [analyse compile-token ?name]
  (|do [;; :let [_ (prn 'analyse-declare-macro ?name "0")]
        module-name &/get-module-name
        ;; :let [_ (prn 'analyse-declare-macro ?name "1")]
        _ (compile-token (&/V &&/$declare-macro (&/T module-name ?name)))
        ;; :let [_ (prn 'analyse-declare-macro ?name "2")]
        ]
    (return &/Nil$)))

(defn analyse-declare-tags [tags type-name]
  (|do [module-name &/get-module-name
        ;; :let [_ (prn 'analyse-declare-tags (&/ident->text (&/T module-name type-name)) (&/->seq tags))]
        [_ def-data] (&&module/find-def module-name type-name)
        ;; :let [_ (prn 'analyse-declare-tags (&/ident->text (&/T module-name type-name)) (&/->seq tags) (&/adt->text def-data))]
        def-type (&&module/ensure-type-def def-data)
        _ (&&module/declare-tags module-name tags def-type)]
    (return &/Nil$)))

(defn analyse-import [analyse compile-module compile-token path]
  ;; (prn 'analyse-import path)
  (|do [module-name &/get-module-name
        _ (if (= module-name path)
            (fail (str "[Analyser Error] Module can't import itself: " path))
            (return nil))]
    (&/save-module
     (|do [already-compiled? (&&module/exists? path)
           ;; :let [_ (prn 'analyse-import module-name path already-compiled?)]
           _ (&&module/add-import path)
           _ (&/when% (not already-compiled?) (compile-module path))]
       (return &/Nil$)))))

(defn analyse-export [analyse compile-token name]
  (|do [module-name &/get-module-name
        _ (&&module/export module-name name)]
    (return &/Nil$)))

(defn analyse-alias [analyse compile-token ex-alias ex-module]
  (|do [module-name &/get-module-name
        _ (&&module/alias module-name ex-alias ex-module)]
    (return &/Nil$)))

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
        =value (&&/analyse-1+ analyse ?value)]
    (return (&/|list (&/T (&/V &&/$ann (&/T =value =type))
                          ==type)))))
