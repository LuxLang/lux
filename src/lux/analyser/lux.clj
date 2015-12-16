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
    (|do [exo-type* (&type/actual-type exo-type)]
      (|case exo-type*
        (&/$UnivQ _)
        (&type/with-var
          (fn [$var]
            (|do [exo-type** (&type/apply-type exo-type* $var)
                  [[tuple-type tuple-cursor] tuple-analysis] (&&/cap-1 (analyse-tuple analyse (&/V &/$Left exo-type**) ?elems))
                  =var (&type/resolve-type $var)
                  inferred-type (|case =var
                                  (&/$VarT iid)
                                  (|do [:let [=var* (next-bound-type tuple-type)]
                                        _ (&type/set-var iid =var*)
                                        tuple-type* (&type/clean $var tuple-type)]
                                    (return (&type/Univ$ &/Nil$ tuple-type*)))

                                  _
                                  (&type/clean $var tuple-type))]
              (return (&/|list (&&/|meta inferred-type tuple-cursor
                                         tuple-analysis))))))

        _
        (analyse-tuple analyse (&/V &/$Right exo-type*) ?elems)))

    (&/$Right exo-type)
    (|do [unknown? (&type/unknown? exo-type)]
      (if unknown?
        (|do [=elems (&/map% #(|do [=analysis (&&/analyse-1+ analyse %)]
                                (return =analysis))
                             ?elems)
              _ (&type/check exo-type (&/V &/$TupleT (&/|map &&/expr-type* =elems)))
              _cursor &/cursor]
          (return (&/|list (&&/|meta exo-type _cursor
                                     (&/V &&/$tuple =elems)
                                     ))))
        (|do [exo-type* (&type/actual-type exo-type)]
          (&/with-attempt
            (|case exo-type*
              (&/$TupleT ?members)
              (|do [=elems (&/map2% (fn [elem-t elem]
                                      (&&/analyse-1 analyse elem-t elem))
                                    ?members ?elems)
                    _cursor &/cursor]
                (return (&/|list (&&/|meta exo-type _cursor
                                           (&/V &&/$tuple =elems)
                                           ))))

              (&/$UnivQ _)
              (|do [$var &type/existential
                    exo-type** (&type/apply-type exo-type* $var)
                    [[tuple-type tuple-cursor] tuple-analysis] (&&/cap-1 (analyse-tuple analyse (&/V &/$Right exo-type**) ?elems))]
                (return (&/|list (&&/|meta exo-type tuple-cursor
                                           tuple-analysis))))

              _
              (fail (str "[Analyser Error] Tuples require tuple-types: " (&type/show-type exo-type*)))
              )
            (fn [err]
              (fail (str err "\n" "[Analyser Error] Tuples require tuple-types: " (&type/show-type exo-type))))))))))

(defn ^:private analyse-variant-body [analyse exo-type ?values]
  (|do [output (&/with-attempt
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
                   ))]
    (|case output
      (&/$Cons x (&/$Nil))
      (return x)

      _
      (fail "[Analyser Error] Can't expand to other than 1 element."))))

(defn analyse-variant [analyse ?exo-type idx ?values]
  (|case ?exo-type
    (&/$Left exo-type)
    (|do [exo-type* (&type/actual-type exo-type)]
      (|case exo-type*
        (&/$UnivQ _)
        (&type/with-var
          (fn [$var]
            (|do [exo-type** (&type/apply-type exo-type* $var)
                  [[variant-type variant-cursor] variant-analysis] (&&/cap-1 (analyse-variant analyse (&/V &/$Left exo-type**) idx ?values))
                  =var (&type/resolve-type $var)
                  inferred-type (|case =var
                                  (&/$VarT iid)
                                  (|do [:let [=var* (next-bound-type variant-type)]
                                        _ (&type/set-var iid =var*)
                                        variant-type* (&type/clean $var variant-type)]
                                    (return (&type/Univ$ &/Nil$ variant-type*)))

                                  _
                                  (&type/clean $var variant-type))]
              (return (&/|list (&&/|meta inferred-type variant-cursor
                                         variant-analysis))))))

        _
        (analyse-variant analyse (&/V &/$Right exo-type*) idx ?values)))

    (&/$Right exo-type)
    (|do [exo-type* (|case exo-type
                      (&/$VarT ?id)
                      (&/try-all% (&/|list (|do [exo-type* (&type/deref ?id)]
                                             (&type/actual-type exo-type*))
                                           (|do [_ (&type/set-var ?id &type/Type)]
                                             (&type/actual-type &type/Type))))

                      _
                      (&type/actual-type exo-type))]
      (&/with-attempt
        (|case exo-type*
          (&/$VariantT ?cases)
          (|case (&/|at idx ?cases)
            (&/$Some vtype)
            (|do [=value (&/with-attempt
                           (analyse-variant-body analyse vtype ?values)
                           (fn [err]
                             (|do [_exo-type (&type/deref+ exo-type)]
                               (fail (str err "\n"
                                          'analyse-variant " " idx " " (&type/show-type exo-type) " " (&type/show-type _exo-type)
                                          " " (->> ?values (&/|map &/show-ast) (&/|interpose " ") (&/fold str "")))))))
                  _cursor &/cursor]
              (return (&/|list (&&/|meta exo-type _cursor
                                         (&/V &&/$variant (&/T idx =value))
                                         ))))

            (&/$None)
            (fail (str "[Analyser Error] There is no case " idx " for variant type " (&type/show-type exo-type*))))

          (&/$UnivQ _)
          (|do [$var &type/existential
                exo-type** (&type/apply-type exo-type* $var)]
            (analyse-variant analyse (&/V &/$Right exo-type**) idx ?values))

          (&/$ExQ _)
          (&type/with-var
            (fn [$var]
              (|do [exo-type** (&type/apply-type exo-type* $var)]
                (analyse-variant analyse (&/V &/$Right exo-type**) idx ?values))))
          
          _
          (fail (str "[Analyser Error] Can't create variant if the expected type is " (&type/show-type exo-type*))))
        (fn [err]
          (|case exo-type
            (&/$VarT ?id)
            (|do [=exo-type (&type/deref ?id)]
              (fail (str err "\n" "[Analyser Error] Can't create variant if the expected type is " (&type/show-type =exo-type))))

            _
            (fail (str err "\n" "[Analyser Error] Can't create variant if the expected type is " (&type/show-type exo-type))))))
      )))

(defn analyse-record [analyse exo-type ?elems]
  (|do [[rec-members rec-type] (&&record/order-record ?elems)]
    (|case exo-type
      (&/$VarT id)
      (|do [? (&type/bound? id)]
        (if ?
          (analyse-tuple analyse (&/V &/$Right exo-type) rec-members)
          (|do [[[tuple-type tuple-cursor] tuple-analysis] (&&/cap-1 (analyse-tuple analyse (&/V &/$Left rec-type) rec-members))
                _ (&type/check exo-type tuple-type)]
            (return (&/|list (&&/|meta exo-type tuple-cursor
                                       tuple-analysis))))))

      _
      (analyse-tuple analyse (&/V &/$Right exo-type) rec-members)
      )))

(defn ^:private analyse-global [analyse exo-type module name]
  (|do [[[r-module r-name] $def] (&&module/find-def module name)
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
            (&type/check exo-type endo-type))
        _cursor &/cursor]
    (return (&/|list (&&/|meta endo-type _cursor
                               (&/V &&/$var (&/V &/$Global (&/T r-module r-name)))
                               )))))

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
        (if-let [global (->> ?genv (&/get$ &/$locals) (&/get$ &/$mappings) (&/|get name))]
          (|case global
            [(&/$Global ?module* name*) _]
            ((|do [[[r-module r-name] $def] (&&module/find-def ?module* name*)
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
                       (&type/check exo-type endo-type))
                   _cursor &/cursor]
               (return (&/|list (&&/|meta endo-type _cursor
                                          (&/V &&/$var (&/V &/$Global (&/T r-module r-name)))
                                          ))))
             state)

            _
            (fail* "[Analyser Error] Can't have anything other than a global def in the global environment."))
          (fail* (str "[Analyser Error] Unknown global definition: " name)))
        
        (&/$Cons top-outer _)
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
           (&/set$ &/$envs (&/|++ inner* outer) state)))
        ))))

(defn analyse-symbol [analyse exo-type ident]
  (|do [:let [[?module ?name] ident]]
    (if (= "" ?module)
      (analyse-local analyse exo-type ?name)
      (analyse-global analyse exo-type ?module ?name))
    ))

(defn ^:private analyse-apply* [analyse exo-type fun-type ?args]
  (|case ?args
    (&/$Nil)
    (|do [_ (&type/check exo-type fun-type)]
      (return (&/T fun-type &/Nil$)))
    
    (&/$Cons ?arg ?args*)
    (|do [?fun-type* (&type/actual-type fun-type)]
      (&/with-attempt
        (|case ?fun-type*
          (&/$UnivQ _)
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

          (&/$ExQ _)
          (|do [$var &type/existential
                type* (&type/apply-type ?fun-type* $var)]
            (analyse-apply* analyse exo-type type* ?args))

          (&/$LambdaT ?input-t ?output-t)
          (|do [[=output-t =args] (analyse-apply* analyse exo-type ?output-t ?args*)
                =arg (&/with-attempt
                       (&&/analyse-1 analyse ?input-t ?arg)
                       (fn [err]
                         (fail (str err "\n" "[Analyser Error] Function expected: " (&type/show-type ?input-t)))))]
            (return (&/T =output-t (&/Cons$ =arg =args))))

          _
          (fail (str "[Analyser Error] Can't apply a non-function: " (&type/show-type ?fun-type*))))
        (fn [err]
          (fail (str err "\n" "[Analyser Error] Can't apply function " (&type/show-type fun-type) " to args: " (->> ?args (&/|map &/show-ast) (&/|interpose " ") (&/fold str "")))))))
    ))

(defn analyse-apply [analyse exo-type form-cursor =fn ?args]
  (|do [loader &/loader
        :let [[[=fn-type =fn-cursor] =fn-form] =fn]]
    (|case =fn-form
      (&&/$var (&/$Global ?module ?name))
      (|do [[real-name $def] (&&module/find-def ?module ?name)]
        (|case $def
          (&/$MacroD macro)
          (|do [macro-expansion (fn [state] (-> macro (.apply ?args) (.apply state)))
                ;; :let [_ (when (or (= "case" (aget real-name 1))
                ;;                   ;; (= "invoke-static$" (aget real-name 1))
                ;;                   ;; (= "invoke-virtual$" (aget real-name 1))
                ;;                   ;; (= "new$" (aget real-name 1))
                ;;                   ;; (= "let%" (aget real-name 1))
                ;;                   ;; (= "jvm-import" (aget real-name 1))
                ;;                   )
                ;;           (->> (&/|map &/show-ast macro-expansion)
                ;;                (&/|interpose "\n")
                ;;                (&/fold str "")
                ;;                (prn (&/ident->text real-name))))]
                ]
            (&/flat-map% (partial analyse exo-type) macro-expansion))

          _
          (|do [[=output-t =args] (analyse-apply* analyse exo-type =fn-type ?args)]
            (return (&/|list (&&/|meta =output-t =fn-cursor
                                       (&/V &&/$apply (&/T =fn =args))
                                       ))))))
      
      _
      (|do [[=output-t =args] (analyse-apply* analyse exo-type =fn-type ?args)]
        (return (&/|list (&&/|meta =output-t =fn-cursor
                                   (&/V &&/$apply (&/T =fn =args))
                                   )))))
    ))

(defn analyse-case [analyse exo-type ?value ?branches]
  (|do [:let [num-branches (&/|length ?branches)]
        _ (&/assert! (> num-branches 0) "[Analyser Error] Can't have empty branches in \"case'\" expression.")
        _ (&/assert! (even? num-branches) "[Analyser Error] Unbalanced branches in \"case'\" expression.")
        =value (&&/analyse-1+ analyse ?value)
        :let [var?? (|case =value
                      [_ (&&/$var =var-kind)]
                      (&/Some$ =value)

                      _
                      &/None$)]
        =match (&&case/analyse-branches analyse exo-type var?? (&&/expr-type* =value) (&/|as-pairs ?branches))
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&/V &&/$case (&/T =value =match))
                               )))))

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
                (|do [[[lambda-type lambda-cursor] lambda-analysis] (analyse-lambda* analyse (&type/Lambda$ $input $output) ?self ?arg ?body)
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
                      _ (&type/check exo-type inferred-type)]
                  (return (&&/|meta inferred-type lambda-cursor
                                    lambda-analysis)))
                ))))))

    _
    (&/with-attempt
      (|do [exo-type* (&type/actual-type exo-type)]
        (|case exo-type
          (&/$UnivQ _)
          (|do [$var &type/existential
                exo-type** (&type/apply-type exo-type* $var)]
            (analyse-lambda* analyse exo-type** ?self ?arg ?body))

          (&/$ExQ _)
          (&type/with-var
            (fn [$var]
              (|do [exo-type** (&type/apply-type exo-type* $var)]
                (analyse-lambda* analyse exo-type** ?self ?arg ?body))))
          
          (&/$LambdaT ?arg-t ?return-t)
          (|do [[=scope =captured =body] (&&lambda/with-lambda ?self exo-type*
                                           ?arg ?arg-t
                                           (&&/analyse-1 analyse ?return-t ?body))
                _cursor &/cursor]
            (return (&&/|meta exo-type* _cursor
                              (&/V &&/$lambda (&/T =scope =captured =body)))))

          
          
          _
          (fail "")))
      (fn [err]
        (fail (str err "\n" "[Analyser Error] Functions require function types: " (&type/show-type exo-type)))))
    ))

(defn analyse-lambda** [analyse exo-type ?self ?arg ?body]
  (|case exo-type
    (&/$UnivQ _)
    (|do [$var &type/existential
          exo-type* (&type/apply-type exo-type $var)
          [_ _expr] (analyse-lambda** analyse exo-type* ?self ?arg ?body)
          _cursor &/cursor]
      (return (&&/|meta exo-type _cursor _expr)))
    
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
  (|do [module-name &/get-module-name
        ? (&&module/defined? module-name ?name)]
    (if ?
      (fail (str "[Analyser Error] Can't redefine " (str module-name ";" ?name)))
      (|do [=value (&/with-scope ?name
                     (&&/analyse-1+ analyse ?value))]
        (|case =value
          [_ (&&/$var (&/$Global ?r-module ?r-name))]
          (|do [_ (&&module/def-alias module-name ?name ?r-module ?r-name (&&/expr-type* =value))]
            (return &/Nil$))

          _
          (|do [_ (compile-token (&/V &&/$def (&/T ?name =value)))
                :let [[[def-type def-cursor] def-analysis] =value
                      _ (println 'DEF (str module-name ";" ?name) ;; (&type/show-type def-type)
                                 )]]
            (return &/Nil$)))
        ))))

(defn analyse-declare-macro [analyse compile-token ?name]
  (|do [module-name &/get-module-name
        _ (compile-token (&/V &&/$declare-macro (&/T module-name ?name)))]
    (return &/Nil$)))

(defn analyse-declare-tags [tags type-name]
  (|do [module-name &/get-module-name
        [_ def-data] (&&module/find-def module-name type-name)
        def-type (&&module/ensure-type-def def-data)
        _ (&&module/declare-tags module-name tags def-type)]
    (return &/Nil$)))

(defn analyse-import [analyse compile-module compile-token path]
  (|do [module-name &/get-module-name
        _ (if (= module-name path)
            (fail (str "[Analyser Error] Module can't import itself: " path))
            (return nil))]
    (&/save-module
     (|do [already-compiled? (&&module/exists? path)
           active? (&/active-module? path)
           _ (&/assert! (not active?) (str "[Analyser Error] Can't import a module that is mid-compilation: " path " @ " module-name))
           _ (&&module/add-import path)
           _ (if (not already-compiled?)
               (compile-module path)
               (return nil))]
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
        =value (&&/analyse-1 analyse ==type ?value)
        _cursor &/cursor]
    (return (&/|list (&&/|meta ==type _cursor
                               (&/V &&/$ann (&/T =value =type ==type))
                               )))))

(defn analyse-coerce [analyse eval! exo-type ?type ?value]
  (|do [=type (&&/analyse-1 analyse &type/Type ?type)
        ==type (eval! =type)
        _ (&type/check exo-type ==type)
        =value (&&/analyse-1+ analyse ?value)
        _cursor &/cursor]
    (return (&/|list (&&/|meta ==type _cursor
                               (&/V &&/$coerce (&/T =value =type ==type))
                               )))))
