;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return fail return* fail* |case]]
                 [reader :as &reader]
                 [parser :as &parser]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lux :as &&lux]
                          [host :as &&host]
                          [module :as &&module]
                          [parser :as &&a-parser])))

;; [Utils]
(defn analyse-variant+ [analyse exo-type ident values]
  (|do [[module tag-name] (&/normalize ident)
        _ (&&module/ensure-can-see-tag module tag-name)
        idx (&&module/tag-index module tag-name)
        group (&&module/tag-group module tag-name)
        :let [is-last? (= idx (dec (&/|length group)))]]
    (if (= 1 (&/|length group))
      (|do [_cursor &/cursor]
        (analyse exo-type (&/T [_cursor (&/$TupleS values)])))
      (|case exo-type
        (&/$VarT id)
        (|do [? (&type/bound? id)]
          (if (or ? (&&/type-tag? module tag-name))
            (&&lux/analyse-variant analyse (&/$Right exo-type) idx is-last? values)
            (|do [wanted-type (&&module/tag-type module tag-name)
                  wanted-type* (&type/instantiate-inference wanted-type)
                  [[variant-type variant-cursor] variant-analysis] (&&/cap-1 (&&lux/analyse-variant analyse (&/$Left wanted-type*) idx is-last? values))
                  _ (&type/check exo-type variant-type)]
              (return (&/|list (&&/|meta exo-type variant-cursor variant-analysis))))))

        _
        (&&lux/analyse-variant analyse (&/$Right exo-type) idx is-last? values)
        ))
    ))

(defn ^:private add-loc [meta ^String msg]
  (if (.startsWith msg "@")
    msg
    (|let [[file line col] meta]
      (str "@ " file "," line "," col "\n" msg))))

(defn ^:private fail-with-loc [msg]
  (fn [state]
    (fail* (add-loc (&/get$ &/$cursor state) msg))))

(defn ^:private aba4 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Classes & interfaces
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_class")]
                       (&/$Cons ?class-decl
                                (&/$Cons ?super-class
                                         (&/$Cons [_ (&/$TupleS ?interfaces)]
                                                  (&/$Cons ?inheritance-modifier
                                                           (&/$Cons [_ (&/$TupleS ?anns)]
                                                                    (&/$Cons [_ (&/$TupleS ?fields)]
                                                                             (&/$Cons [_ (&/$TupleS ?methods)]
                                                                                      (&/$Nil))))))))))
    (|do [=gclass-decl (&&a-parser/parse-gclass-decl ?class-decl)
          =super-class (&&a-parser/parse-gclass-super ?super-class)
          =interfaces (&/map% &&a-parser/parse-gclass-super ?interfaces)
          =inheritance-modifier (&&a-parser/parse-inheritance-modifier ?inheritance-modifier)
          =anns (&/map% &&a-parser/parse-ann ?anns)
          =fields (&/map% &&a-parser/parse-field ?fields)
          =methods (&/map% &&a-parser/parse-method-def ?methods)]
      (&&host/analyse-jvm-class analyse compile-token =gclass-decl =super-class =interfaces =inheritance-modifier =anns =fields =methods))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_interface")]
                       (&/$Cons ?class-decl
                                (&/$Cons [_ (&/$TupleS ?supers)]
                                         (&/$Cons [_ (&/$TupleS ?anns)]
                                                  ?methods)))))
    (|do [=gclass-decl (&&a-parser/parse-gclass-decl ?class-decl)
          =supers (&/map% &&a-parser/parse-gclass-super ?supers)
          =anns (&/map% &&a-parser/parse-ann ?anns)
          =methods (&/map% &&a-parser/parse-method-decl ?methods)]
      (&&host/analyse-jvm-interface analyse compile-token =gclass-decl =supers =anns =methods))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_anon-class")]
                       (&/$Cons ?super-class
                                (&/$Cons [_ (&/$TupleS ?interfaces)]
                                         (&/$Cons [_ (&/$TupleS ?ctor-args)]
                                                  (&/$Cons [_ (&/$TupleS ?methods)]
                                                           (&/$Nil)))))))
    (|do [=super-class (&&a-parser/parse-gclass-super ?super-class)
          =interfaces (&/map% &&a-parser/parse-gclass-super ?interfaces)
          =ctor-args (&/map% &&a-parser/parse-ctor-arg ?ctor-args)
          =methods (&/map% &&a-parser/parse-method-def ?methods)]
      (&&host/analyse-jvm-anon-class analyse compile-token exo-type =super-class =interfaces =ctor-args =methods))

    ;; Programs
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_program")]
                       (&/$Cons [_ (&/$SymbolS "" ?args)]
                                (&/$Cons ?body
                                         (&/$Nil)))))
    (&&host/analyse-jvm-program analyse compile-token ?args ?body)
    
    _
    (fail-with-loc (str "[Analyser Error] Unknown syntax: " (prn-str (&/show-ast (&/T [(&/T ["" -1 -1]) token])))))))

(defn ^:private aba3 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Objects
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_null")] (&/$Nil)))
    (&&host/analyse-jvm-null analyse exo-type)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_null?")]
                       (&/$Cons ?object
                                (&/$Nil))))
    (&&host/analyse-jvm-null? analyse exo-type ?object)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_instanceof")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons ?object
                                         (&/$Nil)))))
    (&&host/analyse-jvm-instanceof analyse exo-type ?class ?object)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_new")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                         (&/$Cons [_ (&/$TupleS ?args)]
                                                  (&/$Nil))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-new analyse exo-type ?class =arg-classes ?args))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_getstatic")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?field)]
                                         (&/$Nil)))))
    (&&host/analyse-jvm-getstatic analyse exo-type ?class ?field)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_getfield")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?field)]
                                         (&/$Cons ?object
                                                  (&/$Nil))))))
    (&&host/analyse-jvm-getfield analyse exo-type ?class ?field ?object)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_putstatic")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?field)]
                                         (&/$Cons ?value
                                                  (&/$Nil))))))
    (&&host/analyse-jvm-putstatic analyse exo-type ?class ?field ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_putfield")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?field)]
                                         (&/$Cons ?value
                                                  (&/$Cons ?object
                                                           (&/$Nil)))))))
    (&&host/analyse-jvm-putfield analyse exo-type ?class ?field ?value ?object)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_invokestatic")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?method)]
                                         (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                                  (&/$Cons [_ (&/$TupleS ?args)]
                                                           (&/$Nil)))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-invokestatic analyse exo-type ?class ?method =arg-classes ?args))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_invokevirtual")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?method)]
                                         (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                                  (&/$Cons ?object
                                                           (&/$Cons [_ (&/$TupleS ?args)]
                                                                    (&/$Nil))))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-invokevirtual analyse exo-type ?class ?method =arg-classes ?object ?args))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_invokeinterface")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?method)]
                                         (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                                  (&/$Cons ?object
                                                           (&/$Cons [_ (&/$TupleS ?args)]
                                                                    (&/$Nil))))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-invokeinterface analyse exo-type ?class ?method =arg-classes ?object ?args))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_invokespecial")]
                       (&/$Cons [_ (&/$TextS ?class)]
                                (&/$Cons [_ (&/$TextS ?method)]
                                         (&/$Cons [_ (&/$TupleS ?arg-classes)]
                                                  (&/$Cons ?object
                                                           (&/$Cons [_ (&/$TupleS ?args)]
                                                                    (&/$Nil))))))))
    (|do [=arg-classes (&/map% &&a-parser/parse-text ?arg-classes)]
      (&&host/analyse-jvm-invokespecial analyse exo-type ?class ?method =arg-classes ?object ?args))

    ;; Exceptions
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_try")]
                       (&/$Cons ?body
                                ?handlers)))
    (|do [catches+finally (&/fold% &&a-parser/parse-handler (&/T [&/$Nil &/$None]) ?handlers)]
      (&&host/analyse-jvm-try analyse exo-type ?body catches+finally))

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_throw")]
                       (&/$Cons ?ex
                                (&/$Nil))))
    (&&host/analyse-jvm-throw analyse exo-type ?ex)

    ;; Syncronization/monitos
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_monitorenter")]
                       (&/$Cons ?monitor
                                (&/$Nil))))
    (&&host/analyse-jvm-monitorenter analyse exo-type ?monitor)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_jvm_monitorexit")]
                       (&/$Cons ?monitor
                                (&/$Nil))))
    (&&host/analyse-jvm-monitorexit analyse exo-type ?monitor)

    _
    (aba4 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba2 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_case")]
                       (&/$Cons ?value ?branches)))
    (&&lux/analyse-case analyse exo-type ?value ?branches)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_lambda")]
                       (&/$Cons [_ (&/$SymbolS "" ?self)]
                                (&/$Cons [_ (&/$SymbolS "" ?arg)]
                                         (&/$Cons ?body
                                                  (&/$Nil))))))
    (&&lux/analyse-lambda analyse exo-type ?self ?arg ?body)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_def")]
                       (&/$Cons [_ (&/$SymbolS "" ?name)]
                                (&/$Cons ?value
                                         (&/$Cons ?meta
                                                  (&/$Nil))
                                         ))))
    (&&lux/analyse-def analyse eval! compile-token ?name ?value ?meta)
    
    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_import")]
                       (&/$Cons [_ (&/$TextS ?path)]
                                (&/$Nil))))
    (&&lux/analyse-import analyse compile-module compile-token ?path)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_:")]
                       (&/$Cons ?type
                                (&/$Cons ?value
                                         (&/$Nil)))))
    (&&lux/analyse-check analyse eval! exo-type ?type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_:!")]
                       (&/$Cons ?type
                                (&/$Cons ?value
                                         (&/$Nil)))))
    (&&lux/analyse-coerce analyse eval! exo-type ?type ?value)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_alias")]
                       (&/$Cons [_ (&/$TextS ?alias)]
                                (&/$Cons [_ (&/$TextS ?module)]
                                         (&/$Nil)))))
    (&&lux/analyse-alias analyse compile-token ?alias ?module)

    (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_host")]
                       (&/$Cons [_ (&/$TupleS (&/$Cons [_ (&/$TextS ?category)]
                                                       (&/$Cons [_ (&/$TextS ?proc)]
                                                                (&/$Nil))))]
                                (&/$Cons [_ (&/$TupleS ?args)]
                                         (&/$Nil)))))
    (&&host/analyse-host analyse exo-type ?category ?proc ?args)
    
    _
    (aba3 analyse eval! compile-module compile-token exo-type token)))

(defn ^:private aba1 [analyse eval! compile-module compile-token exo-type token]
  (|case token
    ;; Standard special forms
    (&/$BoolS ?value)
    (|do [_ (&type/check exo-type &type/Bool)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&&/$bool ?value)))))

    (&/$IntS ?value)
    (|do [_ (&type/check exo-type &type/Int)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&&/$int ?value)))))

    (&/$RealS ?value)
    (|do [_ (&type/check exo-type &type/Real)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&&/$real ?value)))))

    (&/$CharS ?value)
    (|do [_ (&type/check exo-type &type/Char)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&&/$char ?value)))))

    (&/$TextS ?value)
    (|do [_ (&type/check exo-type &type/Text)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor (&&/$text ?value)))))

    (&/$TupleS ?elems)
    (&&lux/analyse-tuple analyse (&/$Right exo-type) ?elems)

    (&/$RecordS ?elems)
    (&&lux/analyse-record analyse exo-type ?elems)

    (&/$TagS ?ident)
    (analyse-variant+ analyse exo-type ?ident &/$Nil)

    (&/$SymbolS ?ident)
    (&&lux/analyse-symbol analyse exo-type ?ident)
    
    _
    (aba2 analyse eval! compile-module compile-token exo-type token)
    ))

(defn ^:private analyse-basic-ast [analyse eval! compile-module compile-token exo-type token]
  (|case token
    [meta ?token]
    (fn [state]
      (|case ((aba1 analyse eval! compile-module compile-token exo-type ?token) state)
        (&/$Right state* output)
        (return* state* output)

        (&/$Left "")
        (fail* (add-loc (&/get$ &/$cursor state) (str "[Analyser Error] Unrecognized token: " (&/show-ast token))))

        (&/$Left msg)
        (fail* (add-loc (&/get$ &/$cursor state) msg))
        ))
    ))

(defn ^:private just-analyse [analyser syntax]
  (&type/with-var
    (fn [?var]
      (|do [[[?output-type ?output-cursor] ?output-term] (&&/analyse-1 analyser ?var syntax)]
        (|case [?var ?output-type]
          [(&/$VarT ?e-id) (&/$VarT ?a-id)]
          (if (= ?e-id ?a-id)
            (|do [=output-type (&type/clean ?var ?output-type)]
              (return (&&/|meta =output-type ?output-cursor ?output-term)))
            (|do [=output-type (&type/clean ?var ?var)]
              (return (&&/|meta =output-type ?output-cursor ?output-term))))

          [_ _]
          (|do [=output-type (&type/clean ?var ?output-type)]
            (return (&&/|meta =output-type ?output-cursor ?output-term))))
        ))))

(defn ^:private analyse-ast [eval! compile-module compile-token exo-type token]
  (|let [[cursor _] token]
    (&/with-cursor cursor
      (&/with-expected-type exo-type
        (|case token
          [meta (&/$FormS (&/$Cons [_ (&/$IntS idx)] ?values))]
          (&&lux/analyse-variant (partial analyse-ast eval! compile-module compile-token) (&/$Right exo-type) idx nil ?values)

          [meta (&/$FormS (&/$Cons [_ (&/$TagS ?ident)] ?values))]
          (analyse-variant+ (partial analyse-ast eval! compile-module compile-token) exo-type ?ident ?values)
          
          [meta (&/$FormS (&/$Cons ?fn ?args))]
          (|case ?fn
            [_ (&/$SymbolS _)]
            (fn [state]
              (|case ((just-analyse (partial analyse-ast eval! compile-module compile-token) ?fn) state)
                (&/$Right state* =fn)
                ((&&lux/analyse-apply (partial analyse-ast eval! compile-module compile-token) exo-type =fn ?args) state*)

                _
                ((analyse-basic-ast (partial analyse-ast eval! compile-module compile-token) eval! compile-module compile-token exo-type token) state)))

            _
            (|do [=fn (just-analyse (partial analyse-ast eval! compile-module compile-token) ?fn)]
              (&&lux/analyse-apply (partial analyse-ast eval! compile-module compile-token) exo-type =fn ?args)))
          
          _
          (analyse-basic-ast (partial analyse-ast eval! compile-module compile-token) eval! compile-module compile-token exo-type token))))))

;; [Resources]
(defn analyse [eval! compile-module compile-token]
  (|do [asts &parser/parse]
    (&/flat-map% (partial analyse-ast eval! compile-module compile-token &/$VoidT) asts)))

(defn clean-output [?var analysis]
  (|do [:let [[[?output-type ?output-cursor] ?output-term] analysis]
        =output-type (&type/clean ?var ?output-type)]
    (return (&&/|meta =output-type ?output-cursor ?output-term))))

(defn repl-analyse [eval! compile-module compile-token]
  (|do [asts &parser/parse]
    (&type/with-var
      (fn [?var]
        (|do [outputs (&/flat-map% (partial analyse-ast eval! compile-module compile-token ?var) asts)]
          (&/map% (partial clean-output ?var) outputs))))))
