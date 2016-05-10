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

(defn ^:private aba1 [analyse optimize eval! compile-module compilers exo-type token]
  (|let [[compile-def compile-program compile-class compile-interface] compilers]
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
      (&&lux/analyse-def analyse optimize eval! compile-def ?name ?value ?meta)
      
      (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_import")]
                         (&/$Cons [_ (&/$TextS ?path)]
                                  (&/$Nil))))
      (&&lux/analyse-import analyse compile-module ?path)

      (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_:")]
                         (&/$Cons ?type
                                  (&/$Cons ?value
                                           (&/$Nil)))))
      (&&lux/analyse-ann analyse eval! exo-type ?type ?value)

      (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_:!")]
                         (&/$Cons ?type
                                  (&/$Cons ?value
                                           (&/$Nil)))))
      (&&lux/analyse-coerce analyse eval! exo-type ?type ?value)

      (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_alias")]
                         (&/$Cons [_ (&/$TextS ?alias)]
                                  (&/$Cons [_ (&/$TextS ?module)]
                                           (&/$Nil)))))
      (&&lux/analyse-alias analyse ?alias ?module)

      (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_proc")]
                         (&/$Cons [_ (&/$TupleS (&/$Cons [_ (&/$TextS ?category)]
                                                         (&/$Cons [_ (&/$TextS ?proc)]
                                                                  (&/$Nil))))]
                                  (&/$Cons [_ (&/$TupleS ?args)]
                                           (&/$Nil)))))
      (&&host/analyse-host analyse exo-type compilers ?category ?proc ?args)

      (&/$FormS (&/$Cons [_ (&/$SymbolS _ "_lux_program")]
                         (&/$Cons [_ (&/$SymbolS "" ?args)]
                                  (&/$Cons ?body
                                           (&/$Nil)))))
      (&&lux/analyse-program analyse optimize compile-program ?args ?body)
      
      _
      (fail-with-loc (str "[Analyser Error] Unknown syntax: " (prn-str (&/show-ast (&/T [(&/T ["" -1 -1]) token])))))
      )))

(defn ^:private analyse-basic-ast [analyse optimize eval! compile-module compilers exo-type token]
  (|case token
    [meta ?token]
    (fn [state]
      (|case ((aba1 analyse optimize eval! compile-module compilers exo-type ?token) state)
        (&/$Right state* output)
        (return* state* output)

        (&/$Left msg)
        (if (= "" msg)
          (fail* (add-loc (&/get$ &/$cursor state) (str "[Analyser Error] Unrecognized token: " (&/show-ast token))))
          (fail* (add-loc (&/get$ &/$cursor state) msg)))
        ))))

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

(defn ^:private analyse-ast [optimize eval! compile-module compilers exo-type token]
  (|let [[cursor _] token
         analyser (partial analyse-ast optimize eval! compile-module compilers)]
    (&/with-cursor cursor
      (&/with-expected-type exo-type
        (|case token
          [meta (&/$FormS (&/$Cons [_ (&/$IntS idx)] ?values))]
          (&&lux/analyse-variant analyser (&/$Right exo-type) idx nil ?values)

          [meta (&/$FormS (&/$Cons [_ (&/$TagS ?ident)] ?values))]
          (analyse-variant+ analyser exo-type ?ident ?values)
          
          [meta (&/$FormS (&/$Cons ?fn ?args))]
          (|case ?fn
            [_ (&/$SymbolS _)]
            (fn [state]
              (|case ((just-analyse analyser ?fn) state)
                (&/$Right state* =fn)
                ((&&lux/analyse-apply analyser exo-type =fn ?args) state*)

                _
                ((analyse-basic-ast analyser optimize eval! compile-module compilers exo-type token) state)))

            _
            (|do [=fn (just-analyse analyser ?fn)]
              (&&lux/analyse-apply analyser exo-type =fn ?args)))
          
          _
          (analyse-basic-ast analyser optimize eval! compile-module compilers exo-type token))))))

;; [Resources]
(defn analyse [optimize eval! compile-module compilers]
  (|do [asts &parser/parse]
    (&/flat-map% (partial analyse-ast optimize eval! compile-module compilers &/$VoidT) asts)))

(defn clean-output [?var analysis]
  (|do [:let [[[?output-type ?output-cursor] ?output-term] analysis]
        =output-type (&type/clean ?var ?output-type)]
    (return (&&/|meta =output-type ?output-cursor ?output-term))))

(defn repl-analyse [eval! compile-module compilers]
  (|do [asts &parser/parse]
    (&type/with-var
      (fn [?var]
        (|do [outputs (&/flat-map% (partial analyse-ast eval! compile-module compilers ?var) asts)]
          (&/map% (partial clean-output ?var) outputs))))))
