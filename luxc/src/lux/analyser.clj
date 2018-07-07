(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return return* |case]]
                 [reader :as &reader]
                 [parser :as &parser]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lux :as &&lux]
                          [module :as &&module]
                          [parser :as &&a-parser])
            (lux.analyser.proc [common :as &&common]
                               [jvm :as &&jvm])))

;; [Utils]
(defn analyse-variant+ [analyse exo-type ident values]
  (|do [[module tag-name] (&/normalize ident)
        _ (&&module/ensure-can-see-tag module tag-name)
        idx (&&module/tag-index module tag-name)
        group (&&module/tag-group module tag-name)
        :let [is-last? (= idx (dec (&/|length group)))]]
    (if (= 1 (&/|length group))
      (|do [_cursor &/cursor]
        (analyse exo-type (&/T [_cursor (&/$Tuple values)])))
      (|case exo-type
        (&/$Var id)
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

(defn ^:private just-analyse [analyser syntax]
  (&type/with-var
    (fn [?var]
      (|do [[[?output-type ?output-cursor] ?output-term] (&&/analyse-1 analyser ?var syntax)]
        (|case [?var ?output-type]
          [(&/$Var ?e-id) (&/$Var ?a-id)]
          (if (= ?e-id ?a-id)
            (|do [=output-type (&type/clean ?var ?output-type)]
              (return (&&/|meta =output-type ?output-cursor ?output-term)))
            (|do [=output-type (&type/clean ?var ?var)]
              (return (&&/|meta =output-type ?output-cursor ?output-term))))

          [_ _]
          (|do [=output-type (&type/clean ?var ?output-type)]
            (return (&&/|meta =output-type ?output-cursor ?output-term))))
        ))))

(defn ^:private analyse-ast [optimize eval! compile-module ^"[Ljava.lang.Object;" compilers exo-type ?token]
  (|let [analyse (partial analyse-ast optimize eval! compile-module compilers)
         [cursor token] ?token
         compile-def (aget compilers 0)
         compile-program (aget compilers 1)
         macro-caller (aget compilers 2)]
    (|case token
      ;; Standard special forms
      (&/$Bool ?value)
      (|do [_ (&type/check exo-type &type/Bool)]
        (return (&/|list (&&/|meta exo-type cursor (&&/$bool ?value)))))

      (&/$Nat ?value)
      (|do [_ (&type/check exo-type &type/Nat)]
        (return (&/|list (&&/|meta exo-type cursor (&&/$nat ?value)))))

      (&/$Int ?value)
      (|do [_ (&type/check exo-type &type/Int)]
        (return (&/|list (&&/|meta exo-type cursor (&&/$int ?value)))))

      (&/$Rev ?value)
      (|do [_ (&type/check exo-type &type/Rev)]
        (return (&/|list (&&/|meta exo-type cursor (&&/$rev ?value)))))

      (&/$Frac ?value)
      (|do [_ (&type/check exo-type &type/Frac)]
        (return (&/|list (&&/|meta exo-type cursor (&&/$frac ?value)))))

      (&/$Text ?value)
      (|do [_ (&type/check exo-type &type/Text)]
        (return (&/|list (&&/|meta exo-type cursor (&&/$text ?value)))))

      (&/$Tuple ?elems)
      (&/with-analysis-meta cursor exo-type
        (&&lux/analyse-tuple analyse (&/$Right exo-type) ?elems))

      (&/$Record ?elems)
      (&/with-analysis-meta cursor exo-type
        (&&lux/analyse-record analyse exo-type ?elems))

      (&/$Tag ?ident)
      (&/with-analysis-meta cursor exo-type
        (analyse-variant+ analyse exo-type ?ident &/$Nil))

      (&/$Symbol ?ident)
      (&/with-analysis-meta cursor exo-type
        (&&lux/analyse-symbol analyse exo-type ?ident))

      (&/$Form (&/$Cons [command-meta command] parameters))
      (|case command
        (&/$Text ?procedure)
        (case ?procedure
          "lux check"
          (|let [(&/$Cons ?type
                          (&/$Cons ?value
                                   (&/$Nil))) parameters]
            (&/with-analysis-meta cursor exo-type
              (&&lux/analyse-ann analyse eval! exo-type ?type ?value)))

          "lux check type"
          (|let [(&/$Cons ?value (&/$Nil)) parameters]
            (analyse-ast optimize eval! compile-module compilers &type/Type ?value))

          "lux coerce"
          (|let [(&/$Cons ?type
                          (&/$Cons ?value
                                   (&/$Nil))) parameters]
            (&/with-analysis-meta cursor exo-type
              (&&lux/analyse-coerce analyse eval! exo-type ?type ?value)))

          "lux def"
          (|let [(&/$Cons [_ (&/$Symbol "" ?name)]
                          (&/$Cons ?value
                                   (&/$Cons ?meta
                                            (&/$Nil))
                                   )) parameters]
            (&/with-cursor cursor
              (&&lux/analyse-def analyse optimize eval! compile-def ?name ?value ?meta)))

          "lux program"
          (|let [(&/$Cons ?program (&/$Nil)) parameters]
            (&/with-cursor cursor
              (&&lux/analyse-program analyse optimize compile-program ?program)))

          "lux function"
          (|let [(&/$Cons [_ (&/$Symbol "" ?self)]
                          (&/$Cons [_ (&/$Symbol "" ?arg)]
                                   (&/$Cons ?body
                                            (&/$Nil)))) parameters]
            (&/with-analysis-meta cursor exo-type
              (&&lux/analyse-function analyse exo-type ?self ?arg ?body)))

          "lux module"
          (|let [(&/$Cons ?meta (&/$Nil)) parameters]
            (&/with-cursor cursor
              (&&lux/analyse-module analyse optimize eval! compile-module ?meta)))

          "lux in-module"
          (|let [(&/$Cons [_ (&/$Text ?module)] (&/$Cons ?expr (&/$Nil))) parameters]
            (&/with-cursor cursor
              (&/with-module ?module
                (analyse exo-type ?expr))))

          ;; else
          (&/with-analysis-meta cursor exo-type
            (cond (.startsWith ^String ?procedure "jvm")
                  (|do [_ &/jvm-host]
                    (&&jvm/analyse-host analyse exo-type compilers ?procedure parameters))
                  
                  :else
                  (&&common/analyse-proc analyse exo-type ?procedure parameters))))
        
        (&/$Nat idx)
        (&/with-analysis-meta cursor exo-type
          (&&lux/analyse-variant analyse (&/$Right exo-type) idx nil parameters))

        (&/$Tag ?ident)
        (&/with-analysis-meta cursor exo-type
          (analyse-variant+ analyse exo-type ?ident parameters))

        (&/$Record ?pattern-matching)
        (|let [(&/$Cons ?input (&/$Nil)) parameters]
          (&/with-analysis-meta cursor exo-type
            (&&lux/analyse-case analyse exo-type ?input ?pattern-matching)))

        _
        (&/with-cursor cursor
          (|do [=fn (just-analyse analyse (&/T [command-meta command]))]
            (&&lux/analyse-apply analyse cursor exo-type macro-caller =fn parameters))))
      
      _
      (&/fail-with-loc (str "[Analyser Error] Unknown syntax: " (&/show-ast (&/T [(&/T ["" -1 -1]) token]))))
      )))

;; [Resources]
(defn analyse [optimize eval! compile-module compilers]
  (|do [asts &parser/parse]
    (&/flat-map% (partial analyse-ast optimize eval! compile-module compilers &type/Nothing) asts)))

(defn clean-output [?var analysis]
  (|do [:let [[[?output-type ?output-cursor] ?output-term] analysis]
        =output-type (&type/clean ?var ?output-type)]
    (return (&&/|meta =output-type ?output-cursor ?output-term))))

(defn repl-analyse [optimize eval! compile-module compilers]
  (|do [asts &parser/parse]
    (&/flat-map% (fn [ast]
                   (&type/with-var
                     (fn [?var]
                       (|do [=outputs (&/with-closure
                                        (analyse-ast optimize eval! compile-module compilers ?var ast))]
                         (&/map% (partial clean-output ?var) =outputs)))))
                 asts)))
