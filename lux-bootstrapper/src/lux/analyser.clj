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
      (|do [_location &/location]
        (analyse exo-type (&/T [_location (&/$Tuple values)])))
      (|case exo-type
        (&/$Var id)
        (|do [? (&type/bound? id)]
          (if (or ? (&&/type-tag? module tag-name))
            (&&lux/analyse-variant analyse (&/$Right exo-type) idx is-last? values)
            (|do [wanted-type (&&module/tag-type module tag-name)
                  wanted-type* (&type/instantiate-inference wanted-type)
                  [[variant-type variant-location] variant-analysis] (&&/cap-1 (&&lux/analyse-variant analyse (&/$Left wanted-type*) idx is-last? values))
                  _ (&type/check exo-type variant-type)]
              (return (&/|list (&&/|meta exo-type variant-location variant-analysis))))))

        _
        (&&lux/analyse-variant analyse (&/$Right exo-type) idx is-last? values)
        ))
    ))

(defn ^:private just-analyse [analyser syntax]
  (&type/with-var
    (fn [?var]
      (|do [[[?output-type ?output-location] ?output-term] (&&/analyse-1 analyser ?var syntax)]
        (|case [?var ?output-type]
          [(&/$Var ?e-id) (&/$Var ?a-id)]
          (if (= ?e-id ?a-id)
            (|do [=output-type (&type/clean ?var ?output-type)]
              (return (&&/|meta =output-type ?output-location ?output-term)))
            (|do [=output-type (&type/clean ?var ?var)]
              (return (&&/|meta =output-type ?output-location ?output-term))))

          [_ _]
          (|do [=output-type (&type/clean ?var ?output-type)]
            (return (&&/|meta =output-type ?output-location ?output-term))))
        ))))

(defn ^:private analyse-ast [optimize eval! compile-module ^"[Ljava.lang.Object;" compilers exo-type ?token]
  (|let [analyse (partial analyse-ast optimize eval! compile-module compilers)
         [location token] ?token
         compile-def (aget compilers 0)
         compile-program (aget compilers 1)
         macro-caller (aget compilers 2)]
    (|case token
      ;; Standard special forms
      (&/$Bit ?value)
      (|do [_ (&type/check exo-type &type/Bit)]
        (return (&/|list (&&/|meta exo-type location (&&/$bit ?value)))))

      (&/$Nat ?value)
      (|do [_ (&type/check exo-type &type/Nat)]
        (return (&/|list (&&/|meta exo-type location (&&/$nat ?value)))))

      (&/$Int ?value)
      (|do [_ (&type/check exo-type &type/Int)]
        (return (&/|list (&&/|meta exo-type location (&&/$int ?value)))))

      (&/$Rev ?value)
      (|do [_ (&type/check exo-type &type/Rev)]
        (return (&/|list (&&/|meta exo-type location (&&/$rev ?value)))))

      (&/$Frac ?value)
      (|do [_ (&type/check exo-type &type/Frac)]
        (return (&/|list (&&/|meta exo-type location (&&/$frac ?value)))))

      (&/$Text ?value)
      (|do [_ (&type/check exo-type &type/Text)]
        (return (&/|list (&&/|meta exo-type location (&&/$text ?value)))))

      (&/$Tuple ?elems)
      (&/with-analysis-meta location exo-type
        (&&lux/analyse-tuple analyse (&/$Right exo-type) ?elems))

      (&/$Record ?elems)
      (&/with-analysis-meta location exo-type
        (&&lux/analyse-record analyse exo-type ?elems))

      (&/$Tag ?ident)
      (&/with-analysis-meta location exo-type
        (analyse-variant+ analyse exo-type ?ident &/$Nil))

      (&/$Identifier ?ident)
      (&/with-analysis-meta location exo-type
        (&&lux/analyse-identifier analyse exo-type ?ident))

      (&/$Form (&/$Cons [command-meta command] parameters))
      (|case command
        (&/$Text ?procedure)
        (case ?procedure
          "lux type check"
          (|let [(&/$Cons ?type
                          (&/$Cons ?value
                                   (&/$Nil))) parameters]
            (&/with-analysis-meta location exo-type
              (&&lux/analyse-type-check analyse eval! exo-type ?type ?value)))

          "lux type check type"
          (|let [(&/$Cons ?value (&/$Nil)) parameters]
            (analyse-ast optimize eval! compile-module compilers &type/Type ?value))

          "lux type as"
          (|let [(&/$Cons ?type
                          (&/$Cons ?value
                                   (&/$Nil))) parameters]
            (&/with-analysis-meta location exo-type
              (&&lux/analyse-type-as analyse eval! exo-type ?type ?value)))

          "lux def"
          (|let [(&/$Cons [_ (&/$Identifier "" ?name)]
                          (&/$Cons ?value
                                   (&/$Cons ?meta
                                            (&/$Cons [_ (&/$Bit exported?)]
                                                     (&/$Nil)))
                                   )) parameters]
            (&/with-location location
              (&&lux/analyse-def analyse optimize eval! compile-def ?name ?value ?meta exported?)))

          "lux def alias"
          (|let [(&/$Cons [_ (&/$Identifier "" ?alias)]
                          (&/$Cons [_ (&/$Identifier ?original)]
                                   (&/$Nil)
                                   )) parameters]
            (&/with-location location
              (&&lux/analyse-def-alias ?alias ?original)))

          "lux def type tagged"
          (|let [(&/$Cons [_ (&/$Identifier "" ?name)]
                          (&/$Cons ?value
                                   (&/$Cons ?meta
                                            (&/$Cons [_ (&/$Tuple ?tags)]
                                                     (&/$Cons [_ (&/$Bit exported?)]
                                                              (&/$Nil))))
                                   )) parameters]
            (&/with-location location
              (&&lux/analyse-def-type-tagged analyse optimize eval! compile-def ?name ?value ?meta ?tags exported?)))

          "lux def program"
          (|let [(&/$Cons ?program (&/$Nil)) parameters]
            (&/with-location location
              (&&lux/analyse-program analyse optimize compile-program ?program)))

          "lux def module"
          (|let [(&/$Cons ?meta (&/$Cons ?imports (&/$Nil))) parameters]
            (&/with-location location
              (&&lux/analyse-module analyse optimize eval! compile-module ?meta ?imports)))

          "lux in-module"
          (|let [(&/$Cons [_ (&/$Text ?module)] (&/$Cons ?expr (&/$Nil))) parameters]
            (&/with-location location
              (&/with-module ?module
                (analyse exo-type ?expr))))

          ;; else
          (&/with-analysis-meta location exo-type
            (cond (.startsWith ^String ?procedure "jvm")
                  (|do [_ &/jvm-host]
                    (&&jvm/analyse-host analyse exo-type compilers ?procedure parameters))
                  
                  :else
                  (&&common/analyse-proc analyse exo-type ?procedure parameters))))
        
        (&/$Nat idx)
        (|let [(&/$Cons [_ (&/$Bit ?right)] parameters*) parameters]
          (&/with-analysis-meta location exo-type
            (&&lux/analyse-variant analyse (&/$Right exo-type) (if ?right (inc idx) idx) ?right parameters*)))

        (&/$Tag ?ident)
        (&/with-analysis-meta location exo-type
          (analyse-variant+ analyse exo-type ?ident parameters))

        ;; Pattern-matching syntax.
        (&/$Record ?pattern-matching)
        (|let [(&/$Cons ?input (&/$Nil)) parameters]
          (&/with-analysis-meta location exo-type
            (&&lux/analyse-case analyse exo-type ?input ?pattern-matching)))

        ;; Function syntax.
        (&/$Tuple (&/$Cons [_ (&/$Identifier "" ?self)]
                           (&/$Cons [_ (&/$Identifier "" ?arg)] (&/$Nil))))
        (|let [(&/$Cons ?body (&/$Nil)) parameters]
          (&/with-analysis-meta location exo-type
            (&&lux/analyse-function analyse exo-type ?self ?arg ?body)))

        _
        (&/with-location location
          (|do [=fn (just-analyse analyse (&/T [command-meta command]))]
            (&&lux/analyse-apply analyse location exo-type macro-caller =fn parameters))))
      
      _
      (&/fail-with-loc (str "[Analyser Error] Unknown syntax: " (&/show-ast (&/T [(&/T ["" -1 -1]) token]))))
      )))

;; [Resources]
(defn analyse [optimize eval! compile-module compilers]
  (|do [asts &parser/parse]
    (&/flat-map% (partial analyse-ast optimize eval! compile-module compilers &type/Nothing) asts)))

(defn clean-output [?var analysis]
  (|do [:let [[[?output-type ?output-location] ?output-term] analysis]
        =output-type (&type/clean ?var ?output-type)]
    (return (&&/|meta =output-type ?output-location ?output-term))))

(defn repl-analyse [optimize eval! compile-module compilers]
  (|do [asts &parser/parse]
    (&/flat-map% (fn [ast]
                   (&type/with-var
                     (fn [?var]
                       (|do [=outputs (&/with-closure
                                        (analyse-ast optimize eval! compile-module compilers ?var ast))]
                         (&/map% (partial clean-output ?var) =outputs)))))
                 asts)))
