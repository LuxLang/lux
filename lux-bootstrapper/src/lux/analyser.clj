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
         macro-caller (aget compilers 1)]
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

      (&/$Variant (&/$Item [command-meta command] parameters))
      (|case command
        (&/$Nat lefts)
        (|let [(&/$Item [_ (&/$Bit ?right)] parameters*) parameters]
          (&/with-analysis-meta location exo-type
            (&&lux/analyse-variant analyse (&/$Right exo-type) lefts ?right parameters*)))

        (&/$Identifier ?ident)
        (&/with-analysis-meta location exo-type
          (|do [[normal-module normal-short] (&/normalize ?ident)]
            (&&lux/analyse-variant+ analyse exo-type normal-module normal-short parameters)))

        _
        (&/fail-with-loc (str "[Analyser Error] Unknown syntax: " (&/show-ast (&/T [(&/T ["" -1 -1]) token])))))

      (&/$Tuple ?elems)
      (&/with-analysis-meta location exo-type
        (&&lux/analyse-record analyse exo-type ?elems))

      (&/$Identifier ?ident)
      (&/with-analysis-meta location exo-type
        (|let [[quoted_module quoted_line quoted_column] location]
          (&&lux/analyse-identifier analyse exo-type quoted_module ?ident)))

      (&/$Form (&/$Item [command-meta command] parameters))
      (|case command
        (&/$Identifier "library/lux" "def#")
        (|let [(&/$Item [_ (&/$Identifier "" ?name)]
                        (&/$Item ?value
                                 (&/$Item exported?
                                          (&/$End))
                                 )) parameters]
          (&/with-location location
            (&&lux/analyse-def analyse optimize eval! compile-def ?name ?value exported?)))

        (&/$Identifier "library/lux" "alias#")
        (|let [(&/$Item [_ (&/$Identifier "" ?alias)]
                        (&/$Item [_ (&/$Identifier ?original)]
                                 (&/$End)
                                 )) parameters]
          (&/with-location location
            (&&lux/analyse-def-alias ?alias ?original)))

        (&/$Identifier "library/lux" "module#")
        (|let [(&/$Item ?imports (&/$End)) parameters]
          (&/with-location location
            (&&lux/analyse-module analyse optimize eval! compile-module ?imports)))

        (&/$Identifier "library/lux" "is#")
        (|let [(&/$Item ?type
                        (&/$Item ?value
                                 (&/$End))) parameters]
          (&/with-analysis-meta location exo-type
            (&&lux/analyse-type-check analyse optimize eval! exo-type ?type ?value)))

        (&/$Identifier "library/lux" "as#")
        (|let [(&/$Item ?type
                        (&/$Item ?value
                                 (&/$End))) parameters]
          (&/with-analysis-meta location exo-type
            (&&lux/analyse-type-as analyse optimize eval! exo-type ?type ?value)))

        (&/$Identifier "library/lux" "is_type#")
        (|let [(&/$Item ?value (&/$End)) parameters]
          (analyse-ast optimize eval! compile-module compilers &type/Type ?value))

        (&/$Identifier "library/lux" "in_module#")
        (|let [(&/$Item [_ (&/$Text ?module)] (&/$Item ?expr (&/$End))) parameters]
          (&/with-location location
            (&/with-module ?module
              (analyse exo-type ?expr))))

        (&/$Identifier "library/lux" extension)
        (if (&&common/uses_new_format? extension)
          (&/with-analysis-meta location exo-type
            (&&common/analyse-proc analyse exo-type extension parameters))
          (&/with-location location
            (|do [=fn (just-analyse analyse (&/T [command-meta command]))]
              (&&lux/analyse-apply analyse location exo-type macro-caller =fn parameters))))
        
        (&/$Text ?procedure)
        (case ?procedure
          ;; else
          (&/with-analysis-meta location exo-type
            (cond (.startsWith ^String ?procedure "jvm")
                  (|do [_ &/jvm-host]
                    (&&jvm/analyse-host analyse exo-type compilers ?procedure parameters))
                  
                  :else
                  (&&common/analyse-proc analyse exo-type ?procedure parameters))))

        ;; Pattern-matching syntax.
        (&/$Variant ?pattern-matching)
        (if (even? (&/|length ?pattern-matching))
          (|let [(&/$Item ?input (&/$End)) parameters]
            (&/with-analysis-meta location exo-type
              (&&lux/analyse-case analyse exo-type ?input (&/|as-pairs ?pattern-matching))))
          (&/fail-with-loc (str "[Analyser Error] Unknown syntax: " (&/show-ast (&/T [(&/T ["" -1 -1]) token])))))

        ;; Function syntax.
        (&/$Tuple (&/$Item [_ (&/$Identifier "" ?self)]
                           (&/$Item [_ (&/$Identifier "" ?arg)] (&/$End))))
        (|let [(&/$Item ?body (&/$End)) parameters]
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
