(ns lux.analyser.case
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return fail |let]]
                 [parser :as &parser]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [env :as &env])))

;; [Resources]
(defn locals [member]
  (matchv ::M/objects [member]
    [["lux;Meta" [_ ["lux;Symbol" [_ ?name]]]]]
    (&/|list ?name)

    [["lux;Meta" [_ ["lux;Tuple" ?submembers]]]]
    (&/flat-map locals ?submembers)

    [["lux;Meta" [_ ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Tag" _]]] ?submembers]]]]]]
    (&/flat-map locals ?submembers)

    [_]
    (&/|list)))

(defn analyse-branch [analyse max-registers bindings+body]
  (|let [[bindings body] bindings+body]
    (do ;; (prn 'analyse-branch max-registers (&/->seq bindings) body)
        (&/fold (fn [body* name]
                  (&&/with-var
                    (fn [=var]
                      (&env/with-local name =var body*))))
                (&/fold (fn [body* _]
                          (&env/with-local "" &type/+dont-care+ body*))
                        (&&/analyse-1 analyse body)
                        (&/|range (- max-registers (&/|length bindings))))
                (&/|reverse bindings)))))
