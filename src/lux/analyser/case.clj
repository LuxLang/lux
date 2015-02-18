(ns lux.analyser.case
  (:require [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return fail
                                         try-all-m map-m mapcat-m reduce-m
                                         assert!]]
                 [parser :as &parser]
                 [type :as &type])
            (lux.analyser [base :as &&]
                          [env :as &env])))

;; [Resources]
(defn locals [member]
  (match member
    [::&parser/Ident ?name]
    (list ?name)

    [::&parser/Tuple ?submembers]
    (mapcat locals ?submembers)

    [::&parser/Form ([[::&parser/Tag _] & ?submembers] :seq)]
    (mapcat locals ?submembers)

    _
    (list)))

(defn analyse-branch [analyse max-registers [bindings body]]
  (reduce (fn [body* name]
            (&env/with-local name :local &type/+dont-care-type+ body*))
          (reduce (fn [body* _]
                    (&env/with-local "#" :local &type/+dont-care-type+ body*))
                  (&&/analyse-1 analyse body)
                  (range (- max-registers (count bindings))))
          bindings))

