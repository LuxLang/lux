(ns lux.analyser.lambda
  (:require [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return fail
                                     try-all-m map-m mapcat-m reduce-m
                                     assert!]])
            (lux.analyser [base :as &&]
                          [env :as &env])))

;; [Resource]
(defn with-lambda [self self-type arg arg-type body]
  (&/with-closure
    (exec [scope-name &/get-scope-name]
      (&env/with-local self self-type
        (&env/with-local arg arg-type
          (exec [=return body
                 =captured &env/captured-vars]
            (return [scope-name =captured =return])))))))

(defn close-over [scope ident register frame]
  (matchv ::M/objects [register]
    [["Expression" [_ register-type]]]
    (let [register* (&/V "Expression" (&/T (&/V "captured" (&/T scope (->> frame (get$ "closure") (get$ "counter")) register)) register-type))]
      [register* (update$ "closure" #(-> %
                                         (update$ "counter" inc)
                                         (update$ "mappings" #(|put ident register* %)))
                          frame)])))
