(ns lux.analyser.lambda
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return fail]])
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
            (return (&/T scope-name =captured =return))))))))

(defn close-over [scope ident register frame]
  (matchv ::M/objects [register]
    [["Expression" [_ register-type]]]
    (let [register* (&/V "Expression" (&/T (&/V "captured" (&/T scope (->> frame (&/get$ "closure") (&/get$ "counter")) register)) register-type))]
      (&/T register* (&/update$ "closure" #(->> %
                                                (&/update$ "counter" inc)
                                                (&/update$ "mappings" (fn [mps] (&/|put ident register* mps))))
                                frame)))))
