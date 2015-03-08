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
  (match register
    [::&&/Expression _ register-type]
    (let [register* [::&&/Expression [::&&/captured scope (get-in frame [:closure :counter]) register] register-type]]
      [register* (update-in frame [:closure] #(-> %
                                                  (update-in [:counter] inc)
                                                  (assoc-in [:mappings ident] register*)))])))
