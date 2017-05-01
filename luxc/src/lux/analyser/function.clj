(ns lux.analyser.function
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return |case]]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [env :as &env])))

;; [Resource]
(defn with-function [self self-type arg arg-type body]
  (&/with-closure
    (|do [scope-name &/get-scope-name]
      (&env/with-local self self-type
        (&env/with-local arg arg-type
          (|do [=return body
                =captured &env/captured-vars]
            (return (&/T [scope-name =captured =return]))))))))

(defn close-over [scope name register frame]
  (|let [[[register-type register-cursor] _] register
         register* (&&/|meta register-type register-cursor
                             (&&/$captured (&/T [scope
                                                 (->> frame (&/get$ &/$captured) (&/get$ &/$counter))
                                                 register])))]
    (&/T [register* (&/update$ &/$captured #(->> %
                                                 (&/update$ &/$counter inc)
                                                 (&/update$ &/$mappings (fn [mps] (&/|put name register* mps))))
                               frame)])))
