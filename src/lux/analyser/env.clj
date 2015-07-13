(ns lux.analyser.env
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def next-local-idx
  (fn [state]
    (return* state (->> state (&/get$ &/$ENVS) &/|head (&/get$ &/$LOCALS) (&/get$ &/$COUNTER)))))

(defn with-local [name type body]
  ;; (prn 'with-local name)
  (fn [state]
    (let [old-mappings (->> state (&/get$ &/$ENVS) &/|head (&/get$ &/$LOCALS) (&/get$ &/$MAPPINGS))
          =return (body (&/update$ &/$ENVS
                                   (fn [stack]
                                     (let [bound-unit (&/V "lux;Local" (->> (&/|head stack) (&/get$ &/$LOCALS) (&/get$ &/$COUNTER)))]
                                       (&/|cons (&/update$ &/$LOCALS #(->> %
                                                                           (&/update$ &/$COUNTER inc)
                                                                           (&/update$ &/$MAPPINGS (fn [m] (&/|put name (&/T bound-unit type) m))))
                                                           (&/|head stack))
                                                (&/|tail stack))))
                                   state))]
      (matchv ::M/objects [=return]
        [["lux;Right" [?state ?value]]]
        (return* (&/update$ &/$ENVS (fn [stack*]
                                      (&/|cons (&/update$ &/$LOCALS #(->> %
                                                                          (&/update$ &/$COUNTER dec)
                                                                          (&/set$ &/$MAPPINGS old-mappings))
                                                          (&/|head stack*))
                                               (&/|tail stack*)))
                            ?state)
                 ?value)
        
        [_]
        =return))))

(def captured-vars
  (fn [state]
    (return* state (->> state (&/get$ &/$ENVS) &/|head (&/get$ &/$CLOSURE) (&/get$ &/$MAPPINGS)))))
