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
                                     (let [bound-unit (&/V "local" (->> (&/|head stack) (&/get$ &/$LOCALS) (&/get$ &/$COUNTER)))]
                                       (&/|cons (->> (&/|head stack)
                                                     (&/update$ &/$LOCALS #(&/update$ &/$COUNTER inc %))
                                                     (&/update$ &/$LOCALS #(&/update$ &/$MAPPINGS (fn [m] (&/|put name (&/T bound-unit type) m)) %)))
                                                (&/|tail stack))))
                                   state))]
      (matchv ::M/objects [=return]
        [["lux;Right" [?state ?value]]]
        (return* (&/update$ &/$ENVS (fn [stack*]
                                               (&/|cons (->> (&/|head stack*)
                                                             (&/update$ &/$LOCALS #(&/update$ &/$COUNTER dec %))
                                                             (&/update$ &/$LOCALS #(&/set$ &/$MAPPINGS old-mappings %)))
                                                        (&/|tail stack*)))
                            ?state)
                 ?value)
        
        [_]
        =return))))

(defn with-locals [locals monad]
  (reduce (fn [inner [label elem]]
            (with-local label elem inner))
          monad
          (reverse locals)))

(def captured-vars
  (fn [state]
    (return* state (->> state (&/get$ &/$ENVS) &/|head (&/get$ &/$CLOSURE) (&/get$ &/$MAPPINGS)))))
