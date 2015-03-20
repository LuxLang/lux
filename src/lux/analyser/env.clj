(ns lux.analyser.env
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return return* fail]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def next-local-idx
  (fn [state]
    (return* state (->> state (&/get$ "local-envs") &/|head (&/get$ "locals") (&/get$ "counter")))))

(defn with-local [name type body]
  (fn [state]
    (let [old-mappings (->> state (&/get$ "local-envs") &/|head (&/get$ "locals") (&/get$ "mappings"))
          =return (body (&/update$ "local-envs"
                                   (fn [stack]
                                     (let [bound-unit (&/V "local" (->> (&/|head stack) (&/get$ "locals") (&/get$ "counter")))]
                                       (&/|cons (->> (&/|head stack)
                                                     (&/update$ "locals" #(&/update$ "counter" inc %))
                                                     (&/update$ "locals" #(&/update$ "mappings" (fn [m] (&/|put name (&/V "Expression" (&/T bound-unit type)) m)) %)))
                                                (&/|tail stack))))
                                   state))]
      (matchv ::M/objects [=return]
        [["Right" [?state ?value]]]
        (return* (&/update$ "local-envs" (fn [stack*]
                                           (&/|cons (->> (&/|head stack*)
                                                         (&/update$ "locals" #(&/update$ "counter" dec %))
                                                         (&/update$ "locals" #(&/set$ "mappings" old-mappings %)))
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
    (return* state (->> state (&/get$ "local-envs") &/|head (&/get$ "closure") (&/get$ "mappings")))))
