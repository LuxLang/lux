(ns lux.analyser.env
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def next-local-idx
  (fn [state]
    (return* state (->> state (&/get$ "lux;local-envs") &/|head (&/get$ "lux;locals") (&/get$ "lux;counter")))))

(defn with-local [name type body]
  ;; (prn 'with-local name)
  (fn [state]
    (let [old-mappings (->> state (&/get$ "lux;local-envs") &/|head (&/get$ "lux;locals") (&/get$ "lux;mappings"))
          =return (body (&/update$ "lux;local-envs"
                                   (fn [stack]
                                     (let [bound-unit (&/V "local" (->> (&/|head stack) (&/get$ "lux;locals") (&/get$ "lux;counter")))]
                                       (&/|cons (->> (&/|head stack)
                                                     (&/update$ "lux;locals" #(&/update$ "lux;counter" inc %))
                                                     (&/update$ "lux;locals" #(&/update$ "lux;mappings" (fn [m] (&/|put name (&/V "Expression" (&/T bound-unit type)) m)) %)))
                                                (&/|tail stack))))
                                   state))]
      (matchv ::M/objects [=return]
        [["lux;Right" [?state ?value]]]
        (return* (&/update$ "lux;local-envs" (fn [stack*]
                                               (&/|cons (->> (&/|head stack*)
                                                             (&/update$ "lux;locals" #(&/update$ "lux;counter" dec %))
                                                             (&/update$ "lux;locals" #(&/set$ "lux;mappings" old-mappings %)))
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
    (return* state (->> state (&/get$ "lux;local-envs") &/|head (&/get$ "lux;closure") (&/get$ "lux;mappings")))))
