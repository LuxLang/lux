(ns lux.analyser.env
  (:require [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return fail
                                     if-m try-all-m map-m mapcat-m reduce-m
                                     assert!]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def next-local-idx
  (fn [state]
    [::&/ok [state (-> state ::&/local-envs first :locals :counter)]]))

(defn with-local [name mode type body]
  (fn [state]
    (let [old-mappings (-> state ::&/local-envs first (get-in [:locals :mappings]))
          =return (body (update-in state [::&/local-envs]
                                   (fn [[top & stack]]
                                     (prn 'env/with-local name mode (get-in top [:locals :counter]))
                                     (let [bound-unit (case mode
                                                        :self  [::&&/self (list)]
                                                        :local [::&&/local (get-in top [:locals :counter])])]
                                       (cons (-> top
                                                 (update-in [:locals :counter] inc)
                                                 (assoc-in [:locals :mappings name] [::&&/Expression bound-unit type]))
                                             stack)))))]
      (match =return
        [::&/ok [?state ?value]]
        [::&/ok [(update-in ?state [::&/local-envs] (fn [[top* & stack*]]
                                                      (cons (-> top*
                                                                (update-in [:locals :counter] dec)
                                                                (assoc-in [:locals :mappings] old-mappings))
                                                            stack*)))
                 ?value]]
        
        _
        =return))))

(defn with-locals [locals monad]
  (reduce (fn [inner [label elem]]
            (with-local label :local elem inner))
          monad
          (reverse locals)))

(def captured-vars
  (fn [state]
    [::&/ok [state (-> state ::&/local-envs first :closure :mappings)]]))
