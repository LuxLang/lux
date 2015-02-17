(ns lux.analyser.env
  (:require [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return fail
                                         try-all-m map-m mapcat-m reduce-m
                                         assert!]])))

;; [Resources]
(def next-local-idx
  (fn [state]
    [::&util/ok [state (-> state ::&util/local-envs first :locals :counter)]]))

(defn defined? [module name]
  (fn [state]
    [::&util/ok [state (get-in state [::&util/modules module name :defined?])]]))

(defn annotated? [module name]
  (fn [state]
    [::&util/ok [state (boolean (get-in state [::&util/modules module name]))]]))

(defn macro? [module name]
  (fn [state]
    [::&util/ok [state (boolean (get-in state [::&util/modules module :macros name]))]]))

(defn annotate [module name access type]
  (fn [state]
    (let [full-name (str module &util/+name-separator+ name)
          bound [::Expression [::global module name] type]]
      [::&util/ok [(-> state
                       (assoc-in [::&util/modules module name] {:args-n [:None]
                                                                :access access
                                                                :type   type
                                                                :defined? false})
                       (update-in [::&util/global-env] merge {full-name bound, name bound}))
                   nil]])))

(defn declare-macro [module name]
  (fn [state]
    [::&util/ok [(assoc-in state [::&util/modules module :macros name] true)
                 nil]]))

(defn define [module name]
  (exec [? annotated?
         _ (assert! ? (str "[Analyser Error] Can't define an unannotated element: " name))]
    (fn [state]
      [::&util/ok [(assoc-in state [::&util/modules module name :defined?] true)
                   nil]])))

(defn with-local [name mode type body]
  (fn [state]
    (let [old-mappings (-> state ::&util/local-envs first (get-in [:locals :mappings]))
          =return (body (update-in state [::&util/local-envs]
                                   (fn [[top & stack]]
                                     (let [bound-unit (case mode
                                                        :self [::self (list)]
                                                        :local [::local (get-in top [:locals :counter])])]
                                       (cons (-> top
                                                 (update-in [:locals :counter] inc)
                                                 (assoc-in [:locals :mappings name] [::Expression bound-unit type]))
                                             stack)))))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(update-in ?state [::&util/local-envs] (fn [[top* & stack*]]
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
    [::&util/ok [state (-> state ::&util/local-envs first :closure :mappings)]]))
