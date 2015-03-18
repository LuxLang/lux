(ns lux.analyser.def
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return return* fail
                                     if-m try-all-m map-m mapcat-m reduce-m
                                     assert!]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def init-module
  (R "defs" (|table)
     "macros" (|table)))

(do-template [<name> <category>]
  (defn <name> [module name]
    (fn [state]
      (return* state
               (->> state (get$ "modules") (|get module) (get$ <category>) (|get name) boolean))))

  defined? "defs"
  macro?   "macros"
  )

(defn declare-macro [module name]
  (fn [state]
    (return* (update$ "modules" (fn [ms] (|update module (fn [m] (update$ "macros" #(|put name true %) m)) ms)) state)
             nil)))

(defn define [module name type]
  (fn [state]
    (let [full-name (str module &/+name-separator+ name)
          bound (&/V "Expression" (&/T (&/V "global" (&/T module name)) type))]
      (return* (->> state
                    (update$ "modules" (fn [ms] (|update module (fn [m] (update$ "defs" #(|put name type %) m)) ms)))
                    (update$ "global-env" #(|merge (|table full-name bound, name bound) %)))
               nil))))
