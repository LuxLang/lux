(ns lux.analyser.def
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return fail
                                     if-m try-all-m map-m mapcat-m reduce-m
                                     assert!]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def init-module
  {::defs {}
   ::macros #{}})

(do-template [<name> <category>]
  (defn <name> [module name]
    (fn [state]
      [::&/ok [state (boolean (get-in state [::&/modules module <category> name]))]]))

  defined? ::defs
  macro?   ::macros
  )

(defn declare-macro [module name]
  (fn [state]
    [::&/ok [(update-in state [::&/modules module ::macros] conj name)
             nil]]))

(defn define [module name type]
  (fn [state]
    (let [full-name (str module &/+name-separator+ name)
          bound [::&&/Expression [::&&/global module name] type]]
      [::&/ok [(-> state
                   (assoc-in [::&/modules module ::defs name] type)
                   (update-in [::&/global-env] merge {full-name bound, name bound}))
               nil]])))
