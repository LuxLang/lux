(ns lux.analyser.def
  (:require [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return fail
                                     if-m try-all-m map-m mapcat-m reduce-m
                                     assert!]])
            [lux.analyser.base :as &&]))

;; [Exports]
(defn defined? [module name]
  (fn [state]
    [::&/ok [state (get-in state [::&/modules module name :defined?])]]))

(defn annotated? [module name]
  (fn [state]
    [::&/ok [state (boolean (get-in state [::&/modules module name]))]]))

(defn macro? [module name]
  (fn [state]
    [::&/ok [state (boolean (get-in state [::&/modules module :macros name]))]]))

(defn annotate [module name access type]
  (fn [state]
    (let [full-name (str module &/+name-separator+ name)
          bound [::&&/Expression [::&&/global module name] type]]
      [::&/ok [(-> state
                   (assoc-in [::&/modules module name] {:args-n [:None]
                                                        :access access
                                                        :type   type
                                                        :defined? false})
                   (update-in [::&/global-env] merge {full-name bound, name bound}))
               nil]])))

(defn declare-macro [module name]
  (fn [state]
    [::&/ok [(assoc-in state [::&/modules module :macros name] true)
             nil]]))

(defn define [module name]
  (if-m (annotated? module name)
        (fn [state]
          [::&/ok [(assoc-in state [::&/modules module name :defined?] true)
                   nil]])
        (fail (str "[Analyser Error] Can't define an unannotated element: " name))))
