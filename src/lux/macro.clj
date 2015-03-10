(ns lux.macro
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [fail* return*]])))

;; [Resources]
(defn expand [loader macro-class tokens]
  (fn [state]
    (let [output (-> (.loadClass loader macro-class)
                     (.getField "_datum")
                     (.get nil)
                     (.apply tokens)
                     (.apply state))]
      (matchv ::M/objects [output]
        [["Ok" [state* tokens*]]]
        (return* state* tokens*)

        [["Error" ?msg]]
        (fail* ?msg)))))
