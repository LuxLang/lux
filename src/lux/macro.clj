(ns lux.macro
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [fail* return*]])))

;; [Resources]
(defn expand [loader macro-class tokens]
  (fn [state]
    ;; (prn 'expand macro-class tokens state)
    ;; (let [expansion (-> (.loadClass loader macro-class)
    ;;                     (.getField "_datum")
    ;;                     (.get nil)
    ;;                     (.apply tokens)
    ;;                     (.apply state))]
    ;;   (matchv ::M/objects [expansion]
    ;;     [["lux;Right" [state* nodes]]]
    ;;     (doseq [node (&/->seq nodes)]
    ;;       (prn 'expansion macro-class (&/show-ast node)))

    ;;     [_]
    ;;     nil)
    ;;   expansion)
    (-> (.loadClass loader macro-class)
        (.getField "_datum")
        (.get nil)
        (.apply tokens)
        (.apply state))))
