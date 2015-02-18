(ns lux.analyser.base
  (:require [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return fail
                                     try-all-m map-m mapcat-m reduce-m
                                     assert!]])))

;; [Resources]
(defn expr-type [syntax+]
  (prn 'expr-type syntax+)
  (match syntax+
    [::Expression _ type]
    (return type)

    _
    (fail "[Analyser Error] Can't retrieve the type of a non-expression.")))

(defn analyse-1 [analyse elem]
  (exec [output (analyse elem)]
    (match output
      ([x] :seq)
      (return x)

      :else
      (fail "[Analyser Error] Can't expand to other than 1 element."))))

(defn analyse-2 [analyse el1 el2]
  (exec [output (mapcat-m analyse (list el1 el2))]
    (match output
      ([x y] :seq)
      (return [x y])

      :else
      (fail "[Analyser Error] Can't expand to other than 2 elements."))))
