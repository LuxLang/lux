(ns lux.analyser.base
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return fail
                                     try-all-m map-m mapcat-m reduce-m
                                     assert!]]
                 [type :as &type])))

;; [Resources]
(defn expr-type [syntax+]
  ;; (prn 'expr-type syntax+)
  (match syntax+
    [::Expression _ type]
    (return type)

    _
    (fail (str "[Analyser Error] Can't retrieve the type of a non-expression: " (pr-str syntax+)))))

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

(defn with-var [k]
  (exec [=var &type/fresh-var
         =ret (k =var)]
    (match =ret
      [::Expression ?expr ?type]
      (exec [=type (&type/clean =var ?type)]
        (return [::Expression ?expr =type])))))
