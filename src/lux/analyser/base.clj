(ns lux.analyser.base
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return fail]]
                 [type :as &type])))

;; [Resources]
(defn expr-type [syntax+]
  ;; (prn 'expr-type syntax+)
  ;; (prn 'expr-type (aget syntax+ 0))
  (matchv ::M/objects [syntax+]
    [["Expression" [_ type]]]
    (do ;; (prn 'expr-type (&type/show-type type))
      (return type))
    
    [["Statement" _]]
    (fail (str "[Analyser Error] Can't retrieve the type of a statement: " (pr-str syntax+)))))

(defn analyse-1 [analyse elem]
  (exec [output (analyse elem)]
    (do ;; (prn 'analyse-1 (aget output 0))
      (matchv ::M/objects [output]
        [["Cons" [x ["Nil" _]]]]
        (return x)

        [_]
        (fail "[Analyser Error] Can't expand to other than 1 element.")))))

(defn analyse-2 [analyse el1 el2]
  (exec [output (&/flat-map% analyse (&/|list el1 el2))]
    (do ;; (prn 'analyse-2 (aget output 0))
      (matchv ::M/objects [output]
        [["Cons" [x ["Cons" [y ["Nil" _]]]]]]
        (return [x y])

        [_]
        (fail "[Analyser Error] Can't expand to other than 2 elements.")))))

(defn with-var [k]
  (exec [=var &type/fresh-var
         =ret (k =var)]
    (matchv ::M/objects [=ret]
      [["Expression" [?expr ?type]]]
      (exec [=type (&type/clean =var ?type)]
        (return (&/V "Expression" (&/T ?expr =type)))))))
