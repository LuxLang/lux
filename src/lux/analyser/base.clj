(ns lux.analyser.base
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return fail]]
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

(defn analyse-1 [analyse exo-type elem]
  (|do [output (analyse exo-type elem)]
    (do ;; (prn 'analyse-1 (aget output 0))
        (matchv ::M/objects [output]
          [["lux;Cons" [x ["lux;Nil" _]]]]
          (return x)

          [_]
          (fail "[Analyser Error] Can't expand to other than 1 element.")))))

(defn analyse-2 [analyse el1 el2]
  (|do [output (&/flat-map% analyse (&/|list el1 el2))]
    (do ;; (prn 'analyse-2 (aget output 0))
        (matchv ::M/objects [output]
          [["lux;Cons" [x ["lux;Cons" [y ["lux;Nil" _]]]]]]
          (return [x y])

          [_]
          (fail "[Analyser Error] Can't expand to other than 2 elements.")))))

(defn resolved-ident [ident]
  (|let [[?module ?name] ident]
    (|do [module* (if (= "" ?module)
                    &/get-module-name
                    (return ?module))]
      (return (&/ident->text (&/T module* ?name))))))
