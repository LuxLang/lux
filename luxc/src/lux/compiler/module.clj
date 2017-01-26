(ns lux.compiler.module
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case]]
                 [type :as &type])
            [lux.analyser.module :as &module]))

;; [Exports]
(def tag-groups
  "(Lux (List (, Text (List Text))))"
  (|do [module &/get-current-module]
    (return (&/|map (fn [pair]
                      (|case pair
                        [name [tags exported? _]]
                        (&/T [name (&/|map (fn [tag]
                                             (|let [[t-prefix t-name] tag]
                                               t-name))
                                           tags)])))
                    (&/get$ &module/$types module)))
    ))
