;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.base
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [defvariant |let |do return* return fail |case]]
                 [type :as &type])))

;; [Tags]
(defvariant
  ("bool" 1)
  ("int" 1)
  ("real" 1)
  ("char" 1)
  ("text" 1)
  ("variant" 3)
  ("tuple" 1)
  ("apply" 2)
  ("case" 2)
  ("lambda" 3)
  ("ann" 3)
  ("def" 3)
  ("var" 1)
  ("captured" 1)
  ("host" 2)

  ("jvm-class" 1)
  ("jvm-interface" 1)
  ("jvm-program" 1)
  )

;; [Exports]
(defn expr-type* [analysis]
  (|let [[[type _] _] analysis]
    type))

(defn expr-term [analysis]
  (|let [[[type _] term] analysis]
    term))

(defn with-type [new-type analysis]
  (|let [[[type cursor] adt] analysis]
    (&/T [(&/T [new-type cursor]) adt])))

(defn clean-analysis [$var an]
  "(-> Type Analysis (Lux Analysis))"
  (|do [=an-type (&type/clean $var (expr-type* an))]
    (return (with-type =an-type an))))

(def jvm-this "_jvm_this")

(defn cap-1 [action]
  (|do [result action]
    (|case result
      (&/$Cons x (&/$Nil))
      (return x)

      _
      (fail "[Analyser Error] Can't expand to other than 1 element."))))

(defn analyse-1 [analyse exo-type elem]
  (cap-1 (analyse exo-type elem)))

(defn analyse-1+ [analyse ?token]
  (&type/with-var
    (fn [$var]
      (|do [=expr (analyse-1 analyse $var ?token)]
        (clean-analysis $var =expr)))))

(defn resolved-ident [ident]
  (|do [:let [[?module ?name] ident]
        module* (if (.equals "" ?module)
                  &/get-module-name
                  (return ?module))]
    (return (&/T [module* ?name]))))

(let [tag-names #{"DataT" "VoidT" "UnitT" "SumT" "ProdT" "LambdaT" "BoundT" "VarT" "ExT" "UnivQ" "ExQ" "AppT" "NamedT"}]
  (defn type-tag? [module name]
    (and (= "lux" module)
         (contains? tag-names name))))

(defn |meta [type cursor analysis]
  (&/T [(&/T [type cursor]) analysis]))
