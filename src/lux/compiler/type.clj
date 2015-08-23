;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.compiler.type
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case $$]]
                 [type :as &type])
            [lux.analyser.base :as &a]))

;; [Utils]
(def ^:private unit$
  "Analysis"
  (&/P (&/S &a/$unit nil)
       &type/$Void))

(defn ^:private sum$ [tag body]
  "(-> Int Analysis Analysis)"
  (&/P (&/S &a/$sum (&/P tag body))
       &type/$Void))

(defn ^:private prod$ [left right]
  "(-> Analysis Analysis Analysis)"
  (&/P (&/S &a/$prod (&/P left right))
       &type/$Void))

(defn ^:private text$ [text]
  "(-> Text Analysis)"
  (&/P (&/S &a/$text text)
       &type/$Void))

(def ^:private $Nil
  "Analysis"
  (sum$ &/$Nil unit$))

(defn ^:private Cons$ [head tail]
  "(-> Analysis Analysis Analysis)"
  (sum$ &/$Cons (prod$ head tail)))

;; [Exports]
(defn ->analysis [type]
  "(-> Type Analysis)"
  (|case type
    (&/$DataT ?class)
    (sum$ &/$DataT (text$ ?class))
    
    (&/$ProdT left right)
    (sum$ &/$ProdT
          (prod$ (->analysis left)
                 (->analysis right)))

    (&/$SumT left right)
    (sum$ &/$SumT
          (prod$ (->analysis left)
                 (->analysis right)))

    (&/$LambdaT ?input ?output)
    (sum$ &/$LambdaT (prod$ (->analysis ?input) (->analysis ?output)))

    (&/$AllT ?env ?name ?arg ?body)
    (sum$ &/$AllT
          ($$ prod$
              (|case ?env
                (&/$None)
                (sum$ &/$None unit$)

                (&/$Some ??env)
                (sum$ &/$Some
                      (&/fold (fn [tail head]
                                (|let [[hlabel htype] head]
                                  (Cons$ (prod$ (text$ hlabel)
                                                (->analysis htype))
                                         tail)))
                              $Nil
                              (&/|reverse ??env))))
              (text$ ?name)
              (text$ ?arg)
              (->analysis ?body)))

    (&/$BoundT ?name)
    (sum$ &/$BoundT (text$ ?name))

    (&/$AppT ?fun ?arg)
    (sum$ &/$AppT (prod$ (->analysis ?fun) (->analysis ?arg)))

    (&/$NamedT [?module ?name] ?type)
    (sum$ &/$NamedT (prod$ (prod$ (text$ ?module) (text$ ?name))
                           (->analysis ?type)))
    ))
