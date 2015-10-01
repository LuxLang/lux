;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.type
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type])
            [lux.analyser.base :as &a]))

;; [Utils]
(defn ^:private variant$ [tag body]
  "(-> Text Analysis Analysis)"
  (&a/|meta &type/$Void &/empty-cursor
            (&/V &a/$variant (&/T tag body))
            ))

(defn ^:private tuple$ [members]
  "(-> (List Analysis) Analysis)"
  (&a/|meta &type/$Void &/empty-cursor
            (&/V &a/$tuple members)
            ))

(defn ^:private int$ [value]
  "(-> Int Analysis)"
  (&a/|meta &type/$Void &/empty-cursor
            (&/V &a/$int value)
            ))

(defn ^:private text$ [text]
  "(-> Text Analysis)"
  (&a/|meta &type/$Void &/empty-cursor
            (&/V &a/$text text)
            ))

(def ^:private $Nil
  "Analysis"
  (variant$ &/$Nil (tuple$ &/Nil$)))

(defn ^:private Cons$ [head tail]
  "(-> Analysis Analysis Analysis)"
  (variant$ &/$Cons (tuple$ (&/|list head tail))))

(defn ^:private List$ [elems]
  (&/fold (fn [tail head]
            (Cons$ head tail))
          $Nil
          (&/|reverse elems)))

;; [Exports]
(defn ->analysis [type]
  "(-> Type Analysis)"
  (|case type
    (&/$DataT class params)
    (variant$ &/$DataT (tuple$ (&/|list (text$ class)
                                        (List$ (&/|map ->analysis params)))))
    
    (&/$TupleT members)
    (variant$ &/$TupleT (List$ (&/|map ->analysis members)))

    (&/$VariantT members)
    (variant$ &/$VariantT (List$ (&/|map ->analysis members)))

    (&/$LambdaT input output)
    (variant$ &/$LambdaT (tuple$ (&/|list (->analysis input) (->analysis output))))

    (&/$UnivQ env body)
    (variant$ &/$UnivQ
              (tuple$ (&/|list (List$ (&/|map ->analysis env))
                               (->analysis body))))

    (&/$BoundT idx)
    (variant$ &/$BoundT (int$ idx))

    (&/$AppT fun arg)
    (variant$ &/$AppT (tuple$ (&/|list (->analysis fun) (->analysis arg))))

    (&/$NamedT [module name] type*)
    (variant$ &/$NamedT (tuple$ (&/|list (tuple$ (&/|list (text$ module) (text$ name)))
                                         (->analysis type*))))

    _
    (assert false (prn '->analysis (&type/show-type type) (&/adt->text type)))
    ))
