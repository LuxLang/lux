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
  (&/T (&/V &a/$variant (&/T tag body))
       &type/$Void))

(defn ^:private tuple$ [members]
  "(-> (List Analysis) Analysis)"
  (&/T (&/V &a/$tuple members)
       &type/$Void))

(defn ^:private int$ [value]
  "(-> Int Analysis)"
  (&/T (&/V &a/$int value)
       &type/$Void))

(defn ^:private text$ [text]
  "(-> Text Analysis)"
  (&/T (&/V &a/$text text)
       &type/$Void))

(def ^:private $Nil
  "Analysis"
  (variant$ &/$Nil (tuple$ (&/|list))))

(defn ^:private Cons$ [head tail]
  "(-> Analysis Analysis Analysis)"
  (variant$ &/$Cons (tuple$ (&/|list head tail))))

;; [Exports]
(defn ->analysis [type]
  "(-> Type Analysis)"
  (|case type
    (&/$DataT ?class)
    (variant$ &/$DataT (text$ ?class))
    
    (&/$TupleT ?members)
    (variant$ &/$TupleT
              (&/fold (fn [tail head]
                        (Cons$ (->analysis head) tail))
                      $Nil
                      (&/|reverse ?members)))

    (&/$VariantT ?members)
    (variant$ &/$VariantT
              (&/fold (fn [tail head]
                        (Cons$ (->analysis head) tail))
                      $Nil
                      (&/|reverse ?members)))

    (&/$LambdaT ?input ?output)
    (variant$ &/$LambdaT (tuple$ (&/|list (->analysis ?input) (->analysis ?output))))

    (&/$UnivQ ?env ?body)
    (variant$ &/$UnivQ
              (tuple$ (&/|list (&/fold (fn [tail head]
                                         (Cons$ (->analysis head) tail))
                                       $Nil
                                       (&/|reverse ?env))
                               (->analysis ?body))))

    (&/$BoundT ?idx)
    (variant$ &/$BoundT (int$ ?idx))

    (&/$AppT ?fun ?arg)
    (variant$ &/$AppT (tuple$ (&/|list (->analysis ?fun) (->analysis ?arg))))

    (&/$NamedT [?module ?name] ?type)
    (variant$ &/$NamedT (tuple$ (&/|list (tuple$ (&/|list (text$ ?module) (text$ ?name)))
                                         (->analysis ?type))))

    _
    (assert false (&type/show-type type))
    ))
