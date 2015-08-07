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
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type])))

;; [Utils]
(defn ^:private variant$ [tag body]
  "(-> Text Analysis Analysis)"
  (&/T (&/V "variant" (&/T tag body))
       &type/$Void))

(defn ^:private tuple$ [members]
  "(-> (List Analysis) Analysis)"
  (&/T (&/V "tuple" members)
       &type/$Void))

(defn ^:private text$ [text]
  "(-> Text Analysis)"
  (&/T (&/V "text" text)
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

    (&/$VariantT ?cases)
    (variant$ &/$VariantT
              (&/fold (fn [tail head]
                        (|let [[hlabel htype] head]
                          (Cons$ (tuple$ (&/|list (text$ hlabel) (->analysis htype)))
                                 tail)))
                      $Nil
                      (&/|reverse ?cases)))

    (&/$RecordT ?slots)
    (variant$ &/$RecordT
              (&/fold (fn [tail head]
                        (|let [[hlabel htype] head]
                          (Cons$ (tuple$ (&/|list (text$ hlabel) (->analysis htype)))
                                 tail)))
                      $Nil
                      (&/|reverse ?slots)))

    (&/$LambdaT ?input ?output)
    (variant$ &/$LambdaT (tuple$ (&/|list (->analysis ?input) (->analysis ?output))))

    (&/$AllT ?env ?name ?arg ?body)
    (variant$ &/$AllT
              (tuple$ (&/|list (|case ?env
                                 (&/$None)
                                 (variant$ &/$None (tuple$ (&/|list)))

                                 (&/$Some ??env)
                                 (variant$ &/$Some
                                           (&/fold (fn [tail head]
                                                     (|let [[hlabel htype] head]
                                                       (Cons$ (tuple$ (&/|list (text$ hlabel) (->analysis htype)))
                                                              tail)))
                                                   $Nil
                                                   (&/|reverse ??env))))
                               (text$ ?name)
                               (text$ ?arg)
                               (->analysis ?body))))

    (&/$BoundT ?name)
    (variant$ &/$BoundT (text$ ?name))

    (&/$AppT ?fun ?arg)
    (variant$ &/$AppT (tuple$ (&/|list (->analysis ?fun) (->analysis ?arg))))
    ))
