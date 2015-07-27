;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.compiler.type
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let]]
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
  (variant$ "lux;Nil" (tuple$ (&/|list))))

(defn ^:private Cons$ [head tail]
  "(-> Analysis Analysis Analysis)"
  (variant$ "lux;Cons" (tuple$ (&/|list head tail))))

;; [Exports]
(defn ->analysis [type]
  "(-> Type Analysis)"
  (matchv ::M/objects [type]
    [["lux;DataT" ?class]]
    (variant$ "lux;DataT" (text$ ?class))
    
    [["lux;TupleT" ?members]]
    (variant$ "lux;TupleT"
              (&/fold (fn [tail head]
                        (Cons$ (->analysis head) tail))
                      $Nil
                      (&/|reverse ?members)))

    [["lux;VariantT" ?cases]]
    (variant$ "lux;VariantT"
              (&/fold (fn [tail head]
                        (|let [[hlabel htype] head]
                          (Cons$ (tuple$ (&/|list (text$ hlabel) (->analysis htype)))
                                 tail)))
                      $Nil
                      (&/|reverse ?cases)))

    [["lux;RecordT" ?slots]]
    (variant$ "lux;RecordT"
              (&/fold (fn [tail head]
                        (|let [[hlabel htype] head]
                          (Cons$ (tuple$ (&/|list (text$ hlabel) (->analysis htype)))
                                 tail)))
                      $Nil
                      (&/|reverse ?slots)))

    [["lux;LambdaT" [?input ?output]]]
    (variant$ "lux;LambdaT" (tuple$ (&/|list (->analysis ?input) (->analysis ?output))))

    [["lux;AllT" [?env ?name ?arg ?body]]]
    (variant$ "lux;AllT"
              (tuple$ (&/|list (matchv ::M/objects [?env]
                                 [["lux;None" _]]
                                 (variant$ "lux;Some" (tuple$ (&/|list)))

                                 [["lux;Some" ??env]]
                                 (variant$ "lux;Some"
                                           (&/fold (fn [tail head]
                                                     (|let [[hlabel htype] head]
                                                       (Cons$ (tuple$ (&/|list (text$ hlabel) (->analysis htype)))
                                                              tail)))
                                                   $Nil
                                                   (&/|reverse ??env))))
                               (text$ ?name)
                               (text$ ?arg)
                               (->analysis ?body))))

    [["lux;BoundT" ?name]]
    (variant$ "lux;BoundT" (text$ ?name))

    [["lux;AppT" [?fun ?arg]]]
    (variant$ "lux;AppT" (tuple$ (&/|list (->analysis ?fun) (->analysis ?arg))))
    ))
