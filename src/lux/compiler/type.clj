;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.type
  (:require [clojure.template :refer [do-template]]
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type])
            [lux.analyser.base :as &a]))

;; [Utils]
(defn ^:private variant$ [tag body]
  "(-> Text Analysis Analysis)"
  (&a/|meta &type/$Void &/empty-cursor
            (&/V &a/$variant (&/T tag body))))

(defn ^:private tuple$ [members]
  "(-> (List Analysis) Analysis)"
  (&a/|meta &type/$Void &/empty-cursor
            (&/V &a/$tuple members)))

(do-template [<name> <tag> <doc>]
  (defn <name> [value]
    <doc>
    (&a/|meta &type/$Void &/empty-cursor
              (&/V <tag> value)))

  ^:private bool$ &a/$bool "(-> Bool Analysis)"
  ^:private int$  &a/$int  "(-> Int Analysis)"
  ^:private real$ &a/$real "(-> Real Analysis)"
  ^:private char$ &a/$char "(-> Char Analysis)"
  ^:private text$ &a/$text "(-> Text Analysis)"
  )

(defn ^:private ident$ [value]
  "(-> Ident Analysis)"
  (|let [[p n] value]
    (tuple$ (&/|list (text$ p) (text$ n)))))

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
(defn type->analysis [type]
  "(-> Type Analysis)"
  (|case type
    (&/$DataT class params)
    (variant$ &/$DataT (tuple$ (&/|list (text$ class)
                                        (List$ (&/|map type->analysis params)))))

    (&/$VoidT)
    (variant$ &/$VoidT (tuple$ (&/|list)))

    (&/$UnitT)
    (variant$ &/$UnitT (tuple$ (&/|list)))
    
    (&/$TupleT members)
    (variant$ &/$TupleT (List$ (&/|map type->analysis members)))

    (&/$SumT left right)
    (variant$ &/$SumT (tuple$ (&/|list (type->analysis left) (type->analysis right))))

    (&/$LambdaT input output)
    (variant$ &/$LambdaT (tuple$ (&/|list (type->analysis input) (type->analysis output))))

    (&/$UnivQ env body)
    (variant$ &/$UnivQ
              (tuple$ (&/|list (List$ (&/|map type->analysis env))
                               (type->analysis body))))

    (&/$BoundT idx)
    (variant$ &/$BoundT (int$ idx))

    (&/$AppT fun arg)
    (variant$ &/$AppT (tuple$ (&/|list (type->analysis fun) (type->analysis arg))))

    (&/$NamedT [module name] type*)
    (variant$ &/$NamedT (tuple$ (&/|list (tuple$ (&/|list (text$ module) (text$ name)))
                                         (type->analysis type*))))

    _
    (assert false (prn 'type->analysis (&/adt->text type)))
    ))

(defn ^:private defmetavalue->analysis [dmv]
  "(-> DefMetaValue Analysis)"
  (|case dmv
    (&/$BoolM value)
    (variant$ &/$BoolM (bool$ value))
    
    (&/$IntM value)
    (variant$ &/$IntM (int$ value))

    (&/$RealM value)
    (variant$ &/$RealM (real$ value))

    (&/$CharM value)
    (variant$ &/$CharM (char$ value))

    (&/$TextM value)
    (variant$ &/$TextM (text$ value))

    (&/$IdentM value)
    (variant$ &/$IdentM (ident$ value))

    (&/$ListM xs)
    (variant$ &/$ListM (List$ (&/|map defmetavalue->analysis xs)))
    
    (&/$DictM kvs)
    (variant$ &/$DictM
              (List$ (&/|map (fn [kv]
                               (|let [[k v] kv]
                                 (tuple$ (&/|list (text$ k)
                                                  (defmetavalue->analysis v)))))
                             kvs)))

    _
    (assert false (prn 'defmetavalue->analysis (&/adt->text dmv)))
    ))

(defn defmeta->analysis [xs]
  "(-> DefMeta Analysis)"
  (List$ (&/|map (fn [kv]
                   (|let [[k v] kv]
                     (tuple$ (&/|list (ident$ k)
                                      (defmetavalue->analysis v)))))
                 xs)))
