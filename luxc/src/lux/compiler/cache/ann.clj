;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.cache.ann
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail* |case]])))

(def ^:private stop (->> 7 char str))
(def ^:private cons-signal (->> 5 char str))
(def ^:private nil-signal (->> 6 char str))
(def ^:private ident-separator ";")

(defn ^:private serialize-seq [serialize-ann params]
  (str (&/fold (fn [so-far param]
                 (str so-far cons-signal (serialize-ann param)))
               ""
               params)
       nil-signal))

(defn ^:private serialize-text [value]
  (str "T" value stop))

(defn ^:private serialize-ident [ident]
  (|let [[module name] ident]
    (str "@" module ident-separator name stop)))

(defn serialize-ann
  "(-> Ann-Value Text)"
  [ann]
  (|case ann
    (&/$BoolM value)
    (str "B" value stop)

    (&/$NatM value)
    (str "N" value stop)

    (&/$IntM value)
    (str "I" value stop)

    (&/$FracM value)
    (str "F" value stop)

    (&/$RealM value)
    (str "R" value stop)

    (&/$CharM value)
    (str "C" value stop)

    (&/$TextM value)
    (serialize-text value)

    (&/$IdentM ident)
    (serialize-ident ident)

    (&/$ListM elems)
    (str "L" (serialize-seq serialize-ann elems))

    (&/$DictM kvs)
    (str "D" (serialize-seq (fn [kv]
                              (|let [[k v] kv]
                                (str (serialize-text k)
                                     (serialize-ann v))))
                            kvs))
    
    _
    (assert false)
    ))

(defn serialize-anns
  "(-> Anns Text)"
  [anns]
  (serialize-seq (fn [kv]
                   (|let [[k v] kv]
                     (str (serialize-ident k)
                          (serialize-ann v))))
                 anns))

(declare deserialize-ann)

(do-template [<name> <signal> <ctor> <parser>]
  (defn <name> [^String input]
    (when (.startsWith input <signal>)
      (let [[value* ^String input*] (.split (.substring input 1) stop 2)]
        [(<ctor> (<parser> value*)) input*])))

  ^:private deserialize-bool "B" &/$BoolM Boolean/parseBoolean
  ^:private deserialize-nat  "N" &/$NatM  Long/parseLong
  ^:private deserialize-int  "I" &/$IntM  Long/parseLong
  ^:private deserialize-frac "F" &/$FracM Long/parseLong
  ^:private deserialize-real "R" &/$RealM Double/parseDouble
  ^:private deserialize-char "C" &/$CharM (fn [^String input] (.charAt input 0))
  ^:private deserialize-text "T" &/$TextM identity
  )

(defn ^:private deserialize-ident* [^String input]
  (when (.startsWith input "@")
    (let [[^String ident* ^String input*] (.split (.substring input 1) stop 2)
          [_module _name] (.split ident* ident-separator 2)]
      [(&/T [_module _name]) input*])))

(defn ^:private deserialize-ident [^String input]
  (when (.startsWith input "@")
    (let [[^String ident* ^String input*] (.split (.substring input 1) stop 2)
          [_module _name] (.split ident* ident-separator 2)]
      [(&/$IdentM (&/T [_module _name])) input*])))

(defn ^:private deserialize-seq [deserializer ^String input]
  (cond (.startsWith input nil-signal)
        [&/$Nil (.substring input 1)]

        (.startsWith input cons-signal)
        (when-let [[head ^String input*] (deserializer (.substring input 1))]
          (when-let [[tail ^String input*] (deserialize-seq deserializer input*)]
            [(&/$Cons head tail) input*]))
        ))

(do-template [<name> <deserialize-key>]
  (defn <name> [input]
    (when-let [[key input*] (<deserialize-key> input)]
      (when-let [[ann input*] (deserialize-ann input*)]
        [(&/T [key ann]) input*])))

  ^:private deserialize-kv        deserialize-text
  ^:private deserialize-ann-entry deserialize-ident*
  )

(do-template [<name> <signal> <type> <deserializer>]
  (defn <name> [^String input]
    (when (.startsWith input <signal>)
      (when-let [[elems ^String input*] (deserialize-seq <deserializer>
                                                         (.substring input 1))]
        [(<type> elems) input*])))

  ^:private deserialize-list "L" &/$ListM deserialize-ann
  ^:private deserialize-dict "D" &/$DictM deserialize-kv
  )

(defn ^:private deserialize-ann
  "(-> Text Anns)"
  [input]
  (or (deserialize-bool input)
      (deserialize-nat input)
      (deserialize-int input)
      (deserialize-frac input)
      (deserialize-real input)
      (deserialize-char input)
      (deserialize-text input)
      (deserialize-ident input)
      (deserialize-list input)
      (deserialize-dict input)
      (assert false "[Cache error] Can't deserialize annocation.")))

(defn deserialize-anns [^String input]
  (deserialize-seq deserialize-ann-entry input))
