(ns lux.compiler.cache.ann
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case]])))

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
    (&/$BoolA value)
    (str "B" value stop)

    (&/$NatA value)
    (str "N" value stop)

    (&/$IntA value)
    (str "I" value stop)

    (&/$DegA value)
    (str "D" value stop)

    (&/$RealA value)
    (str "R" value stop)

    (&/$CharA value)
    (str "C" value stop)

    (&/$TextA value)
    (serialize-text value)

    (&/$IdentA ident)
    (serialize-ident ident)

    (&/$ListA elems)
    (str "L" (serialize-seq serialize-ann elems))

    (&/$DictA kvs)
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

  ^:private deserialize-bool "B" &/$BoolA Boolean/parseBoolean
  ^:private deserialize-nat  "N" &/$NatA  Long/parseLong
  ^:private deserialize-int  "I" &/$IntA  Long/parseLong
  ^:private deserialize-deg "D" &/$DegA Long/parseLong
  ^:private deserialize-real "R" &/$RealA Double/parseDouble
  ^:private deserialize-char "C" &/$CharA (fn [^String input] (.charAt input 0))
  ^:private deserialize-text "T" &/$TextA identity
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
      [(&/$IdentA (&/T [_module _name])) input*])))

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

  ^:private deserialize-list "L" &/$ListA deserialize-ann
  ^:private deserialize-dict "D" &/$DictA deserialize-kv
  )

(defn ^:private deserialize-ann
  "(-> Text Anns)"
  [input]
  (or (deserialize-bool input)
      (deserialize-nat input)
      (deserialize-int input)
      (deserialize-deg input)
      (deserialize-real input)
      (deserialize-char input)
      (deserialize-text input)
      (deserialize-ident input)
      (deserialize-list input)
      (deserialize-dict input)
      (assert false "[Cache error] Cannot deserialize annocation.")))

(defn deserialize-anns
  "(-> Text Text)"
  [^String input]
  (let [[output _] (deserialize-seq deserialize-ann-entry input)]
    output))
