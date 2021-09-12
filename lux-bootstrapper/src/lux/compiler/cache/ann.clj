(ns lux.compiler.cache.ann
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case]])))

(def ^:private stop (->> 7 char str))
(def ^:private cons-signal (->> 5 char str))
(def ^:private nil-signal (->> 6 char str))

(defn ^:private serialize-seq [serialize params]
  (str (&/fold (fn [so-far param]
                 (str so-far cons-signal (serialize param)))
               ""
               params)
       nil-signal))

(defn ^:private serialize-ident [ident]
  (|let [[module name] ident]
    (str module &/+name-separator+ name)))

(defn serialize
  "(-> Code Text)"
  [ann]
  (|case ann
    [_ (&/$Bit value)]
    (str "B" value stop)

    [_ (&/$Nat value)]
    (str "N" value stop)

    [_ (&/$Int value)]
    (str "I" value stop)

    [_ (&/$Rev value)]
    (str "D" value stop)

    [_ (&/$Frac value)]
    (str "F" value stop)

    [_ (&/$Text value)]
    (str "T" value stop)

    [_ (&/$Identifier ident)]
    (str "@" (serialize-ident ident) stop)

    [_ (&/$Form elems)]
    (str "(" (serialize-seq serialize elems))

    [_ (&/$Tuple elems)]
    (str "[" (serialize-seq serialize elems))

    [_ (&/$Variant kvs)]
    (str "{" (serialize-seq serialize elems))
    
    _
    (assert false)
    ))

(declare deserialize)

(def dummy-location
  (&/T ["" 0 0]))

(do-template [<name> <signal> <ctor> <parser>]
  (defn <name> [^String input]
    (when (.startsWith input <signal>)
      (let [[value* ^String input*] (.split (.substring input 1) stop 2)]
        [(&/T [dummy-location (<ctor> (<parser> value*))]) input*])))

  ^:private deserialize-bit  "B" &/$Bit  Boolean/parseBoolean
  ^:private deserialize-nat  "N" &/$Nat  Long/parseLong
  ^:private deserialize-int  "I" &/$Int  Long/parseLong
  ^:private deserialize-rev  "D" &/$Rev  Long/parseLong
  ^:private deserialize-frac "F" &/$Frac Double/parseDouble
  ^:private deserialize-text "T" &/$Text identity
  )

(do-template [<name> <marker> <tag>]
  (defn <name> [^String input]
    (when (.startsWith input <marker>)
      (let [[^String ident* ^String input*] (.split (.substring input 1) stop 2)
            [_module _name] (.split ident* "\\." 2)]
        [(&/T [dummy-location (<tag> (&/T [_module _name]))]) input*])))

  ^:private deserialize-identifier "@" &/$Identifier)

(defn ^:private deserialize-seq [deserializer ^String input]
  (cond (.startsWith input nil-signal)
        [&/$End (.substring input 1)]

        (.startsWith input cons-signal)
        (when-let [[head ^String input*] (deserializer (.substring input 1))]
          (when-let [[tail ^String input*] (deserialize-seq deserializer input*)]
            [(&/$Item head tail) input*]))
        ))

(do-template [<name> <signal> <type> <deserializer>]
  (defn <name> [^String input]
    (when (.startsWith input <signal>)
      (when-let [[elems ^String input*] (deserialize-seq <deserializer>
                                                         (.substring input 1))]
        [(&/T [dummy-location (<type> elems)]) input*])))

  ^:private deserialize-form   "(" &/$Form   deserialize
  ^:private deserialize-tuple  "[" &/$Tuple  deserialize
  ^:private deserialize-variant "{" &/$Variant deserialize
  )

(defn deserialize
  "(-> Text V[Code Text])"
  [input]
  (or (deserialize-bit input)
      (deserialize-nat input)
      (deserialize-int input)
      (deserialize-rev input)
      (deserialize-frac input)
      (deserialize-text input)
      (deserialize-identifier input)
      (deserialize-form input)
      (deserialize-variant input)
      (deserialize-tuple input)
      (assert false "[Cache Error] Cannot deserialize annocation.")))
