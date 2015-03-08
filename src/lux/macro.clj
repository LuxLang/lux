(ns lux.macro
  (:require [clojure.core.match :refer [match]]
            [lux.parser :as &parser]))

;; [Utils]
(defn ^:private ->lux+ [->lux loader xs]
  (reduce (fn [tail x]
            (to-array ["Cons" (to-array [(->lux loader x) tail])]))
          (to-array ["Nil" (to-array [])])
          (reverse xs)))

(defn ^:private ->lux-one [loader tag value]
  (to-array [tag value]))

(defn ^:private ->lux-many [->lux loader tag values]
  (to-array [tag (->lux+ ->lux loader values)]))

(defn ^:private ->lux [loader x]
  (match x
    [::&parser/Bool ?value]
    (->lux-one loader "Bool" ?value)
    [::&parser/Int ?value]
    (->lux-one loader "Int" ?value)
    [::&parser/Real ?value]
    (->lux-one loader "Real" ?value)
    [::&parser/Char ?value]
    (->lux-one loader "Char" ?value)
    [::&parser/Text ?value]
    (->lux-one loader "Text" ?value)
    [::&parser/Tag ?value]
    (->lux-one loader "Tag" ?value)
    [::&parser/Ident ?value]
    (->lux-one loader "Ident" ?value)
    [::&parser/Tuple ?elems]
    (->lux-many ->lux loader "Tuple" ?elems)
    [::&parser/Form ?elems]
    (->lux-many ->lux loader "Form" ?elems)
    ))

(defn ^:private ->clojure+ [->clojure xs]
  (case (aget xs 0)
    "Nil"  (list)
    "Cons" (let [tuple2 (aget xs 1)]
             (cons (->clojure (aget tuple2 0))
                   (->clojure+ ->clojure (aget tuple2 1))))
    ))

(defn ^:private ->clojure [x]
  (case (aget x 0)
    "Bool"  [::&parser/Bool  (aget x 1)]
    "Int"   [::&parser/Int   (aget x 1)]
    "Real"  [::&parser/Real  (aget x 1)]
    "Char"  [::&parser/Char  (aget x 1)]
    "Text"  [::&parser/Text  (aget x 1)]
    "Tag"   [::&parser/Tag   (aget x 1)]
    "Ident" [::&parser/Ident (aget x 1)]
    "Tuple" [::&parser/Tuple (->clojure+ ->clojure (aget x 1))]
    "Form"  [::&parser/Form  (->clojure+ ->clojure (aget x 1))]))

;; [Resources]
(defn expand [loader macro-class tokens]
  (let [output (-> (.loadClass loader macro-class)
                   (.getField "_datum")
                   (.get nil)
                   (.apply (->lux+ ->lux loader tokens))
                   (.apply nil))]
    [(->clojure+ ->clojure (aget output 0))
     (aget output 1)]))
