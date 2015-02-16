(ns lux.macro
  (:require [clojure.core.match :refer [match]]
            [lux.parser :as &parser]))

;; [Utils]
(defn ^:private ->lux+ [->lux loader xs]
  (reduce (fn [tail x]
            (doto (.newInstance (.loadClass loader "lux.Variant2"))
              (-> .-tag (set! "Cons"))
              (-> .-_1 (set! (->lux loader x)))
              (-> .-_2 (set! tail))))
          (doto (.newInstance (.loadClass loader "lux.Variant0"))
            (-> .-tag (set! "Nil")))
          (reverse xs)))

(defn ^:private ->lux-one [loader tag value]
  (doto (.newInstance (.loadClass loader "lux.Variant1"))
    (-> .-tag (set! tag))
    (-> .-_1  (set! value))))

(defn ^:private ->lux-many [->lux loader tag values]
  (doto (.newInstance (.loadClass loader "lux.Variant1"))
    (-> .-tag (set! tag))
    (-> .-_1  (set! (->lux+ ->lux loader values)))))

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
  (case (.-tag xs)
    "Nil"  (list)
    "Cons" (cons (->clojure (.-_1 xs))
                 (->clojure+ ->clojure (.-_2 xs)))
    ))

(defn ^:private ->clojure [x]
  (case (.-tag x)
    "Bool"  [::&parser/Bool  (.-_1 x)]
    "Int"   [::&parser/Int   (.-_1 x)]
    "Real"  [::&parser/Real  (.-_1 x)]
    "Char"  [::&parser/Char  (.-_1 x)]
    "Text"  [::&parser/Text  (.-_1 x)]
    "Tag"   [::&parser/Tag   (.-_1 x)]
    "Ident" [::&parser/Ident (.-_1 x)]
    "Tuple" [::&parser/Tuple (->clojure+ ->clojure (.-_1 x))]
    "Form"  [::&parser/Form  (->clojure+ ->clojure (.-_1 x))]))

;; [Resources]
(defn expand [loader macro-class tokens]
  (let [output (-> (.loadClass loader macro-class)
                   .getDeclaredConstructors
                   first
                   (.newInstance (to-array [(int 0) nil]))
                   (.apply (->lux+ ->lux loader tokens))
                   (.apply nil))]
    [(->> output .-_1 (->clojure+ ->clojure))
     (.-_2 output)]))
