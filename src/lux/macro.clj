(ns lux.macro
  (:require [clojure.core.match :refer [match]]
            [lux.parser :as &parser]))

;; [Utils]
(defn ^:private ->lux+ [->lux loader xs]
  (reduce (fn [tail x]
            (doto (.newInstance (.loadClass loader "lux.Variant"))
              (-> .-tag (set! "Cons"))
              (-> .-value (set! (doto (.newInstance (.loadClass loader "lux.Tuple2"))
                                  (-> .-_0 (set! (->lux loader x)))
                                  (-> .-_1 (set! tail)))))))
          (doto (.newInstance (.loadClass loader "lux.Variant"))
            (-> .-tag (set! "Nil"))
            (-> .-value (set! (.newInstance (.loadClass loader "lux.Tuple0")))))
          (reverse xs)))

(defn ^:private ->lux-one [loader tag value]
  (doto (.newInstance (.loadClass loader "lux.Variant"))
    (-> .-tag (set! tag))
    (-> .-value (set! value))))

(defn ^:private ->lux-many [->lux loader tag values]
  (doto (.newInstance (.loadClass loader "lux.Variant"))
    (-> .-tag (set! tag))
    (-> .-value (set! (->lux+ ->lux loader values)))))

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
    "Cons" (let [tuple2 (.-value xs)]
             (cons (->clojure (.-_0 tuple2))
                   (->clojure+ ->clojure (.-_1 tuple2))))
    ))

(defn ^:private ->clojure [x]
  (case (.-tag x)
    "Bool"  [::&parser/Bool  (.-value x)]
    "Int"   [::&parser/Int   (.-value x)]
    "Real"  [::&parser/Real  (.-value x)]
    "Char"  [::&parser/Char  (.-value x)]
    "Text"  [::&parser/Text  (.-value x)]
    "Tag"   [::&parser/Tag   (.-value x)]
    "Ident" [::&parser/Ident (.-value x)]
    "Tuple" [::&parser/Tuple (->clojure+ ->clojure (.-value x))]
    "Form"  [::&parser/Form  (->clojure+ ->clojure (.-value x))]))

;; [Resources]
(defn expand [loader macro-class tokens]
  (let [output (-> (.loadClass loader macro-class)
                   .getDeclaredConstructors
                   first
                   (.newInstance (to-array [(int 0) nil]))
                   ((fn [macro] (prn 'macro macro "#1") macro))
                   (.apply (->lux+ ->lux loader tokens))
                   ;; (.impl (->lux+ ->lux loader tokens) nil)
                   ((fn [macro] (prn 'macro macro "#2") macro))
                   (.apply nil)
                   ((fn [macro] (prn 'macro macro "#3") macro))
                   ;; (.apply nil)
                   ;; ((fn [macro] (prn 'macro macro "#4?") macro))
                   )
        _ (prn 'expand/output macro-class output (->> output .-_0 (->clojure+ ->clojure)))]
    [(->> output .-_0 (->clojure+ ->clojure))
     (.-_1 output)]))
