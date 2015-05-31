(ns lux.host
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let]]
                 [type :as &type]))
  (:import (java.lang.reflect Field Method Modifier)))

;; [Constants]
(def prefix "lux.")
(def function-class (str prefix "Function"))

;; [Utils]
(defn ^:private class->type [^Class class]
  (if-let [[_ base arr-level] (re-find #"^([^\[]+)(\[\])*$"
                                       (str (if-let [pkg (.getPackage class)]
                                              (str (.getName pkg) ".")
                                              "")
                                            (.getSimpleName class)))]
    (if (.equals "void" base)
      (return &type/$Void)
      (return (&/V "lux;DataT" (str (reduce str "" (repeat (int (/ (count arr-level) 2)) "["))
                                    base)))
      )))

(defn ^:private method->type [^Method method]
  (class->type (.getReturnType method)))

;; [Resources]
(defn ^String ->class [class]
  (string/replace class #"\." "/"))

(def ->package ->class)

(defn ->type-signature [class]
  ;; (assert (string? class))
  (case class
    "void"    "V"
    "boolean" "Z"
    "byte"    "B"
    "short"   "S"
    "int"     "I"
    "long"    "J"
    "float"   "F"
    "double"  "D"
    "char"    "C"
    ;; else
    (let [class* (->class class)]
      (if (.startsWith class* "[")
        class*
        (str "L" class* ";")))
    ))

(defn ->java-sig [^objects type]
  (matchv ::M/objects [type]
    [["lux;DataT" ?name]]
    (->type-signature ?name)

    [["lux;LambdaT" [_ _]]]
    (->type-signature function-class)

    [["lux;VariantT" ["lux;Nil" _]]]
    "V"
    
    [_]
    (assert false (prn-str '->java-sig (aget type 0)))))

(defn extract-jvm-param [token]
  (matchv ::M/objects [token]
    [["lux;Meta" [_ ["lux;SymbolS" [_ ?ident]]]]]
    (return ?ident)

    [_]
    (fail (str "[Host] Unknown JVM param: " (pr-str token)))))

(do-template [<name> <static?>]
  (defn <name> [target field]
    (if-let [type* (first (for [^Field =field (.getDeclaredFields (Class/forName target))
                                :when (and (.equals ^Object field (.getName =field))
                                           (.equals ^Object <static?> (Modifier/isStatic (.getModifiers =field))))]
                            (.getType =field)))]
      (|do [=type (class->type type*)]
        (return =type))
      (fail (str "[Analyser Error] Field does not exist: " target "." field))))

  lookup-static-field true
  lookup-field        false
  )

(do-template [<name> <static?>]
  (defn <name> [target method-name args]
    (if-let [method (first (for [^Method =method (.getDeclaredMethods (Class/forName target))
                                 :when (and (.equals ^Object method-name (.getName =method))
                                            (.equals ^Object <static?> (Modifier/isStatic (.getModifiers =method)))
                                            (&/fold2 #(and %1 (.equals ^Object %2 %3))
                                                     true
                                                     args
                                                     (&/|map #(.getName ^Class %) (&/->list (seq (.getParameterTypes =method))))))]
                             =method))]
      (method->type method)
      (fail (str "[Analyser Error] Method does not exist: " target "." method-name))))

  lookup-static-method  true
  lookup-virtual-method false
  )

(defn location [scope]
  (->> scope (&/|map &/normalize-ident) (&/|interpose "$") (&/fold str "")))
