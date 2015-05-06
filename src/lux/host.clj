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
    (if (= "void" base)
      (return &type/$Void)
      (return (&/V "lux;DataT" (str (reduce str "" (repeat (int (/ (count arr-level) 2)) "["))
                                    base)))
      )))

(defn ^:private method->type [^Method method]
  (|do [=return (class->type (.getReturnType method))]
    (return =return)))

;; [Resources]
(defn full-class [class-name]
  (case class-name
    "boolean" (return Boolean/TYPE)
    "byte"    (return Byte/TYPE)
    "short"   (return Short/TYPE)
    "int"     (return Integer/TYPE)
    "long"    (return Long/TYPE)
    "float"   (return Float/TYPE)
    "double"  (return Double/TYPE)
    "char"    (return Character/TYPE)
    ;; else
    (try (return (Class/forName class-name))
      (catch Exception e
        (fail (str "[Analyser Error] Unknown class: " class-name))))))

(defn full-class-name [class-name]
  (|do [^Class =class (full-class class-name)]
    (return (.getName =class))))

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
    [["lux;Meta" [_ ["lux;Symbol" [_ ?ident]]]]]
    (full-class-name ?ident)

    [_]
    (fail (str "[Host] Unknown JVM param: " (pr-str token)))))

(do-template [<name> <static?>]
  (defn <name> [target field]
    (if-let [type* (first (for [^Field =field (.getDeclaredFields (Class/forName target))
                                :when (and (= field (.getName =field))
                                           (= <static?> (Modifier/isStatic (.getModifiers =field))))]
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
                                 :when (and (= method-name (.getName =method))
                                            (= <static?> (Modifier/isStatic (.getModifiers =method)))
                                            (&/fold2 #(and %1 (= %2 %3))
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
