;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.host
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type]))
  (:import (java.lang.reflect Field Method Modifier)))

;; [Constants]
(def prefix "lux.")
(def function-class (str prefix "Function"))
(def module-separator "_")

;; [Utils]
(defn ^:private class->type [^Class class]
  (if-let [[_ base arr-level] (re-find #"^([^\[]+)(\[\])*$"
                                       (str (if-let [pkg (.getPackage class)]
                                              (str (.getName pkg) ".")
                                              "")
                                            (.getSimpleName class)))]
    (if (.equals "void" base)
      (return &type/Unit)
      (return (&/V "lux;DataT" (str (reduce str "" (repeat (int (/ (count arr-level) 2)) "["))
                                    base)))
      )))

(defn ^:private method->type [^Method method]
  (class->type (.getReturnType method)))

;; [Resources]
(defn ^String ->class [class]
  (string/replace class #"\." "/"))

(defn ^String ->class-name [module]
  (string/replace module #"/" "."))

(defn ^String ->module-class [module-name]
  (string/replace module-name #"/" module-separator))

(def ->package ->module-class)

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
  (|case type
    ("lux;DataT" ?name)
    (->type-signature ?name)

    ("lux;LambdaT" _ _)
    (->type-signature function-class)

    ("lux;TupleT" ("lux;Nil"))
    "V"
    ))

(do-template [<name> <static?>]
  (defn <name> [class-loader target field]
    (if-let [type* (first (for [^Field =field (.getDeclaredFields (Class/forName (&type/as-obj target) true class-loader))
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
  (defn <name> [class-loader target method-name args]
    ;; (prn '<name> target method-name)
    (if-let [method (first (for [^Method =method (.getDeclaredMethods (Class/forName (&type/as-obj target) true class-loader))
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
  (->> scope (&/|map &/normalize-name) (&/|interpose "$") (&/fold str "")))
