;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.host
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type]))
  (:import (java.lang.reflect Field Method Constructor Modifier)
           java.util.regex.Pattern))

;; [Constants]
(def prefix "lux.")
(def function-class (str prefix "Function"))
(def module-separator "/")
(def class-name-separator ".")
(def class-separator "/")
(def array-data-tag "#Array")
(def null-data-tag "#Null")

;; [Utils]
(def class-name-re #"((\[+)L([\.a-zA-Z0-9]+);|([\.a-zA-Z0-9]+))")

(comment
  (let [class (class (to-array []))]
    (str (if-let [pkg (.getPackage class)]
           (str (.getName pkg) ".")
           "")
         (.getSimpleName class)))

  (.getName String) "java.lang.String"

  (.getName (class (to-array []))) "[Ljava.lang.Object;"

  (re-find class-name-re "java.lang.String")
  ["java.lang.String" "java.lang.String" nil nil "java.lang.String"]

  (re-find class-name-re "[Ljava.lang.Object;")
  ["[Ljava.lang.Object;" "[Ljava.lang.Object;" "[" "java.lang.Object" nil]
  )

(defn ^:private class->type [^Class class]
  "(-> Class Type)"
  (do ;; (prn 'class->type/_0 class (.getSimpleName class) (.getName class))
    (if-let [[_ _ arr-brackets arr-base simple-base] (re-find class-name-re (.getName class))]
      (let [base (or arr-base simple-base)]
        ;; (prn 'class->type/_1 class base arr-brackets)
        (let [output-type (if (.equals "void" base)
                            &type/Unit
                            (reduce (fn [inner _] (&type/Data$ array-data-tag (&/|list inner)))
                                    (&type/Data$ base &/Nil$)
                                    (range (count (or arr-brackets ""))))
                            )]
          ;; (prn 'class->type/_2 class (&type/show-type output-type))
          output-type)
        ))))

(defn ^:private method->type [^Method method]
  "(-> Method Type)"
  (class->type (.getReturnType method)))

;; [Resources]
(defn ^String ->class [class]
  (string/replace class (-> class-name-separator Pattern/quote re-pattern) class-separator))

(defn ^String ->class-name [module]
  (string/replace module (-> module-separator Pattern/quote re-pattern) class-name-separator))

(defn ^String ->module-class [module-name]
  (string/replace module-name (-> module-separator Pattern/quote re-pattern) class-separator))

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

(defn unfold-array [type]
  "(-> Type (, Int Type))"
  (|case type
    (&/$DataT "#Array" (&/$Cons param (&/$Nil)))
    (|let [[count inner] (unfold-array param)]
      (&/T (inc count) inner))

    _
    (&/T 0 type)))

(defn ->java-sig [^objects type]
  "(-> Type Text)"
  (|case type
    (&/$DataT ?name params)
    (cond (= array-data-tag ?name) (|let [[level base] (unfold-array type)
                                          base-sig (|case base
                                                     (&/$DataT base-class _)
                                                     (->class base-class)

                                                     _
                                                     (->java-sig base))]
                                     (str (->> (&/|repeat level "[") (&/fold str ""))
                                          "L" base-sig ";"))
          (= null-data-tag ?name)  (->type-signature "java.lang.Object")
          :else                    (->type-signature ?name))

    (&/$LambdaT _ _)
    (->type-signature function-class)

    (&/$TupleT (&/$Nil))
    "V"

    (&/$NamedT ?name ?type)
    (->java-sig ?type)

    _
    (assert false (str '->java-sig " " (&type/show-type type)))
    ))

(do-template [<name> <static?>]
  (defn <name> [class-loader target field]
    (if-let [type* (first (for [^Field =field (.getDeclaredFields (Class/forName (&type/as-obj target) true class-loader))
                                :when (and (.equals ^Object field (.getName =field))
                                           (.equals ^Object <static?> (Modifier/isStatic (.getModifiers =field))))]
                            (.getType =field)))]
      (return (class->type type*))
      (fail (str "[Host Error] Field does not exist: " target "." field))))

  lookup-static-field true
  lookup-field        false
  )

(do-template [<name> <static?>]
  (defn <name> [class-loader target method-name args]
    ;; (prn '<name> target method-name)
    (if-let [method (first (for [^Method =method (.getDeclaredMethods (Class/forName (&type/as-obj target) true class-loader))
                                 :when (and (.equals ^Object method-name (.getName =method))
                                            (.equals ^Object <static?> (Modifier/isStatic (.getModifiers =method)))
                                            (let [param-types (&/->list (seq (.getParameterTypes =method)))]
                                              (and (= (&/|length args) (&/|length param-types))
                                                   (&/fold2 #(and %1 (.equals ^Object %2 %3))
                                                            true
                                                            args
                                                            (&/|map #(.getName ^Class %) param-types)))))]
                             =method))]
      (return (&/T (method->type method) (->> method .getExceptionTypes &/->list (&/|map #(.getName %)))))
      (fail (str "[Host Error] Method does not exist: " target "." method-name))))

  lookup-static-method  true
  lookup-virtual-method false
  )

(defn lookup-constructor [class-loader target args]
  ;; (prn 'lookup-constructor class-loader target (&type/as-obj target))
  (if-let [ctor (first (for [^Constructor =method (.getDeclaredConstructors (Class/forName (&type/as-obj target) true class-loader))
                             :when (let [param-types (&/->list (seq (.getParameterTypes =method)))]
                                     (and (= (&/|length args) (&/|length param-types))
                                          (&/fold2 #(and %1 (.equals ^Object %2 %3))
                                                   true
                                                   args
                                                   (&/|map #(.getName ^Class %) param-types))))]
                         =method))]
    (return (&/T &type/Unit (->> ctor .getExceptionTypes &/->list (&/|map #(.getName %)))))
    (fail (str "[Host Error] Constructor does not exist: " target))))

(defn abstract-methods [class-loader class]
  (return (&/->list (for [^Method =method (.getDeclaredMethods (Class/forName (&type/as-obj class) true class-loader))
                          :when (.equals true (Modifier/isAbstract (.getModifiers =method)))]
                      (&/T (.getName =method) (&/|map #(.getName ^Class %) (&/->list (seq (.getParameterTypes =method)))))))))

(defn location [scope]
  (->> scope (&/|map &/normalize-name) (&/|interpose "$") (&/fold str "")))
