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
                 [type :as &type])
            [lux.type.host :as &host-type])
  (:import (java.lang.reflect Field Method Constructor Modifier Type)
           java.util.regex.Pattern
           (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Constants]
(def prefix "lux.")
(def function-class (str prefix "Function"))
(def module-separator "/")
(def class-name-separator ".")
(def class-separator "/")

;; [Resources]
(do-template [<name> <old-sep> <new-sep>]
  (let [regex (-> <old-sep> Pattern/quote re-pattern)]
    (defn <name> [old]
      (string/replace old regex <new-sep>)))

  ^String ->class        class-name-separator class-separator
  ^String ->class-name   module-separator     class-name-separator
  ^String ->module-class module-separator     class-separator
  )

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
    (cond (= &host-type/array-data-tag ?name) (|let [[level base] (unfold-array type)
                                                     base-sig (|case base
                                                                (&/$DataT base-class _)
                                                                (->class base-class)

                                                                _
                                                                (->java-sig base))]
                                                (str (->> (&/|repeat level "[") (&/fold str ""))
                                                     "L" base-sig ";"))
          (= &host-type/null-data-tag ?name)  (->type-signature "java.lang.Object")
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
    (|let [target-class (Class/forName (&host-type/as-obj target) true class-loader)]
      (if-let [^Type gtype (first (for [^Field =field (.getDeclaredFields target-class)
                                        :when (and (.equals ^Object field (.getName =field))
                                                   (.equals ^Object <static?> (Modifier/isStatic (.getModifiers =field))))]
                                    (.getGenericType =field)))]
        (|let [gvars (->> target-class .getTypeParameters seq &/->list)]
          (return (&/T gvars gtype)))
        (fail (str "[Host Error] Field does not exist: " target "." field)))))

  lookup-static-field true
  lookup-field        false
  )

(do-template [<name> <static?>]
  (defn <name> [class-loader target method-name args]
    (|let [target-class (Class/forName (&host-type/as-obj target) true class-loader)]
      (if-let [^Method method (first (for [^Method =method (.getDeclaredMethods (Class/forName (&host-type/as-obj target) true class-loader))
                                           :when (and (.equals ^Object method-name (.getName =method))
                                                      (.equals ^Object <static?> (Modifier/isStatic (.getModifiers =method)))
                                                      (let [param-types (&/->list (seq (.getParameterTypes =method)))]
                                                        (and (= (&/|length args) (&/|length param-types))
                                                             (&/fold2 #(and %1 (.equals ^Object %2 %3))
                                                                      true
                                                                      args
                                                                      (&/|map #(.getName ^Class %) param-types)))))]
                                       =method))]
        (|let [parent-gvars (->> target-class .getTypeParameters seq &/->list)
               gvars (->> method .getTypeParameters seq &/->list)
               gargs (->> method .getGenericParameterTypes seq &/->list)]
          (return (&/T (.getGenericReturnType method)
                       (->> method .getExceptionTypes &/->list (&/|map #(.getName ^Class %)))
                       parent-gvars
                       gvars
                       gargs)))
        (fail (str "[Host Error] Method does not exist: " target "." method-name)))))

  lookup-static-method  true
  lookup-virtual-method false
  )

(defn lookup-constructor [class-loader target args]
  ;; (prn 'lookup-constructor class-loader target (&host-type/as-obj target))
  (let [target-class (Class/forName (&host-type/as-obj target) true class-loader)]
    (if-let [^Constructor ctor (first (for [^Constructor =method (.getDeclaredConstructors target-class)
                                            :when (let [param-types (&/->list (seq (.getParameterTypes =method)))]
                                                    (and (= (&/|length args) (&/|length param-types))
                                                         (&/fold2 #(and %1 (.equals ^Object %2 %3))
                                                                  true
                                                                  args
                                                                  (&/|map #(.getName ^Class %) param-types))))]
                                        =method))]
      (|let [gvars (->> target-class .getTypeParameters seq &/->list)
             gargs (->> ctor .getGenericParameterTypes seq &/->list)
             exs (->> ctor .getExceptionTypes &/->list (&/|map #(.getName ^Class %)))]
        (return (&/T exs gvars gargs)))
      (fail (str "[Host Error] Constructor does not exist: " target)))))

(defn abstract-methods [class-loader class]
  (return (&/->list (for [^Method =method (.getDeclaredMethods (Class/forName (&host-type/as-obj class) true class-loader))
                          :when (Modifier/isAbstract (.getModifiers =method))]
                      (&/T (.getName =method) (&/|map #(.getName ^Class %) (&/->list (seq (.getParameterTypes =method)))))))))

(defn location [scope]
  (->> scope (&/|map &/normalize-name) (&/|interpose "$") (&/fold str "")))

(defn modifiers->int [mods]
  (+ (case (:visibility mods)
       "default" 0
       "public" Opcodes/ACC_PUBLIC
       "private" Opcodes/ACC_PRIVATE
       "protected" Opcodes/ACC_PROTECTED)
     (if (:static? mods) Opcodes/ACC_STATIC 0)
     (if (:final? mods) Opcodes/ACC_FINAL 0)
     (if (:abstract? mods) Opcodes/ACC_ABSTRACT 0)
     (case (:concurrency mods)
       "synchronized" Opcodes/ACC_SYNCHRONIZED
       "volatile" Opcodes/ACC_VOLATILE
       ;; else
       0)))

(defn ^:private dummy-return [^MethodVisitor writer super-class name output]
  (case output
    "void" (if (= "<init>" name)
             (doto writer
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitMethodInsn Opcodes/INVOKESPECIAL (->class super-class) "<init>" "()V")
               (.visitInsn Opcodes/RETURN))
             (.visitInsn writer Opcodes/RETURN))
    "boolean" (doto writer
                (.visitLdcInsn false)
                (.visitInsn Opcodes/IRETURN))
    "byte" (doto writer
             (.visitLdcInsn (byte 0))
             (.visitInsn Opcodes/IRETURN))
    "short" (doto writer
              (.visitLdcInsn (short 0))
              (.visitInsn Opcodes/IRETURN))
    "int" (doto writer
            (.visitLdcInsn (int 0))
            (.visitInsn Opcodes/IRETURN))
    "long" (doto writer
             (.visitLdcInsn (long 0))
             (.visitInsn Opcodes/LRETURN))
    "float" (doto writer
              (.visitLdcInsn (float 0.0))
              (.visitInsn Opcodes/FRETURN))
    "double" (doto writer
               (.visitLdcInsn (double 0.0))
               (.visitInsn Opcodes/DRETURN))
    "char" (doto writer
             (.visitLdcInsn (char 0))
             (.visitInsn Opcodes/IRETURN))
    ;; else
    (doto writer
      (.visitInsn Opcodes/ACONST_NULL)
      (.visitInsn Opcodes/ARETURN))))

(defn use-dummy-class [name super-class interfaces fields methods]
  (|do [module &/get-module-name
        :let [full-name (str module "/" name)
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                               full-name nil (->class super-class) (->> interfaces (&/|map ->class) &/->seq (into-array String))))
              _ (&/|map (fn [field]
                          (doto (.visitField =class (modifiers->int (:modifiers field)) (:name field)
                                             (->type-signature (:type field)) nil nil)
                            (.visitEnd)))
                        fields)
              _ (&/|map (fn [method]
                          (|let [signature (str "(" (&/fold str "" (&/|map ->type-signature (:inputs method))) ")"
                                                (->type-signature (:output method)))]
                            (doto (.visitMethod =class (modifiers->int (:modifiers method))
                                                (:name method)
                                                signature
                                                nil
                                                (->> (:exceptions method) (&/|map ->class) &/->seq (into-array java.lang.String)))
                              .visitCode
                              (dummy-return super-class (:name method) (:output method))
                              (.visitMaxs 0 0)
                              (.visitEnd))))
                        methods)
              bytecode (.toByteArray (doto =class .visitEnd))]
        ^ClassLoader loader &/loader
        !classes &/classes
        :let [real-name (str (->class-name module) "." name)
              _ (swap! !classes assoc real-name bytecode)
              _ (.loadClass loader real-name)]]
    (return nil)))
