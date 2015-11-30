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
(def bytecode-version Opcodes/V1_6)

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

(let [ex-type-class (str "L" (->class "java.lang.Object") ";")
      object-array (str "[" "L" (->class "java.lang.Object") ";")]
  (defn ->java-sig [^objects type]
    "(-> Type (Lux Text))"
    (|case type
      (&/$DataT ?name params)
      (cond (= &host-type/array-data-tag ?name) (|do [:let [[level base] (unfold-array type)]
                                                      base-sig (|case base
                                                                 (&/$DataT base-class _)
                                                                 (return (->type-signature base-class))

                                                                 _
                                                                 (->java-sig base))]
                                                  (return (str (->> (&/|repeat level "[") (&/fold str ""))
                                                               base-sig)))
            (= &host-type/null-data-tag ?name)  (return (->type-signature "java.lang.Object"))
            :else                               (return (->type-signature ?name)))

      (&/$LambdaT _ _)
      (return (->type-signature function-class))

      (&/$TupleT (&/$Nil))
      (return "V")

      (&/$VariantT _)
      (return object-array)

      (&/$TupleT _)
      (return object-array)

      (&/$NamedT ?name ?type)
      (->java-sig ?type)

      (&/$AppT ?F ?A)
      (|do [type* (&type/apply-type ?F ?A)]
        (->java-sig type*))

      (&/$ExT _)
      (return ex-type-class)

      _
      (assert false (str '->java-sig " " (&type/show-type type)))
      )))

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

(do-template [<name> <static?> <method-type>]
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
        (fail (str "[Host Error] " <method-type> " method does not exist: " target "." method-name)))))

  lookup-static-method  true  "Static"
  lookup-virtual-method false "Virtual"
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

(defn primitive-jvm-type? [type]
  (case type
    ("boolean" "byte" "short" "int" "long" "float" "double" "char")
    true
    ;; else
    false))

(defn dummy-value [^MethodVisitor writer class]
  (case class
    "boolean" (doto writer
                (.visitLdcInsn false))
    "byte" (doto writer
             (.visitLdcInsn (byte 0)))
    "short" (doto writer
              (.visitLdcInsn (short 0)))
    "int" (doto writer
            (.visitLdcInsn (int 0)))
    "long" (doto writer
             (.visitLdcInsn (long 0)))
    "float" (doto writer
              (.visitLdcInsn (float 0.0)))
    "double" (doto writer
               (.visitLdcInsn (double 0.0)))
    "char" (doto writer
             (.visitLdcInsn (char 0)))
    ;; else
    (doto writer
      (.visitInsn Opcodes/ACONST_NULL))))

(defn ^:private dummy-return [^MethodVisitor writer super-class ??ctor-args name output]
  (case output
    "void" (if (= "<init>" name)
             (|let [(&/$Some ctor-args) ??ctor-args
                    ctor-arg-types (->> ctor-args (&/|map (comp ->type-signature &/|first)) (&/fold str ""))]
               (doto writer
                 (.visitVarInsn Opcodes/ALOAD 0)
                 (-> (doto (dummy-value arg-type)
                       (-> (.visitTypeInsn Opcodes/CHECKCAST (->class arg-type))
                           (->> (when (not (primitive-jvm-type? arg-type))))))
                     (->> (doseq [ctor-arg (&/->seq ctor-args)
                                  :let [;; arg-term (&/|first ctor-arg)
                                        arg-type (&/|first ctor-arg)]])))
                 (.visitMethodInsn Opcodes/INVOKESPECIAL (->class super-class) "<init>" (str "(" ctor-arg-types ")V"))
                 (.visitInsn Opcodes/RETURN)))
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

(defn use-dummy-class [name super-class interfaces ctor-args fields methods]
  (|do [module &/get-module-name
        :let [full-name (str module "/" name)
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
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
                              (dummy-return super-class ctor-args (:name method) (:output method))
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
