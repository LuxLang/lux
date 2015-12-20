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
            [lux.type.host :as &host-type]
            [lux.host.generics :as &host-generics])
  (:import (java.lang.reflect Field Method Constructor Modifier Type
                              GenericArrayType ParameterizedType TypeVariable)
           (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Constants]
(def function-class "lux.Function")
(def module-separator "/")
(def class-name-separator ".")
(def class-separator "/")
(def bytecode-version Opcodes/V1_6)

;; [Resources]
(defn ^String ->module-class [old]
  old)

(def ->package ->module-class)

(defn unfold-array [type]
  "(-> Type (, Int Type))"
  (|case type
    (&/$DataT "#Array" (&/$Cons param (&/$Nil)))
    (|let [[count inner] (unfold-array param)]
      (&/T (inc count) inner))

    _
    (&/T 0 type)))

(let [ex-type-class (str "L" (&host-generics/->bytecode-class-name "java.lang.Object") ";")
      object-array (str "[" "L" (&host-generics/->bytecode-class-name "java.lang.Object") ";")]
  (defn ->java-sig [^objects type]
    "(-> Type (Lux Text))"
    (|case type
      (&/$DataT ?name params)
      (cond (= &host-type/array-data-tag ?name) (|do [:let [[level base] (unfold-array type)]
                                                      base-sig (|case base
                                                                 (&/$DataT base-class _)
                                                                 (return (&host-generics/->type-signature base-class))

                                                                 _
                                                                 (->java-sig base))]
                                                  (return (str (->> (&/|repeat level "[") (&/fold str ""))
                                                               base-sig)))
            (= &host-type/null-data-tag ?name)  (return (&host-generics/->type-signature "java.lang.Object"))
            :else                               (return (&host-generics/->type-signature ?name)))

      (&/$LambdaT _ _)
      (return (&host-generics/->type-signature function-class))

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

(defn abstract-methods [class-loader super-class]
  "(-> ClassLoader SuperClassDecl (Lux (List (, Text (List Text)))))"
  (|let [[super-name super-params] super-class]
    (return (&/->list (for [^Method =method (.getDeclaredMethods (Class/forName (&host-type/as-obj super-name) true class-loader))
                            :when (Modifier/isAbstract (.getModifiers =method))]
                        (&/T (.getName =method) (&/|map #(.getName ^Class %) (&/->list (seq (.getParameterTypes =method))))))))))

(defn location [scope]
  (->> scope (&/|map &/normalize-name) (&/|interpose "$") (&/fold str "")))

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

(defn ^:private dummy-return [^MethodVisitor writer output]
  (case output
    "void" (.visitInsn writer Opcodes/RETURN)
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

(def init-method-name "<init>")

(defn ^:private dummy-ctor [^MethodVisitor writer super-class ctor-args]
  (|let [ctor-arg-types (->> ctor-args (&/|map (comp &host-generics/->type-signature &/|first)) (&/fold str ""))]
    (doto writer
      (.visitVarInsn Opcodes/ALOAD 0)
      (-> (doto (dummy-value arg-type)
            (-> (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name arg-type))
                (->> (when (not (primitive-jvm-type? arg-type))))))
          (->> (doseq [ctor-arg (&/->seq ctor-args)
                       :let [;; arg-term (&/|first ctor-arg)
                             arg-type (&/|first ctor-arg)]])))
      (.visitMethodInsn Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name (&host-generics/super-class-name super-class)) init-method-name (str "(" ctor-arg-types ")V"))
      (.visitInsn Opcodes/RETURN))))

(defn ^:private compile-dummy-method [^ClassWriter =class super-class method-def]
  (|case method-def
    (&/$ConstructorMethodSyntax =anns =gvars =exceptions =inputs =ctor-args body)
    (|let [=output (&/V &/$GenericClass (&/T "void" (&/|list)))
           method-decl [init-method-name =anns =gvars =exceptions (&/|map &/|second =inputs) =output]
           [simple-signature generic-signature] (&host-generics/method-signatures method-decl)]
      (doto (.visitMethod =class Opcodes/ACC_PUBLIC
                          init-method-name
                          simple-signature
                          generic-signature
                          (->> =exceptions (&/|map &host-generics/gclass->bytecode-class-name) &/->seq (into-array java.lang.String)))
        .visitCode
        (dummy-ctor super-class =ctor-args)
        (.visitMaxs 0 0)
        (.visitEnd)))

    (&/$VirtualMethodSyntax =name =anns =gvars =exceptions =inputs =output body)
    (|let [method-decl [=name =anns =gvars =exceptions (&/|map &/|second =inputs) =output]
           [simple-signature generic-signature] (&host-generics/method-signatures method-decl)]
      (doto (.visitMethod =class Opcodes/ACC_PUBLIC
                          =name
                          simple-signature
                          generic-signature
                          (->> =exceptions (&/|map &host-generics/gclass->bytecode-class-name) &/->seq (into-array java.lang.String)))
        .visitCode
        (dummy-return =output)
        (.visitMaxs 0 0)
        (.visitEnd)))
    
    (&/$OverridenMethodSyntax =class-decl =name =anns =gvars =exceptions =inputs =output body)
    (|let [method-decl [=name =anns =gvars =exceptions (&/|map &/|second =inputs) =output]
           [simple-signature generic-signature] (&host-generics/method-signatures method-decl)]
      (doto (.visitMethod =class Opcodes/ACC_PUBLIC
                          =name
                          simple-signature
                          generic-signature
                          (->> =exceptions (&/|map &host-generics/gclass->bytecode-class-name) &/->seq (into-array java.lang.String)))
        .visitCode
        (dummy-return =output)
        (.visitMaxs 0 0)
        (.visitEnd)))

    _
    (assert false (println-str 'compile-dummy-method (&/adt->text method-def)))
    ))

(defn use-dummy-class [class-decl super-class interfaces ctor-args fields methods]
  (|do [module &/get-module-name
        :let [[?name ?params] class-decl
              full-name (str module "/" ?name)
              class-signature (&host-generics/gclass-decl->signature class-decl interfaces)
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                               full-name
                               class-signature
                               (&host-generics/->bytecode-class-name (&host-generics/super-class-name super-class))
                               (->> interfaces (&/|map (comp &host-generics/->bytecode-class-name &host-generics/super-class-name)) &/->seq (into-array String))))
              _ (&/|map (fn [field]
                          (|let [[=name =anns =type] field]
                            (do (prn 'use-dummy-class/=name =name (&host-generics/->type-signature =type) (&/adt->text =type))
                              (doto (.visitField =class Opcodes/ACC_PUBLIC =name
                                                 (&host-generics/->type-signature =type) nil nil)
                                (.visitEnd)))))
                        fields)
              _ (&/|map (partial compile-dummy-method =class super-class) methods)
              bytecode (.toByteArray (doto =class .visitEnd))]
        ^ClassLoader loader &/loader
        !classes &/classes
        :let [real-name (str (&host-generics/->class-name module) "." ?name)
              _ (swap! !classes assoc real-name bytecode)
              ;; _ (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. (str "target/jvm/" full-name ".class")))]
              ;;     (.write stream bytecode))
              _ (.loadClass loader real-name)]]
    (return nil)))
