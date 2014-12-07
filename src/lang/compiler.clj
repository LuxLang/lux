(ns lang.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]
            [lang.parser :as &parser]
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

(declare compile-form)

;; [Utils/General]
(defmacro ^:private defcompiler [name match body]
  `(defn ~name [~'*writer* ~'*form*]
     (match ~'*form*
       ~match
       (do ~body
         true)
       _#
       false)))

(defn ^:private unwrap-ident [ident]
  (match ident
    [::&parser/ident ?label]
    ?label))

(defn ^:private unwrap-tagged [ident]
  (match ident
    [::&parser/tagged ?tag ?data]
    [?tag ?data]))

(defn ^:private ->class [class]
  (string/replace class #"\." "/"))

(defn ^:private ->type-signature [class]
  (case class
    "Void" "V"
    ;; else
    (str "L" (->class class) ";")))

;; [Utils/Compilers]
(defcompiler ^:private compile-boolean
  [::&parser/boolean ?boolean]
  (if ?boolean
    (.visitLdcInsn *writer* (int 1))
    (.visitLdcInsn *writer* (int 0))))

(defcompiler ^:private compile-string
  [::&parser/string ?string]
  (.visitLdcInsn *writer* ?string))

(defcompiler ^:private compile-ident
  [::&parser/ident ?name]
  (doto *writer*
    (.visitVarInsn Opcodes/ALOAD (int 0)))
  ;; nil
  )

(defcompiler ^:private compile-fn-call
  [::&parser/fn-call [::&parser/ident ?fn] ?args]
  (do (doseq [arg ?args]
        (compile-form *writer* arg))
    (doto *writer*
      (.visitMethodInsn Opcodes/INVOKESTATIC "output" ?fn "(Ljava/lang/Object;)Ljava/lang/Object;"))))

(defcompiler ^:private compile-static-access
  [::&parser/static-access ?class ?member]
  (doto *writer*
    (.visitFieldInsn Opcodes/GETSTATIC (->class ?class) ?member (->type-signature "java.io.PrintStream"))))

(defcompiler ^:private compile-dynamic-access
  [::&parser/dynamic-access ?object ?access]
  (let [=object (compile-form *writer* ?object)
        method (match ?access
                 [::&parser/fn-call [::&parser/ident ?method] ?args]
                 (do (doseq [arg ?args]
                       (compile-form *writer* arg))
                   ?method))]
    (doto *writer*
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.io.PrintStream") method "(Ljava/lang/Object;)V"))))

(defcompiler ^:private compile-ann-class
  [::&parser/ann-class ?class ?members]
  nil)

(defcompiler ^:private compile-if
  [::&parser/if ?test ?then ?else]
  (let [else-label (new Label)
        end-label (new Label)]
    (compile-form *writer* ?test)
    (.visitJumpInsn *writer* Opcodes/IFEQ else-label)
    (compile-form *writer* ?then)
    (doto *writer*
      (.visitJumpInsn Opcodes/GOTO end-label)
      (.visitLabel else-label))
    (compile-form *writer* ?else)
    (.visitLabel *writer* end-label)))

(defcompiler ^:private compile-def
  [::&parser/def ?form ?body]
  (match ?form
    [::&parser/fn-call [::&parser/ident ?name] ?args]
    (if (= "main" ?name)
      (let [=method (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name "([Ljava/lang/String;)V" nil nil)
                      (.visitCode))]
        (compile-form =method ?body)
        (doto =method
          (.visitInsn Opcodes/RETURN)
          (.visitMaxs 0 0)
          (.visitEnd)))
      (let [=method (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name "(Ljava/lang/Object;)Ljava/lang/Object;" nil nil)
                      (.visitCode))]
        ;; (doto =method
        ;;   (.visitFieldInsn Opcodes/GETSTATIC (->class "java.lang.System") "out" (->type-signature "java.io.PrintStream"))
        ;;   (.visitLdcInsn "IN")
        ;;   (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.io.PrintStream") "println" "(Ljava/lang/String;)V"))
        (compile-form =method ?body)
        ;; (doto =method
        ;;   (.visitFieldInsn Opcodes/GETSTATIC (->class "java.lang.System") "out" (->type-signature "java.io.PrintStream"))
        ;;   (.visitLdcInsn "OUT")
        ;;   (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.io.PrintStream") "println" "(Ljava/lang/String;)V"))
        (doto =method
          (.visitInsn Opcodes/ARETURN)
          (.visitMaxs 0 0)
          (.visitEnd))))
    ))

(let [+compilers+ [compile-boolean
                   compile-string
                   compile-ident
                   compile-fn-call
                   compile-static-access
                   compile-dynamic-access
                   compile-ann-class
                   compile-if
                   compile-def]]
  (defn ^:private compile-form [writer form]
    (prn 'compile-form/form form)
    (some #(% writer form) +compilers+)))

;; [Interface]
(defn compile [inputs]
  (prn 'inputs inputs)
  (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                 (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                         "output" nil "java/lang/Object" nil))]
    (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))
    (doall (map (partial compile-form =class) inputs))
    (.visitEnd =class)
    (.toByteArray =class)))
