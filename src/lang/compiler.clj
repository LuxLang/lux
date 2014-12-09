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
  `(defn ~name [~'*state*]
     (let [~'*name* (:name ~'*state*)
           ~'*writer* (:writer ~'*state*)
           ~'*form* (:form ~'*state*)]
       (match ~'*form*
         ~match
         (do ~body
           true)
         _#
         false))))

(defn compile-form* [writer form]
  (compile-form {:writer writer, :form form}))

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
        (compile-form (assoc *state* :form arg)))
    (doto *writer*
      (.visitMethodInsn Opcodes/INVOKESTATIC *name* ?fn "(Ljava/lang/Object;)Ljava/lang/Object;"))))

(defcompiler ^:private compile-static-access
  [::&parser/static-access ?class ?member]
  (doto *writer*
    (.visitFieldInsn Opcodes/GETSTATIC (->class ?class) ?member (->type-signature "java.io.PrintStream"))))

(defcompiler ^:private compile-dynamic-access
  [::&parser/dynamic-access ?object ?access]
  (let [=object (compile-form (assoc *state* :form ?object))
        method (match ?access
                 [::&parser/fn-call [::&parser/ident ?method] ?args]
                 (do (doseq [arg ?args]
                       (compile-form (assoc *state* :form arg)))
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
    (compile-form (assoc *state* :form ?test))
    (.visitJumpInsn *writer* Opcodes/IFEQ else-label)
    (compile-form (assoc *state* :form ?then))
    (doto *writer*
      (.visitJumpInsn Opcodes/GOTO end-label)
      (.visitLabel else-label))
    (compile-form (assoc *state* :form ?else))
    (.visitLabel *writer* end-label)))

(defcompiler ^:private compile-def
  [::&parser/def ?form ?body]
  (match ?form
    [::&parser/fn-call [::&parser/ident ?name] ?args]
    (if (= "main" ?name)
      (let [=method (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name "([Ljava/lang/String;)V" nil nil)
                      (.visitCode))]
        (compile-form (assoc *state* :writer =method :form ?body))
        (doto =method
          (.visitInsn Opcodes/RETURN)
          (.visitMaxs 0 0)
          (.visitEnd)))
      (let [=method (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name "(Ljava/lang/Object;)Ljava/lang/Object;" nil nil)
                      (.visitCode))]
        (compile-form (assoc *state* :writer =method :form ?body))
        (doto =method
          (.visitInsn Opcodes/ARETURN)
          (.visitMaxs 0 0)
          (.visitEnd))))
    ))

(defcompiler ^:private compile-module
  [::&parser/module]
  (.visit *writer* Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
          (->class *name*) nil "java/lang/Object" nil))

(defcompiler ^:private compile-defclass
  [::&parser/defclass ?name ?fields]
  (do (prn 'compile-defclass ?name ?fields)
    (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                   (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                           (->class (str *name* "." ?name)) nil "java/lang/Object" nil))]
      (doseq [[class field] ?fields]
        (doto (.visitField =class Opcodes/ACC_PUBLIC field (->type-signature class) nil nil)
          (.visitEnd)))
      (.visitEnd =class)
      (let [parent-dir (->class *name*)]
        (.mkdirs (java.io.File. parent-dir))
        (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. (str parent-dir "/" ?name ".class")))]
          (.write stream (.toByteArray =class)))))))

(let [+compilers+ [compile-boolean
                   compile-string
                   compile-ident
                   compile-fn-call
                   compile-static-access
                   compile-dynamic-access
                   compile-ann-class
                   compile-if
                   compile-def
                   compile-module
                   compile-defclass]]
  (defn ^:private compile-form [state]
    (prn 'compile-form/state state)
    (some #(% state) +compilers+)))

;; [Interface]
(defn compile [class-name inputs]
  (prn 'inputs inputs)
  (let [=class (new ClassWriter ClassWriter/COMPUTE_MAXS)
        ;; (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
        ;;          (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
        ;;                  "output" nil "java/lang/Object" nil))
        state {:name class-name
               :writer =class
               :form nil}]
    ;; (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
    ;;   (.visitCode)
    ;;   (.visitVarInsn Opcodes/ALOAD 0)
    ;;   (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
    ;;   (.visitInsn Opcodes/RETURN)
    ;;   (.visitMaxs 0 0)
    ;;   (.visitEnd))
    (doall (map #(compile-form (assoc state :form %)) inputs))
    (.visitEnd =class)
    (.toByteArray =class)))
