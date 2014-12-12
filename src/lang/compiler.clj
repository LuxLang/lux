(ns lang.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]
            (lang [type :as &type]
                  [parser :as &parser]
                  [analyser :as &analyser])
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

(declare compile-form)

;; [Utils/General]
(def ^:private +variant-class+ "test2.Tagged")

(defmacro ^:private defcompiler [name match body]
  `(defn ~name [~'*state*]
     (let [~'*writer* (:writer ~'*state*)
           ~'*type* (:type (:form ~'*state*))]
       (match (:form (:form ~'*state*))
         ~match
         (do ~body
           true)
         _#
         false))))

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

(def ^:private ->package ->class)

(defn ^:private ->type-signature [class]
  (case class
    "Void" "V"
    ;; else
    (str "L" (->class class) ";")))

(defn ^:private ->java-sig [type]
  (match type
    [::&type/object ?name []]
    (->type-signature ?name)

    [::&type/variant ?tag ?value]
    (->type-signature +variant-class+)))

;; [Utils/Compilers]
(defcompiler ^:private compile-literal
  [::&analyser/literal ?literal]
  (cond (string? ?literal)
        (.visitLdcInsn *writer* ?literal)

        (instance? java.lang.Boolean ?literal)
        (if ?literal
          ;; (.visitLdcInsn *writer* (int 1))
          (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class "java.lang.Boolean") "TRUE" (->type-signature "java.lang.Boolean"))
          ;; (.visitLdcInsn *writer* (int 0))
          (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class "java.lang.Boolean") "FALSE" (->type-signature "java.lang.Boolean")))

        :else
        (assert false (str "[Unknown literal type] " ?literal " : " (class ?literal)))))

(defcompiler ^:private compile-local
  [::&analyser/local ?idx]
  (do (prn 'LOCAL ?idx)
    (doto *writer*
      (.visitVarInsn Opcodes/ALOAD (int ?idx)))))

(defcompiler ^:private compile-global
  [::&analyser/global ?owner-class ?name]
  (do (prn 'GLOBAL ?owner-class ?name *type*)
    (doto *writer*
      (.visitFieldInsn Opcodes/GETSTATIC (->class ?owner-class) ?name (->java-sig *type*)))))

(defcompiler ^:private compile-call
  [::&analyser/call ?fn ?args]
  (do (prn 'compile-call ?fn)
    (doseq [arg ?args]
      (compile-form (assoc *state* :form arg)))
    (match (:form ?fn)
      [::&analyser/global ?owner-class ?fn-name]
      (doto *writer*
        (.visitMethodInsn Opcodes/INVOKESTATIC (->class ?owner-class) ?fn-name "(Ljava/lang/Object;)Ljava/lang/Object;")))))

(defcompiler ^:private compile-static-access
  [::&analyser/static-access ?class ?member]
  (doto *writer*
    (.visitFieldInsn Opcodes/GETSTATIC (->class ?class) ?member (->type-signature "java.io.PrintStream"))))

(defcompiler ^:private compile-dynamic-access
  [::&analyser/dynamic-access ?object [?method ?args]]
  (do (compile-form (assoc *state* :form ?object))
    (doseq [arg ?args]
      (compile-form (assoc *state* :form arg)))
    (doto *writer*
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.io.PrintStream") ?method "(Ljava/lang/Object;)V"))))

(defcompiler ^:private compile-ann-class
  [::&analyser/ann-class ?class ?members]
  nil)

(defcompiler ^:private compile-if
  [::&analyser/if ?test ?then ?else]
  (let [else-label (new Label)
        end-label (new Label)]
    (println "PRE")
    (assert (compile-form (assoc *state* :form ?test)) "CAN't COMPILE TEST")
    (doto *writer*
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.lang.Boolean") "booleanValue" "()Z")
      (.visitJumpInsn Opcodes/IFEQ else-label))
    (assert (compile-form (assoc *state* :form ?then)) "CAN't COMPILE THEN")
    (doto *writer*
      (.visitJumpInsn Opcodes/GOTO end-label)
      (.visitLabel else-label))
    (assert (compile-form (assoc *state* :form ?else)) "CAN't COMPILE ELSE")
    (.visitLabel *writer* end-label)))

(defcompiler ^:private compile-def
  [::&analyser/def ?form ?body]
  (match ?form
    (?name :guard string?)
    (let [=type (:type ?body)
          _ (prn '?body ?body)]
      (doto (.visitField *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name (->java-sig =type) nil nil)
        (.visitEnd)))

    [?name ?args]
    (if (= "main" ?name)
      (let [=method (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name "([Ljava/lang/String;)V" nil nil)
                      (.visitCode))]
        (prn 'FN/?body ?body)
        (assert (compile-form (assoc *state* :writer =method :form ?body)) (str "Body couldn't compile: " (pr-str ?body)))
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
  [::&analyser/module ?name]
  (.visit *writer* Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
          (->class ?name) nil "java/lang/Object" nil))

(defcompiler ^:private compile-defclass
  [::&analyser/defclass [?package ?name] ?members]
  (let [parent-dir (->package ?package)
        =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                 (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                         (str parent-dir "/" ?name) nil "java/lang/Object" nil))]
    (doseq [[field props] (:fields ?members)]
      (doto (.visitField =class Opcodes/ACC_PUBLIC field (->type-signature (:type props)) nil nil)
        (.visitEnd)))
    (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))
    (.visitEnd =class)
    (.mkdirs (java.io.File. parent-dir))
    (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. (str parent-dir "/" ?name ".class")))]
      (.write stream (.toByteArray =class)))))

(defcompiler ^:private compile-definterface
  [::&analyser/definterface [?package ?name] ?members]
  (let [parent-dir (->package ?package)
        =interface (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                     (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE)
                             (str parent-dir "/" ?name) nil "java/lang/Object" nil))]
    (doseq [[?method ?props] (:methods ?members)
            :let [[?args ?return] (:type ?props)
                  signature (str "(" (reduce str "" (map ->type-signature ?args)) ")" (->type-signature ?return))]]
      (.visitMethod =interface (+ Opcodes/ACC_PUBLIC) ?method signature nil nil))
    (.visitEnd =interface)
    (.mkdirs (java.io.File. parent-dir))
    (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. (str parent-dir "/" ?name ".class")))]
      (.write stream (.toByteArray =interface)))))

(defcompiler ^:private compile-variant
  [::&analyser/variant ?tag ?value]
  (let [variant-class* (->class +variant-class+)]
    (prn 'compile-variant ?tag ?value)
    (doto *writer*
      (.visitTypeInsn Opcodes/NEW variant-class*)
      (.visitInsn Opcodes/DUP)
      (.visitMethodInsn Opcodes/INVOKESPECIAL variant-class* "<init>" "()V")
      (.visitInsn Opcodes/DUP)
      (.visitLdcInsn ?tag)
      (.visitFieldInsn Opcodes/PUTFIELD variant-class* "tag" "Ljava/lang/String;")
      (.visitInsn Opcodes/DUP))
    (assert (compile-form (assoc *state* :form ?value)) (pr-str "Can't compile value: " ?value))
    (doto *writer*
      (.visitFieldInsn Opcodes/PUTFIELD variant-class* "value" "Ljava/lang/Object;"))
    ))

(let [+compilers+ [compile-literal
                   compile-local
                   compile-global
                   compile-call
                   compile-static-access
                   compile-dynamic-access
                   compile-ann-class
                   compile-if
                   compile-def
                   compile-module
                   compile-defclass
                   compile-definterface
                   compile-variant]]
  (defn ^:private compile-form [state]
    (prn 'compile-form/state state)
    (or (some #(% state) +compilers+)
        (assert false (str "Can't compile: " (pr-str (:form state)))))))

;; [Interface]
(defn compile [class-name inputs]
  (prn 'inputs inputs)
  (let [=class (new ClassWriter ClassWriter/COMPUTE_MAXS)
        ;; (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
        ;;          (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
        ;;                  "output" nil "java/lang/Object" nil))
        state {:writer =class
               :form nil}]
    ;; (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
    ;;   (.visitCode)
    ;;   (.visitVarInsn Opcodes/ALOAD 0)
    ;;   (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
    ;;   (.visitInsn Opcodes/RETURN)
    ;;   (.visitMaxs 0 0)
    ;;   (.visitEnd))
    (doseq [input inputs]
      (when (not (compile-form (assoc state :form input)))
        (assert false input)))
    ;; (doall (map #(compile-form (assoc state :form %)) inputs))
    (prn 'inputs inputs)
    (when-let [constants (seq (for [input inputs
                                    :let [payload (match (:form input)
                                                    [::&analyser/def (?name :guard string?) ?body]
                                                    [?name ?body]
                                                    _
                                                    nil)]
                                    :when payload]
                                payload))]
      (let [=init (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
                    (.visitCode))
            state* (assoc state :writer =init)
            class-name* (->class class-name)]
        (doseq [[?name ?body] constants]
          (do (assert (compile-form (assoc state* :form ?body)) (str "Couldn't compile init: " (pr-str ?body)))
            (.visitFieldInsn =init Opcodes/PUTSTATIC class-name* ?name (->java-sig (:type ?body)))))
        (doto =init
          (.visitInsn Opcodes/RETURN)
          (.visitMaxs 0 0)
          (.visitEnd))))
    (.visitEnd =class)
    (.toByteArray =class)))
