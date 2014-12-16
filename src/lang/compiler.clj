(ns lang.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]
            (lang [type :as &type]
                  [lexer :as &lexer]
                  [parser :as &parser]
                  [analyser :as &analyser])
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

(declare compile-form
         compile)

;; [Utils/General]
(defn ^:private write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(def ^:private +variant-class+ "test2.Tagged")

(defmacro ^:private defcompiler [name match body]
  `(defn ~name [~'*state*]
     (let [~'*class-name* (:class-name ~'*state*)
           ~'*writer* (:writer ~'*state*)
           ~'*parent* (:parent ~'*state*)
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
    (->type-signature +variant-class+)

    [::&type/function ?args ?return]
    (->java-sig [::&type/object "test2/Function" []])))

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

(defcompiler ^:private compile-tuple
  [::&analyser/tuple ?elems]
  (let [num-elems (count ?elems)]
    (let [tuple-class (str "test2/Tuple" num-elems)]
      (doto *writer*
        (.visitTypeInsn Opcodes/NEW tuple-class)
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKESPECIAL tuple-class "<init>" "()V"))
      (dotimes [idx num-elems]
        (.visitInsn *writer* Opcodes/DUP)
        (compile-form (assoc *state* :form (nth ?elems idx)))
        (.visitFieldInsn *writer* Opcodes/PUTFIELD tuple-class (str "_" idx) "Ljava/lang/Object;")))))

(defcompiler ^:private compile-local
  [::&analyser/local ?idx]
  (do ;; (prn 'LOCAL ?idx)
      (doto *writer*
        (.visitVarInsn Opcodes/ALOAD (int ?idx)))))

(defcompiler ^:private compile-global
  [::&analyser/global ?owner-class ?name]
  (do ;; (prn 'GLOBAL ?owner-class ?name *type*)
      (doto *writer*
        (.visitFieldInsn Opcodes/GETSTATIC (->class ?owner-class) ?name (->java-sig *type*)))))

;; (defcompiler ^:private compile-call
;;   [::&analyser/call ?fn ?args]
;;   (do (prn 'compile-call (:form ?fn) ?fn ?args)
;;     (doseq [arg (reverse ?args)]
;;       (compile-form (assoc *state* :form arg)))
;;     (match (:form ?fn)
;;       [::&analyser/global ?owner-class ?fn-name]
;;       (let [signature (str "(" (apply str (repeat (count ?args) "Ljava/lang/Object;")) ")" "Ljava/lang/Object;")]
;;         (doto *writer*
;;           (.visitMethodInsn Opcodes/INVOKESTATIC (->class ?owner-class) ?fn-name signature))))))

(defcompiler ^:private compile-call
  [::&analyser/call ?fn ?args]
  (do (prn 'compile-call (:form ?fn) ?fn ?args)
    (match (:form ?fn)
      [::&analyser/local _]
      (do (compile-form (assoc *state* :form ?fn))
        (let [apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"]
          (doseq [arg ?args]
            (compile-form (assoc *state* :form arg))
            (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE "test2/Function" "apply" apply-signature))))
      
      [::&analyser/lambda _ ?body]
      (do (compile-form (assoc *state* :form ?fn))
        (let [apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"]
          (doseq [arg ?args]
            (compile-form (assoc *state* :form arg))
            (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE "test2/Function" "apply" apply-signature))))
      
      [::&analyser/global ?owner-class ?fn-name]
      (let [apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"
            signature "()V"
            call-class (str (->class ?owner-class) "$" ?fn-name "_0")]
        (doto *writer*
          (.visitTypeInsn Opcodes/NEW call-class)
          (.visitInsn Opcodes/DUP)
          (.visitMethodInsn Opcodes/INVOKESPECIAL call-class "<init>" signature))
        (doseq [arg ?args]
          (compile-form (assoc *state* :form arg))
          (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE "test2/Function" "apply" apply-signature))
        ))))

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
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.io.PrintStream") ?method "(Ljava/lang/Object;)V")
      (.visitInsn Opcodes/ACONST_NULL))))

(defcompiler ^:private compile-ann-class
  [::&analyser/ann-class ?class ?members]
  nil)

(defcompiler ^:private compile-if
  [::&analyser/if ?test ?then ?else]
  (let [else-label (new Label)
        end-label (new Label)]
    ;; (println "PRE")
    (assert (compile-form (assoc *state* :form ?test)) "CAN't COMPILE TEST")
    (doto *writer*
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.lang.Boolean") "booleanValue" "()Z")
      (.visitJumpInsn Opcodes/IFEQ else-label))
    (prn 'compile-if/?then (:form ?then))
    (assert (compile-form (assoc *state* :form ?then)) "CAN't COMPILE THEN")
    (doto *writer*
      (.visitJumpInsn Opcodes/GOTO end-label)
      (.visitLabel else-label))
    (assert (compile-form (assoc *state* :form ?else)) "CAN't COMPILE ELSE")
    (.visitLabel *writer* end-label)))

(defcompiler ^:private compile-do
  [::&analyser/do ?exprs]
  (do (doseq [expr (butlast ?exprs)]
        (compile-form (assoc *state* :form expr))
        (.visitInsn *writer* Opcodes/POP))
    (compile-form (assoc *state* :form (last ?exprs)))))

(defcompiler ^:private compile-let
  [::&analyser/let ?idx ?label ?value ?body]
  (let [start-label (new Label)
        end-label (new Label)
        ?idx (int ?idx)]
    (prn '(:type ?value) (:type ?value) (->java-sig (:type ?value)))
    (.visitLocalVariable *writer* ?label (->java-sig (:type ?value)) nil start-label end-label ?idx)
    (assert (compile-form (assoc *state* :form ?value)) "CAN't COMPILE LET-VALUE")
    (doto *writer*
      (.visitVarInsn Opcodes/ASTORE ?idx)
      (.visitLabel start-label))
    (assert (compile-form (assoc *state* :form ?body)) "CAN't COMPILE LET-BODY")
    (.visitLabel *writer* end-label)))

(defn ^:private compile-method-function [writer class-name fn-name num-args]
  (let [outer-class (->class class-name)
        clo-field-sig (->type-signature "java.lang.Object")
        apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"
        real-signature (str "(" (apply str (repeat num-args "Ljava/lang/Object;")) ")" "Ljava/lang/Object;")]
    ;; (.mkdirs (java.io.File. class-name))
    (doseq [idx (range num-args)
            :let [has-next? (not= idx (dec num-args))
                  local-name (str fn-name "_" idx)
                  current-class (str outer-class "$" local-name)
                  next-class (str outer-class "$" fn-name "_" (inc idx))
                  current-signature (str "(" (apply str (repeat idx "Ljava/lang/Object;")) ")" "V")
                  next-signature (str "(" (apply str (repeat (inc idx) "Ljava/lang/Object;")) ")" "V")]]
      (.visitInnerClass writer current-class outer-class local-name (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
      (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                     (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                             current-class nil "java/lang/Object" (into-array ["test2/Function"])))
            _ (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<init>" current-signature nil nil)
                (.visitCode)
                (.visitVarInsn Opcodes/ALOAD 0)
                (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
                (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                      (.visitVarInsn Opcodes/ALOAD (inc clo_idx))
                      (.visitFieldInsn Opcodes/PUTFIELD current-class field-name clo-field-sig))
                    (->> (let [field-name (str "_" clo_idx)]
                           (doto (.visitField =class Opcodes/ACC_PUBLIC field-name clo-field-sig nil nil)
                             (.visitEnd)))
                         (dotimes [clo_idx idx])))
                (.visitInsn Opcodes/RETURN)
                (.visitMaxs 0 0)
                (.visitEnd))
            =method (doto (.visitMethod =class Opcodes/ACC_PUBLIC "apply" apply-signature nil nil)
                      (.visitCode))
            _ (do (when has-next?
                    (doto =method
                      (.visitTypeInsn Opcodes/NEW next-class)
                      (.visitInsn Opcodes/DUP)))
                (doto =method
                  (-> (doto (.visitVarInsn Opcodes/ALOAD (int 0))
                        (.visitFieldInsn Opcodes/GETFIELD current-class (str "_" clo_idx) clo-field-sig))
                      (->> (dotimes [clo_idx idx])))
                  (.visitVarInsn Opcodes/ALOAD (int 1)))
                (if has-next?
                  (.visitMethodInsn =method Opcodes/INVOKESPECIAL next-class "<init>" next-signature)
                  (.visitMethodInsn =method Opcodes/INVOKESTATIC outer-class fn-name real-signature))
                (doto =method
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
                (.visitEnd =class))]
        ;; (write-file (str class-name "/" current-class ".class") (.toByteArray =class))
        (write-file (str current-class ".class") (.toByteArray =class))))))

(defcompiler ^:private compile-def
  [::&analyser/def ?form ?body]
  (do (prn 'compile-def ?form)
    (match ?form
      (?name :guard string?)
      (let [=type (:type ?body)
            ;; _ (prn '?body ?body)
            ]
        (doto (.visitField *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name (->java-sig =type) nil nil)
          (.visitEnd)))

      [?name ?args]
      (do (prn 'compile-def `(~'def (~(symbol ?name) ~@(map symbol ?args))))
        (if (= "main" ?name)
          (let [signature "([Ljava/lang/String;)V"
                =method (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name signature nil nil)
                          (.visitCode))]
            ;; (prn 'FN/?body ?body)
            (assert (compile-form (assoc *state* :parent *writer* :writer =method :form ?body)) (str "Body couldn't compile: " (pr-str ?body)))
            (doto =method
              (.visitInsn Opcodes/RETURN)
              (.visitMaxs 0 0)
              (.visitEnd)))
          (let [signature (str "(" (apply str (repeat (count ?args) "Ljava/lang/Object;")) ")" "Ljava/lang/Object;")
                _ (prn 'signature signature)
                =method (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name signature nil nil)
                          (.visitCode))]
            (compile-form (assoc *state* :parent *writer* :writer =method :form ?body))
            (doto =method
              (.visitInsn Opcodes/ARETURN)
              (.visitMaxs 0 0)
              (.visitEnd))
            (compile-method-function *writer* *class-name* ?name (count ?args)))))
      )))

(defcompiler ^:private compile-lambda
  [::&analyser/lambda ?args ?body]
  (let [num-args (count ?args)
        signature (str "(" (apply str (repeat num-args "Ljava/lang/Object;")) ")" "Ljava/lang/Object;")
        outer-class (->class *class-name*)
        clo-field-sig (->type-signature "java.lang.Object")
        apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"
        real-signature (str "(" (apply str (repeat num-args "Ljava/lang/Object;")) ")" "Ljava/lang/Object;")]
    (doseq [idx (range num-args)
            :let [has-next? (not= idx (dec num-args))
                  local-name (str "lambda_" idx)
                  current-class (str outer-class "$" local-name)
                  next-class (str outer-class "$" "lambda_" (inc idx))
                  current-signature (str "(" (apply str (repeat idx "Ljava/lang/Object;")) ")" "V")
                  next-signature (str "(" (apply str (repeat (inc idx) "Ljava/lang/Object;")) ")" "V")]]
      (.visitInnerClass *parent* current-class outer-class local-name (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
      (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                     (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                             current-class nil "java/lang/Object" (into-array ["test2/Function"])))
            _ (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<init>" current-signature nil nil)
                (.visitCode)
                (.visitVarInsn Opcodes/ALOAD 0)
                (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
                (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                      (.visitVarInsn Opcodes/ALOAD (inc clo_idx))
                      (.visitFieldInsn Opcodes/PUTFIELD current-class field-name clo-field-sig))
                    (->> (let [field-name (str "_" clo_idx)]
                           (doto (.visitField =class Opcodes/ACC_PUBLIC field-name clo-field-sig nil nil)
                             (.visitEnd)))
                         (dotimes [clo_idx idx])))
                (.visitInsn Opcodes/RETURN)
                (.visitMaxs 0 0)
                (.visitEnd))
            =method (doto (.visitMethod =class Opcodes/ACC_PUBLIC "apply" apply-signature nil nil)
                      (.visitCode))
            _ (do (when has-next?
                    (doto =method
                      (.visitTypeInsn Opcodes/NEW next-class)
                      (.visitInsn Opcodes/DUP)))
                (doto =method
                  (-> (doto (.visitVarInsn Opcodes/ALOAD (int 0))
                        (.visitFieldInsn Opcodes/GETFIELD current-class (str "_" clo_idx) clo-field-sig))
                      (->> (dotimes [clo_idx idx])))
                  (.visitVarInsn Opcodes/ALOAD (int 1)))
                (if has-next?
                  (.visitMethodInsn =method Opcodes/INVOKESPECIAL next-class "<init>" next-signature)
                  (.visitMethodInsn =method Opcodes/INVOKESTATIC outer-class "lambda_impl" real-signature))
                (doto =method
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
                (.visitEnd =class))]
        (println "OUTPUT LAMBDA:" (str current-class ".class"))
        (write-file (str current-class ".class") (.toByteArray =class))))
    (let [=method (doto (.visitMethod *parent* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "lambda_impl" real-signature nil nil)
                    (.visitCode))]
      (prn '(:form ?body) (:form ?body))
      (compile-form (assoc *state* :parent *parent* :writer =method :form ?body))
      (doto =method
        (.visitInsn Opcodes/ARETURN)
        (.visitMaxs 0 0)
        (.visitEnd))
      ;; (compile-form (assoc *state* :writer =method :form ?body))
      ;; (compile-method-function *writer* *class-name* ?name (count ?args))
      )
    (let [init-class (str outer-class "$" "lambda_0")]
      (doto *writer*
        (.visitTypeInsn Opcodes/NEW init-class)
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKESPECIAL init-class "<init>" "()V")))
    ))

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
                     (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_INTERFACE ;; Opcodes/ACC_ABSTRACT
                                             )
                             (str parent-dir "/" ?name) nil "java/lang/Object" nil))]
    (doseq [[?method ?props] (:methods ?members)
            :let [[?args ?return] (:type ?props)
                  signature (str "(" (reduce str "" (map ->type-signature ?args)) ")" (->type-signature ?return))]]
      (.visitMethod =interface (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT) ?method signature nil nil))
    (.visitEnd =interface)
    (.mkdirs (java.io.File. parent-dir))
    (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. (str parent-dir "/" ?name ".class")))]
      (.write stream (.toByteArray =interface)))))

(defcompiler ^:private compile-variant
  [::&analyser/variant ?tag ?value]
  (let [variant-class* (->class +variant-class+)]
    ;; (prn 'compile-variant ?tag ?value)
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

(defcompiler compile-import
  [::&analyser/import ?class]
  nil)

(defcompiler compile-require
  [::&analyser/require ?file ?alias]
  (let [module-name (re-find #"[^/]+$" ?file)
        _ (prn 'module-name module-name)
        source-code (slurp (str module-name ".lang"))
        _ (prn 'source-code source-code)
        tokens (&lexer/lex source-code)
        _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        _ (prn 'syntax syntax)
        ann-syntax (&analyser/analyse module-name syntax)
        _ (prn 'ann-syntax ann-syntax)
        class-data (compile module-name ann-syntax)]
    (write-file (str module-name ".class") class-data)
    nil))

(let [+compilers+ [compile-literal
                   compile-variant
                   compile-tuple
                   compile-local
                   compile-global
                   compile-call
                   compile-static-access
                   compile-dynamic-access
                   compile-ann-class
                   compile-if
                   compile-do
                   compile-let
                   compile-lambda
                   compile-def
                   compile-defclass
                   compile-definterface
                   compile-import
                   compile-require]]
  (defn ^:private compile-form [state]
    ;; (prn 'compile-form/state state)
    (or (some #(% state) +compilers+)
        (assert false (str "Can't compile: " (pr-str (:form state)))))))

;; [Interface]
(defn compile [class-name inputs]
  ;; (prn 'inputs inputs)
  (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                 (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                         (->class class-name) nil "java/lang/Object" nil))
        ;; (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
        ;;          (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
        ;;                  "output" nil "java/lang/Object" nil))
        state {:class-name class-name
               :writer =class
               :form nil
               :parent nil}]
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
    ;; (prn 'inputs inputs)
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
