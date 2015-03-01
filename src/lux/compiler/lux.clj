(ns lux.compiler.lux
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return* return fail fail*
                                     repeat-m exhaust-m try-m try-all-m map-m reduce-m
                                     apply-m
                                     normalize-ident]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.analyser.base :as &a]
            (lux.compiler [base :as &&]
                          [lambda :as &&lambda])
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils]
(defn ^:private compile-field [compile ?name body]
  (exec [*writer* &/get-writer
         module-name &/get-module-name
         :let [outer-class (&host/->class module-name)
               datum-sig (&host/->type-signature "java.lang.Object")
               current-class (&host/location (list ?name outer-class))
               _ (.visitInnerClass *writer* current-class outer-class nil (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
               =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                        (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                                current-class nil "java/lang/Object" (into-array [(&host/->class &host/function-class)]))
                        (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_datum" datum-sig nil nil)
                            (doto (.visitEnd))))]
         _ (&/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
             (exec [*writer* &/get-writer
                    :let [_ (.visitCode *writer*)]
                    _ (compile body)
                    :let [_ (doto *writer*
                              (.visitFieldInsn Opcodes/PUTSTATIC current-class "_datum" datum-sig)
                              (.visitInsn Opcodes/RETURN)
                              (.visitMaxs 0 0)
                              (.visitEnd))]]
               (return nil)))
         :let [_ (.visitEnd *writer*)]
         _ (&&/save-class! current-class (.toByteArray =class))]
    (return nil)))

;; [Resources]
(let [+class+ (&host/->class "java.lang.Boolean")
      +sig+ (&host/->type-signature "java.lang.Boolean")]
  (defn compile-bool [compile *type* ?value]
    (exec [*writer* &/get-writer
           :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") (if ?value "TRUE" "FALSE") (&host/->type-signature "java.lang.Boolean"))]]
      (return nil))))

(do-template [<name> <class> <sig>]
  (let [+class+ (&host/->class <class>)]
    (defn <name> [compile *type* value]
      (exec [*writer* &/get-writer
             :let [_ (doto *writer*
                       (.visitTypeInsn Opcodes/NEW <class>)
                       (.visitInsn Opcodes/DUP)
                       (.visitLdcInsn value)
                       (.visitMethodInsn Opcodes/INVOKESPECIAL <class> "<init>" <sig>))]]
        (return nil))))

  compile-int  "java.lang.Long"      "(J)V"
  compile-real "java.lang.Double"    "(D)V"
  compile-char "java.lang.Character" "(C)V"
  )

(defn compile-text [compile *type* ?value]
  (exec [*writer* &/get-writer
         :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn compile-tuple [compile *type* ?elems]
  (exec [*writer* &/get-writer
         :let [num-elems (count ?elems)
               tuple-class (&host/->class (str &host/tuple-class num-elems))
               _ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW tuple-class)
                   (.visitInsn Opcodes/DUP)
                   (.visitMethodInsn Opcodes/INVOKESPECIAL tuple-class "<init>" "()V"))]
         _ (map-m (fn [idx]
                    (exec [:let [_ (.visitInsn *writer* Opcodes/DUP)]
                           ret (compile (nth ?elems idx))
                           :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD tuple-class (str &&/partial-prefix idx) "Ljava/lang/Object;")]]
                      (return ret)))
                  (range num-elems))]
    (return nil)))

(defn compile-variant [compile *type* ?tag ?value]
  (exec [*writer* &/get-writer
         :let [variant-class* (&host/->class &host/variant-class)
               _ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW variant-class*)
                   (.visitInsn Opcodes/DUP)
                   (.visitMethodInsn Opcodes/INVOKESPECIAL variant-class* "<init>" "()V")
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn ?tag)
                   (.visitFieldInsn Opcodes/PUTFIELD variant-class* "tag" (&host/->type-signature "java.lang.String"))
                   (.visitInsn Opcodes/DUP))]
         _ (compile ?value)
         :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD variant-class* "value" (&host/->type-signature "java.lang.Object"))]]
    (return nil)))

(defn compile-local [compile *type* ?idx]
  (exec [*writer* &/get-writer
         :let [_ (.visitVarInsn *writer* Opcodes/ALOAD (int ?idx))]]
    (return nil)))

(defn compile-captured [compile *type* ?scope ?captured-id ?source]
  (exec [*writer* &/get-writer
         :let [_ (doto *writer*
                   (.visitVarInsn Opcodes/ALOAD 0)
                   (.visitFieldInsn Opcodes/GETFIELD
                                    (normalize-ident ?scope)
                                    (str &&/closure-prefix ?captured-id)
                                    "Ljava/lang/Object;"))]]
    (return nil)))

(defn compile-global [compile *type* ?owner-class ?name]
  (exec [*writer* &/get-writer
         :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (&host/->class (&host/location (list ?name ?owner-class))) "_datum" "Ljava/lang/Object;")]]
    (return nil)))

(defn compile-call [compile *type* ?fn ?args]
  (exec [*writer* &/get-writer
         _ (compile ?fn)
         _ (map-m (fn [arg]
                    (exec [ret (compile arg)
                           :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (&host/->class &host/function-class) "apply" &&/apply-signature)]]
                      (return ret)))
                  ?args)]
    (return nil)))

(defn compile-static-call [compile *type* ?needs-num ?fn ?args]
  (assert false (pr-str 'compile-static-call))
  (exec [*writer* &/get-writer
         :let [_ (match (:form ?fn)
                   [::&a/global ?owner-class ?fn-name]
                   (let [arg-sig (&host/->type-signature "java.lang.Object")
                         call-class (&host/location (list ?fn-name ?owner-class))
                         provides-num (count ?args)]
                     (if (>= provides-num ?needs-num)
                       (let [impl-sig (str "(" (reduce str "" (repeat ?needs-num arg-sig)) ")" arg-sig)]
                         (doto *writer*
                           (-> (do (compile arg))
                               (->> (doseq [arg (take ?needs-num ?args)])))
                           (.visitMethodInsn Opcodes/INVOKESTATIC call-class "impl" impl-sig)
                           (-> (doto (do (compile arg))
                                 (.visitMethodInsn Opcodes/INVOKEINTERFACE (&host/->class &host/function-class) "apply" &&/apply-signature))
                               (->> (doseq [arg (drop ?needs-num ?args)])))))
                       (let [counter-sig "I"
                             init-signature (str "(" (apply str counter-sig (repeat (dec ?needs-num) arg-sig)) ")" "V")]
                         (doto *writer*
                           (.visitTypeInsn Opcodes/NEW call-class)
                           (.visitInsn Opcodes/DUP)
                           (.visitLdcInsn (int provides-num))
                           (-> (do (compile arg))
                               (->> (doseq [arg ?args])))
                           (&&/add-nulls (dec (- ?needs-num provides-num)))
                           (.visitMethodInsn Opcodes/INVOKESPECIAL call-class "<init>" init-signature)))
                       ))
                   )]]
    (return nil)))

(defn compile-def [compile name value]
  (exec [value-type (&a/expr-type value)]
    (match value
      [::&a/Expression ?form _]
      (match ?form
        [::&a/lambda ?scope ?captured ?args ?body]
        (&&lambda/compile-lambda compile value-type ?scope ?captured ?args ?body true false)

        _
        (compile-field compile name value))
      
      _
      (fail "Can only define expressions."))))

(defn compile-self-call [compile ?assumed-args]
  (exec [*writer* &/get-writer
         :let [_ (.visitVarInsn *writer* Opcodes/ALOAD 0)]
         _ (map-m (fn [arg]
                    (exec [ret (compile arg)
                           :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (&host/->class &host/function-class) "apply" &&/apply-signature)]]
                      (return ret)))
                  ?assumed-args)]
    (return nil)))
