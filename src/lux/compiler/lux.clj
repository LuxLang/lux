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
               current-class (&host/location (list outer-class ?name))
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

(do-template [<name> <class> <sig> <caster>]
  (let [+class+ (&host/->class <class>)]
    (defn <name> [compile *type* value]
      (exec [*writer* &/get-writer
             :let [_ (doto *writer*
                       (.visitTypeInsn Opcodes/NEW +class+)
                       (.visitInsn Opcodes/DUP)
                       (.visitLdcInsn (<caster> value))
                       (.visitMethodInsn Opcodes/INVOKESPECIAL +class+ "<init>" <sig>))]]
        (return nil))))

  compile-int  "java.lang.Long"      "(J)V" long
  compile-real "java.lang.Double"    "(D)V" double
  compile-char "java.lang.Character" "(C)V" char
  )

(defn compile-text [compile *type* ?value]
  (exec [*writer* &/get-writer
         :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn compile-tuple [compile *type* ?elems]
  (exec [*writer* &/get-writer
         :let [num-elems (count ?elems)
               _ (doto *writer*
                   (.visitLdcInsn (int num-elems))
                   (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class "java.lang.Object")))]
         _ (map-m (fn [[idx elem]]
                    (exec [:let [_ (doto *writer*
                                     (.visitInsn Opcodes/DUP)
                                     (.visitLdcInsn (int idx)))]
                           ret (compile elem)
                           :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
                      (return ret)))
                  (map vector (range num-elems) ?elems))]
    (return nil)))

(defn compile-variant [compile *type* ?tag ?value]
  (exec [*writer* &/get-writer
         :let [_ (doto *writer*
                   (.visitLdcInsn (int 2))
                   (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class "java.lang.Object"))
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn (int 0))
                   (.visitLdcInsn ?tag)
                   (.visitInsn Opcodes/AASTORE)
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn (int 1)))]
         _ (compile ?value)
         :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
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
         :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (&host/->class (&host/location (list ?owner-class ?name))) "_datum" "Ljava/lang/Object;")]]
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

(defn compile-self-call [compile ?scope ?assumed-args]
  ;; (prn 'compile-self-call ?scope ?assumed-args)
  (exec [*writer* &/get-writer
         :let [lambda-class (&host/location ?scope)]
         :let [_ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW lambda-class)
                   (.visitInsn Opcodes/DUP))]
         :let [num-args (if (= '("lux" "fold") ?scope)
                          3
                          (count ?assumed-args))
               init-signature (str "(" (if (> num-args 1)
                                         (reduce str "I" (repeat (dec num-args) (&host/->type-signature "java.lang.Object"))))
                                   ")"
                                   "V")
               _ (do (when (> num-args 1)
                       (.visitInsn *writer* Opcodes/ICONST_0)
                       (&&/add-nulls *writer* (dec num-args)))
                   (.visitMethodInsn *writer* Opcodes/INVOKESPECIAL lambda-class "<init>" init-signature))]
         _ (map-m (fn [arg]
                    (exec [ret (compile arg)
                           :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (&host/->class &host/function-class) "apply" &&/apply-signature)]]
                      (return ret)))
                  ?assumed-args)]
    (return nil)))
