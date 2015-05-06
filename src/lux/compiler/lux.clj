(ns lux.compiler.lux
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            (lux.analyser [base :as &a]
                          [module :as &a-module])
            (lux.compiler [base :as &&]
                          [lambda :as &&lambda]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Exports]
(let [+class+ (&host/->class "java.lang.Boolean")
      +sig+ (&host/->type-signature "java.lang.Boolean")]
  (defn compile-bool [compile *type* ?value]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") (if ?value "TRUE" "FALSE") (&host/->type-signature "java.lang.Boolean"))]]
      (return nil))))

(do-template [<name> <class> <sig> <caster>]
  (let [+class+ (&host/->class <class>)]
    (defn <name> [compile *type* value]
      (|do [^MethodVisitor *writer* &/get-writer
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
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn compile-tuple [compile *type* ?elems]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [num-elems (&/|length ?elems)
              _ (doto *writer*
                  (.visitLdcInsn (int num-elems))
                  (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class "java.lang.Object")))]
        _ (&/map2% (fn [idx elem]
                     (|do [:let [_ (doto *writer*
                                     (.visitInsn Opcodes/DUP)
                                     (.visitLdcInsn (int idx)))]
                           ret (compile elem)
                           :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
                       (return ret)))
                   (&/|range num-elems) ?elems)]
    (return nil)))

(defn compile-record [compile *type* ?elems]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [elems* (->> ?elems
                          &/->seq
                          (sort #(compare (&/|first %1) (&/|first %2)))
                          &/->list)
              num-elems (&/|length elems*)
              _ (doto *writer*
                  (.visitLdcInsn (int num-elems))
                  (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class "java.lang.Object")))]
        _ (&/map2% (fn [idx kv]
                     (|let [[k v] kv]
                       (|do [:let [_ (doto *writer*
                                       (.visitInsn Opcodes/DUP)
                                       (.visitLdcInsn (int idx)))]
                             ret (compile v)
                             :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
                         (return ret))))
                  (&/|range num-elems) elems*)]
    (return nil)))

(defn compile-variant [compile *type* ?tag ?value]
  (|do [^MethodVisitor *writer* &/get-writer
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
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitVarInsn *writer* Opcodes/ALOAD (int ?idx))]]
    (return nil)))

(defn compile-captured [compile *type* ?scope ?captured-id ?source]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitFieldInsn Opcodes/GETFIELD
                                   (&host/location ?scope)
                                   (str &&/closure-prefix ?captured-id)
                                   "Ljava/lang/Object;"))]]
    (return nil)))

(defn compile-global [compile *type* ?owner-class ?name]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (&host/->class (&host/location (&/|list ?owner-class ?name))) "_datum" "Ljava/lang/Object;")]]
    (return nil)))

(defn compile-apply [compile *type* ?fn ?arg]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?fn)
        _ (compile ?arg)
        :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (&host/->class &host/function-class) "apply" &&/apply-signature)]]
    (return nil)))

(defn compile-def [compile ?name ?body ?def-data]
  (|do [^ClassWriter *writer* &/get-writer
        module-name &/get-module-name
        :let [outer-class (&host/->class module-name)
              datum-sig (&host/->type-signature "java.lang.Object")
              current-class (&host/location (&/|list outer-class ?name))
              _ (.visitInnerClass *writer* current-class outer-class nil (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                               current-class nil "java/lang/Object" (into-array [(&host/->class &host/function-class)]))
                       (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_datum" datum-sig nil nil)
                           (doto (.visitEnd))))]
        _ (&/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
            (|do [^MethodVisitor **writer** &/get-writer
                  :let [_ (.visitCode **writer**)]
                  _ (compile ?body)
                  :let [_ (doto **writer**
                            (.visitFieldInsn Opcodes/PUTSTATIC current-class "_datum" datum-sig)
                            (.visitInsn Opcodes/RETURN)
                            (.visitMaxs 0 0)
                            (.visitEnd))]]
              (return nil)))
        :let [_ (.visitEnd *writer*)]
        _ (&&/save-class! current-class (.toByteArray =class))]
    (return nil)))

(defn compile-declare-macro [compile module name]
  (|do [_ (&a-module/declare-macro module name)]
    (return nil)))
