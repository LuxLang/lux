;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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
                          [lambda :as &&lambda]
                          [type :as &&type]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Exports]
(defn compile-bool [compile *type* ?value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC "java/lang/Boolean" (if ?value "TRUE" "FALSE") "Ljava/lang/Boolean;")]]
    (return nil)))

(do-template [<name> <class> <sig> <caster>]
  (defn <name> [compile *type* value]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/NEW <class>)
                    (.visitInsn Opcodes/DUP)
                    (.visitLdcInsn (<caster> value))
                    (.visitMethodInsn Opcodes/INVOKESPECIAL <class> "<init>" <sig>))]]
      (return nil)))

  compile-int  "java/lang/Long"      "(J)V" long
  compile-real "java/lang/Double"    "(D)V" double
  compile-char "java/lang/Character" "(C)V" char
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
                  (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object"))]
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
                  (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object"))]
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
                  (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object")
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
                                   (str (&host/->module-class (&/|head ?scope)) "/" (&host/location (&/|tail ?scope)))
                                   (str &&/closure-prefix ?captured-id)
                                   "Ljava/lang/Object;"))]]
    (return nil)))

(defn compile-global [compile *type* ?owner-class ?name]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (str (&host/->module-class ?owner-class) "/" (&/normalize-name ?name)) "_datum" "Ljava/lang/Object;")]]
    (return nil)))

(defn compile-apply [compile *type* ?fn ?args]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?fn)
        _ (&/map% (fn [?arg]
                    (|do [=arg (compile ?arg)
                          :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE &&/function-class "apply" &&/apply-signature)]]
                      (return =arg)))
                  ?args)]
    (return nil)))

(defn ^:private compile-def-type [compile ?body ?def-data]
  (|do [^MethodVisitor **writer** &/get-writer]
    (matchv ::M/objects [?def-data]
      [["lux;TypeD" _]]
      (let [_ (doto **writer**
                ;; Tail: Begin
                (.visitLdcInsn (int 2)) ;; S
                (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; V
                (.visitInsn Opcodes/DUP) ;; VV
                (.visitLdcInsn (int 0)) ;; VVI
                (.visitLdcInsn "lux;TypeD") ;; VVIT
                (.visitInsn Opcodes/AASTORE) ;; V
                (.visitInsn Opcodes/DUP) ;; VV
                (.visitLdcInsn (int 1)) ;; VVI
                (.visitInsn Opcodes/ACONST_NULL) ;; VVIN
                (.visitInsn Opcodes/AASTORE) ;; V
                )]
        (return nil))

      [["lux;ValueD" _]]
      (|let [;; _ (prn '?body (aget ?body 0) (aget ?body 1 0))
             [?def-value ?def-type] (matchv ::M/objects [?body]
                                      [[["ann" [?def-value ?type-expr]] ?def-type]]
                                      (&/T ?def-value ?type-expr)

                                      [[?def-value ?def-type]]
                                      (&/T ?body (&&type/->analysis ?def-type)))]
        (|do [:let [_ (doto **writer**
                        (.visitLdcInsn (int 2)) ;; S
                        (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; V
                        (.visitInsn Opcodes/DUP) ;; VV
                        (.visitLdcInsn (int 0)) ;; VVI
                        (.visitLdcInsn "lux;ValueD") ;; VVIT
                        (.visitInsn Opcodes/AASTORE) ;; V
                        (.visitInsn Opcodes/DUP) ;; VV
                        (.visitLdcInsn (int 1)) ;; VVI
                        )]
              _ (compile ?def-type)
              :let [_ (.visitInsn **writer** Opcodes/AASTORE)]]
          (return nil)))
      )))

(defn compile-def [compile ?name ?body ?def-data]
  (|do [^ClassWriter *writer* &/get-writer
        module-name &/get-module-name
        :let [datum-sig "Ljava/lang/Object;"
              def-name (&/normalize-name ?name)
              current-class (str (&host/->module-class module-name) "/" def-name)
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                               current-class nil "java/lang/Object" (into-array [&&/function-class]))
                       (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_name" "Ljava/lang/String;" nil ?name)
                           (doto (.visitEnd)))
                       (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_datum" datum-sig nil nil)
                           (doto (.visitEnd)))
                       (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_meta" datum-sig nil nil)
                           (doto (.visitEnd))))]
        _ (&/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
            (|do [^MethodVisitor **writer** &/get-writer
                  :let [_ (.visitCode **writer**)]
                  _ (compile ?body)
                  :let [_ (.visitFieldInsn **writer** Opcodes/PUTSTATIC current-class "_datum" datum-sig)]
                  _ (compile-def-type compile ?body ?def-data)
                  :let [_ (.visitFieldInsn **writer** Opcodes/PUTSTATIC current-class "_meta" datum-sig)]
                  :let [_ (doto **writer**
                            (.visitInsn Opcodes/RETURN)
                            (.visitMaxs 0 0)
                            (.visitEnd))]]
              (return nil)))
        :let [_ (.visitEnd *writer*)]
        _ (&&/save-class! def-name (.toByteArray =class))]
    (return nil)))

(defn compile-ann [compile *type* ?value-ex ?type-ex]
  (compile ?value-ex))

(defn compile-declare-macro [compile module name]
  (|do [_ (&a-module/declare-macro module name)]
    (return nil)))
