;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.lux
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.host.generics :as &host-generics]
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
(defn compile-bool [compile ?value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC "java/lang/Boolean" (if ?value "TRUE" "FALSE") "Ljava/lang/Boolean;")]]
    (return nil)))

(do-template [<name> <class> <sig> <caster>]
  (defn <name> [compile value]
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

(defn compile-text [compile ?value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn compile-tuple [compile ?elems]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [num-elems (&/|length ?elems)]]
    (if (= 0 num-elems)
      (|do [:let [_ (.visitInsn *writer* Opcodes/ACONST_NULL)]]
        (return nil))
      (|do [:let [_ (doto *writer*
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
        (return nil)))))

(defn compile-variant [compile ?tag ?value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitLdcInsn (int 2))
                  (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object")
                  (.visitInsn Opcodes/DUP)
                  (.visitLdcInsn (int 0))
                  (.visitLdcInsn ?tag)
                  (&&/wrap-long)
                  (.visitInsn Opcodes/AASTORE)
                  (.visitInsn Opcodes/DUP)
                  (.visitLdcInsn (int 1)))]
        _ (compile ?value)
        :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
    (return nil)))

(defn compile-local [compile ?idx]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitVarInsn *writer* Opcodes/ALOAD (int ?idx))]]
    (return nil)))

(defn compile-captured [compile ?scope ?captured-id ?source]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitFieldInsn Opcodes/GETFIELD
                                   (str (&host/->module-class (&/|head ?scope)) "/" (&host/location (&/|tail ?scope)))
                                   (str &&/closure-prefix ?captured-id)
                                   "Ljava/lang/Object;"))]]
    (return nil)))

(defn compile-global [compile ?owner-class ?name]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (str (&host/->module-class ?owner-class) "/" (&/normalize-name ?name)) &/datum-field "Ljava/lang/Object;")]]
    (return nil)))

(defn compile-apply [compile ?fn ?args]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?fn)
        _ (&/map% (fn [?arg]
                    (|do [=arg (compile ?arg)
                          :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE &&/function-class "apply" &&/apply-signature)]]
                      (return =arg)))
                  ?args)]
    (return nil)))

(defn ^:private compile-def-type [compile current-class ?body def-type]
  (|do [^MethodVisitor **writer** &/get-writer]
    (|case def-type
      "type"
      (|do [:let [_ (doto **writer**
                      ;; Tail: Begin
                      (.visitLdcInsn (int 2)) ;; S
                      (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; V
                      (.visitInsn Opcodes/DUP) ;; VV
                      (.visitLdcInsn (int 0)) ;; VVI
                      (.visitLdcInsn &/$TypeD) ;; VVIT
                      (&&/wrap-long)
                      (.visitInsn Opcodes/AASTORE) ;; V
                      (.visitInsn Opcodes/DUP) ;; VV
                      (.visitLdcInsn (int 1)) ;; VVI
                      (.visitFieldInsn Opcodes/GETSTATIC current-class &/datum-field "Ljava/lang/Object;")
                      (.visitInsn Opcodes/AASTORE) ;; V
                      )]]
        (return nil))

      "value"
      (|let [?def-type (|case ?body
                         [[?def-type ?def-cursor] (&a/$ann ?def-value ?type-expr ?def-value-type)]
                         ?type-expr

                         [[?def-type ?def-cursor] ?def-value]
                         (&&type/->analysis ?def-type))]
        (|do [:let [_ (doto **writer**
                        (.visitLdcInsn (int 2)) ;; S
                        (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; V
                        (.visitInsn Opcodes/DUP) ;; VV
                        (.visitLdcInsn (int 0)) ;; VVI
                        (.visitLdcInsn &/$ValueD) ;; VVIT
                        (&&/wrap-long)
                        (.visitInsn Opcodes/AASTORE) ;; V
                        (.visitInsn Opcodes/DUP) ;; VV
                        (.visitLdcInsn (int 1)) ;; VVI
                        )]
              :let [_ (doto **writer**
                        (.visitLdcInsn (int 2)) ;; S
                        (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; V
                        (.visitInsn Opcodes/DUP) ;; VV
                        (.visitLdcInsn (int 0)) ;; VVI
                        )]
              _ (compile ?def-type)
              :let [_ (.visitInsn **writer** Opcodes/AASTORE)]
              :let [_ (doto **writer**
                        (.visitInsn Opcodes/DUP) ;; VV
                        (.visitLdcInsn (int 1)) ;; VVI
                        (.visitFieldInsn Opcodes/GETSTATIC current-class &/datum-field "Ljava/lang/Object;")
                        (.visitInsn Opcodes/AASTORE))]
              :let [_ (.visitInsn **writer** Opcodes/AASTORE)]]
          (return nil)))
      )))

(let [class-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
      field-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC)]
  (defn compile-def [compile ?name ?body]
    (|do [:let [=value-type (&a/expr-type* ?body)
                def-type (cond (&type/type= &type/Type =value-type)
                               "type"
                               
                               :else
                               "value")]
          ^ClassWriter *writer* &/get-writer
          module-name &/get-module-name
          [file-name _ _] &/cursor
          :let [datum-sig "Ljava/lang/Object;"
                def-name (&/normalize-name ?name)
                current-class (str (&host/->module-class module-name) "/" def-name)
                =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit &host/bytecode-version class-flags
                                 current-class nil "java/lang/Object" (into-array [&&/function-class]))
                         (-> (.visitField field-flags &/name-field "Ljava/lang/String;" nil ?name)
                             (doto (.visitEnd)))
                         (-> (.visitField field-flags &/datum-field datum-sig nil nil)
                             (doto (.visitEnd)))
                         (-> (.visitField field-flags &/meta-field datum-sig nil nil)
                             (doto (.visitEnd)))
                         (.visitSource file-name nil))]
          _ (&/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
              (|do [^MethodVisitor **writer** &/get-writer
                    :let [_ (.visitCode **writer**)]
                    _ (compile ?body)
                    :let [_ (.visitFieldInsn **writer** Opcodes/PUTSTATIC current-class &/datum-field datum-sig)]
                    _ (compile-def-type compile current-class ?body def-type)
                    :let [_ (.visitFieldInsn **writer** Opcodes/PUTSTATIC current-class &/meta-field datum-sig)]
                    :let [_ (doto **writer**
                              (.visitInsn Opcodes/RETURN)
                              (.visitMaxs 0 0)
                              (.visitEnd))]]
                (return nil)))
          :let [_ (.visitEnd *writer*)]
          _ (&&/save-class! def-name (.toByteArray =class))
          class-loader &/loader
          :let [def-class (&&/load-class! class-loader (&host-generics/->class-name current-class))]
          _ (&a-module/define module-name ?name (-> def-class (.getField &/meta-field) (.get nil)) =value-type)]
      (return nil))))

(defn compile-ann [compile ?value-ex ?type-ex ?value-type]
  (compile ?value-ex))

(defn compile-coerce [compile ?value-ex ?type-ex ?value-type]
  (compile ?value-ex))

(defn compile-declare-macro [compile module name]
  (|do [_ (&a-module/declare-macro module name)]
    (return nil)))
