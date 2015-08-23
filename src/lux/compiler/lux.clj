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
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
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

;; [Utils]
(defn ^:private array-of [^MethodVisitor *writer* type-name size]
  (do (doto *writer*
        (.visitLdcInsn (int size))
        (.visitTypeInsn Opcodes/ANEWARRAY type-name))
    (return nil)))

(defn ^:private store-at [^MethodVisitor *writer* compile idx value]
  (|do [:let [_ (doto *writer*
                  (.visitInsn Opcodes/DUP)
                  (.visitLdcInsn (int idx)))]
        _ (compile value)
        :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
    (return nil)))

;; [Exports]
(defn compile-unit [compile *type*]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitInsn *writer* Opcodes/ACONST_NULL)]]
    (return nil)))

(defn compile-bool [compile *type* ?value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC "java/lang/Boolean" (if ?value "TRUE" "FALSE") "Ljava/lang/Boolean;")]]
    (return nil)))

(do-template [<name> <wrapper>]
  (defn <name> [compile *type* value]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [_ (doto *writer*
                    (.visitLdcInsn value)
                    (<wrapper>))]]
      (return nil)))

  compile-int  &&/wrap-long
  compile-real &&/wrap-double
  compile-char &&/wrap-char
  )

(defn compile-text [compile *type* ?value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn compile-prod [compile *type* left right]
  ;; (prn 'compile-prod (&type/show-type *type*)
  ;;      (&/adt->text left)
  ;;      (&/adt->text right))
  (|do [^MethodVisitor *writer* &/get-writer
        _ (array-of *writer* "java/lang/Object" 2)
        _ (store-at *writer* compile 0 left)
        ;; :let [_ (prn 'compile-prod (&type/show-type *type*) left right)]
        _ (store-at *writer* compile 1 right)]
    (return nil)))

(defn compile-sum [compile *type* ?tag ?value]
  ;; (prn 'compile-variant ?tag (class ?tag))
  (|do [^MethodVisitor *writer* &/get-writer
        _ (array-of *writer* "java/lang/Object" 2)
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/DUP)
                  (.visitLdcInsn (int 0))
                  (.visitLdcInsn (int ?tag))
                  (&&/wrap-int)
                  (.visitInsn Opcodes/AASTORE))]
        _ (store-at *writer* compile 1 ?value)]
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
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (str (&host/->module-class ?owner-class) "/" (&/normalize-name ?name)) &/datum-field "Ljava/lang/Object;")]]
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

(defn ^:private compile-def-type [compile current-class ?body def-type]
  (|do [^MethodVisitor **writer** &/get-writer]
    (|case def-type
      "type"
      (|do [:let [;; ?type* (&&type/->analysis ?type)
                  _ (doto **writer**
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
                      ;; (.visitInsn Opcodes/ACONST_NULL) ;; VVIN
                      (.visitInsn Opcodes/AASTORE) ;; V
                      )]
            ;; _ (compile ?type*)
            ;; :let [_ (.visitInsn **writer** Opcodes/AASTORE)]
            ]
        (return nil))

      "value"
      (|let [;; _ (prn '?body (aget ?body 0) (aget ?body 1 0))
             ?def-type (|case ?body
                         [(&a/$ann ?def-value ?type-expr) ?def-type]
                         ?type-expr

                         [?def-value ?def-type]
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

(defn compile-def [compile ?name ?body]
  (|do [=value-type (&a/expr-type ?body)
        :let [def-type (cond (&type/type= &type/Type =value-type)
                             "type"
                             
                             :else
                             "value")]
        ^ClassWriter *writer* &/get-writer
        module-name &/get-module-name
        :let [datum-sig "Ljava/lang/Object;"
              def-name (&/normalize-name ?name)
              current-class (str (&host/->module-class module-name) "/" def-name)
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                               current-class nil "java/lang/Object" (into-array [&&/function-class]))
                       (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/name-field "Ljava/lang/String;" nil ?name)
                           (doto (.visitEnd)))
                       (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/datum-field datum-sig nil nil)
                           (doto (.visitEnd)))
                       (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/meta-field datum-sig nil nil)
                           (doto (.visitEnd))))]
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
        :let [def-class (&&/load-class! class-loader (&host/->class-name current-class))]
        _ (&a-module/define module-name ?name (-> def-class (.getField &/meta-field) (.get nil)) =value-type)]
    (return nil)))

(defn compile-ann [compile *type* ?value-ex ?type-ex]
  (compile ?value-ex))

(defn compile-declare-macro [compile module name]
  (|do [_ (&a-module/declare-macro module name)]
    (return nil)))
