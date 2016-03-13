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
                          [module :as &a-module]
                          [meta :as &a-meta])
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
    (|case num-elems
      0
      (|do [:let [_ (.visitLdcInsn *writer* &/unit-tag)]]
        (return nil))

      1
      (compile (&/|head ?elems))
      
      _
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

(defn compile-variant [compile tag tail? value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitLdcInsn *writer* (int tag))
              _ (if tail?
                  (.visitLdcInsn *writer* "")
                  (.visitInsn *writer* Opcodes/ACONST_NULL))]
        _ (compile value)
        :let [_ (.visitMethodInsn *writer* Opcodes/INVOKESTATIC "lux/LuxUtils" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")]]
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
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (str (&host/->module-class ?owner-class) "/" (&host/def-name ?name)) &/value-field "Ljava/lang/Object;")]]
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

(defn ^:private compile-def-type [compile ?body]
  (|do [:let [?def-type (|case ?body
                          [[?def-type ?def-cursor] (&a/$ann ?def-value ?type-expr ?def-value-type)]
                          ?type-expr

                          [[?def-type ?def-cursor] ?def-value]
                          (if (&type/type= &type/Type ?def-type)
                            (&/T [(&/T [?def-type ?def-cursor])
                                  (&a/$tuple (&/|list))])
                            (&&type/type->analysis ?def-type)))]]
    (compile ?def-type)))

(defn ^:private compile-def-meta [compile ?meta]
  (|let [analysis (&&type/defmeta->analysis ?meta)]
    (compile analysis)))

(let [class-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
      field-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC)]
  (defn compile-def [compile ?name ?body ?meta]
    (|do [module-name &/get-module-name
          class-loader &/loader]
      (|case (&a-meta/meta-get &a-meta/alias-tag ?meta)
        (&/$Some (&/$IdentM [r-module r-name]))
        (if (= 1 (&/|length ?meta))
          (|do [:let [current-class (&host-generics/->class-name (str (&host/->module-class r-module) "/" (&host/def-name r-name)))
                      def-class (&&/load-class! class-loader current-class)
                      def-type (-> def-class (.getField &/type-field) (.get nil))
                      def-meta ?meta
                      def-value (-> def-class (.getField &/value-field) (.get nil))]
                _ (&a-module/define module-name ?name def-type def-meta def-value)]
            (return nil))
          (fail (str "[Compilation Error] Aliases cannot contain meta-data: " module-name ";" ?name)))

        (&/$Some _)
        (fail "[Compilation Error] Invalid syntax for lux;alias meta-data. Must be an Ident.")
        
        _
        (|do [:let [=value-type (&a/expr-type* ?body)]
              ;; ^ClassWriter *writer* &/get-writer
              [file-name _ _] &/cursor
              :let [datum-sig "Ljava/lang/Object;"
                    def-name (&host/def-name ?name)
                    current-class (str (&host/->module-class module-name) "/" def-name)
                    =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                             (.visit &host/bytecode-version class-flags
                                     current-class nil "java/lang/Object" (into-array [&&/function-class]))
                             (-> (.visitField field-flags &/name-field "Ljava/lang/String;" nil ?name)
                                 (doto (.visitEnd)))
                             (-> (.visitField field-flags &/type-field datum-sig nil nil)
                                 (doto (.visitEnd)))
                             (-> (.visitField field-flags &/meta-field datum-sig nil nil)
                                 (doto (.visitEnd)))
                             (-> (.visitField field-flags &/value-field datum-sig nil nil)
                                 (doto (.visitEnd)))
                             (.visitSource file-name nil))]
              _ (&/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
                  (|do [^MethodVisitor **writer** &/get-writer
                        :let [_ (.visitCode **writer**)]
                        _ (compile-def-type compile ?body)
                        :let [_ (.visitFieldInsn **writer** Opcodes/PUTSTATIC current-class &/type-field datum-sig)]
                        _ (compile-def-meta compile ?meta)
                        :let [_ (.visitFieldInsn **writer** Opcodes/PUTSTATIC current-class &/meta-field datum-sig)]
                        _ (compile ?body)
                        :let [_ (.visitFieldInsn **writer** Opcodes/PUTSTATIC current-class &/value-field datum-sig)]
                        :let [_ (doto **writer**
                                  (.visitInsn Opcodes/RETURN)
                                  (.visitMaxs 0 0)
                                  (.visitEnd))]]
                    (return nil)))
              ;; :let [_ (.visitEnd *writer*)]
              :let [_ (.visitEnd =class)]
              _ (&&/save-class! def-name (.toByteArray =class))
              :let [def-class (&&/load-class! class-loader (&host-generics/->class-name current-class))
                    [def-type is-type?] (|case (&a-meta/meta-get &a-meta/type?-tag ?meta)
                                          (&/$Some (&/$BoolM true))
                                          (&/T [&type/Type
                                                true])

                                          _
                                          (if (&type/type= &type/Type =value-type)
                                            (&/T [&type/Type
                                                  false])
                                            (&/T [(-> def-class (.getField &/type-field) (.get nil))
                                                  false])))
                    def-meta ?meta
                    def-value (-> def-class (.getField &/value-field) (.get nil))]
              _ (&a-module/define module-name ?name def-type def-meta def-value)
              _ (|case (&/T [is-type? (&a-meta/meta-get &a-meta/tags-tag def-meta)])
                  [true (&/$Some (&/$ListM tags*))]
                  (|do [:let [was-exported? (|case (&a-meta/meta-get &a-meta/export?-tag def-meta)
                                              (&/$Some _)
                                              true

                                              _
                                              false)]
                        tags (&/map% (fn [tag*]
                                       (|case tag*
                                         (&/$TextM tag)
                                         (return tag)

                                         _
                                         (fail "[Compiler Error] Incorrect format for tags.")))
                                     tags*)
                        _ (&a-module/declare-tags module-name tags was-exported? def-value)]
                    (return nil))

                  [false (&/$Some _)]
                  (fail "[Compiler Error] Can't define tags for non-type.")

                  [true (&/$Some _)]
                  (fail "[Compiler Error] Incorrect format for tags.")

                  [_ (&/$None)]
                  (return nil))
              :let [_ (println 'DEF (str module-name ";" ?name))]]
          (return nil))))))

(defn compile-ann [compile ?value-ex ?type-ex ?value-type]
  (compile ?value-ex))

(defn compile-coerce [compile ?value-ex ?type-ex ?value-type]
  (compile ?value-ex))
