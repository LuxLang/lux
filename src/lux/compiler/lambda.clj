;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.lambda
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.host.generics :as &host-generics]
            [lux.analyser.base :as &a]
            (lux.compiler [base :as &&]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils]
(def ^:private clo-field-sig (&host-generics/->type-signature "java.lang.Object"))
(def ^:private lambda-return-sig (&host-generics/->type-signature "java.lang.Object"))
(def ^:private <init>-return "V")

(def ^:private lambda-impl-signature
  (str "("  clo-field-sig ")" lambda-return-sig))

(defn ^:private lambda-<init>-signature [env]
  (str "(" (&/fold str "" (&/|repeat (&/|length env) clo-field-sig)) ")"
       <init>-return))

(defn ^:private add-lambda-<init> [class class-name env]
  (doto (.visitMethod ^ClassWriter class Opcodes/ACC_PUBLIC "<init>" (lambda-<init>-signature env) nil nil)
    (.visitCode)
    (.visitVarInsn Opcodes/ALOAD 0)
    (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
    (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
          (.visitVarInsn Opcodes/ALOAD (inc ?captured-id))
          (.visitFieldInsn Opcodes/PUTFIELD class-name captured-name clo-field-sig))
        (->> (let [captured-name (str &&/closure-prefix ?captured-id)])
             (|case ?name+?captured
               [?name [_ (&a/$captured _ ?captured-id ?source)]])
             (doseq [?name+?captured (&/->seq env)])))
    (.visitInsn Opcodes/RETURN)
    (.visitMaxs 0 0)
    (.visitEnd)))

(defn ^:private add-lambda-apply [class class-name env]
  (doto (.visitMethod ^ClassWriter class Opcodes/ACC_PUBLIC "apply" &&/apply-signature nil nil)
    (.visitCode)
    (.visitVarInsn Opcodes/ALOAD 0)
    (.visitVarInsn Opcodes/ALOAD 1)
    (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "impl" lambda-impl-signature)
    (.visitInsn Opcodes/ARETURN)
    (.visitMaxs 0 0)
    (.visitEnd)))

(let [impl-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)]
  (defn ^:private add-lambda-impl [class compile impl-signature impl-body]
    (&/with-writer (doto (.visitMethod ^ClassWriter class impl-flags "impl" impl-signature nil nil)
                     (.visitCode))
      (|do [^MethodVisitor *writer* &/get-writer
            :let [$start (new Label)
                  $end (new Label)]
            ret (compile impl-body)
            :let [_ (doto *writer*
                      (.visitLabel $end)
                      (.visitInsn Opcodes/ARETURN)
                      (.visitMaxs 0 0)
                      (.visitEnd))]]
        (return ret)))))

(defn ^:private instance-closure [compile lambda-class closed-over init-signature]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/NEW lambda-class)
                  (.visitInsn Opcodes/DUP))]
        _ (&/map% (fn [?name+?captured]
                    (|case ?name+?captured
                      [?name [_ (&a/$captured _ _ ?source)]]
                      (compile ?source)))
                  closed-over)
        :let [_ (.visitMethodInsn *writer* Opcodes/INVOKESPECIAL lambda-class "<init>" init-signature)]]
    (return nil)))

;; [Exports]
(let [lambda-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
      datum-flags (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL)]
  (defn compile-lambda [compile ?scope ?env ?body]
    (|do [[file-name _ _] &/cursor
          :let [name (&host/location (&/|tail ?scope))
                class-name (str (&host/->module-class (&/|head ?scope)) "/" name)
                =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit &host/bytecode-version lambda-flags
                                 class-name nil "java/lang/Object" (into-array [&&/function-class]))
                         (-> (doto (.visitField datum-flags captured-name clo-field-sig nil nil)
                               (.visitEnd))
                             (->> (let [captured-name (str &&/closure-prefix ?captured-id)])
                                  (|case ?name+?captured
                                    [?name [_ (&a/$captured _ ?captured-id ?source)]])
                                  (doseq [?name+?captured (&/->seq ?env)])))
                         (.visitSource file-name nil)
                         (add-lambda-apply class-name ?env)
                         (add-lambda-<init> class-name ?env)
                         )]
          _ (add-lambda-impl =class compile lambda-impl-signature ?body)
          :let [_ (.visitEnd =class)]
          _ (&&/save-class! name (.toByteArray =class))]
      (instance-closure compile class-name ?env (lambda-<init>-signature ?env)))))
