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
            (lux [base :as & :refer [|do return* return fail fail* |case |let]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host]
                 [optimizer :as &o])
            [lux.host.generics :as &host-generics]
            [lux.analyser.base :as &a]
            (lux.compiler [base :as &&]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils]
(def ^:private field-sig (&host-generics/->type-signature "java.lang.Object"))
(def ^:private lambda-return-sig (&host-generics/->type-signature "java.lang.Object"))
(def ^:private <init>-return "V")
(def ^:private num-args-field "_num_args_")
(defn ^:private ^String reset-signature [function-class]
  (str "()" (&host-generics/->type-signature function-class)))

(defn ^:private ^MethodVisitor get-num-args! [^MethodVisitor method-writer class-name]
  (doto method-writer
    (.visitVarInsn Opcodes/ALOAD 0)
    (.visitFieldInsn Opcodes/GETFIELD class-name num-args-field "I")))

(defn ^:private ^MethodVisitor inc-int! [^MethodVisitor method-writer by]
  (doto method-writer
    (.visitLdcInsn (int by))
    (.visitInsn Opcodes/IADD)))

(defn ^:private ^MethodVisitor get-field! [^MethodVisitor method-writer class-name field-name]
  (doto method-writer
    (.visitVarInsn Opcodes/ALOAD 0)
    (.visitFieldInsn Opcodes/GETFIELD class-name field-name field-sig)))

(defn ^:private ^MethodVisitor put-field! [^MethodVisitor method-writer class-name field-name field-sig value-thunk]
  (doto method-writer
    (.visitVarInsn Opcodes/ALOAD 0)
    value-thunk
    (.visitFieldInsn Opcodes/PUTFIELD class-name field-name field-sig)))

(defn ^:private ^MethodVisitor fill-nulls! [^MethodVisitor method-writer amount]
  (doto method-writer
    (-> (.visitInsn Opcodes/ACONST_NULL)
        (->> (dotimes [_ amount])))))

(defn ^:private lambda-impl-signature [level]
  (str "("  (&/fold str "" (&/|repeat level field-sig)) ")" lambda-return-sig))

(defn ^:private lambda-<init>-signature [env level]
  (if (> level 1)
    (str "(" (&/fold str "" (&/|repeat (&/|length env) field-sig)) "I" (&/fold str "" (&/|repeat (dec level) field-sig)) ")"
         <init>-return)
    (str "(" (&/fold str "" (&/|repeat (&/|length env) field-sig)) ")"
         <init>-return)))

(defn ^:private add-lambda-<init> [class class-name level env]
  (let [closure-length (&/|length env)]
    (doto (.visitMethod ^ClassWriter class Opcodes/ACC_PUBLIC "<init>" (lambda-<init>-signature env level) nil nil)
      (.visitCode)
      ;; Do normal object initialization
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
      ;; Add all of the closure variables
      (-> (put-field! class-name (str &&/closure-prefix ?captured-id) field-sig #(.visitVarInsn % Opcodes/ALOAD (inc ?captured-id)))
          (->> (|let [[?name [_ (&o/$captured _ ?captured-id ?source)]] ?name+?captured])
               (doseq [?name+?captured (&/->seq env)])))
      (-> (doto (put-field! class-name num-args-field "I" #(.visitVarInsn % Opcodes/ILOAD (inc closure-length))) ;; Add the counter
            ;; Add all the partial arguments
            (-> (put-field! class-name (str &&/partial-prefix idx*) field-sig #(.visitVarInsn % Opcodes/ALOAD partial-register))
                (->> (|let [partial-register (+ (inc idx*) (inc closure-length))])
                     (dotimes [idx* (dec level)]))))
          (->> (when (> level 1))))
      ;; Finish
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))))

(let [impl-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)]
  (defn ^:private add-lambda-impl [class class-name compile level impl-body]
    (&/with-writer (doto (.visitMethod ^ClassWriter class impl-flags "impl" (lambda-impl-signature level) nil nil)
                     (.visitCode))
      (|do [^MethodVisitor *writer* &/get-writer
            ret (compile impl-body)
            :let [_ (doto *writer*
                      (.visitInsn Opcodes/ARETURN)
                      (.visitMaxs 0 0)
                      (.visitEnd))]]
        (return ret)))))

(defn ^:private instance-closure [compile lambda-class level closed-over]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/NEW lambda-class)
                  (.visitInsn Opcodes/DUP))]
        _ (&/map% (fn [?name+?captured]
                    (|case ?name+?captured
                      [?name [_ (&a/$captured _ _ ?source)]]
                      (compile ?source)))
                  closed-over)
        :let [_ (when (> level 1)
                  (doto *writer*
                    (.visitLdcInsn (int 0))
                    (fill-nulls! (dec level))))]
        :let [_ (.visitMethodInsn *writer* Opcodes/INVOKESPECIAL lambda-class "<init>" (lambda-<init>-signature closed-over level))]]
    (return nil)))

(defn ^:private add-lambda-reset [class-writer class-name level env]
  (if (> level 1)
    (doto (.visitMethod ^ClassWriter class-writer Opcodes/ACC_PUBLIC "reset" (reset-signature class-name) nil nil)
      (.visitCode)
      (.visitTypeInsn Opcodes/NEW class-name)
      (.visitInsn Opcodes/DUP)
      (-> (get-field! class-name (str &&/closure-prefix cidx))
          (->> (dotimes [cidx (&/|length env)])))
      (.visitLdcInsn (int 0))
      (fill-nulls! (dec level))
      (.visitMethodInsn Opcodes/INVOKESPECIAL class-name "<init>" (lambda-<init>-signature env level))
      (.visitInsn Opcodes/ARETURN)
      (.visitMaxs 0 0)
      (.visitEnd))
    (doto (.visitMethod ^ClassWriter class-writer Opcodes/ACC_PUBLIC "reset" (reset-signature class-name) nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitInsn Opcodes/ARETURN)
      (.visitMaxs 0 0)
      (.visitEnd))))

(defn ^:private add-lambda-apply [class-writer class-name level env]
  (if (> level 1)
    (let [$default (new Label)
          $labels* (map (fn [_] (new Label)) (repeat (dec level) nil))
          $labels (vec (concat $labels* (list $default)))
          $end (new Label)
          method-writer (.visitMethod ^ClassWriter class-writer Opcodes/ACC_PUBLIC "apply" &&/apply-signature nil nil)]
      (doto method-writer
        (.visitCode)
        (get-num-args! class-name)
        (.visitFrame Opcodes/F_NEW
                     (int 2)
                     (to-array (list class-name "java/lang/Object"))
                     (int 1)
                     (to-array [Opcodes/INTEGER]))
        (.visitTableSwitchInsn 0 (- level 2) $default (into-array $labels*))
        (-> (doto (.visitLabel $label)
              (.visitFrame Opcodes/F_NEW
                           (int 2)
                           (to-array (list class-name "java/lang/Object"))
                           (int 0)
                           (to-array []))
              (.visitTypeInsn Opcodes/NEW class-name)
              (.visitInsn Opcodes/DUP)
              (-> (get-field! class-name (str &&/closure-prefix cidx))
                  (->> (dotimes [cidx (&/|length env)])))
              (get-num-args! class-name)
              (inc-int! 1)
              (-> (get-field! class-name (str &&/partial-prefix idx))
                  (->> (dotimes [idx stage])))
              (.visitVarInsn Opcodes/ALOAD 1)
              (fill-nulls! (dec (- (dec level) stage)))
              (.visitMethodInsn Opcodes/INVOKESPECIAL class-name "<init>" (lambda-<init>-signature env level))
              (.visitJumpInsn Opcodes/GOTO $end))
            (->> (cond (= stage (dec level))
                       (doto method-writer
                         (.visitLabel $label)
                         (.visitFrame Opcodes/F_NEW
                                      (int 2)
                                      (to-array (list class-name "java/lang/Object"))
                                      (int 0)
                                      (to-array []))
                         (.visitVarInsn Opcodes/ALOAD 0)
                         (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "reset" (reset-signature class-name))
                         (-> (get-field! class-name (str &&/partial-prefix idx))
                             (->> (dotimes [idx stage])))
                         (.visitVarInsn Opcodes/ALOAD 1)
                         (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "impl" (lambda-impl-signature level))
                         (.visitJumpInsn Opcodes/GOTO $end))

                       :else)
                 (doseq [[stage $label] (map vector (range level) $labels)])))
        (.visitLabel $end)
        (.visitInsn Opcodes/ARETURN)
        (.visitMaxs 0 0)
        (.visitEnd)))
    (doto (.visitMethod ^ClassWriter class-writer Opcodes/ACC_PUBLIC "apply" &&/apply-signature nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitVarInsn Opcodes/ALOAD 1)
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "impl" (lambda-impl-signature level))
      (.visitInsn Opcodes/ARETURN)
      (.visitMaxs 0 0)
      (.visitEnd))))

;; [Exports]
(let [lambda-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
      datum-flags (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL)]
  (defn compile-function [compile level ?scope ?env ?body]
    (|do [[file-name _ _] &/cursor
          :let [name (&host/location (&/|tail ?scope))
                class-name (str (&host/->module-class (&/|head ?scope)) "/" name)
                =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit &host/bytecode-version lambda-flags
                                 class-name nil "java/lang/Object" (into-array [&&/function-class]))
                         (-> (doto (.visitField datum-flags captured-name field-sig nil nil)
                               (.visitEnd))
                             (->> (let [captured-name (str &&/closure-prefix ?captured-id)])
                                  (|case ?name+?captured
                                    [?name [_ (&o/$captured _ ?captured-id ?source)]])
                                  (doseq [?name+?captured (&/->seq ?env)])))
                         (-> (doto (-> (.visitField datum-flags num-args-field "I" nil nil)
                                       (doto (.visitEnd)))
                               (-> (.visitField datum-flags (str &&/partial-prefix idx) field-sig nil nil)
                                   (doto (.visitEnd))
                                   (->> (dotimes [idx (dec level)]))))
                             (->> (when (> level 1))))
                         (.visitSource file-name nil)
                         (add-lambda-reset class-name level ?env)
                         (add-lambda-apply class-name level ?env)
                         (add-lambda-<init> class-name level ?env)
                         )]
          _ (add-lambda-impl =class class-name compile level ?body)
          :let [_ (.visitEnd =class)]
          _ (&&/save-class! name (.toByteArray =class))]
      (instance-closure compile class-name level ?env))))
