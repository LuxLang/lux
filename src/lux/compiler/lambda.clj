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

(defn ^:private ^String reset-signature [function-class]
  (str "()" (&host-generics/->type-signature function-class)))

(defn ^:private ^MethodVisitor get-num-partials! [^MethodVisitor method-writer]
  (doto method-writer
    (.visitVarInsn Opcodes/ALOAD 0)
    (.visitFieldInsn Opcodes/GETFIELD &&/function-class &&/partials-field "I")))

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

(defn ^:private ^MethodVisitor consecutive-args [^MethodVisitor method-writer start amount]
  (doto method-writer
    (-> (.visitVarInsn Opcodes/ALOAD (+ start idx))
        (->> (dotimes [idx amount])))))

(defn ^:private ^MethodVisitor consecutive-applys [^MethodVisitor method-writer start amount]
  (let [max-args-num (min amount &&/num-apply-variants)]
    (doto method-writer
      (.visitTypeInsn Opcodes/CHECKCAST &&/function-class)
      (consecutive-args start max-args-num)
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature max-args-num))
      (-> (consecutive-applys (+ start &&/num-apply-variants) (- amount &&/num-apply-variants))
          (->> (when (> amount &&/num-apply-variants)))))))

(defn ^:private lambda-impl-signature [arity]
  (str "(" (&/fold str "" (&/|repeat arity field-sig)) ")" lambda-return-sig))

(defn ^:private lambda-<init>-signature [env arity]
  (if (> arity 1)
    (str "(" (&/fold str "" (&/|repeat (&/|length env) field-sig)) "I" (&/fold str "" (&/|repeat (dec arity) field-sig)) ")"
         <init>-return)
    (str "(" (&/fold str "" (&/|repeat (&/|length env) field-sig)) ")"
         <init>-return)))

(defn ^:private init-function [method-writer arity closure-length]
  (if (= 1 arity)
    (doto method-writer
      (.visitLdcInsn (int 0))
      (.visitMethodInsn Opcodes/INVOKESPECIAL &&/function-class "<init>" "(I)V"))
    (doto method-writer
      (.visitVarInsn Opcodes/ILOAD (inc closure-length))
      (.visitMethodInsn Opcodes/INVOKESPECIAL &&/function-class "<init>" "(I)V"))))

(defn ^:private add-lambda-<init> [class class-name arity env]
  (let [closure-length (&/|length env)]
    (doto (.visitMethod ^ClassWriter class Opcodes/ACC_PUBLIC "<init>" (lambda-<init>-signature env arity) nil nil)
      (.visitCode)
      ;; Do normal object initialization
      (.visitVarInsn Opcodes/ALOAD 0)
      (init-function arity closure-length)
      ;; Add all of the closure variables
      (-> (put-field! class-name (str &&/closure-prefix ?captured-id) field-sig #(.visitVarInsn % Opcodes/ALOAD (inc ?captured-id)))
          (->> (|let [[?name [_ (&o/$captured _ ?captured-id ?source)]] ?name+?captured])
               (doseq [?name+?captured (&/->seq env)])))
      ;; Add all the partial arguments
      (-> (put-field! class-name (str &&/partial-prefix idx*) field-sig #(.visitVarInsn % Opcodes/ALOAD partial-register))
          (->> (|let [partial-register (+ (inc idx*) (inc closure-length))])
               (dotimes [idx* (dec arity)])))
      ;; Finish
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))))

(let [impl-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)]
  (defn ^:private add-lambda-impl [class class-name compile arity impl-body]
    (let [$begin (new Label)]
      (&/with-writer (doto (.visitMethod ^ClassWriter class impl-flags "impl" (lambda-impl-signature arity) nil nil)
                       (.visitCode)
                       (.visitLabel $begin))
        (|do [^MethodVisitor *writer* &/get-writer
              ret (compile $begin impl-body)
              :let [_ (doto *writer*
                        (.visitInsn Opcodes/ARETURN)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return ret))))))

(defn ^:private instance-closure [compile lambda-class arity closed-over]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/NEW lambda-class)
                  (.visitInsn Opcodes/DUP))]
        _ (&/map% (fn [?name+?captured]
                    (|case ?name+?captured
                      [?name [_ (&o/$captured _ _ ?source)]]
                      (compile nil ?source)))
                  closed-over)
        :let [_ (when (> arity 1)
                  (doto *writer*
                    (.visitLdcInsn (int 0))
                    (fill-nulls! (dec arity))))]
        :let [_ (.visitMethodInsn *writer* Opcodes/INVOKESPECIAL lambda-class "<init>" (lambda-<init>-signature closed-over arity))]]
    (return nil)))

(defn ^:private add-lambda-reset [class-writer class-name arity env]
  (if (> arity 1)
    (doto (.visitMethod ^ClassWriter class-writer Opcodes/ACC_PUBLIC "reset" (reset-signature class-name) nil nil)
      (.visitCode)
      (.visitTypeInsn Opcodes/NEW class-name)
      (.visitInsn Opcodes/DUP)
      (-> (get-field! class-name (str &&/closure-prefix cidx))
          (->> (dotimes [cidx (&/|length env)])))
      (.visitLdcInsn (int 0))
      (fill-nulls! (dec arity))
      (.visitMethodInsn Opcodes/INVOKESPECIAL class-name "<init>" (lambda-<init>-signature env arity))
      (.visitInsn Opcodes/ARETURN)
      (.visitMaxs 0 0)
      (.visitEnd))
    (doto (.visitMethod ^ClassWriter class-writer Opcodes/ACC_PUBLIC "reset" (reset-signature class-name) nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitInsn Opcodes/ARETURN)
      (.visitMaxs 0 0)
      (.visitEnd))))

(defn ^:private add-lambda-apply-n [class-writer +degree+ class-name arity env compile impl-body]
  (if (> arity 1)
    (let [num-partials (dec arity)
          $default (new Label)
          $labels* (map (fn [_] (new Label)) (repeat num-partials nil))
          $labels (vec (concat $labels* (list $default)))
          $end (new Label)
          method-writer (.visitMethod ^ClassWriter class-writer Opcodes/ACC_PUBLIC &&/apply-method (&&/apply-signature +degree+) nil nil)
          frame-locals (to-array (list class-name "java/lang/Object" "java/lang/Object"))
          frame-stack (to-array [Opcodes/INTEGER])
          arity-over-extent (- arity +degree+)]
      (do (doto method-writer
            (.visitCode)
            get-num-partials!
            (.visitTableSwitchInsn 0 (dec num-partials) $default (into-array Label $labels*))
            ;; (< stage (- arity +degree+))
            (-> (doto (.visitLabel $label)
                  (.visitTypeInsn Opcodes/NEW class-name)
                  (.visitInsn Opcodes/DUP)
                  (-> (get-field! class-name (str &&/closure-prefix cidx))
                      (->> (dotimes [cidx (&/|length env)])))
                  get-num-partials!
                  (inc-int! +degree+)
                  (-> (get-field! class-name (str &&/partial-prefix idx))
                      (->> (dotimes [idx stage])))
                  (consecutive-args 1 +degree+)
                  (fill-nulls! (- (- num-partials +degree+) stage))
                  (.visitMethodInsn Opcodes/INVOKESPECIAL class-name "<init>" (lambda-<init>-signature env arity))
                  (.visitJumpInsn Opcodes/GOTO $end))
                (->> (cond (= stage arity-over-extent)
                           (doto method-writer
                             (.visitLabel $label)
                             (.visitVarInsn Opcodes/ALOAD 0)
                             (-> (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "reset" (reset-signature class-name))
                                 (->> (when (not= 0 stage))))
                             (-> (get-field! class-name (str &&/partial-prefix idx))
                                 (->> (dotimes [idx stage])))
                             (consecutive-args 1 +degree+)
                             (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "impl" (lambda-impl-signature arity))
                             (.visitJumpInsn Opcodes/GOTO $end))

                           (> stage arity-over-extent)
                           (let [args-to-completion (- arity stage)
                                 args-left (- +degree+ args-to-completion)]
                             (doto method-writer
                               (.visitLabel $label)
                               (.visitVarInsn Opcodes/ALOAD 0)
                               (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "reset" (reset-signature class-name))
                               (-> (get-field! class-name (str &&/partial-prefix idx))
                                   (->> (dotimes [idx stage])))
                               (consecutive-args 1 args-to-completion)
                               (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "impl" (lambda-impl-signature arity))
                               (consecutive-applys (+ 1 args-to-completion) args-left)
                               (.visitJumpInsn Opcodes/GOTO $end)))

                           :else)
                     (doseq [[stage $label] (map vector (range arity) $labels)])))
            (.visitLabel $end)
            (.visitInsn Opcodes/ARETURN)
            (.visitMaxs 0 0)
            (.visitEnd))
        (return nil)))
    (let [$begin (new Label)]
      (&/with-writer (doto (.visitMethod ^ClassWriter class-writer Opcodes/ACC_PUBLIC &&/apply-method (&&/apply-signature 1) nil nil)
                       (.visitCode)
                       (.visitLabel $begin))
        (|do [^MethodVisitor *writer* &/get-writer
              ret (compile $begin impl-body)
              :let [_ (doto *writer*
                        (.visitInsn Opcodes/ARETURN)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return ret))))
    ))

;; [Exports]
(let [lambda-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
      datum-flags (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL)]
  (defn compile-function [compile ?prev-writer arity ?scope ?env ?body]
    (|do [[file-name _ _] &/cursor
          :let [??scope (&/|reverse ?scope)
                name (&host/location (&/|tail ??scope))
                class-name (str (&host/->module-class (&/|head ??scope)) "/" name)
                [=class save?] (|case ?prev-writer
                                 (&/$Some _writer)
                                 (&/T [_writer false])

                                 (&/$None)
                                 (&/T [(doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                                         (.visit &host/bytecode-version lambda-flags
                                                 class-name nil &&/function-class (into-array String [])))
                                       true]))
                _ (doto =class
                    (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_FINAL) &&/arity-field "I" nil (int arity))
                        (doto (.visitEnd)))
                    (-> (doto (.visitField datum-flags captured-name field-sig nil nil)
                          (.visitEnd))
                        (->> (let [captured-name (str &&/closure-prefix ?captured-id)])
                             (|case ?name+?captured
                               [?name [_ (&o/$captured _ ?captured-id ?source)]])
                             (doseq [?name+?captured (&/->seq ?env)])))
                    (-> (.visitField datum-flags (str &&/partial-prefix idx) field-sig nil nil)
                        (doto (.visitEnd))
                        (->> (dotimes [idx (dec arity)])))
                    (-> (.visitSource file-name nil)
                        (when save?))
                    (add-lambda-<init> class-name arity ?env)
                    (add-lambda-reset class-name arity ?env)
                    )]
          _ (if (> arity 1)
              (add-lambda-impl =class class-name compile arity ?body)
              (return nil))
          _ (&/map% #(add-lambda-apply-n =class % class-name arity ?env compile ?body)
                    (&/|range* 1 (min arity &&/num-apply-variants)))
          :let [_ (.visitEnd =class)]
          _ (if save?
              (&&/save-class! name (.toByteArray =class))
              (return nil))]
      (if save?
        (instance-closure compile class-name arity ?env)
        (return (instance-closure compile class-name arity ?env))))))
