;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.case
  (:require (clojure [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host]
                 [optimizer :as &o])
            [lux.analyser.case :as &a-case]
            [lux.compiler.base :as &&])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils]
(defn ^:private pop-alt-stack [^MethodVisitor writer stack-depth]
  (cond (= 0 stack-depth)
        writer

        (= 1 stack-depth)
        (doto writer
          (.visitInsn Opcodes/POP))
        
        (= 2 stack-depth)
        (doto writer
          (.visitInsn Opcodes/POP2))
        
        :else ;; > 2
        (doto writer
          (.visitInsn Opcodes/POP2)
          (pop-alt-stack (- stack-depth 2)))))

(defn ^:private add-jump-frame [^MethodVisitor writer func-class-name arity stack-size]
  writer
  ;; (if (= 0 arity)
  ;;   (doto writer
  ;;     (.visitFrame Opcodes/F_NEW
  ;;                  (int 0) (to-array [])
  ;;                  (int stack-size) (to-array (repeat stack-size "java/lang/Object"))))
  ;;   (doto writer
  ;;     (.visitFrame Opcodes/F_NEW
  ;;                  (int (inc arity)) (to-array (cons func-class-name (repeat arity "java/lang/Object")))
  ;;                  (int stack-size) (to-array (repeat stack-size "java/lang/Object")))))
  )

(defn ^:private compile-pattern* [^MethodVisitor writer in-tuple? func-class-name arity stack-size bodies stack-depth $else pm]
  "(-> MethodVisitor Case-Pattern (List Label) stack-depth Label MethodVisitor)"
  (|case pm
    (&o/$AltPM _left-pm _right-pm)
    (|let [$alt-else (new Label)]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (compile-pattern* in-tuple? func-class-name arity (inc stack-size) bodies (inc stack-depth) $alt-else _left-pm)
        (.visitLabel $alt-else)
        (compile-pattern* in-tuple? func-class-name arity stack-size bodies stack-depth $else _right-pm)))

    (&o/$ExecPM _body-idx)
    (|case (&/|at _body-idx bodies)
      (&/$Some $body)
      (doto writer
        (pop-alt-stack stack-depth)
        (.visitJumpInsn Opcodes/GOTO $body))

      (&/$None)
      (assert false))

    (&o/$BindPM _var-id _next-pm)
    (doto writer
      (.visitVarInsn Opcodes/ASTORE _var-id)
      (compile-pattern* in-tuple? func-class-name (inc arity) stack-size bodies stack-depth $else _next-pm))

    (&o/$BoolPM _value _next-pm)
    (doto writer
      (.visitTypeInsn Opcodes/CHECKCAST "java/lang/Boolean")
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Boolean" "booleanValue" "()Z")
      (.visitLdcInsn _value)
      (.visitJumpInsn Opcodes/IF_ICMPNE $else)
      (compile-pattern* in-tuple? func-class-name arity stack-size bodies stack-depth $else _next-pm))

    (&o/$IntPM _value _next-pm)
    (doto writer
      (.visitTypeInsn Opcodes/CHECKCAST "java/lang/Long")
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Long" "longValue" "()J")
      (.visitLdcInsn (long _value))
      (.visitInsn Opcodes/LCMP)
      (.visitJumpInsn Opcodes/IFNE $else)
      (compile-pattern* in-tuple? func-class-name arity stack-size bodies stack-depth $else _next-pm))

    (&o/$RealPM _value _next-pm)
    (doto writer
      (.visitTypeInsn Opcodes/CHECKCAST "java/lang/Double")
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Double" "doubleValue" "()D")
      (.visitLdcInsn (double _value))
      (.visitInsn Opcodes/DCMPL)
      (.visitJumpInsn Opcodes/IFNE $else)
      (compile-pattern* in-tuple? func-class-name arity stack-size bodies stack-depth $else _next-pm))

    (&o/$CharPM _value _next-pm)
    (doto writer
      (.visitTypeInsn Opcodes/CHECKCAST "java/lang/Character")
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Character" "charValue" "()C")
      (.visitLdcInsn _value)
      (.visitJumpInsn Opcodes/IF_ICMPNE $else)
      (compile-pattern* in-tuple? func-class-name arity stack-size bodies stack-depth $else _next-pm))

    (&o/$TextPM _value _next-pm)
    (doto writer
      (.visitLdcInsn _value)
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z")
      (.visitJumpInsn Opcodes/IFEQ $else)
      (compile-pattern* in-tuple? func-class-name arity stack-size bodies stack-depth $else _next-pm))

    (&o/$UnitPM _next-pm)
    (doto writer
      (.visitInsn Opcodes/POP)
      (compile-pattern* in-tuple? func-class-name arity stack-size bodies stack-depth $else _next-pm))

    (&o/$InnerPM _next-pm)
    (doto writer
      (.visitInsn Opcodes/POP)
      (compile-pattern* false func-class-name arity stack-size bodies stack-depth $else _next-pm))

    ;; (&o/$TuplePM _idx+ _next-pm)
    ;; (|let [$tuple-else (new Label)
    ;;        [_idx is-tail?] (|case _idx+
    ;;                          (&/$Left _idx)
    ;;                          (&/T [_idx false])

    ;;                          (&/$Right _idx)
    ;;                          (&/T [_idx true]))
    ;;        _ (prn 'is-tail? is-tail?)]
    ;;   (doto writer
    ;;     (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
    ;;     (.visitInsn Opcodes/DUP)
    ;;     (.visitLdcInsn (int _idx))
    ;;     (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxUtils" (if is-tail? "product_getRight" "product_getLeft") "([Ljava/lang/Object;I)Ljava/lang/Object;")
    ;;     ;; (compile-pattern* in-tuple? func-class-name arity (inc stack-size) bodies stack-depth $else _next-pm)
    ;;     (compile-pattern* true func-class-name arity (inc stack-size) bodies stack-depth (if is-tail?
    ;;                                                                                        $tuple-else
    ;;                                                                                        $else) _next-pm)
    ;;     (-> (doto (.visitLabel $tuple-else)
    ;;           ;; (add-jump-frame func-class-name arity stack-size)
    ;;           (.visitInsn Opcodes/POP)
    ;;           (.visitJumpInsn Opcodes/GOTO $else))
    ;;         (->> (when is-tail?)))
    ;;     ))

    (&o/$TuplePM _next-pm)
    (|let [$tuple-else (new Label)]
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
        (compile-pattern* true func-class-name arity (inc stack-size) bodies stack-depth $tuple-else _next-pm)
        (.visitLabel $tuple-else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $else)
        ))

    (&o/$SeqPM _idx+ _next-pm)
    (|let [$tuple-else (new Label)
           [_idx is-tail?] (|case _idx+
                             (&/$Left _idx)
                             (&/T [_idx false])

                             (&/$Right _idx)
                             (&/T [_idx true]))]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (.visitLdcInsn (int _idx))
        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxUtils" (if is-tail? "product_getRight" "product_getLeft") "([Ljava/lang/Object;I)Ljava/lang/Object;")
        (compile-pattern* true func-class-name arity (inc stack-size) bodies stack-depth $else _next-pm)
        ))

    (&o/$VariantPM _idx+ _next-pm)
    (|let [;; _ (prn 'IN-VARIANT arity stack-size)
           $variant-else (new Label)
           [_idx is-last] (|case _idx+
                            (&/$Left _idx)
                            (&/T [_idx false])

                            (&/$Right _idx)
                            (&/T [_idx true]))
           _ (doto writer
               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
               (.visitLdcInsn (int _idx)))
           _ (if is-last
               (.visitLdcInsn writer "")
               (.visitInsn writer Opcodes/ACONST_NULL))]
      (doto writer
        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxUtils" "sum_get" "([Ljava/lang/Object;ILjava/lang/Object;)Ljava/lang/Object;")
        ;; (add-jump-frame func-class-name arity stack-size)
        (.visitInsn Opcodes/DUP)
        (.visitInsn Opcodes/ACONST_NULL)
        ;; (add-jump-frame func-class-name arity (+ 2 stack-size))
        (.visitJumpInsn Opcodes/IF_ACMPEQ $variant-else)
        (compile-pattern* in-tuple? func-class-name arity stack-size bodies stack-depth $else _next-pm)
        (.visitLabel $variant-else)
        ;; (add-jump-frame func-class-name arity stack-size)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $else)))
    ))

(defn ^:private compile-pattern [^MethodVisitor writer func-class-name arity bodies pm]
  ;; (compile-pattern* writer false func-class-name arity 1 bodies 0 nil pm)
  (|let [$else (new Label)]
    (doto writer
      (compile-pattern* false func-class-name arity 1 bodies 0 $else pm)
      (.visitLabel $else)
      (.visitInsn Opcodes/POP)
      (.visitTypeInsn Opcodes/NEW "java/lang/IllegalStateException")
      (.visitInsn Opcodes/DUP)
      (.visitLdcInsn "Invalid expression for pattern-matching.")
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/IllegalStateException" "<init>" "(Ljava/lang/String;)V")
      (.visitInsn Opcodes/ATHROW)))
  )

(defn ^:private compile-bodies [^MethodVisitor writer compile bodies-labels ?bodies $end]
  (&/map% (fn [label+body]
            (|let [[_label _body] label+body]
              (|do [:let [_ (.visitLabel writer _label)]
                    _ (compile _body)
                    :let [_ (.visitJumpInsn writer Opcodes/GOTO $end)]]
                (return nil))))
          (&/zip2 bodies-labels ?bodies)))

;; [Resources]
(defn compile-case [compile func-class-name+arity ?value ?pm ?bodies]
  (|do [:let [[func-class-name arity] func-class-name+arity]
        ^MethodVisitor *writer* &/get-writer
        :let [$end (new Label)
              bodies-labels (&/|map (fn [_] (new Label)) ?bodies)]
        _ (compile ?value)
        :let [_ (prn 'compile-pattern* (&/adt->text ?pm))
              _ (compile-pattern *writer* func-class-name arity bodies-labels ?pm)]
        _ (compile-bodies *writer* compile bodies-labels ?bodies $end)
        :let [_ (.visitLabel *writer* $end)]]
    (return nil)))
