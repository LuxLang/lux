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

(defn ^:private stack-peek [^MethodVisitor writer]
  (doto writer
    (.visitInsn Opcodes/DUP)
    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_peek" "([Ljava/lang/Object;)Ljava/lang/Object;")))

(defn ^:private compile-pattern*
  "(-> MethodVisitor Case-Pattern (List Label) Int Label MethodVisitor)"
  [^MethodVisitor writer current-registers frame-registers total-registers-per-body bodies stack-depth $else pm]
  (|case pm
    (&o/$ExecPM _body-idx)
    (|case (&/|at _body-idx bodies)
      (&/$Some $body)
      (do (aset total-registers-per-body _body-idx current-registers)
        (doto writer
          (pop-alt-stack stack-depth)
          ;; (.visitFrame Opcodes/F_NEW
          ;;              (count current-registers) (to-array current-registers)
          ;;              0 (to-array []))
          (.visitJumpInsn Opcodes/GOTO $body)))

      (&/$None)
      (assert false))

    (&o/$PopPM)
    (doto writer
      (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_pop" "([Ljava/lang/Object;)[Ljava/lang/Object;"))

    (&o/$SeqPM (&o/$BindPM _var-id) _right-pm)
    (|let [current-registers* (conj current-registers "java/lang/Object")]
      (doto writer
        stack-peek
        (.visitVarInsn Opcodes/ASTORE _var-id)
        ;; (.visitFrame Opcodes/F_NEW
        ;;              (count current-registers*) (to-array current-registers*)
        ;;              0 (to-array []))
        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_pop" "([Ljava/lang/Object;)[Ljava/lang/Object;")
        (compile-pattern* current-registers* frame-registers total-registers-per-body bodies stack-depth $else _right-pm)))

    (&o/$BoolPM _value)
    (doto writer
      stack-peek
      &&/unwrap-boolean
      (.visitJumpInsn (if _value Opcodes/IFEQ Opcodes/IFNE) $else))

    (&o/$NatPM _value)
    (doto writer
      stack-peek
      &&/unwrap-long
      (.visitLdcInsn (long _value))
      (.visitInsn Opcodes/LCMP)
      (.visitJumpInsn Opcodes/IFNE $else))

    (&o/$IntPM _value)
    (doto writer
      stack-peek
      &&/unwrap-long
      (.visitLdcInsn (long _value))
      (.visitInsn Opcodes/LCMP)
      (.visitJumpInsn Opcodes/IFNE $else))

    (&o/$FracPM _value)
    (doto writer
      stack-peek
      &&/unwrap-long
      (.visitLdcInsn (long _value))
      (.visitInsn Opcodes/LCMP)
      (.visitJumpInsn Opcodes/IFNE $else))

    (&o/$RealPM _value)
    (doto writer
      stack-peek
      &&/unwrap-double
      (.visitLdcInsn (double _value))
      (.visitInsn Opcodes/DCMPL)
      (.visitJumpInsn Opcodes/IFNE $else))

    (&o/$CharPM _value)
    (doto writer
      stack-peek
      &&/unwrap-char
      (.visitLdcInsn _value)
      (.visitJumpInsn Opcodes/IF_ICMPNE $else))

    (&o/$TextPM _value)
    (doto writer
      stack-peek
      (.visitLdcInsn _value)
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z")
      (.visitJumpInsn Opcodes/IFEQ $else))

    (&o/$TuplePM _idx+)
    (|let [[_idx is-tail?] (|case _idx+
                             (&/$Left _idx)
                             (&/T [_idx false])

                             (&/$Right _idx)
                             (&/T [_idx true]))]
      (doto writer
        stack-peek
        (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
        (.visitLdcInsn (int _idx))
        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" (if is-tail? "product_getRight" "product_getLeft") "([Ljava/lang/Object;I)Ljava/lang/Object;")
        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_push" "([Ljava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
        ))

    (&o/$VariantPM _idx _total)
    (|let [$success (new Label)
           $fail (new Label)
           _ (doto writer
               stack-peek
               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
               (.visitLdcInsn (int _idx)))
           _ (if (= _idx (dec _total))
               (.visitLdcInsn writer "")
               (.visitInsn writer Opcodes/ACONST_NULL))]
      (doto writer
        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_get" "([Ljava/lang/Object;ILjava/lang/Object;)Ljava/lang/Object;")
        (.visitInsn Opcodes/DUP)
        (.visitJumpInsn Opcodes/IFNULL $fail)
        (.visitJumpInsn Opcodes/GOTO $success)
        (.visitLabel $fail)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO $else)
        (.visitLabel $success)
        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_push" "([Ljava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")))

    (&o/$SeqPM _left-pm _right-pm)
    (doto writer
      (compile-pattern* current-registers frame-registers total-registers-per-body bodies stack-depth $else _left-pm)
      (compile-pattern* current-registers frame-registers total-registers-per-body bodies stack-depth $else _right-pm))

    (&o/$AltPM _left-pm _right-pm)
    (|let [$alt-else (new Label)]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (compile-pattern* current-registers frame-registers total-registers-per-body bodies (inc stack-depth) $alt-else _left-pm)
        (.visitLabel $alt-else)
        (.visitInsn Opcodes/POP)
        (compile-pattern* current-registers frame-registers total-registers-per-body bodies stack-depth $else _right-pm)))

    (&o/$AltVariantPM _total _branches _?default)
    (|let [$default (new Label);; (|case _?default
           ;; (&/$Some _)
           ;; (new Label)

           ;; _
           ;; $else)
           $branches (make-array Label (alength _branches))
           _ (amap _branches
                   idx 
                   ret 
                   (aset $branches idx (if (aget _branches idx)
                                         (new Label)
                                         $default)))
           writer (doto writer
                    stack-peek
                    (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
                    (.visitLdcInsn (int 0))
                    (.visitInsn Opcodes/AALOAD)
                    &&/unwrap-int
                    ;; (.visitFrame Opcodes/F_NEW
                    ;;              (count frame-registers) (to-array frame-registers)
                    ;;              2 (to-array ["[Ljava/lang/Object;" Opcodes/INTEGER]))
                    (.visitTableSwitchInsn 0 (dec _total) $default $branches))
           writer (|case _?default
                    (&/$Some _default)
                    (doto writer
                      (.visitLabel $default)
                      ;; (.visitFrame Opcodes/F_NEW
                      ;;              (count frame-registers) (to-array frame-registers)
                      ;;              1 (to-array ["[Ljava/lang/Object;"]))
                      (compile-pattern* current-registers frame-registers total-registers-per-body bodies stack-depth $else _default))

                    _
                    (doto writer
                      (.visitLabel $default)
                      ;; (.visitFrame Opcodes/F_NEW
                      ;;              (count frame-registers) (to-array frame-registers)
                      ;;              1 (to-array ["[Ljava/lang/Object;"]))
                      (.visitJumpInsn Opcodes/GOTO $else)))
           _ (amap _branches
                   idx
                   _
                   (if-let [_branch (aget _branches idx)]
                     (do (doto writer
                           (.visitLabel (aget $branches idx))
                           ;; (.visitFrame Opcodes/F_NEW
                           ;;              (count frame-registers) (to-array frame-registers)
                           ;;              1 (to-array ["[Ljava/lang/Object;"]))
                           stack-peek
                           (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
                           (.visitLdcInsn (int idx)))
                       (if (= idx (dec _total))
                         (.visitLdcInsn writer "")
                         (.visitInsn writer Opcodes/ACONST_NULL))
                       (doto writer
                         (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_get" "([Ljava/lang/Object;ILjava/lang/Object;)Ljava/lang/Object;")
                         (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_push" "([Ljava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                         (compile-pattern* current-registers frame-registers total-registers-per-body bodies stack-depth $default _branch)))
                     nil)
                   )]
      writer)
    ))

(defn ^:private compile-pattern [^MethodVisitor writer current-registers frame-registers total-registers-per-body bodies pm $end]
  (|let [$else (new Label)]
    (doto writer
      (compile-pattern* current-registers frame-registers total-registers-per-body bodies 1 $else pm)
      (.visitLabel $else)
      ;; (.visitFrame Opcodes/F_NEW
      ;;              (count frame-registers) (to-array frame-registers)
      ;;              1 (to-array ["[Ljava/lang/Object;"]))
      (.visitInsn Opcodes/POP)
      (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_fail" "()V")
      (.visitInsn Opcodes/ACONST_NULL)
      ;; (.visitFrame Opcodes/F_NEW
      ;;              (count frame-registers) (to-array frame-registers)
      ;;              1 (to-array ["java/lang/Object"]))
      (.visitJumpInsn Opcodes/GOTO $end))))

(defn ^:private compile-bodies [^MethodVisitor writer compile frame-registers total-registers-per-body bodies-labels ?bodies $end]
  (&/map% (fn [label+registers+body]
            (|let [[_label [registers _body]] label+registers+body]
              (|do [:let [_ (doto writer
                              (.visitLabel _label)
                              ;; (.visitFrame Opcodes/F_NEW
                              ;;              (count registers) (to-array registers)
                              ;;              0 (to-array []))
                              )]
                    _ (&/with-frame-registers registers
                        (compile _body))
                    :let [_ (doto writer
                              (.visitJumpInsn Opcodes/GOTO $end))]]
                (return nil))))
          (&/zip2 bodies-labels
                  (&/zip2 (->> total-registers-per-body seq &/->list)
                          ?bodies))))

;; [Resources]
(defn compile-case [compile ?value ?pm ?bodies]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [$end (new Label)
              bodies-labels (&/|map (fn [_] (new Label)) ?bodies)
              total-registers-per-body (object-array (&/|length ?bodies))]
        _ (compile ?value)
        frame-registers &/get-frame-registers
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/ACONST_NULL)
                  (.visitInsn Opcodes/SWAP)
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_push" "([Ljava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;"))
              _ (compile-pattern *writer*
                                 frame-registers
                                 frame-registers
                                 total-registers-per-body
                                 bodies-labels
                                 ?pm
                                 $end)]
        _ (compile-bodies *writer* compile frame-registers total-registers-per-body bodies-labels ?bodies $end)
        :let [_ (.visitLabel *writer* $end)]]
    (return nil)))
