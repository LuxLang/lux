(ns lux.compiler.jvm.case
  (:require (clojure [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host]
                 [optimizer :as &o])
            [lux.analyser.case :as &a-case]
            [lux.compiler.jvm.base :as &&])
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

(defn ^:private compile-pattern* [^MethodVisitor writer bodies stack-depth $else pm]
  "(-> MethodVisitor Case-Pattern (List Label) Int Label MethodVisitor)"
  (|case pm
    (&o/$ExecPM _body-idx)
    (|case (&/|at _body-idx bodies)
      (&/$Some $body)
      (doto writer
        (pop-alt-stack stack-depth)
        (.visitJumpInsn Opcodes/GOTO $body))

      (&/$None)
      (assert false))

    (&o/$PopPM)
    (doto writer
      (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_pop" "([Ljava/lang/Object;)[Ljava/lang/Object;"))

    (&o/$BindPM _var-id)
    (doto writer
      stack-peek
      (.visitVarInsn Opcodes/ASTORE _var-id)
      (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_pop" "([Ljava/lang/Object;)[Ljava/lang/Object;"))

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

    (&o/$DegPM _value)
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
      (if (= 0 _idx)
        (doto writer
          stack-peek
          (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
          (.visitLdcInsn (int 0))
          (.visitInsn Opcodes/AALOAD)
          (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_push" "([Ljava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;"))
        (doto writer
          stack-peek
          (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
          (.visitLdcInsn (int _idx))
          (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" (if is-tail? "product_getRight" "product_getLeft") "([Ljava/lang/Object;I)Ljava/lang/Object;")
          (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_push" "([Ljava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
          )))

    (&o/$VariantPM _idx+)
    (|let [$success (new Label)
           $fail (new Label)
           [_idx is-last] (|case _idx+
                            (&/$Left _idx)
                            (&/T [_idx false])

                            (&/$Right _idx)
                            (&/T [_idx true]))
           _ (doto writer
               stack-peek
               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
               (.visitLdcInsn (int _idx)))
           _ (if is-last
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
      (compile-pattern* bodies stack-depth $else _left-pm)
      (compile-pattern* bodies stack-depth $else _right-pm))

    (&o/$AltPM _left-pm _right-pm)
    (|let [$alt-else (new Label)]
      (doto writer
        (.visitInsn Opcodes/DUP)
        (compile-pattern* bodies (inc stack-depth) $alt-else _left-pm)
        (.visitLabel $alt-else)
        (.visitInsn Opcodes/POP)
        (compile-pattern* bodies stack-depth $else _right-pm)))
    ))

(defn ^:private compile-pattern [^MethodVisitor writer bodies pm $end]
  (|let [$else (new Label)]
    (doto writer
      (compile-pattern* bodies 1 $else pm)
      (.visitLabel $else)
      (.visitInsn Opcodes/POP)
      (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_fail" "()V")
      (.visitInsn Opcodes/ACONST_NULL)
      (.visitJumpInsn Opcodes/GOTO $end))))

(defn ^:private compile-bodies [^MethodVisitor writer compile bodies-labels ?bodies $end]
  (&/map% (fn [label+body]
            (|let [[_label _body] label+body]
              (|do [:let [_ (.visitLabel writer _label)]
                    _ (compile _body)
                    :let [_ (.visitJumpInsn writer Opcodes/GOTO $end)]]
                (return nil))))
          (&/zip2 bodies-labels ?bodies)))

;; [Resources]
(defn compile-case [compile ?value ?pm ?bodies]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [$end (new Label)
              bodies-labels (&/|map (fn [_] (new Label)) ?bodies)]
        _ (compile ?value)
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/ACONST_NULL)
                  (.visitInsn Opcodes/SWAP)
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "pm_stack_push" "([Ljava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;"))
              _ (compile-pattern *writer* bodies-labels ?pm $end)]
        _ (compile-bodies *writer* compile bodies-labels ?bodies $end)
        :let [_ (.visitLabel *writer* $end)]]
    (return nil)))
