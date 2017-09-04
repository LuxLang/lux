(ns lux.compiler.jvm.rt
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [optimizer :as &o]
                 [host :as &host])
            [lux.type.host :as &host-type]
            [lux.host.generics :as &host-generics]
            [lux.analyser.base :as &a]
            [lux.compiler.jvm.base :as &&])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor
                              AnnotationVisitor)))

;; [Utils]
(def init-method "<init>")

;; [Resources]
;; Functions
(def compile-Function-class
  (|do [_ (return nil)
        :let [super-class "java/lang/Object"
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER
                                                         Opcodes/ACC_ABSTRACT
                                                         ;; Opcodes/ACC_INTERFACE
                                                         )
                               &&/function-class nil super-class (into-array String []))
                       (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL) &&/partials-field "I" nil nil)
                           (doto (.visitEnd))))
              =init-method (doto (.visitMethod =class Opcodes/ACC_PUBLIC init-method "(I)V" nil nil)
                             (.visitCode)
                             (.visitVarInsn Opcodes/ALOAD 0)
                             (.visitMethodInsn Opcodes/INVOKESPECIAL super-class init-method "()V")
                             (.visitVarInsn Opcodes/ALOAD 0)
                             (.visitVarInsn Opcodes/ILOAD 1)
                             (.visitFieldInsn Opcodes/PUTFIELD &&/function-class &&/partials-field "I")
                             (.visitInsn Opcodes/RETURN)
                             (.visitMaxs 0 0)
                             (.visitEnd))
              _ (dotimes [arity* &&/num-apply-variants]
                  (let [arity (inc arity*)]
                    (if (= 1 arity)
                      (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT) &&/apply-method (&&/apply-signature arity) nil nil)
                        (.visitEnd))
                      (doto (.visitMethod =class Opcodes/ACC_PUBLIC &&/apply-method (&&/apply-signature arity) nil nil)
                        (.visitCode)
                        (-> (.visitVarInsn Opcodes/ALOAD idx)
                            (->> (dotimes [idx arity])))
                        (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature (dec arity)))
                        (.visitTypeInsn Opcodes/CHECKCAST &&/function-class)
                        (.visitVarInsn Opcodes/ALOAD arity)
                        (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature 1))
                        (.visitInsn Opcodes/ARETURN)
                        (.visitMaxs 0 0)
                        (.visitEnd)))))]]
    (&&/save-class! (second (string/split &&/function-class #"/"))
                    (.toByteArray (doto =class .visitEnd)))))

;; Custom Runnable
(def compile-LuxRunnable-class
  (|do [_ (return nil)
        :let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                               "lux/LuxRunnable" nil "java/lang/Object" (into-array String ["java/lang/Runnable"])))
              _ (doto (.visitField =class Opcodes/ACC_PUBLIC "procedure" "Llux/Function;" nil nil)
                  (.visitEnd))
              _ (doto (.visitMethod =class Opcodes/ACC_PUBLIC init-method "(Llux/Function;)V" nil nil)
                  (.visitCode)
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" init-method "()V")
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitVarInsn Opcodes/ALOAD 1)
                  (.visitFieldInsn Opcodes/PUTFIELD "lux/LuxRunnable" "procedure" "Llux/Function;")
                  (.visitInsn Opcodes/RETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
              _ (doto (.visitMethod =class Opcodes/ACC_PUBLIC "run" "()V" nil nil)
                  (.visitCode)
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitFieldInsn Opcodes/GETFIELD "lux/LuxRunnable" "procedure" "Llux/Function;")
                  (.visitInsn Opcodes/ACONST_NULL)
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "lux/Function" &&/apply-method (&&/apply-signature 1))
                  (.visitInsn Opcodes/RETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))]]
    (&&/save-class! "LuxRunnable"
                    (.toByteArray (doto =class .visitEnd)))))

;; Runtime infrastructure
(defn ^:private compile-LuxRT-adt-methods [^ClassWriter =class]
  (|let [_ (let [$begin (new Label)
                 $not-rec (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "product_getLeft" "([Ljava/lang/Object;I)Ljava/lang/Object;" nil nil)
               (.visitCode)
               (.visitLabel $begin)
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; tuple-size
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple-size, index
               (.visitLdcInsn (int 1)) ;; tuple-size, index, offset-last-elem
               (.visitInsn Opcodes/IADD) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/DUP2) ;; tuple-size, index-last-elem, tuple-size, index-last-elem
               (.visitJumpInsn Opcodes/IF_ICMPGT $not-rec) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/SWAP) ;; index-last-elem, tuple-size
               (.visitInsn Opcodes/ISUB) ;; sub-index
               (.visitVarInsn Opcodes/ALOAD 0) ;; sub-index, tuple
               (.visitInsn Opcodes/DUP) ;; sub-index, tuple, tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; sub-index, tuple, tuple-size
               (.visitLdcInsn (int 1)) ;; sub-index, tuple, tuple-size, offset-last-elem
               (.visitInsn Opcodes/ISUB) ;; sub-index, tuple, index-last-elem
               (.visitInsn Opcodes/AALOAD) ;; sub-index, sub-tuple
               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
               (.visitVarInsn Opcodes/ASTORE 0) ;; sub-index
               (.visitVarInsn Opcodes/ISTORE 1) ;;
               (.visitJumpInsn Opcodes/GOTO $begin)
               (.visitLabel $not-rec) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/POP2) ;;
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple, index
               (.visitInsn Opcodes/AALOAD) ;; elem
               (.visitInsn Opcodes/ARETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$begin (new Label)
                 $is-last (new Label)
                 $must-copy (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "product_getRight" "([Ljava/lang/Object;I)Ljava/lang/Object;" nil nil)
               (.visitCode)
               (.visitLabel $begin)
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; tuple-size
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple-size, index
               (.visitLdcInsn (int 1)) ;; tuple-size, index, offset-last-elem
               (.visitInsn Opcodes/IADD) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/DUP2) ;; tuple-size, index-last-elem, tuple-size, index-last-elem
               (.visitJumpInsn Opcodes/IF_ICMPEQ $is-last) ;; tuple-size, index-last-elem
               (.visitJumpInsn Opcodes/IF_ICMPGT $must-copy) ;;
               ;; Must recurse
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitInsn Opcodes/DUP) ;; tuple, tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; tuple, tuple-size
               (.visitLdcInsn (int 1)) ;; tuple, tuple-size, offset-last-elem
               (.visitInsn Opcodes/ISUB) ;; tuple, offset-tuple-last-elem
               (.visitInsn Opcodes/AALOAD) ;; tuple-tail
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple-tail, index
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple-tail, index, tuple
               (.visitInsn Opcodes/ARRAYLENGTH) ;; tuple-tail, index, tuple-size
               (.visitLdcInsn (int 1)) ;; tuple-tail, index, tuple-size, 1
               (.visitInsn Opcodes/ISUB) ;; tuple-tail, index, tuple-size*
               (.visitInsn Opcodes/ISUB) ;; tuple-tail, index*
               (.visitVarInsn Opcodes/ISTORE 1) ;; tuple-tail
               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;") ;; tuple-tail
               (.visitVarInsn Opcodes/ASTORE 0) ;;
               (.visitJumpInsn Opcodes/GOTO $begin)
               (.visitLabel $must-copy)
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitVarInsn Opcodes/ILOAD 1)
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitInsn Opcodes/ARRAYLENGTH)
               (.visitMethodInsn Opcodes/INVOKESTATIC "java/util/Arrays" "copyOfRange" "([Ljava/lang/Object;II)[Ljava/lang/Object;")
               (.visitInsn Opcodes/ARETURN)
               (.visitLabel $is-last) ;; tuple-size, index-last-elem
               (.visitInsn Opcodes/POP2) ;;
               (.visitVarInsn Opcodes/ALOAD 0) ;; tuple
               (.visitVarInsn Opcodes/ILOAD 1) ;; tuple, index
               (.visitInsn Opcodes/AALOAD) ;; elem
               (.visitInsn Opcodes/ARETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$begin (new Label)
                 $just-return (new Label)
                 $then (new Label)
                 $further (new Label)
                 $shorten (new Label)
                 $not-right (new Label)
                 failure (fn [^MethodVisitor writer]
                           (doto writer
                             (.visitInsn Opcodes/POP2)
                             (.visitInsn Opcodes/ACONST_NULL)
                             (.visitInsn Opcodes/ARETURN)))
                 shortened (fn [^MethodVisitor writer]
                             (doto writer
                               ;; Get Tag
                               (.visitVarInsn Opcodes/ALOAD 0) (.visitLdcInsn (int 0)) (.visitInsn Opcodes/AALOAD)
                               ;; Shorten tag
                               &&/unwrap-int (.visitVarInsn Opcodes/ILOAD 1) (.visitInsn Opcodes/ISUB)
                               ;; Get flag
                               (.visitVarInsn Opcodes/ALOAD 0) (.visitLdcInsn (int 1)) (.visitInsn Opcodes/AALOAD)
                               ;; Get value
                               (.visitVarInsn Opcodes/ALOAD 0) (.visitLdcInsn (int 2)) (.visitInsn Opcodes/AALOAD)
                               ;; Build sum
                               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")))]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "sum_get" "([Ljava/lang/Object;ILjava/lang/Object;)Ljava/lang/Object;" nil nil)
               (.visitCode)
               (.visitLabel $begin)
               (.visitVarInsn Opcodes/ILOAD 1) ;; tag
               (.visitVarInsn Opcodes/ALOAD 0) ;; tag, sum
               (.visitLdcInsn (int 0)) ;; tag, sum, sum-tag-idx
               (.visitInsn Opcodes/AALOAD) ;; tag, sum-tag'
               &&/unwrap-int ;; tag, sum-tag
               (.visitInsn Opcodes/DUP2) ;; tag, sum-tag, tag, sum-tag
               (.visitJumpInsn Opcodes/IF_ICMPEQ $then) ;; tag, sum-tag
               (.visitInsn Opcodes/DUP2) ;; tag, sum-tag, tag, sum-tag
               (.visitJumpInsn Opcodes/IF_ICMPGT $further) ;; tag, sum-tag
               (.visitInsn Opcodes/DUP2) ;; tag, sum-tag, tag, sum-tag
               (.visitJumpInsn Opcodes/IF_ICMPLT $shorten) ;; tag, sum-tag
               failure
               (.visitLabel $then) ;; tag, sum-tag
               (.visitVarInsn Opcodes/ALOAD 2) ;; tag, sum-tag, wants-last?
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitLdcInsn (int 1))
               (.visitInsn Opcodes/AALOAD) ;; tag, sum-tag, wants-last?, is-last?
               (.visitJumpInsn Opcodes/IF_ACMPEQ $just-return)
               (.visitJumpInsn Opcodes/GOTO $further)
               (.visitLabel $just-return)
               (.visitInsn Opcodes/POP2)
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitLdcInsn (int 2))
               (.visitInsn Opcodes/AALOAD)
               (.visitInsn Opcodes/ARETURN)
               (.visitLabel $shorten)
               (.visitVarInsn Opcodes/ALOAD 2)
               (.visitJumpInsn Opcodes/IFNULL $not-right)
               (.visitInsn Opcodes/POP2)
               shortened
               (.visitInsn Opcodes/ARETURN)
               (.visitLabel $further) ;; tag, sum-tag
               (.visitVarInsn Opcodes/ALOAD 0) ;; tag, sum-tag, sum
               (.visitLdcInsn (int 1)) ;; tag, sum-tag, sum, last-index?
               (.visitInsn Opcodes/AALOAD) ;; tag, sum-tag, last?
               (.visitJumpInsn Opcodes/IFNULL $not-right) ;; tag, sum-tag
               (.visitInsn Opcodes/ISUB) ;; sub-tag
               (.visitVarInsn Opcodes/ALOAD 0) ;; sub-tag, sum
               (.visitLdcInsn (int 2)) ;; sub-tag, sum, sub-sum-idx
               (.visitInsn Opcodes/AALOAD) ;; sub-tag, sub-sum
               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
               (.visitVarInsn Opcodes/ASTORE 0) ;; sub-tag
               (.visitVarInsn Opcodes/ISTORE 1) ;;
               (.visitJumpInsn Opcodes/GOTO $begin)
               (.visitLabel $not-right) ;; tag, sum-tag
               failure
               (.visitMaxs 0 0)
               (.visitEnd)))
         ;; I commented-out some parts because a null-check was
         ;; done to ensure variants were never created with null
         ;; values (this would interfere later with
         ;; pattern-matching).
         ;; Since Lux itself does not have null values as part of
         ;; the language, the burden of ensuring non-nulls was
         ;; shifted to library code dealing with host-interop, to
         ;; ensure variant-making was as fast as possible.
         ;; The null-checking code was left as comments in case I
         ;; ever change my mind.
         _ (let [;; $is-null (new Label)
                 ]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;" nil nil)
               (.visitCode)
               ;; (.visitVarInsn Opcodes/ALOAD 2)
               ;; (.visitJumpInsn Opcodes/IFNULL $is-null)
               (.visitLdcInsn (int 3))
               (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object")
               (.visitInsn Opcodes/DUP)
               (.visitLdcInsn (int 0))
               (.visitVarInsn Opcodes/ILOAD 0)
               (&&/wrap-int)
               (.visitInsn Opcodes/AASTORE)
               (.visitInsn Opcodes/DUP)
               (.visitLdcInsn (int 1))
               (.visitVarInsn Opcodes/ALOAD 1)
               (.visitInsn Opcodes/AASTORE)
               (.visitInsn Opcodes/DUP)
               (.visitLdcInsn (int 2))
               (.visitVarInsn Opcodes/ALOAD 2)
               (.visitInsn Opcodes/AASTORE)
               (.visitInsn Opcodes/ARETURN)
               ;; (.visitLabel $is-null)
               ;; (.visitTypeInsn Opcodes/NEW "java/lang/IllegalStateException")
               ;; (.visitInsn Opcodes/DUP)
               ;; (.visitLdcInsn "Cannot create variant for null pointer")
               ;; (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/IllegalStateException" "<init>" "(Ljava/lang/String;)V")
               ;; (.visitInsn Opcodes/ATHROW)
               (.visitMaxs 0 0)
               (.visitEnd)))]
    nil))

(defn ^:private low-4b [^MethodVisitor =method]
  (doto =method
    ;; Assume there is a long at the top of the stack...
    ;; Add mask corresponding to -1 (FFFF...), on the low 32 bits.
    (.visitLdcInsn (int -1))
    (.visitInsn Opcodes/I2L)
    ;; Then do a bitwise and.
    (.visitInsn Opcodes/LAND)
    ))

(defn ^:private high-4b [^MethodVisitor =method]
  (doto =method
    ;; Assume there is a long at the top of the stack...
    (.visitLdcInsn (int 32))
    (.visitInsn Opcodes/LUSHR)
    ))

(defn ^:private swap2 [^MethodVisitor =method]
  (doto =method
    ;; X2, Y2
    (.visitInsn Opcodes/DUP2_X2) ;; Y2, X2, Y2
    (.visitInsn Opcodes/POP2) ;; Y2, X2
    ))

(defn ^:private swap2x1 [^MethodVisitor =method]
  (doto =method
    ;; X1, Y2
    (.visitInsn Opcodes/DUP2_X1) ;; Y2, X1, Y2
    (.visitInsn Opcodes/POP2) ;; Y2, X1
    ))

(defn ^:private bit-set-64? [^MethodVisitor =method]
  (doto =method
    ;; L, I
    (.visitLdcInsn (long 1)) ;; L, I, L
    (.visitInsn Opcodes/DUP2_X1) ;; L, L, I, L
    (.visitInsn Opcodes/POP2) ;; L, L, I
    (.visitInsn Opcodes/LSHL) ;; L, L
    (.visitInsn Opcodes/LAND) ;; L
    (.visitLdcInsn (long 0)) ;; L, L
    (.visitInsn Opcodes/LCMP) ;; I
    ))

(defn ^:private compile-LuxRT-deg-methods [^ClassWriter =class]
  (|let [deg-bits 64
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "mul_deg" "(JJ)J" nil nil)
             ;; Based on: http://stackoverflow.com/a/31629280/6823464
             (.visitCode)
             ;; Bottom part
             (.visitVarInsn Opcodes/LLOAD 0) low-4b
             (.visitVarInsn Opcodes/LLOAD 2) low-4b
             (.visitInsn Opcodes/LMUL)
             (.visitLdcInsn (int 32))
             (.visitInsn Opcodes/LUSHR)
             ;; Middle part
             (.visitVarInsn Opcodes/LLOAD 0) high-4b
             (.visitVarInsn Opcodes/LLOAD 2) low-4b
             (.visitInsn Opcodes/LMUL)
             (.visitVarInsn Opcodes/LLOAD 0) low-4b
             (.visitVarInsn Opcodes/LLOAD 2) high-4b
             (.visitInsn Opcodes/LMUL)
             (.visitInsn Opcodes/LADD)
             ;; Join middle and bottom
             (.visitInsn Opcodes/LADD)
             (.visitLdcInsn (int 32))
             (.visitInsn Opcodes/LUSHR)
             ;; Top part
             (.visitVarInsn Opcodes/LLOAD 0) high-4b
             (.visitVarInsn Opcodes/LLOAD 2) high-4b
             (.visitInsn Opcodes/LMUL)
             ;; Join top with rest
             (.visitInsn Opcodes/LADD)
             ;; Return
             (.visitInsn Opcodes/LRETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (let [$loop-start (new Label)
                 $done (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "count_leading_zeros" "(J)I" nil nil)
               (.visitCode)
               (.visitLdcInsn (int 64))
               (.visitLabel $loop-start)
               (.visitVarInsn Opcodes/LLOAD 0)
               (.visitLdcInsn (long 0))
               (.visitInsn Opcodes/LCMP)
               (.visitJumpInsn Opcodes/IFEQ $done)
               (.visitVarInsn Opcodes/LLOAD 0)
               (.visitLdcInsn (int 1))
               (.visitInsn Opcodes/LUSHR)
               (.visitVarInsn Opcodes/LSTORE 0)
               (.visitLdcInsn (int 1))
               (.visitInsn Opcodes/ISUB)
               (.visitJumpInsn Opcodes/GOTO $loop-start)
               (.visitLabel $done)
               (.visitInsn Opcodes/IRETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$same (new Label)]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "div_deg" "(JJ)J" nil nil)
               (.visitCode)
               (.visitVarInsn Opcodes/LLOAD 0)
               (.visitVarInsn Opcodes/LLOAD 2)
               (.visitInsn Opcodes/LCMP)
               (.visitJumpInsn Opcodes/IFEQ $same)
               ;; Based on: http://stackoverflow.com/a/8510587/6823464
               ;; Shifting the operands as much as possible can help
               ;; avoid some loss of precision later.
               (.visitVarInsn Opcodes/LLOAD 0)
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "count_leading_zeros" "(J)I")
               (.visitVarInsn Opcodes/LLOAD 2)
               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "count_leading_zeros" "(J)I")
               (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Math" "min" "(II)I")
               (.visitVarInsn Opcodes/ISTORE 4)
               (.visitVarInsn Opcodes/LLOAD 0) (.visitVarInsn Opcodes/ILOAD 4) (.visitInsn Opcodes/LSHL)
               (.visitVarInsn Opcodes/LLOAD 2) (.visitVarInsn Opcodes/ILOAD 4) (.visitInsn Opcodes/LSHL) high-4b
               (.visitInsn Opcodes/LDIV)
               (.visitLdcInsn (int 32))
               (.visitInsn Opcodes/LSHL)
               (.visitInsn Opcodes/LRETURN)
               (.visitLabel $same)
               (.visitLdcInsn (long -1)) ;; ~= 1.0 DEG
               (.visitInsn Opcodes/LRETURN)
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "deg-to-frac" "(J)D" nil nil)
             (.visitCode)
             ;; Translate high bytes
             (.visitVarInsn Opcodes/LLOAD 0) high-4b
             (.visitInsn Opcodes/L2D)
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DDIV)
             ;; Translate low bytes
             (.visitVarInsn Opcodes/LLOAD 0) low-4b
             (.visitInsn Opcodes/L2D)
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DDIV)
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DDIV)
             ;; Combine and return
             (.visitInsn Opcodes/DADD)
             (.visitInsn Opcodes/DRETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "frac-to-deg" "(D)J" nil nil)
             (.visitCode)
             ;; Drop any excess
             (.visitVarInsn Opcodes/DLOAD 0)
             (.visitLdcInsn (double 1.0))
             (.visitInsn Opcodes/DREM)
             ;; Shift upper half, but retain remaining decimals
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DMUL)
             ;; Make a copy, so the lower half can be extracted
             (.visitInsn Opcodes/DUP2)
             ;; Get that lower half
             (.visitLdcInsn (double 1.0))
             (.visitInsn Opcodes/DREM)
             (.visitLdcInsn (double (Math/pow 2 32)))
             (.visitInsn Opcodes/DMUL)
             ;; Turn it into a deg
             (.visitInsn Opcodes/D2L)
             ;; Turn the upper half into deg too
             swap2
             (.visitInsn Opcodes/D2L)
             ;; Combine both pieces
             (.visitInsn Opcodes/LADD)
             ;; FINISH
             (.visitInsn Opcodes/LRETURN)
             (.visitMaxs 0 0)
             (.visitEnd))]
    nil))

(let [+wrapper-class+ (&host-generics/->bytecode-class-name "java.lang.Long")]
  (defn ^:private compile-LuxRT-nat-methods [^ClassWriter =class]
    (|let [;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#215
           _ (let [$simple-case (new Label)]
               (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;" nil nil)
                 (.visitCode)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFGE $simple-case)
                 ;; else
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (int 32))
                 (.visitInsn Opcodes/LUSHR)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;")
                 (.visitLdcInsn (int 32))
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "shiftLeft" "(I)Ljava/math/BigInteger;")
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (int 32))
                 (.visitInsn Opcodes/LSHL)
                 (.visitLdcInsn (int 32))
                 (.visitInsn Opcodes/LUSHR)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "add" "(Ljava/math/BigInteger;)Ljava/math/BigInteger;")
                 (.visitInsn Opcodes/ARETURN)
                 ;; then
                 (.visitLabel $simple-case)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;")
                 (.visitInsn Opcodes/ARETURN)
                 (.visitMaxs 0 0)
                 (.visitEnd)))
           ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java?av=f#1267
           _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "_compareUnsigned" "(JJ)I" nil nil)
               (.visitCode)
               (.visitVarInsn Opcodes/LLOAD 0)
               (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Long" "MIN_VALUE" "J")
               (.visitInsn Opcodes/LADD)
               (.visitVarInsn Opcodes/LLOAD 2)
               (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Long" "MIN_VALUE" "J")
               (.visitInsn Opcodes/LADD)
               (.visitInsn Opcodes/LCMP)
               (.visitInsn Opcodes/IRETURN)
               (.visitMaxs 0 0)
               (.visitEnd))
           ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#1290
           _ (let [$case-1 (new Label)
                   $0 (new Label)
                   $case-2 (new Label)]
               (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "div_nat" "(JJ)J" nil nil)
                 (.visitCode)
                 ;; Test #1
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFLT $case-1)
                 ;; Test #2
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFGT $case-2)
                 ;; Case #3
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "divide" "(Ljava/math/BigInteger;)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "longValue" "()J")
                 (.visitInsn Opcodes/LRETURN)
                 ;; Case #2
                 (.visitLabel $case-2)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitInsn Opcodes/LDIV)
                 (.visitInsn Opcodes/LRETURN)
                 ;; Case #1
                 (.visitLabel $case-1)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
                 (.visitJumpInsn Opcodes/IFLT $0)
                 ;; 1
                 (.visitLdcInsn (long 1))
                 (.visitInsn Opcodes/LRETURN)
                 ;; 0
                 (.visitLabel $0)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LRETURN)
                 (.visitMaxs 0 0)
                 (.visitEnd)))
           ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#1323
           _ (let [$test-2 (new Label)
                   $case-2 (new Label)]
               (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "rem_nat" "(JJ)J" nil nil)
                 (.visitCode)
                 ;; Test #1
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFLE $test-2)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitLdcInsn (long 0))
                 (.visitInsn Opcodes/LCMP)
                 (.visitJumpInsn Opcodes/IFLE $test-2)
                 ;; Case #1
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitInsn Opcodes/LREM)
                 (.visitInsn Opcodes/LRETURN)
                 ;; Test #2
                 (.visitLabel $test-2)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
                 (.visitJumpInsn Opcodes/IFLT $case-2)
                 ;; Case #3
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
                 (.visitVarInsn Opcodes/LLOAD 2)
                 (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "remainder" "(Ljava/math/BigInteger;)Ljava/math/BigInteger;")
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "longValue" "()J")
                 (.visitInsn Opcodes/LRETURN)
                 ;; Case #2
                 (.visitLabel $case-2)
                 (.visitVarInsn Opcodes/LLOAD 0)
                 (.visitInsn Opcodes/LRETURN)
                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 (.visitMaxs 0 0)
                 (.visitEnd)))]
      nil)))

(do-template [<name> <method> <class> <parse-method> <signature> <wrapper>]
  (defn <name> [^ClassWriter =class]
    (do (let [$from (new Label)
              $to (new Label)
              $handler (new Label)]
          (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) <method> "(Ljava/lang/String;)[Ljava/lang/Object;" nil nil)
            (.visitCode)
            (.visitTryCatchBlock $from $to $handler "java/lang/Exception")
            (.visitLabel $from)
            (.visitVarInsn Opcodes/ALOAD 0)
            (.visitMethodInsn Opcodes/INVOKESTATIC <class> <parse-method> <signature>)
            <wrapper>
            (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)[Ljava/lang/Object;")
            (.visitInsn Opcodes/ARETURN)
            (.visitLabel $to)
            (.visitLabel $handler)
            (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()[Ljava/lang/Object;")
            (.visitInsn Opcodes/ARETURN)
            (.visitMaxs 0 0)
            (.visitEnd)))
      nil))

  ^:private compile-LuxRT-int-methods  "decode_int"  "java/lang/Long"   "parseLong"   "(Ljava/lang/String;)J" &&/wrap-long
  ^:private compile-LuxRT-frac-methods "decode_frac" "java/lang/Double" "parseDouble" "(Ljava/lang/String;)D" &&/wrap-double
  )

(defn ^:private compile-LuxRT-pm-methods [^ClassWriter =class]
  (|let [_ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "pm_fail" "()V" nil nil)
             (.visitCode)
             (.visitTypeInsn Opcodes/NEW "java/lang/IllegalStateException")
             (.visitInsn Opcodes/DUP)
             (.visitLdcInsn "Invalid expression for pattern-matching.")
             (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/IllegalStateException" "<init>" "(Ljava/lang/String;)V")
             (.visitInsn Opcodes/ATHROW)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "pm_stack_push" "([Ljava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;" nil nil)
             (.visitCode)
             (.visitLdcInsn (int 2))
             (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object")
             (.visitInsn Opcodes/DUP)
             (.visitLdcInsn (int 0))
             (.visitVarInsn Opcodes/ALOAD 0)
             (.visitInsn Opcodes/AASTORE)
             (.visitInsn Opcodes/DUP)
             (.visitLdcInsn (int 1))
             (.visitVarInsn Opcodes/ALOAD 1)
             (.visitInsn Opcodes/AASTORE)
             (.visitInsn Opcodes/ARETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "pm_stack_pop" "([Ljava/lang/Object;)[Ljava/lang/Object;" nil nil)
             (.visitCode)
             (.visitVarInsn Opcodes/ALOAD 0)
             (.visitLdcInsn (int 0))
             (.visitInsn Opcodes/AALOAD)
             (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
             (.visitInsn Opcodes/ARETURN)
             (.visitMaxs 0 0)
             (.visitEnd))
         _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "pm_stack_peek" "([Ljava/lang/Object;)Ljava/lang/Object;" nil nil)
             (.visitCode)
             (.visitVarInsn Opcodes/ALOAD 0)
             (.visitLdcInsn (int 1))
             (.visitInsn Opcodes/AALOAD)
             (.visitInsn Opcodes/ARETURN)
             (.visitMaxs 0 0)
             (.visitEnd))]
    nil))

(defn ^:private compile-LuxRT-text-methods [^ClassWriter =class]
  (do (let [$from (new Label)
            $to (new Label)
            $handler (new Label)
            $end (new Label)]
        (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "text_clip" "(Ljava/lang/String;II)[Ljava/lang/Object;" nil nil)
          (.visitCode)
          (.visitTryCatchBlock $from $to $handler "java/lang/IndexOutOfBoundsException")
          (.visitLabel $from)
          (.visitVarInsn Opcodes/ALOAD 0)
          (.visitVarInsn Opcodes/ILOAD 1)
          (.visitVarInsn Opcodes/ILOAD 2)
          (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(II)Ljava/lang/String;")
          (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)[Ljava/lang/Object;")
          (.visitJumpInsn Opcodes/GOTO $end)
          (.visitLabel $to)
          (.visitLabel $handler)
          (.visitInsn Opcodes/POP)
          (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()[Ljava/lang/Object;")
          (.visitLabel $end)
          (.visitInsn Opcodes/ARETURN)
          (.visitMaxs 0 0)
          (.visitEnd)))
    (let [$from (new Label)
          $to (new Label)
          $handler (new Label)]
      (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "text_char" "(Ljava/lang/String;I)[Ljava/lang/Object;" nil nil)
        (.visitCode)
        (.visitTryCatchBlock $from $to $handler "java/lang/IndexOutOfBoundsException")
        (.visitLabel $from)
        (.visitVarInsn Opcodes/ALOAD 0)
        (.visitVarInsn Opcodes/ILOAD 1)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "codePointAt" "(I)I")
        (.visitInsn Opcodes/I2L)
        &&/wrap-long
        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)[Ljava/lang/Object;")
        (.visitInsn Opcodes/ARETURN)
        (.visitLabel $to)
        (.visitLabel $handler)
        (.visitInsn Opcodes/POP)
        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()[Ljava/lang/Object;")
        (.visitInsn Opcodes/ARETURN)
        (.visitMaxs 0 0)
        (.visitEnd)))
    nil))

(defn ^:private compile-LuxRT-process-methods [^ClassWriter =class]
  (do (doto (.visitField =class
                         (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC)
                         "concurrency_level" "I" nil nil)
        (.visitEnd))
    (doto (.visitField =class
                       (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC)
                       "executor" "Ljava/util/concurrent/ScheduledThreadPoolExecutor;" nil nil)
      (.visitEnd))
    (doto (.visitMethod =class Opcodes/ACC_STATIC "<clinit>" "()V" nil nil)
      (.visitCode)
      ;; concurrency_level
      (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Runtime" "getRuntime" "()Ljava/lang/Runtime;")
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Runtime" "availableProcessors" "()I")
      (.visitFieldInsn Opcodes/PUTSTATIC "lux/LuxRT" "concurrency_level" "I")
      ;; executor
      (.visitTypeInsn Opcodes/NEW "java/util/concurrent/ScheduledThreadPoolExecutor")
      (.visitInsn Opcodes/DUP)
      (.visitFieldInsn Opcodes/GETSTATIC "lux/LuxRT" "concurrency_level" "I")
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/util/concurrent/ScheduledThreadPoolExecutor" "<init>" "(I)V")
      (.visitFieldInsn Opcodes/PUTSTATIC "lux/LuxRT" "executor" "Ljava/util/concurrent/ScheduledThreadPoolExecutor;")
      ;; DONE
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))
    (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "future" "(Llux/Function;)Ljava/lang/Object;" nil nil)
      (.visitCode)
      (.visitTypeInsn Opcodes/NEW "java/lang/Thread")
      (.visitInsn Opcodes/DUP)
      (.visitTypeInsn Opcodes/NEW "lux/LuxRunnable")
      (.visitInsn Opcodes/DUP)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL "lux/LuxRunnable" "<init>" "(Llux/Function;)V")
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Thread" "<init>" "(Ljava/lang/Runnable;)V")
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Thread" "start" "()V")
      (.visitLdcInsn &/unit-tag)
      (.visitInsn Opcodes/ARETURN)
      (.visitMaxs 0 0)
      (.visitEnd))
    (let [$immediately (new Label)]
      (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "schedule" "(JLlux/Function;)Ljava/lang/Object;" nil nil)
        (.visitCode)
        (.visitVarInsn Opcodes/LLOAD 0)
        (.visitLdcInsn (long 0))
        (.visitInsn Opcodes/LCMP)
        (.visitJumpInsn Opcodes/IFEQ $immediately)
        ;; Schedule for later
        (.visitFieldInsn Opcodes/GETSTATIC "lux/LuxRT" "executor" "Ljava/util/concurrent/ScheduledThreadPoolExecutor;")
        (.visitTypeInsn Opcodes/NEW "lux/LuxRunnable")
        (.visitInsn Opcodes/DUP)
        (.visitVarInsn Opcodes/ALOAD 2)
        (.visitMethodInsn Opcodes/INVOKESPECIAL "lux/LuxRunnable" "<init>" "(Llux/Function;)V")
        (.visitVarInsn Opcodes/LLOAD 0)
        (.visitFieldInsn Opcodes/GETSTATIC "java/util/concurrent/TimeUnit" "MILLISECONDS" "Ljava/util/concurrent/TimeUnit;")
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/util/concurrent/ScheduledThreadPoolExecutor" "schedule" "(Ljava/lang/Runnable;JLjava/util/concurrent/TimeUnit;)Ljava/util/concurrent/ScheduledFuture;")
        (.visitLdcInsn &/unit-tag)
        (.visitInsn Opcodes/ARETURN)
        ;; Run immediately
        (.visitLabel $immediately)
        (.visitFieldInsn Opcodes/GETSTATIC "lux/LuxRT" "executor" "Ljava/util/concurrent/ScheduledThreadPoolExecutor;")
        (.visitTypeInsn Opcodes/NEW "lux/LuxRunnable")
        (.visitInsn Opcodes/DUP)
        (.visitVarInsn Opcodes/ALOAD 2)
        (.visitMethodInsn Opcodes/INVOKESPECIAL "lux/LuxRunnable" "<init>" "(Llux/Function;)V")
        (.visitMethodInsn Opcodes/INVOKEINTERFACE "java/util/concurrent/Executor" "execute" "(Ljava/lang/Runnable;)V")
        (.visitLdcInsn &/unit-tag)
        (.visitInsn Opcodes/ARETURN)
        (.visitMaxs 0 0)
        (.visitEnd)))
    nil))

(def compile-LuxRT-class
  (|do [_ (return nil)
        :let [full-name &&/lux-utils-class
              super-class (&host-generics/->bytecode-class-name "java.lang.Object")
              tag-sig (&host-generics/->type-signature "java.lang.String")
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                               full-name nil super-class (into-array String [])))
              =unit-tag (doto (.visitField =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) &&/unit-tag-field tag-sig nil &/unit-tag)
                          (.visitEnd))
              =init-method (doto (.visitMethod =class Opcodes/ACC_PRIVATE init-method "()V" nil nil)
                             (.visitCode)
                             (.visitVarInsn Opcodes/ALOAD 0)
                             (.visitMethodInsn Opcodes/INVOKESPECIAL super-class init-method "()V")
                             (.visitInsn Opcodes/RETURN)
                             (.visitMaxs 0 0)
                             (.visitEnd))
              _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "log" "(Ljava/lang/Object;)Ljava/lang/Object;" nil nil)
                  (.visitCode)
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;")
                  (.visitLdcInsn "LOG: ")
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "print" "(Ljava/lang/Object;)V")
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;")
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "println" "(Ljava/lang/Object;)V")
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
              _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "make_none" "()[Ljava/lang/Object;" nil nil)
                  (.visitCode)
                  (.visitLdcInsn (->> #'&/$None meta ::&/idx int)) ;; I
                  (.visitInsn Opcodes/ACONST_NULL) ;; I?
                  (.visitLdcInsn &/unit-tag) ;; I?U
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
              _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "make_some" "(Ljava/lang/Object;)[Ljava/lang/Object;" nil nil)
                  (.visitCode)
                  (.visitLdcInsn (->> #'&/$Some meta ::&/idx int)) ;; I
                  (.visitLdcInsn "") ;; I?
                  (.visitVarInsn Opcodes/ALOAD 0) ;; I?O
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
              _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "clean_separators" "(Ljava/lang/String;)Ljava/lang/String;" nil nil)
                  (.visitCode)
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitLdcInsn "_")
                  (.visitLdcInsn "")
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "replaceAll" "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;")
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
              _ (let [$from (new Label)
                      $to (new Label)
                      $handler (new Label)
                      make-string-writerI (fn [_method_]
                                            (doto _method_
                                              (.visitTypeInsn Opcodes/NEW "java/io/StringWriter")
                                              (.visitInsn Opcodes/DUP)
                                              (.visitMethodInsn Opcodes/INVOKESPECIAL "java/io/StringWriter" "<init>" "()V")))
                      make-print-writerI (fn [_method_]
                                           (doto _method_
                                             ;; W
                                             (.visitTypeInsn Opcodes/NEW "java/io/PrintWriter") ;; WP
                                             (.visitInsn Opcodes/SWAP) ;; PW
                                             (.visitInsn Opcodes/DUP2) ;; PWPW
                                             (.visitInsn Opcodes/POP) ;; PWP
                                             (.visitInsn Opcodes/SWAP) ;; PPW
                                             (.visitLdcInsn true) ;; PPW?
                                             (.visitMethodInsn Opcodes/INVOKESPECIAL "java/io/PrintWriter" "<init>" "(Ljava/io/Writer;Z)V")
                                             ;; P
                                             ))]
                  (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "runTry" "(Llux/Function;)[Ljava/lang/Object;" nil nil)
                    (.visitCode)
                    (.visitTryCatchBlock $from $to $handler "java/lang/Throwable")
                    (.visitLabel $from)
                    (.visitVarInsn Opcodes/ALOAD 0)
                    (.visitInsn Opcodes/ACONST_NULL)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL "lux/Function" &&/apply-method (&&/apply-signature 1))
                    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)[Ljava/lang/Object;")
                    (.visitInsn Opcodes/ARETURN)
                    (.visitLabel $to)
                    (.visitLabel $handler) ;; T
                    make-string-writerI ;; TW
                    (.visitInsn Opcodes/DUP2) ;; TWTW
                    make-print-writerI ;; TWTP
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Throwable" "printStackTrace" "(Ljava/io/PrintWriter;)V") ;; TW
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/io/StringWriter" "toString" "()Ljava/lang/String;") ;; TS
                    (.visitInsn Opcodes/SWAP) (.visitInsn Opcodes/POP) ;; S
                    (.visitLdcInsn (->> #'&/$Left meta ::&/idx int)) ;; SI
                    (.visitInsn Opcodes/ACONST_NULL) ;; SI?
                    swap2x1 ;; I?S
                    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                    (.visitInsn Opcodes/ARETURN)
                    (.visitMaxs 0 0)
                    (.visitEnd)))
              _ (doto =class
                  (compile-LuxRT-pm-methods)
                  (compile-LuxRT-adt-methods)
                  (compile-LuxRT-nat-methods)
                  (compile-LuxRT-int-methods)
                  (compile-LuxRT-deg-methods)
                  (compile-LuxRT-frac-methods)
                  (compile-LuxRT-text-methods)
                  (compile-LuxRT-process-methods))]]
    (&&/save-class! (second (string/split &&/lux-utils-class #"/"))
                    (.toByteArray (doto =class .visitEnd)))))
