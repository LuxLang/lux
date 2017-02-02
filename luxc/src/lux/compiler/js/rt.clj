(ns lux.compiler.js.rt
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
            [lux.analyser.base :as &a]
            [lux.compiler.js.base :as &&]))

;; (defn ^:private low-4b [^MethodVisitor =method]
;;   (doto =method
;;     ;; Assume there is a long at the top of the stack...
;;     ;; Add mask corresponding to -1 (FFFF...), on the low 32 bits.
;;     (.visitLdcInsn (int -1))
;;     (.visitInsn Opcodes/I2L)
;;     ;; Then do a bitwise and.
;;     (.visitInsn Opcodes/LAND)
;;     ))

;; (defn ^:private high-4b [^MethodVisitor =method]
;;   (doto =method
;;     ;; Assume there is a long at the top of the stack...
;;     (.visitLdcInsn (int 32))
;;     (.visitInsn Opcodes/LUSHR)
;;     ))

;; (defn ^:private swap2 [^MethodVisitor =method]
;;   (doto =method
;;     ;; X2, Y2
;;     (.visitInsn Opcodes/DUP2_X2) ;; Y2, X2, Y2
;;     (.visitInsn Opcodes/POP2) ;; Y2, X2
;;     ))

;; (defn ^:private swap2x1 [^MethodVisitor =method]
;;   (doto =method
;;     ;; X1, Y2
;;     (.visitInsn Opcodes/DUP2_X1) ;; Y2, X1, Y2
;;     (.visitInsn Opcodes/POP2) ;; Y2, X1
;;     ))

;; (defn ^:private bit-set-64? [^MethodVisitor =method]
;;   (doto =method
;;     ;; L, I
;;     (.visitLdcInsn (long 1)) ;; L, I, L
;;     (.visitInsn Opcodes/DUP2_X1) ;; L, L, I, L
;;     (.visitInsn Opcodes/POP2) ;; L, L, I
;;     (.visitInsn Opcodes/LSHL) ;; L, L
;;     (.visitInsn Opcodes/LAND) ;; L
;;     (.visitLdcInsn (long 0)) ;; L, L
;;     (.visitInsn Opcodes/LCMP) ;; I
;;     ))

;; (defn ^:private compile-LuxRT-deg-methods [^ClassWriter =class]
;;   (|let [deg-bits 64
;;          _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "mul_deg" "(JJ)J" nil nil)
;;              ;; Based on: http://stackoverflow.com/a/31629280/6823464
;;              (.visitCode)
;;              ;; Bottom part
;;              (.visitVarInsn Opcodes/LLOAD 0) low-4b
;;              (.visitVarInsn Opcodes/LLOAD 2) low-4b
;;              (.visitInsn Opcodes/LMUL)
;;              (.visitLdcInsn (int 32))
;;              (.visitInsn Opcodes/LUSHR)
;;              ;; Middle part
;;              (.visitVarInsn Opcodes/LLOAD 0) high-4b
;;              (.visitVarInsn Opcodes/LLOAD 2) low-4b
;;              (.visitInsn Opcodes/LMUL)
;;              (.visitVarInsn Opcodes/LLOAD 0) low-4b
;;              (.visitVarInsn Opcodes/LLOAD 2) high-4b
;;              (.visitInsn Opcodes/LMUL)
;;              (.visitInsn Opcodes/LADD)
;;              ;; Join middle and bottom
;;              (.visitInsn Opcodes/LADD)
;;              (.visitLdcInsn (int 32))
;;              (.visitInsn Opcodes/LUSHR)
;;              ;; Top part
;;              (.visitVarInsn Opcodes/LLOAD 0) high-4b
;;              (.visitVarInsn Opcodes/LLOAD 2) high-4b
;;              (.visitInsn Opcodes/LMUL)
;;              ;; Join top with rest
;;              (.visitInsn Opcodes/LADD)
;;              ;; Return
;;              (.visitInsn Opcodes/LRETURN)
;;              (.visitMaxs 0 0)
;;              (.visitEnd))
;;          _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "div_deg" "(JJ)J" nil nil)
;;              (.visitCode)
;;              ;; Based on: http://stackoverflow.com/a/8510587/6823464
;;              (.visitVarInsn Opcodes/LLOAD 0)
;;              (.visitVarInsn Opcodes/LLOAD 2) high-4b
;;              (.visitInsn Opcodes/LDIV)
;;              (.visitLdcInsn (int 32))
;;              (.visitInsn Opcodes/LSHL)
;;              (.visitInsn Opcodes/LRETURN)
;;              (.visitMaxs 0 0)
;;              (.visitEnd))
;;          _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "deg-to-real" "(J)D" nil nil)
;;              (.visitCode)
;;              ;; Translate high bytes
;;              (.visitVarInsn Opcodes/LLOAD 0) high-4b
;;              (.visitInsn Opcodes/L2D)
;;              (.visitLdcInsn (double (Math/pow 2 32)))
;;              (.visitInsn Opcodes/DDIV)
;;              ;; Translate low bytes
;;              (.visitVarInsn Opcodes/LLOAD 0) low-4b
;;              (.visitInsn Opcodes/L2D)
;;              (.visitLdcInsn (double (Math/pow 2 32)))
;;              (.visitInsn Opcodes/DDIV)
;;              (.visitLdcInsn (double (Math/pow 2 32)))
;;              (.visitInsn Opcodes/DDIV)
;;              ;; Combine and return
;;              (.visitInsn Opcodes/DADD)
;;              (.visitInsn Opcodes/DRETURN)
;;              (.visitMaxs 0 0)
;;              (.visitEnd))
;;          _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "real-to-deg" "(D)J" nil nil)
;;              (.visitCode)
;;              ;; Drop any excess
;;              (.visitVarInsn Opcodes/DLOAD 0)
;;              (.visitLdcInsn (double 1.0))
;;              (.visitInsn Opcodes/DREM)
;;              ;; Shift upper half, but retain remaining decimals
;;              (.visitLdcInsn (double (Math/pow 2 32)))
;;              (.visitInsn Opcodes/DMUL)
;;              ;; Make a copy, so the lower half can be extracted
;;              (.visitInsn Opcodes/DUP2)
;;              ;; Get that lower half
;;              (.visitLdcInsn (double 1.0))
;;              (.visitInsn Opcodes/DREM)
;;              (.visitLdcInsn (double (Math/pow 2 32)))
;;              (.visitInsn Opcodes/DMUL)
;;              ;; Turn it into a deg
;;              (.visitInsn Opcodes/D2L)
;;              ;; Turn the upper half into deg too
;;              swap2
;;              (.visitInsn Opcodes/D2L)
;;              ;; Combine both pieces
;;              (.visitInsn Opcodes/LADD)
;;              ;; FINISH
;;              (.visitInsn Opcodes/LRETURN)
;;              (.visitMaxs 0 0)
;;              (.visitEnd))
;;          _ (let [$loop-start (new Label)
;;                  $do-a-round (new Label)]
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "times5" "(I[B)[B" nil nil)
;;                (.visitCode)
;;                (.visitLdcInsn (int 0)) ;; {carry}
;;                (.visitLabel $loop-start)
;;                (.visitVarInsn Opcodes/ILOAD 0)
;;                (.visitJumpInsn Opcodes/IFGE $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 1)
;;                (.visitInsn Opcodes/ARETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;; {carry}
;;                (.visitLabel $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 1)
;;                (.visitVarInsn Opcodes/ILOAD 0)
;;                (.visitInsn Opcodes/BALOAD) ;; {carry, current-digit}
;;                (.visitLdcInsn (int 5))
;;                (.visitInsn Opcodes/IMUL)
;;                (.visitInsn Opcodes/IADD) ;; {next-raw-digit}
;;                (.visitInsn Opcodes/DUP)
;;                (.visitLdcInsn (int 10))
;;                (.visitInsn Opcodes/IREM) ;; {next-raw-digit, next-digit}
;;                (.visitVarInsn Opcodes/ALOAD 1)
;;                (.visitVarInsn Opcodes/ILOAD 0)
;;                swap2x1
;;                (.visitInsn Opcodes/BASTORE) ;; {next-raw-digit}
;;                (.visitLdcInsn (int 10))
;;                (.visitInsn Opcodes/IDIV) ;; {next-carry}
;;                ;; Decrement index
;;                (.visitVarInsn Opcodes/ILOAD 0)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitVarInsn Opcodes/ISTORE 0)
;;                ;; Iterate
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))
;;          _ (let [$loop-start (new Label)
;;                  $do-a-round (new Label)]
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "deg_digit_power" "(I)[B" nil nil)
;;                (.visitCode)
;;                ;; Initialize digits array.
;;                (.visitLdcInsn (int deg-bits))
;;                (.visitIntInsn Opcodes/NEWARRAY Opcodes/T_BYTE) ;; {digits}
;;                (.visitInsn Opcodes/DUP)
;;                (.visitVarInsn Opcodes/ILOAD 0)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/BASTORE) ;; digits = 5^0
;;                (.visitVarInsn Opcodes/ASTORE 1)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitVarInsn Opcodes/ILOAD 0) ;; {times}
;;                (.visitLabel $loop-start)
;;                (.visitInsn Opcodes/DUP)
;;                (.visitJumpInsn Opcodes/IFGE $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 1)
;;                (.visitInsn Opcodes/ARETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $do-a-round)
;;                ;; {times}
;;                (.visitVarInsn Opcodes/ILOAD 0)
;;                (.visitVarInsn Opcodes/ALOAD 1)
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "times5" "(I[B)[B") ;; {digits*5, times}
;;                (.visitVarInsn Opcodes/ASTORE 1) ;; {times}
;;                ;; Decrement index
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/ISUB)
;;                ;; {times-1}
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))
;;          _ (let [$loop-start (new Label)
;;                  $do-a-round (new Label)]
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "add_deg_digit_powers" "([B[B)[B" nil nil)
;;                (.visitCode)
;;                (.visitLdcInsn (int (dec deg-bits)))
;;                (.visitVarInsn Opcodes/ISTORE 2) ;; Index
;;                (.visitLdcInsn (int deg-bits))
;;                (.visitIntInsn Opcodes/NEWARRAY Opcodes/T_BYTE)
;;                (.visitVarInsn Opcodes/ASTORE 3) ;; added_digits
;;                (.visitLdcInsn (int 0)) ;; {carry}
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;; {carry}
;;                (.visitLabel $loop-start)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitJumpInsn Opcodes/IFGE $do-a-round)
;;                ;; {carry}
;;                (.visitVarInsn Opcodes/ALOAD 3)
;;                (.visitInsn Opcodes/ARETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;; {carry}
;;                (.visitLabel $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitInsn Opcodes/BALOAD) ;; {carry, dL}
;;                (.visitVarInsn Opcodes/ALOAD 1)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitInsn Opcodes/BALOAD) ;; {carry, dL, dR}
;;                (.visitInsn Opcodes/IADD)
;;                (.visitInsn Opcodes/IADD) ;; {raw-next-digit}
;;                (.visitInsn Opcodes/DUP)
;;                (.visitLdcInsn (int 10))
;;                (.visitInsn Opcodes/IREM) ;; {raw-next-digit, next-digit}
;;                (.visitVarInsn Opcodes/ALOAD 3)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                swap2x1
;;                (.visitInsn Opcodes/BASTORE) ;; {raw-next-digit}
;;                (.visitLdcInsn (int 10))
;;                (.visitInsn Opcodes/IDIV) ;; {next-carry}
;;                ;; Decrement index
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitVarInsn Opcodes/ISTORE 2)
;;                ;; Iterate
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))
;;          _ (let [$loop-start (new Label)
;;                  $do-a-round (new Label)]
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "deg_digits_to_text" "([B)Ljava/lang/String;" nil nil)
;;                (.visitCode)
;;                (.visitLdcInsn (int (dec deg-bits)))
;;                (.visitVarInsn Opcodes/ISTORE 1) ;; Index
;;                (.visitLdcInsn "") ;; {text}
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $loop-start)
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitJumpInsn Opcodes/IFGE $do-a-round)
;;                (.visitInsn Opcodes/ARETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitInsn Opcodes/BALOAD) ;; {text, digit}
;;                (.visitLdcInsn (int 10)) ;; {text, digit, radix}
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Character" "forDigit" "(II)C") ;; {text, digit-char}
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Character" "toString" "(C)Ljava/lang/String;") ;; {text, digit-char-text}
;;                (.visitInsn Opcodes/SWAP)
;;                (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;")
;;                ;; Decrement index
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitVarInsn Opcodes/ISTORE 1)
;;                ;; Iterate
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))
;;          _ (let [$loop-start (new Label)
;;                  $do-a-round (new Label)
;;                  $not-set (new Label)
;;                  $next-iteration (new Label)
;;                  $normal-path (new Label)]
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "encode_deg" "(J)Ljava/lang/String;" nil nil)
;;                (.visitCode)
;;                ;; A quick corner-case to handle.
;;                (.visitVarInsn Opcodes/LLOAD 0)
;;                (.visitLdcInsn (long 0))
;;                (.visitInsn Opcodes/LCMP)
;;                (.visitJumpInsn Opcodes/IFNE $normal-path)
;;                (.visitLdcInsn ".0")
;;                (.visitInsn Opcodes/ARETURN)
;;                (.visitLabel $normal-path)
;;                ;; Normal case
;;                (.visitLdcInsn (int (dec deg-bits)))
;;                (.visitVarInsn Opcodes/ISTORE 2) ;; Index
;;                (.visitLdcInsn (int deg-bits))
;;                (.visitIntInsn Opcodes/NEWARRAY Opcodes/T_BYTE)
;;                (.visitVarInsn Opcodes/ASTORE 3) ;; digits
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $loop-start)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitJumpInsn Opcodes/IFGE $do-a-round)
;;                ;; Prepare text to return.
;;                (.visitVarInsn Opcodes/ALOAD 3)
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "deg_digits_to_text" "([B)Ljava/lang/String;")
;;                (.visitLdcInsn ".")
;;                (.visitInsn Opcodes/SWAP)
;;                (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;")
;;                ;; Trim unnecessary 0s at the end...
;;                (.visitLdcInsn "0*$")
;;                (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "split" "(Ljava/lang/String;)[Ljava/lang/String;")
;;                (.visitLdcInsn (int 0))
;;                (.visitInsn Opcodes/AALOAD)
;;                (.visitInsn Opcodes/ARETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $do-a-round)
;;                (.visitVarInsn Opcodes/LLOAD 0)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                bit-set-64?
;;                (.visitJumpInsn Opcodes/IFEQ $next-iteration)
;;                (.visitLdcInsn (int (dec deg-bits)))
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "deg_digit_power" "(I)[B")
;;                (.visitVarInsn Opcodes/ALOAD 3)
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "add_deg_digit_powers" "([B[B)[B")
;;                (.visitVarInsn Opcodes/ASTORE 3)
;;                (.visitJumpInsn Opcodes/GOTO $next-iteration)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $next-iteration)
;;                ;; Decrement index
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitVarInsn Opcodes/ISTORE 2)
;;                ;; Iterate
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))
;;          _ (let [$loop-start (new Label)
;;                  $do-a-round (new Label)
;;                  $not-set (new Label)
;;                  $next-iteration (new Label)]
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "deg_text_to_digits" "(Ljava/lang/String;)[B" nil nil)
;;                (.visitCode)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitVarInsn Opcodes/ISTORE 1) ;; Index
;;                (.visitLdcInsn (int deg-bits))
;;                (.visitIntInsn Opcodes/NEWARRAY Opcodes/T_BYTE)
;;                (.visitVarInsn Opcodes/ASTORE 2) ;; digits
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $loop-start)
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitJumpInsn Opcodes/IFGE $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 2)
;;                (.visitInsn Opcodes/ARETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/IADD)
;;                (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(II)Ljava/lang/String;")
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Byte" "parseByte" "(Ljava/lang/String;)B")
;;                ;; Set digit
;;                (.visitVarInsn Opcodes/ALOAD 2)
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                swap2x1
;;                (.visitInsn Opcodes/BASTORE)
;;                ;; Decrement index
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitVarInsn Opcodes/ISTORE 1)
;;                ;; Iterate
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))
;;          _ (let [$loop-start (new Label)
;;                  $do-a-round (new Label)
;;                  $is-less-than (new Label)
;;                  $is-equal (new Label)]
;;              ;; [B0 <= [B1
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "deg_digits_lt" "([B[B)Z" nil nil)
;;                (.visitCode)
;;                (.visitLdcInsn (int 0))
;;                (.visitVarInsn Opcodes/ISTORE 2) ;; Index
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $loop-start)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitLdcInsn (int deg-bits))
;;                (.visitJumpInsn Opcodes/IF_ICMPLT $do-a-round)
;;                (.visitLdcInsn false)
;;                (.visitInsn Opcodes/IRETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitInsn Opcodes/BALOAD) ;; {D0}
;;                (.visitVarInsn Opcodes/ALOAD 1)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitInsn Opcodes/BALOAD) ;; {D0, D1}
;;                (.visitInsn Opcodes/DUP2)
;;                (.visitJumpInsn Opcodes/IF_ICMPLT $is-less-than)
;;                (.visitJumpInsn Opcodes/IF_ICMPEQ $is-equal)
;;                ;; Is greater than...
;;                (.visitLdcInsn false)
;;                (.visitInsn Opcodes/IRETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $is-less-than)
;;                (.visitInsn Opcodes/POP2)
;;                (.visitLdcInsn true)
;;                (.visitInsn Opcodes/IRETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $is-equal)
;;                ;; Increment index
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/IADD)
;;                (.visitVarInsn Opcodes/ISTORE 2)
;;                ;; Iterate
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))
;;          _ (let [$loop-start (new Label)
;;                  $do-a-round (new Label)
;;                  $simple-sub (new Label)]
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "deg_digits_sub_once" "([BBI)[B" nil nil)
;;                (.visitCode)
;;                (.visitLabel $loop-start)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitVarInsn Opcodes/ILOAD 2) ;; {target-digit}
;;                (.visitInsn Opcodes/BALOAD)
;;                (.visitVarInsn Opcodes/ILOAD 1) ;; {target-digit, param-digit}
;;                (.visitInsn Opcodes/DUP2)
;;                (.visitJumpInsn Opcodes/IF_ICMPGE $simple-sub)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;; Since $0 < $1
;;                (.visitInsn Opcodes/SWAP)
;;                (.visitInsn Opcodes/ISUB) ;; $1 - $0
;;                (.visitLdcInsn (byte 10))
;;                (.visitInsn Opcodes/SWAP)
;;                (.visitInsn Opcodes/ISUB) ;; 10 - ($1 - $0)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                swap2x1
;;                (.visitInsn Opcodes/BASTORE)
;;                ;; Prepare to iterate...
;;                ;; Decrement index
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitVarInsn Opcodes/ISTORE 2)
;;                ;; Subtract 1 from next digit
;;                (.visitLdcInsn (int 1))
;;                (.visitVarInsn Opcodes/ISTORE 1)
;;                ;; Iterate
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $simple-sub)
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                swap2x1
;;                (.visitInsn Opcodes/BASTORE)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitInsn Opcodes/ARETURN)
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))
;;          _ (let [$loop-start (new Label)
;;                  $do-a-round (new Label)]
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "deg_digits_sub" "([B[B)[B" nil nil)
;;                (.visitCode)
;;                (.visitLdcInsn (int (dec deg-bits)))
;;                (.visitVarInsn Opcodes/ISTORE 2) ;; Index
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $loop-start)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitJumpInsn Opcodes/IFGE $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitInsn Opcodes/ARETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 0) ;; {target-digits}
;;                (.visitVarInsn Opcodes/ALOAD 1)
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitInsn Opcodes/BALOAD) ;; {target-digits, param-digit}
;;                (.visitVarInsn Opcodes/ILOAD 2) ;; {target-digits, param-digit, idx}
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "deg_digits_sub_once" "([BBI)[B")
;;                (.visitVarInsn Opcodes/ASTORE 0) ;; Update target digits
;;                ;; Decrement index
;;                (.visitVarInsn Opcodes/ILOAD 2)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitVarInsn Opcodes/ISTORE 2)
;;                ;; Iterate
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))
;;          _ (let [$from (new Label)
;;                  $to (new Label)
;;                  $handler (new Label)
;;                  $loop-start (new Label)
;;                  $do-a-round (new Label)
;;                  $skip-power (new Label)
;;                  $iterate (new Label)
;;                  $bad-format (new Label)]
;;              (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "decode_deg" "(Ljava/lang/String;)Ljava/lang/Object;" nil nil)
;;                (.visitCode)
;;                ;; Check prefix
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitLdcInsn ".")
;;                (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "startsWith" "(Ljava/lang/String;)Z")
;;                (.visitJumpInsn Opcodes/IFEQ $bad-format)
;;                ;; Check if size is valid
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
;;                (.visitLdcInsn (int (inc deg-bits))) ;; It's increased, to account for the prefix .
;;                (.visitJumpInsn Opcodes/IF_ICMPGT $bad-format)
;;                ;; Initialization
;;                (.visitTryCatchBlock $from $to $handler "java/lang/Exception")
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitLdcInsn (int 1))
;;                (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(I)Ljava/lang/String;")
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "clean_separators" "(Ljava/lang/String;)Ljava/lang/String;")
;;                (.visitLabel $from)
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "deg_text_to_digits" "(Ljava/lang/String;)[B")
;;                (.visitLabel $to)
;;                (.visitVarInsn Opcodes/ASTORE 0) ;; From test to digits...
;;                (.visitLdcInsn (int 0))
;;                (.visitVarInsn Opcodes/ISTORE 1) ;; Index
;;                (.visitLdcInsn (long 0))
;;                (.visitVarInsn Opcodes/LSTORE 2) ;; Output
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $loop-start)
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitLdcInsn (int deg-bits))
;;                (.visitJumpInsn Opcodes/IF_ICMPLT $do-a-round)
;;                (.visitVarInsn Opcodes/LLOAD 2)
;;                &&/wrap-long
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)Ljava/lang/Object;")
;;                (.visitInsn Opcodes/ARETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $do-a-round)
;;                (.visitVarInsn Opcodes/ALOAD 0)
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "deg_digit_power" "(I)[B")
;;                (.visitInsn Opcodes/DUP2)
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "deg_digits_lt" "([B[B)Z")
;;                (.visitJumpInsn Opcodes/IFNE $skip-power)
;;                ;; Subtract power
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "deg_digits_sub" "([B[B)[B")
;;                (.visitVarInsn Opcodes/ASTORE 0)
;;                ;; Set bit on output
;;                (.visitVarInsn Opcodes/LLOAD 2)
;;                (.visitLdcInsn (long 1))
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitLdcInsn (int (dec deg-bits)))
;;                (.visitInsn Opcodes/SWAP)
;;                (.visitInsn Opcodes/ISUB)
;;                (.visitInsn Opcodes/LSHL)
;;                (.visitInsn Opcodes/LOR)
;;                (.visitVarInsn Opcodes/LSTORE 2)
;;                (.visitJumpInsn Opcodes/GOTO $iterate)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $skip-power)
;;                (.visitInsn Opcodes/POP2)
;;                ;; (.visitJumpInsn Opcodes/GOTO $iterate)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $iterate)
;;                (.visitVarInsn Opcodes/ILOAD 1)
;;                (.visitLdcInsn (int 1))
;;                (.visitInsn Opcodes/IADD)
;;                (.visitVarInsn Opcodes/ISTORE 1)
;;                ;; Iterate
;;                (.visitJumpInsn Opcodes/GOTO $loop-start)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $handler)
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
;;                (.visitInsn Opcodes/ARETURN)
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                (.visitLabel $bad-format)
;;                (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
;;                (.visitInsn Opcodes/ARETURN)
;;                (.visitMaxs 0 0)
;;                (.visitEnd)))]
;;     nil))

;; (let [+wrapper-class+ (&host-generics/->bytecode-class-name "java.lang.Long")]
;;   (defn ^:private compile-LuxRT-nat-methods [^ClassWriter =class]
;;     (|let [;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#677
;;            _ (let [$from (new Label)
;;                    $to (new Label)
;;                    $handler (new Label)

;;                    $good-start (new Label)
;;                    $short-enough (new Label)
;;                    $bad-digit (new Label)
;;                    $out-of-bounds (new Label)]
;;                (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "decode_nat" "(Ljava/lang/String;)Ljava/lang/Object;" nil nil)
;;                  (.visitCode)
;;                  (.visitTryCatchBlock $from $to $handler "java/lang/Exception")
;;                  (.visitLabel $from)
;;                  ;; Remove the + at the beginning...
;;                  (.visitVarInsn Opcodes/ALOAD 0)
;;                  (.visitLdcInsn (int 0))
;;                  (.visitLdcInsn (int 1))
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(II)Ljava/lang/String;")
;;                  (.visitLdcInsn "+")
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z")
;;                  (.visitJumpInsn Opcodes/IFNE $good-start)
;;                  ;; Doesn't start with +
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  ;; Starts with +
;;                  (.visitLabel $good-start)
;;                  (.visitVarInsn Opcodes/ALOAD 0)
;;                  (.visitLdcInsn (int 1))
;;                  (.visitVarInsn Opcodes/ALOAD 0)
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(II)Ljava/lang/String;")
;;                  (.visitVarInsn Opcodes/ASTORE 0) ;; Removed the + prefix...
;;                  ;; Begin parsing processs
;;                  (.visitVarInsn Opcodes/ALOAD 0)
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
;;                  (.visitLdcInsn (int 18))
;;                  (.visitJumpInsn Opcodes/IF_ICMPLE $short-enough)
;;                  ;; Too long
;;                  ;; Get prefix...
;;                  (.visitVarInsn Opcodes/ALOAD 0)
;;                  (.visitLdcInsn (int 0))
;;                  (.visitVarInsn Opcodes/ALOAD 0)
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
;;                  (.visitLdcInsn (int 1))
;;                  (.visitInsn Opcodes/ISUB)
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(II)Ljava/lang/String;")
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "parseLong" "(Ljava/lang/String;)J")
;;                  (.visitInsn Opcodes/DUP2) ;; Clone prefix, for later...
;;                  ;; Get last digit...
;;                  (.visitVarInsn Opcodes/ALOAD 0)
;;                  (.visitVarInsn Opcodes/ALOAD 0)
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "length" "()I")
;;                  (.visitLdcInsn (int 1))
;;                  (.visitInsn Opcodes/ISUB)
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "charAt" "(I)C")
;;                  (.visitLdcInsn (int 10))
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Character" "digit" "(CI)I")
;;                  ;; Test last digit...
;;                  (.visitInsn Opcodes/DUP)
;;                  (.visitJumpInsn Opcodes/IFLT $bad-digit)
;;                  ;; Good digit...
;;                  ;; Stack: prefix::L, prefix::L, last-digit::I
;;                  (.visitInsn Opcodes/I2L)
;;                  ;; Build the result...
;;                  swap2
;;                  (.visitLdcInsn (long 10))
;;                  (.visitInsn Opcodes/LMUL)
;;                  (.visitInsn Opcodes/LADD) ;; Stack: prefix::L, result::L
;;                  (.visitInsn Opcodes/DUP2_X2) ;; Stack: result::L, prefix::L, result::L
;;                  swap2 ;; Stack: result::L, result::L, prefix::L
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
;;                  (.visitJumpInsn Opcodes/IFLT $out-of-bounds)
;;                  ;; Within bounds
;;                  ;; Stack: result::L
;;                  &&/wrap-long
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)Ljava/lang/Object;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  ;; Out of bounds
;;                  (.visitLabel $out-of-bounds)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  ;; Bad digit...
;;                  (.visitLabel $bad-digit)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  ;; 18 chars or less
;;                  (.visitLabel $short-enough)
;;                  (.visitVarInsn Opcodes/ALOAD 0)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "parseLong" "(Ljava/lang/String;)J")
;;                  &&/wrap-long
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)Ljava/lang/Object;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  (.visitLabel $to)
;;                  (.visitLabel $handler)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()Ljava/lang/Object;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  (.visitMaxs 0 0)
;;                  (.visitEnd)))
;;            ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#172
;;            _ (let [$too-big (new Label)]
;;                (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "encode_nat" "(J)Ljava/lang/String;" nil nil)
;;                  (.visitCode)
;;                  (.visitLdcInsn "+")
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitLdcInsn (long 0))
;;                  (.visitInsn Opcodes/LCMP)
;;                  (.visitJumpInsn Opcodes/IFLT $too-big)
;;                  ;; then
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "toString" "(J)Ljava/lang/String;")
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  ;; else
;;                  (.visitLabel $too-big)
;;                  ;; Set up parts of the number string...
;;                  ;; First digits
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitLdcInsn (int 1))
;;                  (.visitInsn Opcodes/LUSHR)
;;                  (.visitLdcInsn (long 5))
;;                  (.visitInsn Opcodes/LDIV) ;; quot
;;                  ;; Last digit
;;                  (.visitInsn Opcodes/DUP2)
;;                  (.visitLdcInsn (long 10))
;;                  (.visitInsn Opcodes/LMUL)
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  swap2
;;                  (.visitInsn Opcodes/LSUB) ;; quot, rem
;;                  ;; Conversion to string...
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "toString" "(J)Ljava/lang/String;") ;; quot, rem*
;;                  (.visitInsn Opcodes/DUP_X2);; rem*, quot, rem*
;;                  (.visitInsn Opcodes/POP) ;; rem*, quot
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "toString" "(J)Ljava/lang/String;") ;; rem*, quot*
;;                  (.visitInsn Opcodes/SWAP) ;; quot*, rem*
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;")
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  (.visitMaxs 0 0)
;;                  (.visitEnd)))
;;            ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#215
;;            _ (let [$simple-case (new Label)]
;;                (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;" nil nil)
;;                  (.visitCode)
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitLdcInsn (long 0))
;;                  (.visitInsn Opcodes/LCMP)
;;                  (.visitJumpInsn Opcodes/IFGE $simple-case)
;;                  ;; else
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitLdcInsn (int 32))
;;                  (.visitInsn Opcodes/LUSHR)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;")
;;                  (.visitLdcInsn (int 32))
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "shiftLeft" "(I)Ljava/math/BigInteger;")
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitLdcInsn (int 32))
;;                  (.visitInsn Opcodes/LSHL)
;;                  (.visitLdcInsn (int 32))
;;                  (.visitInsn Opcodes/LUSHR)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;")
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "add" "(Ljava/math/BigInteger;)Ljava/math/BigInteger;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  ;; then
;;                  (.visitLabel $simple-case)
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;")
;;                  (.visitInsn Opcodes/ARETURN)
;;                  (.visitMaxs 0 0)
;;                  (.visitEnd)))
;;            ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java?av=f#1267
;;            _ (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "_compareUnsigned" "(JJ)I" nil nil)
;;                (.visitCode)
;;                (.visitVarInsn Opcodes/LLOAD 0)
;;                (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Long" "MIN_VALUE" "J")
;;                (.visitInsn Opcodes/LADD)
;;                (.visitVarInsn Opcodes/LLOAD 2)
;;                (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Long" "MIN_VALUE" "J")
;;                (.visitInsn Opcodes/LADD)
;;                (.visitInsn Opcodes/LCMP)
;;                (.visitInsn Opcodes/IRETURN)
;;                (.visitMaxs 0 0)
;;                (.visitEnd))
;;            ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#1290
;;            _ (let [$case-1 (new Label)
;;                    $0 (new Label)
;;                    $case-2 (new Label)]
;;                (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "div_nat" "(JJ)J" nil nil)
;;                  (.visitCode)
;;                  ;; Test #1
;;                  (.visitVarInsn Opcodes/LLOAD 2)
;;                  (.visitLdcInsn (long 0))
;;                  (.visitInsn Opcodes/LCMP)
;;                  (.visitJumpInsn Opcodes/IFLT $case-1)
;;                  ;; Test #2
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitLdcInsn (long 0))
;;                  (.visitInsn Opcodes/LCMP)
;;                  (.visitJumpInsn Opcodes/IFGT $case-2)
;;                  ;; Case #3
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
;;                  (.visitVarInsn Opcodes/LLOAD 2)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "divide" "(Ljava/math/BigInteger;)Ljava/math/BigInteger;")
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "longValue" "()J")
;;                  (.visitInsn Opcodes/LRETURN)
;;                  ;; Case #2
;;                  (.visitLabel $case-2)
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitVarInsn Opcodes/LLOAD 2)
;;                  (.visitInsn Opcodes/LDIV)
;;                  (.visitInsn Opcodes/LRETURN)
;;                  ;; Case #1
;;                  (.visitLabel $case-1)
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitVarInsn Opcodes/LLOAD 2)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
;;                  (.visitJumpInsn Opcodes/IFLT $0)
;;                  ;; 1
;;                  (.visitLdcInsn (long 1))
;;                  (.visitInsn Opcodes/LRETURN)
;;                  ;; 0
;;                  (.visitLabel $0)
;;                  (.visitLdcInsn (long 0))
;;                  (.visitInsn Opcodes/LRETURN)
;;                  (.visitMaxs 0 0)
;;                  (.visitEnd)))
;;            ;; http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/Long.java#1323
;;            _ (let [$test-2 (new Label)
;;                    $case-2 (new Label)]
;;                (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "rem_nat" "(JJ)J" nil nil)
;;                  (.visitCode)
;;                  ;; Test #1
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitLdcInsn (long 0))
;;                  (.visitInsn Opcodes/LCMP)
;;                  (.visitJumpInsn Opcodes/IFLE $test-2)
;;                  (.visitVarInsn Opcodes/LLOAD 2)
;;                  (.visitLdcInsn (long 0))
;;                  (.visitInsn Opcodes/LCMP)
;;                  (.visitJumpInsn Opcodes/IFLE $test-2)
;;                  ;; Case #1
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitVarInsn Opcodes/LLOAD 2)
;;                  (.visitInsn Opcodes/LREM)
;;                  (.visitInsn Opcodes/LRETURN)
;;                  ;; Test #2
;;                  (.visitLabel $test-2)
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitVarInsn Opcodes/LLOAD 2)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
;;                  (.visitJumpInsn Opcodes/IFLT $case-2)
;;                  ;; Case #3
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
;;                  (.visitVarInsn Opcodes/LLOAD 2)
;;                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_toUnsignedBigInteger" "(J)Ljava/math/BigInteger;")
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "remainder" "(Ljava/math/BigInteger;)Ljava/math/BigInteger;")
;;                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/math/BigInteger" "longValue" "()J")
;;                  (.visitInsn Opcodes/LRETURN)
;;                  ;; Case #2
;;                  (.visitLabel $case-2)
;;                  (.visitVarInsn Opcodes/LLOAD 0)
;;                  (.visitInsn Opcodes/LRETURN)
;;                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  (.visitMaxs 0 0)
;;                  (.visitEnd)))]
;;       nil)))

(def ^:private adt-methods
  {:product_getLeft (str "(function product_getLeft(product,index) {"
                         "var index_min_length = (index+1);"
                         "if(product.length > index_min_length) {"
                         ;; No need for recursion
                         "return product[index];"
                         "}"
                         "else {"
                         ;; Needs recursion
                         "return product_getLeft(product[product.length - 1], (index_min_length - product.length));"
                         "}"
                         "})")
   :product_getRight (str "(function product_getRight(product,index) {"
                          "var index_min_length = (index+1);"
                          "if(product.length === index_min_length) {"
                          ;; Last element.
                          "return product[index];"
                          "}"
                          "else if(product.length < index_min_length) {"
                          ;; Needs recursion
                          "return product_getRight(product[product.length - 1], (index_min_length - product.length));"
                          "}"
                          "else {"
                          ;; Must slice
                          "return product.slice(index);"
                          "}"
                          "})")
   :sum_get (str "(function sum_get(sum,wantedTag,wantsLast) {"
                 "if(sum[0] === wantedTag && sum[1] === wantsLast) {"
                 ;; Exact match.
                 "return sum[2];"
                 "}"
                 "else if(sum[0] < wantedTag || sum[1] !== wantsLast) {"
                 "if(sum[1]) {"
                 ;; Must recurse.
                 "return sum_get(sum[2], (wantedTag - sum[0]), wantsLast);"
                 "}"
                 ;; Not match.
                 "else { return null; }"
                 "}"
                 ;; Not match.
                 "else { return null; }"
                 "})")})

(def LuxRT "LuxRT")

(def compile-LuxRT
  (|do [_ (return nil)
        :let [rt-object (str "{" (->> adt-methods
                                      (map (fn [[key val]]
                                             (str (name key) ":" val)))
                                      (interpose ",")
                                      (reduce str ""))
                             "}")]]
    (&&/save-js! LuxRT
                 (str "var " LuxRT " = " rt-object ";"))))
