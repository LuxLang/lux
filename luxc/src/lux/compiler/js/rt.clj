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
  {"product_getLeft" (str "(function product_getLeft(product,index) {"
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
   "product_getRight" (str "(function product_getRight(product,index) {"
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
   "sum_get" (str "(function sum_get(sum,wantedTag,wantsLast) {"
                  "if(sum[0] === wantedTag && sum[1] === wantsLast) {"
                  ;; Exact match.
                  "return sum[2];"
                  "}"
                  "else if(sum[0] < wantedTag || sum[1] !== wantsLast) {"
                  "if(sum[1]) {"
                  ;; Must recurse.
                  "return sum_get(sum[2], (wantedTag - sum[0]), wantsLast);"
                  "}"
                  ;; No match.
                  "else { return null; }"
                  "}"
                  ;; No match.
                  "else { return null; }"
                  "})")
   })

(def ^:private i64-methods
  {"makeI64" (str "(function makeI64(high,low) {"
                  "return { H: (high|0), L: (low|0)};"
                  "})")
   "MIN_VALUE" "{ H: 0x80000000, L: 0}"
   "ONE" "{ H: 0, L: 1}"
   "notI64" (str "(function notI64(i64) {"
                 "return LuxRT.makeI64(~i64.H,~i64.L);"
                 "})")
   "negateI64" (str "(function negateI64(i64) {"
                    "return LuxRT.addI64(LuxRT.notI64(i64),LuxRT.makeI64(0,1));"
                    "})")
   "eqI64" (str "(function eqI64(l,r) {"
                "return (l.H === r.H) && (l.L === r.L);"
                "})")
   "addI64" (str "(function addI64(l,r) {"
                 "var l48 = l.H >>> 16;"
                 "var l32 = l.H & 0xFFFF;"
                 "var l16 = l.L >>> 16;"
                 "var l00 = l.L & 0xFFFF;"

                 "var r48 = r.H >>> 16;"
                 "var r32 = r.H & 0xFFFF;"
                 "var r16 = r.L >>> 16;"
                 "var r00 = r.L & 0xFFFF;"

                 "var x48 = 0, x32 = 0, x16 = 0, x00 = 0;"
                 "x00 += l00 + r00;"
                 "x16 += x00 >>> 16;"
                 "x00 &= 0xFFFF;"
                 "x16 += l16 + r16;"
                 "x32 += x16 >>> 16;"
                 "x16 &= 0xFFFF;"
                 "x32 += l32 + r32;"
                 "x48 += x32 >>> 16;"
                 "x32 &= 0xFFFF;"
                 "x48 += l48 + r48;"
                 "x48 &= 0xFFFF;"

                 "return LuxRT.makeI64((x48 << 16) | x32, (x16 << 16) | x00);"
                 "})")
   "subI64" (str "(function subI64(l,r) {"
                 "return LuxRT.addI64(l,LuxRT.negateI64(r));"
                 "})")
   "mulI64" (str "(function mulI64(l,r) {"
                 "if (l.H < 0) {"
                 (str "if (r.H < 0) {"
                      ;; Both are negative
                      "return mulI64(LuxRT.negateI64(l),LuxRT.negateI64(r));"
                      "}"
                      "else {"
                      ;; Left is negative
                      "return LuxRT.negateI64(mulI64(LuxRT.negateI64(l),r));"
                      "}")
                 "}"
                 "else if (r.H < 0) {"
                 ;; Right is negative
                 "return LuxRT.negateI64(mulI64(l,LuxRT.negateI64(r)));"
                 "}"
                 ;; Both are positive
                 "else {"
                 "var l48 = l.H >>> 16;"
                 "var l32 = l.H & 0xFFFF;"
                 "var l16 = l.L >>> 16;"
                 "var l00 = l.L & 0xFFFF;"

                 "var r48 = r.H >>> 16;"
                 "var r32 = r.H & 0xFFFF;"
                 "var r16 = r.L >>> 16;"
                 "var r00 = r.L & 0xFFFF;"

                 "var x48 = 0, x32 = 0, x16 = 0, x00 = 0;"
                 "x00 += l00 * r00;"
                 "x16 += x00 >>> 16;"
                 "x00 &= 0xFFFF;"
                 "x16 += l16 * r00;"
                 "x32 += x16 >>> 16;"
                 "x16 &= 0xFFFF;"
                 "x16 += l00 * r16;"
                 "x32 += x16 >>> 16;"
                 "x16 &= 0xFFFF;"
                 "x32 += l32 * r00;"
                 "x48 += x32 >>> 16;"
                 "x32 &= 0xFFFF;"
                 "x32 += l16 * r16;"
                 "x48 += x32 >>> 16;"
                 "x32 &= 0xFFFF;"
                 "x32 += l00 * r32;"
                 "x48 += x32 >>> 16;"
                 "x32 &= 0xFFFF;"
                 "x48 += (l48 * r00) + (l32 * r16) + (l16 * r32) + (l00 * r48);"
                 "x48 &= 0xFFFF;"

                 "return LuxRT.makeI64((x48 << 16) | x32, (x16 << 16) | x00);"
                 "}"
                 "})")
   "divI64" (str "(function divI64(l,r) {"
                 (str "if((r.H === 0) && (r.L === 0)) {"
                      ;; Special case: R = 0
                      "throw Error('division by zero');"
                      "}"
                      "else if((l.H === 0) && (l.L === 0)) {"
                      ;; Special case: L = 0
                      "return l;"
                      "}")
                 (str "if(LuxRT.eqI64(l,LuxRT.MIN_VALUE)) {"
                      ;; Special case: L = MIN
                      (str "if(LuxRT.eqI64(r,LuxRT.ONE) || LuxRT.eqI64(r,LuxRT.negateI64(LuxRT.ONE))) {"
                           ;; Special case: L = MIN, R = 1|-1
                           "return LuxRT.MIN_VALUE;"
                           "}"
                           ;; Special case: L = R = MIN
                           "else if(LuxRT.eqI64(r,LuxRT.MIN_VALUE)) {"
                           "return LuxRT.ONE;"
                           "}"
                           ;; Special case: L = MIN
                           "else {"
                           "var halfL = LuxRT.shrI64(l,LuxRT.ONE);"
                           "var approx = LuxRT.shlI64(LuxRT.divI64(halfL,r),LuxRT.ONE);"
                           (str "if((approx.H === 0) && (approx.L === 0)) {"
                                (str "if(r.H < 0) {"
                                     "return LuxRT.ONE;"
                                     "}"
                                     "else {"
                                     "return LuxRT.negateI64(LuxRT.ONE);"
                                     "}")
                                "}"
                                "else {"
                                "var rem = LuxRT.subI64(l,LuxRT.mulI64(r,approx));"
                                "return LuxRT.addI64(approx,LuxRT.divI64(rem,r));"
                                "}")
                           "}")
                      "}"
                      "else if(LuxRT.eqI64(r,LuxRT.MIN_VALUE)) {"
                      ;; Special case: R = MIN
                      "return LuxRT.makeI64(0,0);"
                      "}")
                 ;; Special case: negatives
                 (str "if(l.H < 0) {"
                      (str "if(r.H < 0) {"
                           ;; Both are negative
                           "return LuxRT.divI64(LuxRT.negateI64(l),LuxRT.negateI64(r));"
                           "}"
                           "else {"
                           ;; Only L is negative
                           "return LuxRT.negateI64(LuxRT.divI64(LuxRT.negateI64(l),r));"
                           "}")
                      "}"
                      "else if(r.H < 0) {"
                      ;; R is negative
                      "return LuxRT.negateI64(LuxRT.divI64(l,LuxRT.negateI64(r)));"
                      "}")
                 ;; Common case
                 (str "var res = { H: 0, L: 0};"
                      "var rem = l;"
                      (str "while(LuxRT.ltI64(r,rem) || LuxRT.eqI64(r,rem)) {"
                           "var approx = Math.max(1, Math.floor(LuxRT.toNumberI64(rem) / LuxRT.toNumberI64(r)));"
                           "var log2 = Math.ceil(Math.log(approx) / Math.LN2);"
                           "var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);"
                           "var approxRes = LuxRT.fromNumberI64(approx);"
                           "var approxRem = LuxRT.mulI64(approxRes,r);"
                           (str "while((approxRem.H < 0) || LuxRT.ltI64(rem,approxRem)) {"
                                "approx -= delta;"
                                "approxRes = LuxRT.fromNumberI64(approx);"
                                "approxRem = LuxRT.mulI64(approxRes,r);"
                                "}")
                           (str "if((approxRes.H === 0) && (approxRes.L === 0)) {"
                                "approxRes = LuxRT.ONE;"
                                "}")
                           "res = LuxRT.addI64(res,approxRes);"
                           "rem = LuxRT.subI64(rem,approxRem);"
                           "}")
                      "return res;")
                 "})")
   "remI64" (str "(function remI64(l,r) {"
                 "return LuxRT.subI64(l,LuxRT.mulI64(LuxRT.divI64(l,r),r));"
                 "})")
   "encodeI64" (str "(function encodeI64(input) {"
                    ;; If input = 0
                    (str "if((input.H === 0) && (input.L === 0)) {"
                         "return '0';"
                         "}")
                    ;; If input < 0
                    (str "if(input.H < 0) {"
                         (str "if(LuxRT.eqI64(input,LuxRT.MIN_VALUE)) {"
                              "var radix = LuxRT.makeI64(0,10);"
                              "var div = LuxRT.divI64(input,radix);"
                              "var rem = LuxRT.subI64(LuxRT.mulI64(div,radix),input);"
                              "return LuxRT.encodeI64(div).concat(rem.L+'');"
                              "}")
                         "}"
                         "else {"
                         "return '-'.concat(LuxRT.encodeI64(LuxRT.negateI64(input)));"
                         "}")
                    ;; If input > 0
                    (str "var chunker = LuxRT.makeI64(0,1000000);"
                         "var rem = input;"
                         "var result = '';"
                         "while (true) {"
                         (str "var remDiv = LuxRT.divI64(rem,chunker);"
                              "var chunk = LuxRT.subI64(rem,LuxRT.mulI64(remDiv,chunker));"
                              "var digits = (chunk.L >>> 0)+'';"
                              "rem = remDiv;"
                              (str "if((rem.H === 0) && (rem.L === 0)) {"
                                   "return digits.concat(result);"
                                   "}"
                                   "else {"
                                   (str "while (digits.length < 6) {"
                                        "digits = '0' + digits;"
                                        "}")
                                   "result = '' + digits + result;"
                                   "}"))
                         "}")
                    "})")
   "ltI64" (str "(function ltI64(l,r) {"
                "var ln = l.H < 0;"
                "var rn = r.H < 0;"
                "if(ln && !rn) { return true; }"
                "if(!ln && rn) { return false; }"
                "return (LuxRT.subI64(l,r).H < 0);"
                "})")
   })

(def ^:private n64-methods
  {"encodeN64" (str "(function encodeN64(input) {"
                    (str "if(input.H < 0) {"
                         ;; Too big
                         "var lastDigit = LuxRT.remI64(input, LuxRT.makeI64(0,10));"
                         "var minusLastDigit = LuxRT.divI64(input, LuxRT.makeI64(0,10));"
                         "return '+'.concat(LuxRT.encodeI64(minusLastDigit)).concat(LuxRT.encodeI64(lastDigit));"
                         "}"
                         "else {"
                         ;; Small enough
                         "return '+'.concat(LuxRT.encodeI64(input));"
                         "}")
                    "})")
   })

(def ^:private io-methods
  {"log" (str "(function log(message) {"
              "console.log(message);"
              (str "return " &&/unit ";")
              "})")
   })

(def LuxRT "LuxRT")

(def compile-LuxRT
  (|do [_ (&&/run-js! "var console = { log: print };")
        :let [rt-object (str "{" (->> (merge adt-methods
                                             i64-methods
                                             n64-methods
                                             io-methods)
                                      (map (fn [[key val]]
                                             (str key ":" val)))
                                      (interpose ",")
                                      (reduce str ""))
                             "}")]]
    (&&/save-js! LuxRT
                 (str "var " LuxRT " = " rt-object ";"))))
