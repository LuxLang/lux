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

;; Runtime infrastructure
(defn ^:private compile-LuxRT-adt-methods [^ClassWriter =class]
  (|let [lefts #(doto ^MethodVisitor %
                  (.visitVarInsn Opcodes/ILOAD 1))
         tuple-size #(doto ^MethodVisitor %
                       (.visitVarInsn Opcodes/ALOAD 0)
                       (.visitInsn Opcodes/ARRAYLENGTH))
         last-right #(doto ^MethodVisitor %
                       tuple-size
                       (.visitLdcInsn (int 1))
                       (.visitInsn Opcodes/ISUB))
         sub-lefts #(doto ^MethodVisitor %
                      lefts
                      last-right
                      (.visitInsn Opcodes/ISUB))
         sub-tuple #(doto ^MethodVisitor %
                      (.visitVarInsn Opcodes/ALOAD 0)
                      last-right
                      (.visitInsn Opcodes/AALOAD)
                      (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;"))
         recurI (fn [$begin]
                  #(doto ^MethodVisitor %
                     sub-lefts (.visitVarInsn Opcodes/ISTORE 1)
                     sub-tuple (.visitVarInsn Opcodes/ASTORE 0)
                     (.visitJumpInsn Opcodes/GOTO $begin)))
         _ (let [$begin (new Label)
                 $recursive (new Label)
                 left-index lefts
                 left-access #(doto ^MethodVisitor %
                                (.visitVarInsn Opcodes/ALOAD 0)
                                left-index
                                (.visitInsn Opcodes/AALOAD))]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "tuple_left" "([Ljava/lang/Object;I)Ljava/lang/Object;" nil nil)
               (.visitCode)
               (.visitLabel $begin)
               lefts last-right (.visitJumpInsn Opcodes/IF_ICMPGE $recursive)
               left-access
               (.visitInsn Opcodes/ARETURN)
               (.visitLabel $recursive)
               ((recurI $begin))
               (.visitMaxs 0 0)
               (.visitEnd)))
         _ (let [$begin (new Label)
                 $not-last (new Label)
                 $must-copy (new Label)
                 right-index #(doto ^MethodVisitor %
                                lefts
                                (.visitLdcInsn (int 1))
                                (.visitInsn Opcodes/IADD))
                 right-access #(doto ^MethodVisitor %
                                 (.visitVarInsn Opcodes/ALOAD 0)
                                 (.visitInsn Opcodes/SWAP)
                                 (.visitInsn Opcodes/AALOAD))
                 sub-right #(doto ^MethodVisitor %
                              (.visitVarInsn Opcodes/ALOAD 0)
                              right-index
                              tuple-size
                              (.visitMethodInsn Opcodes/INVOKESTATIC "java/util/Arrays" "copyOfRange" "([Ljava/lang/Object;II)[Ljava/lang/Object;"))]
             (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "tuple_right" "([Ljava/lang/Object;I)Ljava/lang/Object;" nil nil)
               (.visitCode)
               (.visitLabel $begin)
               last-right right-index
               (.visitInsn Opcodes/DUP2) (.visitJumpInsn Opcodes/IF_ICMPNE $not-last)
               right-access
               (.visitInsn Opcodes/ARETURN)
               (.visitLabel $not-last)
               (.visitJumpInsn Opcodes/IF_ICMPGT $must-copy)
               ;; Must recurse
               ((recurI $begin))
               (.visitLabel $must-copy)
               sub-right
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
                             ;; (.visitInsn Opcodes/POP2)
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
               ;; (.visitInsn Opcodes/POP2)
               (.visitVarInsn Opcodes/ALOAD 0)
               (.visitLdcInsn (int 2))
               (.visitInsn Opcodes/AALOAD)
               (.visitInsn Opcodes/ARETURN)
               (.visitLabel $shorten)
               (.visitVarInsn Opcodes/ALOAD 2)
               (.visitJumpInsn Opcodes/IFNULL $not-right)
               ;; (.visitInsn Opcodes/POP2)
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

(defn ^:private swap2x1 [^MethodVisitor =method]
  (doto =method
    ;; X1, Y2
    (.visitInsn Opcodes/DUP2_X1) ;; Y2, X1, Y2
    (.visitInsn Opcodes/POP2) ;; Y2, X1
    ))

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

(defn peekI [^MethodVisitor writer]
  (doto writer
    (.visitLdcInsn (int 0))
    (.visitInsn Opcodes/AALOAD)))

(defn popI [^MethodVisitor writer]
  (doto writer
    (.visitLdcInsn (int 1))
    (.visitInsn Opcodes/AALOAD)
    (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")))

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
             (.visitLdcInsn (int 1))
             (.visitVarInsn Opcodes/ALOAD 0)
             (.visitInsn Opcodes/AASTORE)
             (.visitInsn Opcodes/DUP)
             (.visitLdcInsn (int 0))
             (.visitVarInsn Opcodes/ALOAD 1)
             (.visitInsn Opcodes/AASTORE)
             (.visitInsn Opcodes/ARETURN)
             (.visitMaxs 0 0)
             (.visitEnd))]
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
                      make-string-writerI (fn [^MethodVisitor _method_]
                                            (doto _method_
                                              (.visitTypeInsn Opcodes/NEW "java/io/StringWriter")
                                              (.visitInsn Opcodes/DUP)
                                              (.visitMethodInsn Opcodes/INVOKESPECIAL "java/io/StringWriter" "<init>" "()V")))
                      make-print-writerI (fn [^MethodVisitor _method_]
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
                  (compile-LuxRT-int-methods)
                  (compile-LuxRT-frac-methods))]]
    (&&/save-class! (second (string/split &&/lux-utils-class #"/"))
                    (.toByteArray (doto =class .visitEnd)))))
