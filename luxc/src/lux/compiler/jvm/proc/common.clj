(ns lux.compiler.jvm.proc.common
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

;; [Resources]
(defn ^:private compile-array-new [compile ?values special-args]
  (|do [:let [(&/$Cons ?length (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?length)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        :let [_ (.visitTypeInsn *writer* Opcodes/ANEWARRAY "java/lang/Object")]]
    (return nil)))

(defn ^:private compile-array-get [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Nil))) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        array-type (&host/->java-sig (&a/expr-type* ?array))
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST array-type)]
        _ (compile ?idx)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        :let [_ (.visitInsn *writer* Opcodes/AALOAD)]
        :let [$is-null (new Label)
              $end (new Label)
              _ (doto *writer*
                  (.visitInsn Opcodes/DUP)
                  (.visitJumpInsn Opcodes/IFNULL $is-null)
                  (.visitLdcInsn (int 1))
                  (.visitLdcInsn "")
                  (.visitInsn Opcodes/DUP2_X1) ;; I?2I?
                  (.visitInsn Opcodes/POP2) ;; I?2
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $is-null)
                  (.visitInsn Opcodes/POP)
                  (.visitLdcInsn (int 0))
                  (.visitInsn Opcodes/ACONST_NULL)
                  (.visitLdcInsn &/unit-tag)
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")
                  (.visitLabel $end))]]
    (return nil)))

(defn ^:private compile-array-put [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil)))) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?array)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
                  (.visitInsn Opcodes/DUP))]
        _ (compile ?idx)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        _ (compile ?elem)
        :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
    (return nil)))

(defn ^:private compile-array-remove [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Nil))) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?array)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
                  (.visitInsn Opcodes/DUP))]
        _ (compile ?idx)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/ACONST_NULL)
                  (.visitInsn Opcodes/AASTORE))]]
    (return nil)))

(defn ^:private compile-array-size [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Nil)) ?values
              ;; (&/$Nil) special-args
              ]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST "[Ljava/lang/Object;")]
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/ARRAYLENGTH)
                  (.visitInsn Opcodes/I2L)
                  &&/wrap-long)]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?input (&/$Cons ?mask (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?input)
          :let [_ (&&/unwrap-long *writer*)]
          _ (compile ?mask)
          :let [_ (&&/unwrap-long *writer*)]
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    &&/wrap-long)]]
      (return nil)))

  ^:private compile-bit-and Opcodes/LAND
  ^:private compile-bit-or  Opcodes/LOR
  ^:private compile-bit-xor Opcodes/LXOR
  )

(defn ^:private compile-bit-count [compile ?values special-args]
  (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?input)
        :let [_ (&&/unwrap-long *writer*)]
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "bitCount" "(J)I")
                  (.visitInsn Opcodes/I2L)
                  &&/wrap-long)]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?input (&/$Cons ?shift (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?input)
          :let [_ (&&/unwrap-long *writer*)]
          _ (compile ?shift)
          :let [_ (doto *writer*
                    &&/unwrap-long
                    (.visitInsn Opcodes/L2I))]
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    &&/wrap-long)]]
      (return nil)))

  ^:private compile-bit-shift-left           Opcodes/LSHL
  ^:private compile-bit-shift-right          Opcodes/LSHR
  ^:private compile-bit-unsigned-shift-right Opcodes/LUSHR
  )

(defn ^:private compile-lux-== [compile ?values special-args]
  (|do [:let [(&/$Cons ?left (&/$Cons ?right (&/$Nil))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?left)
        _ (compile ?right)
        :let [$then (new Label)
              $end (new Label)
              _ (doto *writer*
                  (.visitJumpInsn Opcodes/IF_ACMPEQ $then)
                  ;; else
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Boolean" "FALSE" "Ljava/lang/Boolean;")
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $then)
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/Boolean" "TRUE" "Ljava/lang/Boolean;")
                  (.visitLabel $end))]]
    (return nil)))

(do-template [<name> <opcode> <unwrap> <wrap>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    <unwrap>)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    <unwrap>)
                _ (doto *writer*
                    (.visitInsn <opcode>)
                    <wrap>)]]
      (return nil)))

  ^:private compile-int-add   Opcodes/LADD &&/unwrap-long &&/wrap-long
  ^:private compile-int-sub   Opcodes/LSUB &&/unwrap-long &&/wrap-long
  ^:private compile-int-mul   Opcodes/LMUL &&/unwrap-long &&/wrap-long
  ^:private compile-int-div   Opcodes/LDIV &&/unwrap-long &&/wrap-long
  ^:private compile-int-rem   Opcodes/LREM &&/unwrap-long &&/wrap-long
  
  ^:private compile-nat-add   Opcodes/LADD &&/unwrap-long &&/wrap-long
  ^:private compile-nat-sub   Opcodes/LSUB &&/unwrap-long &&/wrap-long
  ^:private compile-nat-mul   Opcodes/LMUL &&/unwrap-long &&/wrap-long

  ^:private compile-deg-add   Opcodes/LADD &&/unwrap-long &&/wrap-long
  ^:private compile-deg-sub   Opcodes/LSUB &&/unwrap-long &&/wrap-long
  ^:private compile-deg-rem   Opcodes/LSUB &&/unwrap-long &&/wrap-long
  ^:private compile-deg-scale Opcodes/LMUL &&/unwrap-long &&/wrap-long

  ^:private compile-real-add   Opcodes/DADD &&/unwrap-double &&/wrap-double
  ^:private compile-real-sub   Opcodes/DSUB &&/unwrap-double &&/wrap-double
  ^:private compile-real-mul   Opcodes/DMUL &&/unwrap-double &&/wrap-double
  ^:private compile-real-div   Opcodes/DDIV &&/unwrap-double &&/wrap-double
  ^:private compile-real-rem   Opcodes/DREM &&/unwrap-double &&/wrap-double
  )

(do-template [<name> <comp-method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    &&/unwrap-long)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    &&/unwrap-long)
                _ (doto *writer*
                    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <comp-method> "(JJ)J")
                    (&&/wrap-long))]]
      (return nil)))

  ^:private compile-nat-div "div_nat"
  ^:private compile-nat-rem "rem_nat"
  )

(do-template [<name> <cmpcode> <cmp-output> <unwrap>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    <unwrap>)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    <unwrap>)
                $then (new Label)
                $end (new Label)
                _ (doto *writer*
                    (.visitInsn <cmpcode>)
                    (.visitLdcInsn (int <cmp-output>))
                    (.visitJumpInsn Opcodes/IF_ICMPEQ $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE"  (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitJumpInsn Opcodes/GOTO $end)
                    (.visitLabel $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE" (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitLabel $end))]]
      (return nil)))

  ^:private compile-int-eq  Opcodes/LCMP   0 &&/unwrap-long
  ^:private compile-int-lt  Opcodes/LCMP  -1 &&/unwrap-long

  ^:private compile-real-eq Opcodes/DCMPG  0 &&/unwrap-double
  ^:private compile-real-lt Opcodes/DCMPG -1 &&/unwrap-double
  )

(do-template [<name> <cmp-output>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    &&/unwrap-long)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    &&/unwrap-long)
                $then (new Label)
                $end (new Label)
                _ (doto *writer*
                    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
                    (.visitLdcInsn (int <cmp-output>))
                    (.visitJumpInsn Opcodes/IF_ICMPEQ $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE"  (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitJumpInsn Opcodes/GOTO $end)
                    (.visitLabel $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE" (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitLabel $end))]]
      (return nil)))

  ^:private compile-nat-eq   0
  
  ^:private compile-deg-eq  0
  ^:private compile-deg-lt -1
  )

(defn ^:private compile-nat-lt [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?x)
        :let [_ (doto *writer*
                  &&/unwrap-long)]
        _ (compile ?y)
        :let [_ (doto *writer*
                  &&/unwrap-long)
              $then (new Label)
              $end (new Label)
              _ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
                  (.visitLdcInsn (int -1))
                  (.visitJumpInsn Opcodes/IF_ICMPEQ $then)
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE"  (&host-generics/->type-signature "java.lang.Boolean"))
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $then)
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE" (&host-generics/->type-signature "java.lang.Boolean"))
                  (.visitLabel $end))]]
    (return nil)))

(do-template [<name> <instr> <wrapper>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Nil) ?values]
          ^MethodVisitor *writer* &/get-writer
          :let [_ (doto *writer*
                    <instr>
                    <wrapper>)]]
      (return nil)))

  ^:private compile-nat-min-value (.visitLdcInsn 0)  &&/wrap-long
  ^:private compile-nat-max-value (.visitLdcInsn -1) &&/wrap-long

  ^:private compile-deg-min-value (.visitLdcInsn 0)  &&/wrap-long
  ^:private compile-deg-max-value (.visitLdcInsn -1) &&/wrap-long
  )

(do-template [<encode-name> <encode-method> <decode-name> <decode-method>]
  (do (defn <encode-name> [compile ?values special-args]
        (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
              ^MethodVisitor *writer* &/get-writer
              _ (compile ?x)
              :let [_ (doto *writer*
                        &&/unwrap-long
                        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <encode-method> "(J)Ljava/lang/String;"))]]
          (return nil)))

    (let [+wrapper-class+ (&host-generics/->bytecode-class-name "java.lang.String")]
      (defn <decode-name> [compile ?values special-args]
        (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
              ^MethodVisitor *writer* &/get-writer
              _ (compile ?x)
              :let [_ (doto *writer*
                        (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                        (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <decode-method> "(Ljava/lang/String;)Ljava/lang/Object;"))]]
          (return nil)))))

  ^:private compile-nat-encode "encode_nat" ^:private compile-nat-decode "decode_nat"
  ^:private compile-deg-encode "encode_deg" ^:private compile-deg-decode "decode_deg"
  )

(defn ^:private compile-int-encode [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?x)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "toString" "(J)Ljava/lang/String;"))]]
    (return nil)))

(defn ^:private compile-real-encode [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?x)
        :let [_ (doto *writer*
                  &&/unwrap-double
                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Double" "toString" "(D)Ljava/lang/String;"))]]
    (return nil)))

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    &&/unwrap-long)]
          _ (compile ?y)
          :let [_ (doto *writer*
                    &&/unwrap-long)]
          :let [_ (doto *writer*
                    (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <method> "(JJ)J")
                    &&/wrap-long)]]
      (return nil)))

  ^:private compile-deg-mul   "mul_deg"
  ^:private compile-deg-div   "div_deg"
  )

(do-template [<name> <class> <method> <sig> <unwrap> <wrap>]
  (let [+wrapper-class+ (&host-generics/->bytecode-class-name <class>)]
    (defn <name> [compile ?values special-args]
      (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
            ^MethodVisitor *writer* &/get-writer
            _ (compile ?x)
            :let [_ (doto *writer*
                      <unwrap>
                      (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <method> <sig>)
                      <wrap>)]]
        (return nil))))

  ^:private compile-deg-to-real "java.lang.Long"   "deg-to-real" "(J)D" &&/unwrap-long   &&/wrap-double
  ^:private compile-real-to-deg "java.lang.Double" "real-to-deg" "(D)J" &&/unwrap-double &&/wrap-long
  )

(let [widen (fn [^MethodVisitor *writer*]
              (doto *writer*
                (.visitInsn Opcodes/I2L)))
      shrink (fn [^MethodVisitor *writer*]
               (doto *writer*
                 (.visitInsn Opcodes/L2I)
                 (.visitInsn Opcodes/I2C)))]
  (do-template [<name> <unwrap> <wrap> <adjust>]
    (defn <name> [compile ?values special-args]
      (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
            ^MethodVisitor *writer* &/get-writer
            _ (compile ?x)
            :let [_ (doto *writer*
                      <unwrap>
                      <adjust>
                      <wrap>)]]
        (return nil)))

    ^:private compile-nat-to-char &&/unwrap-long &&/wrap-char shrink
    ^:private compile-char-to-nat &&/unwrap-char &&/wrap-long widen
    ))

(defn ^:private compile-char-to-text [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?x)
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Object" "toString" "()Ljava/lang/String;"))]]
    (return nil)))

(do-template [<name>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)]
      (return nil)))

  ^:private compile-nat-to-int
  ^:private compile-int-to-nat
  )

(defn compile-text-eq [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?x)
        _ (compile ?y)
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z")
                  (&&/wrap-boolean))]]
    (return nil)))

(defn compile-text-append [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?x)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String"))]
        _ (compile ?y)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String"))]
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;"))]]
    (return nil)))

(defn compile-io-log! [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;"))]
        _ (compile ?x)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String")
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "println" "(Ljava/lang/String;)V")
                  (.visitLdcInsn &/unit-tag))]]
    (return nil)))

(defn compile-proc [compile proc-category proc-name ?values special-args]
  (case proc-category
    "lux"
    (case proc-name
      "=="                   (compile-lux-== compile ?values special-args))

    "io"
    (case proc-name
      "log!"                 (compile-io-log! compile ?values special-args))

    "text"
    (case proc-name
      "="                    (compile-text-eq compile ?values special-args)
      "append"               (compile-text-append compile ?values special-args))
    
    "bit"
    (case proc-name
      "count"                (compile-bit-count compile ?values special-args)
      "and"                  (compile-bit-and compile ?values special-args)
      "or"                   (compile-bit-or compile ?values special-args)
      "xor"                  (compile-bit-xor compile ?values special-args)
      "shift-left"           (compile-bit-shift-left compile ?values special-args)
      "shift-right"          (compile-bit-shift-right compile ?values special-args)
      "unsigned-shift-right" (compile-bit-unsigned-shift-right compile ?values special-args))
    
    "array"
    (case proc-name
      "new" (compile-array-new compile ?values special-args)
      "get" (compile-array-get compile ?values special-args)
      "put" (compile-array-put compile ?values special-args)
      "remove" (compile-array-remove compile ?values special-args)
      "size" (compile-array-size compile ?values special-args))

    "nat"
    (case proc-name
      "+"         (compile-nat-add compile ?values special-args)
      "-"         (compile-nat-sub compile ?values special-args)
      "*"         (compile-nat-mul compile ?values special-args)
      "/"         (compile-nat-div compile ?values special-args)
      "%"         (compile-nat-rem compile ?values special-args)
      "="         (compile-nat-eq compile ?values special-args)
      "<"         (compile-nat-lt compile ?values special-args)
      "encode"    (compile-nat-encode compile ?values special-args)
      "decode"    (compile-nat-decode compile ?values special-args)
      "max-value" (compile-nat-max-value compile ?values special-args)
      "min-value" (compile-nat-min-value compile ?values special-args)
      "to-int"    (compile-nat-to-int compile ?values special-args)
      "to-char"   (compile-nat-to-char compile ?values special-args)
      )
    
    "deg"
    (case proc-name
      "+"         (compile-deg-add compile ?values special-args)
      "-"         (compile-deg-sub compile ?values special-args)
      "*"         (compile-deg-mul compile ?values special-args)
      "/"         (compile-deg-div compile ?values special-args)
      "%"         (compile-deg-rem compile ?values special-args)
      "="         (compile-deg-eq compile ?values special-args)
      "<"         (compile-deg-lt compile ?values special-args)
      "encode"    (compile-deg-encode compile ?values special-args)
      "decode"    (compile-deg-decode compile ?values special-args)
      "max-value" (compile-deg-max-value compile ?values special-args)
      "min-value" (compile-deg-min-value compile ?values special-args)
      "to-real"   (compile-deg-to-real compile ?values special-args)
      "scale"     (compile-deg-scale compile ?values special-args)
      )

    "int"
    (case proc-name
      "+"         (compile-int-add compile ?values special-args)
      "-"         (compile-int-sub compile ?values special-args)
      "*"         (compile-int-mul compile ?values special-args)
      "/"         (compile-int-div compile ?values special-args)
      "%"         (compile-int-rem compile ?values special-args)
      "="         (compile-int-eq compile ?values special-args)
      "<"         (compile-int-lt compile ?values special-args)
      "to-nat"    (compile-int-to-nat compile ?values special-args)
      "encode"    (compile-int-encode compile ?values special-args)
      )

    "real"
    (case proc-name
      "+"         (compile-real-add compile ?values special-args)
      "-"         (compile-real-sub compile ?values special-args)
      "*"         (compile-real-mul compile ?values special-args)
      "/"         (compile-real-div compile ?values special-args)
      "%"         (compile-real-rem compile ?values special-args)
      "="         (compile-real-eq compile ?values special-args)
      "<"         (compile-real-lt compile ?values special-args)
      "encode"    (compile-real-encode compile ?values special-args)
      "to-deg"    (compile-real-to-deg compile ?values special-args)
      )

    "char"
    (case proc-name
      "to-nat"    (compile-char-to-nat compile ?values special-args)
      "to-text"   (compile-char-to-text compile ?values special-args)
      )
    
    ;; else
    (&/fail-with-loc (str "[Compiler Error] Unknown procedure: " [proc-category proc-name]))))
