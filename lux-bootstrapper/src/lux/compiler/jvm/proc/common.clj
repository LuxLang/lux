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

  ^:private compile-i64-and Opcodes/LAND
  ^:private compile-i64-or  Opcodes/LOR
  ^:private compile-i64-xor Opcodes/LXOR
  )

(do-template [<op> <name>]
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

  Opcodes/LSHL ^:private compile-i64-left-shift
  Opcodes/LUSHR ^:private compile-i64-right-shift
  )

(defn ^:private compile-lux-is [compile ?values special-args]
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

(defn ^:private compile-lux-try [compile ?values special-args]
  (|do [:let [(&/$Cons ?op (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?op)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "lux/Function")
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "runTry" "(Llux/Function;)[Ljava/lang/Object;"))]]
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

  ^:private compile-i64-add   Opcodes/LADD &&/unwrap-long &&/wrap-long
  ^:private compile-i64-sub   Opcodes/LSUB &&/unwrap-long &&/wrap-long
  
  ^:private compile-int-mul   Opcodes/LMUL &&/unwrap-long &&/wrap-long
  ^:private compile-int-div   Opcodes/LDIV &&/unwrap-long &&/wrap-long
  ^:private compile-int-rem   Opcodes/LREM &&/unwrap-long &&/wrap-long
  
  ^:private compile-frac-add  Opcodes/DADD &&/unwrap-double &&/wrap-double
  ^:private compile-frac-sub  Opcodes/DSUB &&/unwrap-double &&/wrap-double
  ^:private compile-frac-mul  Opcodes/DMUL &&/unwrap-double &&/wrap-double
  ^:private compile-frac-div  Opcodes/DDIV &&/unwrap-double &&/wrap-double
  ^:private compile-frac-rem  Opcodes/DREM &&/unwrap-double &&/wrap-double
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

  ^:private compile-i64-eq  Opcodes/LCMP   0 &&/unwrap-long
  
  ^:private compile-int-lt  Opcodes/LCMP  -1 &&/unwrap-long

  ^:private compile-frac-eq Opcodes/DCMPG  0 &&/unwrap-double
  ^:private compile-frac-lt Opcodes/DCMPG -1 &&/unwrap-double
  )

(defn ^:private compile-frac-encode [compile ?values special-args]
  (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?input)
        :let [_ (doto *writer*
                  &&/unwrap-double
                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Double" "toString" "(D)Ljava/lang/String;"))]]
    (return nil)))

(defn ^:private compile-frac-decode [compile ?values special-args]
  (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?input)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String")
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "decode_frac" "(Ljava/lang/String;)[Ljava/lang/Object;"))]]
    (return nil)))

(defn ^:private compile-int-char [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?x)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I)
                  (.visitInsn Opcodes/I2C)
                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/String" "valueOf" "(C)Ljava/lang/String;"))]]
    (return nil)))

(do-template [<name> <unwrap> <op> <wrap>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?input)
          :let [_ (doto *writer*
                    <unwrap>
                    (.visitInsn <op>)
                    <wrap>)]]
      (return nil)))

  ^:private compile-frac-int &&/unwrap-double Opcodes/D2L &&/wrap-long
  ^:private compile-int-frac &&/unwrap-long   Opcodes/L2D &&/wrap-double
  )

(defn ^:private compile-text-eq [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?x)
        _ (compile ?y)
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z")
                  (&&/wrap-boolean))]]
    (return nil)))

(defn ^:private compile-text-lt [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?x)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String"))]
        _ (compile ?y)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String"))]
        :let [$then (new Label)
              $end (new Label)
              _ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "compareTo" "(Ljava/lang/String;)I")
                  (.visitJumpInsn Opcodes/IFLT $then)
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE" (&host-generics/->type-signature "java.lang.Boolean"))
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $then)
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE" (&host-generics/->type-signature "java.lang.Boolean"))
                  (.visitLabel $end))]]
    (return nil)))

(defn compile-text-concat [compile ?values special-args]
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

(defn compile-text-clip [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?offset (&/$Cons ?length (&/$Nil)))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?text)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String"))]
        _ (compile ?offset)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I)
                  (.visitInsn Opcodes/DUP))]
        _ (compile ?length)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I)
                  (.visitInsn Opcodes/IADD))]
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "substring" "(II)Ljava/lang/String;"))]]
    (return nil)))

(defn ^:private compile-text-index [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?part (&/$Cons ?start (&/$Nil)))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?text)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String"))]
        _ (compile ?part)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String"))]
        _ (compile ?start)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "indexOf" "(Ljava/lang/String;I)I"))]
        :let [$not-found (new Label)
              $end (new Label)
              _ (doto *writer*
                  (.visitInsn Opcodes/DUP)
                  (.visitLdcInsn (int -1))
                  (.visitJumpInsn Opcodes/IF_ICMPEQ $not-found)
                  (.visitInsn Opcodes/I2L)
                  &&/wrap-long
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_some" "(Ljava/lang/Object;)[Ljava/lang/Object;")
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $not-found)
                  (.visitInsn Opcodes/POP)
                  (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "make_none" "()[Ljava/lang/Object;")
                  (.visitLabel $end))]]
    (return nil)))

(do-template [<name> <class> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?text (&/$Nil)) ?values]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?text)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String")
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL <class> <method> "()I")
                    (.visitInsn Opcodes/I2L)
                    &&/wrap-long)]]
      (return nil)))

  ^:private compile-text-size "java/lang/String" "length"
  )

(defn ^:private compile-text-char [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?idx (&/$Nil))) ?values]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?text)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String"))]
        _ (compile ?idx)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I)
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/lang/String" "charAt" "(I)C")
                  (.visitInsn Opcodes/I2L)
                  &&/wrap-long)]]
    (return nil)))

(defn ^:private compile-io-log [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitFieldInsn Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;"))]
        _ (compile ?x)
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "println" "(Ljava/lang/Object;)V")
                  (.visitLdcInsn &/unit-tag))]]
    (return nil)))

(defn ^:private compile-io-error [compile ?values special-args]
  (|do [:let [(&/$Cons ?message (&/$Nil)) ?values]
        ^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/NEW "java/lang/Error")
                  (.visitInsn Opcodes/DUP))]
        _ (compile ?message)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST "java/lang/String")
                  (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Error" "<init>" "(Ljava/lang/String;)V")
                  (.visitInsn Opcodes/ATHROW))]]
    (return nil)))

(defn ^:private compile-io-current-time [compile ?values special-args]
  (|do [:let [(&/$Nil) ?values]
        ^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/System" "currentTimeMillis" "()J")
                  &&/wrap-long)]]
    (return nil)))

(defn ^:private compile-syntax-char-case! [compile ?values ?patterns]
  (|do [:let [(&/$Cons ?input (&/$Cons ?else ?matches)) ?values]
        ^MethodVisitor *writer* &/get-writer
        :let [pattern-labels (&/|map (fn [_] (new Label)) ?patterns)
              matched-patterns (&/fold (fn [matches chars+label]
                                         (|let [[chars label] chars+label]
                                           (&/fold (fn [matches char]
                                                     (assoc matches char label))
                                                   matches
                                                   chars)))
                                       {}
                                       (&/zip2 ?patterns pattern-labels))
              end-label (new Label)
              else-label (new Label)
              match-keys (keys matched-patterns)
              min (apply min match-keys)
              max (apply max match-keys)
              capacity (inc (- max min))
              switch (map-indexed (fn [index label]
                                    (get matched-patterns (+ min index) else-label))
                                  (repeat capacity else-label))]
        _ (compile ?input)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I)
                  (.visitTableSwitchInsn (int min)
                                         (int max)
                                         else-label
                                         (into-array switch)))]
        _ (&/map% (fn [?label+?match]
                    (|let [[?label ?match] ?label+?match]
                      (|do [:let [_ (doto *writer*
                                      (.visitLabel ?label))]
                            _ (compile ?match)
                            :let [_ (doto *writer*
                                      (.visitJumpInsn Opcodes/GOTO end-label))]]
                        (return nil))))
                  (&/zip2 pattern-labels ?matches))
        :let [_ (doto *writer*
                  (.visitLabel else-label))]
        _ (compile ?else)
        :let [_ (doto *writer*
                  (.visitLabel end-label))]]
    (return nil)))

(defn compile-proc [compile category proc ?values special-args]
  (case category
    "lux"
    (case proc
      "is"                   (compile-lux-is compile ?values special-args)
      "try"                  (compile-lux-try compile ?values special-args)
      ;; TODO: Special extensions for performance reasons
      ;; Will be replaced by custom extensions in the future.
      "syntax char case!" (compile-syntax-char-case! compile ?values special-args))

    "io"
    (case proc
      "log"                  (compile-io-log compile ?values special-args)
      "error"                (compile-io-error compile ?values special-args)
      "current-time"         (compile-io-current-time compile ?values special-args)
      )

    "text"
    (case proc
      "="                    (compile-text-eq compile ?values special-args)
      "<"                    (compile-text-lt compile ?values special-args)
      "concat"               (compile-text-concat compile ?values special-args)
      "clip"                 (compile-text-clip compile ?values special-args)
      "index"                (compile-text-index compile ?values special-args)
      "size"                 (compile-text-size compile ?values special-args)
      "char"                 (compile-text-char compile ?values special-args)
      )
    
    "i64"
    (case proc
      "and"         (compile-i64-and compile ?values special-args)
      "or"          (compile-i64-or compile ?values special-args)
      "xor"         (compile-i64-xor compile ?values special-args)
      "left-shift"  (compile-i64-left-shift compile ?values special-args)
      "right-shift" (compile-i64-right-shift compile ?values special-args)
      "="           (compile-i64-eq compile ?values special-args)
      "+"           (compile-i64-add compile ?values special-args)
      "-"           (compile-i64-sub compile ?values special-args)
      "*"           (compile-int-mul compile ?values special-args)
      "/"           (compile-int-div compile ?values special-args)
      "%"           (compile-int-rem compile ?values special-args)
      "<"           (compile-int-lt compile ?values special-args)
      "f64"         (compile-int-frac compile ?values special-args)
      "char"        (compile-int-char compile ?values special-args)
      )
    
    "f64"
    (case proc
      "+"      (compile-frac-add compile ?values special-args)
      "-"      (compile-frac-sub compile ?values special-args)
      "*"      (compile-frac-mul compile ?values special-args)
      "/"      (compile-frac-div compile ?values special-args)
      "%"      (compile-frac-rem compile ?values special-args)
      "="      (compile-frac-eq compile ?values special-args)
      "<"      (compile-frac-lt compile ?values special-args)
      "i64"    (compile-frac-int compile ?values special-args)
      "encode" (compile-frac-encode compile ?values special-args)
      "decode" (compile-frac-decode compile ?values special-args)
      )

    ;; else
    (&/fail-with-loc (str "[Compiler Error] Unknown procedure: " [category proc]))))
