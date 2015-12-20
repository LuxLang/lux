;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.host
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.type.host :as &host-type]
            [lux.host.generics :as &host-generics]
            [lux.analyser.base :as &a]
            [lux.compiler.base :as &&])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor
                              AnnotationVisitor)))

;; [Utils]
(def init-method "<init>")

(let [class+method+sig {"boolean" [(&host-generics/->bytecode-class-name "java.lang.Boolean")   "booleanValue" "()Z"]
                        "byte"    [(&host-generics/->bytecode-class-name "java.lang.Byte")      "byteValue"    "()B"]
                        "short"   [(&host-generics/->bytecode-class-name "java.lang.Short")     "shortValue"   "()S"]
                        "int"     [(&host-generics/->bytecode-class-name "java.lang.Integer")   "intValue"     "()I"]
                        "long"    [(&host-generics/->bytecode-class-name "java.lang.Long")      "longValue"    "()J"]
                        "float"   [(&host-generics/->bytecode-class-name "java.lang.Float")     "floatValue"   "()F"]
                        "double"  [(&host-generics/->bytecode-class-name "java.lang.Double")    "doubleValue"  "()D"]
                        "char"    [(&host-generics/->bytecode-class-name "java.lang.Character") "charValue"    "()C"]}]
  (defn ^:private prepare-arg! [^MethodVisitor *writer* class-name]
    (if-let [[class method sig] (get class+method+sig class-name)]
      (doto *writer*
        (.visitTypeInsn Opcodes/CHECKCAST class)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL class method sig))
      (.visitTypeInsn *writer* Opcodes/CHECKCAST (&host-generics/->bytecode-class-name class-name)))))

(let [boolean-class "java.lang.Boolean"
      byte-class "java.lang.Byte"
      short-class "java.lang.Short"
      int-class "java.lang.Integer"
      long-class "java.lang.Long"
      float-class "java.lang.Float"
      double-class "java.lang.Double"
      char-class "java.lang.Character"]
  (defn prepare-return! [^MethodVisitor *writer* *type*]
    (|case *type*
      (&/$TupleT (&/$Nil))
      (.visitInsn *writer* Opcodes/ACONST_NULL)

      (&/$DataT "boolean" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name boolean-class) "valueOf" (str "(Z)" (&host-generics/->type-signature boolean-class)))
      
      (&/$DataT "byte" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name byte-class) "valueOf" (str "(B)" (&host-generics/->type-signature byte-class)))

      (&/$DataT "short" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name short-class) "valueOf" (str "(S)" (&host-generics/->type-signature short-class)))

      (&/$DataT "int" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name int-class) "valueOf" (str "(I)" (&host-generics/->type-signature int-class)))

      (&/$DataT "long" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name long-class) "valueOf" (str "(J)" (&host-generics/->type-signature long-class)))

      (&/$DataT "float" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name float-class) "valueOf" (str "(F)" (&host-generics/->type-signature float-class)))

      (&/$DataT "double" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name double-class) "valueOf" (str "(D)" (&host-generics/->type-signature double-class)))

      (&/$DataT "char" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name char-class) "valueOf" (str "(C)" (&host-generics/->type-signature char-class)))
      
      (&/$DataT _ _)
      nil

      (&/$NamedT ?name ?type)
      (prepare-return! *writer* ?type)

      (&/$ExT _)
      nil

      _
      (assert false (str 'prepare-return! " " (&type/show-type *type*))))
    *writer*))

;; [Resources]
(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig> <wrap>]
  (defn <name> [compile ?x ?y]
    (|do [:let [+wrapper-class+ (&host-generics/->bytecode-class-name <wrapper-class>)]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))
                _ (doto *writer*
                    (.visitInsn <opcode>)
                    (<wrap>))]]
      (return nil)))

  compile-jvm-iadd Opcodes/IADD "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  compile-jvm-isub Opcodes/ISUB "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  compile-jvm-imul Opcodes/IMUL "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  compile-jvm-idiv Opcodes/IDIV "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  compile-jvm-irem Opcodes/IREM "java.lang.Integer" "intValue"    "()I" &&/wrap-int
  
  compile-jvm-ladd Opcodes/LADD "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  compile-jvm-lsub Opcodes/LSUB "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  compile-jvm-lmul Opcodes/LMUL "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  compile-jvm-ldiv Opcodes/LDIV "java.lang.Long"    "longValue"   "()J" &&/wrap-long
  compile-jvm-lrem Opcodes/LREM "java.lang.Long"    "longValue"   "()J" &&/wrap-long

  compile-jvm-fadd Opcodes/FADD "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  compile-jvm-fsub Opcodes/FSUB "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  compile-jvm-fmul Opcodes/FMUL "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  compile-jvm-fdiv Opcodes/FDIV "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  compile-jvm-frem Opcodes/FREM "java.lang.Float"   "floatValue"  "()F" &&/wrap-float
  
  compile-jvm-dadd Opcodes/DADD "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  compile-jvm-dsub Opcodes/DSUB "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  compile-jvm-dmul Opcodes/DMUL "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  compile-jvm-ddiv Opcodes/DDIV "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  compile-jvm-drem Opcodes/DREM "java.lang.Double"  "doubleValue" "()D" &&/wrap-double
  )

(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig>]
  (defn <name> [compile ?x ?y]
    (|do [:let [+wrapper-class+ (&host-generics/->bytecode-class-name <wrapper-class>)]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))]
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))
                $then (new Label)
                $end (new Label)
                _ (doto *writer*
                    (.visitJumpInsn <opcode> $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE"  (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitJumpInsn Opcodes/GOTO $end)
                    (.visitLabel $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE" (&host-generics/->type-signature "java.lang.Boolean"))
                    (.visitLabel $end))]]
      (return nil)))

  compile-jvm-ieq Opcodes/IF_ICMPEQ "java.lang.Integer" "intValue" "()I"
  compile-jvm-ilt Opcodes/IF_ICMPLT "java.lang.Integer" "intValue" "()I"
  compile-jvm-igt Opcodes/IF_ICMPGT "java.lang.Integer" "intValue" "()I"

  compile-jvm-ceq Opcodes/IF_ICMPEQ "java.lang.Character" "charValue" "()C"
  compile-jvm-clt Opcodes/IF_ICMPLT "java.lang.Character" "charValue" "()C"
  compile-jvm-cgt Opcodes/IF_ICMPGT "java.lang.Character" "charValue" "()C"
  )

(do-template [<name> <cmpcode> <cmp-output> <wrapper-class> <value-method> <value-method-sig>]
  (defn <name> [compile ?x ?y]
    (|do [:let [+wrapper-class+ (&host-generics/->bytecode-class-name <wrapper-class>)]
          ^MethodVisitor *writer* &/get-writer
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))]
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))
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

  compile-jvm-leq Opcodes/LCMP  0 "java.lang.Long"   "longValue"   "()J"
  compile-jvm-llt Opcodes/LCMP  1 "java.lang.Long"   "longValue"   "()J"
  compile-jvm-lgt Opcodes/LCMP -1 "java.lang.Long"   "longValue"   "()J"

  compile-jvm-feq Opcodes/FCMPG  0 "java.lang.Float"  "floatValue"  "()F"
  compile-jvm-flt Opcodes/FCMPG  1 "java.lang.Float"  "floatValue"  "()F"
  compile-jvm-fgt Opcodes/FCMPG -1 "java.lang.Float"  "floatValue"  "()F"
  
  compile-jvm-deq Opcodes/DCMPG  0 "java.lang.Double" "doubleValue" "()D"
  compile-jvm-dlt Opcodes/DCMPG  1 "java.lang.Double" "doubleValue" "()D"
  compile-jvm-dgt Opcodes/FCMPG -1 "java.lang.Double" "doubleValue" "()D"
  )

(defn compile-jvm-invokestatic [compile ?class ?method ?classes ?args ?output-type]
  (|do [^MethodVisitor *writer* &/get-writer
        =output-type (&host/->java-sig ?output-type)
        :let [method-sig (str "(" (&/fold str "" (&/|map &host-generics/->type-signature ?classes)) ")" =output-type)]
        _ (&/map2% (fn [class-name arg]
                     (|do [ret (compile arg)
                           :let [_ (prepare-arg! *writer* class-name)]]
                       (return ret)))
                   ?classes ?args)
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESTATIC (&host-generics/->bytecode-class-name (&host-type/as-obj ?class)) ?method method-sig)
                  (prepare-return! ?output-type))]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile ?class ?method ?classes ?object ?args ?output-type]
    (|do [:let [?class* (&host-generics/->bytecode-class-name (&host-type/as-obj ?class))]
          ^MethodVisitor *writer* &/get-writer
          =output-type (&host/->java-sig ?output-type)
          :let [method-sig (str "(" (&/fold str "" (&/|map &host-generics/->type-signature ?classes)) ")" =output-type)]
          _ (compile ?object)
          :let [_ (when (not= "<init>" ?method)
                    (.visitTypeInsn *writer* Opcodes/CHECKCAST ?class*))]
          _ (&/map2% (fn [class-name arg]
                       (|do [ret (compile arg)
                             :let [_ (prepare-arg! *writer* class-name)]]
                         (return ret)))
                     ?classes ?args)
          :let [_ (doto *writer*
                    (.visitMethodInsn <op> ?class* ?method method-sig)
                    (prepare-return! ?output-type))]]
      (return nil)))

  compile-jvm-invokevirtual   Opcodes/INVOKEVIRTUAL
  compile-jvm-invokeinterface Opcodes/INVOKEINTERFACE
  compile-jvm-invokespecial   Opcodes/INVOKESPECIAL
  )

(defn compile-jvm-null [compile]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitInsn *writer* Opcodes/ACONST_NULL)]]
    (return nil)))

(defn compile-jvm-null? [compile ?object]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        :let [$then (new Label)
              $end (new Label)
              _ (doto *writer*
                  (.visitJumpInsn Opcodes/IFNULL $then)
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE" (&host-generics/->type-signature "java.lang.Boolean"))
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $then)
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE"  (&host-generics/->type-signature "java.lang.Boolean"))
                  (.visitLabel $end))]]
    (return nil)))

(defn compile-jvm-new [compile ?class ?classes ?args]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [init-sig (str "(" (&/fold str "" (&/|map &host-generics/->type-signature ?classes)) ")V")
              class* (&host-generics/->bytecode-class-name ?class)
              _ (doto *writer*
                  (.visitTypeInsn Opcodes/NEW class*)
                  (.visitInsn Opcodes/DUP))]
        _ (&/map% (fn [[class-name arg]]
                    (|do [ret (compile arg)
                          :let [_ (prepare-arg! *writer* class-name)]]
                      (return ret)))
                  (&/zip2 ?classes ?args))
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESPECIAL class* "<init>" init-sig))]]
    (return nil)))

(do-template [<prim-type> <array-type> <new-name> <load-name> <load-op> <store-name> <store-op> <wrapper> <unwrapper>]
  (do (defn <new-name> [compile ?length]
        (|do [^MethodVisitor *writer* &/get-writer
              _ (compile ?length)
              :let [_ (.visitInsn *writer* Opcodes/L2I)]
              :let [_ (.visitIntInsn *writer* Opcodes/NEWARRAY <prim-type>)]]
          (return nil)))

    (defn <load-name> [compile ?array ?idx]
      (|do [^MethodVisitor *writer* &/get-writer
            _ (compile ?array)
            :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST <array-type>)]
            _ (compile ?idx)
            :let [_ (doto *writer*
                      &&/unwrap-long
                      (.visitInsn Opcodes/L2I))]
            :let [_ (doto *writer*
                      (.visitInsn <load-op>)
                      <wrapper>)]]
        (return nil)))

    (defn <store-name> [compile ?array ?idx ?elem]
      (|do [^MethodVisitor *writer* &/get-writer
            _ (compile ?array)
            :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST <array-type>)]
            :let [_ (.visitInsn *writer* Opcodes/DUP)]
            _ (compile ?idx)
            :let [_ (doto *writer*
                      &&/unwrap-long
                      (.visitInsn Opcodes/L2I))]
            _ (compile ?elem)
            :let [_ (doto *writer*
                      <unwrapper>
                      (.visitInsn <store-op>))]]
        (return nil)))
    )

  Opcodes/T_BOOLEAN "[Z" compile-jvm-znewarray compile-jvm-zaload Opcodes/BALOAD compile-jvm-zastore Opcodes/BASTORE &&/wrap-boolean &&/unwrap-boolean
  Opcodes/T_BYTE    "[B" compile-jvm-bnewarray compile-jvm-baload Opcodes/BALOAD compile-jvm-bastore Opcodes/BASTORE &&/wrap-byte    &&/unwrap-byte
  Opcodes/T_SHORT   "[S" compile-jvm-snewarray compile-jvm-saload Opcodes/SALOAD compile-jvm-sastore Opcodes/SASTORE &&/wrap-short   &&/unwrap-short
  Opcodes/T_INT     "[I" compile-jvm-inewarray compile-jvm-iaload Opcodes/IALOAD compile-jvm-iastore Opcodes/IASTORE &&/wrap-int     &&/unwrap-int
  Opcodes/T_LONG    "[J" compile-jvm-lnewarray compile-jvm-laload Opcodes/LALOAD compile-jvm-lastore Opcodes/LASTORE &&/wrap-long    &&/unwrap-long
  Opcodes/T_FLOAT   "[F" compile-jvm-fnewarray compile-jvm-faload Opcodes/FALOAD compile-jvm-fastore Opcodes/FASTORE &&/wrap-float   &&/unwrap-float
  Opcodes/T_DOUBLE  "[D" compile-jvm-dnewarray compile-jvm-daload Opcodes/DALOAD compile-jvm-dastore Opcodes/DASTORE &&/wrap-double  &&/unwrap-double
  Opcodes/T_CHAR    "[C" compile-jvm-cnewarray compile-jvm-caload Opcodes/CALOAD compile-jvm-castore Opcodes/CASTORE &&/wrap-char    &&/unwrap-char
  )

(defn compile-jvm-anewarray [compile ?class ?length]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?length)
        :let [_ (.visitInsn *writer* Opcodes/L2I)]
        :let [_ (.visitTypeInsn *writer* Opcodes/ANEWARRAY (&host-generics/->bytecode-class-name ?class))]]
    (return nil)))

(defn compile-jvm-aaload [compile ?array ?idx]
  (|do [^MethodVisitor *writer* &/get-writer
        array-type (&host/->java-sig (&a/expr-type* ?array))
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST array-type)]
        _ (compile ?idx)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        :let [_ (.visitInsn *writer* Opcodes/AALOAD)]]
    (return nil)))

(defn compile-jvm-aastore [compile ?array ?idx ?elem]
  (|do [^MethodVisitor *writer* &/get-writer
        array-type (&host/->java-sig (&a/expr-type* ?array))
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST array-type)]
        :let [_ (.visitInsn *writer* Opcodes/DUP)]
        _ (compile ?idx)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        _ (compile ?elem)
        :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
    (return nil)))

(defn compile-jvm-arraylength [compile ?array]
  (|do [^MethodVisitor *writer* &/get-writer
        array-type (&host/->java-sig (&a/expr-type* ?array))
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST array-type)]
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/ARRAYLENGTH)
                  (.visitInsn Opcodes/I2L)
                  &&/wrap-long)]]
    (return nil)))

(defn compile-jvm-getstatic [compile ?class ?field ?output-type]
  (|do [^MethodVisitor *writer* &/get-writer
        =output-type (&host/->java-sig ?output-type)
        :let [_ (doto *writer*
                  (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name (&host-type/as-obj ?class)) ?field =output-type)
                  (prepare-return! ?output-type))]]
    (return nil)))

(defn compile-jvm-getfield [compile ?class ?field ?object ?output-type]
  (|do [:let [class* (&host-generics/->bytecode-class-name (&host-type/as-obj ?class))]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        =output-type (&host/->java-sig ?output-type)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST class*)
                  (.visitFieldInsn Opcodes/GETFIELD class* ?field =output-type)
                  (prepare-return! ?output-type))]]
    (return nil)))

(defn compile-jvm-putstatic [compile ?class ?field ?value ?output-type]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?value)
        =output-type (&host/->java-sig ?output-type)
        :let [_ (.visitFieldInsn *writer* Opcodes/PUTSTATIC (&host-generics/->bytecode-class-name (&host-type/as-obj ?class)) ?field =output-type)]
        :let [_ (.visitInsn *writer* Opcodes/ACONST_NULL)]]
    (return nil)))

(defn compile-jvm-putfield [compile ?class ?field ?object ?value ?output-type]
  (|do [:let [class* (&host-generics/->bytecode-class-name (&host-type/as-obj ?class))]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        :let [_ (.visitInsn *writer* Opcodes/DUP)]
        _ (compile ?value)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST class*)]
        =output-type (&host/->java-sig ?output-type)
        :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD class* ?field =output-type)]]
    (return nil)))

(defn compile-jvm-instanceof [compile class object]
  (|do [:let [class* (&host-generics/->bytecode-class-name class)]
        ^MethodVisitor *writer* &/get-writer
        _ (compile object)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/INSTANCEOF class*)
                  (&&/wrap-boolean))]]
    (return nil)))

(defn ^:private compile-annotation [writer ann]
  (doto ^AnnotationVisitor (.visitAnnotation writer (&host-generics/->bytecode-class-name (:name ann)) true)
        (-> (.visit param-name param-value)
            (->> (|let [[param-name param-value] param])
                 (doseq [param (&/->seq (:params ann))])))
        (.visitEnd))
  nil)

(defn ^:private compile-field [^ClassWriter writer field]
  (|let [[=name =anns =type] field
         =field (.visitField writer Opcodes/ACC_PUBLIC =name
                             (&host-generics/->type-signature =type) nil nil)]
    (do (&/|map (partial compile-annotation =field) =anns)
      (.visitEnd =field)
      nil)))

(defn ^:private compile-method-return [^MethodVisitor writer output]
  (|case output
    (&/$GenericClass "void" (&/$Nil))
    (.visitInsn writer Opcodes/RETURN)
    
    (&/$GenericClass "boolean" (&/$Nil))
    (doto writer
      &&/unwrap-boolean
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "byte" (&/$Nil))
    (doto writer
      &&/unwrap-byte
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "short" (&/$Nil))
    (doto writer
      &&/unwrap-short
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "int" (&/$Nil))
    (doto writer
      &&/unwrap-int
      (.visitInsn Opcodes/IRETURN))
    
    (&/$GenericClass "long" (&/$Nil))
    (doto writer
      &&/unwrap-long
      (.visitInsn Opcodes/LRETURN))
    
    (&/$GenericClass "float" (&/$Nil))
    (doto writer
      &&/unwrap-float
      (.visitInsn Opcodes/FRETURN))
    
    (&/$GenericClass "double" (&/$Nil))
    (doto writer
      &&/unwrap-double
      (.visitInsn Opcodes/DRETURN))
    
    (&/$GenericClass "char" (&/$Nil))
    (doto writer
      &&/unwrap-char
      (.visitInsn Opcodes/IRETURN))
    
    _
    (.visitInsn writer Opcodes/ARETURN)))

(defn ^:private compile-method-def [compile ^ClassWriter class-writer ?super-class method-def]
  (|case method-def
    (&/$ConstructorMethodAnalysis ?anns ?gvars ?exceptions ?inputs ?ctor-args ?body)
    (|let [?output (&/V &/$GenericClass (&/T "void" (&/|list)))
           =method-decl (&/T init-method ?anns ?gvars ?exceptions (&/|map &/|second ?inputs) ?output)
           [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)]
      (&/with-writer (.visitMethod class-writer
                                   Opcodes/ACC_PUBLIC
                                   init-method
                                   simple-signature
                                   generic-signature
                                   (->> ?exceptions (&/|map &host-generics/->bytecode-class-name) &/->seq (into-array java.lang.String)))
        (|do [^MethodVisitor =method &/get-writer
              :let [[super-class-name super-class-params] ?super-class
                    init-types (->> ?ctor-args (&/|map (comp &host-generics/->type-signature &/|first)) (&/fold str ""))
                    init-sig (str "(" init-types ")" "V")
                    _ (&/|map (partial compile-annotation =method) ?anns)
                    _ (doto =method
                        (.visitCode)
                        (.visitMethodInsn Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name super-class-name) init-method init-sig))]
              _ (compile ?body)
              :let [_ (doto =method
                        (compile-method-return ?output)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return nil))))
    
    (&/$VirtualMethodAnalysis ?name ?anns ?gvars ?exceptions ?inputs ?output ?body)
    (|let [=method-decl (&/T ?name ?anns ?gvars ?exceptions (&/|map &/|second ?inputs) ?output)
           [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)]
      (&/with-writer (.visitMethod class-writer
                                   Opcodes/ACC_PUBLIC
                                   ?name
                                   simple-signature
                                   generic-signature
                                   (->> ?exceptions (&/|map &host-generics/->bytecode-class-name) &/->seq (into-array java.lang.String)))
        (|do [^MethodVisitor =method &/get-writer
              :let [_ (&/|map (partial compile-annotation =method) ?anns)
                    _ (.visitCode =method)]
              _ (compile ?body)
              :let [_ (doto =method
                        (compile-method-return ?output)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return nil))))
    
    (&/$OverridenMethodAnalysis ?class-decl ?name ?anns ?gvars ?exceptions ?inputs ?output ?body)
    (|let [=method-decl (&/T ?name ?anns ?gvars ?exceptions (&/|map &/|second ?inputs) ?output)
           [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)]
      (&/with-writer (.visitMethod class-writer
                                   Opcodes/ACC_PUBLIC
                                   ?name
                                   simple-signature
                                   generic-signature
                                   (->> ?exceptions (&/|map &host-generics/->bytecode-class-name) &/->seq (into-array java.lang.String)))
        (|do [^MethodVisitor =method &/get-writer
              :let [_ (&/|map (partial compile-annotation =method) ?anns)
                    _ (.visitCode =method)]
              _ (compile ?body)
              :let [_ (doto =method
                        (compile-method-return ?output)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return nil))))
    ))

(defn ^:private compile-method-decl [^ClassWriter class-writer =method-decl]
  (|let [[=name =anns =gvars =exceptions =inputs =output] =method-decl
         [simple-signature generic-signature] (&host-generics/method-signatures =method-decl)
         =method (.visitMethod class-writer
                               (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT)
                               =name
                               simple-signature
                               generic-signature
                               (->> =exceptions (&/|map &host-generics/gclass->class-name) &/->seq (into-array java.lang.String)))
         _ (&/|map (partial compile-annotation =method) =anns)
         _ (.visitEnd =method)]
    nil))

(defn ^:private prepare-ctor-arg [^MethodVisitor writer type]
  (case type
    "boolean" (doto writer
                (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Boolean"))
                &&/unwrap-boolean)
    "byte" (doto writer
             (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Byte"))
             &&/unwrap-byte)
    "short" (doto writer
              (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Short"))
              &&/unwrap-short)
    "int" (doto writer
            (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Integer"))
            &&/unwrap-int)
    "long" (doto writer
             (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Long"))
             &&/unwrap-long)
    "float" (doto writer
              (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Float"))
              &&/unwrap-float)
    "double" (doto writer
               (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Double"))
               &&/unwrap-double)
    "char" (doto writer
             (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name "java.lang.Character"))
             &&/unwrap-char)
    ;; else
    (doto writer
      (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name type)))))

(let [clo-field-sig (&host-generics/->type-signature "java.lang.Object")
      <init>-return "V"]
  (defn ^:private anon-class-<init>-signature [env]
    (str "(" (&/fold str "" (&/|repeat (&/|length env) clo-field-sig)) ")"
         <init>-return))

  (defn ^:private add-anon-class-<init> [^ClassWriter class-writer compile class-name super-class env ctor-args]
    (let [init-types (->> ctor-args (&/|map (comp &host-generics/->type-signature &/|first)) (&/fold str ""))]
      (&/with-writer (.visitMethod class-writer Opcodes/ACC_PUBLIC init-method (anon-class-<init>-signature env) nil nil)
        (|do [^MethodVisitor =method &/get-writer
              :let [_ (doto =method (.visitCode)
                            (.visitVarInsn Opcodes/ALOAD 0))]
              _ (&/map% (fn [type+term]
                          (|let [[type term] type+term]
                            (|do [_ (compile term)
                                  :let [_ (prepare-ctor-arg =method type)]]
                              (return nil))))
                        ctor-args)
              :let [_ (doto =method
                        (.visitMethodInsn Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name super-class) init-method (str "(" init-types ")" <init>-return))
                        (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                              (.visitVarInsn Opcodes/ALOAD (inc ?captured-id))
                              (.visitFieldInsn Opcodes/PUTFIELD class-name captured-name clo-field-sig))
                            (->> (let [captured-name (str &&/closure-prefix ?captured-id)])
                                 (|case ?name+?captured
                                   [?name [_ (&a/$captured _ ?captured-id ?source)]])
                                 (doseq [?name+?captured (&/->seq env)])))
                        (.visitInsn Opcodes/RETURN)
                        (.visitMaxs 0 0)
                        (.visitEnd))]]
          (return nil)))))
  )

(defn compile-jvm-class [compile class-decl ?super-class ?interfaces ?anns ?fields ?methods env ??ctor-args]
  (|do [module &/get-module-name
        [file-name _ _] &/cursor
        :let [[?name ?params] class-decl
              class-signature (&host-generics/gclass-decl->signature class-decl (&/Cons$ ?super-class ?interfaces))
              full-name (str module "/" ?name)
              super-class* (&host-generics/->bytecode-class-name (&host-generics/super-class-name ?super-class))
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                               full-name nil super-class* (->> ?interfaces (&/|map (comp &host-generics/->bytecode-class-name &host-generics/super-class-name)) &/->seq (into-array String)))
                       (.visitSource file-name nil))
              _ (&/|map (partial compile-annotation =class) ?anns)
              _ (&/|map (partial compile-field =class)
                        ?fields)]
        _ (&/map% (partial compile-method-def compile =class ?super-class) ?methods)
        _ (|case ??ctor-args
            (&/$Some ctor-args)
            (add-anon-class-<init> =class compile full-name ?super-class env ctor-args)

            _
            (return nil))]
    (&&/save-class! ?name (.toByteArray (doto =class .visitEnd)))))

(defn compile-jvm-interface [compile interface-decl ?supers ?anns ?methods]
  (|do [:let [[interface-name interface-vars] interface-decl]
        module &/get-module-name
        [file-name _ _] &/cursor
        :let [=interface (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                           (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE)
                                   (str module "/" interface-name)
                                   (&host-generics/gclass-decl->signature interface-decl ?supers)
                                   "java/lang/Object"
                                   (->> ?supers (&/|map (comp &host-generics/->bytecode-class-name &host-generics/super-class-name)) &/->seq (into-array String)))
                           (.visitSource file-name nil))
              _ (&/|map (partial compile-annotation =interface) ?anns)
              _ (do (&/|map (partial compile-method-decl =interface) ?methods)
                  (.visitEnd =interface))]]
    (&&/save-class! interface-name (.toByteArray =interface))))

(def compile-Function-class
  (let [object-class (&/V &/$GenericClass (&/T "java.lang.Object" (&/|list)))
        interface-decl (&/T "Function" (&/|list))
        ?supers (&/|list)
        ?anns (&/|list)
        ?methods (&/|list (&/T "apply"
                               (&/|list)
                               (&/|list)
                               (&/|list)
                               (&/|list object-class)
                               object-class))]
    (compile-jvm-interface nil interface-decl ?supers ?anns ?methods)))

(defn compile-jvm-try [compile ?body ?catches ?finally]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [$from (new Label)
              $to (new Label)
              $end (new Label)
              $catch-finally (new Label)
              compile-finally (|case ?finally
                                (&/$Some ?finally*) (|do [_ (return nil)
                                                          _ (compile ?finally*)
                                                          :let [_ (doto *writer*
                                                                    (.visitInsn Opcodes/POP)
                                                                    (.visitJumpInsn Opcodes/GOTO $end))]]
                                                      (return nil))
                                (&/$None) (|do [_ (return nil)
                                                :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)]]
                                            (return nil)))
              catch-boundaries (&/|map (fn [[?ex-class ?ex-idx ?catch-body]] [?ex-class (new Label) (new Label)])
                                       ?catches)
              _ (doseq [[?ex-class $handler-start $handler-end] (&/->seq catch-boundaries)]
                  (doto *writer*
                    (.visitTryCatchBlock $from $to $handler-start (&host-generics/->bytecode-class-name ?ex-class))
                    (.visitTryCatchBlock $handler-start $handler-end $catch-finally nil)))
              _ (.visitTryCatchBlock *writer* $from $to $catch-finally nil)]
        :let [_ (.visitLabel *writer* $from)]
        _ (compile ?body)
        :let [_ (.visitLabel *writer* $to)]
        _ compile-finally
        handlers (&/map2% (fn [[?ex-class ?ex-idx ?catch-body] [_ $handler-start $handler-end]]
                            (|do [:let [_ (doto *writer*
                                            (.visitLabel $handler-start)
                                            (.visitVarInsn Opcodes/ASTORE ?ex-idx))]
                                  _ (compile ?catch-body)
                                  :let [_ (.visitLabel *writer* $handler-end)]]
                              compile-finally))
                          ?catches
                          catch-boundaries)
        :let [_ (.visitLabel *writer* $catch-finally)]
        _ (|case ?finally
            (&/$Some ?finally*) (|do [_ (compile ?finally*)
                                      :let [_ (.visitInsn *writer* Opcodes/POP)]
                                      :let [_ (.visitInsn *writer* Opcodes/ATHROW)]]
                                  (return nil))
            (&/$None) (|do [_ (return nil)
                            :let [_ (.visitInsn *writer* Opcodes/ATHROW)]]
                        (return nil)))
        :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)]
        :let [_ (.visitLabel *writer* $end)]]
    (return nil)))

(defn compile-jvm-throw [compile ?ex]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?ex)
        :let [_ (.visitInsn *writer* Opcodes/ATHROW)]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile ?monitor]
    (|do [^MethodVisitor *writer* &/get-writer
          _ (compile ?monitor)
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    (.visitInsn Opcodes/ACONST_NULL))]]
      (return nil)))

  compile-jvm-monitorenter Opcodes/MONITORENTER
  compile-jvm-monitorexit  Opcodes/MONITOREXIT
  )

(do-template [<name> <op> <from-class> <from-method> <from-sig> <to-class> <to-sig>]
  (defn <name> [compile ?value]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/NEW (&host-generics/->bytecode-class-name <to-class>))
                    (.visitInsn Opcodes/DUP))]
          _ (compile ?value)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name <from-class>))
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host-generics/->bytecode-class-name <from-class>) <from-method> <from-sig>)
                    (.visitInsn <op>)
                    (.visitMethodInsn Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name <to-class>) init-method <to-sig>))]]
      (return nil)))

  compile-jvm-d2f Opcodes/D2F "java.lang.Double"  "doubleValue" "()D" "java.lang.Float"     "(F)V"
  compile-jvm-d2i Opcodes/D2I "java.lang.Double"  "doubleValue" "()D" "java.lang.Integer"   "(I)V"
  compile-jvm-d2l Opcodes/D2L "java.lang.Double"  "doubleValue" "()D" "java.lang.Long"      "(J)V"

  compile-jvm-f2d Opcodes/F2D "java.lang.Float"   "floatValue"  "()F" "java.lang.Double"    "(D)V"
  compile-jvm-f2i Opcodes/F2I "java.lang.Float"   "floatValue"  "()F" "java.lang.Integer"   "(I)V"
  compile-jvm-f2l Opcodes/F2L "java.lang.Float"   "floatValue"  "()F" "java.lang.Long"      "(J)V"

  compile-jvm-i2b Opcodes/I2B "java.lang.Integer" "intValue"    "()I" "java.lang.Byte"      "(B)V"
  compile-jvm-i2c Opcodes/I2C "java.lang.Integer" "intValue"    "()I" "java.lang.Character" "(C)V"
  compile-jvm-i2d Opcodes/I2D "java.lang.Integer" "intValue"    "()I" "java.lang.Double"    "(D)V"
  compile-jvm-i2f Opcodes/I2F "java.lang.Integer" "intValue"    "()I" "java.lang.Float"     "(F)V"
  compile-jvm-i2l Opcodes/I2L "java.lang.Integer" "intValue"    "()I" "java.lang.Long"      "(J)V"
  compile-jvm-i2s Opcodes/I2S "java.lang.Integer" "intValue"    "()I" "java.lang.Short"     "(S)V"

  compile-jvm-l2d Opcodes/L2D "java.lang.Long"    "longValue"   "()J" "java.lang.Double"    "(D)V"
  compile-jvm-l2f Opcodes/L2F "java.lang.Long"    "longValue"   "()J" "java.lang.Float"     "(F)V"
  compile-jvm-l2i Opcodes/L2I "java.lang.Long"    "longValue"   "()J" "java.lang.Integer"   "(I)V"
  )

(do-template [<name> <op> <from1-method> <from1-sig> <from1-class> <from2-method> <from2-sig> <from2-class> <to-class> <to-sig>]
  (defn <name> [compile ?x ?y]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/NEW (&host-generics/->bytecode-class-name <to-class>))
                    (.visitInsn Opcodes/DUP))]
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name <from1-class>))
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host-generics/->bytecode-class-name <from1-class>) <from1-method> <from1-sig>))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST (&host-generics/->bytecode-class-name <from2-class>))
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host-generics/->bytecode-class-name <from2-class>) <from2-method> <from2-sig>))]
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    (.visitMethodInsn Opcodes/INVOKESPECIAL (&host-generics/->bytecode-class-name <to-class>) init-method <to-sig>))]]
      (return nil)))

  compile-jvm-iand  Opcodes/IAND  "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  compile-jvm-ior   Opcodes/IOR   "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  compile-jvm-ixor  Opcodes/IXOR  "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  compile-jvm-ishl  Opcodes/ISHL  "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  compile-jvm-ishr  Opcodes/ISHR  "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  compile-jvm-iushr Opcodes/IUSHR "intValue"  "()I" "java.lang.Integer" "intValue"  "()I" "java.lang.Integer" "java.lang.Integer" "(I)V"
  
  compile-jvm-land  Opcodes/LAND  "longValue" "()J" "java.lang.Long"    "longValue" "()J" "java.lang.Long"    "java.lang.Long"    "(J)V"
  compile-jvm-lor   Opcodes/LOR   "longValue" "()J" "java.lang.Long"    "longValue" "()J" "java.lang.Long"    "java.lang.Long"    "(J)V"
  compile-jvm-lxor  Opcodes/LXOR  "longValue" "()J" "java.lang.Long"    "longValue" "()J" "java.lang.Long"    "java.lang.Long"    "(J)V"
  compile-jvm-lshl  Opcodes/LSHL  "longValue" "()J" "java.lang.Long"    "intValue"  "()I" "java.lang.Integer" "java.lang.Long"    "(J)V"
  compile-jvm-lshr  Opcodes/LSHR  "longValue" "()J" "java.lang.Long"    "intValue"  "()I" "java.lang.Integer" "java.lang.Long"    "(J)V"
  compile-jvm-lushr Opcodes/LUSHR "longValue" "()J" "java.lang.Long"    "intValue"  "()I" "java.lang.Integer" "java.lang.Long"    "(J)V"
  )

(defn compile-jvm-program [compile ?body]
  (|do [module-name &/get-module-name
        ^ClassWriter *writer* &/get-writer]
    (&/with-writer (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "main" "([Ljava/lang/String;)V" nil nil)
                     (.visitCode))
      (|do [^MethodVisitor main-writer &/get-writer
            :let [$loop (new Label)
                  $end (new Label)
                  _ (doto main-writer
                      ;; Tail: Begin
                      (.visitLdcInsn (int 2)) ;; S
                      (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; V
                      (.visitInsn Opcodes/DUP) ;; VV
                      (.visitLdcInsn (int 0)) ;; VVI
                      (.visitLdcInsn &/$Nil) ;; VVIT
                      (&&/wrap-long)
                      (.visitInsn Opcodes/AASTORE) ;; V
                      (.visitInsn Opcodes/DUP) ;; VV
                      (.visitLdcInsn (int 1)) ;; VVI
                      (.visitInsn Opcodes/ACONST_NULL) ;; VVIN
                      (.visitInsn Opcodes/AASTORE) ;; V
                      ;; Tail: End
                      ;; Size: Begin
                      (.visitVarInsn Opcodes/ALOAD 0) ;; VA
                      (.visitInsn Opcodes/ARRAYLENGTH) ;; VI
                      ;; Size: End
                      ;; Loop: Begin
                      (.visitLabel $loop)
                      (.visitLdcInsn (int 1)) ;; VII
                      (.visitInsn Opcodes/ISUB) ;; VI
                      (.visitInsn Opcodes/DUP) ;; VII
                      (.visitJumpInsn Opcodes/IFLT $end) ;; VI
                      ;; Head: Begin
                      (.visitInsn Opcodes/DUP) ;; VII
                      (.visitVarInsn Opcodes/ALOAD 0) ;; VIIA
                      (.visitInsn Opcodes/SWAP) ;; VIAI
                      (.visitInsn Opcodes/AALOAD) ;; VIO
                      (.visitInsn Opcodes/SWAP) ;; VOI
                      (.visitInsn Opcodes/DUP_X2) ;; IVOI
                      (.visitInsn Opcodes/POP) ;; IVO
                      ;; Head: End
                      ;; Tuple: Begin
                      (.visitLdcInsn (int 2)) ;; IVOS
                      (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; IVO2
                      (.visitInsn Opcodes/DUP_X1) ;; IV2O2
                      (.visitInsn Opcodes/SWAP) ;; IV22O
                      (.visitLdcInsn (int 0)) ;; IV22OI
                      (.visitInsn Opcodes/SWAP) ;; IV22IO
                      (.visitInsn Opcodes/AASTORE) ;; IV2
                      (.visitInsn Opcodes/DUP_X1) ;; I2V2
                      (.visitInsn Opcodes/SWAP) ;; I22V
                      (.visitLdcInsn (int 1)) ;; I22VI
                      (.visitInsn Opcodes/SWAP) ;; I22IV
                      (.visitInsn Opcodes/AASTORE) ;; I2
                      ;; Tuple: End
                      ;; Cons: Begin
                      (.visitLdcInsn (int 2)) ;; I2I
                      (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; I2V
                      (.visitInsn Opcodes/DUP) ;; I2VV
                      (.visitLdcInsn (int 0)) ;; I2VVI
                      (.visitLdcInsn &/$Cons) ;; I2VVIT
                      (&&/wrap-long)
                      (.visitInsn Opcodes/AASTORE) ;; I2V
                      (.visitInsn Opcodes/DUP_X1) ;; IV2V
                      (.visitInsn Opcodes/SWAP) ;; IVV2
                      (.visitLdcInsn (int 1)) ;; IVV2I
                      (.visitInsn Opcodes/SWAP) ;; IVVI2
                      (.visitInsn Opcodes/AASTORE) ;; IV
                      ;; Cons: End
                      (.visitInsn Opcodes/SWAP) ;; VI
                      (.visitJumpInsn Opcodes/GOTO $loop)
                      ;; Loop: End
                      (.visitLabel $end) ;; VI
                      (.visitInsn Opcodes/POP) ;; V
                      (.visitVarInsn Opcodes/ASTORE (int 0)) ;;
                      )
                  ]
            _ (compile ?body)
            :let [_ (doto main-writer
                      (.visitInsn Opcodes/ACONST_NULL)
                      (.visitMethodInsn Opcodes/INVOKEINTERFACE &&/function-class "apply" &&/apply-signature))]
            :let [_ (doto main-writer
                      (.visitInsn Opcodes/POP)
                      (.visitInsn Opcodes/RETURN)
                      (.visitMaxs 0 0)
                      (.visitEnd))]]
        (return nil)))))
