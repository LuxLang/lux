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
            [lux.analyser.base :as &a]
            [lux.compiler.base :as &&]
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor
                              AnnotationVisitor)))

;; [Utils]
(let [class+method+sig {"boolean" [(&host/->class "java.lang.Boolean")   "booleanValue" "()Z"]
                        "byte"    [(&host/->class "java.lang.Byte")      "byteValue"    "()B"]
                        "short"   [(&host/->class "java.lang.Short")     "shortValue"   "()S"]
                        "int"     [(&host/->class "java.lang.Integer")   "intValue"     "()I"]
                        "long"    [(&host/->class "java.lang.Long")      "longValue"    "()J"]
                        "float"   [(&host/->class "java.lang.Float")     "floatValue"   "()F"]
                        "double"  [(&host/->class "java.lang.Double")    "doubleValue"  "()D"]
                        "char"    [(&host/->class "java.lang.Character") "charValue"    "()C"]}]
  (defn ^:private prepare-arg! [^MethodVisitor *writer* class-name]
    (if-let [[class method sig] (get class+method+sig class-name)]
      (doto *writer*
        (.visitTypeInsn Opcodes/CHECKCAST class)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL class method sig))
      (.visitTypeInsn *writer* Opcodes/CHECKCAST (&host/->class class-name)))))

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
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class boolean-class) "valueOf" (str "(Z)" (&host/->type-signature boolean-class)))
      
      (&/$DataT "byte" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class byte-class) "valueOf" (str "(B)" (&host/->type-signature byte-class)))

      (&/$DataT "short" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class short-class) "valueOf" (str "(S)" (&host/->type-signature short-class)))

      (&/$DataT "int" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class int-class) "valueOf" (str "(I)" (&host/->type-signature int-class)))

      (&/$DataT "long" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class long-class) "valueOf" (str "(J)" (&host/->type-signature long-class)))

      (&/$DataT "float" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class float-class) "valueOf" (str "(F)" (&host/->type-signature float-class)))

      (&/$DataT "double" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class double-class) "valueOf" (str "(D)" (&host/->type-signature double-class)))

      (&/$DataT "char" (&/$Nil))
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class char-class) "valueOf" (str "(C)" (&host/->type-signature char-class)))
      
      (&/$DataT _ _)
      nil

      (&/$NamedT ?name ?type)
      (prepare-return! *writer* ?type)

      _
      (assert false (str 'prepare-return! " " (&type/show-type *type*))))
    *writer*))

;; [Resources]
(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig> <wrap>]
  (defn <name> [compile ?x ?y]
    (|do [:let [+wrapper-class+ (&host/->class <wrapper-class>)]
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
    (|do [:let [+wrapper-class+ (&host/->class <wrapper-class>)]
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
                    (.visitFieldInsn Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") "TRUE"  (&host/->type-signature "java.lang.Boolean"))
                    (.visitJumpInsn Opcodes/GOTO $end)
                    (.visitLabel $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") "FALSE" (&host/->type-signature "java.lang.Boolean"))
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
    (|do [:let [+wrapper-class+ (&host/->class <wrapper-class>)]
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
                    (.visitFieldInsn Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") "FALSE"  (&host/->type-signature "java.lang.Boolean"))
                    (.visitJumpInsn Opcodes/GOTO $end)
                    (.visitLabel $then)
                    (.visitFieldInsn Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") "TRUE" (&host/->type-signature "java.lang.Boolean"))
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
        :let [method-sig (str "(" (&/fold str "" (&/|map &host/->type-signature ?classes)) ")" (&host/->java-sig ?output-type))]
        _ (&/map2% (fn [class-name arg]
                     (|do [ret (compile arg)
                           :let [_ (prepare-arg! *writer* class-name)]]
                       (return ret)))
                   ?classes ?args)
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESTATIC (&host/->class (&host-type/as-obj ?class)) ?method method-sig)
                  (prepare-return! ?output-type))]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile ?class ?method ?classes ?object ?args ?output-type]
    (|do [:let [?class* (&host/->class (&host-type/as-obj ?class))]
          ^MethodVisitor *writer* &/get-writer
          :let [method-sig (str "(" (&/fold str "" (&/|map &host/->type-signature ?classes)) ")" (&host/->java-sig ?output-type))]
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
                  (.visitFieldInsn Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") "FALSE" (&host/->type-signature "java.lang.Boolean"))
                  (.visitJumpInsn Opcodes/GOTO $end)
                  (.visitLabel $then)
                  (.visitFieldInsn Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") "TRUE"  (&host/->type-signature "java.lang.Boolean"))
                  (.visitLabel $end))]]
    (return nil)))

(defn compile-jvm-new [compile ?class ?classes ?args]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [init-sig (str "(" (&/fold str "" (&/|map &host/->type-signature ?classes)) ")V")
              class* (&host/->class ?class)
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

(do-template [<prim-type> <new-name> <load-name> <load-op> <store-name> <store-op> <wrapper> <unwrapper>]
  (do (defn <new-name> [compile ?length]
        (|do [^MethodVisitor *writer* &/get-writer
              _ (compile ?length)
              :let [_ (.visitInsn *writer* Opcodes/L2I)]
              :let [_ (.visitIntInsn *writer* Opcodes/NEWARRAY <prim-type>)]]
          (return nil)))

    (defn <load-name> [compile ?array ?idx]
      (|do [^MethodVisitor *writer* &/get-writer
            _ (compile ?array)
            :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST "[Ljava/lang/Object;")]
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
            :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST "[Ljava/lang/Object;")]
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

  Opcodes/T_BOOLEAN compile-jvm-znewarray compile-jvm-zaload Opcodes/BALOAD compile-jvm-zastore Opcodes/BASTORE &&/wrap-boolean &&/unwrap-boolean
  Opcodes/T_BYTE    compile-jvm-bnewarray compile-jvm-baload Opcodes/BALOAD compile-jvm-bastore Opcodes/BASTORE &&/wrap-byte    &&/unwrap-byte
  Opcodes/T_SHORT   compile-jvm-snewarray compile-jvm-saload Opcodes/SALOAD compile-jvm-sastore Opcodes/SASTORE &&/wrap-short   &&/unwrap-short
  Opcodes/T_INT     compile-jvm-inewarray compile-jvm-iaload Opcodes/IALOAD compile-jvm-iastore Opcodes/IASTORE &&/wrap-int     &&/unwrap-int
  Opcodes/T_LONG    compile-jvm-lnewarray compile-jvm-laload Opcodes/LALOAD compile-jvm-lastore Opcodes/LASTORE &&/wrap-long    &&/unwrap-long
  Opcodes/T_FLOAT   compile-jvm-fnewarray compile-jvm-faload Opcodes/FALOAD compile-jvm-fastore Opcodes/FASTORE &&/wrap-float   &&/unwrap-float
  Opcodes/T_DOUBLE  compile-jvm-dnewarray compile-jvm-daload Opcodes/DALOAD compile-jvm-dastore Opcodes/DASTORE &&/wrap-double  &&/unwrap-double
  Opcodes/T_CHAR    compile-jvm-cnewarray compile-jvm-caload Opcodes/CALOAD compile-jvm-castore Opcodes/CASTORE &&/wrap-char    &&/unwrap-char
  )

(defn compile-jvm-anewarray [compile ?class ?length]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?length)
        :let [_ (.visitInsn *writer* Opcodes/L2I)]
        :let [_ (.visitTypeInsn *writer* Opcodes/ANEWARRAY (&host/->class ?class))]]
    (return nil)))

(defn compile-jvm-aaload [compile ?array ?idx]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST "[Ljava/lang/Object;")]
        _ (compile ?idx)
        :let [_ (doto *writer*
                  &&/unwrap-long
                  (.visitInsn Opcodes/L2I))]
        :let [_ (.visitInsn *writer* Opcodes/AALOAD)]]
    (return nil)))

(defn compile-jvm-aastore [compile ?array ?idx ?elem]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?array)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST "[Ljava/lang/Object;")]
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
        _ (compile ?array)
        ;; :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST "[Ljava/lang/Object;")]
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/ARRAYLENGTH)
                  (.visitInsn Opcodes/I2L)
                  &&/wrap-long)]]
    (return nil)))

(defn compile-jvm-getstatic [compile ?class ?field ?output-type]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitFieldInsn Opcodes/GETSTATIC (&host/->class (&host-type/as-obj ?class)) ?field (&host/->java-sig ?output-type))
                  (prepare-return! ?output-type))]]
    (return nil)))

(defn compile-jvm-getfield [compile ?class ?field ?object ?output-type]
  (|do [:let [class* (&host/->class (&host-type/as-obj ?class))]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST class*)
                  (.visitFieldInsn Opcodes/GETFIELD class* ?field (&host/->java-sig ?output-type))
                  (prepare-return! ?output-type))]]
    (return nil)))

(defn compile-jvm-putstatic [compile ?class ?field ?value ?output-type]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?value)
        :let [_ (.visitFieldInsn *writer* Opcodes/PUTSTATIC (&host/->class (&host-type/as-obj ?class)) ?field (&host/->java-sig ?output-type))]
        :let [_ (.visitInsn *writer* Opcodes/ACONST_NULL)]]
    (return nil)))

(defn compile-jvm-putfield [compile ?class ?field ?object ?value ?output-type]
  (|do [:let [class* (&host/->class (&host-type/as-obj ?class))]
        ^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        :let [_ (.visitInsn *writer* Opcodes/DUP)]
        _ (compile ?value)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST class*)]
        :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD class* ?field (&host/->java-sig ?output-type))]]
    (return nil)))

(defn compile-jvm-instanceof [compile class object]
  (|do [:let [class* (&host/->class class)]
        ^MethodVisitor *writer* &/get-writer
        _ (compile object)
        :let [_ (doto *writer*
                  (.visitTypeInsn Opcodes/INSTANCEOF class*)
                  (&&/wrap-boolean))]]
    (return nil)))

(defn ^:private compile-annotation [writer ann]
  (doto ^AnnotationVisitor (.visitAnnotation writer (&host/->class (:name ann)) true)
        (-> (.visit param-name param-value)
            (->> (|let [[param-name param-value] param])
                 (doseq [param (&/->seq (:params ann))])))
        (.visitEnd))
  nil)

(defn ^:private compile-field [^ClassWriter writer field]
  (let [=field (.visitField writer (&host/modifiers->int (:modifiers field)) (:name field)
                            (&host/->type-signature (:type field)) nil nil)]
    (&/|map (partial compile-annotation =field) (:anns field))
    (.visitEnd =field)
    nil))

(defn ^:private compile-method-return [^MethodVisitor writer output]
  (case output
    "void" (.visitInsn writer Opcodes/RETURN)
    "boolean" (doto writer
                &&/unwrap-boolean
                (.visitInsn Opcodes/IRETURN))
    "byte" (doto writer
             &&/unwrap-byte
             (.visitInsn Opcodes/IRETURN))
    "short" (doto writer
              &&/unwrap-short
              (.visitInsn Opcodes/IRETURN))
    "int" (doto writer
            &&/unwrap-int
            (.visitInsn Opcodes/IRETURN))
    "long" (doto writer
             &&/unwrap-long
             (.visitInsn Opcodes/LRETURN))
    "float" (doto writer
              &&/unwrap-float
              (.visitInsn Opcodes/FRETURN))
    "double" (doto writer
               &&/unwrap-double
               (.visitInsn Opcodes/DRETURN))
    "char" (doto writer
             &&/unwrap-char
             (.visitInsn Opcodes/IRETURN))
    ;; else
    (.visitInsn writer Opcodes/ARETURN)))

(defn ^:private compile-method [compile ^ClassWriter class-writer method]
  (|let [signature (str "(" (&/fold str "" (&/|map &host/->type-signature (:inputs method))) ")"
                        (&host/->type-signature (:output method)))]
    (&/with-writer (.visitMethod class-writer (&host/modifiers->int (:modifiers method))
                                 (:name method)
                                 signature
                                 nil
                                 (->> (:exceptions method) (&/|map &host/->class) &/->seq (into-array java.lang.String)))
      (|do [^MethodVisitor =method &/get-writer
            :let [_ (&/|map (partial compile-annotation =method) (:anns method))
                  _ (.visitCode =method)]
            _ (compile (:body method))
            :let [_ (doto =method
                      (compile-method-return (:output method))
                      (.visitMaxs 0 0)
                      (.visitEnd))]]
        (return nil)))))

(defn ^:private compile-method-decl [^ClassWriter class-writer method]
  (|let [signature (str "(" (&/fold str "" (&/|map &host/->type-signature (:inputs method))) ")"
                        (&host/->type-signature (:output method)))]
    (let [=method (.visitMethod class-writer (&host/modifiers->int (:modifiers method)) (:name method) signature nil (->> (:exceptions method) (&/|map &host/->class) &/->seq (into-array java.lang.String)))]
      (&/|map (partial compile-annotation =method) (:anns method))
      nil)))

(let [clo-field-sig (&host/->type-signature "java.lang.Object")
      <init>-return "V"]
  (defn ^:private anon-class-<init>-signature [env]
    (str "(" (&/fold str "" (&/|repeat (&/|length env) clo-field-sig)) ")"
         <init>-return))

  (defn ^:private add-anon-class-<init> [^ClassWriter class-writer class-name super-class env]
    (doto (.visitMethod class-writer Opcodes/ACC_PUBLIC "<init>" (anon-class-<init>-signature env) nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL (&host/->class super-class) "<init>" "()V")
      (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
            (.visitVarInsn Opcodes/ALOAD (inc ?captured-id))
            (.visitFieldInsn Opcodes/PUTFIELD class-name captured-name clo-field-sig))
          (->> (let [captured-name (str &&/closure-prefix ?captured-id)])
               (|case ?name+?captured
                 [?name [_ (&a/$captured _ ?captured-id ?source)]])
               (doseq [?name+?captured (&/->seq env)])))
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd)))
  )

(defn compile-jvm-class [compile ?name ?super-class ?interfaces ?anns ?fields ?methods env]
  (|do [module &/get-module-name
        [file-name _ _] &/cursor
        :let [full-name (str module "/" ?name)
              super-class* (&host/->class ?super-class)
              =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                               full-name nil super-class* (->> ?interfaces (&/|map &host/->class) &/->seq (into-array String)))
                       (.visitSource file-name nil))
              _ (&/|map (partial compile-annotation =class) ?anns)
              _ (&/|map (partial compile-field =class)
                        ?fields)]
        _ (&/map% (partial compile-method compile =class) ?methods)
        :let [_ (when env
                  (add-anon-class-<init> =class full-name ?super-class env))]]
    (&&/save-class! ?name (.toByteArray (doto =class .visitEnd)))))

(defn compile-jvm-interface [compile ?name ?supers ?anns ?methods]
  (|do [module &/get-module-name
        [file-name _ _] &/cursor]
    (let [=interface (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_INTERFACE)
                               (str module "/" ?name) nil "java/lang/Object" (->> ?supers (&/|map &host/->class) &/->seq (into-array String)))
                       (.visitSource file-name nil))
          _ (&/|map (partial compile-annotation =interface) ?anns)
          _ (do (&/|map (partial compile-method-decl =interface) ?methods)
              (.visitEnd =interface))]
      (&&/save-class! ?name (.toByteArray =interface)))))

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
                    (.visitTryCatchBlock $from $to $handler-start (&host/->class ?ex-class))
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
                    (.visitTypeInsn Opcodes/NEW (&host/->class <to-class>))
                    (.visitInsn Opcodes/DUP))]
          _ (compile ?value)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST (&host/->class <from-class>))
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class <from-class>) <from-method> <from-sig>)
                    (.visitInsn <op>)
                    (.visitMethodInsn Opcodes/INVOKESPECIAL (&host/->class <to-class>) "<init>" <to-sig>))]]
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
                    (.visitTypeInsn Opcodes/NEW (&host/->class <to-class>))
                    (.visitInsn Opcodes/DUP))]
          _ (compile ?x)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST (&host/->class <from1-class>))
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class <from1-class>) <from1-method> <from1-sig>))]
          _ (compile ?y)
          :let [_ (doto *writer*
                    (.visitTypeInsn Opcodes/CHECKCAST (&host/->class <from2-class>))
                    (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class <from2-class>) <from2-method> <from2-sig>))]
          :let [_ (doto *writer*
                    (.visitInsn <op>)
                    (.visitMethodInsn Opcodes/INVOKESPECIAL (&host/->class <to-class>) "<init>" <to-sig>))]]
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
