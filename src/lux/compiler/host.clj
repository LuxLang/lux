(ns lux.compiler.host
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return* return fail fail*
                                     repeat-m exhaust-m try-m try-all-m map-m reduce-m
                                     apply-m
                                     normalize-ident]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.analyser.base :as &a]
            [lux.compiler.base :as &&]
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils]
(let [class+method+sig {"boolean" [(&host/->class "java.lang.Boolean")   "booleanValue" "()Z"]
                        "byte"    [(&host/->class "java.lang.Byte")      "byteValue"    "()B"]
                        "short"   [(&host/->class "java.lang.Short")     "shortValue"   "()S"]
                        "int"     [(&host/->class "java.lang.Integer")   "intValue"     "()I"]
                        "long"    [(&host/->class "java.lang.Long")      "longValue"    "()J"]
                        "float"   [(&host/->class "java.lang.Float")     "floatValue"   "()F"]
                        "double"  [(&host/->class "java.lang.Double")    "doubleValue"  "()D"]
                        "char"    [(&host/->class "java.lang.Character") "charValue"    "()C"]}]
  (defn ^:private prepare-arg! [*writer* class-name]
    (if-let [[class method sig] (get class+method+sig class-name)]
      (doto *writer*
        (.visitTypeInsn Opcodes/CHECKCAST class)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL class method sig))
      (.visitTypeInsn *writer* Opcodes/CHECKCAST (&host/->class class-name)))))

(let [boolean-class "java.lang.Boolean"
      integer-class "java.lang.Integer"
      long-class "java.lang.Long"
      char-class "java.lang.Character"]
  (defn prepare-return! [*writer* *type*]
    (match *type*
      [::&type/Nothing]
      (.visitInsn *writer* Opcodes/ACONST_NULL)

      [::&type/Data "char"]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class char-class) "valueOf" (str "(C)" (&host/->type-signature char-class)))

      [::&type/Data "int"]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class integer-class) "valueOf" (str "(I)" (&host/->type-signature integer-class)))

      [::&type/Data "long"]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class long-class) "valueOf" (str "(J)" (&host/->type-signature long-class)))

      [::&type/Data "boolean"]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class boolean-class) "valueOf" (str "(Z)" (&host/->type-signature boolean-class)))

      [::&type/Data _]
      nil
      )
    *writer*))

;; [Resources]
(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig> <wrapper-method> <wrapper-method-sig>]
  (defn <name> [compile *type* ?x ?y]
    (exec [:let [+wrapper-class+ (&host/->class <wrapper-class>)]
           *writer* &/get-writer
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
                     (.visitMethodInsn Opcodes/INVOKESTATIC +wrapper-class+ <wrapper-method> (str <wrapper-method-sig> (&host/->type-signature <wrapper-class>))))]]
      (return nil)))

  compile-jvm-iadd Opcodes/IADD "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  compile-jvm-isub Opcodes/ISUB "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  compile-jvm-imul Opcodes/IMUL "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  compile-jvm-idiv Opcodes/IDIV "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  compile-jvm-irem Opcodes/IREM "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  
  compile-jvm-ladd Opcodes/LADD "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"
  compile-jvm-lsub Opcodes/LSUB "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"
  compile-jvm-lmul Opcodes/LMUL "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"
  compile-jvm-ldiv Opcodes/LDIV "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"
  compile-jvm-lrem Opcodes/LREM "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"

  compile-jvm-fadd Opcodes/FADD "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  compile-jvm-fsub Opcodes/FSUB "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  compile-jvm-fmul Opcodes/FMUL "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  compile-jvm-fdiv Opcodes/FDIV "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  compile-jvm-frem Opcodes/FREM "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  
  compile-jvm-dadd Opcodes/DADD "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  compile-jvm-dsub Opcodes/DSUB "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  compile-jvm-dmul Opcodes/DMUL "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  compile-jvm-ddiv Opcodes/DDIV "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  compile-jvm-drem Opcodes/DREM "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  )

(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig>]
  (defn <name> [compile *type* ?x ?y]
    (exec [:let [+wrapper-class+ (&host/->class <wrapper-class>)]
           *writer* &/get-writer
           _ (compile ?x)
           :let [_ (doto *writer*
                     (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                     (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))]
           _ (compile ?y)
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
  )

(do-template [<name> <cmpcode> <ifcode> <wrapper-class> <value-method> <value-method-sig>]
  (defn <name> [compile *type* ?x ?y]
    (exec [:let [+wrapper-class+ (&host/->class <wrapper-class>)]
           *writer* &/get-writer
           _ (compile ?x)
           :let [_ (doto *writer*
                     (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                     (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))]
           _ (compile ?y)
           :let [_ (doto *writer*
                     (.visitTypeInsn Opcodes/CHECKCAST +wrapper-class+)
                     (.visitMethodInsn Opcodes/INVOKEVIRTUAL +wrapper-class+ <value-method> <value-method-sig>))
                 $then (new Label)
                 $end (new Label)
                 _ (doto *writer*
                     (.visitInsn <cmpcode>)
                     (.visitJumpInsn <ifcode> $then)
                     (.visitFieldInsn Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") "TRUE"  (&host/->type-signature "java.lang.Boolean"))
                     (.visitJumpInsn Opcodes/GOTO $end)
                     (.visitLabel $then)
                     (.visitFieldInsn Opcodes/GETSTATIC (&host/->class "java.lang.Boolean") "FALSE" (&host/->type-signature "java.lang.Boolean"))
                     (.visitLabel $end))]]
      (return nil)))

  compile-jvm-leq Opcodes/LCMP  Opcodes/IFEQ "java.lang.Long"   "longValue"   "()J"
  compile-jvm-llt Opcodes/LCMP  Opcodes/IFLT "java.lang.Long"   "longValue"   "()J"
  compile-jvm-lgt Opcodes/LCMP  Opcodes/IFGT "java.lang.Long"   "longValue"   "()J"

  compile-jvm-feq Opcodes/FCMPG Opcodes/IFEQ "java.lang.Float"  "floatValue"  "()F"
  compile-jvm-flt Opcodes/FCMPG Opcodes/IFLT "java.lang.Float"  "floatValue"  "()F"
  compile-jvm-fgt Opcodes/FCMPG Opcodes/IFGT "java.lang.Float"  "floatValue"  "()F"
  
  compile-jvm-deq Opcodes/DCMPG Opcodes/IFEQ "java.lang.Double" "doubleValue" "()I"
  compile-jvm-dlt Opcodes/DCMPG Opcodes/IFLT "java.lang.Double" "doubleValue" "()I"
  compile-jvm-dgt Opcodes/FCMPG Opcodes/IFGT "java.lang.Double" "doubleValue" "()I"
  )

(defn compile-jvm-invokestatic [compile *type* ?class ?method ?classes ?args]
  (exec [*writer* &/get-writer
         :let [method-sig (str "(" (reduce str "" (map &host/->type-signature ?classes)) ")" (&host/->java-sig *type*))]
         _ (map-m (fn [[class-name arg]]
                    (exec [ret (compile arg)
                           :let [_ (prepare-arg! *writer* class-name)]]
                      (return ret)))
                  (map vector ?classes ?args))
         :let [_ (doto *writer*
                   (.visitMethodInsn Opcodes/INVOKESTATIC (&host/->class ?class) ?method method-sig)
                   (prepare-return! *type*))]]
    (return nil)))

(defn compile-jvm-invokevirtual [compile *type* ?class ?method ?classes ?object ?args]
  ;; (prn 'compile-jvm-invokevirtual ?classes *type*)
  (exec [*writer* &/get-writer
         :let [method-sig (str "(" (reduce str "" (map &host/->type-signature ?classes)) ")" (&host/->java-sig *type*))]
         _ (compile ?object)
         :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST (&host/->class ?class))]
         _ (map-m (fn [[class-name arg]]
                    (exec [ret (compile arg)
                           :let [_ (prepare-arg! *writer* class-name)]]
                      (return ret)))
                  (map vector ?classes ?args))
         :let [_ (doto *writer*
                   (.visitMethodInsn Opcodes/INVOKEVIRTUAL (&host/->class ?class) ?method method-sig)
                   (prepare-return! *type*))]]
    (return nil)))

(defn compile-jvm-null [compile *type*]
  (exec [*writer* &/get-writer
         :let [_ (.visitInsn *writer* Opcodes/ACONST_NULL)]]
    (return nil)))

(defn compile-jvm-null? [compile *type* ?object]
  (exec [*writer* &/get-writer
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

(defn compile-jvm-new [compile *type* ?class ?classes ?args]
  (exec [*writer* &/get-writer
         :let [init-sig (str "(" (reduce str "" (map &host/->type-signature ?classes)) ")V")
               class* (&host/->class ?class)
               _ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW class*)
                   (.visitInsn Opcodes/DUP))]
         _ (map-m (fn [[class-name arg]]
                    (exec [ret (compile arg)
                           :let [_ (prepare-arg! *writer* class-name)]]
                      (return ret)))
                  (map vector ?classes ?args))
         :let [_ (doto *writer*
                   (.visitMethodInsn Opcodes/INVOKESPECIAL class* "<init>" init-sig))]]
    (return nil)))

(defn compile-jvm-new-array [compile *type* ?class ?length]
  (exec [*writer* &/get-writer
         :let [_ (doto *writer*
                   (.visitLdcInsn (int ?length))
                   (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class ?class)))]]
    (return nil)))

(defn compile-jvm-aastore [compile *type* ?array ?idx ?elem]
  (exec [*writer* &/get-writer
         _ (compile ?array)
         :let [_ (doto *writer*
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn (int ?idx)))]
         _ (compile ?elem)
         :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
    (return nil)))

(defn compile-jvm-aaload [compile *type* ?array ?idx]
  (exec [*writer* &/get-writer
         _ (compile ?array)
         :let [_ (doto *writer*
                   (.visitLdcInsn (int ?idx))
                   (.visitInsn Opcodes/AALOAD))]]
    (return nil)))

(defn compile-jvm-getstatic [compile *type* ?class ?field]
  (exec [*writer* &/get-writer
         :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (&host/->class ?class) ?field (&host/->java-sig *type*))]]
    (return nil)))

(defn compile-jvm-getfield [compile *type* ?class ?field ?object]
  (exec [*writer* &/get-writer
         _ (compile ?object)
         :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST (&host/->class ?class))]
         :let [_ (.visitFieldInsn *writer* Opcodes/GETFIELD (&host/->class ?class) ?field (&host/->java-sig *type*))]]
    (return nil)))

(defn compile-jvm-class [compile ?package ?name ?super-class ?fields ?methods]
  (let [parent-dir (&host/->package ?package)
        full-name (str parent-dir "/" ?name)
        super-class* (&host/->class ?super-class)
        =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                 (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                         full-name nil super-class* nil))
        _ (do (doseq [[field props] ?fields]
                (doto (.visitField =class Opcodes/ACC_PUBLIC field (&host/->type-signature (:type props)) nil nil)
                  (.visitEnd)))
            (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
              (.visitCode)
              (.visitVarInsn Opcodes/ALOAD 0)
              (.visitMethodInsn Opcodes/INVOKESPECIAL super-class* "<init>" "()V")
              (.visitInsn Opcodes/RETURN)
              (.visitMaxs 0 0)
              (.visitEnd))
            (.visitEnd =class)
            (.mkdirs (java.io.File. (str "output/" parent-dir))))]
    (&&/save-class! full-name (.toByteArray =class))))

(defn compile-jvm-interface [compile ?package ?name ?methods]
  (let [parent-dir (&host/->package ?package)
        full-name (str parent-dir "/" ?name)
        =interface (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                     (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_INTERFACE)
                             full-name nil "java/lang/Object" nil))
        _ (do (doseq [[?method ?props] ?methods
                      :let [[?args ?return] (:type ?props)
                            signature (str "(" (reduce str "" (map &host/->type-signature ?args)) ")" (&host/->type-signature ?return))]]
                (.visitMethod =interface (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT) ?method signature nil nil))
            (.visitEnd =interface)
            (.mkdirs (java.io.File. (str "output/" parent-dir))))]
    (&&/save-class! full-name (.toByteArray =interface))))

(defn compile-exec [compile *type* ?exprs]
  (exec [*writer* &/get-writer
         _ (map-m (fn [expr]
                    (exec [ret (compile expr)
                           :let [_ (.visitInsn *writer* Opcodes/POP)]]
                      (return ret)))
                  (butlast ?exprs))
         _ (compile (last ?exprs))]
    (return nil)))

(defn compile-jvm-try [compile *type* ?body ?catches ?finally]
  (exec [*writer* &/get-writer
         :let [$from (new Label)
               $to (new Label)
               $end (new Label)
               $catch-finally (new Label)
               compile-finally (if ?finally
                                 (exec [_ (return nil)
                                        _ (compile ?finally)
                                        :let [_ (doto *writer*
                                                  (.visitInsn Opcodes/POP)
                                                  (.visitJumpInsn Opcodes/GOTO $end))]]
                                   (return nil))
                                 (exec [_ (return nil)
                                        :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)]]
                                   (return nil)))
               _ (.visitLabel *writer* $from)]
         _ (compile ?body)
         :let [_ (.visitLabel *writer* $to)]
         _ compile-finally
         handlers (map-m (fn [[?ex-class ?ex-arg ?catch-body]]
                           (exec [:let [$handler-start (new Label)
                                        $handler-end (new Label)]
                                  _ (compile ?catch-body)
                                  :let [_ (.visitLabel *writer* $handler-end)]
                                  _ compile-finally]
                             (return [?ex-class $handler-start $handler-end])))
                         ?catches)
         :let [_ (.visitLabel *writer* $catch-finally)]
         _ (if ?finally
             (exec [_ (compile ?finally)
                    :let [_ (doto *writer*
                              (.visitInsn Opcodes/POP)
                              (.visitInsn Opcodes/ATHROW))]]
               (return nil))
             (exec [_ (return nil)
                    :let [_ (.visitInsn *writer* Opcodes/ATHROW)]]
               (return nil)))
         :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)]
         :let [_ (.visitLabel *writer* $end)]
         :let [_ (doseq [[?ex-class $handler-start $handler-end] handlers]
                   (doto *writer*
                     (.visitTryCatchBlock $from $to $handler-start ?ex-class)
                     (.visitTryCatchBlock $handler-start $handler-end $catch-finally nil))
                   )
               _ (.visitTryCatchBlock *writer* $from $to $catch-finally nil)]]
    (return nil)))

(defn compile-jvm-throw [compile *type* ?ex]
  (exec [*writer* &/get-writer
         _ (compile ?ex)
         :let [_ (.visitInsn *writer* Opcodes/ATHROW)]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile *type* ?monitor]
    (exec [*writer* &/get-writer
           _ (compile ?monitor)
           :let [_ (doto *writer*
                     (.visitInsn <op>)
                     (.visitInsn Opcodes/ACONST_NULL))]]
      (return nil)))

  compile-jvm-monitorenter Opcodes/MONITORENTER
  compile-jvm-monitorexit  Opcodes/MONITOREXIT
  )

(do-template [<name> <op> <from-class> <to-class>]
  (defn <name> [compile *type* ?value]
    (exec [*writer* &/get-writer
           _ (compile ?value)
           :let [_ (doto *writer*
                     (.visitInsn <op>))]]
      (return nil)))

  compile-jvm-d2f Opcodes/D2F "java.lang.Double"  "java.lang.Float"
  compile-jvm-d2i Opcodes/D2I "java.lang.Double"  "java.lang.Integer"
  compile-jvm-d2l Opcodes/D2L "java.lang.Double"  "java.lang.Long"

  compile-jvm-f2d Opcodes/F2D "java.lang.Float"   "java.lang.Double"
  compile-jvm-f2i Opcodes/F2I "java.lang.Float"   "java.lang.Integer"
  compile-jvm-f2l Opcodes/F2L "java.lang.Float"   "java.lang.Long"

  compile-jvm-i2b Opcodes/I2B "java.lang.Integer" "java.lang.Byte"
  compile-jvm-i2c Opcodes/I2C "java.lang.Integer" "java.lang.Character"
  compile-jvm-i2d Opcodes/I2D "java.lang.Integer" "java.lang.Double"
  compile-jvm-i2f Opcodes/I2F "java.lang.Integer" "java.lang.Float"
  compile-jvm-i2l Opcodes/I2L "java.lang.Integer" "java.lang.Long"
  compile-jvm-i2s Opcodes/I2S "java.lang.Integer" "java.lang.Short"

  compile-jvm-l2d Opcodes/L2D "java.lang.Long"    "java.lang.Double"
  compile-jvm-l2f Opcodes/L2F "java.lang.Long"    "java.lang.Float"
  compile-jvm-l2i Opcodes/L2I "java.lang.Long"    "java.lang.Integer"
  )

(do-template [<name> <op> <from-class> <to-class>]
  (defn <name> [compile *type* ?x ?y]
    (exec [*writer* &/get-writer
           _ (compile ?x)
           _ (compile ?y)
           :let [_ (doto *writer*
                     (.visitInsn <op>))]]
      (return nil)))

  compile-jvm-iand  Opcodes/IAND  "java.lang.Integer" "java.lang.Integer"
  compile-jvm-ior   Opcodes/IOR   "java.lang.Integer" "java.lang.Integer"
  
  compile-jvm-land  Opcodes/LAND  "java.lang.Long"    "java.lang.Long"
  compile-jvm-lor   Opcodes/LOR   "java.lang.Long"    "java.lang.Long"
  compile-jvm-lxor  Opcodes/LXOR  "java.lang.Long"    "java.lang.Long"

  compile-jvm-lshl  Opcodes/LSHL  "java.lang.Long"    "java.lang.Integer"
  compile-jvm-lshr  Opcodes/LSHR  "java.lang.Long"    "java.lang.Integer"
  compile-jvm-lushr Opcodes/LUSHR "java.lang.Long"    "java.lang.Integer"
  )
