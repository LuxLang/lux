(ns lux.compiler.host
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let]]
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
    (matchv ::M/objects [*type*]
      [["lux;VariantT" ["lux;Nil" _]]]
      (.visitInsn *writer* Opcodes/ACONST_NULL)

      [["lux;DataT" "boolean"]]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class boolean-class) "valueOf" (str "(Z)" (&host/->type-signature boolean-class)))
      
      [["lux;DataT" "byte"]]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class byte-class) "valueOf" (str "(B)" (&host/->type-signature byte-class)))

      [["lux;DataT" "short"]]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class short-class) "valueOf" (str "(S)" (&host/->type-signature short-class)))

      [["lux;DataT" "int"]]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class int-class) "valueOf" (str "(I)" (&host/->type-signature int-class)))

      [["lux;DataT" "long"]]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class long-class) "valueOf" (str "(J)" (&host/->type-signature long-class)))

      [["lux;DataT" "float"]]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class float-class) "valueOf" (str "(F)" (&host/->type-signature float-class)))

      [["lux;DataT" "double"]]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class double-class) "valueOf" (str "(D)" (&host/->type-signature double-class)))

      [["lux;DataT" "char"]]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (&host/->class char-class) "valueOf" (str "(C)" (&host/->type-signature char-class)))
      
      [["lux;DataT" _]]
      nil)
    *writer*))

;; [Resources]
(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig> <wrapper-method> <wrapper-method-sig>]
  (defn <name> [compile *type* ?x ?y]
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
  (defn <name> [compile *type* ?x ?y]
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
  
  compile-jvm-deq Opcodes/DCMPG  0 "java.lang.Double" "doubleValue" "()I"
  compile-jvm-dlt Opcodes/DCMPG  1 "java.lang.Double" "doubleValue" "()I"
  compile-jvm-dgt Opcodes/FCMPG -1 "java.lang.Double" "doubleValue" "()I"
  )

(defn compile-jvm-invokestatic [compile *type* ?class ?method ?classes ?args]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [method-sig (str "(" (&/fold str "" (&/|map &host/->type-signature ?classes)) ")" (&host/->java-sig *type*))]
        _ (&/map2% (fn [class-name arg]
                     (|do [ret (compile arg)
                           :let [_ (prepare-arg! *writer* class-name)]]
                       (return ret)))
                   ?classes ?args)
        :let [_ (doto *writer*
                  (.visitMethodInsn Opcodes/INVOKESTATIC (&host/->class ?class) ?method method-sig)
                  (prepare-return! *type*))]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile *type* ?class ?method ?classes ?object ?args]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [method-sig (str "(" (&/fold str "" (&/|map &host/->type-signature ?classes)) ")" (&host/->java-sig *type*))]
          _ (compile ?object)
          :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST (&host/->class ?class))]
          _ (&/map2% (fn [class-name arg]
                       (|do [ret (compile arg)
                             :let [_ (prepare-arg! *writer* class-name)]]
                         (return ret)))
                     ?classes ?args)
          :let [_ (doto *writer*
                    (.visitMethodInsn <op> (&host/->class ?class) ?method method-sig)
                    (prepare-return! *type*))]]
      (return nil)))

  compile-jvm-invokevirtual   Opcodes/INVOKEVIRTUAL
  compile-jvm-invokeinterface Opcodes/INVOKEINTERFACE
  compile-jvm-invokespecial   Opcodes/INVOKESPECIAL
  )

(defn compile-jvm-null [compile *type*]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitInsn *writer* Opcodes/ACONST_NULL)]]
    (return nil)))

(defn compile-jvm-null? [compile *type* ?object]
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

(defn compile-jvm-new [compile *type* ?class ?classes ?args]
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

(defn compile-jvm-new-array [compile *type* ?class ?length]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitLdcInsn (int ?length))
                  (.visitTypeInsn Opcodes/ANEWARRAY (&host/->class ?class)))]]
    (return nil)))

(defn compile-jvm-aastore [compile *type* ?array ?idx ?elem]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?array)
        :let [_ (doto *writer*
                  (.visitInsn Opcodes/DUP)
                  (.visitLdcInsn (int ?idx)))]
        _ (compile ?elem)
        :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
    (return nil)))

(defn compile-jvm-aaload [compile *type* ?array ?idx]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?array)
        :let [_ (doto *writer*
                  (.visitLdcInsn (int ?idx))
                  (.visitInsn Opcodes/AALOAD))]]
    (return nil)))

(defn compile-jvm-getstatic [compile *type* ?class ?field]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (&host/->class ?class) ?field (&host/->java-sig *type*))]]
    (return nil)))

(defn compile-jvm-getfield [compile *type* ?class ?field ?object]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST (&host/->class ?class))]
        :let [_ (.visitFieldInsn *writer* Opcodes/GETFIELD (&host/->class ?class) ?field (&host/->java-sig *type*))]]
    (return nil)))

(defn compile-jvm-putstatic [compile *type* ?class ?field ?value]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?value)
        :let [_ (.visitFieldInsn *writer* Opcodes/PUTSTATIC (&host/->class ?class) ?field (&host/->java-sig *type*))]]
    (return nil)))

(defn compile-jvm-putfield [compile *type* ?class ?field ?object ?value]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?object)
        _ (compile ?value)
        :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST (&host/->class ?class))]
        :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD (&host/->class ?class) ?field (&host/->java-sig *type*))]]
    (return nil)))

(defn ^:private modifiers->int [mods]
  (+ (case (:visibility mods)
       "default" 0
       "public" Opcodes/ACC_PUBLIC
       "private" Opcodes/ACC_PRIVATE
       "protected" Opcodes/ACC_PROTECTED)
     (if (:static? mods) Opcodes/ACC_STATIC 0)
     (if (:final? mods) Opcodes/ACC_FINAL 0)
     (if (:abstract? mods) Opcodes/ACC_ABSTRACT 0)
     (case (:concurrency mods)
       "synchronized" Opcodes/ACC_SYNCHRONIZED
       "volatile" Opcodes/ACC_VOLATILE
       ;; else
       0)))

(defn compile-jvm-instanceof [compile *type* class object]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile object)
        :let [_ (doto *writer*
                  (.visitLdcInsn class)
                  (.visitTypeInsn Opcodes/INSTANCEOF class))]]
    (return nil)))

(defn compile-jvm-class [compile ?name ?super-class ?interfaces ?fields ?methods]
  (|do [module &/get-module-name]
    (let [super-class* (&host/->class ?super-class)
          =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                   (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                           (str module "/" ?name) nil super-class* (->> ?interfaces (&/|map &host/->class) &/->seq (into-array String))))
          _ (&/|map (fn [field]
                      (doto (.visitField =class (modifiers->int (:modifiers field)) (:name field)
                                         (&host/->type-signature (:type field)) nil nil)
                        (.visitEnd)))
                    ?fields)]
      (|do [_ (&/map% (fn [method]
                        (|let [signature (str "(" (&/fold str "" (&/|map &host/->type-signature (:inputs method))) ")"
                                              (&host/->type-signature (:output method)))]
                          (&/with-writer (.visitMethod =class (modifiers->int (:modifiers method))
                                                       (:name method)
                                                       signature nil nil)
                            (|do [^MethodVisitor =method &/get-writer
                                  :let [_ (.visitCode =method)]
                                  _ (compile (:body method))
                                  :let [_ (doto =method
                                            (.visitInsn (if (= "void" (:output method)) Opcodes/RETURN Opcodes/ARETURN))
                                            (.visitMaxs 0 0)
                                            (.visitEnd))]]
                              (return nil)))))
                      ?methods)]
        (&&/save-class! ?name (.toByteArray (doto =class .visitEnd)))))))

(defn compile-jvm-interface [compile ?name ?supers ?methods]
  ;; (prn 'compile-jvm-interface (->> ?supers &/->seq pr-str))
  (|do [module &/get-module-name]
    (let [=interface (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_INTERFACE)
                               (str module "/" ?name) nil "java/lang/Object" (->> ?supers (&/|map &host/->class) &/->seq (into-array String))))
          _ (do (&/|map (fn [method]
                          (|let [signature (str "(" (&/fold str "" (&/|map &host/->type-signature (:inputs method))) ")"
                                                (&host/->type-signature (:output method)))]
                            (.visitMethod =interface (modifiers->int (:modifiers method)) (:name method) signature nil nil)))
                        ?methods)
              (.visitEnd =interface))]
      (&&/save-class! ?name (.toByteArray =interface)))))

(defn compile-jvm-try [compile *type* ?body ?catches ?finally]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [$from (new Label)
              $to (new Label)
              $end (new Label)
              $catch-finally (new Label)
              compile-finally (if ?finally
                                (|do [_ (return nil)
                                      _ (compile ?finally)
                                      :let [_ (doto *writer*
                                                (.visitInsn Opcodes/POP)
                                                (.visitJumpInsn Opcodes/GOTO $end))]]
                                  (return nil))
                                (|do [_ (return nil)
                                      :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)]]
                                  (return nil)))
              _ (.visitLabel *writer* $from)]
        _ (compile ?body)
        :let [_ (.visitLabel *writer* $to)]
        _ compile-finally
        handlers (&/map% (fn [[?ex-class ?ex-arg ?catch-body]]
                           (|do [:let [$handler-start (new Label)
                                       $handler-end (new Label)]
                                 _ (compile ?catch-body)
                                 :let [_ (.visitLabel *writer* $handler-end)]
                                 _ compile-finally]
                             (return [?ex-class $handler-start $handler-end])))
                         ?catches)
        :let [_ (.visitLabel *writer* $catch-finally)]
        _ (if ?finally
            (|do [_ (compile ?finally)
                  :let [_ (doto *writer*
                            (.visitInsn Opcodes/POP)
                            (.visitInsn Opcodes/ATHROW))]]
              (return nil))
            (|do [_ (return nil)
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
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile ?ex)
        :let [_ (.visitInsn *writer* Opcodes/ATHROW)]]
    (return nil)))

(do-template [<name> <op>]
  (defn <name> [compile *type* ?monitor]
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
  (defn <name> [compile *type* ?value]
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
  (defn <name> [compile *type* ?x ?y]
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
  
  compile-jvm-land  Opcodes/LAND  "longValue" "()J" "java.lang.Long"    "longValue" "()J" "java.lang.Long"    "java.lang.Long"    "(J)V"
  compile-jvm-lor   Opcodes/LOR   "longValue" "()J" "java.lang.Long"    "longValue" "()J" "java.lang.Long"    "java.lang.Long"    "(J)V"
  compile-jvm-lxor  Opcodes/LXOR  "longValue" "()J" "java.lang.Long"    "longValue" "()J" "java.lang.Long"    "java.lang.Long"    "(J)V"

  compile-jvm-lshl  Opcodes/LSHL  "longValue" "()J" "java.lang.Long"    "intValue"  "()I" "java.lang.Integer" "java.lang.Long"    "(J)V"
  compile-jvm-lshr  Opcodes/LSHR  "longValue" "()J" "java.lang.Long"    "intValue"  "()I" "java.lang.Integer" "java.lang.Long"    "(J)V"
  compile-jvm-lushr Opcodes/LUSHR "longValue" "()J" "java.lang.Long"    "intValue"  "()I" "java.lang.Integer" "java.lang.Long"    "(J)V"
  )

(defn compile-jvm-program [compile ?body]
  (|do [^ClassWriter *writer* &/get-writer]
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
                      (.visitLdcInsn "lux;Nil") ;; VVIT
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
                      (.visitLdcInsn "lux;Cons") ;; I2VVIT
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
                      )]
            _ (compile ?body)
            :let [_ (doto main-writer
                      (.visitInsn Opcodes/ACONST_NULL)
                      (.visitMethodInsn Opcodes/INVOKEINTERFACE "lux/Function" "apply" &&/apply-signature))]
            :let [_ (doto main-writer
                          (.visitInsn Opcodes/POP)
                          (.visitInsn Opcodes/RETURN)
                          (.visitMaxs 0 0)
                          (.visitEnd))]]
        (return nil)))))
