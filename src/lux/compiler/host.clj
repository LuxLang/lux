
(let [class+metthod+sig {"boolean" [(->class "java.lang.Boolean")   "booleanValue" "()Z"]
                         "byte"    [(->class "java.lang.Byte")      "byteValue"    "()B"]
                         "short"   [(->class "java.lang.Short")     "shortValue"   "()S"]
                         "int"     [(->class "java.lang.Integer")   "intValue"     "()I"]
                         "long"    [(->class "java.lang.Long")      "longValue"    "()J"]
                         "float"   [(->class "java.lang.Float")     "floatValue"   "()F"]
                         "double"  [(->class "java.lang.Double")    "doubleValue"  "()D"]
                         "char"    [(->class "java.lang.Character") "charValue"    "()C"]}]
  (defn ^:private prepare-arg! [*writer* class-name]
    (if-let [[class method sig] (get class+metthod+sig class-name)]
      (doto *writer*
        (.visitTypeInsn Opcodes/CHECKCAST class)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL class method sig))
      (.visitTypeInsn *writer* Opcodes/CHECKCAST (->class class-name)))))

;; (let [boolean-class "java.lang.Boolean"
;;       integer-class "java.lang.Integer"
;;       char-class "java.lang.Character"]
;;   (defn prepare-return! [*writer* *type*]
;;     (match *type*
;;       ::&type/nothing
;;       (.visitInsn *writer* Opcodes/ACONST_NULL)

;;       [::&type/primitive "char"]
;;       (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (->class char-class) "valueOf" (str "(C)" (->type-signature char-class)))

;;       [::&type/primitive "int"]
;;       (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (->class integer-class) "valueOf" (str "(I)" (->type-signature integer-class)))

;;       [::&type/primitive "boolean"]
;;       (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (->class boolean-class) "valueOf" (str "(Z)" (->type-signature boolean-class)))

;;       [::&type/Data ?oclass]
;;       nil)))


(defn ^:private compile-jvm-invokestatic [compile *type* ?class ?method ?classes ?args]
  (exec [*writer* &util/get-writer
         :let [method-sig (str "(" (reduce str "" (map ->type-signature ?classes)) ")" (->java-sig *type*))]
         _ (map-m (fn [[class-name arg]]
                    (exec [ret (compile arg)
                           :let [_ (prepare-arg! *writer* class-name)]]
                      (return ret)))
                  (map vector ?classes ?args))
         :let [_ (do (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (->class ?class) ?method method-sig)
                   ;; (prepare-return! *writer* *type*)
                   )]]
    (return nil)))

(defn ^:private compile-jvm-invokevirtual [compile *type* ?class ?method ?classes ?object ?args]
  (exec [*writer* &util/get-writer
         :let [method-sig (str "(" (reduce str "" (map ->type-signature ?classes)) ")" (->java-sig *type*))]
         _ (compile ?object)
         :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST (->class ?class))]
         _ (map-m (fn [[class-name arg]]
                    (exec [ret (compile arg)
                           :let [_ (prepare-arg! *writer* class-name)]]
                      (return ret)))
                  (map vector ?classes ?args))
         :let [_ (do (.visitMethodInsn *writer* Opcodes/INVOKEVIRTUAL (->class ?class) ?method method-sig)
                   ;; (prepare-return! *writer* *type*)
                   )]]
    (return nil)))

(defn ^:private compile-jvm-new [compile *type* ?class ?classes ?args]
  (exec [*writer* &util/get-writer
         :let [init-sig (str "(" (reduce str "" (map ->type-signature ?classes)) ")V")
               class* (->class ?class)
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

(defn ^:private compile-jvm-new-array [compile *type* ?class ?length]
  (exec [*writer* &util/get-writer
         :let [_ (doto *writer*
                   (.visitLdcInsn (int ?length))
                   (.visitTypeInsn Opcodes/ANEWARRAY (->class ?class)))]]
    (return nil)))

(defn ^:private compile-jvm-aastore [compile *type* ?array ?idx ?elem]
  (exec [*writer* &util/get-writer
         _ (compile ?array)
         :let [_ (doto *writer*
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn (int ?idx)))]
         _ (compile ?elem)
         :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
    (return nil)))

(defn ^:private compile-jvm-aaload [compile *type* ?array ?idx]
  (exec [*writer* &util/get-writer
         _ (compile ?array)
         :let [_ (doto *writer*
                   (.visitLdcInsn (int ?idx))
                   (.visitInsn Opcodes/AALOAD))]]
    (return nil)))

(defn ^:private compile-jvm-getstatic [compile *type* ?class ?field]
  (exec [*writer* &util/get-writer
         :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class ?class) ?field (->java-sig *type*))]]
    (return nil)))

(defn ^:private compile-jvm-getfield [compile *type* ?class ?field ?object]
  (exec [*writer* &util/get-writer
         _ (compile ?object)
         :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST (->class ?class))]
         :let [_ (.visitFieldInsn *writer* Opcodes/GETFIELD (->class ?class) ?field (->java-sig *type*))]]
    (return nil)))

(defn ^:private compile-jvm-class [compile *type* ?package ?name ?super-class ?fields ?methods]
  (let [parent-dir (->package ?package)
        full-name (str parent-dir "/" ?name)
        super-class* (->class ?super-class)
        =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                 (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                         full-name nil super-class* nil))
        _ (do (doseq [[field props] ?fields]
                (doto (.visitField =class Opcodes/ACC_PUBLIC field (->type-signature (:type props)) nil nil)
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
    (save-class! full-name (.toByteArray =class))))

(defn ^:private compile-jvm-interface [compile *type* ?package ?name ?fields ?methods]
  (let [parent-dir (->package ?package)
        full-name (str parent-dir "/" ?name)
        =interface (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                     (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_INTERFACE)
                             full-name nil "java/lang/Object" nil))
        _ (do (doseq [[?method ?props] ?methods
                      :let [[?args ?return] (:type ?props)
                            signature (str "(" (reduce str "" (map ->type-signature ?args)) ")" (->type-signature ?return))]]
                (.visitMethod =interface (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT) ?method signature nil nil))
            (.visitEnd =interface)
            (.mkdirs (java.io.File. (str "output/" parent-dir))))]
    (save-class! full-name (.toByteArray =interface))))

(do-template [<name> <opcode> <wrapper-class> <value-method> <value-method-sig> <wrapper-method> <wrapper-method-sig>]
  (defn <name> [compile *type* ?x ?y]
    (exec [:let [+wrapper-class+ (->class <wrapper-class>)]
           *writer* &util/get-writer
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
                     (.visitMethodInsn Opcodes/INVOKESTATIC +wrapper-class+ <wrapper-method> (str <wrapper-method-sig> (->type-signature <wrapper-class>))))]]
      (return nil)))

  ^:private compile-jvm-iadd Opcodes/IADD "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  ^:private compile-jvm-isub Opcodes/ISUB "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  ^:private compile-jvm-imul Opcodes/IMUL "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  ^:private compile-jvm-idiv Opcodes/IDIV "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  ^:private compile-jvm-irem Opcodes/IREM "java.lang.Integer" "intValue"    "()I" "valueOf" "(I)"
  
  ^:private compile-jvm-ladd Opcodes/LADD "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"
  ^:private compile-jvm-lsub Opcodes/LSUB "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"
  ^:private compile-jvm-lmul Opcodes/LMUL "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"
  ^:private compile-jvm-ldiv Opcodes/LDIV "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"
  ^:private compile-jvm-lrem Opcodes/LREM "java.lang.Long"    "longValue"   "()J" "valueOf" "(J)"

  ^:private compile-jvm-fadd Opcodes/FADD "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  ^:private compile-jvm-fsub Opcodes/FSUB "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  ^:private compile-jvm-fmul Opcodes/FMUL "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  ^:private compile-jvm-fdiv Opcodes/FDIV "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  ^:private compile-jvm-frem Opcodes/FREM "java.lang.Float"   "floatValue"  "()F" "valueOf" "(F)"
  
  ^:private compile-jvm-dadd Opcodes/DADD "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  ^:private compile-jvm-dsub Opcodes/DSUB "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  ^:private compile-jvm-dmul Opcodes/DMUL "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  ^:private compile-jvm-ddiv Opcodes/DDIV "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  ^:private compile-jvm-drem Opcodes/DREM "java.lang.Double"  "doubleValue" "()D" "valueOf" "(D)"
  )
