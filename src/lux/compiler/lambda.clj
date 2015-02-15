(ns lux.compiler.lambda)

;; [Utils]
(def ^:private clo-field-sig (->type-signature "java.lang.Object"))
(def ^:private lambda-return-sig (->type-signature "java.lang.Object"))
(def ^:private <init>-return "V")
(def ^:private counter-sig "I")
(def ^:private +datum-sig+ (->type-signature "java.lang.Object"))

(defn ^:private lambda-impl-signature [args]
  (str (reduce str "("  (repeat (count args) clo-field-sig)) ")" lambda-return-sig))

(defn ^:private lambda-<init>-signature [closed-over args]
  (let [num-args (count args)]
    (str "(" (reduce str "" (repeat (count closed-over) clo-field-sig))
         (if (> num-args 1)
           (reduce str counter-sig (repeat (dec num-args) clo-field-sig)))
         ")"
         <init>-return)))

(defn ^:private add-lambda-<init> [class class-name closed-over args init-signature]
  (let [num-args (count args)
        num-mappings (count closed-over)]
    (doto (.visitMethod class Opcodes/ACC_PUBLIC "<init>" init-signature nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
      (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
            (.visitVarInsn Opcodes/ALOAD ?captured-id)
            (.visitFieldInsn Opcodes/PUTFIELD class-name captured-name clo-field-sig))
          (->> (let [captured-name (str +closure-prefix+ ?captured-id)])
               (match (:form ?captured)
                 [::&analyser/captured ?closure-id ?captured-id ?source])
               (doseq [[?name ?captured] closed-over])))
      (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
            (.visitInsn Opcodes/ICONST_0)
            (.visitFieldInsn Opcodes/PUTFIELD class-name "_counter" counter-sig)
            (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitVarInsn Opcodes/ALOAD (+ clo_idx offset))
                  (.visitFieldInsn Opcodes/PUTFIELD class-name field-name clo-field-sig))
                (->> (let [field-name (str +partial-prefix+ clo_idx)]
                       (doto (.visitField class (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) field-name clo-field-sig nil nil)
                         (.visitEnd)))
                     (dotimes [clo_idx (dec num-args)])
                     (let [offset (+ 2 num-mappings)]))))
          (->> (when (> num-args 1))))
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))))

(do-template [<name> <prefix>]
  (defn <name> [writer class-name vars]
    (dotimes [idx (count vars)]
      (doto writer
        (.visitVarInsn Opcodes/ALOAD 0)
        (.visitFieldInsn Opcodes/GETFIELD class-name (str <prefix> idx) clo-field-sig))))

  ^:private add-closure-vars +closure-prefix+
  ^:private add-partial-vars +partial-prefix+
  )

(defn ^:private add-nulls [writer amount]
  (dotimes [_ amount]
    (.visitInsn writer Opcodes/ACONST_NULL)))

(defn ^:private add-lambda-apply [class class-name closed-over args impl-signature init-signature]
  (let [num-args (count args)
        num-captured (dec num-args)
        default-label (new Label)
        branch-labels (for [_ (range num-captured)]
                        (new Label))]
    (doto (.visitMethod class Opcodes/ACC_PUBLIC "apply" +apply-signature+ nil nil)
      (.visitCode)
      (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
            (.visitFieldInsn Opcodes/GETFIELD class-name "_counter" counter-sig)
            (.visitTableSwitchInsn 0 (dec num-captured) default-label (into-array Label branch-labels))
            (-> (doto (.visitLabel branch-label)
                  (.visitTypeInsn Opcodes/NEW class-name)
                  (.visitInsn Opcodes/DUP)
                  (add-closure-vars class-name closed-over)
                  (.visitLdcInsn (int current-captured))
                  (add-partial-vars class-name (take current-captured args))
                  (.visitVarInsn Opcodes/ALOAD 1)
                  (add-nulls (- (dec num-captured) current-captured))
                  (.visitMethodInsn Opcodes/INVOKESPECIAL class-name "<init>" init-signature)
                  (.visitInsn Opcodes/ARETURN))
                (->> (doseq [[branch-label current-captured] (map vector branch-labels (range (count branch-labels)))])))
            (.visitLabel default-label))
          (->> (when (> num-args 1))))
      (.visitVarInsn Opcodes/ALOAD 0)
      (add-partial-vars class-name (butlast args))
      (.visitVarInsn Opcodes/ALOAD 1)
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL class-name "impl" impl-signature)
      (.visitInsn Opcodes/ARETURN)
      (.visitMaxs 0 0)
      (.visitEnd))))

(defn ^:private add-lambda-impl [class compile impl-signature impl-body]
  (&util/with-writer (doto (.visitMethod class Opcodes/ACC_PUBLIC "impl" impl-signature nil nil)
                       (.visitCode))
    (exec [;; :let [_ (prn 'add-lambda-impl/_0)]
           *writer* &util/get-writer
           ;; :let [_ (prn 'add-lambda-impl/_1 *writer*)]
           ret (compile impl-body)
           ;; :let [_ (prn 'add-lambda-impl/_2 ret)]
           :let [_ (doto *writer*
                     (.visitInsn Opcodes/ARETURN)
                     (.visitMaxs 0 0)
                     (.visitEnd))]
           ;; :let [_ (prn 'add-lambda-impl/_3)]
           ]
      (return ret))))

(defn ^:private instance-closure [compile lambda-class closed-over args init-signature]
  (exec [*writer* &util/get-writer
         :let [_ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW lambda-class)
                   (.visitInsn Opcodes/DUP))]
         _ (->> closed-over
                (sort #(< (-> %1 second :form (nth 2))
                          (-> %2 second :form (nth 2))))
                (map-m (fn [[?name ?captured]]
                         (match (:form ?captured)
                           [::&analyser/captured ?closure-id ?captured-id ?source]
                           (compile ?source)))))
         :let [num-args (count args)
               _ (do (when (> num-args 1)
                       (.visitInsn *writer* Opcodes/ICONST_0)
                       (add-nulls *writer* (dec num-args)))
                   (.visitMethodInsn *writer* Opcodes/INVOKESPECIAL lambda-class "<init>" init-signature))]]
    (return nil)))

(defn ^:private add-lambda-<clinit> [class class-name args <init>-sig]
  (let [num-args (count args)]
    (doto (.visitMethod class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
      (.visitCode)
      (.visitTypeInsn Opcodes/NEW class-name)
      (.visitInsn Opcodes/DUP)
      (-> (doto (.visitInsn *writer* Opcodes/ICONST_0)
            (add-nulls (dec num-args)))
          (->> (when (> num-args 1))))
      (.visitMethodInsn Opcodes/INVOKESPECIAL class-name "<init>" <init>-sig)
      (.visitFieldInsn Opcodes/PUTSTATIC class-name "_datum" +datum-sig+)
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))))

;; [Resources]
(defn compile-lambda [compile *type* ?scope ?closure ?args ?body with-datum? instance?]
  (exec [:let [lambda-class (&host/location ?scope)
               impl-signature (lambda-impl-signature ?args)
               <init>-sig (lambda-<init>-signature ?closure ?args)
               =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                        (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                                lambda-class nil "java/lang/Object" (into-array [(->class +function-class+)]))
                        (-> (doto (.visitField (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) captured-name clo-field-sig nil nil)
                              (.visitEnd))
                            (->> (let [captured-name (str +closure-prefix+ ?captured-id)])
                                 (match (:form ?captured)
                                   [::&analyser/captured ?closure-id ?captured-id ?source])
                                 (doseq [[?name ?captured] ?closure])))
                        (-> (doto (.visitField (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) "_counter" counter-sig nil nil)
                              (.visitEnd))
                            (->> (when (> (count ?args) 1))))
                        (-> (doto (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "_datum" +datum-sig+ nil nil)
                              (add-lambda-<clinit> lambda-class ?args <init>-sig))
                            (when with-datum?))
                        (add-lambda-apply lambda-class ?closure ?args impl-signature <init>-sig)
                        (add-lambda-<init> lambda-class ?closure ?args <init>-sig)
                        )]
         _ (add-lambda-impl =class compile impl-signature ?body)
         :let [_ (.visitEnd =class)]
         _ (save-class! lambda-class (.toByteArray =class))]
    (if instance?
      (instance-closure compile lambda-class ?closure ?args <init>-sig)
      (return nil))))
