(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m exhaust-m try-m try-all-m map-m reduce-m
                                         do-all-m
                                         apply-m within
                                         normalize-ident]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser])
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

(def +prefix+ "lux")

;; [Utils/General]
(defn ^:private write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(defn ^:private write-class [name data]
  (write-file (str "output/" name ".class") data))

(defn load-class! [loader name]
  (.loadClass loader name))

(def ^:private +variant-class+ (str +prefix+ ".Variant"))
(def ^:private +tuple-class+ (str +prefix+ ".Tuple"))

(defn ^:private unwrap-ident [ident]
  (match ident
    [::&parser/ident ?label]
    ?label))

(def ^:private get-writer
  (fn [state]
    (return* state (::writer state))))

(defn ^:private ->class [class]
  (string/replace class #"\." "/"))

(def ^:private ->package ->class)

(defn ^:private ->type-signature [class]
  (case class
    "Void" "V"
    "boolean" "Z"
    "byte" "B"
    "short" "S"
    "int" "I"
    "long" "J"
    "float" "F"
    "double" "D"
    "char" "C"
    ;; else
    (let [class* (->class class)]
      (if (.startsWith class* "[")
        class*
        (str "L" class* ";")))
    ))

(defn ^:private ->java-sig [type]
  (match type
    ::&type/nothing
    "V"
    
    ::&type/any
    (->java-sig [::&type/object "java.lang.Object" []])

    [::&type/primitive "boolean"]
    "Z"

    [::&type/primitive "int"]
    "I"

    [::&type/primitive "char"]
    "C"

    [::&type/object ?name []]
    (->type-signature ?name)

    [::&type/array [::&type/object ?name _]]
    (str "[" (->type-signature ?name))

    [::&type/variant ?tag ?value]
    (->type-signature +variant-class+)

    [::&type/function ?args ?return]
    (->java-sig [::&type/object (str +prefix+ "/Function") []])))

(defn ^:private method->sig [method]
  (match method
    [::&type/function ?args ?return]
    (str "(" (apply str (map ->java-sig ?args)) ")"
         (if (= ::&type/nothing ?return)
           "V"
           (->java-sig ?return)))))

;; [Utils/Compilers]
(defn ^:private compile-literal [compile *type* ?literal]
  (exec [*writer* get-writer
         :let [_ (cond (instance? java.lang.Integer ?literal)
                       (doto *writer*
                         (.visitTypeInsn Opcodes/NEW (->class "java.lang.Integer"))
                         (.visitInsn Opcodes/DUP)
                         (.visitLdcInsn ?literal)
                         (.visitMethodInsn Opcodes/INVOKESPECIAL (->class "java.lang.Integer") "<init>" "(I)V"))

                       (instance? java.lang.Float ?literal)
                       (doto *writer*
                         (.visitTypeInsn Opcodes/NEW (->class "java.lang.Float"))
                         (.visitInsn Opcodes/DUP)
                         (.visitLdcInsn ?literal)
                         (.visitMethodInsn Opcodes/INVOKESPECIAL (->class "java.lang.Float") "<init>" "(F)V"))

                       (instance? java.lang.Character ?literal)
                       (doto *writer*
                         (.visitTypeInsn Opcodes/NEW (->class "java.lang.Character"))
                         (.visitInsn Opcodes/DUP)
                         (.visitLdcInsn ?literal)
                         (.visitMethodInsn Opcodes/INVOKESPECIAL (->class "java.lang.Character") "<init>" "(C)V"))

                       (instance? java.lang.Boolean ?literal)
                       (if ?literal
                         (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class "java.lang.Boolean") "TRUE" (->type-signature "java.lang.Boolean"))
                         (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class "java.lang.Boolean") "FALSE" (->type-signature "java.lang.Boolean")))

                       (string? ?literal)
                       (.visitLdcInsn *writer* ?literal)

                       :else
                       (assert false (str "[Unknown literal type] " ?literal " : " (class ?literal))))]]
    (return nil)))

(defn ^:private compile-tuple [compile *type* ?elems]
  (exec [*writer* get-writer
         :let [_ (let [num-elems (count ?elems)]
                   (let [tuple-class (str (str +prefix+ "/Tuple") num-elems)]
                     (doto *writer*
                       (.visitTypeInsn Opcodes/NEW tuple-class)
                       (.visitInsn Opcodes/DUP)
                       (.visitMethodInsn Opcodes/INVOKESPECIAL tuple-class "<init>" "()V"))
                     (dotimes [idx num-elems]
                       (.visitInsn *writer* Opcodes/DUP)
                       (compile (nth ?elems idx))
                       (.visitFieldInsn *writer* Opcodes/PUTFIELD tuple-class (str "_" (inc idx)) "Ljava/lang/Object;"))))]]
    (return nil)))

(defn ^:private compile-local [compile *type* ?env ?idx]
  (exec [*writer* get-writer
         :let [_ (doto *writer*
                   (.visitVarInsn Opcodes/ALOAD (int ?idx)))]]
    (return nil)))

(defn ^:private compile-captured [compile *type* ?scope ?captured-id ?source]
  (exec [*writer* get-writer
         :let [_ (doto *writer*
                   (.visitVarInsn Opcodes/ALOAD 0)
                   (.visitFieldInsn Opcodes/GETFIELD
                                    (normalize-ident ?scope)
                                    (str "__" ?captured-id)
                                    "Ljava/lang/Object;"))]]
    (return nil)))

(defn ^:private compile-global [compile *type* ?owner-class ?name]
  (exec [*writer* get-writer
         :let [_ (doto *writer*
                   (.visitFieldInsn Opcodes/GETSTATIC (->class (str ?owner-class "$" (normalize-ident ?name))) "_datum" "Ljava/lang/Object;"))]]
    (return nil)))

(defn ^:private compile-global-fn [compile *type* ?owner-class ?name]
  (exec [*writer* get-writer
         :let [_ (let [fn-class (str ?owner-class "$" (normalize-ident ?name))]
                   (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class fn-class) "_datum" (->type-signature fn-class)))]]
    (return nil)))

(defn ^:private compile-call [compile *type* ?fn ?args]
  (exec [*writer* get-writer
         :let [_ (do (compile ?fn)
                   (let [apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"]
                     (doseq [arg ?args]
                       (compile arg)
                       (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (str +prefix+ "/Function") "apply" apply-signature))))]]
    (return nil)))

(defn ^:private compile-static-call [compile *type* ?needs-num ?fn ?args]
  (exec [*writer* get-writer
         :let [_ (match (:form ?fn)
                   [::&analyser/global-fn ?owner-class ?fn-name]
                   (let [arg-sig (->type-signature "java.lang.Object")
                         call-class (str (->class ?owner-class) "$" (normalize-ident ?fn-name))
                         provides-num (count ?args)]
                     (if (>= provides-num ?needs-num)
                       (let [apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"
                             impl-sig (str "(" (reduce str "" (repeat ?needs-num arg-sig)) ")" arg-sig)]
                         (doto *writer*
                           (-> (do (compile arg))
                               (->> (doseq [arg (take ?needs-num ?args)])))
                           (.visitMethodInsn Opcodes/INVOKESTATIC call-class "impl" impl-sig)
                           (-> (doto (do (compile arg))
                                 (.visitMethodInsn Opcodes/INVOKEINTERFACE (str +prefix+ "/Function") "apply" apply-signature))
                               (->> (doseq [arg (drop ?needs-num ?args)])))))
                       (let [counter-sig "I"
                             init-signature (str "(" (apply str counter-sig (repeat (dec ?needs-num) arg-sig)) ")" "V")]
                         (doto *writer*
                           (.visitTypeInsn Opcodes/NEW call-class)
                           (.visitInsn Opcodes/DUP)
                           (.visitLdcInsn (int provides-num))
                           (-> (do (compile arg))
                               (->> (doseq [arg ?args])))
                           (-> (.visitInsn Opcodes/ACONST_NULL)
                               (->> (dotimes [_ (dec (- ?needs-num provides-num))])))
                           (.visitMethodInsn Opcodes/INVOKESPECIAL call-class "<init>" init-signature)))
                       ))
                   )]]
    (return nil)))

(defn ^:private compile-jvm-getstatic [compile *type* ?owner ?field]
  (exec [*writer* get-writer
         :let [_ (doto *writer*
                   (.visitFieldInsn Opcodes/GETSTATIC (->class ?owner) ?field (->java-sig *type*)))]]
    (return nil)))

(defn prepare-arg! [*writer* class-name]
  (condp = class-name
    "boolean" (let [wrapper-class (->class "java.lang.Boolean")]
                (doto *writer*
                  (.visitTypeInsn Opcodes/CHECKCAST wrapper-class)
                  (.visitMethodInsn Opcodes/INVOKEVIRTUAL wrapper-class "booleanValue" "()Z")))
    "byte" (let [wrapper-class (->class "java.lang.Byte")]
             (doto *writer*
               (.visitTypeInsn Opcodes/CHECKCAST wrapper-class)
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL wrapper-class "byteValue" "()B")))
    "short" (let [wrapper-class (->class "java.lang.Short")]
              (doto *writer*
                (.visitTypeInsn Opcodes/CHECKCAST wrapper-class)
                (.visitMethodInsn Opcodes/INVOKEVIRTUAL wrapper-class "shortValue" "()S")))
    "int" (let [wrapper-class (->class "java.lang.Integer")]
            (doto *writer*
              (.visitTypeInsn Opcodes/CHECKCAST wrapper-class)
              (.visitMethodInsn Opcodes/INVOKEVIRTUAL wrapper-class "intValue" "()I")))
    "long" (let [wrapper-class (->class "java.lang.Long")]
             (doto *writer*
               (.visitTypeInsn Opcodes/CHECKCAST wrapper-class)
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL wrapper-class "longValue" "()J")))
    "float" (let [wrapper-class (->class "java.lang.Float")]
              (doto *writer*
                (.visitTypeInsn Opcodes/CHECKCAST wrapper-class)
                (.visitMethodInsn Opcodes/INVOKEVIRTUAL wrapper-class "floatValue" "()F")))
    "double" (let [wrapper-class (->class "java.lang.Double")]
               (doto *writer*
                 (.visitTypeInsn Opcodes/CHECKCAST wrapper-class)
                 (.visitMethodInsn Opcodes/INVOKEVIRTUAL wrapper-class "doubleValue" "()D")))
    "char" (let [wrapper-class (->class "java.lang.Character")]
             (doto *writer*
               (.visitTypeInsn Opcodes/CHECKCAST wrapper-class)
               (.visitMethodInsn Opcodes/INVOKEVIRTUAL wrapper-class "charValue" "()C")))
    ;; else
    (.visitTypeInsn *writer* Opcodes/CHECKCAST (->class class-name))))

(let [boolean-class "java.lang.Boolean"
      integer-class "java.lang.Integer"
      char-class "java.lang.Character"]
  (defn prepare-return! [*writer* *type*]
    (match *type*
      ::&type/nothing
      (.visitInsn *writer* Opcodes/ACONST_NULL)

      [::&type/primitive "char"]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (->class char-class) "valueOf" (str "(C)" (->type-signature char-class)))

      [::&type/primitive "int"]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (->class integer-class) "valueOf" (str "(I)" (->type-signature integer-class)))

      [::&type/primitive "boolean"]
      (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (->class boolean-class) "valueOf" (str "(Z)" (->type-signature boolean-class)))
      
      [::&type/object ?oclass _]
      nil)))

(defn ^:private compile-jvm-invokevirtual [compile *type* ?class ?method ?classes ?object ?args]
  (exec [*writer* get-writer
         :let [_ (let [method-sig (str "(" (reduce str "" (map ->type-signature ?classes)) ")" (->java-sig *type*))]
                   (compile ?object)
                   (.visitTypeInsn *writer* Opcodes/CHECKCAST (->class ?class))
                   (doseq [[class-name arg] (map vector ?classes ?args)]
                     (do (compile arg)
                       (prepare-arg! *writer* class-name)))
                   (.visitMethodInsn *writer* Opcodes/INVOKEVIRTUAL (->class ?class) ?method method-sig)
                   (prepare-return! *writer* *type*)
                   )]]
    (return nil)))

(defn ^:private compile-jvm-new [compile *type* ?class ?classes ?args]
  (exec [*writer* get-writer
         :let [_ (let [init-sig (str "(" (reduce str "" (map ->type-signature ?classes)) ")V")
                       class* (->class ?class)]
                   (doto *writer*
                     (.visitTypeInsn Opcodes/NEW class*)
                     (.visitInsn Opcodes/DUP))
                   (doseq [[class-name arg] (map vector ?classes ?args)]
                     (do (compile arg)
                       (prepare-arg! *writer* class-name)))
                   (doto *writer*
                     (.visitMethodInsn Opcodes/INVOKESPECIAL class* "<init>" init-sig))
                   )]]
    (return nil)))

(defn ^:private compile-jvm-new-array [compile *type* ?class ?length]
  (exec [*writer* get-writer
         :let [_ (doto *writer*
                   (.visitLdcInsn (int ?length))
                   (.visitTypeInsn Opcodes/ANEWARRAY (->class ?class)))]]
    (return nil)))

(defn ^:private compile-jvm-aastore [compile *type* ?array ?idx ?elem]
  (exec [*writer* get-writer
         :let [_ (doto *writer*
                   (do (compile ?array))
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn (int ?idx))
                   (do (compile ?elem))
                   (.visitInsn Opcodes/AASTORE))]]
    (return nil)))

(defn ^:private compile-jvm-aaload [compile *type* ?array ?idx]
  (exec [*writer* get-writer
         :let [_ (doto *writer*
                   (do (compile ?array))
                   (.visitLdcInsn (int ?idx))
                   (.visitInsn Opcodes/AALOAD))]]
    (return nil)))

(let [+bool-class+ (->class "java.lang.Boolean")]
  (defn ^:private compile-if [compile *type* ?test ?then ?else]
    (exec [*writer* get-writer
           :let [_ (let [else-label (new Label)
                         end-label (new Label)]
                     (compile ?test)
                     (doto *writer*
                       (.visitTypeInsn Opcodes/CHECKCAST +bool-class+)
                       (.visitMethodInsn Opcodes/INVOKEVIRTUAL +bool-class+ "booleanValue" "()Z")
                       (.visitJumpInsn Opcodes/IFEQ else-label))
                     (compile ?then)
                     (doto *writer*
                       (.visitJumpInsn Opcodes/GOTO end-label)
                       (.visitLabel else-label))
                     (compile ?else)
                     (.visitLabel *writer* end-label))]]
      (return nil))))

(defn ^:private compile-do [compile *type* ?exprs]
  (exec [*writer* get-writer
         :let [_ (do (doseq [expr (butlast ?exprs)]
                       (compile expr)
                       (.visitInsn *writer* Opcodes/POP))
                   (compile (last ?exprs)))]]
    (return nil)))

(let [+tag-sig+ (->type-signature "java.lang.String")
      variant-class* (->class +variant-class+)
      tuple-class* (->class +tuple-class+)
      oclass (->class "java.lang.Object")
      +variant-field-sig+ (->type-signature "java.lang.Object")
      equals-sig (str "(" (->type-signature "java.lang.Object") ")Z")]
  (defn compile-decision-tree [writer mappings default-label decision-tree]
    (match decision-tree
      [::test-char ?pairs]
      (do (doseq [[?token $body] ?pairs
                  :let [$else (new Label)]]
            (doto writer
              ;; object
              (.visitInsn Opcodes/DUP) ;; object, object
              (.visitTypeInsn Opcodes/NEW (->class "java.lang.Character"))
              (.visitInsn Opcodes/DUP)
              (.visitLdcInsn ?token) ;; object, object, text
              (.visitMethodInsn Opcodes/INVOKESPECIAL (->class "java.lang.Character") "<init>" "(C)V")
              (.visitMethodInsn Opcodes/INVOKEVIRTUAL oclass "equals" equals-sig) ;; object, B
              (.visitJumpInsn Opcodes/IFEQ $else) ;; object
              (.visitInsn Opcodes/POP)
              (.visitJumpInsn Opcodes/GOTO (get mappings $body))
              (.visitLabel $else)))
        (doto writer
          (.visitInsn Opcodes/POP)
          (.visitJumpInsn Opcodes/GOTO default-label)))
      
      [::test-text ?pairs]
      (do (doseq [[?text $body] ?pairs
                  :let [$else (new Label)]]
            (doto writer
              ;; object
              (.visitInsn Opcodes/DUP) ;; object, object
              (.visitLdcInsn ?text) ;; object, object, text
              (.visitMethodInsn Opcodes/INVOKEVIRTUAL oclass "equals" equals-sig) ;; object, B
              (.visitJumpInsn Opcodes/IFEQ $else) ;; object
              (.visitInsn Opcodes/POP)
              (.visitJumpInsn Opcodes/GOTO (get mappings $body))
              (.visitLabel $else)))
        (doto writer
          (.visitInsn Opcodes/POP)
          (.visitJumpInsn Opcodes/GOTO default-label)))

      [::default [::&analyser/local _ ?idx] $body]
      (doto writer
        (.visitVarInsn Opcodes/ASTORE ?idx)
        (.visitJumpInsn Opcodes/GOTO (get mappings $body)))
      
      [::store [::&analyser/local _ ?idx] $body]
      (doto writer
        (.visitVarInsn Opcodes/ASTORE ?idx)
        (.visitJumpInsn Opcodes/GOTO (get mappings $body)))
      
      [::test-tuple ?branches ?cases]
      (let [[_ ?subcases] (first ?cases)
            arity (-> ?subcases first (nth 2) count)
            tuple-class** (str tuple-class* arity)]
        (doto writer
          ;; object
          (.visitTypeInsn Opcodes/CHECKCAST tuple-class**) ;; tuple
          (do (doseq [subcase ?subcases
                      :let [next-subcase (new Label)]]
                (match subcase
                  [::subcase $body ?subseq]
                  (do (doseq [[?subpart ?subidx] (map vector ?subseq (range (count ?subseq)))
                              :let [sub-next-elem (new Label)]]
                        (doto writer
                          (.visitInsn Opcodes/DUP) ;; tuple, tuple
                          (.visitFieldInsn Opcodes/GETFIELD tuple-class** (str "_" (inc ?subidx)) +variant-field-sig+) ;; tuple, object
                          (compile-decision-tree (assoc mappings $body sub-next-elem) next-subcase ?subpart) ;; tuple
                          (.visitLabel sub-next-elem)))
                    (doto writer
                      (.visitInsn Opcodes/POP)
                      (.visitJumpInsn Opcodes/GOTO (get mappings $body))
                      (.visitLabel next-subcase)))
                  )))
          (.visitInsn Opcodes/POP) ;; ->
          (.visitJumpInsn Opcodes/GOTO default-label)))

      [::test-adt ?branches ?cases]
      (doto writer
        ;; object
        (.visitTypeInsn Opcodes/CHECKCAST variant-class*) ;; variant
        (.visitInsn Opcodes/DUP) ;; variant, variant
        (.visitFieldInsn Opcodes/GETFIELD variant-class* "tag" +tag-sig+) ;; variant, tag
        (-> (doto (.visitInsn Opcodes/DUP) ;; variant, tag, tag
              (.visitLdcInsn ?tag) ;; variant, tag, tag, text
              (.visitMethodInsn Opcodes/INVOKEVIRTUAL oclass "equals" equals-sig) ;; variant, tag, B
              (.visitJumpInsn Opcodes/IFEQ tag-else-label) ;; variant, tag
              (.visitInsn Opcodes/POP) ;; variant
              (do (let [arity (-> ?subcases first (nth 2) count)
                        variant-class** (str variant-class* arity)]
                    (.visitTypeInsn writer Opcodes/CHECKCAST variant-class**) ;; variantN
                    (doseq [subcase ?subcases
                            :let [next-subcase (new Label)]]
                      (match subcase
                        [::subcase $body ?subseq]
                        (do (doseq [[?subpart ?subidx] (map vector ?subseq (range (count ?subseq)))
                                    :let [sub-next-elem (new Label)]]
                              (doto writer
                                (.visitInsn Opcodes/DUP) ;; variant, variant
                                (.visitFieldInsn Opcodes/GETFIELD variant-class** (str "_" (inc ?subidx)) +variant-field-sig+) ;; variant, object
                                (compile-decision-tree (assoc mappings $body sub-next-elem) next-subcase ?subpart) ;; variant
                                (.visitLabel sub-next-elem)))
                          (doto writer
                            (.visitInsn Opcodes/POP)
                            (.visitJumpInsn Opcodes/GOTO (get mappings $body))
                            (.visitLabel next-subcase)))
                        ))
                    ))
              (.visitInsn Opcodes/POP) ;; ->
              (.visitJumpInsn Opcodes/GOTO default-label)
              ;; variant, tag ->
              (.visitLabel tag-else-label))
            (->> (doseq [[?tag ?subcases] ?cases
                         :let [tag-else-label (new Label)]])))
        ;; variant, tag ->
        (.visitInsn Opcodes/POP) ;; variant ->
        (.visitInsn Opcodes/POP) ;; ->
        (.visitJumpInsn Opcodes/GOTO default-label)))
    ))

(defn sequence-parts [branches parts]
  (if (empty? parts)
    '(())
    (let [[head & tail] parts
          expanded (case (:type head)
                     ::&analyser/defaults
                     (for [[?local ?supports] (:stores head)
                           ?body (set/intersection branches ?supports)]
                       [[::store ?local ?body] #{?body}])

                     ::&analyser/char-tests
                     (concat (list [[::test-char (for [[?token ?supports] (:patterns head)
                                                       ?body (set/intersection branches ?supports)]
                                                   [?token ?body])]
                                    branches])
                             (for [[_ ?local ?body] (:defaults head)
                                   :when (contains? branches ?body)]
                               [[::store ?local ?body] #{?body}]))

                     ::&analyser/text-tests
                     (concat (list [[::test-text (for [[?token ?supports] (:patterns head)
                                                       ?body (set/intersection branches ?supports)]
                                                   [?token ?body])]
                                    branches])
                             (for [[_ ?local ?body] (:defaults head)
                                   :when (contains? branches ?body)]
                               [[::store ?local ?body] #{?body}]))

                     ::&analyser/tuple*
                     (concat (let [patterns (into {} (for [[?tag ?struct] (:patterns head)
                                                           :let [?parts (:parts ?struct)
                                                                 num-parts (count ?parts)
                                                                 ?supports (:branches ?struct)
                                                                 subcases (for [?body (set/intersection branches ?supports)
                                                                                subseq (sequence-parts #{?body} ?parts)
                                                                                :when (= num-parts (count subseq))]
                                                                            [::subcase ?body subseq])]
                                                           :when (not (empty? subcases))]
                                                       [?tag subcases]))]
                               (if (empty? patterns)
                                 '()
                                 (list [[::test-tuple branches patterns]
                                        branches])))
                             (if-let [[_ ?local ?body] (:default head)]
                               (for [?body (set/intersection branches #{?body})]
                                 [[::default ?local ?body] #{?body}])
                               '()))

                     ::&analyser/adt*
                     (concat (let [patterns (into {} (for [[?tag ?struct] (:patterns head)
                                                           :let [?parts (:parts ?struct)
                                                                 num-parts (count ?parts)
                                                                 ?supports (:branches ?struct)
                                                                 subcases (for [?body (set/intersection branches ?supports)
                                                                                subseq (sequence-parts #{?body} ?parts)
                                                                                :when (= num-parts (count subseq))]
                                                                            [::subcase ?body subseq])]
                                                           :when (not (empty? subcases))]
                                                       [?tag subcases]))]
                               (if (empty? patterns)
                                 '()
                                 (list [[::test-adt branches patterns]
                                        branches])))
                             (if-let [[_ ?local ?body] (:default head)]
                               (for [?body (set/intersection branches #{?body})]
                                 [[::default ?local ?body] #{?body}])
                               '()))
                     )]
      (for [[step branches*] expanded
            tail* (sequence-parts branches* tail)]
        (cons step tail*)))))

(def !case-vars (atom -1))

(let [oclass (->class "java.lang.Object")
      equals-sig (str "(" (->type-signature "java.lang.Object") ")Z")
      ex-class (->class "java.lang.IllegalStateException")]
  (defn ^:private compile-case [compile *type* ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree]
    (exec [*writer* get-writer
           :let [_ (let [start-label (new Label)
                         end-label (new Label)
                         entries (for [[?branch ?body] ?branch-mappings
                                       :let [label (new Label)]]
                                   [[?branch label]
                                    [label ?body]])
                         mappings* (into {} (map first entries))]
                     (dotimes [idx ?max-registers]
                       (.visitLocalVariable *writer* (str "__" (swap! !case-vars inc) "__") (->java-sig ::&type/any) nil start-label end-label (+ ?base-idx (inc idx))))
                     (compile ?variant)
                     (doto *writer*
                       (.visitInsn Opcodes/DUP)
                       (.visitLabel start-label))
                     (let [default-label (new Label)]
                       (doseq [decision-tree (let [pieces (map first (sequence-parts (:branches ?decision-tree) (list ?decision-tree)))]
                                               (if (or (:default ?decision-tree)
                                                       (not (empty? (:defaults ?decision-tree))))
                                                 (butlast pieces)
                                                 pieces))]
                         (compile-decision-tree *writer* mappings* default-label decision-tree))
                       (.visitLabel *writer* default-label)
                       (if-let [[_ [_ _ ?idx] ?body] (or (:default ?decision-tree)
                                                         (first (:defaults ?decision-tree)))]
                         (doto *writer*
                           (.visitInsn Opcodes/DUP)
                           (.visitVarInsn Opcodes/ASTORE ?idx)
                           (.visitJumpInsn Opcodes/GOTO (get mappings* ?body)))
                         (doto *writer*
                           (.visitInsn Opcodes/POP)
                           (.visitTypeInsn Opcodes/NEW ex-class)
                           (.visitInsn Opcodes/DUP)
                           (.visitMethodInsn Opcodes/INVOKESPECIAL ex-class "<init>" "()V")
                           (.visitInsn Opcodes/ATHROW))))
                     (doseq [[?label ?body] (map second entries)]
                       (.visitLabel *writer* ?label)
                       (.visitInsn *writer* Opcodes/POP)
                       (compile ?body)
                       (.visitJumpInsn *writer* Opcodes/GOTO end-label))
                     (.visitLabel *writer* end-label)
                     )]]
      (return nil))))

(defn ^:private compile-let [compile *type* ?idx ?label ?value ?body]
  (exec [*writer* get-writer
         :let [_ (let [start-label (new Label)
                       end-label (new Label)
                       ?idx (int ?idx)]
                   (.visitLocalVariable *writer* (normalize-ident ?label) (->java-sig (:type ?value)) nil start-label end-label ?idx)
                   (compile ?value)
                   (doto *writer*
                     (.visitVarInsn Opcodes/ASTORE ?idx)
                     (.visitLabel start-label))
                   (compile ?body)
                   (.visitLabel *writer* end-label))]]
    (return nil)))

(defn compile-field [compile writer loader class-name ?name body]
  (let [outer-class (->class class-name)
        datum-sig (->type-signature "java.lang.Object")
        current-class (str outer-class "$" (normalize-ident ?name))]
    (.visitInnerClass writer current-class outer-class nil (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
    (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                   (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                           current-class nil "java/lang/Object" (into-array [(str +prefix+ "/Function")]))
                   (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_datum" datum-sig nil nil)
                       (doto (.visitEnd)))
                   (-> (.visitMethod Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
                       (doto (.visitCode)
                         (compile body)
                         (.visitFieldInsn Opcodes/PUTSTATIC current-class "_datum" datum-sig)
                         (.visitInsn Opcodes/RETURN)
                         (.visitMaxs 0 0)
                         (.visitEnd)))
                   (.visitEnd))
          bytecode (.toByteArray =class)]
      (write-class current-class bytecode)
      (load-class! loader (string/replace current-class #"/" ".")))
    ))

(defn ^:private compile-def [compile *type* ?name ?value]
  (exec [*writer* get-writer
         *class-name* &analyser/module-name
         loader &util/loader
         :let [_ (compile-field compile *writer* loader *class-name* ?name ?value)]]
    (return nil)))

(defn ^:private captured? [form]
  (match form
    [::&analyser/captured ?closure-id ?captured-id ?source]
    true
    _
    false))

(defn ^:private compile-lambda [compile *type* ?scope ?frame ?args ?body]
  (exec [*writer* get-writer
         loader &util/loader
         :let [_ (let [num-args (count ?args)
                       clo-field-sig (->type-signature "java.lang.Object")
                       counter-sig "I"
                       apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"
                       real-signature (str "(" (apply str (repeat num-args clo-field-sig)) ")" "Ljava/lang/Object;")
                       current-class (apply str (interpose "$" (map (comp normalize-ident str) ?scope)))
                       num-captured (dec num-args)
                       init-signature (str "(" (apply str (repeat (->> (:mappings ?frame)
                                                                       (map (comp :form second))
                                                                       (filter captured?)
                                                                       count)
                                                                  clo-field-sig))
                                           (if (not= 0 num-captured)
                                             (apply str counter-sig (repeat num-captured clo-field-sig)))
                                           ")"
                                           "V")
                       =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                                (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                                        current-class nil "java/lang/Object" (into-array [(str +prefix+ "/Function")]))
                                (-> (doto (.visitField (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) captured-name clo-field-sig nil nil)
                                      (.visitEnd))
                                    (->> (let [captured-name (str "__" ?captured-id)])
                                         (match (:form ?captured)
                                           [::&analyser/captured ?closure-id ?captured-id ?source])
                                         (doseq [[?name ?captured] (:mappings ?frame)
                                                 :when (captured? (:form ?captured))])))
                                (-> (doto (.visitField (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) "_counter" counter-sig nil nil)
                                      (.visitEnd))
                                    (->> (when (not= 0 num-captured)))))
                       =init (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<init>" init-signature nil nil)
                               (.visitCode)
                               (.visitVarInsn Opcodes/ALOAD 0)
                               (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
                               (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                                     (.visitVarInsn Opcodes/ALOAD (inc ?captured-id))
                                     (.visitFieldInsn Opcodes/PUTFIELD current-class captured-name clo-field-sig))
                                   (->> (let [captured-name (str "__" ?captured-id)])
                                        (match (:form ?captured)
                                          [::&analyser/captured ?closure-id ?captured-id ?source])
                                        (doseq [[?name ?captured] (:mappings ?frame)
                                                :when (captured? (:form ?captured))])))
                               (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                                     (.visitVarInsn Opcodes/ILOAD (inc (count (:mappings ?frame))))
                                     (.visitFieldInsn Opcodes/PUTFIELD current-class "_counter" counter-sig)
                                     (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                                           (.visitVarInsn Opcodes/ALOAD (+ clo_idx offset))
                                           (.visitFieldInsn Opcodes/PUTFIELD current-class field-name clo-field-sig))
                                         (->> (let [field-name (str "_" clo_idx)]
                                                (doto (.visitField =class (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) field-name clo-field-sig nil nil)
                                                  (.visitEnd)))
                                              (dotimes [clo_idx num-captured])
                                              (let [offset (+ 2 (count (:mappings ?frame)))]))))
                                   (->> (when (not= 0 num-captured))))
                               (.visitInsn Opcodes/RETURN)
                               (.visitMaxs 0 0)
                               (.visitEnd))
                       =method (let [default-label (new Label)
                                     branch-labels (for [_ (range num-captured)]
                                                     (new Label))]
                                 (doto (.visitMethod =class Opcodes/ACC_PUBLIC "apply" apply-signature nil nil)
                                   (.visitCode)
                                   (.visitVarInsn Opcodes/ALOAD 0)
                                   (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                                         (.visitFieldInsn Opcodes/GETFIELD current-class "_counter" counter-sig)
                                         (.visitTableSwitchInsn 0 (dec num-captured) default-label (into-array Label branch-labels))
                                         (-> (doto (.visitLabel branch-label)
                                               (.visitTypeInsn Opcodes/NEW current-class)
                                               (.visitInsn Opcodes/DUP)
                                               (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                                                     (.visitFieldInsn Opcodes/GETFIELD current-class (str "__" capt_idx) clo-field-sig))
                                                   (->> (dotimes [capt_idx (count (:mappings ?frame))])))
                                               (.visitVarInsn Opcodes/ALOAD 0)
                                               (.visitFieldInsn Opcodes/GETFIELD current-class "_counter" counter-sig)
                                               (.visitInsn Opcodes/ICONST_1)
                                               (.visitInsn Opcodes/IADD)
                                               (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                                                     (.visitFieldInsn Opcodes/GETFIELD current-class (str "_" clo_idx) clo-field-sig))
                                                   (->> (dotimes [clo_idx current-captured])))
                                               (.visitVarInsn Opcodes/ALOAD 1)
                                               (-> (.visitInsn Opcodes/ACONST_NULL)
                                                   (->> (dotimes [clo_idx (- (dec num-captured) current-captured)])))
                                               (.visitMethodInsn Opcodes/INVOKESPECIAL current-class "<init>" init-signature)
                                               (.visitInsn Opcodes/ARETURN))
                                             (->> (doseq [[branch-label current-captured] (map vector branch-labels (range (count branch-labels)))])))
                                         (.visitLabel default-label)
                                         (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                                               (.visitFieldInsn Opcodes/GETFIELD current-class (str "_" clo_idx) clo-field-sig))
                                             (->> (dotimes [clo_idx num-captured]))))
                                       (->> (when (not= 0 num-captured))))
                                   (.visitVarInsn Opcodes/ALOAD 1)
                                   (.visitMethodInsn Opcodes/INVOKEVIRTUAL current-class "impl" real-signature)
                                   (.visitInsn Opcodes/ARETURN)
                                   (.visitMaxs 0 0)
                                   (.visitEnd)))
                       =impl (doto (.visitMethod =class Opcodes/ACC_PUBLIC "impl" real-signature nil nil)
                               (.visitCode)
                               (compile ?body)
                               (.visitInsn Opcodes/ARETURN)
                               (.visitMaxs 0 0)
                               (.visitEnd))
                       _ (.visitEnd =class)
                       bytecode (.toByteArray =class)]
                   (write-class current-class bytecode)
                   (load-class! loader (string/replace current-class #"/" "."))
                   (doto *writer*
                     (.visitTypeInsn Opcodes/NEW current-class)
                     (.visitInsn Opcodes/DUP)
                     (-> (do (compile ?source))
                         (->> (match (:form ?captured)
                                [::&analyser/captured ?closure-id ?captured-id ?source])
                              (doseq [[?name ?captured] (->> (:mappings ?frame)
                                                             (filter (comp captured? :form second))
                                                             (sort #(< (-> %1 second :form (nth 2))
                                                                       (-> %2 second :form (nth 2)))))])))
                     (-> (doto (.visitInsn Opcodes/ICONST_0)
                           (-> (.visitInsn Opcodes/ACONST_NULL)
                               (->> (doseq [_ (butlast ?args)]))))
                         (->> (when (> (count ?args) 1))))
                     (.visitMethodInsn Opcodes/INVOKESPECIAL current-class "<init>" init-signature))
                   )]]
    (return nil)))

(defn ^:private compile-defclass [compile *type* ?package ?name ?super-class ?members]
  (exec [*writer* get-writer
         loader &util/loader
         :let [_ (let [parent-dir (->package ?package)
                       super-class* (->class ?super-class)
                       =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                                (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                        (str parent-dir "/" ?name) nil super-class* nil))]
                   (doseq [[field props] (:fields ?members)]
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
                   (.mkdirs (java.io.File. (str "output/" parent-dir)))
                   (write-class (str parent-dir "/" ?name) (.toByteArray =class))
                   (load-class! loader (string/replace (str parent-dir "/" ?name) #"/" ".")))]]
    (return nil)))

(defn ^:private compile-definterface [compile *type* ?package ?name ?members]
  (exec [*writer* get-writer
         loader &util/loader
         :let [_ (let [parent-dir (->package ?package)
                       =interface (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                                    (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_INTERFACE ;; Opcodes/ACC_ABSTRACT
                                                            )
                                            (str parent-dir "/" ?name) nil "java/lang/Object" nil))]
                   (doseq [[?method ?props] (:methods ?members)
                           :let [[?args ?return] (:type ?props)
                                 signature (str "(" (reduce str "" (map ->type-signature ?args)) ")" (->type-signature ?return))]]
                     (.visitMethod =interface (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT) ?method signature nil nil))
                   (.visitEnd =interface)
                   (.mkdirs (java.io.File. (str "output/" parent-dir)))
                   (write-class (str parent-dir "/" ?name) (.toByteArray =interface))
                   (load-class! loader (string/replace (str parent-dir "/" ?name) #"/" ".")))]]
    (return nil)))

(defn ^:private compile-variant [compile *type* ?tag ?members]
  (exec [*writer* get-writer
         :let [_ (let [variant-class* (str (->class +variant-class+) (count ?members))]
                   (doto *writer*
                     (.visitTypeInsn Opcodes/NEW variant-class*)
                     (.visitInsn Opcodes/DUP)
                     (.visitMethodInsn Opcodes/INVOKESPECIAL variant-class* "<init>" "()V")
                     (.visitInsn Opcodes/DUP)
                     (.visitLdcInsn ?tag)
                     (.visitFieldInsn Opcodes/PUTFIELD variant-class* "tag" (->type-signature "java.lang.String"))
                     (-> (doto (.visitInsn Opcodes/DUP)
                           (do (compile ?member))
                           (.visitFieldInsn Opcodes/PUTFIELD variant-class* (str "_" (inc ?tfield)) "Ljava/lang/Object;"))
                         (->> (doseq [[?tfield ?member] (mapv vector (range (count ?members)) ?members)]))))
                   )]]
    (return nil)))

(let [+int-class+ (->class "java.lang.Integer")]
  (do-template [<name> <opcode>]
    (defn <name> [compile *type* ?x ?y]
      (exec [*writer* get-writer
             :let [_ (do (compile ?x)
                       (doto *writer*
                         (.visitTypeInsn Opcodes/CHECKCAST +int-class+)
                         (.visitMethodInsn Opcodes/INVOKEVIRTUAL +int-class+ "intValue" "()I"))
                       (compile ?y)
                       (doto *writer*
                         (.visitTypeInsn Opcodes/CHECKCAST +int-class+)
                         (.visitMethodInsn Opcodes/INVOKEVIRTUAL +int-class+ "intValue" "()I")
                         (.visitInsn <opcode>)
                         (.visitMethodInsn Opcodes/INVOKESTATIC +int-class+ "valueOf" (str "(I)" (->type-signature "java.lang.Integer")))))]]
        (return nil)))

    ^:private compile-jvm-iadd Opcodes/IADD
    ^:private compile-jvm-isub Opcodes/ISUB
    ^:private compile-jvm-imul Opcodes/IMUL
    ^:private compile-jvm-idiv Opcodes/IDIV
    ^:private compile-jvm-irem Opcodes/IREM
    ))

(defn ^:private compile [syntax]
  (match (:form syntax)
    [::&analyser/literal ?literal]
    (compile-literal compile (:type syntax) ?literal)

    [::&analyser/tuple ?elems]
    (compile-tuple compile (:type syntax) ?elems)

    [::&analyser/local ?env ?idx]
    (compile-local compile (:type syntax) ?env ?idx)

    [::&analyser/captured ?scope ?captured-id ?source]
    (compile-captured compile (:type syntax) ?scope ?captured-id ?source)

    [::&analyser/global ?owner-class ?name]
    (compile-global compile (:type syntax) ?owner-class ?name)

    [::&analyser/global-fn ?owner-class ?name]
    (compile-global-fn compile (:type syntax) ?owner-class ?name)

    [::&analyser/call ?fn ?args]
    (compile-call compile (:type syntax) ?fn ?args)

    [::&analyser/static-call ?needs-num ?fn ?args]
    (compile-static-call compile (:type syntax) ?needs-num ?fn ?args)

    [::&analyser/jvm-getstatic ?owner ?field]
    (compile-jvm-getstatic compile (:type syntax) ?owner ?field)
    
    [::&analyser/variant ?tag ?members]
    (compile-variant compile (:type syntax) ?tag ?members)

    [::&analyser/let ?idx ?label ?value ?body]
    (compile-let compile (:type syntax) ?idx ?label ?value ?body)

    [::&analyser/case ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree]
    (compile-case compile (:type syntax) ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree)

    [::&analyser/if ?test ?then ?else]
    (compile-if compile (:type syntax) ?test ?then ?else)

    [::&analyser/lambda ?scope ?frame ?args ?body]
    (compile-lambda compile (:type syntax) ?scope ?frame ?args ?body)

    [::&analyser/def ?form ?body]
    (compile-def compile (:type syntax) ?form ?body)
    
    [::&analyser/jvm-iadd ?x ?y]
    (compile-jvm-iadd compile (:type syntax) ?x ?y)
    
    [::&analyser/jvm-isub ?x ?y]
    (compile-jvm-isub compile (:type syntax) ?x ?y)
    
    [::&analyser/jvm-imul ?x ?y]
    (compile-jvm-imul compile (:type syntax) ?x ?y)
    
    [::&analyser/jvm-idiv ?x ?y]
    (compile-jvm-idiv compile (:type syntax) ?x ?y)
    
    [::&analyser/jvm-irem ?x ?y]
    (compile-jvm-irem compile (:type syntax) ?x ?y)

    [::&analyser/do ?exprs]
    (compile-do compile (:type syntax) ?exprs)

    [::&analyser/jvm-new ?class ?classes ?args]
    (compile-jvm-new compile (:type syntax) ?class ?classes ?args)

    [::&analyser/jvm-invokevirtual ?class ?method ?classes ?object ?args]
    (compile-jvm-invokevirtual compile (:type syntax) ?class ?method ?classes ?object ?args)

    [::&analyser/jvm-new-array ?class ?length]
    (compile-jvm-new-array compile (:type syntax) ?class ?length)

    [::&analyser/jvm-aastore ?array ?idx ?elem]
    (compile-jvm-aastore compile (:type syntax) ?array ?idx ?elem)

    [::&analyser/jvm-aaload ?array ?idx]
    (compile-jvm-aaload compile (:type syntax) ?array ?idx)

    [::&analyser/definterface [?package ?name] ?members]
    (compile-definterface compile (:type syntax) ?package ?name ?members)

    [::&analyser/defclass [?package ?name] ?super-class ?members]
    (compile-defclass compile (:type syntax) ?package ?name ?super-class ?members)
    ))

;; [Interface]
(let [compiler-step (exec [analysis+ &analyser/analyse]
                      (map-m compile analysis+))]
  (defn compile-module [name]
    (exec [loader &util/loader]
      (fn [state]
        (if (-> state :modules (contains? name))
          (fail "[Compiler Error] Can't redefine a module!")
          (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 (->class name) nil "java/lang/Object" nil))]
            (match ((repeat-m compiler-step) (assoc state
                                               ::&lexer/source (slurp (str "source/" name ".lux"))
                                               ::&analyser/current-module name
                                               ::writer =class))
              [::&util/ok [?state ?forms]]
              (if (empty? (::&lexer/source ?state))
                (do (.visitEnd =class)
                  (write-class name (.toByteArray =class))
                  (load-class! loader (string/replace name #"/" "."))
                  [::&util/ok [?state nil]])
                (assert false (str "[Compiler Error] Can't compile: " (::&lexer/source ?state))))
              
              [::&util/failure ?message]
              (fail* ?message))))))))

(defn compile-all [modules]
  (let [state {::&lexer/source nil
               ::&analyser/current-module nil
               ::&analyser/modules {}
               ::&analyser/global-env {}
               ::&analyser/local-envs (list)
               ::&analyser/types &type/+init+
               ::writer nil
               ::&util/loader (&util/class-loader!)}]
    (match ((map-m compile-module modules) state)
      [::&util/ok [?state ?forms]]
      (println (str "Compilation complete! " (pr-str modules)))

      [::&util/failure ?message]
      (assert false ?message))))

(comment
  (compile-all ["lux"])
  )
