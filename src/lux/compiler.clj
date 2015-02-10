(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m exhaust-m try-m try-all-m map-m reduce-m
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

;; [Utils/General]
(defn ^:private storage-id [scope]
  (->> scope reverse (map normalize-ident) (interpose "$") (reduce str "")))

(defn ^:private write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(defn ^:private write-class [name data]
  (write-file (str "output/" name ".class") data))

(defn ^:private load-class! [loader name]
  (.loadClass loader name))

(defn ^:private save-class! [name bytecode]
  (exec [loader &util/loader
         :let [_ (write-class name bytecode)
               _ (load-class! loader (string/replace name #"/" "."))]]
    (return nil)))

(def ^:private +prefix+ "lux.")
(def ^:private +variant-class+  (str +prefix+ "Variant"))
(def ^:private +tuple-class+    (str +prefix+ "Tuple"))
(def ^:private +function-class+ (str +prefix+ "Function"))
(def ^:private +local-prefix+ "l")
(def ^:private +partial-prefix+ "p")
(def ^:private +closure-prefix+ "c")

(def ^:private ->package ->class)

(defn ^:private ->type-signature [class]
  (case class
    "void"    "V"
    "boolean" "Z"
    "byte"    "B"
    "short"   "S"
    "int"     "I"
    "long"    "J"
    "float"   "F"
    "double"  "D"
    "char"    "C"
    ;; else
    (let [class* (->class class)]
      (if (.startsWith class* "[")
        class*
        (str "L" class* ";")))
    ))

(defn ^:private ->java-sig [type]
  (match type
    ::&type/Any
    (->type-signature "java.lang.Object")
    
    [::&type/Data ?name]
    (->type-signature ?name)

    [::&type/Array ?elem]
    (str "[" (->java-sig ?elem))

    [::&type/variant ?tag ?value]
    (->type-signature +variant-class+)

    [::&type/Lambda _ _]
    (->type-signature +function-class+)))

;; [Utils/Compilers]
(let [+class+ (->class "java.lang.Boolean")
      +sig+ (->type-signature "java.lang.Boolean")]
  (defn ^:private compile-bool [compile *type* ?value]
    (exec [*writer* &util/get-writer
           :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class "java.lang.Boolean") (if ?value "TRUE" "FALSE") (->type-signature "java.lang.Boolean"))]]
      (return nil))))

(do-template [<name> <class> <sig>]
  (let [+class+ (->class <class>)]
    (defn <name> [compile *type* ?value]
      (exec [*writer* &util/get-writer
             :let [_ (doto *writer*
                       (.visitTypeInsn Opcodes/NEW <class>)
                       (.visitInsn Opcodes/DUP)
                       (.visitLdcInsn ?literal)
                       (.visitMethodInsn Opcodes/INVOKESPECIAL <class> "<init>" <sig>))]]
        (return nil))))

  ^:private compile-int  "java.lang.Long"      "(J)V"
  ^:private compile-real "java.lang.Double"    "(D)V"
  ^:private compile-char "java.lang.Character" "(C)V"
  )

(defn ^:private compile-text [compile *type* ?value]
  (exec [*writer* &util/get-writer
         :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn ^:private compile-tuple [compile *type* ?elems]
  (exec [*writer* &util/get-writer
         :let [num-elems (count ?elems)
               tuple-class (->class (str +tuple-class+ num-elems))
               _ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW tuple-class)
                   (.visitInsn Opcodes/DUP)
                   (.visitMethodInsn Opcodes/INVOKESPECIAL tuple-class "<init>" "()V"))]
         _ (map-m (fn [idx]
                    (exec [:let [_ (.visitInsn *writer* Opcodes/DUP)]
                           ret (compile (nth ?elems idx))
                           :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD tuple-class (str +partial-prefix+ idx) "Ljava/lang/Object;")]]
                      (return ret)))
                  (range num-elems))]
    (return nil)))

(defn ^:private compile-variant [compile *type* ?tag ?members]
  (exec [*writer* &util/get-writer
         :let [variant-class* (str (->class +variant-class+) (count ?members))
               _ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW variant-class*)
                   (.visitInsn Opcodes/DUP)
                   (.visitMethodInsn Opcodes/INVOKESPECIAL variant-class* "<init>" "()V")
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn ?tag)
                   (.visitFieldInsn Opcodes/PUTFIELD variant-class* "tag" (->type-signature "java.lang.String")))]
         _ (map-m (fn [[?tfield ?member]]
                    (exec [:let [_ (.visitInsn *writer* Opcodes/DUP)]
                           ret (compile ?member)
                           :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD variant-class* (str +partial-prefix+ ?tfield) "Ljava/lang/Object;")]]
                      (return ret)))
                  (map vector (range (count ?members)) ?members))]
    (return nil)))

(defn ^:private compile-local [compile *type* ?idx]
  (exec [*writer* &util/get-writer
         :let [_ (.visitVarInsn *writer* Opcodes/ALOAD (int ?idx))]]
    (return nil)))

(defn ^:private compile-captured [compile *type* ?scope ?captured-id ?source]
  (exec [*writer* &util/get-writer
         :let [_ (doto *writer*
                   (.visitVarInsn Opcodes/ALOAD 0)
                   (.visitFieldInsn Opcodes/GETFIELD
                                    (normalize-ident ?scope)
                                    (str +closure-prefix+ ?captured-id)
                                    "Ljava/lang/Object;"))]]
    (return nil)))

(defn ^:private compile-global [compile *type* ?owner-class ?name]
  (exec [*writer* &util/get-writer
         :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class (storage-id (list ?name ?owner-class))) "_datum" "Ljava/lang/Object;")]]
    (return nil)))

(def +apply-signature+ "(Ljava/lang/Object;)Ljava/lang/Object;")

(defn ^:private compile-call [compile *type* ?fn ?args]
  (exec [*writer* &util/get-writer
         _ (compile ?fn)
         _ (map-m (fn [arg]
                    (exec [ret (compile arg)
                           :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (->class +function-class+) "apply" +apply-signature+)]]
                      (return ret)))
                  ?args)]
    (return nil)))

(defn ^:private compile-static-call [compile *type* ?needs-num ?fn ?args]
  (assert false (pr-str 'compile-static-call))
  (exec [*writer* &util/get-writer
         :let [_ (match (:form ?fn)
                   [::&analyser/global ?owner-class ?fn-name]
                   (let [arg-sig (->type-signature "java.lang.Object")
                         call-class (storage-id (list ?fn-name ?owner-class))
                         provides-num (count ?args)]
                     (if (>= provides-num ?needs-num)
                       (let [impl-sig (str "(" (reduce str "" (repeat ?needs-num arg-sig)) ")" arg-sig)]
                         (doto *writer*
                           (-> (do (compile arg))
                               (->> (doseq [arg (take ?needs-num ?args)])))
                           (.visitMethodInsn Opcodes/INVOKESTATIC call-class "impl" impl-sig)
                           (-> (doto (do (compile arg))
                                 (.visitMethodInsn Opcodes/INVOKEINTERFACE (->class +function-class+) "apply" +apply-signature+))
                               (->> (doseq [arg (drop ?needs-num ?args)])))))
                       (let [counter-sig "I"
                             init-signature (str "(" (apply str counter-sig (repeat (dec ?needs-num) arg-sig)) ")" "V")]
                         (doto *writer*
                           (.visitTypeInsn Opcodes/NEW call-class)
                           (.visitInsn Opcodes/DUP)
                           (.visitLdcInsn (int provides-num))
                           (-> (do (compile arg))
                               (->> (doseq [arg ?args])))
                           (add-nulls (dec (- ?needs-num provides-num)))
                           (.visitMethodInsn Opcodes/INVOKESPECIAL call-class "<init>" init-signature)))
                       ))
                   )]]
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

(defn ^:private compile-do [compile *type* ?exprs]
  (exec [*writer* &util/get-writer
         _ (map-m (fn [expr]
                    (exec [ret (compile expr)
                           :let [_ (.visitInsn *writer* Opcodes/POP)]]
                      (return ret)))
                  (butlast ?exprs))
         _ (compile (last ?exprs))]
    (return nil)))

(do-template [<name> <wrapper-class> <value-method> <method-sig>]
  (defn <name> [writer mappings default-label ?pairs]
    (doseq [[?token $body] ?pairs
            :let [$else (new Label)]]
      (doto writer
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class <wrapper-class>) <value-method> <method-sig>)
        (.visitLdcInsn ?token)
        (.visitJumpInsn Opcodes/IF_ICMPNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO (get mappings $body))
        (.visitLabel $else)))
    (doto writer
      (.visitInsn Opcodes/POP)
      (.visitJumpInsn Opcodes/GOTO default-label)))

  ^:private compile-compare-bools "java.lang.Boolean"   "booleanValue" "()Z"
  ^:private compile-compare-chars "java.lang.Character" "charValue"    "()C"
  )

(do-template [<name> <wrapper-class> <value-method> <method-sig> <cmp-op>]
  (defn <name> [writer mappings default-label ?pairs]
    (doseq [[?token $body] ?pairs
            :let [$else (new Label)]]
      (doto writer
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class <wrapper-class>) <value-method> <method-sig>)
        (.visitLdcInsn ?token)
        (.visitInsn <cmp-op>)
        (.visitJumpInsn Opcodes/IFNE $else)
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO (get mappings $body))
        (.visitLabel $else)))
    (doto writer
      (.visitInsn Opcodes/POP)
      (.visitJumpInsn Opcodes/GOTO default-label)))

  ^:private compile-compare-ints  "java.lang.Long"   "longValue"   "()J" Opcodes/LCMP
  ^:private compile-compare-reals "java.lang.Double" "doubleValue" "()D" Opcodes/DCMPL
  )

(defn ^:private compile-compare-texts [writer mappings default-label ?pairs]
  (doseq [[?token $body] ?pairs
          :let [$else (new Label)]]
    (doto writer
      (.visitInsn Opcodes/DUP)
      (.visitLdcInsn ?token)
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.lang.Object") "equals" (str "(" (->type-signature "java.lang.Object") ")Z"))
      (.visitJumpInsn Opcodes/IFEQ $else)
      (.visitInsn Opcodes/POP)
      (.visitJumpInsn Opcodes/GOTO (get mappings $body))
      (.visitLabel $else)))
  (doto writer
    (.visitInsn Opcodes/POP)
    (.visitJumpInsn Opcodes/GOTO default-label)))

(let [+tag-sig+ (->type-signature "java.lang.String")
      variant-class* (->class +variant-class+)
      tuple-class* (->class +tuple-class+)
      +variant-field-sig+ (->type-signature "java.lang.Object")
      oclass (->class "java.lang.Object")
      equals-sig (str "(" (->type-signature "java.lang.Object") ")Z")]
  (defn ^:private compile-decision-tree [writer mappings default-label decision-tree]
    (match decision-tree
      [::test-bool ?pairs]
      (compile-compare-bools writer mappings default-label ?pairs)
      
      [::test-int  ?pairs]
      (compile-compare-ints writer mappings default-label ?pairs)

      [::test-real ?pairs]
      (compile-compare-reals writer mappings default-label ?pairs)

      [::test-char ?pairs]
      (compile-compare-chars writer mappings default-label ?pairs)
      
      [::test-text ?pairs]
      (compile-compare-texts writer mappings default-label ?pairs)

      [::store [::&analyser/local ?idx] $body]
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
                          (.visitFieldInsn Opcodes/GETFIELD tuple-class** (str +partial-prefix+ ?subidx) +variant-field-sig+) ;; tuple, object
                          (compile-decision-tree (assoc mappings $body sub-next-elem) next-subcase ?subpart) ;; tuple
                          (.visitLabel sub-next-elem)))
                    (doto writer
                      (.visitInsn Opcodes/POP)
                      (.visitJumpInsn Opcodes/GOTO (get mappings $body))
                      (.visitLabel next-subcase)))
                  )))
          (.visitInsn Opcodes/POP) ;; ->
          (.visitJumpInsn Opcodes/GOTO default-label)))

      [::test-variant ?branches ?cases]
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
                                (.visitFieldInsn Opcodes/GETFIELD variant-class** (str +partial-prefix+ ?subidx) +variant-field-sig+) ;; variant, object
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

(defn ^:private sequence-val [<test-tag> struct branches]
  (concat (list [[<test-tag> (for [[?token ?supports] (:patterns struct)
                                   ?body (set/intersection branches ?supports)]
                               [?token ?body])]
                 branches])
          (for [[_ ?local ?body] (:defaults struct)
                :when (contains? branches ?body)]
            [[::store ?local ?body] #{?body}])))

(defn ^:private sequence-product [<test-tag> struct branches]
  (concat (let [patterns (into {} (for [[?tag ?struct] (:patterns struct)
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
              (list [[<test-tag> branches patterns]
                     branches])))
          (if-let [[_ ?local ?body] (:default struct)]
            (for [?body (set/intersection branches #{?body})]
              [[::store ?local ?body] #{?body}])
            '())))

(defn ^:private sequence-parts [branches parts]
  (if (empty? parts)
    (list (list))
    (let [[head & tail] parts
          expanded (case (:type head)
                     ::&analyser/defaults
                     (for [[?local ?supports] (:stores head)
                           ?body (set/intersection branches ?supports)]
                       [[::store ?local ?body] #{?body}])

                     ::&analyser/bool-tests
                     (sequence-val ::test-bool head branches)

                     ::&analyser/int-tests
                     (sequence-val ::test-int head branches)

                     ::&analyser/real-tests
                     (sequence-val ::test-real head branches)

                     ::&analyser/char-tests
                     (sequence-val ::test-char head branches)

                     ::&analyser/text-tests
                     (sequence-val ::test-text head branches)

                     ::&analyser/tuple
                     (sequence-product ::test-tuple head branches)

                     ::&analyser/variant
                     (sequence-product ::test-variant head branches)
                     )]
      (for [[step branches*] expanded
            tail* (sequence-parts branches* tail)]
        (cons step tail*)))))

(let [oclass (->class "java.lang.Object")
      equals-sig (str "(" (->type-signature "java.lang.Object") ")Z")
      ex-class (->class "java.lang.IllegalStateException")]
  (defn ^:private compile-case [compile *type* ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree]
    (exec [*writer* &util/get-writer
           :let [start-label (new Label)
                 end-label (new Label)
                 entries (for [[?branch ?body] ?branch-mappings
                               :let [label (new Label)]]
                           [[?branch label]
                            [label ?body]])
                 mappings* (into {} (map first entries))
                 _ (dotimes [offset ?max-registers]
                     (let [idx (+ ?base-idx offset)]
                       (.visitLocalVariable *writer* (str +local-prefix+ idx) (->java-sig [::&type/Any]) nil start-label end-label idx)))]
           _ (compile ?variant)
           :let [_ (doto *writer*
                     (.visitInsn Opcodes/DUP)
                     (.visitLabel start-label))
                 default-label (new Label)
                 ;; _ (prn '?decision-tree ?decision-tree)
                 _ (do (doseq [decision-tree (let [pieces (map first (sequence-parts (:branches ?decision-tree) (list ?decision-tree)))]
                                               (if (or (:default ?decision-tree)
                                                       (not (empty? (:defaults ?decision-tree))))
                                                 (butlast pieces)
                                                 pieces))]
                         (compile-decision-tree *writer* mappings* default-label decision-tree))
                     (.visitLabel *writer* default-label)
                     (if-let [[_ [_ ?idx] ?body] (or (:default ?decision-tree)
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
                         (.visitInsn Opcodes/ATHROW))))]
           _ (map-m (fn [[?label ?body]]
                      (exec [:let [_ (do (.visitLabel *writer* ?label)
                                       (.visitInsn *writer* Opcodes/POP))]
                             ret (compile ?body)
                             :let [_ (.visitJumpInsn *writer* Opcodes/GOTO end-label)]]
                        (return ret)))
                    (map second entries))
           :let [_ (.visitLabel *writer* end-label)]]
      (return nil))))

(let [clo-field-sig (->type-signature "java.lang.Object")
      lambda-return-sig (->type-signature "java.lang.Object")
      <init>-return "V"
      counter-sig "I"
      +datum-sig+ (->type-signature "java.lang.Object")]
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
  
  (defn ^:private compile-lambda [compile *type* ?scope ?closure ?args ?body with-datum? instance?]
    (exec [:let [lambda-class (storage-id ?scope)
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
  )

(defn ^:private compile-field [compile *type* ?name body]
  (exec [*writer* &util/get-writer
         class-name &analyser/module-name
         :let [outer-class (->class class-name)
               datum-sig (->type-signature "java.lang.Object")
               current-class (storage-id (list ?name outer-class))
               _ (.visitInnerClass *writer* current-class outer-class nil (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
               =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                        (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                                current-class nil "java/lang/Object" (into-array [(->class +function-class+)]))
                        (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_datum" datum-sig nil nil)
                            (doto (.visitEnd))))]
         _ (&util/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
             (exec [*writer* &util/get-writer
                    :let [_ (.visitCode *writer*)]
                    _ (compile body)
                    :let [_ (doto *writer*
                              (.visitFieldInsn Opcodes/PUTSTATIC current-class "_datum" datum-sig)
                              (.visitInsn Opcodes/RETURN)
                              (.visitMaxs 0 0)
                              (.visitEnd))]]
               (return nil)))
         :let [_ (.visitEnd *writer*)]
         _ (save-class! current-class (.toByteArray =class))]
    (return nil)))

(defn ^:private compile-def [compile *type* name value]
  (exec [_ (match value
             [::&analyser/Expression ?form _]
             (match ?form
               [::&analyser/lambda ?scope ?captured ?args ?body]
               (compile-lambda compile *type* ?scope ?closure ?args ?body true false)

               _
               (compile-field compile *type* name value))
             
             _
             (fail "Can only define expressions."))]
    (return nil)))

(defn ^:private compile-jvm-class [compile *type* ?package ?name ?super-class ?fields ?methods]
  (exec [*writer* &util/get-writer
         loader &util/loader
         :let [parent-dir (->package ?package)
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
         _ (save-class! full-name (.toByteArray =class))]
    (return nil)))

(defn ^:private compile-jvm-interface [compile *type* ?package ?name ?fields ?methods]
  (exec [*writer* &util/get-writer
         loader &util/loader
         :let [parent-dir (->package ?package)
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
         _ (save-class! full-name (.toByteArray =interface))]
    (return nil)))

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

(defn ^:private compile-self-call [compile ?assumed-args]
  (exec [*writer* &util/get-writer
         :let [_ (.visitVarInsn *writer* Opcodes/ALOAD 0)]
         _ (map-m (fn [arg]
                    (exec [ret (compile arg)
                           :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (->class +function-class+) "apply" +apply-signature+)]]
                      (return ret)))
                  ?assumed-args)]
    (return nil)))

(defn ^:private compile-expression [syntax]
  (match (:form syntax)
    [::&analyser/bool ?value]
    (compile-bool compile-expression (:type syntax) ?value)

    [::&analyser/int ?value]
    (compile-int compile-expression (:type syntax) ?value)

    [::&analyser/real ?value]
    (compile-real compile-expression (:type syntax) ?value)

    [::&analyser/char ?value]
    (compile-char compile-expression (:type syntax) ?value)

    [::&analyser/text ?value]
    (compile-text compile-expression (:type syntax) ?value)

    [::&analyser/tuple ?elems]
    (compile-tuple compile-expression (:type syntax) ?elems)

    [::&analyser/local ?idx]
    (compile-local compile-expression (:type syntax) ?idx)

    [::&analyser/captured ?scope ?captured-id ?source]
    (compile-captured compile-expression (:type syntax) ?scope ?captured-id ?source)

    [::&analyser/global ?owner-class ?name]
    (compile-global compile-expression (:type syntax) ?owner-class ?name)

    [::&analyser/call ?fn ?args]
    (compile-call compile-expression (:type syntax) ?fn ?args)

    [::&analyser/static-call ?needs-num ?fn ?args]
    (compile-static-call compile-expression (:type syntax) ?needs-num ?fn ?args)

    [::&analyser/variant ?tag ?members]
    (compile-variant compile-expression (:type syntax) ?tag ?members)

    [::&analyser/case ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree]
    (compile-case compile-expression (:type syntax) ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree)

    [::&analyser/lambda ?scope ?frame ?args ?body]
    (compile-lambda compile-expression (:type syntax) ?scope ?frame ?args ?body false true)

    ;; Integer arithmetic
    [::&analyser/jvm-iadd ?x ?y]
    (compile-jvm-iadd compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-isub ?x ?y]
    (compile-jvm-isub compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-imul ?x ?y]
    (compile-jvm-imul compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-idiv ?x ?y]
    (compile-jvm-idiv compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-irem ?x ?y]
    (compile-jvm-irem compile-expression (:type syntax) ?x ?y)

    ;; Long arithmetic
    [::&analyser/jvm-ladd ?x ?y]
    (compile-jvm-ladd compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-lsub ?x ?y]
    (compile-jvm-lsub compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-lmul ?x ?y]
    (compile-jvm-lmul compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-ldiv ?x ?y]
    (compile-jvm-ldiv compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-lrem ?x ?y]
    (compile-jvm-lrem compile-expression (:type syntax) ?x ?y)

    ;; Float arithmetic
    [::&analyser/jvm-fadd ?x ?y]
    (compile-jvm-fadd compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-fsub ?x ?y]
    (compile-jvm-fsub compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-fmul ?x ?y]
    (compile-jvm-fmul compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-fdiv ?x ?y]
    (compile-jvm-fdiv compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-frem ?x ?y]
    (compile-jvm-frem compile-expression (:type syntax) ?x ?y)

    ;; Double arithmetic
    [::&analyser/jvm-dadd ?x ?y]
    (compile-jvm-dadd compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-dsub ?x ?y]
    (compile-jvm-dsub compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-dmul ?x ?y]
    (compile-jvm-dmul compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-ddiv ?x ?y]
    (compile-jvm-ddiv compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-drem ?x ?y]
    (compile-jvm-drem compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/do ?exprs]
    (compile-do compile-expression (:type syntax) ?exprs)

    [::&analyser/jvm-new ?class ?classes ?args]
    (compile-jvm-new compile-expression (:type syntax) ?class ?classes ?args)

    [::&analyser/jvm-getstatic ?class ?field]
    (compile-jvm-getstatic compile-expression (:type syntax) ?class ?field)

    [::&analyser/jvm-getfield ?class ?field ?object]
    (compile-jvm-getfield compile-expression (:type syntax) ?class ?field ?object)
    
    [::&analyser/jvm-invokestatic ?class ?method ?classes ?args]
    (compile-jvm-invokestatic compile-expression (:type syntax) ?class ?method ?classes ?args)

    [::&analyser/jvm-invokevirtual ?class ?method ?classes ?object ?args]
    (compile-jvm-invokevirtual compile-expression (:type syntax) ?class ?method ?classes ?object ?args)

    [::&analyser/jvm-new-array ?class ?length]
    (compile-jvm-new-array compile-expression (:type syntax) ?class ?length)

    [::&analyser/jvm-aastore ?array ?idx ?elem]
    (compile-jvm-aastore compile-expression (:type syntax) ?array ?idx ?elem)

    [::&analyser/jvm-aaload ?array ?idx]
    (compile-jvm-aaload compile-expression (:type syntax) ?array ?idx)

    [::&analyser/self ?assumed-args]
    (compile-self-call compile-expression ?assumed-args)

    _
    (fail "[Compiler Error] Can't compile expressions as top-level forms.")
    ))

(defn ^:private compile-statement [syntax]
  (match (:form syntax)
    [::&analyser/def ?form ?body]
    (compile-def compile-expression (:type syntax) ?form ?body)
    
    [::&analyser/jvm-interface [?package ?name] ?members]
    (compile-jvm-interface compile-expression (:type syntax) ?package ?name ?members)

    [::&analyser/jvm-class [?package ?name] ?super-class ?members]
    (compile-jvm-class compile-expression (:type syntax) ?package ?name ?super-class ?members)

    _
    (fail "[Compiler Error] Can't compile expressions as top-level forms.")
    ))

;; [Interface]
(let [compiler-step (exec [analysis+ &analyser/analyse]
                      (map-m compile-statement analysis+))]
  (defn compile-module [name]
    (exec [loader &util/loader]
      (fn [state]
        (if (-> state ::&util/modules (contains? name))
          (fail "[Compiler Error] Can't redefine a module!")
          (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 (->class name) nil "java/lang/Object" nil))]
            (match (&util/run-state (exhaust-m compiler-step) (assoc state
                                                                ::&util/source (slurp (str "source/" name ".lux"))
                                                                ::&util/current-module name
                                                                ::&util/writer =class))
              [::&util/ok [?state _]]
              (do (.visitEnd =class)
                (&util/run-state (save-class! name (.toByteArray =class)) ?state))
              
              [::&util/failure ?message]
              (fail* ?message))))))))

(defn compile-all [modules]
  (.mkdir (java.io.File. "output"))
  (match (&util/run-state (map-m compile-module modules) (&util/init-state))
    [::&util/ok [?state _]]
    (println (str "Compilation complete! " (pr-str modules)))

    [::&util/failure ?message]
    (assert false ?message)))

(comment
  (compile-all ["lux"])
  )
