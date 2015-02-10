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

(def +prefix+ "lux")

;; [Utils/General]
(defn ^:private write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(defn ^:private write-class [name data]
  (write-file (str "output/" name ".class") data))

(defn ^:private load-class! [loader name]
  (.loadClass loader name))

(defn save-class! [name bytecode]
  (exec [loader &util/loader
         :let [_ (write-class name bytecode)
               _ (load-class! loader (string/replace name #"/" "."))]]
    (return nil)))

(def ^:private +variant-class+ (str +prefix+ ".Variant"))
(def ^:private +tuple-class+   (str +prefix+ ".Tuple"))

(defn ^:private unwrap-ident [ident]
  (match ident
    [::&parser/ident ?label]
    ?label))

(defn ^:private with-writer [writer body]
  (fn [state]
    ;; (prn 'with-writer/_0 body)
    (let [result (body (assoc state ::&util/writer writer))]
      ;; (prn 'with-writer/_1 result)
      (match result
        [::&util/ok [?state ?value]]
        [::&util/ok [(assoc ?state ::&util/writer (::&util/writer state)) ?value]]

        _
        result))))

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
    (->type-signature (str +prefix+ "/Function"))))

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

  ^:private compile-int  "java.lang.Integer"   "(I)V"
  ^:private compile-real "java.lang.Float"     "(F)V"
  ^:private compile-char "java.lang.Character" "(C)V"
  )

(defn ^:private compile-text [compile *type* ?value]
  (exec [*writer* &util/get-writer
         :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn ^:private compile-tuple [compile *type* ?elems]
  (exec [*writer* &util/get-writer
         :let [num-elems (count ?elems)
               tuple-class (str +prefix+ "/Tuple" num-elems)
               _ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW tuple-class)
                   (.visitInsn Opcodes/DUP)
                   (.visitMethodInsn Opcodes/INVOKESPECIAL tuple-class "<init>" "()V"))]
         _ (map-m (fn [idx]
                    (exec [:let [_ (.visitInsn *writer* Opcodes/DUP)]
                           ret (compile (nth ?elems idx))
                           :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD tuple-class (str "_" (inc idx)) "Ljava/lang/Object;")]]
                      (return ret)))
                  (range num-elems))]
    (return nil)))

(defn ^:private compile-local [compile *type* ?idx]
  (exec [*writer* &util/get-writer
         :let [_ (.visitVarInsn *writer* Opcodes/ALOAD (int (inc ?idx)))]]
    (return nil)))

(defn ^:private compile-captured [compile *type* ?scope ?captured-id ?source]
  (exec [*writer* &util/get-writer
         :let [_ (doto *writer*
                   (.visitVarInsn Opcodes/ALOAD 0)
                   (.visitFieldInsn Opcodes/GETFIELD
                                    (normalize-ident ?scope)
                                    (str "__" ?captured-id)
                                    "Ljava/lang/Object;"))]]
    (return nil)))

(defn ^:private compile-global [compile *type* ?owner-class ?name]
  (exec [*writer* &util/get-writer
         :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class (str ?owner-class "$" (normalize-ident ?name))) "_datum" "Ljava/lang/Object;")]]
    (return nil)))

(def +apply-signature+ "(Ljava/lang/Object;)Ljava/lang/Object;")

(defn ^:private compile-call [compile *type* ?fn ?args]
  (exec [*writer* &util/get-writer
         _ (compile ?fn)
         _ (map-m (fn [arg]
                    (exec [ret (compile arg)
                           :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (str +prefix+ "/Function") "apply" +apply-signature+)]]
                      (return ret)))
                  ?args)]
    (return nil)))

(defn ^:private compile-static-call [compile *type* ?needs-num ?fn ?args]
  (assert false (pr-str 'compile-static-call))
  (exec [*writer* &util/get-writer
         :let [_ (match (:form ?fn)
                   [::&analyser/global ?owner-class ?fn-name]
                   (let [arg-sig (->type-signature "java.lang.Object")
                         call-class (str (->class ?owner-class) "$" (normalize-ident ?fn-name))
                         provides-num (count ?args)]
                     (if (>= provides-num ?needs-num)
                       (let [impl-sig (str "(" (reduce str "" (repeat ?needs-num arg-sig)) ")" arg-sig)]
                         (doto *writer*
                           (-> (do (compile arg))
                               (->> (doseq [arg (take ?needs-num ?args)])))
                           (.visitMethodInsn Opcodes/INVOKESTATIC call-class "impl" impl-sig)
                           (-> (doto (do (compile arg))
                                 (.visitMethodInsn Opcodes/INVOKEINTERFACE (str +prefix+ "/Function") "apply" +apply-signature+))
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

(let [oclass (->class "java.lang.Object")
      equals-sig (str "(" (->type-signature "java.lang.Object") ")Z")]
  (defn ^:private compile-compare-primitive [writer mappings default-label ?pairs wrapper-class signature]
    (let [wrapper-class (->class wrapper-class)]
      (doseq [[?token $body] ?pairs
              :let [$else (new Label)]]
        (doto writer
          ;; object
          (.visitInsn Opcodes/DUP) ;; object, object
          (-> (doto (.visitTypeInsn Opcodes/NEW wrapper-class)
                (.visitInsn Opcodes/DUP)
                (.visitLdcInsn ?token)
                (.visitMethodInsn Opcodes/INVOKESPECIAL wrapper-class "<init>" signature))
              (->> (if (nil? wrapper-class)
                     (.visitLdcInsn writer ?token))))
          (.visitMethodInsn Opcodes/INVOKEVIRTUAL oclass "equals" equals-sig) ;; object, B
          (.visitJumpInsn Opcodes/IFEQ $else) ;; object
          (.visitInsn Opcodes/POP)
          (.visitJumpInsn Opcodes/GOTO (get mappings $body))
          (.visitLabel $else)))
      (doto writer
        (.visitInsn Opcodes/POP)
        (.visitJumpInsn Opcodes/GOTO default-label)))))

(let [+tag-sig+ (->type-signature "java.lang.String")
      variant-class* (->class +variant-class+)
      tuple-class* (->class +tuple-class+)
      +variant-field-sig+ (->type-signature "java.lang.Object")
      oclass (->class "java.lang.Object")
      equals-sig (str "(" (->type-signature "java.lang.Object") ")Z")]
  (defn ^:private compile-decision-tree [writer mappings default-label decision-tree]
    (match decision-tree
      [::test-bool ?pairs]
      (compile-compare-primitive writer mappings default-label ?pairs "java.lang.Boolean"   "(Z)V")
      
      [::test-int ?pairs]
      (compile-compare-primitive writer mappings default-label ?pairs "java.lang.Integer"   "(I)V")

      [::test-real ?pairs]
      (compile-compare-primitive writer mappings default-label ?pairs "java.lang.Float"     "(F)V")

      [::test-char ?pairs]
      (compile-compare-primitive writer mappings default-label ?pairs "java.lang.Character" "(C)V")
      
      [::test-text ?pairs]
      (compile-compare-primitive writer mappings default-label ?pairs nil                   nil)

      [::store [::&analyser/local ?idx] $body]
      (doto writer
        (.visitVarInsn Opcodes/ASTORE (inc ?idx))
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
                       (.visitLocalVariable *writer* (str "v" idx) (->java-sig [::&type/Any]) nil start-label end-label idx)))]
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
                         (.visitVarInsn Opcodes/ASTORE (inc ?idx))
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

(defn ^:private compile-let [compile *type* ?idx ?value ?body]
  (exec [*writer* &util/get-writer
         _ (compile ?value)
         :let [start-label (new Label)
               end-label (new Label)
               _ (doto *writer*
                   (.visitLocalVariable (str "v" ?idx) (->java-sig (:type ?value)) nil start-label end-label ?idx)
                   (.visitLabel start-label)
                   (.visitVarInsn Opcodes/ASTORE (inc ?idx)))]
         _ (compile ?body)
         :let [_ (.visitLabel *writer* end-label)]]
    (return nil)))

(defn ^:private compile-field [compile ?name body]
  (exec [*writer* &util/get-writer
         class-name &analyser/module-name
         :let [outer-class (->class class-name)
               datum-sig (->type-signature "java.lang.Object")
               current-class (str outer-class "$" (normalize-ident ?name))
               _ (.visitInnerClass *writer* current-class outer-class nil (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
               =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                        (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                                current-class nil "java/lang/Object" (into-array [(str +prefix+ "/Function")]))
                        (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_datum" datum-sig nil nil)
                            (doto (.visitEnd))))]
         _ (with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
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
              (.visitVarInsn Opcodes/ALOAD (inc ?captured-id))
              (.visitFieldInsn Opcodes/PUTFIELD class-name captured-name clo-field-sig))
            (->> (let [captured-name (str "__" ?captured-id)])
                 (match (:form ?captured)
                   [::&analyser/captured ?closure-id ?captured-id ?source])
                 (doseq [[?name ?captured] closed-over])))
        (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
              (.visitVarInsn Opcodes/ILOAD (inc num-mappings))
              (.visitFieldInsn Opcodes/PUTFIELD class-name "_counter" counter-sig)
              (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                    (.visitVarInsn Opcodes/ALOAD (+ clo_idx offset))
                    (.visitFieldInsn Opcodes/PUTFIELD class-name field-name clo-field-sig))
                  (->> (let [field-name (str "_" clo_idx)]
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

    ^:private add-closed-over-vars "__"
    ^:private add-partial-vars     "_"
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
                    (add-closed-over-vars class-name closed-over)
                    (.visitLdcInsn (-> current-captured inc int))
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
    (with-writer (doto (.visitMethod class Opcodes/ACC_PUBLIC "impl" impl-signature nil nil)
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
           :let [;; _ (prn 'instance-closure/*writer* *writer*)
                 num-args (count args)
                 _ (doto *writer*
                     (.visitTypeInsn Opcodes/NEW lambda-class)
                     (.visitInsn Opcodes/DUP))]
           _ (->> closed-over
                  (sort #(< (-> %1 second :form (nth 2))
                            (-> %2 second :form (nth 2))))
                  (map-m (fn [[?name ?captured]]
                           (match (:form ?captured)
                             [::&analyser/captured ?closure-id ?captured-id ?source]
                             (compile ?source)))))
           :let [_ (do (when (> num-args 1)
                         (.visitInsn *writer* Opcodes/ICONST_0)
                         (dotimes [_ (dec num-args)]
                           (.visitInsn *writer* Opcodes/ACONST_NULL)))
                     (.visitMethodInsn *writer* Opcodes/INVOKESPECIAL lambda-class "<init>" init-signature))]]
      (return nil)))
  
  (defn ^:private add-lambda-<clinit> [class class-name args <init>-sig]
    (let [num-args (count args)]
      (doto (.visitMethod class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
        (.visitCode)
        (.visitTypeInsn Opcodes/NEW class-name)
        (.visitInsn Opcodes/DUP)
        (-> (doto (.visitLdcInsn (int 0))
              (-> (.visitInsn Opcodes/ACONST_NULL)
                  (->> (dotimes [_ (dec num-args)]))))
            (->> (when (> num-args 1))))
        (.visitMethodInsn Opcodes/INVOKESPECIAL class-name "<init>" <init>-sig)
        (.visitFieldInsn Opcodes/PUTSTATIC class-name "_datum" +datum-sig+)
        (.visitInsn Opcodes/RETURN)
        (.visitMaxs 0 0)
        (.visitEnd))))

  (defn ^:private compile-lambda [compile *type* ?scope ?closure ?args ?body with-datum? instance?]
    (exec [:let [lambda-class (reduce str "" (interpose "$" (map normalize-ident ?scope)))
                 impl-signature (lambda-impl-signature ?args)
                 <init>-sig (lambda-<init>-signature ?closure ?args)
                 =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                          (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                                  lambda-class nil "java/lang/Object" (into-array [(str +prefix+ "/Function")]))
                          (-> (doto (.visitField (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) captured-name clo-field-sig nil nil)
                                (.visitEnd))
                              (->> (let [captured-name (str "__" ?captured-id)])
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

(defn ^:private compile-def [compile *type* ?name ?value]
  (exec [_ (match (:form ?value)
             [::&analyser/lambda ?scope ?captured ?args ?body]
             (compile-lambda compile *type* ?scope ?closure ?args ?body true false)

             _
             (compile-field compile ?name ?value))]
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
                           :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD variant-class* (str "_" (inc ?tfield)) "Ljava/lang/Object;")]]
                      (return ret)))
                  (map vector (range (count ?members)) ?members))]
    (return nil)))

(let [+int-class+ (->class "java.lang.Integer")]
  (do-template [<name> <opcode>]
    (defn <name> [compile *type* ?x ?y]
      (exec [*writer* &util/get-writer
             _ (compile ?x)
             :let [_ (doto *writer*
                       (.visitTypeInsn Opcodes/CHECKCAST +int-class+)
                       (.visitMethodInsn Opcodes/INVOKEVIRTUAL +int-class+ "intValue" "()I"))]
             _ (compile ?y)
             :let [_ (doto *writer*
                       (.visitTypeInsn Opcodes/CHECKCAST +int-class+)
                       (.visitMethodInsn Opcodes/INVOKEVIRTUAL +int-class+ "intValue" "()I"))
                   _ (doto *writer*
                       (.visitInsn <opcode>)
                       (.visitMethodInsn Opcodes/INVOKESTATIC +int-class+ "valueOf" (str "(I)" (->type-signature "java.lang.Integer"))))]]
        (return nil)))

    ^:private compile-jvm-iadd Opcodes/IADD
    ^:private compile-jvm-isub Opcodes/ISUB
    ^:private compile-jvm-imul Opcodes/IMUL
    ^:private compile-jvm-idiv Opcodes/IDIV
    ^:private compile-jvm-irem Opcodes/IREM
    ))

(defn compile-self-call [compile ?assumed-args]
  (exec [*writer* &util/get-writer
         :let [_ (.visitVarInsn *writer* Opcodes/ALOAD 0)]
         _ (map-m (fn [arg]
                    (exec [ret (compile arg)
                           :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (str +prefix+ "/Function") "apply" +apply-signature+)]]
                      (return ret)))
                  ?assumed-args)]
    (return nil)))

(defn ^:private compile [syntax]
  (match (:form syntax)
    [::&analyser/bool ?value]
    (compile-bool compile (:type syntax) ?value)

    [::&analyser/int ?value]
    (compile-int compile (:type syntax) ?value)

    [::&analyser/real ?value]
    (compile-real compile (:type syntax) ?value)

    [::&analyser/char ?value]
    (compile-char compile (:type syntax) ?value)

    [::&analyser/text ?value]
    (compile-text compile (:type syntax) ?value)

    [::&analyser/tuple ?elems]
    (compile-tuple compile (:type syntax) ?elems)

    [::&analyser/local ?idx]
    (compile-local compile (:type syntax) ?idx)

    [::&analyser/captured ?scope ?captured-id ?source]
    (compile-captured compile (:type syntax) ?scope ?captured-id ?source)

    [::&analyser/global ?owner-class ?name]
    (compile-global compile (:type syntax) ?owner-class ?name)

    [::&analyser/call ?fn ?args]
    (compile-call compile (:type syntax) ?fn ?args)

    [::&analyser/static-call ?needs-num ?fn ?args]
    (compile-static-call compile (:type syntax) ?needs-num ?fn ?args)

    [::&analyser/variant ?tag ?members]
    (compile-variant compile (:type syntax) ?tag ?members)

    [::&analyser/let ?idx ?value ?body]
    (compile-let compile (:type syntax) ?idx ?value ?body)

    [::&analyser/case ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree]
    (compile-case compile (:type syntax) ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree)

    [::&analyser/lambda ?scope ?frame ?args ?body]
    (compile-lambda compile (:type syntax) ?scope ?frame ?args ?body false true)

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

    [::&analyser/jvm-getstatic ?class ?field]
    (compile-jvm-getstatic compile (:type syntax) ?class ?field)

    [::&analyser/jvm-getfield ?class ?field ?object]
    (compile-jvm-getfield compile (:type syntax) ?class ?field ?object)
    
    [::&analyser/jvm-invokestatic ?class ?method ?classes ?args]
    (compile-jvm-invokestatic compile (:type syntax) ?class ?method ?classes ?args)

    [::&analyser/jvm-invokevirtual ?class ?method ?classes ?object ?args]
    (compile-jvm-invokevirtual compile (:type syntax) ?class ?method ?classes ?object ?args)

    [::&analyser/jvm-new-array ?class ?length]
    (compile-jvm-new-array compile (:type syntax) ?class ?length)

    [::&analyser/jvm-aastore ?array ?idx ?elem]
    (compile-jvm-aastore compile (:type syntax) ?array ?idx ?elem)

    [::&analyser/jvm-aaload ?array ?idx]
    (compile-jvm-aaload compile (:type syntax) ?array ?idx)

    [::&analyser/jvm-interface [?package ?name] ?members]
    (compile-jvm-interface compile (:type syntax) ?package ?name ?members)

    [::&analyser/jvm-class [?package ?name] ?super-class ?members]
    (compile-jvm-class compile (:type syntax) ?package ?name ?super-class ?members)

    [::&analyser/self ?assumed-args]
    (compile-self-call compile ?assumed-args)
    ))

;; [Interface]
(let [compiler-step (exec [analysis+ &analyser/analyse]
                      (map-m compile analysis+))]
  (defn compile-module [name]
    (exec [loader &util/loader]
      (fn [state]
        (if (-> state ::&util/modules (contains? name))
          (fail "[Compiler Error] Can't redefine a module!")
          (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 (->class name) nil "java/lang/Object" nil))]
            (match ((repeat-m compiler-step) (assoc state
                                               ::&util/source (slurp (str "source/" name ".lux"))
                                               ::&util/current-module name
                                               ::&util/writer =class))
              [::&util/ok [?state ?forms]]
              (if (empty? (::&util/source ?state))
                (do (.visitEnd =class)
                  ((save-class! name (.toByteArray =class)) ?state))
                (assert false (str "[Compiler Error] Can't compile: " (::&util/source ?state))))
              
              [::&util/failure ?message]
              (fail* ?message))))))))

(defn compile-all [modules]
  (.mkdir (java.io.File. "output"))
  (let [state (&util/init-state)]
    (match ((map-m compile-module modules) state)
      [::&util/ok [?state ?forms]]
      (println (str "Compilation complete! " (pr-str modules)))

      [::&util/failure ?message]
      (assert false ?message))))

(comment
  (compile-all ["lux"])
  )
