(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-m try-all-m map-m reduce-m
                                         apply-m within
                                         normalize-ident
                                         loader]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser])
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

(declare compile-form
         compile)

;; [Utils/General]
(defn ^:private write-file [file data]
  ;; (println 'write-file file (alength data))
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data))
  ;; (Thread/sleep 2000)
  )

(let [;; loader (proxy [ClassLoader] [])
      ]
  (defn load-class! [name file-name]
    ;; (println "Defining..." name "@" file-name ;; (alength bytecode)
    ;;          )
    ;; (prn 'loader loader)
    (.loadClass loader name)
    ;; (println "SUCCESFUL LOAD!")
    ;; (.defineClass loader name bytecode 0 (alength bytecode))
    ))

(def ^:private +variant-class+ "test2.Variant")
(def ^:private +tuple-class+ "test2.Tuple")

(defmacro ^:private defcompiler [name match body]
  `(defn ~name [~'*state*]
     (let [~'*class-name* (:class-name ~'*state*)
           ~'*writer* (:writer ~'*state*)
           ~'*parent* (:parent ~'*state*)
           ~'*type* (:type (:form ~'*state*))]
       ;; (prn '~name (:form (:form ~'*state*)))
       (match (:form (:form ~'*state*))
         ~match
         (do ~body
           true)
         _#
         false))))

(defn ^:private unwrap-ident [ident]
  (match ident
    [::&parser/ident ?label]
    ?label))

(defn ^:private ->class [class]
  (string/replace class #"\." "/"))

(def ^:private ->package ->class)

(defn ^:private ->type-signature [class]
  (case class
    "Void" "V"
    ;; else
    (str "L" (->class class) ";")))

(defn ^:private ->java-sig [type]
  (match type
    ::&type/nothing
    "V"
    
    ::&type/any
    (->java-sig [::&type/object "java.lang.Object" []])

    [::&type/primitive "boolean"]
    "Z"

    [::&type/object ?name []]
    (->type-signature ?name)

    [::&type/variant ?tag ?value]
    (->type-signature +variant-class+)

    [::&type/function ?args ?return]
    (->java-sig [::&type/object "test2/Function" []])))

(defn ^:private method->sig [method]
  (match method
    [::&type/function ?args ?return]
    (str "(" (apply str (map ->java-sig ?args)) ")"
         (if (= ::&type/nothing ?return)
           "V"
           (->java-sig ?return)))))

;; [Utils/Compilers]
(defcompiler ^:private compile-literal
  [::&analyser/literal ?literal]
  (cond (instance? java.lang.Integer ?literal)
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
          ;; (.visitLdcInsn *writer* (int 1))
          (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class "java.lang.Boolean") "TRUE" (->type-signature "java.lang.Boolean"))
          ;; (.visitLdcInsn *writer* (int 0))
          (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class "java.lang.Boolean") "FALSE" (->type-signature "java.lang.Boolean")))

        (string? ?literal)
        (.visitLdcInsn *writer* ?literal)

        :else
        (assert false (str "[Unknown literal type] " ?literal " : " (class ?literal)))))

(defcompiler ^:private compile-tuple
  [::&analyser/tuple ?elems]
  (let [;; _ (prn 'compile-tuple (count ?elems))
        num-elems (count ?elems)]
    (let [tuple-class (str "test2/Tuple" num-elems)]
      (doto *writer*
        (.visitTypeInsn Opcodes/NEW tuple-class)
        (.visitInsn Opcodes/DUP)
        (.visitMethodInsn Opcodes/INVOKESPECIAL tuple-class "<init>" "()V"))
      (dotimes [idx num-elems]
        (.visitInsn *writer* Opcodes/DUP)
        (compile-form (assoc *state* :form (nth ?elems idx)))
        (.visitFieldInsn *writer* Opcodes/PUTFIELD tuple-class (str "_" (inc idx)) "Ljava/lang/Object;")))))

(defcompiler ^:private compile-local
  [::&analyser/local ?env ?idx]
  (do ;; (prn 'LOCAL ?idx)
      (doto *writer*
        (.visitVarInsn Opcodes/ALOAD (int ?idx)))))

(defcompiler ^:private compile-captured
  [::&analyser/captured ?scope ?captured-id ?source]
  (do ;; (prn 'CAPTURED [?scope ?captured-id])
      (doto *writer*
        (.visitVarInsn Opcodes/ALOAD 0)
        (.visitFieldInsn Opcodes/GETFIELD (apply str (interpose "$" ?scope)) (str "__" ?captured-id) "Ljava/lang/Object;"))))

(defcompiler ^:private compile-global
  [::&analyser/global ?owner-class ?name]
  (do ;; (prn 'GLOBAL ?owner-class ?name *type*)
      ;; (prn 'compile-global (->class (str ?owner-class "$" ?name)) "_datum")
      (doto *writer*
        (.visitFieldInsn Opcodes/GETSTATIC (->class (str ?owner-class "$" (normalize-ident ?name))) "_datum" "Ljava/lang/Object;" ;; (->java-sig *type*)
                         ))))

(defcompiler ^:private compile-global-fn
  [::&analyser/global-fn ?owner-class ?name]
  (let [fn-class (str ?owner-class "$" (normalize-ident ?name))]
    (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class fn-class) "_datum" (->type-signature fn-class))))

;; (defcompiler ^:private compile-call
;;   [::&analyser/call ?fn ?args]
;;   (do (prn 'compile-call (:form ?fn) ?fn ?args)
;;     (doseq [arg (reverse ?args)]
;;       (compile-form (assoc *state* :form arg)))
;;     (match (:form ?fn)
;;       [::&analyser/global ?owner-class ?fn-name]
;;       (let [signature (str "(" (apply str (repeat (count ?args) "Ljava/lang/Object;")) ")" "Ljava/lang/Object;")]
;;         (doto *writer*
;;           (.visitMethodInsn Opcodes/INVOKESTATIC (->class ?owner-class) ?fn-name signature))))))

(defcompiler ^:private compile-call
  [::&analyser/call ?fn ?args]
  (do ;; (prn 'compile-call (:form ?fn) ?fn ?args)
      (do (compile-form (assoc *state* :form ?fn))
        (let [apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"]
          (doseq [arg ?args]
            (compile-form (assoc *state* :form arg))
            (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE "test2/Function" "apply" apply-signature))))))

(defcompiler ^:private compile-static-call
  [::&analyser/static-call ?needs-num ?fn ?args]
  (do ;; (prn 'compile-call (:form ?fn) ?fn ?args)
      (match (:form ?fn)
        [::&analyser/global-fn ?owner-class ?fn-name]
        (let [arg-sig (->type-signature "java.lang.Object")
              call-class (str (->class ?owner-class) "$" (normalize-ident ?fn-name))
              provides-num (count ?args)]
          (if (>= provides-num ?needs-num)
            (let [apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"
                  impl-sig (str "(" (reduce str "" (repeat ?needs-num arg-sig)) ")" arg-sig)]
              (doto *writer*
                (-> (do (compile-form (assoc *state* :form arg)))
                    (->> (doseq [arg (take ?needs-num ?args)])))
                (.visitMethodInsn Opcodes/INVOKESTATIC call-class "impl" impl-sig)
                (-> (doto (do (compile-form (assoc *state* :form arg)))
                      (.visitMethodInsn Opcodes/INVOKEINTERFACE "test2/Function" "apply" apply-signature))
                    (->> (doseq [arg (drop ?needs-num ?args)])))))
            (let [counter-sig "I"
                  init-signature (str "(" (apply str counter-sig (repeat (dec ?needs-num) arg-sig)) ")" "V")]
              (doto *writer*
                (.visitTypeInsn Opcodes/NEW call-class)
                (.visitInsn Opcodes/DUP)
                (.visitLdcInsn (int provides-num))
                (-> (do (compile-form (assoc *state* :form arg)))
                    (->> (doseq [arg ?args])))
                (-> (.visitInsn Opcodes/ACONST_NULL)
                    (->> (dotimes [_ (dec (- ?needs-num provides-num))])))
                (.visitMethodInsn Opcodes/INVOKESPECIAL call-class "<init>" init-signature)))
            ))
        )))

(defcompiler ^:private compile-jvm-getstatic
  [::&analyser/jvm-getstatic ?owner ?field]
  (do ;; (prn 'compile-static-field ?owner ?field)
      ;; (assert false)
      (doto *writer*
        (.visitFieldInsn Opcodes/GETSTATIC (->class ?owner) ?field (->java-sig *type*)))
    ))

(defcompiler ^:private compile-dynamic-field
  [::&analyser/dynamic-field ?target ?owner ?field]
  (do ;; (prn 'compile-static-field ?owner ?field)
      ;; (assert false)
      (compile-form (assoc *state* :form ?target))
    (doto *writer*
      (.visitFieldInsn Opcodes/GETFIELD (->class ?owner) ?field (->java-sig *type*)))
    ))

(defcompiler ^:private compile-static-method
  [::&analyser/static-method ?owner ?method-name ?method-type ?args]
  (do ;; (prn 'compile-dynamic-access ?target ?owner ?method-name ?method-type ?args)
      ;; (assert false)
      (do (doseq [arg ?args]
            (compile-form (assoc *state* :form arg)))
        (doto *writer*
          (.visitMethodInsn Opcodes/INVOKESTATIC (->class ?owner) ?method-name (method->sig ?method-type))
          (.visitInsn Opcodes/ACONST_NULL)))
    ))

(let [boolean-class "java.lang.Boolean"]
  (defcompiler ^:private compile-jvm-invokevirtual
    [::&analyser/jvm-invokevirtual ?class ?method ?classes ?object ?args]
    (let [_ (prn 'compile-jvm-invokevirtual [?class ?method ?classes] '-> *type*)
          method-sig (str "(" (reduce str "" (map ->type-signature ?classes)) ")" (->java-sig *type*))]
      (compile-form (assoc *state* :form ?object))
      (.visitTypeInsn *writer* Opcodes/CHECKCAST (->class ?class))
      (doseq [[class-name arg] (map vector ?classes ?args)]
        (do (compile-form (assoc *state* :form arg))
          (.visitTypeInsn *writer* Opcodes/CHECKCAST (->class class-name))))
      (.visitMethodInsn *writer* Opcodes/INVOKEVIRTUAL (->class ?class) ?method method-sig)
      (match *type*
        ::&type/nothing
        (.visitInsn *writer* Opcodes/ACONST_NULL)

        [::&type/primitive "boolean"]
        (.visitMethodInsn *writer* Opcodes/INVOKESTATIC (->class boolean-class) "valueOf" (str "(Z)" (->type-signature boolean-class)))
        
        [::&type/object ?oclass _]
        nil)
      )))

(defcompiler ^:private compile-if
  [::&analyser/if ?test ?then ?else]
  (let [else-label (new Label)
        end-label (new Label)]
    ;; (println "PRE")
    (compile-form (assoc *state* :form ?test))
    (doto *writer*
      (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->class "java.lang.Boolean") "booleanValue" "()Z")
      (.visitJumpInsn Opcodes/IFEQ else-label))
    ;; (prn 'compile-if/?then (:form ?then))
    (compile-form (assoc *state* :form ?then))
    ;; (.visitInsn *writer* Opcodes/POP)
    (doto *writer*
      (.visitJumpInsn Opcodes/GOTO end-label)
      (.visitLabel else-label))
    (compile-form (assoc *state* :form ?else))
    ;; (.visitInsn *writer* Opcodes/POP)
    (.visitLabel *writer* end-label)))

(defcompiler ^:private compile-do
  [::&analyser/do ?exprs]
  (do (doseq [expr (butlast ?exprs)]
        (compile-form (assoc *state* :form expr))
        (.visitInsn *writer* Opcodes/POP))
    (compile-form (assoc *state* :form (last ?exprs)))))

(let [+tag-sig+ (->type-signature "java.lang.String")
      variant-class* (->class +variant-class+)
      tuple-class* (->class +tuple-class+)
      oclass (->class "java.lang.Object")
      +variant-field-sig+ (->type-signature "java.lang.Object")
      equals-sig (str "(" (->type-signature "java.lang.Object") ")Z")]
  (defn compile-decision-tree [writer mappings default-label decision-tree]
    ;; (prn 'compile-decision-tree decision-tree)
    (match decision-tree
      [::test-text ?text $body]
      (let [$else (new Label)]
        (doto writer
          ;; object
          (.visitInsn Opcodes/DUP) ;; object, object
          (.visitLdcInsn ?text) ;; object, object, text
          (.visitMethodInsn Opcodes/INVOKEVIRTUAL oclass "equals" equals-sig) ;; object, B
          (.visitJumpInsn Opcodes/IFEQ $else) ;; object
          (.visitInsn Opcodes/POP)
          (.visitJumpInsn Opcodes/GOTO (get mappings $body))
          (.visitLabel $else)
          (.visitInsn Opcodes/POP)
          (.visitJumpInsn Opcodes/GOTO default-label)))

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
                        variant-class** (str variant-class* arity)
                        ;; _ (prn ?tag arity variant-class**)
                        ]
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
                         ;; :let [_ (.print System/out (prn-str 'COMPILE-PATTERN ?tag ?subcases))]
                         :let [tag-else-label (new Label)]])))
        ;; variant, tag ->
        (.visitInsn Opcodes/POP) ;; variant ->
        (.visitInsn Opcodes/POP) ;; ->
        (.visitJumpInsn Opcodes/GOTO default-label)))
    ))

(defn sequence-parts [branches parts]
  ;; (.print System/out (prn-str 'sequence-parts branches parts))
  (if (empty? parts)
    '(())
    (let [[head & tail] parts
          expanded (case (:type head)
                     ::&analyser/defaults
                     (for [[?local ?supports] (:stores head)
                           ?body (set/intersection branches ?supports)
                           ;; :when (set/subset? branches ?supports)
                           ]
                       [[::store ?local ?body] #{?body}])

                     ::&analyser/text-tests
                     (concat (for [[?text ?supports] (:patterns head)
                                   ?body (set/intersection branches ?supports)
                                   ;; :when (set/subset? branches ?supports)
                                   ]
                               [[::test-text ?text ?body] #{?body}])
                             (for [[_ ?local ?body] (:defaults head)
                                   :when (contains? branches ?body)]
                               [[::store ?local ?body] #{?body}]))

                     ::&analyser/tuple*
                     (concat (let [patterns (into {} (for [[?tag ?struct] (:patterns head)
                                                           ;; :let [_ (.print System/out (prn-str 'PATTERN ?tag ?struct))]
                                                           :let [?parts (:parts ?struct)
                                                                 num-parts (count ?parts)
                                                                 ?supports (:branches ?struct)
                                                                 subcases (for [?body (set/intersection branches ?supports)
                                                                                subseq (sequence-parts #{?body} ?parts)
                                                                                ;; :let [_ (when (= "Symbol" ?tag)
                                                                                ;;           (.print System/out (prn-str 'counting ?tag num-parts (count subseq) subseq)))]
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
                     (do ;; (prn '(:default head) (:default head))
                         ;; (assert (nil? (:default head)))
                         ;; (let [patterns (into {} (for [[?tag ?struct] (:patterns head)
                         ;;                               ;; :let [_ (.print System/out (prn-str 'PATTERN ?tag ?struct))]
                         ;;                               :let [?parts (:parts ?struct)
                         ;;                                     num-parts (count ?parts)
                         ;;                                     ?supports (:branches ?struct)
                         ;;                                     subcases (for [?body (set/intersection branches ?supports)
                         ;;                                                    subseq (sequence-parts #{?body} ?parts)
                         ;;                                                    ;; :let [_ (when (= "Symbol" ?tag)
                         ;;                                                    ;;           (.print System/out (prn-str 'counting ?tag num-parts (count subseq) subseq)))]
                         ;;                                                    :when (= num-parts (count subseq))]
                         ;;                                                [::subcase ?body subseq])]
                         ;;                               :when (not (empty? subcases))]
                         ;;                           [?tag subcases]))]
                         ;;   (if (empty? patterns)
                         ;;     '()
                         ;;     (list [[::test-adt branches patterns]
                         ;;            branches])))
                         (concat (let [patterns (into {} (for [[?tag ?struct] (:patterns head)
                                                               ;; :let [_ (.print System/out (prn-str 'PATTERN ?tag ?struct))]
                                                               :let [?parts (:parts ?struct)
                                                                     num-parts (count ?parts)
                                                                     ?supports (:branches ?struct)
                                                                     subcases (for [?body (set/intersection branches ?supports)
                                                                                    subseq (sequence-parts #{?body} ?parts)
                                                                                    ;; :let [_ (when (= "Symbol" ?tag)
                                                                                    ;;           (.print System/out (prn-str 'counting ?tag num-parts (count subseq) subseq)))]
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
                       )
                     )]
      (for [[step branches*] expanded
            tail* (sequence-parts branches* tail)
            ;; :let [_ (.print System/out (prn-str 'tail* tail*))]
            ]
        (cons step tail*)))))

(def !case-vars (atom -1))

(let [oclass (->class "java.lang.Object")
      equals-sig (str "(" (->type-signature "java.lang.Object") ")Z")
      ex-class (->class "java.lang.IllegalStateException")]
  (defcompiler ^:private compile-case
    ;; [::&analyser/case ?variant ?branches]
    [::&analyser/case ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree]
    (do ;; (prn 'compile-case ?base-idx ?variant ?max-registers ?branch-mappings ?decision-tree)
        ;; (assert false)
        (let [start-label (new Label)
              end-label (new Label)
              ;; default-label (new Label)
              entries (for [[?branch ?body] ?branch-mappings
                            :let [label (new Label)]]
                        [[?branch label]
                         [label ?body]])
              mappings* (into {} (map first entries))]
          (dotimes [idx ?max-registers]
            (.visitLocalVariable *writer* (str "__" (swap! !case-vars inc) "__") (->java-sig ::&type/any) nil start-label end-label (+ ?base-idx (inc idx))))
          (compile-form (assoc *state* :form ?variant))
          (doto *writer*
            (.visitInsn Opcodes/DUP)
            (.visitLabel start-label))
          (let [default-label (new Label)
                ;; default-code (:default ?decision-tree)
                ]
            ;; (prn 'sequence-parts
            ;;      (sequence-parts (:branches ?decision-tree) (list ?decision-tree)))
            (doseq [decision-tree (let [pieces (map first (sequence-parts (:branches ?decision-tree) (list ?decision-tree)))]
                                    (if (:default ?decision-tree)
                                      (butlast pieces)
                                      pieces))]
              (compile-decision-tree *writer* mappings* default-label decision-tree))
            (.visitLabel *writer* default-label)
            (if-let [[_ [_ _ ?idx] ?body] (:default ?decision-tree)]
              (doto *writer*
                (.visitInsn Opcodes/DUP)
                (.visitVarInsn Opcodes/ASTORE ?idx)
                (.visitJumpInsn Opcodes/GOTO (get mappings* ?body)))
              (doto *writer*
                (.visitInsn Opcodes/POP)
                (.visitTypeInsn Opcodes/NEW ex-class)
                (.visitInsn Opcodes/DUP)
                (.visitMethodInsn Opcodes/INVOKESPECIAL ex-class "<init>" "()V")
                (.visitInsn Opcodes/ATHROW)))
            ;; (if default-code
            ;;   ;; (do (prn 'default-code default-code)
            ;;   ;;   (assert false)
            ;;   ;;   ;; (.visitInsn Opcodes/POP) ;; ...
            ;;   ;;   (compile-form (assoc *state* :form default-code))
            ;;   ;;   (.visitJumpInsn *writer* Opcodes/GOTO end-label))
            ;;   (doto *writer*
            ;;     (.visitInsn Opcodes/POP)
            ;;     (.visitTypeInsn Opcodes/NEW ex-class)
            ;;     (.visitInsn Opcodes/DUP)
            ;;     (.visitMethodInsn Opcodes/INVOKESPECIAL ex-class "<init>" "()V")
            ;;     (.visitInsn Opcodes/ATHROW)))
            )
          ;; (compile-decision-tree *state* *writer* mappings* 1 nil (:branches ?decision-tree) ?decision-tree)
          (doseq [[?label ?body] (map second entries)]
            (.visitLabel *writer* ?label)
            (.visitInsn *writer* Opcodes/POP)
            (compile-form (assoc *state* :form ?body))
            (.visitJumpInsn *writer* Opcodes/GOTO end-label))
          (.visitLabel *writer* end-label)
          ))
    ))

(defcompiler ^:private compile-let
  [::&analyser/let ?idx ?label ?value ?body]
  (let [start-label (new Label)
        end-label (new Label)
        ?idx (int ?idx)]
    ;; (prn '(:type ?value) (:type ?value) (->java-sig (:type ?value)))
    (.visitLocalVariable *writer* (normalize-ident ?label) (->java-sig (:type ?value)) nil start-label end-label ?idx)
    (assert (compile-form (assoc *state* :form ?value)) "CAN't COMPILE LET-VALUE")
    (doto *writer*
      (.visitVarInsn Opcodes/ASTORE ?idx)
      (.visitLabel start-label))
    (assert (compile-form (assoc *state* :form ?body)) "CAN't COMPILE LET-BODY")
    (.visitLabel *writer* end-label)))

(defn ^:private compile-method-function [writer class-name fn-name num-args body *state*]
  (let [outer-class (->class class-name)
        clo-field-sig (->type-signature "java.lang.Object")
        counter-sig "I"
        apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"
        real-signature (str "(" (apply str (repeat num-args clo-field-sig)) ")" "Ljava/lang/Object;")
        current-class (str outer-class "$" (normalize-ident fn-name))
        self-sig (->type-signature current-class)
        num-captured (dec num-args)
        init-signature (if (not= 0 num-captured)
                         (str "(" (apply str counter-sig (repeat num-captured clo-field-sig)) ")" "V")
                         (str "()" "V"))]
    (.visitInnerClass writer current-class outer-class nil (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
    (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                   (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                           current-class nil "java/lang/Object" (into-array ["test2/Function"]))
                   (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "_datum" self-sig nil nil)
                   (-> (.visitField (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) "_counter" counter-sig nil nil)
                       (->> (when (not= 0 num-captured))))
                   (.visitEnd))
          =impl (doto (.visitMethod =class (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "impl" real-signature nil nil)
                  (.visitCode)
                  (->> (assoc *state* :form body :writer) compile-form)
                  (.visitInsn Opcodes/ARETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
          =init (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<init>" init-signature nil nil)
                  (.visitCode)
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
                  (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                        (.visitVarInsn Opcodes/ILOAD 1)
                        (.visitFieldInsn Opcodes/PUTFIELD current-class "_counter" counter-sig)
                        (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                              (.visitVarInsn Opcodes/ALOAD (+ clo_idx 2))
                              (.visitFieldInsn Opcodes/PUTFIELD current-class field-name clo-field-sig))
                            (->> (let [field-name (str "_" clo_idx)]
                                   (doto (.visitField =class (+ Opcodes/ACC_PRIVATE Opcodes/ACC_FINAL) field-name clo-field-sig nil nil)
                                     (.visitEnd)))
                                 (dotimes [clo_idx num-captured]))))
                      (->> (when (not= 0 num-captured))))
                  (.visitInsn Opcodes/RETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd))
          =clinit (doto (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
                    (.visitCode)
                    (.visitTypeInsn Opcodes/NEW current-class)
                    (.visitInsn Opcodes/DUP)
                    (-> (doto (.visitLdcInsn (int 0))
                          (-> (.visitInsn Opcodes/ACONST_NULL)
                              (->> (dotimes [_ num-captured]))))
                        (->> (when (> num-captured 0))))
                    (.visitMethodInsn Opcodes/INVOKESPECIAL current-class "<init>" init-signature)
                    (.visitFieldInsn Opcodes/PUTSTATIC current-class "_datum" self-sig)
                    (.visitInsn Opcodes/RETURN)
                    (.visitMaxs 0 0)
                    (.visitEnd))
          =method (let [default-label (new Label)
                        branch-labels (for [_ (range num-captured)]
                                        (new Label))]
                    (doto (.visitMethod =class Opcodes/ACC_PUBLIC "apply" apply-signature nil nil)
                      (.visitCode)
                      (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                            (.visitFieldInsn Opcodes/GETFIELD current-class "_counter" counter-sig)
                            (.visitTableSwitchInsn 0 (dec num-captured) default-label (into-array Label branch-labels))
                            (-> (doto (.visitLabel branch-label)
                                  (.visitTypeInsn Opcodes/NEW current-class)
                                  (.visitInsn Opcodes/DUP)
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
                                  ;; (.visitJumpInsn Opcodes/GOTO end-label)
                                  (.visitInsn Opcodes/ARETURN))
                                (->> (doseq [[branch-label current-captured] (map vector branch-labels (range (count branch-labels)))
                                             ;; :let [_ (prn '[branch-label current-captured] [branch-label current-captured])]
                                             ])))
                            (.visitLabel default-label)
                            (-> (doto (.visitVarInsn Opcodes/ALOAD 0)
                                  (.visitFieldInsn Opcodes/GETFIELD current-class (str "_" clo_idx) clo-field-sig))
                                (->> (dotimes [clo_idx num-captured]))))
                          (->> (when (not= 0 num-captured))))
                      (.visitVarInsn Opcodes/ALOAD 1)
                      (.visitMethodInsn Opcodes/INVOKESTATIC current-class "impl" real-signature)
                      ;; (.visitLabel end-label)
                      (.visitInsn Opcodes/ARETURN)
                      (.visitMaxs 0 0)
                      (.visitEnd)))
          _ (.visitEnd =class)
          bytecode (.toByteArray =class)]
      (write-file (str current-class ".class") bytecode)
      (load-class! (string/replace current-class #"/" ".") (str current-class ".class")))
    ))

(defn compile-field [writer class-name ?name body state]
  (let [outer-class (->class class-name)
        datum-sig (->type-signature "java.lang.Object")
        current-class (str outer-class "$" ?name)]
    (.visitInnerClass writer current-class outer-class nil (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
    (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                   (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                           current-class nil "java/lang/Object" (into-array ["test2/Function"]))
                   (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_datum" datum-sig nil nil)
                       (doto (.visitEnd)))
                   (-> (.visitMethod Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
                       (doto (.visitCode)
                         (->> (assoc state :form body :writer) compile-form)
                         (.visitFieldInsn Opcodes/PUTSTATIC current-class "_datum" datum-sig)
                         (.visitInsn Opcodes/RETURN)
                         (.visitMaxs 0 0)
                         (.visitEnd)))
                   (.visitEnd))
          bytecode (.toByteArray =class)]
      (write-file (str current-class ".class") bytecode)
      (load-class! (string/replace current-class #"/" ".") (str current-class ".class")))
    ))

(defcompiler ^:private compile-def
  [::&analyser/def ?form ?body]
  (do ;; (prn 'compile-def ?form)
      (match ?form
        (?name :guard string?)
        (compile-field *writer* *class-name* ?name ?body *state*)

        [?name ?args]
        (do ;; (prn 'compile-def `(~'def (~(symbol ?name) ~@(map symbol ?args))))
            (if (= "main" ?name)
              (let [signature "([Ljava/lang/String;)V"
                    =method (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) ?name signature nil nil)
                              (.visitCode))]
                ;; (prn 'FN/?body ?body)
                (compile-form (assoc *state* :parent *writer* :writer =method :form ?body))
                (doto =method
                  (.visitInsn Opcodes/RETURN)
                  (.visitMaxs 0 0)
                  (.visitEnd)))
              (compile-method-function *writer* *class-name* ?name (count ?args) ?body *state*)))
        )))

(defn ^:private captured? [form]
  (match form
    [::&analyser/captured ?closure-id ?captured-id ?source]
    true
    _
    false))

(defcompiler ^:private compile-lambda
  [::&analyser/lambda ?scope ?frame ?args ?body]
  (let [_ (prn '[?scope ?frame] ?scope ?frame ?args)
        num-args (count ?args)
        outer-class (->class *class-name*)
        clo-field-sig (->type-signature "java.lang.Object")
        counter-sig "I"
        apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;"
        real-signature (str "(" (apply str (repeat num-args clo-field-sig)) ")" "Ljava/lang/Object;")
        current-class (apply str (interpose "$" ?scope))
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
        ;; _ (prn current-class 'init-signature init-signature)
        ;; _ (prn current-class 'real-signature real-signature)
        =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                 (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                         current-class nil "java/lang/Object" (into-array ["test2/Function"]))
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
                              (->> (doseq [[branch-label current-captured] (map vector branch-labels (range (count branch-labels)))
                                           ;; :let [_ (prn '[branch-label current-captured] [branch-label current-captured])]
                                           ])))
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
        ;; _ (prn 'LAMBDA/?body ?body)
        =impl (doto (.visitMethod =class Opcodes/ACC_PUBLIC "impl" real-signature nil nil)
                (.visitCode)
                (->> (assoc *state* :form ?body :writer)
                     compile-form)
                (.visitInsn Opcodes/ARETURN)
                (.visitMaxs 0 0)
                (.visitEnd))
        _ (.visitEnd =class)
        bytecode (.toByteArray =class)]
    (write-file (str current-class ".class") bytecode)
    (load-class! (string/replace current-class #"/" ".") (str current-class ".class"))
    ;; (apply prn 'LAMBDA ?scope ?args (->> (:mappings ?frame)
    ;;                                      (map second)
    ;;                                      (map :form)
    ;;                                      (filter captured?)))
    (doto *writer*
      (.visitTypeInsn Opcodes/NEW current-class)
      (.visitInsn Opcodes/DUP)
      (-> (do (compile-form (assoc *state* :form ?source)))
          (->> (match (:form ?captured)
                 [::&analyser/captured ?closure-id ?captured-id ?source])
               (doseq [[?name ?captured] (:mappings ?frame)
                       :when (captured? (:form ?captured))])))
      (-> (doto (.visitInsn Opcodes/ICONST_0)
            ;; (.visitInsn Opcodes/ICONST_0)
            (-> (.visitInsn Opcodes/ACONST_NULL)
                (->> (doseq [_ (butlast ?args)]))))
          (->> (when (> (count ?args) 1))))
      (.visitMethodInsn Opcodes/INVOKESPECIAL current-class "<init>" init-signature))
    ))

(defcompiler ^:private compile-defclass
  [::&analyser/defclass [?package ?name] ?super-class ?members]
  (let [parent-dir (->package ?package)
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
    (.mkdirs (java.io.File. parent-dir))
    (write-file (str parent-dir "/" ?name ".class") (.toByteArray =class))
    (load-class! (string/replace (str parent-dir "/" ?name) #"/" ".") (str parent-dir "/" ?name ".class"))))

(defcompiler ^:private compile-definterface
  [::&analyser/definterface [?package ?name] ?members]
  (let [parent-dir (->package ?package)
        =interface (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                     (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_INTERFACE ;; Opcodes/ACC_ABSTRACT
                                             )
                             (str parent-dir "/" ?name) nil "java/lang/Object" nil))]
    (doseq [[?method ?props] (:methods ?members)
            :let [[?args ?return] (:type ?props)
                  signature (str "(" (reduce str "" (map ->type-signature ?args)) ")" (->type-signature ?return))]]
      (.visitMethod =interface (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT) ?method signature nil nil))
    (.visitEnd =interface)
    (.mkdirs (java.io.File. parent-dir))
    (write-file (str parent-dir "/" ?name ".class") (.toByteArray =interface))
    (load-class! (string/replace (str parent-dir "/" ?name) #"/" ".") (str parent-dir "/" ?name ".class"))))

(defcompiler ^:private compile-variant
  [::&analyser/variant ?tag ?members]
  (let [variant-class* (str (->class +variant-class+) (count ?members))]
    ;; (prn 'compile-variant ?tag ?value)
    (doto *writer*
      (.visitTypeInsn Opcodes/NEW variant-class*)
      (.visitInsn Opcodes/DUP)
      (.visitMethodInsn Opcodes/INVOKESPECIAL variant-class* "<init>" "()V")
      (.visitInsn Opcodes/DUP)
      (.visitLdcInsn ?tag)
      (.visitFieldInsn Opcodes/PUTFIELD variant-class* "tag" (->type-signature "java.lang.String"))
      (-> (doto (.visitInsn Opcodes/DUP)
            (do (compile-form (assoc *state* :form ?member)))
            (.visitFieldInsn Opcodes/PUTFIELD variant-class* (str "_" (inc ?tfield)) "Ljava/lang/Object;"))
          (->> (doseq [[?tfield ?member] (mapv vector (range (count ?members)) ?members)]))))
    ))

(defcompiler compile-import
  [::&analyser/import ?class]
  nil)

(defcompiler compile-use
  [::&analyser/use ?file ?alias]
  (let [module-name (re-find #"[^/]+$" ?file)
        ;; _ (prn 'module-name module-name)
        source-code (slurp (str module-name ".lux"))
        ;; _ (prn 'source-code source-code)
        tokens (&lexer/lex source-code)
        ;; _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        ;; _ (prn 'syntax syntax)
        ;; ann-syntax (&analyser/analyse module-name syntax)
        ;; _ (prn 'ann-syntax ann-syntax)
        bytecode (compile module-name syntax)]
    ;; (write-file (str module-name ".class") bytecode)
    ;; (load-class! (string/replace module-name #"/" ".") (str module-name ".class"))
    nil))

(let [+int-class+ (->class "java.lang.Integer")]
  (do-template [<name> <tag> <opcode>]
    (defcompiler <name>
      [<tag> ?x ?y]
      (do (compile-form (assoc *state* :form ?x))
        (doto *writer*
          (.visitTypeInsn Opcodes/CHECKCAST +int-class+)
          (.visitMethodInsn Opcodes/INVOKEVIRTUAL +int-class+ "intValue" "()I"))
        (compile-form (assoc *state* :form ?y))
        (doto *writer*
          (.visitTypeInsn Opcodes/CHECKCAST +int-class+)
          (.visitMethodInsn Opcodes/INVOKEVIRTUAL +int-class+ "intValue" "()I")
          (.visitInsn <opcode>)
          (.visitMethodInsn Opcodes/INVOKESTATIC +int-class+ "valueOf" (str "(I)" (->type-signature "java.lang.Integer"))))))

    ^:private compile-jvm-i+   ::&analyser/jvm-i+   Opcodes/IADD
    ^:private compile-jvm-i-   ::&analyser/jvm-i-   Opcodes/ISUB
    ^:private compile-jvm-i*   ::&analyser/jvm-i*   Opcodes/IMUL
    ^:private compile-jvm-idiv ::&analyser/jvm-idiv Opcodes/IDIV
    ^:private compile-jvm-irem ::&analyser/jvm-irem Opcodes/IREM
    ))

(let [+compilers+ [compile-literal
                   compile-variant
                   compile-tuple
                   compile-local
                   compile-captured
                   compile-global
                   compile-global-fn
                   compile-static-call
                   compile-call
                   compile-dynamic-field
                   compile-static-method
                   compile-if
                   compile-do
                   compile-case
                   compile-let
                   compile-lambda
                   compile-def
                   compile-defclass
                   compile-definterface
                   compile-import
                   compile-use
                   compile-jvm-i+
                   compile-jvm-i-
                   compile-jvm-i*
                   compile-jvm-idiv
                   compile-jvm-irem
                   compile-jvm-getstatic
                   compile-jvm-invokevirtual]]
  (defn ^:private compile-form [state]
    ;; (prn 'compile-form/state state)
    (or (some #(% state) +compilers+)
        (assert false (str "Can't compile: " (pr-str (:form state)))))))

;; [Interface]
(defn compile [class-name inputs]
  ;; (prn 'inputs inputs)
  (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                 (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                         (->class class-name) nil "java/lang/Object" nil))
        compiler-state {:class-name class-name
                        :writer =class
                        :form nil
                        :parent nil}]
    (match ((repeat-m
             (&analyser/with-scope class-name
               (exec [ann-input &analyser/analyse-form
                      :let [_ (when (not (compile-form (assoc compiler-state :form ann-input)))
                                (assert false ann-input))]]
                 (return ann-input))))
            {:name class-name
             :forms inputs
             :deps {}
             :imports {}
             :defs {}
             :defs-env {}
             :lambda-scope [[] 0]
             :env (list (&analyser/fresh-env 0))
             :types &type/+init+})
      [::&util/ok [?state ?forms]]
      (if (empty? (:forms ?state))
        ?forms
        (assert false (str "Unconsumed input: " (pr-str (first (:forms ?state))))))
      
      [::&util/failure ?message]
      (assert false ?message))
    ;;;
    (.visitEnd =class)
    (let [bytecode (.toByteArray =class)]
      (write-file (str class-name ".class") bytecode)
      (load-class! (string/replace class-name #"/" ".") (str class-name ".class"))
      bytecode)
    )
  ;; (comment
  ;;   (-> (java.io.File. "./") .toURL vector into-array java.net.URLClassLoader. (.loadClass "test2"))
  ;;   (-> (java.io.File. "./") .toURL vector into-array java.net.URLClassLoader. (.loadClass "test2.Function"))
  ;;   (let [test2 (-> (java.io.File. "./") .toURL vector into-array java.net.URLClassLoader. (.loadClass "test2"))
  ;;         main (first (.getDeclaredMethods test2))]
  ;;     (.invoke main nil (to-array [nil])))
  ;;   )
  )
