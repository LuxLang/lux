(ns lang.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]
            (lang [util :as &util :refer [exec return* return fail fail*
                                          repeat-m try-m try-all-m map-m
                                          apply-m]]
                  [parser :as &parser]
                  [lexer :as &lexer]
                  [type :as &type])
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

(declare compile-form)

;; [Utils]
(def ^:private +state+
  {:globals {}
   :stack {}
   :forms '()
   :classes {}})

(defn define-class [class members]
  (fn [state]
    (return* (assoc-in state [:classes class] members) nil)))

(defn find-class [class]
  (fn [state]
    (if-let [class-data (get-in state [:classes class])]
      (return* state class-data)
      (fail* (str "Unknown class: " class)))))

(defn wrap [x]
  (update-in +state+ [:forms] conj x))

(defn wrap-in [state x]
  (assoc-in state [:forms] (list x)))

(defn wrap* [env x]
  (-> +state+
      (update-in [:stack] merge env)
      (update-in [:forms] conj x)))

(defmacro ^:private defcompiler [name match return]
  `(def ~name
     (fn [state#]
       (let [~'*token* (first (:forms state#))]
         ;; (prn '~name ~'*token*)
         (match ~'*token*
           ~match
           (let [output# (~return (update-in state# [:forms] rest))]
             ;; (prn "output#" output#)
             output#)
           _#
           (fail* (str "Unknown syntax: " (pr-str ~'*token*))))))))

(defn unwrap-ident [ident]
  (match ident
    [::&parser/ident ?label]
    ?label))

(defn unwrap-tagged [ident]
  (match ident
    [::&parser/tagged ?tag ?data]
    [?tag ?data]))

(defcompiler compile-int
  [::&parser/int ?int]
  (return ?int))

(defcompiler compile-float
  [::&parser/float ?float]
  (return ?float))

(defcompiler compile-ident
  [::&parser/ident ?name]
  (return (symbol ?name)))

(defcompiler compile-tuple
  [::&parser/tuple ?elems]
  (exec [=elems (map-m (fn [elem] (apply-m compile-form (wrap elem)))
                       ?elems)]
    (return (vec =elems))))

(defcompiler compile-record
  [::&parser/record ?kvs]
  (exec [=kvs (map-m (fn [[?label ?value]]
                       (exec [=value (apply-m compile-form (wrap ?value))]
                         (return [?label =value])))
                     ?kvs)]
    (return (into {} =kvs))))

(defcompiler compile-tagged
  [::&parser/tagged ?tag ?data]
  (exec [=data (apply-m compile-form (wrap ?data))]
    (return {:tag ?tag :data =data})))

(defcompiler compile-fn-call
  [::&parser/fn-call ?fn ?args]
  (exec [=fn (apply-m compile-form (wrap ?fn))
         =args (map-m (fn [arg] (apply-m compile-form (wrap arg)))
                      ?args)]
    (return (reduce (fn [f a] `(~f ~a))
                    =fn =args))))

(defcompiler compile-if
  [::&parser/if ?test ?then ?else]
  (exec [=test (apply-m compile-form (wrap ?test))
         =then (apply-m compile-form (wrap ?then))
         =else (apply-m compile-form (wrap ?else))]
    (return `(if ~=test ~=then ~=else))))

(defcompiler compile-case-branch
  [::&parser/case-branch [::&parser/tagged ?tag [::&parser/tuple ?bindings]] ?expr]
  (exec [:let [=bindings (map (comp symbol unwrap-ident) ?bindings)
               fn-env (into {} (for [a =bindings] [a nil]))]
         =expr (apply-m compile-form (wrap* fn-env ?expr))]
    (return [?tag =bindings =expr])))

(defcompiler compile-let-binding
  [::&parser/let-binding [::&parser/ident ?name] ?expr]
  (exec [=expr (apply-m compile-form (wrap ?expr))]
    (return [(symbol ?name) =expr])))

(defcompiler compile-case
  [::&parser/case ?variant ?branches]
  (exec [=variant (apply-m compile-form (wrap ?variant))
         =branches (map-m #(apply-m compile-case-branch (wrap %))
                          ?branches)
         :let [g!variant (gensym "variant")
               =case `(let [~g!variant ~=variant]
                        (case (:tag ~g!variant)
                          ~@(apply concat (for [[tag bindings expr] =branches]
                                            [tag `(let [~(vec bindings) (:data ~g!variant)]
                                                    ~expr)]))))
               ;; _ (prn '=case =case)
               ]]
    (return =case)))

(defcompiler compile-let
  [::&parser/let ?bindings ?expr]
  (exec [=expr (apply-m compile-form (wrap ?expr))
         =bindings (map-m #(apply-m compile-let-binding (wrap %))
                          ?bindings)
         :let [;; _ (prn '=bindings =bindings)
               =let (reduce (fn [inner [?name ?expr]]
                              `(let [~?name ~?expr]
                                 ~inner))
                            =expr
                            =bindings)
               ;; _ (prn '=let =let)
               ]]
    (return =let)))

(defcompiler compile-def
  [::&parser/def ?form ?body]
  (match ?form
    [::&parser/fn-call [::&parser/ident ?name] ?args]
    (exec [:let [=name (symbol ?name)
                 =args (map (comp symbol unwrap-ident) ?args)
                 fn-env (into {} (for [a =args] [a nil]))]
           =body (apply-m compile-form (wrap* fn-env ?body))
           :let [curled-body (reduce (fn [inner arg] `(fn [~arg] ~inner))
                                     =body (reverse =args))
                 ;; _ (prn 'curled-body curled-body)
                 fn-def (let [[_ ?arg ?body] curled-body]
                          `(fn ~=name ~?arg ~?body))
                 ;; _ (prn 'fn-def fn-def)
                 ]]
      (return fn-def))

    [::&parser/ident ?name]
    (apply-m compile-form (wrap ?body))))

(defcompiler compile-defdata
  [::&parser/defdata ?form ?cases]
  (match ?form
    [::&parser/fn-call ?name ?args]
    (let [=name (unwrap-ident ?name)
          ;; _ (prn '=name =name)
          =args (map unwrap-ident ?args)
          ;; _ (prn '=args =args)
          =cases (map unwrap-tagged ?cases)
          ;; _ (prn '=cases =cases)
          ]
      (return `(fn ~(symbol =name) ~(mapv symbol =args))))))

;; (def compile-form
;;   (try-all-m [compile-int
;;               compile-float
;;               compile-ident
;;               compile-tuple
;;               compile-record
;;               compile-tagged
;;               compile-if
;;               compile-case
;;               compile-let
;;               compile-def
;;               compile-defdata
;;               compile-fn-call]))

;; (defn compile [inputs]
;;   (assert false)
;;   (match ((repeat-m compile-form) inputs)
;;     [::&util/ok [?state ?forms]]
;;     (if (empty? (:forms ?state))
;;       ?forms
;;       (assert false (str "Unconsumed input: " ?state)))

;;     [::&util/failure ?message]
;;     (assert false ?message)))

(def ^:dynamic *code*)

(defcompiler compile-boolean
  [::&parser/boolean ?boolean]
  (do (if ?boolean
        (.visitLdcInsn *code* (int 1))
        (.visitLdcInsn *code* (int 0)))
    (return nil)))

(defcompiler compile-string
  [::&parser/string ?string]
  (do (doto *code*
        (.visitLdcInsn ?string))
    (return nil)))

(defn ->java-class [class]
  (string/replace class #"\." "/"))

(defn ->java-class* [class]
  (case class
    "Void" "V"
    ;; else
    (str "L" (->java-class class) ";")))

(defn method->signature [method]
  (match method
    [::&type/fn ?args ?return]
    (str "(" (reduce str "" (map ->java-class* ?args)) ")" (->java-class* ?return))))

(defcompiler compile-static-access
  [::&parser/static-access ?class ?member]
  (exec [=class (find-class ?class)
         :let [member-type (get-in =class [:fields ?member])
               ?field-class (match member-type
                              [::&type/object ?field-class _]
                              ?field-class)]]
    (do (doto *code*
          (.visitFieldInsn Opcodes/GETSTATIC (->java-class ?class) ?member (->java-class* ?field-class)))
      (return member-type))))

(defcompiler compile-dynamic-access
  [::&parser/dynamic-access ?object ?access]
  (exec [_state &util/get-state
         =object (apply-m compile-form (wrap-in _state ?object))
         :let [?oclass (match =object
                         [::&type/object ?oclass _]
                         ?oclass)]
         =class (find-class ?oclass)
         [method signature] (match ?access
                              [::&parser/fn-call [::&parser/ident ?method] ?args]
                              (exec [=args (map-m #(apply-m compile-form (wrap %))
                                                  ?args)]
                                (return [?method (method->signature (get-in =class [:methods ?method]))])))]
    (do (doto *code*
          (.visitMethodInsn Opcodes/INVOKEVIRTUAL (->java-class ?oclass) method signature))
      (return nil))))

(defcompiler compile-ann-class
  [::&parser/ann-class ?class ?members]
  (exec [_ (define-class ?class ?members)
         _state &util/get-state]
    (return nil)))

(defcompiler compile-if
  [::&parser/if ?test ?then ?else]
  (exec [_state &util/get-state
         =test (apply-m compile-form (wrap-in _state ?test))
         :let [else-label (new Label)
               end-label (new Label)]
         =then (do (doto *code*
                     (.visitJumpInsn Opcodes/IFEQ else-label))
                 (apply-m compile-form (wrap-in _state ?then)))
         :let [_ (doto *code*
                   (.visitJumpInsn Opcodes/GOTO end-label)
                   (.visitLabel else-label))]
         =else (apply-m compile-form (wrap-in _state ?else))]
    (do (doto *code*
          (.visitLabel end-label))
      (return nil))))

(def compile-form
  (try-all-m [compile-boolean
              compile-string
              compile-static-access
              compile-dynamic-access
              compile-ann-class
              compile-if]))

(defn compile [inputs]
  (let [cw (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
             (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                     "output" nil "java/lang/Object" nil))]
    (doto (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)
      (.visitCode)
      (.visitVarInsn Opcodes/ALOAD 0)
      (.visitMethodInsn Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
      (.visitInsn Opcodes/RETURN)
      (.visitMaxs 0 0)
      (.visitEnd))
    (let [_main_ (doto (.visitMethod cw (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "main" "([Ljava/lang/String;)V" nil nil)
                   (.visitCode)
                   ;; (.visitFieldInsn Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;")
                   ;; (.visitLdcInsn "Hello, World!")
                   ;; (.visitMethodInsn Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "println" "(Ljava/lang/String;)V")
                   )]
      (binding [*code* _main_]
        (match ((repeat-m compile-form) inputs)
          [::&util/ok [?state ?forms]]
          (if (empty? (:forms ?state))
            ?forms
            (assert false (str "Unconsumed input: " ?state)))
          
          [::&util/failure ?message]
          (assert false ?message)))
      (doto _main_
        (.visitInsn Opcodes/RETURN)
        (.visitMaxs 0 0)
        (.visitEnd)))
    (.visitEnd cw)
    (.toByteArray cw)))
