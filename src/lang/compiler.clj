(ns lang.compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.core.match :refer [match]]
            (lang [util :as &util :refer [exec return* return fail fail*
                                          repeat-m try-m try-all-m map-m
                                          apply-m]]
                  [parser :as &parser]
                  [lexer :as &lexer])
            :reload))

(declare compile-form)

;; [Utils]
(def ^:private +state+
  {:globals {}
   :stack {}
   :forms '()})

(defn wrap [x]
  (update-in +state+ [:forms] conj x))

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

(defcompiler compile-ident
  [::&parser/ident ?name]
  (return (symbol ?name)))

(defcompiler compile-tuple
  [::&parser/tuple ?elems]
  (exec [=elems (map-m (fn [elem] (apply-m compile-form (wrap elem)))
                       ?elems)]
    (return (vec =elems))))

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

(defcompiler compile-def
  [::&parser/def ?form ?body]
  (match ?form
    [::&parser/fn-call ?name ?args]
    (exec [:let [=name (symbol (unwrap-ident ?name))
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
      (return fn-def))))

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

(def compile-form
  (try-all-m [compile-int
              compile-ident
              compile-tuple
              compile-tagged
              compile-if
              compile-case
              compile-def
              compile-defdata
              compile-fn-call]))

(defn compile [inputs]
  (match ((repeat-m compile-form) inputs)
    [::&util/ok [?state ?forms]]
    (if (empty? (:forms ?state))
      ?forms
      (assert false (str "Unconsumed input: " ?state)))
    
    [::&util/failure ?message]
    (assert false ?message)))

(comment
  
  )
