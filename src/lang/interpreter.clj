(ns lang.interpreter
  (:refer-clojure :exclude [eval resolve])
  (:require [clojure.core.match :refer [match]]
            (lang [util :as &util :refer [exec return* return fail fail*
                                          repeat-m try-m try-all-m map-m
                                          apply-m]]
                  [parser :as &parser]
                  [lexer :as &lexer])
            :reload))

(declare eval-form)

;; [Utils]
(def ^:private +state+
  {:globals {"*" (fn [x] (fn [y] (* x y)))}
   :stack {}
   :forms '()})

(defn wrap [x]
  (update-in +state+ [:forms] conj x))

(defn resolve [ident]
  (fn [state]
    (if-let [value (get-in state [:globals ident])]
      (return* state value)
      (fail* (str "Unrecognized identifier: " ident)))))

(defn fn-call [f args]
  (return (reduce #(%1 %2) f args)))

(defmacro ^:private defeval [name match return]
  `(def ~name
     (fn [state#]
       (let [token# (first (:forms state#))]
         ;; (prn '~name token#)
         (match token#
           ~match
           (~return (update-in state# [:forms] rest))
           _#
           (fail* (str "Unknown syntax: " (pr-str token#))))))))

(defeval eval-ident
  [::&parser/ident ?ident]
  (resolve ?ident))

(defeval eval-int
  [::&parser/int ?int]
  (return ?int))

(defeval eval-def
  [::&parser/def ?form ?body]
  (match ?form
    [::&parser/fn-call ?name ?args]
    (exec [=body (apply-m eval-form (wrap ?body))
           =args (map-m (fn [arg] (apply-m eval-form (wrap arg)))
                        ?args)]
      (return `(fn ~(vec =args) ~=body)))))

(defeval eval-fn-call
  [::&parser/fn-call ?fn ?args]
  (exec [=fn (apply-m eval-form (wrap ?fn))
         =args (map-m (fn [arg] (apply-m eval-form (wrap arg)))
                      ?args)]
    (fn-call =fn =args)))

(def eval-form
  (try-all-m [eval-ident
              eval-int
              eval-def
              eval-fn-call]))

;; [::def [::fn-call [::ident "**"] ([::ident "base"] [::ident "exp"])]
;;  [::fn-call [::ident "reduce"] ([::ident "*"]
;;                                 [::int 1]
;;                                 [::fn-call [::ident "repeat"] ([::ident "exp"]
;;                                                                [::ident "base"])])]]

(defn eval [state]
  (match (eval-form state)
    [::&util/ok [?state ?datum]]
    (if (empty? (:forms ?state))
      ?datum
      (assert false (str "Unconsumed input: " ?state)))
    
    [::&util/failure ?message]
    (assert false ?message)))

(comment
  (let [source-code (slurp "src/example/test1.lang")
        tokens (&lexer/lex source-code)
        syntax (&parser/parse (list tokens))]
    ;; (prn 'syntax syntax)
    (eval (update-in +state+ [:forms] concat (list syntax))))

  ;; (clojure.core/fn [base exp] (fold * 1 (repeat exp base)))

  ;; (* 5 6)

  ;; (defdata (List x)
  ;;   (#Nil [])
  ;;   (#Cons [x] (List x)))

  ;; (def (repeat n val)
  ;;   (if (> v n)
  ;;     (#Nil [])
  ;;     (#Cons [val (repeat (- n 1) val)])))

  ;; (def (fold f init inputs)
  ;;   (case input
  ;;     (#Nil _)            init
  ;;     (#Cons [head tail]) (fold f (f init head) tail)))

  ;; (def (** base exp)
  ;;   (fold * 1 (repeat exp base)))
  )
