(ns lang.interpreter
  (:refer-clojure :exclude [eval resolve -' *'])
  (:require [clojure.core.match :refer [match]]
            (lang [util :as &util :refer [exec return* return fail fail*
                                          repeat-m try-m try-all-m map-m
                                          apply-m]]
                  [parser :as &parser]
                  [lexer :as &lexer]
                  [compiler :as &compiler])
            :reload)
  )

(declare eval-form)

;; (defonce _init_
;;   (do (alter-var-root #'clojure.core/prn
;;                       (constantly #(.println System/out (apply pr-str %&))))))

(def <=' (fn [x] (fn [y] (<= x y))))
(def -' (fn [x] (fn [y] (- x y))))

;; [Utils]
(def ^:private +state+
  {:globals {"*" (fn [x] (fn [y] (* x y)))}
   :stack {}
   :forms '()})

(defn wrap [x]
  (update-in +state+ [:forms] conj x))

(defn wrap-in [state x]
  (assoc state :forms (list x)))

(defn resolve [ident]
  (fn [state]
    ;; (prn 'resolve ident (get-in state [:globals ident]) (get-in state [:globals]))
    (if-let [value (get-in state [:globals ident])]
      (return* state value)
      (fail* (str "Unrecognized identifier: " ident)))))

(defn define [name value]
  (fn [state]
    ;; (prn 'define name value (assoc-in state [:globals name] value))
    (return* (assoc-in state [:globals name] value) nil)))

(defn fn-call [f args]
  ;; (prn 'fn-call/call f args (first args) (second args))
  ;; (prn 'fn-call/output* (f (first args)))
  ;; (prn 'fn-call/output* ((f (first args)) (second args)))
  (let [output (reduce #(%1 %2) f args)]
    ;; (prn 'fn-call/output output)
    (return output)))

(defmacro ^:private defeval [name match return]
  `(def ~name
     (fn [state#]
       (let [~'*token* (first (:forms state#))]
         ;; (prn '~name ~'*token*)
         ;; (prn '~name state#)
         (match ~'*token*
           ~match
           (let [output# (~return (update-in state# [:forms] rest))]
             ;; (prn "output#" output#)
             output#)
           _#
           (do ;; (println "Unknown syntax: " (pr-str ~'*token*))
             (fail* (str "Unknown syntax: " (pr-str ~'*token*)))))))))

(defeval eval-ident
  [::&parser/ident ?ident]
  (resolve ?ident))

(defeval eval-int
  [::&parser/int ?int]
  (return ?int))

(defeval eval-float
  [::&parser/float ?float]
  (return ?float))

(defeval eval-def
  [::&parser/def ?form ?body]
  (exec [;; :let [_ (prn 'eval-defdata ?form ?cases)]
         =value (apply-m &compiler/compile-form (wrap *token*))
         ;; :let [_ (prn 'eval-def 'DONE =value)]
         :let [=name (match ?form
                       [::&parser/fn-call [::&parser/ident ?name] ?args]
                       ?name

                       [::&parser/ident ?name]
                       ?name)
               =value* (clojure.core/eval =value)
               ;; _ (prn '=value* =value*)
               ]
         ]
    (define =name =value*)))

(defeval eval-defdata
  [::&parser/defdata ?form & ?cases]
  (exec [;; :let [_ (prn 'eval-defdata ?form ?cases)]
         _ (apply-m &compiler/compile-form (wrap `[::&parser/defdata ~?form ~@?cases]))
         ;; :let [_ (prn 'eval-defdata 'DONE)]
         ]
    (return nil)))

(defeval eval-fn-call
  [::&parser/fn-call ?fn ?args]
  (exec [state &util/get-state
         =fn (apply-m eval-form (wrap-in state ?fn))
         ;; :let [_ (prn '=fn ?fn =fn)]
         =args (map-m (fn [arg] (apply-m eval-form (wrap-in state arg)))
                      ?args)
         ;; :let [_ (prn '=args =args)]
         ]
    (fn-call =fn =args)))

(def eval-form
  (try-all-m [eval-int
              eval-float
              eval-ident
              eval-def
              eval-defdata
              eval-fn-call]))

(defn eval [text]
  (match ((repeat-m eval-form) text)
    [::&util/ok [?state ?forms]]
    (if (empty? (:forms ?state))
      ?forms
      (assert false (str "Unconsumed input: " ?state)))
    
    [::&util/failure ?message]
    (assert false ?message)))

(comment
  (let [source-code (slurp "src/example/test1.lang")
        tokens (&lexer/lex source-code)
        ;; _ (prn 'tokens tokens)
        syntax (&parser/parse tokens)
        ;; _ (prn 'syntax syntax)
        ]
    (eval (update-in +state+ [:forms] concat syntax)))

  
  ;; (defdata (List x)
  ;;   (#Nil [])
  ;;   (#Cons [x] (List x)))
  
  ;; (def (** base exp)
  ;;   (fold * 1 (repeat exp base)))

  ;; Syntax for chars: #"a"
  )
