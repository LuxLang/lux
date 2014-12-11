(ns lang.analyser
  (:refer-clojure :exclude [resolve])
  (:require [clojure.core.match :refer [match]]
            (lang [util :as &util :refer [exec return* return fail fail*
                                          repeat-m try-m try-all-m map-m
                                          apply-m within]]
                  [parser :as &parser]
                  [type :as &type])))

(declare analyse-form)

;; [Util]
(defn ^:private with-env [env monad]
  (fn [state]
    (let [=return (monad (update-in state [:env] merge env))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(assoc ?state :env (:env state)) ?value]]
        
        _
        =return))))

(def ^:private module-name
  (fn [state]
    [::&util/ok [state (:name state)]]))

(defn ^:private resolve [ident]
  (fn [state]
    (if-let [=ident (get-in state [:env ident])]
      [::&util/ok [state =ident]]
      [::&util/failure (str "Unresolved identifier: " ident)])))

(defn ^:private define [name desc]
  (fn [state]
    [::&util/ok [(assoc-in state [:defs (:name state) name] desc) nil]]))

(defn ^:private annotated [form type]
  {:form form
   :type type})

(defmacro ^:private defanalyser [name match return]
  `(def ~name
     (fn [{[token# & left#] :forms :as state#}]
       (match token#
         ~match
         (~return (assoc state# :forms left#))
         _#
         (fail* (str "Unmatched token: " token#))))))

(defn analyse-form* [form]
  (fn [state]
    (let [old-forms (:forms state)
          =return (analyse-form (assoc state :forms (list form)))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(assoc ?state :forms old-forms) ?value]]
        
        _
        =return))))

(defanalyser analyse-boolean
  [::&parser/boolean ?boolean]
  (return (annotated [::literal ?boolean] [::&type/object "java.lang.Boolean" []])))

(defanalyser analyse-string
  [::&parser/string ?string]
  (return (annotated [::literal ?string] [::&type/object "java.lang.String" []])))

(defanalyser analyse-variant
  [::&parser/tagged ?tag ?value]
  (exec [=value (analyse-form* ?value)]
    (return (annotated [::variant ?tag =value] [::&type/variant ?tag (:type =value)]))))

(defanalyser analyse-ident
  [::&parser/ident ?name]
  (exec [=ident (resolve ?name)]
    (return (annotated [::ident ?name] =ident))))

(defanalyser analyse-ann-class
  [::&parser/ann-class ?class ?members]
  (return (annotated [::ann-class ?class ?members] ::&type/nothing)))

(defanalyser analyse-static-access
  [::&parser/static-access ?class ?member]
  (return (annotated [::static-access ?class ?member] ::&type/nothing)))

(defanalyser analyse-dynamic-access
  [::&parser/dynamic-access ?object ?member]
  (exec [=object (analyse-form* ?object)]
    (match ?member
      [::&parser/fn-call [::&parser/ident ?method] ?args]
      (exec [=args (map-m analyse-form* ?args)]
        (return (annotated [::dynamic-access =object [?method =args]] ::&type/nothing))))))

(defanalyser analyse-fn-call
  [::&parser/fn-call [::&parser/ident ?fn] ?args]
  (exec [name module-name
         =args (map-m analyse-form* ?args)]
    (return (annotated [::call [name ?fn] =args] ::&type/nothing))))

(defanalyser analyse-if
  [::&parser/if ?test ?then ?else]
  (exec [=test (analyse-form* ?test)
         =then (analyse-form* ?then)
         =else (analyse-form* ?else)]
    (return (annotated [::if =test =then =else] ::&type/nothing))))

(defanalyser analyse-defclass
  [::&parser/defclass ?name ?fields]
  (let [=members {:fields (into {} (for [[class field] ?fields]
                                     [field {:access ::public
                                             :type class}]))}
        =class [::class ?name =members]]
    (exec [name module-name]
      (return (annotated [::defclass [name ?name] =members] ::&type/nothing)))))

(defanalyser analyse-definterface
  [::&parser/definterface ?name ?members]
  (let [=members {:methods (into {} (for [[method [inputs output]] ?members]
                                      [method {:access ::public
                                               :type [inputs output]}]))}
        =interface [::interface ?name =members]]
    (exec [name module-name]
      (return (annotated [::definterface [name ?name] =members] ::&type/nothing)))))

(defanalyser analyse-def
  [::&parser/def ?usage ?value]
  (match ?usage
    [::&parser/ident ?name]
    (exec [=value (analyse-form* ?value)
           _ (define ?name {:mode ::constant
                            :access ::public
                            :type =value})]
      (return (annotated [::def ?name =value] ::&type/nothing)))

    [::&parser/fn-call [::&parser/ident ?name] ?args]
    (let [args (for [a ?args]
                 (match a
                   [::&parser/ident ?ident]
                   ?ident))]
      (exec [[=function =args =return] (within :types (&type/fresh-function (count args)))
             :let [_ (prn '[=function =args =return] [=function =args =return])]
             :let [env (-> {}
                           (assoc ?name =function)
                           (into (map vector args =args)))
                   _ (prn 'env env)]
             =value (with-env env
                      (analyse-form* ?value))
             :let [_ (prn '=value =value)]
             =function (within :types (exec [_ (&type/solve =return (:type =value))]
                                        (&type/clean =function)))
             :let [_ (prn '=function =function)]
             _ (define ?name {:mode ::function
                              :access ::public
                              :type =function})]
        (return (annotated [::def [?name args] =value] ::&type/nothing))))
    ))

(defanalyser analyse-module
  [::&parser/module]
  (exec [name module-name]
    (return (annotated [::module name] ::&type/nothing))))

(def ^:private analyse-form
  (try-all-m [analyse-boolean
              analyse-string
              analyse-variant
              analyse-ident
              analyse-ann-class
              analyse-static-access
              analyse-dynamic-access
              analyse-fn-call
              analyse-if
              analyse-defclass
              analyse-definterface
              analyse-def
              analyse-module]))

;; [Interface]
(defn analyse [module-name tokens]
  (match ((repeat-m analyse-form) {:name module-name,
                                   :forms tokens
                                   :env {}
                                   :types &type/+init+})
    [::&util/ok [?state ?forms]]
    (if (empty? (:forms ?state))
      ?forms
      (assert false (str "Unconsumed input: " (pr-str (:forms ?state)))))
    
    [::&util/failure ?message]
    (assert false ?message)))
