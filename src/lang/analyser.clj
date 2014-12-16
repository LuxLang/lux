(ns lang.analyser
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]
            (lang [util :as &util :refer [exec return* return fail fail*
                                          repeat-m try-m try-all-m map-m
                                          apply-m within]]
                  [parser :as &parser]
                  [type :as &type])))

(declare analyse-form)

;; [Util]
(defn ^:private annotated [form type]
  {:form form
   :type type})

(def ^:private module-name
  (fn [state]
    [::&util/ok [state (:name state)]]))

(defn ^:private define [name desc]
  (fn [state]
    [::&util/ok [(-> state
                     (assoc-in [:defs (:name state) name] desc)
                     (assoc-in [:env :mappings name] (annotated [::global (:name state) name] (:type desc)))) nil]]))

(def ^:private next-local-idx
  (fn [state]
    [::&util/ok [state (-> state :env :counter)]]))

(defn ^:private with-local [name type body]
  (fn [state]
    (let [=return (body (update-in state [:env] #(-> %
                                                     (update-in [:counter] inc)
                                                     (assoc-in [:mappings name] (annotated [::local (:counter %)] type)))))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(assoc ?state :env (:env state)) ?value]]
        
        _
        =return))))

(defn ^:private with-fresh-env [body]
  (fn [state]
    (let [=return (body (update-in state [:env]
                                   #(-> %
                                        (assoc :counter 0)
                                        (update-in [:mappings] (fn [ms]
                                                                 (let [ms* (into {} (for [[k v] ms
                                                                                          :when (match (:form v)
                                                                                                  [::local _]
                                                                                                  false
                                                                                                  _
                                                                                                  true)]
                                                                                      [k v]))]
                                                                   (prn 'ms ms 'ms* ms*)
                                                                   ms*))))))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(assoc ?state :env (:env state)) ?value]]
        
        _
        =return))))

(defn ^:private import-class [long-name short-name]
  (fn [state]
    (let [=class (annotated [::class long-name] ::&type/nothing)]
      [::&util/ok [(update-in state [:env :mappings] merge {long-name =class,
                                                            short-name =class})
                   nil]])))

(defn ^:private require-module [name alias]
  (fn [state]
    [::&util/ok [(assoc-in state [:deps alias] name)
                 nil]]))

(defn ^:private resolve [ident]
  (fn [state]
    (if-let [[_ ?alias ?binding] (re-find #"^(.*)/(.*)$" ident)]
      (let [?module (get-in state [:deps ?alias])]
        (prn 'resolve ?module ?alias ?binding)
        [::&util/ok [state (annotated [::global ?module ?binding] ::&type/nothing)]])
      (if-let [resolved (get-in state [:env :mappings ident])]
        [::&util/ok [state resolved]]
        [::&util/failure (str "Unresolved identifier: " ident)]))))

(defmacro ^:private defanalyser [name match return]
  `(def ~name
     (fn [{[token# & left#] :forms :as state#}]
       (match token#
         ~match
         (~return (assoc state# :forms left#))
         _#
         (fail* (str "Unmatched token: " token#))))))

(defn analyse-form* [form]
  (prn 'analyse-form* form)
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

(defanalyser analyse-tuple
  [::&parser/tuple ?elems]
  (exec [=elems (map-m analyse-form* ?elems)]
    (return (annotated [::tuple =elems] [::&type/tuple (mapv :type =elems)]))))

(defanalyser analyse-ident
  [::&parser/ident ?ident]
  (exec [_env (fn [state] [::&util/ok [state (:env state)]])
         :let [_ (prn 'analyse-ident ?ident _env)]]
    (resolve ?ident)))

(defanalyser analyse-ann-class
  [::&parser/ann-class ?class ?members]
  (return (annotated [::ann-class ?class ?members] ::&type/nothing)))

(defanalyser analyse-static-access
  [::&parser/static-access ?target ?member]
  (exec [=target (resolve ?target)
         :let [_ (prn '=target ?target (:form =target))]]
    (match (:form =target)
      [::class ?class]
      (return (annotated [::static-access ?class ?member] ::&type/nothing)))))

(defanalyser analyse-dynamic-access
  [::&parser/dynamic-access ?object ?member]
  (exec [=object (analyse-form* ?object)]
    (match ?member
      [::&parser/fn-call [::&parser/ident ?method] ?args]
      (exec [=args (map-m analyse-form* ?args)]
        (return (annotated [::dynamic-access =object [?method =args]] ::&type/nothing))))))

(defanalyser analyse-fn-call
  [::&parser/fn-call ?fn ?args]
  (exec [:let [_ (prn 'PRE '?fn ?fn)]
         =fn (analyse-form* ?fn)
         :let [_ (prn '=fn =fn)]
         =args (map-m analyse-form* ?args)
         :let [_ (prn '=args =args)]]
    (return (annotated [::call =fn =args] [::&type/object "java.lang.Object" []]))))

(defanalyser analyse-if
  [::&parser/if ?test ?then ?else]
  (exec [=test (analyse-form* ?test)
         :let [_ (prn '=test =test)]
         :let [_ (prn 'PRE '?then ?then)]
         =then (analyse-form* ?then)
         :let [_ (prn '=then =then)]
         =else (analyse-form* ?else)
         :let [_ (prn '=else =else)]]
    (return (annotated [::if =test =then =else] ::&type/nothing))))

(defanalyser analyse-do
  [::&parser/do ?exprs]
  (exec [=exprs (map-m analyse-form* ?exprs)]
    (return (annotated [::do =exprs] (-> =exprs last :type)))))

(defanalyser analyse-let
  [::&parser/let ?label ?value ?body]
  (exec [=value (analyse-form* ?value)
         idx next-local-idx
         =body (with-local ?label =value
                 (analyse-form* ?body))]
    (return (annotated [::let idx ?label =value =body] (:type =body)))))

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
                            :type (:type =value)})]
      (return (annotated [::def ?name =value] ::&type/nothing)))

    [::&parser/fn-call [::&parser/ident ?name] ?args]
    (let [args (for [a ?args]
                 (match a
                   [::&parser/ident ?ident]
                   ?ident))]
      (exec [[=function =args =return] (within :types (&type/fresh-function (count args)))
             :let [_ (prn '[=function =args =return] [=function =args =return])]
             ;; :let [env (-> {}
             ;;               (assoc ?name =function)
             ;;               (into (map vector args =args)))
             ;;       _ (prn 'env env)]
             =value (reduce (fn [inner [label type]]
                              (with-local label type inner))
                            (analyse-form* ?value)
                            (reverse (map vector args =args)))
             :let [_ (prn '=value =value)]
             =function (within :types (exec [_ (&type/solve =return (:type =value))]
                                        (&type/clean =function)))
             :let [_ (prn '=function =function)]
             _ (define ?name {:mode ::function
                              :access ::public
                              :type =function})]
        (return (annotated [::def [?name args] =value] ::&type/nothing))))
    ))

(defanalyser analyse-lambda
  [::&parser/lambda ?args ?body]
  (exec [:let [_ (prn 'analyse-lambda ?args ?body)]
         [=function =args =return] (within :types (&type/fresh-function (count ?args)))
         :let [_ (prn '[=function =args =return] [=function =args =return])]
         :let [_ (prn 'PRE/?body ?body)]
         _env (fn [state] [::&util/ok [state (:env state)]])
         :let [_ (prn 'analyse-lambda _env)]
         =body (with-fresh-env
                 (reduce (fn [inner [label type]]
                           (with-local label type inner))
                         (analyse-form* ?body)
                         (reverse (map vector ?args =args))))
         :let [_ (prn '=body =body)]
         =function (within :types (exec [_ (&type/solve =return (:type =body))]
                                    (&type/clean =function)))
         :let [_ (prn '=function =function)]]
    (return (annotated [::lambda ?args =body] =function))))

(defanalyser analyse-import
  [::&parser/import ?class]
  (exec [_ (import-class ?class (last (string/split ?class #"\.")))]
    (return (annotated [::import ?class] ::&type/nothing))))

(defanalyser analyse-require
  [::&parser/require ?file ?alias]
  (let [_ (prn `[require ~?file ~?alias])
        module-name (re-find #"[^/]+$" ?file)
        _ (prn 'module-name module-name)]
    (exec [_ (require-module module-name ?alias)]
      (return (annotated [::require ?file ?alias] ::&type/nothing)))))

(def ^:private analyse-form
  (try-all-m [analyse-boolean
              analyse-string
              analyse-variant
              analyse-tuple
              analyse-lambda
              analyse-ident
              analyse-ann-class
              analyse-static-access
              analyse-dynamic-access
              analyse-fn-call
              analyse-if
              analyse-do
              analyse-let
              analyse-defclass
              analyse-definterface
              analyse-def
              analyse-import
              analyse-require]))

;; [Interface]
(defn analyse [module-name tokens]
  (match ((repeat-m analyse-form) {:name module-name,
                                   :forms tokens
                                   :deps {}
                                   :env {:counter 0
                                         :mappings {}}
                                   :types &type/+init+})
    [::&util/ok [?state ?forms]]
    (if (empty? (:forms ?state))
      ?forms
      (assert false (str "Unconsumed input: " (pr-str (:forms ?state)))))
    
    [::&util/failure ?message]
    (assert false ?message)))
