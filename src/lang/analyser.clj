(ns lang.analyser
  (:refer-clojure :exclude [resolve])
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
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

(defn ^:private fresh-env [id]
  {:id id
   :counter 0
   :mappings {}
   :closure/id 0})

(def ^:private module-name
  (fn [state]
    [::&util/ok [state (:name state)]]))

(defn ^:private define [name desc]
  (fn [state]
    [::&util/ok [(-> state
                     (assoc-in [:defs (:name state) name] desc)
                     (assoc-in [:defs-env name] (annotated [::global (:name state) name] (:type desc))))
                 nil]]))

(def ^:private next-local-idx
  (fn [state]
    [::&util/ok [state (-> state :env first :counter)]]))

(def ^:private my-frame
  (fn [state]
    [::&util/ok [state (-> state :env first)]]))

(defn ^:private with-scope [scope body]
  (fn [state]
    (let [=return (body (-> state
                            (update-in [:lambda-scope 0] conj scope)
                            (assoc-in [:lambda-scope 1] 0)))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(assoc ?state :lambda-scope (:lambda-scope state)) ?value]]
        
        _
        =return))))

(defn ^:private with-lambda-scope [body]
  (fn [state]
    (let [_ (prn 'with-lambda-scope (get-in state [:lambda-scope 0]) (get-in state [:lambda-scope 1]))
          =return (body (-> state
                            (update-in [:lambda-scope 0] conj (get-in state [:lambda-scope 1]))
                            (assoc-in [:lambda-scope 1] 0)))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(do (prn [:lambda-scope 0] (get-in ?state [:lambda-scope 0]))
                       (prn [:lambda-scope 1] (get-in ?state [:lambda-scope 1]))
                       (-> ?state
                           (update-in [:lambda-scope 0] pop)
                           (assoc-in [:lambda-scope 1] (inc (get-in state [:lambda-scope 1])))))
                     ?value]]
        
        _
        =return))))

(def ^:private scope
  (fn [state]
    [::&util/ok [state (get-in state [:lambda-scope 0])]]))

(defn ^:private with-local [name type body]
  (fn [state]
    (let [=return (body (update-in state [:env]
                                   #(cons (-> (first %)
                                              (update-in [:counter] inc)
                                              (assoc-in [:mappings name] (annotated [::local (:id (first %)) (:counter (first %))] type)))
                                          (rest %))))]
      ;; =return
      (match =return
        [::&util/ok [?state ?value]]
        (do ;; (prn 'POST-WITH-LOCAL name (-> ?state :env first))
            [::&util/ok [(update-in ?state [:env] #(cons (-> (first %)
                                                             (update-in [:counter] dec)
                                                             (update-in [:mappings] dissoc name))
                                                         (rest %)))
                         ;; (update-in ?state [:env] (fn [[top & oframes]]
                         ;;                            (prn 'NEW-FRAMES name (cons (-> state :env first (assoc :closure (-> top :closure))) oframes))
                         ;;                            (cons (-> state :env first (assoc :closure (-> top :closure))) oframes)))
                         ?value]])
        
        _
        =return)
      )))

(defn ^:private with-fresh-env [[args-vars args-types] body]
  (with-lambda-scope
    (fn [state]
      ;; (prn '(:env state) (:env state) (-> state :env first :id inc))
      (let [state* (update-in state [:env]
                              (fn [outer]
                                (let [frame-id (-> outer first :id inc)
                                      new-top (reduce (fn [frame [name type]]
                                                        (-> frame
                                                            (update-in [:counter] inc)
                                                            (assoc-in [:mappings name] (annotated [::local frame-id (:counter frame)] type))))
                                                      (update-in (fresh-env frame-id) [:counter] inc)
                                                      (map vector args-vars args-types))]
                                  (conj outer new-top))))
            =return (body state*)
            ;; _ (prn '=return =return)
            ]
        (match =return
          [::&util/ok [?state ?value]]
          (do (prn 'PRE-LAMBDA (:env state))
            (prn 'POST-LAMBDA (:env ?state) ?value)
            [::&util/ok [(-> ?state
                             (update-in [:env] rest)
                             ;; (update-in [:lambda-scope 1] inc)
                             )
                         [(get-in ?state [:lambda-scope 0]) (-> ?state :env first) ?value]]])
          
          _
          =return)))))

(defn ^:private import-class [long-name short-name]
  (fn [state]
    (let [=class (annotated [::class long-name] ::&type/nothing)]
      [::&util/ok [(update-in state [:imports] merge {long-name =class,
                                                      short-name =class})
                   nil]])))

(defn ^:private require-module [name alias]
  (fn [state]
    [::&util/ok [(assoc-in state [:deps alias] name)
                 nil]]))

(defn ^:private close-over [scope ident register frame]
  ;; (prn 'close-over scope ident register)
  (let [register* (annotated [::captured scope (:closure/id frame) register] (:type register))]
    [register* (-> frame
                   (update-in [:closure/id] inc)
                   (assoc-in [:mappings ident] register*))]))

(defn ^:private resolve [ident]
  (fn [state]
    (if-let [[_ ?alias ?binding] (re-find #"^(.*)/(.*)$" ident)]
      (let [?module (get-in state [:deps ?alias])]
        ;; (prn 'resolve ?module ?alias ?binding)
        [::&util/ok [state (annotated [::global ?module ?binding] ::&type/nothing)]])
      (let [_ (prn 'resolve/_1 ident)
            [inner outer] (split-with #(nil? (get-in % [:mappings ident])) (:env state))
            ;; _ (prn ident '[inner outer] [inner outer])
            _ (prn 'resolve/_2 '[inner outer] [inner outer])]
        (cond (empty? inner)
              [::&util/ok [state (-> state :env first :mappings (get ident))]]
              
              (empty? outer)
              (if-let [global|import (or (get-in state [:defs-env ident])
                                         (get-in state [:imports ident]))]
                (do (prn 'resolve/_3 'global|import global|import)
                  [::&util/ok [state global|import]])
                [::&util/failure (str "Unresolved identifier: " ident)])

              :else
              (let [[=local inner*] (reduce (fn [[register new-inner] [frame scope]]
                                              (let [[register* frame*] (close-over scope ident register frame)]
                                                [register* (cons frame* new-inner)]))
                                            [(-> outer first :mappings (get ident)) '()]
                                            (map vector
                                                 (reverse inner)
                                                 (->> (get-in state [:lambda-scope 0])
                                                      (iterate pop)
                                                      (take (count inner))
                                                      reverse)))
                    _ (prn 'resolve/_4 '[=local inner*] =local inner*)]
                [::&util/ok [(assoc state :env (concat inner* outer)) =local]])))
      )))

(defmacro ^:private defanalyser [name match return]
  `(def ~name
     (fn [{[token# & left#] :forms :as state#}]
       (match token#
         ~match
         (~return (assoc state# :forms left#))
         _#
         (fail* (str "Unmatched token: " token#))))))

(defn analyse-form* [form]
  ;; (prn 'analyse-form* form)
  (fn [state]
    (let [old-forms (:forms state)
          =return (analyse-form (assoc state :forms (list form)))
          ;; _ (prn 'analyse-form*/=return =return)
          ]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(assoc ?state :forms old-forms) ?value]]
        
        _
        =return))))

(do-template [<name> <tag> <class>]
  (defanalyser <name>
    [<tag> ?value]
    (return (annotated [::literal ?value] [::&type/object <class> []])))

  analyse-boolean ::&parser/boolean "java.lang.Boolean"
  analyse-int     ::&parser/int     "java.lang.Integer"
  analyse-float   ::&parser/float   "java.lang.Float"
  analyse-char    ::&parser/char    "java.lang.Character"
  analyse-string  ::&parser/string  "java.lang.String"
  )

(defanalyser analyse-variant
  [::&parser/tagged ?tag ?value]
  (exec [;; :let [_ (prn 'analyse-variant [?tag ?value])]
         =value (analyse-form* ?value)
         ;; :let [_ (prn '=value =value)]
         ]
    (return (annotated [::variant ?tag =value] [::&type/variant ?tag (:type =value)]))))

(defanalyser analyse-tuple
  [::&parser/tuple ?elems]
  (exec [=elems (map-m analyse-form* ?elems)]
    (return (annotated [::tuple =elems] [::&type/tuple (mapv :type =elems)]))))

(defanalyser analyse-ident
  [::&parser/ident ?ident]
  ;; (exec [_env (fn [state] [::&util/ok [state (:env state)]])
  ;;        ;; :let [_ (prn 'analyse-ident ?ident _env)]
  ;;        ]
  ;;   (resolve ?ident))
  (exec [=ident (resolve ?ident)
         ;; :let [_ (prn 'analyse-ident ?ident =ident)]
         state &util/get-state
         :let [_ (prn 'analyse-ident ?ident (:form =ident) (:env state))]]
    (return =ident)))

(defanalyser analyse-ann-class
  [::&parser/ann-class ?class ?members]
  (return (annotated [::ann-class ?class ?members] ::&type/nothing)))

(defanalyser analyse-static-access
  [::&parser/static-access ?target ?member]
  (exec [=target (resolve ?target)
         ;; :let [_ (prn '=target ?target (:form =target))]
         ]
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
  (exec [;; :let [_ (prn 'PRE '?fn ?fn)]
         =fn (analyse-form* ?fn)
         ;; :let [_ (prn '=fn =fn)]
         =args (map-m analyse-form* ?args)
         ;; :let [_ (prn '=args =args)]
         ]
    (return (annotated [::call =fn =args] [::&type/object "java.lang.Object" []]))))

(defanalyser analyse-if
  [::&parser/if ?test ?then ?else]
  (exec [=test (analyse-form* ?test)
         ;; :let [_ (prn '=test =test)]
         ;; :let [_ (prn 'PRE '?then ?then)]
         =then (analyse-form* ?then)
         ;; :let [_ (prn '=then =then)]
         =else (analyse-form* ?else)
         ;; :let [_ (prn '=else =else)]
         ]
    (return (annotated [::if =test =then =else] ::&type/nothing))))

(defanalyser analyse-do
  [::&parser/do ?exprs]
  (exec [=exprs (map-m analyse-form* ?exprs)]
    (return (annotated [::do =exprs] (-> =exprs last :type)))))

(defanalyser analyse-case
  [::&parser/case ?variant ?branches]
  (exec [;; :let [_ (prn '?variant ?variant)]
         =variant (analyse-form* ?variant)
         ;; :let [_ (prn '=variant =variant)]
         =branches (map-m (fn [branch]
                            ;; (prn 'branch branch)
                            (match branch
                              [::&parser/case-branch [::&parser/tagged ?tag [::&parser/ident ?label]] ?body]
                              (exec [;; :let [_ (prn ?tag ?label '?body ?body)]
                                     idx next-local-idx
                                     =body (with-local ?label [::&type/object "java.lang.Object" []]
                                             (analyse-form* ?body))
                                     ;; :let [_ (prn ?tag ?label '=body =body)]
                                     ]
                                (return [?tag ?label idx =body]))))
                          ?branches)
         ;; :let [_ (prn '=branches =branches)]
         ]
    (return (annotated [::case =variant =branches] ::&type/nothing))))

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
    (exec [=value (with-scope ?name
                    (analyse-form* ?value))
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
             ;; :let [_ (prn '[=function =args =return] [=function =args =return])]
             ;; :let [env (-> {}
             ;;               (assoc ?name =function)
             ;;               (into (map vector args =args)))
             ;;       _ (prn 'env env)]
             =value (with-scope ?name
                      (reduce (fn [inner [label type]]
                                (with-local label type inner))
                              (analyse-form* ?value)
                              (reverse (map vector args =args))))
             ;; :let [_ (prn '=value =value)]
             =function (within :types (exec [_ (&type/solve =return (:type =value))]
                                        (&type/clean =function)))
             ;; :let [_ (prn '=function =function)]
             _ (define ?name {:mode ::function
                              :access ::public
                              :type =function})]
        (return (annotated [::def [?name args] =value] ::&type/nothing))))
    ))

(defanalyser analyse-lambda
  [::&parser/lambda ?args ?body]
  (exec [;; :let [_ (prn 'analyse-lambda ?args ?body)]
         [=function =args =return] (within :types (&type/fresh-function (count ?args)))
         ;; :let [_ (prn '[=function =args =return] [=function =args =return])]
         ;; :let [_ (prn 'PRE/?body ?body)]
         ;; _env (fn [state] [::&util/ok [state (:env state)]])
         ;; :let [_ (prn 'analyse-lambda _env)]
         [=scope =frame =body] (with-fresh-env [?args =args]
                                 (analyse-form* ?body))
         ;; :let [_ (prn '=body =body)]
         =function (within :types (exec [_ (&type/solve =return (:type =body))]
                                    (&type/clean =function)))
         ;; :let [_ (prn '=function =function)]
         ]
    (return (annotated [::lambda =scope =frame ?args =body] =function))))

(defanalyser analyse-import
  [::&parser/import ?class]
  (exec [_ (import-class ?class (last (string/split ?class #"\.")))]
    (return (annotated [::import ?class] ::&type/nothing))))

(defanalyser analyse-require
  [::&parser/require ?file ?alias]
  (let [;; _ (prn `[require ~?file ~?alias])
        module-name (re-find #"[^/]+$" ?file)
        ;; _ (prn 'module-name module-name)
        ]
    (exec [_ (require-module module-name ?alias)]
      (return (annotated [::require ?file ?alias] ::&type/nothing)))))

(def ^:private analyse-form
  (try-all-m [analyse-boolean
              analyse-int
              analyse-float
              analyse-char
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
              analyse-case
              analyse-let
              analyse-defclass
              analyse-definterface
              analyse-def
              analyse-import
              analyse-require]))

;; [Interface]
(defn analyse [module-name tokens]
  (match ((repeat-m (with-scope module-name
                      analyse-form)) {:name module-name,
                                      :forms tokens
                                      :deps {}
                                      :imports {}
                                      :defs {}
                                      :defs-env {}
                                      :lambda-scope [[] 0]
                                      :env (list (fresh-env 0))
                                      :types &type/+init+})
    [::&util/ok [?state ?forms]]
    (if (empty? (:forms ?state))
      ?forms
      (assert false (str "Unconsumed input: " (pr-str (:forms ?state)))))
    
    [::&util/failure ?message]
    (assert false ?message)))
