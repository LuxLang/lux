(ns lux.analyser
  (:refer-clojure :exclude [resolve])
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-m try-all-m map-m reduce-m
                                         apply-m within
                                         normalize-ident
                                         loader]]
                 [parser :as &parser]
                 [type :as &type])))

(declare analyse-form
         ->tokens
         tokens->clojure)

;; [Util]
(defn ^:private annotated [form type]
  {:form form
   :type type})

(defn fresh-env [id]
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

(defn ^:private define-fn [name desc]
  (fn [state]
    [::&util/ok [(-> state
                     (assoc-in [:defs (:name state) name] desc)
                     (assoc-in [:defs-env name] (annotated [::global-fn (:name state) name] (:type desc))))
                 nil]]))

(defn ^:private is-macro? [name]
  (fn [state]
    ;; (prn 'is-macro? (nth name 1)
    ;;      (get-in state [:defs (:name state) (nth name 1) :mode])
    ;;       (= (get-in state [:defs (:name state) (nth name 1) :mode]) ::macro))
    [::&util/ok [state (= (get-in state [:defs (:name state) (nth name 1) :mode]) ::macro)]]))

(def ^:private next-local-idx
  (fn [state]
    [::&util/ok [state (-> state :env first :counter)]]))

(def ^:private scope-id
  (fn [state]
    [::&util/ok [state (-> state :env first :id)]]))

(def ^:private my-frame
  (fn [state]
    [::&util/ok [state (-> state :env first)]]))

(defn ^:private in-scope? [scope]
  (fn [state]
    (match scope
      [::&parser/ident ?macro-name]
      (do ;; (prn 'in-scope?
          ;;      ?macro-name
          ;;      (get-in state [:lambda-scope 0])
          ;;      (some (partial = ?macro-name) (get-in state [:lambda-scope 0])))
          [::&util/ok [state (some (partial = ?macro-name) (get-in state [:lambda-scope 0]))]])

      _
      [::&util/ok [state false]])
    ))

(defn with-scope [scope body]
  (fn [state]
    (let [=return (body (-> state
                            (update-in [:lambda-scope 0] conj scope)
                            (assoc-in [:lambda-scope 1] 0)))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(assoc ?state :lambda-scope (:lambda-scope state)) ?value]]
        
        _
        =return))))

(defn ^:private with-scoped-name [name type body]
  (fn [state]
    (let [=return (body (update-in state [:env]
                                   #(cons (assoc-in (first %) [:mappings name] (annotated [::global (:name state) name] type))
                                          (rest %))))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(update-in ?state [:env] #(cons (update-in (first %) [:mappings] dissoc name)
                                                     (rest %)))
                     ?value]]
        
        _
        =return))))

(defn ^:private with-lambda-scope [body]
  (fn [state]
    (let [;; _ (prn 'with-lambda-scope (get-in state [:lambda-scope 0]) (get-in state [:lambda-scope 1]))
          =return (body (-> state
                            (update-in [:lambda-scope 0] conj (get-in state [:lambda-scope 1]))
                            (assoc-in [:lambda-scope 1] 0)))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(do ;; (prn [:lambda-scope 0] (get-in ?state [:lambda-scope 0]))
                         ;; (prn [:lambda-scope 1] (get-in ?state [:lambda-scope 1]))
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

(defn ^:private with-locals [mappings monad]
  (fn [state]
    (let [=return (monad (update-in state [:env] #(cons (update-in (first %) [:mappings] merge mappings)
                                                        (rest %))))]
      (match =return
        [::&util/ok [?state ?value]]
        (do ;; (prn 'POST-WITH-LOCAL name (-> ?state :env first))
            [::&util/ok [(update-in ?state [:env] #(cons (assoc (first %) :mappings (-> state :env first :mappings))
                                                         (rest %)))
                         ?value]])
        
        _
        =return))))

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
          (do ;; (prn 'PRE-LAMBDA (:env state))
              ;; (prn 'POST-LAMBDA (:env ?state) ?value)
              [::&util/ok [(-> ?state
                               (update-in [:env] rest)
                               ;; (update-in [:lambda-scope 1] inc)
                               )
                           [(get-in ?state [:lambda-scope 0]) (-> ?state :env first) ?value]]])
          
          _
          =return)))))

(defn ^:private import-class [long-name short-name]
  (fn [state]
    (let [=class (annotated [::class long-name] [::&type/object long-name []])]
      [::&util/ok [(update-in state [:imports] merge {long-name =class,
                                                      short-name =class})
                   nil]])))

(defn ^:private use-module [name alias]
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
      (let [;; _ (prn 'resolve/_1 ident)
            [inner outer] (split-with #(nil? (get-in % [:mappings ident])) (:env state))
            ;; _ (prn ident '[inner outer] [inner outer])
            ;; _ (prn 'resolve/_2 '[inner outer] [inner outer])
            ]
        (cond (empty? inner)
              [::&util/ok [state (-> state :env first :mappings (get ident))]]
              
              (empty? outer)
              (if-let [global|import (or (get-in state [:defs-env ident])
                                         (get-in state [:imports ident]))]
                (do ;; (prn 'resolve/_3 'global|import global|import)
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
                    ;; _ (prn 'resolve/_4 '[=local inner*] =local inner*)
                    ]
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
        
        [::&util/failure ?message]
        (do (prn 'analyse-form* ?message)
          [::&util/failure ?message])))))

(do-template [<name> <tag> <class>]
  (defanalyser <name>
    [<tag> ?value]
    (return (annotated [::literal ?value] [::&type/object <class> []])))

  analyse-bool ::&parser/bool "java.lang.Boolean"
  analyse-int  ::&parser/int  "java.lang.Integer"
  analyse-real ::&parser/real "java.lang.Float"
  analyse-char ::&parser/char "java.lang.Character"
  analyse-text ::&parser/text "java.lang.String"
  )

(defanalyser analyse-variant
  [::&parser/variant ?tag ?data]
  (exec [;; :let [_ (prn 'analyse-variant [?tag ?value])]
         =data (map-m analyse-form* ?data)
         ;; :let [_ (prn '=value =value)]
         ]
    (return (annotated [::variant ?tag =data] [::&type/variant ?tag (map :type =data)]))))

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
  (exec [;; :let [_ (prn 'analyse-ident '?ident ?ident)]
         =ident (resolve ?ident)
         ;; :let [_ (prn 'analyse-ident '=ident =ident)]
         ;; :let [_ (prn 'analyse-ident ?ident =ident)]
         ;; state &util/get-state
         ;; :let [_ (prn 'analyse-ident ?ident (:form =ident) (:env state))]
         ]
    (return =ident)))

(defanalyser analyse-access
  [::&parser/static-access ?target ?member]
  (exec [=target (resolve ?target)]
    (match (:form =target)
      [::class ?class]
      (return (annotated [::static-access ?class ?member] ::&type/nothing)))))

(defn extract-ident [ident]
  (match ident
    [::&parser/ident ?ident]
    (return ?ident)

    _
    (fail "")))

(defn extract-class [x]
  (match x
    [::class ?class]
    (return ?class)

    _
    (fail "")))

(defn class-type [x]
  (match x
    [::&type/object ?class []]
    (return ?class)

    _
    (fail "")))

(defn lookup-field [mode target field]
  ;; (prn 'lookup-field mode target field)
  (if-let [[[owner type]] (seq (for [=field (.getFields (Class/forName target))
                                     ;; :let [_ (prn target (.getName =field) (if (java.lang.reflect.Modifier/isStatic (.getModifiers =field))
                                     ;;                                         :static
                                     ;;                                         :dynamic))]
                                     :when (and (= field (.getName =field))
                                                (case mode
                                                  :static (java.lang.reflect.Modifier/isStatic (.getModifiers =field))
                                                  :dynamic (not (java.lang.reflect.Modifier/isStatic (.getModifiers =field)))))]
                                 [(.getDeclaringClass =field) (.getType =field)]))]
    (exec [=type (&type/class->type type)]
      (return [(.getName owner) =type]))
    (fail (str "Field does not exist: " target field mode))))

(defn lookup-method [mode target method args]
  ;; (prn 'lookup-method mode target method args)
  (if-let [methods (seq (for [=method (.getMethods (Class/forName target))
                              ;; :let [_ (prn target (.getName =method) (if (java.lang.reflect.Modifier/isStatic (.getModifiers =method))
                              ;;                                          :static
                              ;;                                          :dynamic))]
                              :when (and (= method (.getName =method))
                                         (case mode
                                           :static (java.lang.reflect.Modifier/isStatic (.getModifiers =method))
                                           :dynamic (not (java.lang.reflect.Modifier/isStatic (.getModifiers =method)))))]
                          [(.getDeclaringClass =method) =method]))]
    (map-m (fn [[owner method]]
             (exec [=method (&type/method->type method)]
               (return [(.getName owner) =method])))
           methods)
    (fail (str "Method does not exist: " target method mode))))

(defanalyser analyse-access
  [::&parser/access ?object ?member]
  (match ?member
    [::&parser/ident ?field] ;; Field
    (try-all-m [(exec [?target (extract-ident ?object)
                       =target (resolve ?target)
                       ?class (extract-class (:form =target))
                       [=owner =type] (lookup-field :static ?class ?field)
                       ;; :let [_ (prn '=type =type)]
                       ]
                  (return (annotated [::static-field =owner ?field] =type)))
                (exec [=target (analyse-form* ?object)
                       ?class (class-type (:type =target))
                       [=owner =type] (lookup-field :dynamic ?class ?field)
                       ;; :let [_ (prn '=type =type)]
                       ]
                  (return (annotated [::dynamic-field =target =owner ?field] =type)))])
    [::&parser/fn-call [::&parser/ident ?method] ?args] ;; Method
    (exec [=args (map-m analyse-form* ?args)]
      (try-all-m [(exec [?target (extract-ident ?object)
                         =target (resolve ?target)
                         ?class (extract-class (:form =target))
                         =methods (lookup-method :static ?class ?method (map :type =args))
                         ;; :let [_ (prn '=methods =methods)]
                         [=owner =method] (within :types (&type/pick-matches =methods (map :type =args)))
                         ;; :let [_ (prn '=method =owner ?method =method)]
                         ]
                    (return (annotated [::static-method =owner ?method =method =args] (&type/return-type =method))))
                  (exec [=target (analyse-form* ?object)
                         ?class (class-type (:type =target))
                         =methods (lookup-method :dynamic ?class ?method (map :type =args))
                         ;; :let [_ (prn '=methods =methods)]
                         [=owner =method] (within :types (&type/pick-matches =methods (map :type =args)))
                         ;; :let [_ (prn '=method =owner ?method =method)]
                         ]
                    (return (annotated [::dynamic-method =target =owner ?method =method =args] (&type/return-type =method))))]))))

(defn ->token [x]
  ;; (prn '->token x)
  (match x
    [::&parser/text ?text]
    (doto (.newInstance (.loadClass loader "test2.Variant1"))
      (-> .-tag (set! "Text"))
      (-> .-_1 (set! ?text)))
    [::&parser/ident ?ident]
    (doto (.newInstance (.loadClass loader "test2.Variant1"))
      (-> .-tag (set! "Ident"))
      (-> .-_1 (set! ?ident)))
    [::&parser/fn-call ?fn ?args]
    (doto (.newInstance (.loadClass loader "test2.Variant1"))
      (-> .-tag (set! "Form"))
      (-> .-_1 (set! (->tokens (cons ?fn ?args)))))
    ))

(defn ->tokens [xs]
  (reduce (fn [tail x]
            ;; (prn 'tail (.-tag tail) 'x x)
            (doto (.newInstance (.loadClass loader "test2.Variant2"))
              (-> .-tag (set! "Cons"))
              (-> .-_1 (set! (->token x)))
              (-> .-_2 (set! tail))))
          (doto (.newInstance (.loadClass loader "test2.Variant0"))
            (-> .-tag (set! "Nil")))
          (reverse xs)))

(defn ->clojure-token [x]
  ;; (prn '->clojure-token x (.-tag x))
  (case (.-tag x)
    "Text" [::&parser/text (-> x .-_1 (doto (-> string? assert)))]
    "Ident" [::&parser/ident (-> x .-_1 (doto (-> string? assert)))]
    "Form" (let [[?fn & ?args] (-> x .-_1 tokens->clojure)]
             [::&parser/fn-call ?fn ?args])
    "Quote" [::&parser/quote (-> x .-_1 ->clojure-token)]))

(defn tokens->clojure [xs]
  ;; (prn 'tokens->clojure xs (.-tag xs))
  (case (.-tag xs)
    "Nil" '()
    "Cons" (cons (->clojure-token (.-_1 xs))
                 (tokens->clojure (.-_2 xs)))
    ))

(defanalyser analyse-fn-call
  [::&parser/fn-call ?fn ?args]
  (exec [=fn (analyse-form* ?fn)]
    (match (:form =fn)
      [::global-fn ?module ?name]
      (exec [macro? (is-macro? ?fn)
             scoped? (in-scope? ?fn)]
        (if (and macro? (not scoped?))
          (let [macro-class (str ?module "$" (normalize-ident ?name))]
            (-> (.loadClass loader macro-class)
                .newInstance
                (.apply (->tokens ?args))
                ->clojure-token
                analyse-form*))
          (exec [=args (map-m analyse-form* ?args)
                 :let [[needs-num =return-type] (match (:type =fn)
                                                  [::&type/function ?fargs ?freturn]
                                                  (let [needs-num (count ?fargs)
                                                        provides-num (count =args)]
                                                    (if (> needs-num provides-num)
                                                      [needs-num [::&type/function (drop provides-num ?fargs) ?freturn]]
                                                      [needs-num [::&type/object "java.lang.Object" []]])))
                       ;; _ (prn '[needs-num =return-type] [needs-num =return-type])
                       ]]
            (return (annotated [::static-call needs-num =fn =args] =return-type)))))

      _
      (exec [=args (map-m analyse-form* ?args)]
        (return (annotated [::call =fn =args] [::&type/object "java.lang.Object" []]))))
    ))

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

(let [fold-branches (fn [struct entry]
                      (let [struct* (clojure.core.match/match (nth entry 0)
                                      [::pm-text ?text]
                                      (clojure.core.match/match (:type struct)
                                        ::text-tests (update-in struct [:patterns ?text] (fn [bodies]
                                                                                           (if bodies
                                                                                             (conj bodies (nth entry 1))
                                                                                             #{(nth entry 1)})))
                                        nil (-> struct
                                                (assoc :type ::text-tests)
                                                (assoc-in [:patterns ?text] #{(nth entry 1)}))
                                        _ (assert false "Can't do match."))
                                      [::pm-variant ?tag ?members]
                                      (clojure.core.match/match (:type struct)
                                        ::adt (update-in struct [:patterns]
                                                         (fn [branches]
                                                           (if-let [{:keys [arity cases]} (get branches ?tag)]
                                                             (if (= arity (count ?members))
                                                               (-> branches
                                                                   (update-in [?tag :cases] conj {:case ?members
                                                                                                  :body (nth entry 1)})
                                                                   (update-in [?tag :branches] conj (nth entry 1)))
                                                               (assert false (str "Arity doesn't match. " (count ?members) "=/=" arity)))
                                                             (assoc branches ?tag {:arity (count ?members)
                                                                                   :cases [{:case ?members
                                                                                            :body (nth entry 1)}]
                                                                                   :branches #{(nth entry 1)}}))))
                                        nil (-> struct
                                                (assoc :type ::adt)
                                                (assoc-in [:patterns ?tag] {:arity (count ?members)
                                                                            :cases [{:case ?members
                                                                                     :body (nth entry 1)}]
                                                                            :branches #{(nth entry 1)}}))
                                        _ (assert false "Can't do match."))

                                      [::pm-local ?local]
                                      (update-in struct [:defaults] conj [::default ?local (nth entry 1)]))]
                        (update-in struct* [:branches] conj (nth entry 1))))
      base-struct {:type nil
                   :patterns {}
                   :defaults []
                   :branches #{}}
      generate-branches (fn generate-branches [data]
                          (let [branches* (reduce fold-branches base-struct data)]
                            ;; (prn 'generate-branches data)
                            ;; (prn 'branches* branches*)
                            ;; (.print System/out (prn-str 'branches* branches*))
                            ;; (.print System/out (prn-str '(:type branches*) (:type branches*)))
                            (clojure.core.match/match (:type branches*)
                              ::text-tests branches*
                              ::adt (do (assert (<= (count (:defaults branches*)) 1))
                                      {:type ::adt*
                                       :patterns (into {} (for [[?tag ?struct] (:patterns branches*)
                                                                ;; :let [_ (prn '(:patterns branches*) ?tag ?struct)]
                                                                ]
                                                            [?tag {:parts (let [grouped-parts (apply map list (for [{:keys [case body]} (:cases ?struct)]
                                                                                                                (map #(vector % body) case)))]
                                                                            (map generate-branches grouped-parts))
                                                                   :branches (:branches ?struct)}]))
                                       :default (-> branches* :defaults first)
                                       :branches (:branches branches*)})
                              nil {:type ::defaults,
                                   :stores (reduce (fn [total [_ ?store ?body]]
                                                     (update-in total [?store] (fn [mapping]
                                                                                 (if mapping
                                                                                   (conj mapping ?body)
                                                                                   #{?body}))))
                                                   {}
                                                   (:defaults branches*))
                                   :branches (:branches branches*)})))
      get-vars (fn get-vars [pattern]
                 (clojure.core.match/match pattern
                   [::&parser/ident ?name]
                   (list ?name)
                   
                   [::&parser/variant ?tag ?members]
                   (mapcat get-vars ?members)

                   [::&parser/text ?text]
                   '()))
      ->instructions (fn ->instructions [locals pattern]
                       (clojure.core.match/match pattern
                         [::&parser/variant ?tag ?members]
                         [::pm-variant ?tag (map (partial ->instructions locals) ?members)]
                         
                         [::&parser/ident ?name]
                         [::pm-local (get locals ?name)]
                         
                         [::&parser/text ?text]
                         [::pm-text ?text]
                         ))]
  (defn ->decision-tree [$scope $base branches]
    (let [;; Step 1: Get all vars
          vars+body (for [branch branches]
                      (clojure.core.match/match branch
                        [::&parser/case-branch ?pattern ?body]
                        [(get-vars ?pattern) ?body]))
          max-registers (reduce max 0 (map (comp count first) vars+body))
          ;; Step 2: Analyse bodies
          [_ branch-mappings branches*] (reduce (fn [[$link links branches*] branch]
                                                  (clojure.core.match/match branch
                                                    [::&parser/case-branch ?pattern ?body]
                                                    [(inc $link) (assoc links $link ?body) (conj branches* [::&parser/case-branch ?pattern $link])]))
                                                [0 {} []]
                                                branches)
          ;; Step 4: Pattens -> Instructions
          branches** (for [[branch branch-vars] (map vector branches* (map first vars+body))
                           :let [[_ locals] (reduce (fn [[$local =locals] $var]
                                                      [(inc $local) (assoc =locals $var [::local $scope $local])])
                                                    [$base {}] branch-vars)]]
                       (clojure.core.match/match branch
                         [::&parser/case-branch ?pattern ?body]
                         [(->instructions locals ?pattern) ?body]))
          ;; _ (prn branches**)
          ;; Step 5: Re-structure branching
          ]
      [max-registers branch-mappings (generate-branches branches**)])))

(defanalyser analyse-case
  [::&parser/case ?variant ?branches]
  (exec [=variant (analyse-form* ?variant)
         ;; :let [_ (prn 'analyse-case '=variant =variant)]
         $scope scope-id
         ;; :let [_ (prn 'analyse-case '$scope $scope)]
         $base next-local-idx
         ;; :let [_ (prn 'analyse-case '$base $base)]
         [registers mappings tree] (exec [=branches (map-m (fn [?branch]
                                                             (match ?branch
                                                               [::&parser/case-branch [::&parser/ident ?name] ?body]
                                                               (exec [=body (with-locals {?name (annotated [::local $scope $base] [::&type/object "java.lang.Object" []])}
                                                                              (analyse-form* ?body))]
                                                                 (return [::&parser/case-branch [::&parser/ident ?name] =body]))

                                                               [::&parser/case-branch [::&parser/variant ?tag ?members] ?body]
                                                               (exec [[_ locals+] (reduce-m (fn member-fold [[$local locals-map] ?member]
                                                                                              (match ?member
                                                                                                [::&parser/ident ?name]
                                                                                                (return [(inc $local) (assoc locals-map ?name (annotated [::local $scope $local] [::&type/object "java.lang.Object" []]))])

                                                                                                [::&parser/variant ?subtag ?submembers]
                                                                                                (reduce-m member-fold [$local locals-map] ?submembers)

                                                                                                _
                                                                                                (return [$local locals-map])
                                                                                                ))
                                                                                            [$base {}]
                                                                                            ?members)
                                                                      ;; :let [_ (prn 'analyse-case 'locals+ locals+)]
                                                                      =body (with-locals locals+
                                                                              (analyse-form* ?body))
                                                                      ;; :let [_ (prn 'analyse-case '=body =body)]
                                                                      ]
                                                                 (return [::&parser/case-branch [::&parser/variant ?tag ?members] =body]))))
                                                           ?branches)]
                                     (return (->decision-tree $scope $base =branches)))
         ;; :let [_ (prn 'analyse-case '[registers mappings tree] [registers mappings tree])]
         ]
    (return (annotated [::case (dec $base) =variant registers mappings tree] ::&type/nothing))))

(defanalyser analyse-let
  [::&parser/let ?label ?value ?body]
  (exec [=value (analyse-form* ?value)
         idx next-local-idx
         =body (with-local ?label =value
                 (analyse-form* ?body))]
    (return (annotated [::let idx ?label =value =body] (:type =body)))))

(defanalyser analyse-defclass
  [::&parser/defclass ?name ?super-class ?fields]
  (let [=members {:fields (into {} (for [[class field] ?fields]
                                     [field {:access ::public
                                             :type class}]))}
        =class [::class ?name =members]]
    (exec [name module-name]
      (return (annotated [::defclass [name ?name] ?super-class =members] ::&type/nothing)))))

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
           _ (define ?name {:mode   ::constant
                            :access ::public
                            :type   (:type =value)})]
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
                      (with-scoped-name ?name =function
                        (reduce (fn [inner [label type]]
                                  (with-local label type inner))
                                (analyse-form* ?value)
                                (reverse (map vector args =args)))))
             ;; :let [_ (prn '=value =value)]
             =function (within :types (exec [_ (&type/solve =return (:type =value))]
                                        (&type/clean =function)))
             ;; :let [_ (prn '=function =function)]
             _ (define-fn ?name {:mode   ::function
                                 :access ::public
                                 :type   =function})]
        (return (annotated [::def [?name args] =value] ::&type/nothing))))
    ))

(defanalyser analyse-defmacro
  [::&parser/defmacro [::&parser/fn-call [::&parser/ident ?name] ([[::&parser/ident ?tokens]] :seq)] ?value]
  (exec [[=function =tokens =return] (within :types (&type/fresh-function 1))
         =value (with-scope ?name
                  (with-scoped-name ?name =function
                    (with-local ?tokens =tokens
                      (analyse-form* ?value))))
         =function (within :types (exec [_ (&type/solve =return (:type =value))]
                                    (&type/clean =function)))
         _ (define-fn ?name {:mode   ::macro
                             :access ::public
                             :type   =function})]
    (return (annotated [::def [?name (list ?tokens)] =value] ::&type/nothing))))

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

(defanalyser analyse-use
  [::&parser/use ?file ?alias]
  (let [;; _ (prn `[use ~?file ~?alias])
        module-name (re-find #"[^/]+$" ?file)
        ;; _ (prn 'module-name module-name)
        ]
    (exec [_ (use-module module-name ?alias)]
      (return (annotated [::use ?file ?alias] ::&type/nothing)))))

(defanalyser analyse-quote
  [::&parser/quote ?quoted]
  (return (annotated [::quote ?quoted] ::&type/nothing)))

(do-template [<name> <input-tag> <output-tag>]
  (defanalyser <name>
    [<input-tag> ?x ?y]
    (exec [=x (analyse-form* ?x)
           =y (analyse-form* ?y)]
      (return (annotated [<output-tag> =x =y] [::&type/object "java.lang.Integer" []]))))

  ^:private analyse-jvm-i+   ::&parser/jvm-i+   ::jvm-i+
  ^:private analyse-jvm-i-   ::&parser/jvm-i-   ::jvm-i-
  ^:private analyse-jvm-i*   ::&parser/jvm-i*   ::jvm-i*
  ^:private analyse-jvm-idiv ::&parser/jvm-idiv ::jvm-idiv
  )

(def analyse-form
  (try-all-m [analyse-bool
              analyse-int
              analyse-real
              analyse-char
              analyse-text
              analyse-variant
              analyse-tuple
              analyse-lambda
              analyse-ident
              analyse-access
              analyse-fn-call
              analyse-if
              analyse-do
              analyse-case
              analyse-let
              analyse-defclass
              analyse-definterface
              analyse-def
              analyse-defmacro
              analyse-import
              analyse-use
              analyse-quote
              analyse-jvm-i+
              analyse-jvm-i-
              analyse-jvm-i*
              analyse-jvm-idiv]))

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
