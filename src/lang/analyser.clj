(ns lang.analyser
  (:refer-clojure :exclude [resolve])
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lang [util :as &util :refer [exec return* return fail fail*
                                          repeat-m try-m try-all-m map-m reduce-m
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

(def ^:private scope-id
  (fn [state]
    [::&util/ok [state (-> state :env first :id)]]))

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

(defn ^:private with-anon-locals [amount k]
  (fn [state]
    (let [env (-> state :env first)
          $scope (:id env)
          =locals (for [$local (take amount (iterate inc (:counter env)))]
                    (annotated [::local $scope $local] [::&type/object "java.lang.Object" []]))
          =return ((k =locals) (update-in state [:env] #(cons (update-in (first %) [:counter] + amount) (rest %))))]
      (match =return
        [::&util/ok [?state ?value]]
        (do ;; (prn 'POST-WITH-LOCAL name (-> ?state :env first))
            [::&util/ok [(update-in ?state [:env] #(cons (update-in (first %) [:counter] - amount) (rest %)))
                         ?value]])
        
        _
        =return))))

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
         ;; {:registers 3,
         ;;  :patterns {"Cons" {:arity 2, :branches [{:test [:lang/case-try "Cons" [[:lang/case-sub-bind 1] [:lang/case-sub-bind 2]]], :link 1}]},
         ;;             "Nil" {:arity 0, :branches [{:test [:lang/case-try "Nil" []], :link 0}]}},
         ;;  :paths {:total 2,
         ;;          :links {1 [:lang.parser/variant "Cons" ([:lang.parser/ident "x"] [:lang.parser/fn-call [:lang.parser/ident "++"] ([:lang.parser/ident "xs*"] [:lang.parser/ident "ys"])])],
         ;;                  0 [:lang.parser/ident "ys"]}}}
         [$base =branches] (with-anon-locals 1
                             (fn [=locals]
                               ;; (prn 'analyse-case '=locals (map :form =locals))
                               (exec [=branches (map-m (fn [?branch]
                                                         ;; (prn '?branch ?branch)
                                                         (match ?branch
                                                           [::&parser/case-branch [::&parser/variant ?tag ?members] ?body]
                                                           (let [num-members (count ?members)]
                                                             (with-anon-locals num-members
                                                               (fn [=locals]
                                                                 ;; (prn '?branch/=locals (map :form =locals))
                                                                 (exec [[inner-num locals+ members+] (reduce-m (fn member-fold [[?inner-num locals-map =members] [?local ?member]]
                                                                                                                 (match ?member
                                                                                                                   [::&parser/ident ?name]
                                                                                                                   (return [?inner-num
                                                                                                                            (assoc locals-map ?name ?local)
                                                                                                                            (conj =members (:form ?local))])

                                                                                                                   [::&parser/string ?text]
                                                                                                                   (return [?inner-num
                                                                                                                            locals-map
                                                                                                                            (conj =members [::match-text ?text])])

                                                                                                                   [::&parser/variant ?subtag ?submembers]
                                                                                                                   (let [num-submembers (count ?submembers)]
                                                                                                                     (with-anon-locals num-submembers
                                                                                                                       (fn [=sublocals]
                                                                                                                         (exec [[subinner-num sublocals+ submembers+] (reduce-m member-fold [0 {} []] (map vector =sublocals ?submembers))
                                                                                                                                ;; :let [_ (prn 'subinner-num subinner-num 'sublocals+ sublocals+ 'submembers+ submembers+)]
                                                                                                                                ]
                                                                                                                           (return [(+ ?inner-num num-submembers subinner-num)
                                                                                                                                    (merge locals-map sublocals+)
                                                                                                                                    (conj =members [::subcase ?subtag submembers+])])))))
                                                                                                                   ))
                                                                                                               [0 {} []]
                                                                                                               (map vector =locals ?members))
                                                                        ;; :let [_ (prn 'inner-num inner-num 'locals+ locals+ 'members+ members+)]
                                                                        ;; :let [_ (prn (first =members) ?body)]
                                                                        =body (with-locals locals+
                                                                                (analyse-form* ?body))
                                                                        ;; :let [_ (prn '?body ?body =body)]
                                                                        ]
                                                                   (return [(+ num-members inner-num) [::branch-adt ?tag members+ =body]])))))))
                                                       ?branches)]
                                 (return [(first =locals) =branches]))))
         :let [total-registers (+ 1 (reduce max 0 (map first =branches)))
               ;; _ (prn '=branches total-registers (map second =branches))
               ;; _ (assert false)
               ]
         ;; ([::&parser/case-branch [::&parser/variant "Nil" ()]
         ;;   [::&parser/ident "ys"]]
         ;;    [::&parser/case-branch [::&parser/variant "Cons" ([::&parser/ident "x"] [::&parser/ident "xs*"])]
         ;;     [::&parser/variant "Cons" ([::&parser/ident "x"] [::&parser/fn-call [::&parser/ident "++"] ([::&parser/ident "xs*"] [::&parser/ident "ys"])])]])
         ;; :let [_ (prn '?branches ?branches)
         ;;       case-analysis (let [gen-impl (fn gen-impl [offset pattern]
         ;;                                      (clojure.core.match/match pattern
         ;;                                        [::&parser/ident _]
         ;;                                        [1 [::case-bind -1 offset]]
                                                
         ;;                                        [::&parser/variant ?tag ?members]
         ;;                                        (let [regs+insns (mapv (fn [idx member]
         ;;                                                                 (clojure.core.match/match member
         ;;                                                                   [::&parser/ident _]
         ;;                                                                   [1 [::case-sub-bind (+ offset (inc idx))]]))
         ;;                                                               (range (count ?members))
         ;;                                                               ?members)]
         ;;                                          [(reduce + 1 (map first regs+insns)) [::case-try ?tag (mapv second regs+insns)]])
         ;;                                        (reduce + (count ?members) (map gen-impl ?members))))]
         ;;                       (reduce (fn [accum branch]
         ;;                                 (clojure.core.match/match branch
         ;;                                   [::&parser/case-branch ?pattern ?body]
         ;;                                   (clojure.core.match/match ?pattern
         ;;                                     [::&parser/variant ?tag ?members]
         ;;                                     (let [[extra-registers impl] (gen-impl 0 ?pattern)
         ;;                                           _ (prn 'impl extra-registers impl)
         ;;                                           $branch (get-in accum [:paths :total])]
         ;;                                       (-> accum
         ;;                                           (update-in [:patterns]
         ;;                                                      (fn [patterns]
         ;;                                                        (if (contains? patterns ?tag)
         ;;                                                          (if (= (get patterns [?tag :arity]) (count ?members))
         ;;                                                            (update-in patterns [?tag :branches] conj {:test impl
         ;;                                                                                                       :link $branch})
         ;;                                                            (assert "Pattern arity doesn't match!"))
         ;;                                                          (assoc patterns ?tag {:arity (count ?members)
         ;;                                                                                :branches [{:test impl
         ;;                                                                                            :link $branch}]}))))
         ;;                                           (update-in [:paths]
         ;;                                                      (fn [paths]
         ;;                                                        (-> paths
         ;;                                                            (update-in [:total] inc)
         ;;                                                            (assoc-in [:links $branch] ?body))))
         ;;                                           (update-in [:registers] + (dec extra-registers)))))
         ;;                                   ))
         ;;                               {:registers 1
         ;;                                :patterns {}
         ;;                                :paths {:total 0
         ;;                                        :links {}}}
         ;;                               ?branches))
         ;;       _ (prn 'case-analysis case-analysis)
         ;;       _ (assert false)]
         ;; =branches (map-m identity ;; (fn [branch]
         ;;                  ;;   ;; (prn 'branch branch)
         ;;                  ;;   (match branch
         ;;                  ;;     [::&parser/case-branch [::&parser/variant ?tag ?parts] ?body]
         ;;                  ;;     (exec [;; :let [_ (prn ?tag ?label '?body ?body)]
         ;;                  ;;            ;; (reduce-m (fn [?part]
         ;;                  ;;            ;;             (match ?part
         ;;                  ;;            ;;               [::&parser/ident ?label]
         ;;                  ;;            ;;               (exec [idx next-local-idx
         ;;                  ;;            ;;                      =body (with-local ?label [::&type/object "java.lang.Object" []]
         ;;                  ;;            ;;                              (analyse-form* ?body))]
         ;;                  ;;            ;;                 (return ...)))
         ;;                  ;;            ;;             )
         ;;                  ;;            ;;           ?parts)
         ;;                  ;;            idx next-local-idx
         ;;                  ;;            =body (with-local ?label [::&type/object "java.lang.Object" []]
         ;;                  ;;                    (analyse-form* ?body))
         ;;                  ;;            ;; :let [_ (prn ?tag ?label '=body =body)]
         ;;                  ;;            ]
         ;;                  ;;       (return [?tag ?label idx =body]))))
         ;;                  ?branches)
         ;; :let [_ (prn '=branches =branches)]
         ]
    (return (annotated [::case $base =variant total-registers (map second =branches)] ::&type/nothing))))

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
             _ (define ?name {:mode   ::function
                              :access ::public
                              :type   =function})]
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
              analyse-access
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
