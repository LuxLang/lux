(ns lux.analyser
  (:refer-clojure :exclude [resolve])
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m exhaust-m try-m try-all-m map-m reduce-m
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
                     (assoc-in [:modules (:name state) name] desc)
                     (assoc-in [:defs-env name] (annotated [::global (:name state) name] (:type desc))))
                 nil]]))

(defn ^:private define-fn [name desc]
  (fn [state]
    [::&util/ok [(-> state
                     (assoc-in [:modules (:name state) name] desc)
                     (assoc-in [:defs-env name] (annotated [::global-fn (:name state) name] (:type desc))))
                 nil]]))

(defn ^:private is-macro? [module name]
  (fn [state]
    ;; (prn 'is-macro? (nth name 1)
    ;;      (get-in state [:defs (:name state) (nth name 1) :mode])
    ;;       (= (get-in state [:defs (:name state) (nth name 1) :mode]) ::macro))
    ;; (prn 'is-macro? name (get-in state [:modules module name :mode])
    ;;      (get-in state [:modules module])
    ;;      (get-in state [:modules]))
    [::&util/ok [state (= (get-in state [:modules module name :mode]) ::macro)]]))

(def ^:private next-local-idx
  (fn [state]
    [::&util/ok [state (-> state :env first :counter)]]))

(def ^:private scope-id
  (fn [state]
    [::&util/ok [state (-> state :env first :id)]]))

(def ^:private my-frame
  (fn [state]
    [::&util/ok [state (-> state :env first)]]))

(defn ^:private in-scope? [module name]
  (fn [state]
    (do ;; (prn 'in-scope?
        ;;      ?macro-name
        ;;      (get-in state [:lambda-scope 0])
        ;;      (some (partial = ?macro-name) (get-in state [:lambda-scope 0])))
        [::&util/ok [state (some (partial = name) (get-in state [:lambda-scope 0]))]])
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
                                   #(cons (assoc-in (first %) [:mappings name] (annotated [::global-fn (:name state) name] type))
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
            [::&util/ok [(update-in ?state [:env] #(cons (update-in (first %) [:mappings] (fn [m] (apply dissoc m (keys mappings))))
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
              ;; (prn 'POST-LAMBDA1 (get-in ?state [:lambda-scope 0]) (-> ?state :env first :mappings))
              ;; (prn 'POST-LAMBDA2 (get-in ?state [:lambda-scope 0]) (-> ?state :env first (update-in [:mappings] #(reduce dissoc % args-vars)) :mappings))
              [::&util/ok [(-> ?state
                               (update-in [:env] rest)
                               ;; (update-in [:lambda-scope 1] inc)
                               )
                           [(get-in ?state [:lambda-scope 0])
                            (-> ?state :env first (update-in [:mappings] #(reduce dissoc % args-vars)))
                            ?value]]])
          
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
    (or (if-let [[_ ?alias ?binding] (re-find #"^(.*)/(.*)$" ident)]
          (if-let [?module (get-in state [:deps ?alias])]
            (do (prn 'resolve '[_ ?alias ?binding] ident [:global ?module ?binding])
              [::&util/ok [state (annotated [::global ?module ?binding] ::&type/nothing)]])))
        (let [[inner outer] (split-with #(nil? (get-in % [:mappings ident])) (:env state))]
          (cond (empty? inner)
                (do ;; (prn 'resolve/inner ident (get-in state [:lambda-scope 0]))
                    (prn 'resolve/env ident (-> state :env first :mappings (get ident)))
                  [::&util/ok [state (-> state :env first :mappings (get ident))]])
                
                (empty? outer)
                (do ;; (prn 'resolve/outer ident (get-in state [:lambda-scope 0]))
                  (if-let [global|import (or (get-in state [:defs-env ident])
                                             (get-in state [:imports ident]))]
                    (do (prn 'resolve/global|import ident global|import)
                      [::&util/ok [state global|import]])
                    (do (prn 'resolve/UNRESOLVED (str "Unresolved identifier: " ident))
                      [::&util/failure (str "Unresolved identifier: " ident)])))

                :else
                (do ;; (prn 'resolve/:else ident (get-in state [:lambda-scope 0]))
                  (let [[=local inner*] (reduce (fn [[register new-inner] [frame scope]]
                                                  (let [[register* frame*] (close-over scope ident register frame)]
                                                    [register* (cons frame* new-inner)]))
                                                [(-> outer first :mappings (get ident)) '()]
                                                (map vector
                                                     (reverse inner)
                                                     (->> (get-in state [:lambda-scope 0])
                                                          (iterate pop)
                                                          (take (count inner))
                                                          reverse)))]
                    ;; (prn 'resolve/inner* inner*)
                    (prn 'resolve/=local ident =local)
                    [::&util/ok [(assoc state :env (concat inner* outer)) =local]])))))))

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
        (do ;; (prn 'analyse-form* ?message)
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
  ?token
  (match ?token
    [::&parser/tag ?tag]
    (return (annotated [::variant ?tag '()] [::&type/variant ?tag '()]))
    
    [::&parser/form ([[::&parser/tag ?tag] & ?data] :seq)]
    (exec [=data (map-m analyse-form* ?data)]
      (return (annotated [::variant ?tag =data] [::&type/variant ?tag (map :type =data)])))

    _
    (fail "")))

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

(defn full-class [class]
  ;; (prn 'full-class-name class)
  (case class
    "boolean" (return Boolean/TYPE)
    "byte"    (return Byte/TYPE)
    "short"   (return Short/TYPE)
    "int"     (return Integer/TYPE)
    "long"    (return Long/TYPE)
    "float"   (return Float/TYPE)
    "double"  (return Double/TYPE)
    "char"    (return Character/TYPE)
    ;; else
    (if (.contains class ".")
      (return (Class/forName class))
      (try-all-m [(exec [=class (resolve class)
                         ;; :let [_ (prn '=class =class)]
                         ]
                    (match (:form =class)
                      [::class ?full-name]
                      (return (Class/forName ?full-name))
                      _
                      (fail "Unknown class.")))
                  (let [full-name* (str "java.lang." class)]
                    (if-let [full-name (try (Class/forName full-name*)
                                         full-name*
                                         (catch Exception e
                                           nil))]
                      (return (Class/forName full-name))
                      (fail "Unknown class.")))]))))

(defn extract-jvm-param [token]
  (match token
    [::&parser/ident ?ident]
    (full-class ?ident)

    [::&parser/form ([[::&parser/ident "Array"] [::&parser/ident ?inner]] :seq)]
    (exec [;; :let [_ (prn '?inner ?inner)]
           =inner (full-class ?inner)
           ;; :let [_ (prn '=inner =inner)
           ;;       _ (prn '(.getName =inner) (.getName =inner))]
           ]
      (return (Class/forName (str "[L" (.getName =inner) ";"))))

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
                                           :virtual (not (java.lang.reflect.Modifier/isStatic (.getModifiers =method)))))]
                          [(.getDeclaringClass =method) =method]))]
    (map-m (fn [[owner method]]
             (exec [=method (&type/method->type method)]
               (return [(.getName owner) =method])))
           methods)
    (fail (str "Method does not exist: " target method mode))))

(defn lookup-static-field [target field]
  (if-let [type* (first (for [=field (.getFields target)
                              :when (and (= target (.getDeclaringClass =field))
                                         (= field (.getName =field))
                                         (java.lang.reflect.Modifier/isStatic (.getModifiers =field)))]
                          (.getType =field)))]
    (exec [=type (&type/class->type type*)]
      (return =type))
    (fail (str "Field does not exist: " target field))))

(defn lookup-virtual-method [target method-name args]
  ;; (prn 'lookup-virtual-method target method-name args)
  (if-let [method (first (for [=method (.getMethods target)
                               :when (and (= target (.getDeclaringClass =method))
                                          (= method-name (.getName =method))
                                          (not (java.lang.reflect.Modifier/isStatic (.getModifiers =method))))]
                           =method))]
    (do ;; (prn 'lookup-virtual-method 'method method)
      (exec [=method (&type/method->type method)]
        (&type/return-type =method)))
    (do ;; (prn 'lookup-virtual-method (str "Virtual method does not exist: " target method-name))
      (fail (str "Virtual method does not exist: " target method-name)))))

(defn full-class-name [class]
  ;; (prn 'full-class-name class)
  (if (.contains class ".")
    (return class)
    (try-all-m [(exec [=class (resolve class)
                       ;; :let [_ (prn '=class =class)]
                       ]
                  (match (:form =class)
                    [::class ?full-name]
                    (return ?full-name)
                    _
                    (fail "Unknown class.")))
                (let [full-name* (str "java.lang." class)]
                  (if-let [full-name (try (Class/forName full-name*)
                                       full-name*
                                       (catch Exception e
                                         nil))]
                    (return full-name)
                    (fail "Unknown class.")))])))

(defanalyser analyse-jvm-getstatic
  [::&parser/form ([[::&parser/ident "jvm/getstatic"] [::&parser/ident ?class] [::&parser/ident ?field]] :seq)]
  (exec [=class (full-class-name ?class)
         =type (lookup-static-field (Class/forName =class) ?field)]
    (return (annotated [::jvm-getstatic =class ?field] =type))))

(defanalyser analyse-jvm-invokevirtual
  [::&parser/form ([[::&parser/ident "jvm/invokevirtual"] [::&parser/ident ?class] [::&parser/text ?method] [::&parser/tuple ?classes] ?object [::&parser/tuple ?args]] :seq)]
  (exec [=class (full-class-name ?class)
         =classes (map-m extract-jvm-param ?classes)
         =return (lookup-virtual-method (Class/forName =class) ?method =classes)
         ;; :let [_ (prn 'analyse-jvm-invokevirtual ?class ?method  =classes '-> =return)]
         ;; =return =return
         =object (analyse-form* ?object)
         =args (map-m analyse-form* ?args)]
    (return (annotated [::jvm-invokevirtual =class ?method (map #(.getName %) =classes) =object =args] =return))))

(defanalyser analyse-jvm-new
  [::&parser/form ([[::&parser/ident "jvm/new"] [::&parser/ident ?class] [::&parser/tuple ?classes] [::&parser/tuple ?args]] :seq)]
  (exec [=class (full-class-name ?class)
         =classes (map-m extract-jvm-param ?classes)
         =args (map-m analyse-form* ?args)]
    (return (annotated [::jvm-new =class (map #(.getName %) =classes) =args] [::&type/object =class []]))))

(defanalyser analyse-jvm-new-array
  [::&parser/form ([[::&parser/ident "jvm/new-array"] [::&parser/ident ?class] [::&parser/int ?length]] :seq)]
  (exec [=class (full-class-name ?class)]
    (return (annotated [::jvm-new-array =class ?length] [::&type/array [::&type/object =class []]]))))

(defanalyser analyse-jvm-aastore
  [::&parser/form ([[::&parser/ident "jvm/aastore"] ?array [::&parser/int ?idx] ?elem] :seq)]
  (exec [=array (analyse-form* ?array)
         =elem (analyse-form* ?elem)]
    (return (annotated [::jvm-aastore =array ?idx =elem] (:type =array)))))

(defanalyser analyse-jvm-aaload
  [::&parser/form ([[::&parser/ident "jvm/aaload"] ?array [::&parser/int ?idx]] :seq)]
  (exec [=array (analyse-form* ?array)]
    (return (annotated [::jvm-aaload =array ?idx] (-> =array :type (nth 1))))))

;; (defanalyser analyse-access
;;   [::&parser/access ?object ?member]
;;   (match ?member
;;     [::&parser/ident ?field] ;; Field
;;     (try-all-m [(exec [?target (extract-ident ?object)
;;                        =target (resolve ?target)
;;                        ?class (extract-class (:form =target))
;;                        [=owner =type] (lookup-field :static ?class ?field)
;;                        ;; :let [_ (prn '=type =type)]
;;                        ]
;;                   (return (annotated [::static-field =owner ?field] =type)))
;;                 (exec [=target (analyse-form* ?object)
;;                        ?class (class-type (:type =target))
;;                        [=owner =type] (lookup-field :dynamic ?class ?field)
;;                        ;; :let [_ (prn '=type =type)]
;;                        ]
;;                   (return (annotated [::dynamic-field =target =owner ?field] =type)))])
;;     [::&parser/fn-call [::&parser/ident ?method] ?args] ;; Method
;;     (exec [=args (map-m analyse-form* ?args)]
;;       (try-all-m [(exec [?target (extract-ident ?object)
;;                          =target (resolve ?target)
;;                          ?class (extract-class (:form =target))
;;                          =methods (lookup-method :static ?class ?method (map :type =args))
;;                          ;; :let [_ (prn '=methods =methods)]
;;                          [=owner =method] (within :types (&type/pick-matches =methods (map :type =args)))
;;                          ;; :let [_ (prn '=method =owner ?method =method)]
;;                          ]
;;                     (return (annotated [::static-method =owner ?method =method =args] (&type/return-type =method))))
;;                   (exec [=target (analyse-form* ?object)
;;                          ?class (class-type (:type =target))
;;                          =methods (lookup-method :dynamic ?class ?method (map :type =args))
;;                          ;; :let [_ (prn '=methods =methods)]
;;                          [=owner =method] (within :types (&type/pick-matches =methods (map :type =args)))
;;                          ;; :let [_ (prn '=method =owner ?method =method)]
;;                          ]
;;                     (return (annotated [::dynamic-method =target =owner ?method =method =args] (&type/return-type =method))))]))))

(defn ->token [x]
  ;; (prn '->token x)
  (match x
    [::&parser/bool ?bool]
    (doto (.newInstance (.loadClass @loader "lux.Variant1"))
      (-> .-tag (set! "Bool"))
      (-> .-_1 (set! ?bool)))
    [::&parser/int ?int]
    (doto (.newInstance (.loadClass @loader "lux.Variant1"))
      (-> .-tag (set! "Int"))
      (-> .-_1 (set! ?int)))
    [::&parser/real ?real]
    (doto (.newInstance (.loadClass @loader "lux.Variant1"))
      (-> .-tag (set! "Real"))
      (-> .-_1 (set! ?real)))
    [::&parser/char ?elem]
    (doto (.newInstance (.loadClass @loader "lux.Variant1"))
      (-> .-tag (set! "Char"))
      (-> .-_1 (set! ?elem)))
    [::&parser/text ?text]
    (doto (.newInstance (.loadClass @loader "lux.Variant1"))
      (-> .-tag (set! "Text"))
      (-> .-_1 (set! ?text)))
    [::&parser/tag ?tag]
    (doto (.newInstance (.loadClass @loader "lux.Variant1"))
      (-> .-tag (set! "Tag"))
      (-> .-_1 (set! ?tag)))
    [::&parser/ident ?ident]
    (doto (.newInstance (.loadClass @loader "lux.Variant1"))
      (-> .-tag (set! "Ident"))
      (-> .-_1 (set! ?ident)))
    [::&parser/tuple ?elems]
    (doto (.newInstance (.loadClass @loader "lux.Variant1"))
      (-> .-tag (set! "Tuple"))
      (-> .-_1 (set! (->tokens ?elems))))
    [::&parser/form ?elems]
    (doto (.newInstance (.loadClass @loader "lux.Variant1"))
      (-> .-tag (set! "Form"))
      (-> .-_1 (set! (->tokens ?elems))))
    ))

(defn ->tokens [xs]
  (reduce (fn [tail x]
            ;; (prn 'tail (.-tag tail) 'x x)
            (doto (.newInstance (.loadClass @loader "lux.Variant2"))
              (-> .-tag (set! "Cons"))
              (-> .-_1 (set! (->token x)))
              (-> .-_2 (set! tail))))
          (doto (.newInstance (.loadClass @loader "lux.Variant0"))
            (-> .-tag (set! "Nil")))
          (reverse xs)))

(defn ->clojure-token [x]
  ;; (prn '->clojure-token x (.-tag x))
  (case (.-tag x)
    "Bool"  [::&parser/bool (-> x .-_1)]
    "Int"   [::&parser/int (-> x .-_1)]
    "Real"  [::&parser/real (-> x .-_1)]
    "Char"  [::&parser/char (-> x .-_1)]
    "Text"  [::&parser/text (-> x .-_1)]
    "Tag"   [::&parser/tag (-> x .-_1)]
    "Ident" [::&parser/ident (-> x .-_1)]
    "Tuple" [::&parser/tuple (-> x .-_1 tokens->clojure)]
    "Form"  [::&parser/form (-> x .-_1 tokens->clojure)]))

(defn tokens->clojure [xs]
  ;; (prn 'tokens->clojure xs (.-tag xs))
  (case (.-tag xs)
    "Nil" '()
    "Cons" (cons (->clojure-token (.-_1 xs))
                 (tokens->clojure (.-_2 xs)))
    ))

(defanalyser analyse-call
  [::&parser/form ([?fn & ?args] :seq)]
  (exec [=fn (analyse-form* ?fn)
         ;; :let [_ (prn 'analyse-call/=fn =fn)]
         ]
    (match (:form =fn)
      [::global-fn ?module ?name]
      (exec [macro? (is-macro? ?module ?name)
             scoped? (in-scope? ?module ?name)
             :let [_ (prn 'analyse-call [:global-fn ?module ?name] macro? scoped?)]
             ;; :let [_ (prn 'analyse-call [:global-fn ?module ?name] macro? scoped?)]
             ]
        (if (and macro? (not scoped?))
          (let [macro-class (str ?module "$" (normalize-ident ?name))
                transformed (-> (.loadClass @loader macro-class)
                                .newInstance
                                (.apply (->tokens ?args))
                                ->clojure-token)
                _ (prn 'analyse-call/macro-raw ?args)
                _ (prn 'analyse-call/transformed transformed)
                ]
            (-> transformed
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
  [::&parser/form ([[::&parser/ident "if"] ?test ?then ?else] :seq)]
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
  [::&parser/form ([[::&parser/ident "do"] & ?exprs] :seq)]
  (exec [=exprs (map-m analyse-form* ?exprs)]
    (return (annotated [::do =exprs] (-> =exprs last :type)))))

(let [fold-branch (fn [struct entry]
                    (let [struct* (clojure.core.match/match (nth entry 0)
                                    [::pm-char ?token]
                                    (clojure.core.match/match (:type struct)
                                      ::char-tests (update-in struct [:patterns ?token] (fn [bodies]
                                                                                          (if bodies
                                                                                            (conj bodies (nth entry 1))
                                                                                            #{(nth entry 1)})))
                                      nil (-> struct
                                              (assoc :type ::char-tests)
                                              (assoc-in [:patterns ?token] #{(nth entry 1)}))
                                      _ (assert false "Can't do match."))

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
                                    
                                    [::pm-local ?local]
                                    (update-in struct [:defaults] conj [::default ?local (nth entry 1)])
                                    
                                    [::pm-tuple ?members]
                                    (clojure.core.match/match (:type struct)
                                      ::tuple (update-in struct [:patterns]
                                                         (fn [{:keys [arity cases] :as branch}]
                                                           (if (= arity (count ?members))
                                                             (-> branch
                                                                 (update-in [:cases] conj {:case ?members
                                                                                           :body (nth entry 1)})
                                                                 (update-in [:branches] conj (nth entry 1)))
                                                             (assert false (str "Arity doesn't match. " (count ?members) "=/=" arity)))))
                                      nil (-> struct
                                              (assoc :type ::tuple)
                                              (assoc :patterns {:arity (count ?members)
                                                                :cases [{:case ?members
                                                                         :body (nth entry 1)}]
                                                                :branches #{(nth entry 1)}}))
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
                                    )]
                      (update-in struct* [:branches] conj (nth entry 1))))
      base-struct {:type nil
                   :patterns {}
                   :defaults []
                   :branches #{}}
      generate-branches (fn generate-branches [data]
                          (let [branches* (reduce fold-branch base-struct data)]
                            ;; (prn 'generate-branches data)
                            ;; (prn 'branches* branches*)
                            ;; (.print System/out (prn-str 'branches* branches*))
                            ;; (.print System/out (prn-str '(:type branches*) (:type branches*)))
                            (clojure.core.match/match (:type branches*)
                              ::char-tests branches*
                              ::text-tests branches*
                              ::tuple (do (assert (<= (count (:defaults branches*)) 1))
                                        {:type ::tuple*
                                         :patterns (into {} (for [[?tag ?struct] {nil (:patterns branches*)}
                                                                  ;; :let [_ (prn '(:patterns branches*) ?tag ?struct)]
                                                                  ]
                                                              [?tag {:parts (let [grouped-parts (apply map list (for [{:keys [case body]} (:cases ?struct)]
                                                                                                                  (map #(vector % body) case)))]
                                                                              (map generate-branches grouped-parts))
                                                                     :branches (:branches ?struct)}]))
                                         :default (-> branches* :defaults first)
                                         :branches (:branches branches*)})
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
                   [::&parser/char ?token]
                   '()

                   [::&parser/text ?text]
                   '()

                   [::&parser/tag _]
                   '()

                   [::&parser/ident ?name]
                   (list ?name)

                   [::&parser/tuple ?members]
                   (mapcat get-vars ?members)

                   [::&parser/variant ?tag ?members]
                   (mapcat get-vars ?members)

                   [::&parser/form ([[::&parser/tag _] & ?members] :seq)]
                   (mapcat get-vars ?members)
                   ))
      ->instructions (fn ->instructions [locals pattern]
                       (clojure.core.match/match pattern
                         [::&parser/char ?token]
                         [::pm-char ?token]

                         [::&parser/text ?text]
                         [::pm-text ?text]

                         [::&parser/tag ?tag]
                         [::pm-variant ?tag '()]

                         [::&parser/ident ?name]
                         [::pm-local (get locals ?name)]

                         [::&parser/tuple ?members]
                         [::pm-tuple (map (partial ->instructions locals) ?members)]

                         [::&parser/variant ?tag ?members]
                         [::pm-variant ?tag (map (partial ->instructions locals) ?members)]

                         [::&parser/form ([[::&parser/tag ?tag] & ?members] :seq)]
                         [::pm-variant ?tag (map (partial ->instructions locals) ?members)]
                         ))]
  (defn ->decision-tree [$scope $base branches]
    (let [;; Step 1: Get all vars
          vars+body (for [branch branches]
                      (clojure.core.match/match branch
                        [::case-branch ?pattern ?body]
                        [(get-vars ?pattern) ?body]))
          max-registers (reduce max 0 (map (comp count first) vars+body))
          ;; Step 2: Analyse bodies
          [_ branch-mappings branches*] (reduce (fn [[$link links branches*] branch]
                                                  (clojure.core.match/match branch
                                                    [::case-branch ?pattern ?body]
                                                    [(inc $link) (assoc links $link ?body) (conj branches* [::case-branch ?pattern $link])]))
                                                [0 {} []]
                                                branches)
          ;; Step 4: Pattens -> Instructions
          branches** (for [[branch branch-vars] (map vector branches* (map first vars+body))
                           :let [[_ locals] (reduce (fn [[$local =locals] $var]
                                                      [(inc $local) (assoc =locals $var [::local $scope $local])])
                                                    [$base {}] branch-vars)]]
                       (clojure.core.match/match branch
                         [::case-branch ?pattern ?body]
                         [(->instructions locals ?pattern) ?body]))
          ;; _ (prn branches**)
          ;; Step 5: Re-structure branching
          ]
      [max-registers branch-mappings (generate-branches branches**)])))

(let [locals-getter (fn [$scope]
                      (fn member-fold [[$local locals-map] ?member]
                        (match ?member
                          [::&parser/ident ?name]
                          (return [(inc $local) (assoc locals-map ?name (annotated [::local $scope $local] [::&type/object "java.lang.Object" []]))])

                          [::&parser/tuple ?submembers]
                          (reduce-m member-fold [$local locals-map] ?submembers)

                          [::&parser/form ([[::&parser/tag ?subtag] & ?submembers] :seq)]
                          (reduce-m member-fold [$local locals-map] ?submembers)

                          _
                          (return [$local locals-map])
                          )))]
  (defanalyser analyse-case
    [::&parser/form ([[::&parser/ident "case"] ?variant & ?branches] :seq)]
    (exec [=variant (analyse-form* ?variant)
           ;; :let [_ (prn 'analyse-case '=variant =variant)]
           $scope scope-id
           ;; :let [_ (prn 'analyse-case '$scope $scope)]
           $base next-local-idx
           ;; :let [_ (prn 'analyse-case '$base $base)]
           [registers mappings tree] (exec [=branches (map-m (fn [[?pattern ?body]]
                                                               ;; (prn '?branch ?branch)
                                                               (match ?pattern
                                                                 [::&parser/char ?token]
                                                                 (exec [=body (analyse-form* ?body)]
                                                                   (return [::case-branch [::&parser/char ?token] =body]))

                                                                 [::&parser/text ?token]
                                                                 (exec [=body (analyse-form* ?body)]
                                                                   (return [::case-branch [::&parser/text ?token] =body]))
                                                                 
                                                                 [::&parser/ident ?name]
                                                                 (exec [=body (with-locals {?name (annotated [::local $scope $base] [::&type/object "java.lang.Object" []])}
                                                                                (analyse-form* ?body))]
                                                                   (return [::case-branch [::&parser/ident ?name] =body]))

                                                                 [::&parser/tag ?tag]
                                                                 (exec [=body (analyse-form* ?body)]
                                                                   (return [::case-branch [::&parser/variant ?tag '()] =body]))

                                                                 [::&parser/tuple ?members]
                                                                 (exec [[_ locals+] (reduce-m (locals-getter $scope) [$base {}] ?members)
                                                                        ;; :let [_ (prn 'analyse-case 'locals+ locals+)]
                                                                        =body (with-locals locals+
                                                                                (analyse-form* ?body))
                                                                        ;; :let [_ (prn 'analyse-case '=body =body)]
                                                                        ]
                                                                   (return [::case-branch [::&parser/tuple ?members] =body]))
                                                                 
                                                                 [::&parser/form ([[::&parser/tag ?tag] & ?members] :seq)]
                                                                 (exec [[_ locals+] (reduce-m (locals-getter $scope) [$base {}] ?members)
                                                                        ;; :let [_ (prn 'analyse-case 'locals+ locals+)]
                                                                        =body (with-locals locals+
                                                                                (analyse-form* ?body))
                                                                        ;; :let [_ (prn 'analyse-case '=body =body)]
                                                                        ]
                                                                   (return [::case-branch [::&parser/variant ?tag ?members] =body]))
                                                                 ))
                                                             (partition 2 ?branches))]
                                       (return (->decision-tree $scope $base =branches)))
           ;; :let [_ (prn 'analyse-case '[registers mappings tree] [registers mappings tree])]
           ]
      (return (annotated [::case (dec $base) =variant registers mappings tree] ::&type/nothing)))))

(defanalyser analyse-let
  [::&parser/form ([[::&parser/ident "let"] [::&parser/ident ?label] ?value ?body] :seq)]
  (exec [=value (analyse-form* ?value)
         idx next-local-idx
         =body (with-local ?label (:type =value)
                 (analyse-form* ?body))]
    (return (annotated [::let idx ?label =value =body] (:type =body)))))

(defanalyser analyse-defclass
  [::&parser/form ([[::&parser/ident "jvm/defclass"] [::&parser/ident ?name] [::&parser/ident ?super-class] [::&parser/tuple ?fields]] :seq)]
  (exec [;; :let [_ (prn 'analyse-defclass/?fields ?fields)]
         ?fields (map-m (fn [?field]
                          (match ?field
                            [::&parser/tuple ([[::&parser/ident ?class] [::&parser/ident ?field-name]] :seq)]
                            (return [?class ?field-name])
                            
                            _
                            (fail "")))
                        ?fields)
         :let [;; _ (prn 'analyse-defclass/?fields ?fields)
               =members {:fields (into {} (for [[class field] ?fields]
                                            [field {:access ::public
                                                    :type class}]))}]
         name module-name]
    (return (annotated [::defclass [name ?name] ?super-class =members] ::&type/nothing))))

(defanalyser analyse-definterface
  [::&parser/form ([[::&parser/ident "jvm/definterface"] [::&parser/ident ?name] & ?members] :seq)]
  (exec [;; :let [_ (prn 'analyse-definterface/?members ?members)]
         ?members (map-m #(match %
                            [::&parser/form ([[::&parser/ident ":"] [::&parser/ident ?member-name]
                                              [::&parser/form ([[::&parser/ident "->"] [::&parser/tuple ?inputs] [::&parser/ident ?output]] :seq)]]
                                               :seq)]
                            (exec [;; :let [_ (prn '[?member-name ?inputs ?output] [?member-name ?inputs ?output])]
                                   ?inputs (map-m extract-ident ?inputs)
                                   ;; :let [_ (prn '[?member-name ?inputs ?output] [?member-name ?inputs ?output])]
                                   ]
                              (return [?member-name [?inputs ?output]]))
                            
                            _
                            (fail ""))
                         ?members)
         :let [;; _ (prn '?members ?members)
               =members {:methods (into {} (for [[method [inputs output]] ?members]
                                             [method {:access ::public
                                                      :type [inputs output]}]))}
               =interface [::interface ?name =members]]
         name module-name]
    (return (annotated [::definterface [name ?name] =members] ::&type/nothing))))

(defanalyser analyse-def
  [::&parser/form ([[::&parser/ident "def"] ?usage ?value] :seq)]
  (match ?usage
    [::&parser/ident ?name]
    (exec [=value (with-scope ?name
                    (analyse-form* ?value))
           _ (define ?name {:mode   ::constant
                            :access ::public
                            :type   (:type =value)})]
      (return (annotated [::def ?name =value] ::&type/nothing)))

    [::&parser/form ([[::&parser/ident ?name] & ?args] :seq)]
    (exec [args (map-m extract-ident ?args)
           ;; :let [_ (prn 'analyse-def/args args)]
           [=function =args =return] (within :types (&type/fresh-function (count args)))
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
      (return (annotated [::def [?name args] =value] ::&type/nothing)))
    ))

(defanalyser analyse-defmacro
  [::&parser/form ([[::&parser/ident "defmacro"] [::&parser/form ([[::&parser/ident ?name] [::&parser/ident ?tokens]] :seq)] ?value] :seq)]
  (exec [[=function =tokens =return] (within :types (&type/fresh-function 1))
         :let [_ (prn 'analyse-defmacro/_1 ?name)]
         =value (with-scope ?name
                  (with-scoped-name ?name =function
                    (with-local ?tokens =tokens
                      (analyse-form* ?value))))
         :let [_ (prn 'analyse-defmacro/_2 ?name)]
         =function (within :types (exec [_ (&type/solve =return (:type =value))]
                                    (&type/clean =function)))
         :let [_ (prn 'analyse-defmacro/_3 ?name)]
         _ (define-fn ?name {:mode   ::macro
                             :access ::public
                             :type   =function})
         :let [_ (prn 'analyse-defmacro/_4 ?name)]]
    (return (annotated [::def [?name (list ?tokens)] =value] ::&type/nothing))))

(defanalyser analyse-lambda
  [::&parser/form ([[::&parser/ident "lambda"] [::&parser/tuple ?args] ?body] :seq)]
  (exec [?args (map-m extract-ident ?args)
         [=function =args =return] (within :types (&type/fresh-function (count ?args)))
         [=scope =frame =body] (with-fresh-env [?args =args]
                                 (analyse-form* ?body))
         =function (within :types (exec [_ (&type/solve =return (:type =body))]
                                    (&type/clean =function)))]
    (return (annotated [::lambda =scope =frame ?args =body] =function))))

(defanalyser analyse-import
  [::&parser/form ([[::&parser/ident "import"] [::&parser/ident ?class]] :seq)]
  (exec [_ (import-class ?class (last (string/split ?class #"\.")))]
    (return (annotated [::import ?class] ::&type/nothing))))

(defanalyser analyse-use
  [::&parser/form ([[::&parser/ident "use"] [::&parser/text ?file] [::&parser/ident "as"] [::&parser/ident ?alias]] :seq)]
  (let [module-name (re-find #"[^/]+$" ?file)]
    (exec [_ (use-module module-name ?alias)]
      (return (annotated [::use ?file ?alias] ::&type/nothing)))))

(do-template [<name> <ident> <output-tag>]
  (defanalyser <name>
    [::&parser/form ([[::&parser/ident <ident>] ?x ?y] :seq)]
    (exec [=x (analyse-form* ?x)
           =y (analyse-form* ?y)]
      (return (annotated [<output-tag> =x =y] [::&type/object "java.lang.Integer" []]))))

  ^:private analyse-jvm-i+   "jvm/i+"   ::jvm-i+
  ^:private analyse-jvm-i-   "jvm/i-"   ::jvm-i-
  ^:private analyse-jvm-i*   "jvm/i*"   ::jvm-i*
  ^:private analyse-jvm-idiv "jvm/i/"   ::jvm-idiv
  ^:private analyse-jvm-irem "jvm/irem" ::jvm-irem
  )

(def analyse-form
  (try-all-m [analyse-bool
              analyse-int
              analyse-real
              analyse-char
              analyse-text
              analyse-ident
              analyse-tuple
              analyse-variant
              analyse-call
              analyse-do
              analyse-if
              analyse-let
              analyse-case
              analyse-lambda
              analyse-def
              analyse-defmacro
              analyse-defclass
              analyse-definterface
              analyse-use
              analyse-import
              analyse-jvm-i+
              analyse-jvm-i-
              analyse-jvm-i*
              analyse-jvm-idiv
              analyse-jvm-irem
              analyse-jvm-getstatic
              analyse-jvm-invokevirtual
              analyse-jvm-new
              analyse-jvm-new-array
              analyse-jvm-aastore
              analyse-jvm-aaload
              ]))
