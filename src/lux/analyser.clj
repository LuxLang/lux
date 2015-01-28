(ns lux.analyser
  (:refer-clojure :exclude [resolve])
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-all-m map-m reduce-m
                                         within do-all-m*
                                         normalize-ident]]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [type :as &type])))

;; [Util]
(def +int-class+ "java.lang.Integer")

(defn ^:private annotated [form type]
  {:form form
   :type type})

(defn fresh-env [name]
  {:name name
   :inner-closures 0
   :counter 0
   :mappings {}
   :mappings/closure {}
   :closure/id 0})

(def module-name
  (fn [state]
    [::&util/ok [state (::current-module state)]]))

(def scope
  (fn [state]
    [::&util/ok [state (::scope state)]]))

(defn ^:private annotate [name mode access macro? type]
  (fn [state]
    [::&util/ok [(assoc-in state [::modules (::current-module state) name] {:mode   mode
                                                                            :access access
                                                                            :macro? macro?
                                                                            :type   type
                                                                            :defined? false})
                 nil]]))

(defn ^:private define [name]
  (fn [state]
    (if-let [{:keys [mode type]} (get-in state [::modules (::current-module state) name])]
      (let [full-name (str (::current-module state) ":" name)
            tag (if (= ::function mode)
                  ::global-fn
                  ::global)
            bound (annotated [tag (::current-module state) name] type)]
        [::&util/ok [(-> state
                         (assoc-in [::modules (::current-module state) name :defined?] true)
                         (update-in [::global-env] merge {full-name bound, name bound}))
                     nil]])
      (fail* (str "Can't define an unannotated element [" name "]")))))

(defn ^:private defined? [name]
  (fn [state]
    [::&util/ok [state (get-in state [::modules (::current-module state) name :defined?])]]))

(defn ^:private annotated? [name]
  (fn [state]
    [::&util/ok [state (boolean (get-in state [::modules (::current-module state) name]))]]))

(defn ^:private is-macro? [module name]
  (fn [state]
    [::&util/ok [state (boolean (get-in state [::modules module name :macro?]))]]))

(def ^:private next-local-idx
  (fn [state]
    [::&util/ok [state (-> state ::local-envs first :counter)]]))

(def ^:private scope-id
  (fn [state]
    [::&util/ok [state (-> state ::local-envs first :name)]]))

(defn with-global [top-level-name body]
  (exec [$module module-name]
    (fn [state]
      (let [=return (body (-> state
                              (update-in [::local-envs] conj (fresh-env top-level-name))
                              (assoc ::scope [$module top-level-name])))]
        (match =return
          [::&util/ok [?state ?value]]
          [::&util/ok [(assoc ?state ::scope []) ?value]]
          
          _
          =return))
      )))

(defn with-env [label body]
  (fn [state]
    (let [=return (body (-> state
                            (update-in [::local-envs] conj (fresh-env label))
                            (update-in [::scope] conj label)))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(-> ?state
                         (update-in [::local-envs] rest)
                         (update-in [::scope] rest))
                     ?value]]
        
        _
        =return))))

(defn ^:private with-local [name value body]
  (fn [state]
    (let [=return (body (update-in state [::local-envs]
                                   (fn [[env & other-envs]]
                                     (cons (assoc-in env [:mappings name] value)
                                           other-envs))))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(update-in ?state [::local-envs] #(cons (update-in (first %) [:mappings] dissoc name)
                                                             (rest %)))
                     ?value]]
        
        _
        =return)
      )))

(defn ^:private with-let [name type body]
  (fn [state]
    (let [[top & stack] (::local-envs state)
          body* (with-local name (annotated [::local (:name top) (:counter top)] type)
                  body)
          =return (body* (assoc state ::local-envs (cons (update-in top [:counter] inc) stack)))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(update-in ?state [::local-envs] (fn [[top* & stack*]]
                                                        (cons (update-in top* [:counter] dec)
                                                              stack*)))
                     ?value]]
        
        _
        =return))))

(do-template [<name> <unit-fn>]
  (defn <name> [locals monad]
    (reduce (fn [inner [label elem]]
              (<unit-fn> label elem inner))
            monad
            (reverse locals)))

  ^:private with-locals with-local
  ^:private with-lets   with-let
  )

(def captured-vars
  (fn [state]
    [::&util/ok [state (-> state ::local-envs first :mappings/closure)]]))

(defn with-lambda [self self-type arg arg-type body]
  (exec [$module module-name]
    (fn [state]
      (let [body* (with-env (-> state ::local-envs first :inner-closures str)
                    (exec [$scope scope]
                      (with-local self (annotated [::self $scope []] self-type)
                        (with-let arg arg-type
                          (exec [=return body
                                 =next next-local-idx
                                 =captured captured-vars]
                            (return [$scope =next =captured =return]))))))]
        (body* (update-in state [::local-envs] #(cons (update-in (first %) [:inner-closures] inc)
                                                      (rest %))))
        ))))

(defn ^:private close-over [scope ident register frame]
  (let [register* (annotated [::captured scope (:closure/id frame) register] (:type register))]
    [register* (-> frame
                   (update-in [:closure/id] inc)
                   (assoc-in [:mappings/closure ident] register*))]))

(defn ^:private resolve [ident]
  (fn [state]
    ;; (prn 'resolve ident)
    (let [[top & stack*] (::local-envs state)]
      (if-let [=bound (or (get-in top [:mappings ident])
                          (get-in top [:mappings/closure ident]))]
        [::&util/ok [state (list =bound)]]
        (let [no-binding? #(and (-> % :mappings (contains? ident) not) (-> % :mappings/closure (contains? ident) not))
              [inner outer] (split-with no-binding? stack*)]
          (if (empty? outer)
            (if-let [global|import (get-in state [::global-env ident])]
              [::&util/ok [state (list global|import)]]
              [::&util/failure (str "[Analyser Error] Unresolved identifier: " ident)])
            (let [[=local inner*] (reduce (fn [[register new-inner] frame]
                                            (let [[register* frame*] (close-over (:name frame) ident register frame)]
                                              [register* (cons frame* new-inner)]))
                                          [(or (get-in (first outer) [:mappings ident])
                                               (get-in (first outer) [:mappings/closure ident]))
                                           '()]
                                          (reverse (cons top inner)))]
              [::&util/ok [(assoc state ::local-envs (concat inner* outer)) (list =local)]])
            ))
        ))
    ))

(defn extract-ident [ident]
  (match ident
    [::&parser/ident ?ident]
    (return ?ident)

    _
    (fail "")))

(defn full-class [class]
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
      (try-all-m [(exec [=class (resolve class)]
                    (match (:form =class)
                      [::class ?full-name]
                      (return (Class/forName ?full-name))
                      _
                      (fail "[Analyser Error] Unknown class.")))
                  (let [full-name* (str "java.lang." class)]
                    (if-let [full-name (try (Class/forName full-name*)
                                         full-name*
                                         (catch Exception e
                                           nil))]
                      (return (Class/forName full-name))
                      (fail "[Analyser Error] Unknown class.")))]))))

(defn extract-jvm-param [token]
  (match token
    [::&parser/ident ?ident]
    (full-class ?ident)

    [::&parser/form ([[::&parser/ident "Array"] [::&parser/ident ?inner]] :seq)]
    (exec [=inner (full-class ?inner)]
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

(defn ^:private lookup-static-field [target field]
  (if-let [type* (first (for [=field (.getFields target)
                              :when (and (= target (.getDeclaringClass =field))
                                         (= field (.getName =field))
                                         (java.lang.reflect.Modifier/isStatic (.getModifiers =field)))]
                          (.getType =field)))]
    (exec [=type (&type/class->type type*)]
      (return =type))
    (fail (str "[Analyser Error] Field does not exist: " target field))))

(defn ^:private lookup-virtual-method [target method-name args]
  (if-let [method (first (for [=method (.getMethods target)
                               :when (and (= target (.getDeclaringClass =method))
                                          (= method-name (.getName =method))
                                          (not (java.lang.reflect.Modifier/isStatic (.getModifiers =method))))]
                           =method))]
    (exec [=method (&type/method->type method)]
      (&type/return-type =method))
    (fail (str "[Analyser Error] Virtual method does not exist: " target method-name))))

(defn ^:private full-class-name [class]
  (if (.contains class ".")
    (return class)
    (try-all-m [(exec [=class (resolve class)]
                  (match (:form =class)
                    [::class ?full-name]
                    (return ?full-name)
                    _
                    (fail "[Analyser Error] Unknown class.")))
                (let [full-name* (str "java.lang." class)]
                  (if-let [full-name (try (Class/forName full-name*)
                                       full-name*
                                       (catch Exception e
                                         nil))]
                    (return full-name)
                    (fail "[Analyser Error] Unknown class.")))])))

(defn ^:private ->lux+* [->lux loader xs]
  (reduce (fn [tail x]
            (doto (.newInstance (.loadClass loader "lux.Variant2"))
              (-> .-tag (set! "Cons"))
              (-> .-_1 (set! (->lux x)))
              (-> .-_2 (set! tail))))
          (doto (.newInstance (.loadClass loader "lux.Variant0"))
            (-> .-tag (set! "Nil")))
          (reverse xs)))

(defn ^:private ->lux [loader x]
  (match x
    [::&parser/bool ?bool]
    (doto (.newInstance (.loadClass loader "lux.Variant1"))
      (-> .-tag (set! "Bool"))
      (-> .-_1 (set! ?bool)))
    [::&parser/int ?int]
    (doto (.newInstance (.loadClass loader "lux.Variant1"))
      (-> .-tag (set! "Int"))
      (-> .-_1 (set! ?int)))
    [::&parser/real ?real]
    (doto (.newInstance (.loadClass loader "lux.Variant1"))
      (-> .-tag (set! "Real"))
      (-> .-_1 (set! ?real)))
    [::&parser/char ?elem]
    (doto (.newInstance (.loadClass loader "lux.Variant1"))
      (-> .-tag (set! "Char"))
      (-> .-_1 (set! ?elem)))
    [::&parser/text ?text]
    (doto (.newInstance (.loadClass loader "lux.Variant1"))
      (-> .-tag (set! "Text"))
      (-> .-_1 (set! ?text)))
    [::&parser/tag ?tag]
    (doto (.newInstance (.loadClass loader "lux.Variant1"))
      (-> .-tag (set! "Tag"))
      (-> .-_1 (set! ?tag)))
    [::&parser/ident ?ident]
    (doto (.newInstance (.loadClass loader "lux.Variant1"))
      (-> .-tag (set! "Ident"))
      (-> .-_1 (set! ?ident)))
    [::&parser/tuple ?elems]
    (doto (.newInstance (.loadClass loader "lux.Variant1"))
      (-> .-tag (set! "Tuple"))
      (-> .-_1 (set! (->lux+* ->lux loader ?elems))))
    [::&parser/form ?elems]
    (doto (.newInstance (.loadClass loader "lux.Variant1"))
      (-> .-tag (set! "Form"))
      (-> .-_1 (set! (->lux+* ->lux loader ?elems))))
    ))

(def ^:private ->lux+ (partial ->lux+* ->lux))

(defn ->clojure+* [->clojure xs]
  (case (.-tag xs)
    "Nil" '()
    "Cons" (cons (->clojure (.-_1 xs))
                 (->clojure+* ->clojure (.-_2 xs)))
    ))

(defn ->clojure [x]
  (case (.-tag x)
    "Bool"  [::&parser/bool (-> x .-_1)]
    "Int"   [::&parser/int (-> x .-_1)]
    "Real"  [::&parser/real (-> x .-_1)]
    "Char"  [::&parser/char (-> x .-_1)]
    "Text"  [::&parser/text (-> x .-_1)]
    "Tag"   [::&parser/tag (-> x .-_1)]
    "Ident" [::&parser/ident (-> x .-_1)]
    "Tuple" [::&parser/tuple (->> x .-_1 (->clojure+* ->clojure))]
    "Form"  [::&parser/form (->> x .-_1 (->clojure+* ->clojure))]))

(def ^:private ->clojure+ (partial ->clojure+* ->clojure))

(defn ^:private analyse-tuple [analyse-ast ?elems]
  (exec [=elems (do-all-m* (map analyse-ast ?elems))]
    (return (list (annotated [::tuple =elems] [::&type/tuple (mapv :type =elems)])))))

(defn ^:private analyse-ident [analyse-ast ?ident]
  (resolve ?ident))

(defn ^:private analyse-call [analyse-ast ?fn ?args]
  (exec [[=fn] (analyse-ast ?fn)
         loader &util/loader]
    (match (:form =fn)
      [::global-fn ?module ?name]
      (exec [macro? (is-macro? ?module ?name)]
        (if macro?
          (let [macro-class (str ?module "$" (normalize-ident ?name))]
            (-> (.loadClass loader macro-class)
                .newInstance
                (.apply (->lux+ loader ?args))
                ->clojure
                analyse-ast))
          (exec [=args (do-all-m* (map analyse-ast ?args))
                 :let [[needs-num =return-type] (match (:type =fn)
                                                  [::&type/function ?fargs ?freturn]
                                                  (let [needs-num (count ?fargs)
                                                        provides-num (count =args)]
                                                    (if (> needs-num provides-num)
                                                      [needs-num [::&type/function (drop provides-num ?fargs) ?freturn]]
                                                      [needs-num [::&type/object "java.lang.Object" []]])))]]
            (return (list (annotated [::static-call needs-num =fn =args] =return-type))))))

      _
      (exec [=args (do-all-m* (map analyse-ast ?args))]
        (return (list (annotated [::call =fn =args] [::&type/object "java.lang.Object" []])))))
    ))

(defn ^:private analyse-if [analyse-ast ?test ?then ?else]
  (exec [[=test] (analyse-ast ?test)
         [=then] (analyse-ast ?then)
         [=else] (analyse-ast ?else)]
    (return (list (annotated [::if =test =then =else] ::&type/nothing)))))

(defn ^:private analyse-do [analyse-ast ?exprs]
  (exec [=exprs (do-all-m* (map analyse-ast ?exprs))]
    (return (list (annotated [::do =exprs] (-> =exprs last :type))))))

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
                            (clojure.core.match/match (:type branches*)
                              ::char-tests branches*
                              ::text-tests branches*
                              ::tuple (do (assert (<= (count (:defaults branches*)) 1))
                                        {:type ::tuple*
                                         :patterns (into {} (for [[?tag ?struct] {nil (:patterns branches*)}]
                                                              [?tag {:parts (let [grouped-parts (apply map list (for [{:keys [case body]} (:cases ?struct)]
                                                                                                                  (map #(vector % body) case)))]
                                                                              (map generate-branches grouped-parts))
                                                                     :branches (:branches ?struct)}]))
                                         :default (-> branches* :defaults first)
                                         :branches (:branches branches*)})
                              ::adt (do (assert (<= (count (:defaults branches*)) 1))
                                      {:type ::adt*
                                       :patterns (into {} (for [[?tag ?struct] (:patterns branches*)]
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
          ;; Step 5: Re-structure branching
          ]
      [max-registers branch-mappings (generate-branches branches**)])))

(let [locals-getter (fn [$scope]
                      (fn member-fold [[$local locals] ?member]
                        (match ?member
                          [::&parser/ident ?name]
                          (return [(inc $local) (cons [?name (annotated [::local $scope $local] [::&type/object "java.lang.Object" []])] locals)])

                          [::&parser/tuple ?submembers]
                          (reduce-m member-fold [$local locals] ?submembers)

                          [::&parser/form ([[::&parser/tag ?subtag] & ?submembers] :seq)]
                          (reduce-m member-fold [$local locals] ?submembers)

                          _
                          (return [$local locals])
                          )))]
  (defn ^:private analyse-case [analyse-ast ?variant ?branches]
    (exec [[=variant] (analyse-ast ?variant)
           $scope scope-id
           $base next-local-idx
           [registers mappings tree] (exec [=branches (map-m (fn [[?pattern ?body]]
                                                               (match ?pattern
                                                                 [::&parser/char ?token]
                                                                 (exec [[=body] (analyse-ast ?body)]
                                                                   (return [::case-branch [::&parser/char ?token] =body]))

                                                                 [::&parser/text ?token]
                                                                 (exec [[=body] (analyse-ast ?body)]
                                                                   (return [::case-branch [::&parser/text ?token] =body]))
                                                                 
                                                                 [::&parser/ident ?name]
                                                                 (exec [[=body] (with-local ?name (annotated [::local $scope $base] [::&type/object "java.lang.Object" []])
                                                                                  (analyse-ast ?body))]
                                                                   (return [::case-branch [::&parser/ident ?name] =body]))

                                                                 [::&parser/tag ?tag]
                                                                 (exec [[=body] (analyse-ast ?body)]
                                                                   (return [::case-branch [::&parser/variant ?tag '()] =body]))

                                                                 [::&parser/tuple ?members]
                                                                 (exec [[_ locals+] (reduce-m (locals-getter $scope) [$base '()] ?members)
                                                                        [=body] (with-locals (reverse locals+)
                                                                                  (analyse-ast ?body))]
                                                                   (return [::case-branch [::&parser/tuple ?members] =body]))
                                                                 
                                                                 [::&parser/form ([[::&parser/tag ?tag] & ?members] :seq)]
                                                                 (exec [[_ locals+] (reduce-m (locals-getter $scope) [$base '()] ?members)
                                                                        [=body] (with-locals (reverse locals+)
                                                                                  (analyse-ast ?body))]
                                                                   (return [::case-branch [::&parser/variant ?tag ?members] =body]))
                                                                 ))
                                                             (partition 2 ?branches))]
                                       (return (->decision-tree $scope $base =branches)))]
      (return (list (annotated [::case (dec $base) =variant registers mappings tree] ::&type/nothing))))))

(defn ^:private analyse-let [analyse-ast ?label ?value ?body]
  (exec [[=value] (analyse-ast ?value)
         idx next-local-idx
         [=body] (with-let ?label (:type =value)
                   (analyse-ast ?body))]
    (return (list (annotated [::let idx ?label =value =body] (:type =body))))))

(declare raise-bindings)
(defn ^:private raise-tree-bindings [outer-scope ?tree]
  (case (:type ?tree)
    ::adt*
    (update-in ?tree [:patterns]
               #(into {} (for [[?tag ?unapply] %
                               :let [=unapply (update-in ?unapply [:parts] (partial map (partial raise-tree-bindings outer-scope)))]]
                           [?tag =unapply])))
    
    ::defaults
    (update-in ?tree [:stores]
               #(into {} (for [[?store ?branches] %
                               :let [=store (raise-bindings outer-scope {:form ?store :type ::&type/nothing})]]
                           [(:form =store) ?branches])))
    ;; else
    (assert false (pr-str ?tree))
    ))

(defn ^:private raise-bindings [outer-scope body]
  ;; (prn 'raise-bindings body)
  (match (:form body)
    [::local ?scope ?idx]
    {:form [::local outer-scope (inc ?idx)]
     :type (:type body)}
    
    [::captured _ _ ?source]
    ?source

    [::jvm:iadd ?x ?y]
    {:form [::jvm:iadd
            (raise-bindings outer-scope ?x)
            (raise-bindings outer-scope ?y)]
     :type (:type body)}

    [::case ?base ?variant ?registers ?mappings ?tree]
    (let [=variant (raise-bindings outer-scope ?variant)
          =mappings (into {} (for [[idx syntax] ?mappings]
                               [idx (raise-bindings outer-scope syntax)]))
          =tree (raise-tree-bindings outer-scope ?tree)]
      {:form [::case ?base =variant ?registers =mappings =tree]
       :type (:type body)})

    [::call ?func ?args]
    {:form [::call (raise-bindings outer-scope ?func)
            (map (partial raise-bindings outer-scope) ?args)]
     :type (:type body)}
    ))

(defn ^:private analyse-lambda [analyse-ast ?self ?arg ?body]
  (exec [[_ =arg =return :as =function] (within ::types &type/fresh-function)
         [=scope =next-local =captured =body] (with-lambda ?self =function
                                                ?arg =arg
                                                (analyse-ast ?body))
         _ (&util/assert! (= 1 (count =body)) "Can't return more than 1 value.")
         :let [[=body] =body]
         ;; :let [_ (prn 'analyse-lambda/=body ?arg =captured =body)]
         =function (within ::types (exec [_ (&type/solve =return (:type =body))]
                                     (&type/clean =function)))
         ;; :let [_ (prn 'LAMBDA/PRE (:form =body))]
         :let [;; _ (prn '(:form =body) (:form =body))
               =lambda (match (:form =body)
                    [::lambda ?sub-scope ?sub-captured ?sub-args ?sub-body]
                    (let [?sub-body* (raise-bindings =scope ?sub-body)]
                      [::lambda =scope =captured (cons ?arg ?sub-args) ?sub-body*])

                    _
                    [::lambda =scope =captured (list ?arg) =body])]
         ;; :let [_ (prn 'LAMBDA/POST =lambda)]
         ]
    (return (list (annotated =lambda =function)))))

(declare ->def-lambda)
(defn ^:private ->def-lambda-tree [old-scope new-scope ?tree]
  (case (:type ?tree)
    ::adt*
    (update-in ?tree [:patterns]
               #(into {} (for [[?tag ?unapply] %
                               :let [=unapply (update-in ?unapply [:parts] (partial map (partial ->def-lambda-tree old-scope new-scope)))]]
                           [?tag =unapply])))
    
    ::defaults
    (update-in ?tree [:stores]
               #(into {} (for [[?store ?branches] %
                               :let [=store (->def-lambda old-scope new-scope {:form ?store :type ::&type/nothing})]]
                           [(:form =store) ?branches])))
    ;; else
    (assert false (pr-str ?tree))
    ))

(defn ^:private ->def-lambda [old-scope new-scope syntax]
  (match (:form syntax)
    [::local ?local-scope ?idx]
    {:form [::local new-scope (inc ?idx)]
     :type (:type syntax)}

    [::self ?self-name ?curried]
    (if (= ?self-name old-scope)
      {:form [::self new-scope (mapv (partial ->def-lambda old-scope new-scope) ?curried)]
       :type (:type syntax)}
      syntax)
    

    [::jvm:iadd ?x ?y]
    {:form [::jvm:iadd (->def-lambda old-scope new-scope ?x) (->def-lambda old-scope new-scope ?y)]
     :type (:type syntax)}

    [::case ?base ?variant ?registers ?mappings ?tree]
    (let [=variant (->def-lambda old-scope new-scope ?variant)
          =mappings (into {} (for [[idx syntax] ?mappings]
                               [idx (->def-lambda old-scope new-scope syntax)]))
          =tree (->def-lambda-tree old-scope new-scope ?tree)]
      {:form [::case ?base =variant ?registers =mappings =tree]
       :type (:type syntax)})

    [::call ?func ?args]
    {:form [::call (->def-lambda old-scope new-scope ?func)
            (map (partial ->def-lambda old-scope new-scope) ?args)]
     :type (:type syntax)}
    
    [::lambda ?scope ?captured ?args ?value]
    {:form [::lambda new-scope
            (into {} (for [[?name ?sub-syntax] ?captured]
                       [?name (->def-lambda old-scope new-scope ?sub-syntax)]))
            ?args
            ?value]
     :type (:type syntax)}
    
    _
    (assert false (pr-str (:form syntax)))))

(defn ^:private analyse-def [analyse-ast ?name ?value]
  (exec [def?? (defined? ?name)]
    (if def??
      (fail (str "Can't redefine function/constant: " ?name))
      (exec [ann?? (annotated? ?name)
             $module module-name
             [=value] (with-global ?name
                        (analyse-ast ?value))
             ;; :let [_ (prn 'DEF/PRE =value)]
             :let [;; _ (prn 'analyse-def/=value =value)
                   =value (match (:form =value)
                            [::lambda ?scope ?env ?args ?body]
                            {:form [::lambda ?scope ?env ?args (->def-lambda ?scope [$module ?name] ?body)]
                             :type (:type =value)}
                            
                            _
                            =value)]
             ;; :let [_ (prn 'DEF/POST =value)]
             _ (if ann??
                 (return nil)
                 (annotate ?name ::constant ::public false (:type =value)))
             _ (define ?name)]
        (return (list (annotated [::def ?name =value] ::&type/nothing)))))))

(defn ^:private analyse-annotate [?ident]
  (exec [_ (annotate ?ident ::function ::public true ::&type/nothing)]
    (return (list))))

(defn ^:private analyse-require [analyse-ast ?path]
  (assert false)
  (return (list)))

(do-template [<name> <ident> <output-tag>]
  (defn <name> [analyse-ast ?x ?y]
    (exec [[=x] (analyse-ast ?x)
           [=y] (analyse-ast ?y)]
      (return (list (annotated [<output-tag> =x =y] [::&type/object +int-class+ []])))))

  ^:private analyse-jvm-iadd "jvm:iadd" ::jvm:iadd
  ^:private analyse-jvm-isub "jvm:isub" ::jvm:isub
  ^:private analyse-jvm-imul "jvm:imul" ::jvm:imul
  ^:private analyse-jvm-idiv "jvm:idiv" ::jvm:idiv
  ^:private analyse-jvm-irem "jvm:irem" ::jvm:irem
  )

(defn ^:private analyse-jvm-getstatic [analyse-ast ?class ?field]
  (exec [=class (full-class-name ?class)
         =type (lookup-static-field (Class/forName =class) ?field)]
    (return (list (annotated [::jvm:getstatic =class ?field] =type)))))

(defn ^:private analyse-jvm-invokevirtual [analyse-ast ?class ?method ?classes ?object ?args]
  (exec [=class (full-class-name ?class)
         =classes (map-m extract-jvm-param ?classes)
         =return (lookup-virtual-method (Class/forName =class) ?method =classes)
         [=object] (analyse-ast ?object)
         =args (do-all-m* (map analyse-ast ?args))]
    (return (list (annotated [::jvm:invokevirtual =class ?method (map #(.getName %) =classes) =object =args] =return)))))

(defn ^:private analyse-jvm-new [analyse-ast ?class ?classes ?args]
  (exec [=class (full-class-name ?class)
         =classes (map-m extract-jvm-param ?classes)
         =args (do-all-m* (map analyse-ast ?args))]
    (return (list (annotated [::jvm:new =class (map #(.getName %) =classes) =args] [::&type/object =class []])))))

(defn ^:private analyse-jvm-new-array [analyse-ast ?class ?length]
  (exec [=class (full-class-name ?class)]
    (return (list (annotated [::jvm:new-array =class ?length] [::&type/array [::&type/object =class []]])))))

(defn ^:private analyse-jvm-aastore [analyse-ast ?array ?idx ?elem]
  (exec [[=array] (analyse-ast ?array)
         [=elem] (analyse-ast ?elem)]
    (return (list (annotated [::jvm:aastore =array ?idx =elem] (:type =array))))))

(defn ^:private analyse-jvm-aaload [analyse-ast ?array ?idx]
  (exec [[=array] (analyse-ast ?array)]
    (return (list (annotated [::jvm:aaload =array ?idx] (-> =array :type (nth 1)))))))

(defn ^:private analyse-jvm-class [analyse-ast ?name ?super-class ?fields]
  (exec [?fields (map-m (fn [?field]
                          (match ?field
                            [::&parser/tuple ([[::&parser/ident ?class] [::&parser/ident ?field-name]] :seq)]
                            (return [?class ?field-name])
                            
                            _
                            (fail "")))
                        ?fields)
         :let [=members {:fields (into {} (for [[class field] ?fields]
                                            [field {:access ::public
                                                    :type class}]))}]
         name module-name]
    (return (list (annotated [::defclass [name ?name] ?super-class =members] ::&type/nothing)))))

(defn ^:private analyse-jvm-interface [analyse-ast ?name ?members]
  (exec [?members (map-m #(match %
                            [::&parser/form ([[::&parser/ident ":"] [::&parser/ident ?member-name]
                                              [::&parser/form ([[::&parser/ident "->"] [::&parser/tuple ?inputs] [::&parser/ident ?output]] :seq)]]
                                               :seq)]
                            (exec [?inputs (map-m extract-ident ?inputs)]
                              (return [?member-name [?inputs ?output]]))
                            
                            _
                            (fail ""))
                         ?members)
         :let [=members {:methods (into {} (for [[method [inputs output]] ?members]
                                             [method {:access ::public
                                                      :type [inputs output]}]))}
               =interface [::interface ?name =members]]
         name module-name]
    (return (list (annotated [::definterface [name ?name] =members] ::&type/nothing)))))

(defn ^:private analyse-basic-ast [analyse-ast token]
  (match token
    ;; Standard special forms
    [::&parser/bool ?value]
    (return (list (annotated [::literal ?value] [::&type/object "java.lang.Boolean" []])))

    [::&parser/int ?value]
    (return (list (annotated [::literal ?value] [::&type/object +int-class+ []])))

    [::&parser/real ?value]
    (return (list (annotated [::literal ?value] [::&type/object "java.lang.Float" []])))

    [::&parser/char ?value]
    (return (list (annotated [::literal ?value] [::&type/object "java.lang.Character" []])))

    [::&parser/text ?value]
    (return (list (annotated [::literal ?value] [::&type/object "java.lang.String" []])))

    [::&parser/tag ?tag]
    (return (list (annotated [::variant ?tag '()] [::&type/variant ?tag '()])))

    [::&parser/form ([[::&parser/tag ?tag] & ?data] :seq)]
    (exec [=data (do-all-m* (map analyse-ast ?data))]
      (return (list (annotated [::variant ?tag =data] [::&type/variant ?tag (map :type =data)]))))

    [::&parser/tuple ?elems]
    (analyse-tuple analyse-ast ?elems)

    [::&parser/ident ?ident]
    (analyse-ident analyse-ast ?ident)

    [::&parser/form ([[::&parser/ident "if"] ?test ?then ?else] :seq)]
    (analyse-if analyse-ast ?test ?then ?else)

    [::&parser/form ([[::&parser/ident "let"] [::&parser/ident ?label] ?value ?body] :seq)]
    (analyse-let analyse-ast ?label ?value ?body)

    [::&parser/form ([[::&parser/ident "case"] ?variant & ?branches] :seq)]
    (analyse-case analyse-ast ?variant ?branches)
    
    [::&parser/form ([[::&parser/ident "lambda'"] [::&parser/ident ?self] [::&parser/ident ?arg] ?body] :seq)]
    (analyse-lambda analyse-ast ?self ?arg ?body)

    [::&parser/form ([[::&parser/ident "def'"] [::&parser/ident ?name] ?value] :seq)]
    (analyse-def analyse-ast ?name ?value)

    [::&parser/form ([[::&parser/ident "annotate"] [::&parser/ident ?ident] [::&parser/ident "Macro"]] :seq)]
    (analyse-annotate ?ident)
    
    [::&parser/form ([[::&parser/ident "require"] [::&parser/text ?path]] :seq)]
    (analyse-require analyse-ast ?path)

    ;; Host special forms
    [::&parser/form ([[::&parser/ident "do"] & ?exprs] :seq)]
    (analyse-do ?exprs)
    
    [::&parser/form ([[::&parser/ident "jvm:iadd"] ?x ?y] :seq)]
    (analyse-jvm-iadd analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm:isub"] ?x ?y] :seq)]
    (analyse-jvm-isub analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm:imul"] ?x ?y] :seq)]
    (analyse-jvm-imul analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm:idiv"] ?x ?y] :seq)]
    (analyse-jvm-idiv analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm:irem"] ?x ?y] :seq)]
    (analyse-jvm-irem analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm:getstatic"] [::&parser/ident ?class] [::&parser/ident ?field]] :seq)]
    (analyse-jvm-getstatic analyse-ast ?class ?field)

    [::&parser/form ([[::&parser/ident "jvm:invokevirtual"] [::&parser/ident ?class] [::&parser/text ?method] [::&parser/tuple ?classes] ?object [::&parser/tuple ?args]] :seq)]
    (analyse-jvm-invokevirtual analyse-ast ?class ?method ?classes ?object ?args)

    [::&parser/form ([[::&parser/ident "jvm:new"] [::&parser/ident ?class] [::&parser/tuple ?classes] [::&parser/tuple ?args]] :seq)]
    (analyse-jvm-new analyse-ast ?class ?classes ?args)

    [::&parser/form ([[::&parser/ident "jvm:new-array"] [::&parser/ident ?class] [::&parser/int ?length]] :seq)]
    (analyse-jvm-new-array analyse-ast ?class ?length)

    [::&parser/form ([[::&parser/ident "jvm:aastore"] ?array [::&parser/int ?idx] ?elem] :seq)]
    (analyse-jvm-aastore analyse-ast ?array ?idx ?elem)

    [::&parser/form ([[::&parser/ident "jvm:aaload"] ?array [::&parser/int ?idx]] :seq)]
    (analyse-jvm-aaload analyse-ast ?array ?idx)

    [::&parser/form ([[::&parser/ident "jvm:class"] [::&parser/ident ?name] [::&parser/ident ?super-class] [::&parser/tuple ?fields]] :seq)]
    (analyse-jvm-class analyse-ast ?name ?super-class ?fields)

    [::&parser/form ([[::&parser/ident "jvm:interface"] [::&parser/ident ?name] & ?members] :seq)]
    (analyse-jvm-interface analyse-ast ?name ?members)

    _
    (fail (str "[Analyser Error] Unmatched token: " token))))

(defn analyse-ast [token]
  ;; (prn 'analyse-ast token)
  (match token
    [::&parser/form ([?fn & ?args] :seq)]
    (try-all-m [(analyse-call analyse-ast ?fn ?args)
                (analyse-basic-ast analyse-ast token)])

    _
    (analyse-basic-ast analyse-ast token)))

(def analyse
  (exec [asts &parser/parse
         ;; :let [_ (prn 'asts asts)]
         ]
    (do-all-m* (map analyse-ast asts))))
