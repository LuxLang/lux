(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-all-m map-m mapcat-m reduce-m
                                         normalize-ident]]
                 [parser :as &parser]
                 [type :as &type]
                 [macros :as &macros]
                 [host :as &host])))

;; [Util]
(def ^:private +dont-care-type+ [::&type/Any])

(defn ^:private annotate [module name access type]
  (fn [state]
    (let [full-name (str module &util/+name-separator+ name)
          bound [::Expression [::global module name] type]]
      [::&util/ok [(-> state
                       (assoc-in [::&util/modules module name] {:args-n [:None]
                                                                :access access
                                                                :type   type
                                                                :defined? false})
                       (update-in [::&util/global-env] merge {full-name bound, name bound}))
                   nil]])))

(defn ^:private declare-macro [module name]
  (fn [state]
    [::&util/ok [(assoc-in state [::&util/modules module :macros name] true)
                 nil]]))

(defn ^:private expr-type [syntax+]
  (match syntax+
    [::Expression _ type]
    (return type)

    _
    (fail "Can't retrieve the type of a non-expression.")))

(defn ^:private define [module name]
  (exec [? annotated?
         _ (assert! ? (str "[Analyser Error] Can't define an unannotated element: " name))]
    (fn [state]
      [::&util/ok [(assoc-in state [::&util/modules module name :defined?] true)
                   nil]])))

(defn ^:private defined? [module name]
  (fn [state]
    [::&util/ok [state (get-in state [::&util/modules module name :defined?])]]))

(defn ^:private annotated? [module name]
  (fn [state]
    [::&util/ok [state (boolean (get-in state [::&util/modules module name]))]]))

(defn ^:private macro? [module name]
  (fn [state]
    [::&util/ok [state (boolean (get-in state [::&util/modules module :macros name]))]]))

(def ^:private next-local-idx
  (fn [state]
    [::&util/ok [state (-> state ::&util/local-envs first :locals :counter)]]))

(defn ^:private with-env [label body]
  (fn [state]
    (let [=return (body (update-in state [::&util/local-envs] conj (fresh-env label)))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(update-in ?state [::&util/local-envs] rest)
                     ?value]]
        
        _
        =return))))

(defn ^:private with-let [name mode type body]
  (fn [state]
    (let [old-mappings (-> state ::&util/local-envs first (get-in [:locals :mappings]))
          =return (body (update-in state [::&util/local-envs]
                                   (fn [[top & stack]]
                                     (let [bound-unit (case mode
                                                        :self [::self (list)]
                                                        :local [::local (get-in top [:locals :counter])])]
                                       (cons (-> top
                                                 (update-in [:locals :counter] inc)
                                                 (assoc-in [:locals :mappings name] [::Expression bound-unit type]))
                                             stack)))))]
      (match =return
        [::&util/ok [?state ?value]]
        [::&util/ok [(update-in ?state [::&util/local-envs] (fn [[top* & stack*]]
                                                              (cons (-> top*
                                                                        (update-in [:locals :counter] dec)
                                                                        (assoc-in [:locals :mappings] old-mappings))
                                                                    stack*)))
                     ?value]]
        
        _
        =return))))

(defn ^:private with-lets [locals monad]
  (reduce (fn [inner [label elem]]
            (with-let label :local elem inner))
          monad
          (reverse locals)))

(def ^:private captured-vars
  (fn [state]
    [::&util/ok [state (-> state ::&util/local-envs first :closure :mappings)]]))

(defn ^:private analyse-1 [elem]
  (exec [output (analyse-ast elem)
         _ (&util/assert! (= 1 (count output)) "[Analyser Error] Can't expand to other than 1 element.")]
    (return (first output))))

(defn ^:private analyse-2 [el1 el2]
  (exec [output (mapcat-m analyse-ast (list el1 el2))
         _ (&util/assert! (= 2 (count output))
                          "[Analyser Error] Can't expand to other than 2 elements.")]
    (return [(first output) (second output)])))

(defn ^:private with-lambda [self self-type arg arg-type body]
  (fn [state]
    (let [body* (with-env (-> state ::&util/local-envs first :inner-closures str)
                  (exec [$scope &util/get-scope]
                    (with-let self :self self-type
                      (with-let arg :local arg-type
                        (exec [=return body
                               =captured captured-vars]
                          (return [$scope =next =captured =return]))))))]
      (body* (update-in state [::&util/local-envs] #(cons (update-in (first %) [:inner-closures] inc)
                                                          (rest %))))
      )))

(defn ^:private close-over [scope ident register frame]
  (match register
    [::Expression _ register-type]
    (let [register* [::Expression [::captured scope (get-in frame [:closure :counter]) register] register-type]]
      [register* (-> frame
                     (update-in [:closure :counter] inc)
                     (assoc-in [:closure :mappings ident] register*))])))

(defn ^:private extract-ident [ident]
  (match ident
    [::&parser/ident ?ident]
    (return ?ident)

    _
    (fail "")))

(defn ^:private analyse-tuple [analyse-ast ?elems]
  (exec [=elems (mapcat-m analyse-ast ?elems)
         =elems-types (map-m expr-type =elems)
         ;; :let [_ (prn 'analyse-tuple =elems)]
         ]
    (return (list [::Expression [::tuple =elems] [::&type/Tuple =elems-types]]))))

(defn ^:private analyse-ident [analyse-ast ident]
  (fn [state]
    (let [[top & stack*] (::local-envs state)]
      (if-let [=bound (or (get-in top [:locals  :mappings ident])
                          (get-in top [:closure :mappings ident]))]
        [::&util/ok [state (list =bound)]]
        (let [no-binding? #(and (-> % :locals  :mappings (contains? ident) not)
                                (-> % :closure :mappings (contains? ident) not))
              [inner outer] (split-with no-binding? stack*)]
          (if (empty? outer)
            (if-let [global|import (get-in state [::&util/global-env ident])]
              [::&util/ok [state (list global|import)]]
              [::&util/failure (str "[Analyser Error] Unresolved identifier: " ident)])
            (let [[=local inner*] (reduce (fn [[register new-inner] frame]
                                            (let [[register* frame*] (close-over (:name frame) ident register frame)]
                                              [register* (cons frame* new-inner)]))
                                          [(or (get-in (first outer) [:locals  :mappings ident])
                                               (get-in (first outer) [:closure :mappings ident]))
                                           '()]
                                          (reverse (cons top inner)))]
              [::&util/ok [(assoc state ::&util/local-envs (concat inner* outer)) (list =local)]])
            ))
        ))
    ))

(defn ^:private analyse-call [analyse-ast ?fn ?args]
  (exec [=fn (analyse-1 ?fn)
         loader &util/loader]
    (match =fn
      [::Expression =fn-form =fn-type]
      (match =fn-form
        [::global ?module ?name]
        (exec [macro? (macro? ?module ?name)]
          (if macro?
            (let [macro-class (str ?module "$" (normalize-ident ?name))
                  output (-> (.loadClass loader macro-class)
                             .getDeclaredConstructors
                             first
                             (.newInstance (to-array [(int 0) nil]))
                             (.apply (&macros/->lux+ loader ?args))
                             (.apply nil))
                  ;; _ (prn 'output (str ?module ":" ?name) output (.-_1 output) (.-tag (.-_1 output)))
                  macro-expansion (&macros/->clojure+ (.-_1 output))
                  state* (.-_2 output)
                  ;; _ (prn 'macro-expansion (str ?module ":" ?name) state* macro-expansion)
                  ]
              (mapcat-m analyse-ast macro-expansion))
            (exec [=args (mapcat-m analyse-ast ?args)
                   :let [[needs-num =return-type] (match =fn-type
                                                    [::&type/function ?fargs ?freturn]
                                                    (let [needs-num (count ?fargs)
                                                          provides-num (count =args)]
                                                      (if (> needs-num provides-num)
                                                        [needs-num [::&type/function (drop provides-num ?fargs) ?freturn]]
                                                        [needs-num +dont-care-type+])))]]
              (return (list [::Expression [::static-call needs-num =fn =args] =return-type])))))

        _
        (exec [=args (mapcat-m analyse-ast ?args)]
          (return (list [::Expression [::call =fn =args] +dont-care-type+]))))

      :else
      (fail "Can't call something without a type."))
    ))

(defn ^:private analyse-do [analyse-ast ?exprs]
  (exec [_ (assert! (count ?exprs) "\"do\" expressions can't have empty bodies.")
         =exprs (mapcat-m analyse-ast ?exprs)
         =exprs-types (map-m expr-type =exprs)]
    (return (list [::Expression [::do =exprs] (last =exprs-types)]))))

(do-template [<name> <tag>]
  (defn <name> [tests ?token body-id]
    (match (:struct tests)
      [<tag> ?patterns ?defaults]
      {:struct [<tag> (update-in ?patterns [?token] (fn [bodies]
                                                      (if bodies
                                                        (conj bodies body-id)
                                                        #{body-id})))
                ?defaults]
       :branches (conj (:branches tests) body-id)}

      [::???Tests]
      {:struct [<tag> {?token #{body-id}} (list)]
       :branches (conj (:branches tests) body-id)}

      :else
      (assert false "Can't do match.")))

  ^:private bool-tests ::BoolTests
  ^:private int-tests  ::IntTests
  ^:private real-tests ::RealTests
  ^:private char-tests ::CharTests
  ^:private text-tests ::TextTests
  )

(defn with-default [struct ?local $body]
  (match (:struct tests)
    [::BoolTests ?patterns ?defaults]
    {:struct [::BoolTests ?patterns (conj ?defaults [::default ?local $body])]
     :branches (conj (:branches tests) body-id)}

    [::IntTests ?patterns ?defaults]
    {:struct [::IntTests ?patterns (conj ?defaults [::default ?local $body])]
     :branches (conj (:branches tests) body-id)}

    [::RealTests ?patterns ?defaults]
    {:struct [::RealTests ?patterns (conj ?defaults [::default ?local $body])]
     :branches (conj (:branches tests) body-id)}

    [::CharTests ?patterns ?defaults]
    {:struct [::CharTests ?patterns (conj ?defaults [::default ?local $body])]
     :branches (conj (:branches tests) body-id)}

    [::TextTests ?patterns ?defaults]
    {:struct [::TextTests ?patterns (conj ?defaults [::default ?local $body])]
     :branches (conj (:branches tests) body-id)}
    ))

(def ^:private product-match [<type> ?tag ?members body-id]
  (condp = (:type struct)
    <type> (update-in struct [:patterns]
                      (fn [branches]
                        (if-let [{:keys [arity cases]} (get branches ?tag)]
                          (if (= arity (count ?members))
                            (-> branches
                                (update-in [?tag :cases] conj {:case ?members
                                                               :body body-id})
                                (update-in [?tag :branches] conj body-id))
                            (assert false (str "Arity doesn't match. " (count ?members) "=/=" arity)))
                          (assoc branches ?tag {:arity (count ?members)
                                                :cases [{:case ?members
                                                         :body body-id}]
                                                :branches #{body-id}}))))
    nil (-> struct
            (assoc :type <type>)
            (assoc-in [:patterns ?tag] {:arity (count ?members)
                                        :cases [{:case ?members
                                                 :body body-id}]
                                        :branches #{body-id}}))
    ;; else
    (assert false "Can't do match.")
    ))

(def ^:private gen-product-branches [generate-branches <type> branches]
  (do (assert (<= (count (:defaults branches)) 1))
    {:type <type>
     :patterns (into {} (for [[?tag ?struct] (:patterns branches)]
                          [?tag {:parts (let [grouped-parts (apply map list (for [{:keys [case body]} (:cases ?struct)]
                                                                              (map #(vector % body) case)))]
                                          (map generate-branches grouped-parts))
                                 :branches (:branches ?struct)}]))
     :default (-> branches :defaults first)
     :branches (:branches branches)}))

(let [fold-branch (fn [struct [pattern $body]]
                    (match pattern
                      [::BoolPM ?value]
                      (bool-tests struct $body)

                      [::IntPM ?value]
                      (int-tests struct  $body)

                      [::RealPM ?value]
                      (real-tests struct $body)
                      
                      [::CharPM ?token]
                      (char-tests struct $body)

                      [::TextPM ?text]
                      (text-tests struct $body)
                      
                      [::TuplePM ?members]
                      (product-match struct ::tuple-tests    nil ?members $body)
                      
                      [::VariantPM ?tag ?members]
                      (product-match struct ::variant-tests ?tag ?members $body)

                      [::LocalPM ?local]
                      (with-default struct ?local $body)
                      ))
      base-struct [::???Tests]
      generate-branches (fn generate-branches [data]
                          (let [branches* (reduce fold-branch base-struct data)]
                            (match branches*
                              [::BoolTests _]  branches*
                              [::IntTests  _]  branches*
                              [::RealTests _]  branches*
                              [::CharTests _]  branches*
                              [::TextTests _]  branches*
                              ::TupleTests     (gen-product-branches generate-branches ::tuple-tests   branches*)
                              ::VariantTests   (gen-product-branches generate-branches ::variant-tests branches*)
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
                 (match pattern
                   [::&parser/Bool ?value]
                   (list)

                   [::&parser/Int ?value]
                   (list)

                   [::&parser/Real ?value]
                   (list)
                   
                   [::&parser/Char ?token]
                   (list)

                   [::&parser/Text ?text]
                   (list)

                   [::&parser/Tag _]
                   (list)

                   [::&parser/Ident ?name]
                   (list ?name)

                   [::&parser/Tuple ?members]
                   (mapcat get-vars ?members)

                   [::&parser/Form ([[::&parser/Tag _] & ?members] :seq)]
                   (mapcat get-vars ?members)
                   ))
      ->instructions (fn ->instructions [locals pattern]
                       (clojure.core.match/match pattern
                         [::&parser/Bool ?value]
                         [::BoolPM ?value]

                         [::&parser/Int ?value]
                         [::IntPM ?value]

                         [::&parser/Real ?value]
                         [::RealPM ?value]

                         [::&parser/Char ?value]
                         [::CharPM ?value]

                         [::&parser/Text ?value]
                         [::TextPM ?value]

                         [::&parser/Tag ?tag]
                         [::VariantPM ?tag (list)]

                         [::&parser/Ident ?name]
                         [::LocalPM (get locals ?name)]

                         [::&parser/Tuple ?members]
                         [::TuplePM (map (partial ->instructions locals) ?members)]
                         
                         [::&parser/Form ([[::&parser/Tag ?tag] & ?members] :seq)]
                         [::VariantPM ?tag (map (partial ->instructions locals) ?members)]
                         ))]
  (defn ^:private ->decision-tree [$base branches]
    (let [vars (for [branch branches]
                 (clojure.core.match/match branch
                   [::case-branch ?pattern ?body]
                   (get-vars ?pattern)))
          [_ branch-mappings branches*] (reduce (fn [[$link links branches*] branch]
                                                  (clojure.core.match/match branch
                                                    [::case-branch ?pattern ?body]
                                                    [(inc $link) (assoc links $link ?body) (conj branches* [::case-branch ?pattern $link])]))
                                                [0 {} []]
                                                branches)
          branches** (for [[branch branch-vars] (map vector branches* vars)
                           :let [[_ locals] (reduce (fn [[$local =locals] $var]
                                                      [(inc $local) (assoc =locals $var [::local $local])])
                                                    [$base {}] branch-vars)]]
                       (clojure.core.match/match branch
                         [::case-branch ?pattern ?body]
                         [(->instructions locals ?pattern) ?body]))
          max-registers (reduce max 0 (map count vars))]
      [max-registers branch-mappings (generate-branches branches**)])))

(defn ^:private locals-getter [?member]
  (match ?member
    [::&parser/Ident ?name]
    (list [?name +dont-care-type+])

    [::&parser/Tuple ?submembers]
    (mapcat locals-getter ?submembers)

    [::&parser/Form ([[::&parser/Tag ?subtag] & ?submembers] :seq)]
    (mapcat locals-getter ?submembers)

    _
    (list)
    ))

(defn ^:private analyse-case-branches [branches]
  (map-m (fn [[?pattern ?body]]
           (match ?pattern
             [::&parser/Bool ?token]
             (exec [=body (analyse-1 ?body)]
               (return [::case-branch ?pattern =body]))

             [::&parser/Int ?token]
             (exec [=body (analyse-1 ?body)]
               (return [::case-branch ?pattern =body]))

             [::&parser/Real ?token]
             (exec [=body (analyse-1 ?body)]
               (return [::case-branch ?pattern =body]))

             [::&parser/Char ?token]
             (exec [=body (analyse-1 ?body)]
               (return [::case-branch ?pattern =body]))

             [::&parser/Text ?token]
             (exec [=body (analyse-1 ?body)]
               (return [::case-branch ?pattern =body]))
             
             [::&parser/Ident ?name]
             (exec [=body (with-let ?name :local +dont-care-type+
                            (analyse-1 ?body))]
               (return [::case-branch ?pattern =body]))

             [::&parser/Tag ?tag]
             (exec [=body (analyse-1 ?body)]
               (return [::case-branch ?pattern =body]))
             
             [::&parser/Tuple ?members]
             (exec [=body (with-lets (mapcat locals-getter ?members)
                            (analyse-1 ?body))]
               (return [::case-branch ?pattern =body]))
             
             [::&parser/Form ([[::&parser/Tag ?tag] & ?members] :seq)]
             (exec [=body (with-lets (mapcat locals-getter ?members)
                            (analyse-1 ?body))]
               (return [::case-branch ?pattern =body]))
             ))
         branches))

(defn ^:private analyse-case [analyse-ast ?variant ?branches]
  (exec [=variant (analyse-1 ?variant)
         _ (assert! (and (> (count ?branches) 0) (even? (count ?branches)))
                    "Imbalanced branches in \"case'\" expression.")
         $base next-local-idx
         [num-registers mappings tree] (exec [=branches (analyse-case-branches (partition 2 ?branches))]
                                         (return (->decision-tree $base =branches)))]
    (return (list [::Expression [::case $base =variant num-registers mappings tree] +dont-care-type+]))))

(defn ^:private raise-tree-bindings [raise-expr arg ?tree]
  (let [tree-partial-f (partial raise-tree-bindings raise-expr arg)]
    (case (:type ?tree)
      (::tuple ::variant)
      (-> ?tree
          (update-in [:patterns]
                     #(into {} (for [[?tag ?unapply] %]
                                 [?tag (update-in ?unapply [:parts] (partial map tree-partial-f))])))
          (update-in [:default]
                     (fn [[tag local $branch :as total]]
                       (if total
                         (match (raise-expr arg [::Expression local [::&type/Nothing]])
                           [::Expression local* [::&type/Nothing]]
                           [tag local* $branch])))))
      
      ::defaults
      (update-in ?tree [:stores]
                 #(into {} (for [[?store ?branches] %]
                             (match (raise-expr arg [::Expression ?store [::&type/Nothing]])
                               [::Expression =store [::&type/Nothing]]
                               [=store ?branches]))))
      ;; else
      (assert false (pr-str ?tree))
      )))

(defn ^:private raise-expr [arg syntax]
  ;; (prn 'raise-bindings body)
  (let [partial-f (partial raise-expr arg)
        tree-partial-f (partial raise-tree-bindings raise-expr arg)]
    (match syntax
      [::Expression ?form ?type]
      (match ?form
        [::bool ?value]
        syntax

        [::int ?value]
        syntax

        [::real ?value]
        syntax

        [::char ?value]
        syntax

        [::text ?value]
        syntax
        
        [::tuple ?members]
        [::Expression [::tuple (map partial-f ?members)] ?type]

        [::variant ?tag ?members]
        [::Expression [::variant ?tag (map partial-f ?members)] ?type]
        
        [::local ?idx]
        [::Expression [::local (inc ?idx)] ?type]
        
        [::captured _ _ ?source]
        ?source

        [::self ?curried]
        [::Expression [::self (cons arg (map partial-f ?curried))] ?type]

        [::global _ _]
        syntax

        [::let ?idx ?value ?body]
        [::Expression [::let (inc ?idx) (partial-f ?value)
                       (partial-f ?body)]
         ?type]

        [::case ?base ?variant ?registers ?mappings ?tree]
        (let [=variant (partial-f ?variant)
              =mappings (into {} (for [[idx syntax] ?mappings]
                                   [idx (partial-f syntax)]))
              =tree (tree-partial-f ?tree)]
          [::Expression [::case (inc ?base) =variant ?registers =mappings =tree] ?type])

        [::lambda ?scope ?captured ?args ?value]
        [::Expression [::lambda (pop ?scope)
                       (into {} (for [[?name ?sub-syntax] ?captured]
                                  [?name (partial-f ?sub-syntax)]))
                       ?args
                       ?value]
         ?type]

        [::call ?func ?args]
        [::Expression [::call (partial-f ?func) (map partial-f ?args)] ?type]

        [::do ?asts]
        [::Expression [::do (map partial-f ?asts)] ?type]

        [::jvm-getstatic _ _]
        syntax
        
        [::jvm-invokevirtual ?class ?method ?arg-classes ?obj ?args]
        [::Expression [::jvm-invokevirtual ?class ?method ?arg-classes
                       (partial-f ?obj)
                       (map partial-f ?args)]
         ?type]

        ;; Integer arithmetic
        [::jvm-iadd ?x ?y]
        [::Expression [::jvm-iadd (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-isub ?x ?y]
        [::Expression [::jvm-isub (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-imul ?x ?y]
        [::Expression [::jvm-imul (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-idiv ?x ?y]
        [::Expression [::jvm-idiv (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-irem ?x ?y]
        [::Expression [::jvm-irem (partial-f ?x) (partial-f ?y)] ?type]

        ;; Long arithmetic
        [::jvm-ladd ?x ?y]
        [::Expression [::jvm-ladd (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-lsub ?x ?y]
        [::Expression [::jvm-lsub (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-lmul ?x ?y]
        [::Expression [::jvm-lmul (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-ldiv ?x ?y]
        [::Expression [::jvm-ldiv (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-lrem ?x ?y]
        [::Expression [::jvm-lrem (partial-f ?x) (partial-f ?y)] ?type]

        ;; Float arithmetic
        [::jvm-fadd ?x ?y]
        [::Expression [::jvm-fadd (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-fsub ?x ?y]
        [::Expression [::jvm-fsub (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-fmul ?x ?y]
        [::Expression [::jvm-fmul (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-fdiv ?x ?y]
        [::Expression [::jvm-fdiv (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-frem ?x ?y]
        [::Expression [::jvm-frem (partial-f ?x) (partial-f ?y)] ?type]

        ;; Double arithmetic
        [::jvm-dadd ?x ?y]
        [::Expression [::jvm-dadd (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-dsub ?x ?y]
        [::Expression [::jvm-dsub (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-dmul ?x ?y]
        [::Expression [::jvm-dmul (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-ddiv ?x ?y]
        [::Expression [::jvm-ddiv (partial-f ?x) (partial-f ?y)] ?type]

        [::jvm-drem ?x ?y]
        [::Expression [::jvm-drem (partial-f ?x) (partial-f ?y)] ?type]

        _
        (assert false syntax)
        ))))

(defn ^:private analyse-lambda [analyse-ast ?self ?arg ?body]
  (exec [[_ =arg =return :as =function] &type/fresh-function
         [=scope =captured =body] (with-lambda ?self =function
                                    ?arg =arg
                                    (analyse-1 ?body))
         =body-type (expr-type =body)
         =function (exec [_ (&type/solve =return =body-type)]
                     (&type/clean =function))
         :let [=lambda (match =body
                    [::Expression [::lambda ?sub-scope ?sub-captured ?sub-args ?sub-body] =body-type]
                    [::Expression [::lambda =scope =captured (cons ?arg ?sub-args) (raise-expr ?arg ?sub-body)] =body-type]

                    _
                    [::Expression [::lambda =scope =captured (list ?arg) =body] =body-type])]]
    (return (list [::Expression =lambda =function]))))

(defn ^:private analyse-def [analyse-ast ?name ?value]
  ;; (prn 'analyse-def ?name ?value)
  (exec [def?? (defined? ?name)]
    (if def??
      (fail (str "Can't redefine " ?name))
      (exec [ann?? (annotated? ?name)
             $module &util/get-module-name
             =value (analyse-1 ?value)
             =value (match =value
                      [::Expression =value-form =value-type]
                      (return (match =value-form
                                [::lambda ?old-scope ?env ?args ?body]
                                [::Expression [::lambda (list ?name $module) ?env ?args ?body] =value-type]
                                
                                _
                                =value))

                      _
                      (fail ""))
             =value-type (expr-type =value)
             _ (if ann??
                 (return nil)
                 (annotate ?name ::public false =value-type))
             _ (define ?name)]
        (return (list [::Statement [::def ?name =value]]))))))

(defn ^:private analyse-declare-macro [?ident]
  (exec [_ (annotate ?ident ::public true [::&type/Any])]
    (return (list))))

(defn ^:private analyse-require [analyse-ast ?path]
  (assert false)
  (return (list)))

(do-template [<name> <ident> <output-tag> <wrapper-class>]
  (defn <name> [analyse-ast ?x ?y]
    (exec [:let [=type [::&type/Data <wrapper-class>]]
           [=x =y] (analyse-2 ?x ?y)
           =x-type (expr-type =x)
           =y-type (expr-type =y)
           _ (&type/solve =type =x-type)
           _ (&type/solve =type =y-type)]
      (return (list [::Expression [<output-tag> =x =y] =type]))))

  ^:private analyse-jvm-iadd "jvm;iadd" ::jvm-iadd "java.lang.Integer"
  ^:private analyse-jvm-isub "jvm;isub" ::jvm-isub "java.lang.Integer"
  ^:private analyse-jvm-imul "jvm;imul" ::jvm-imul "java.lang.Integer"
  ^:private analyse-jvm-idiv "jvm;idiv" ::jvm-idiv "java.lang.Integer"
  ^:private analyse-jvm-irem "jvm;irem" ::jvm-irem "java.lang.Integer"

  ^:private analyse-jvm-ladd "jvm;ladd" ::jvm-ladd "java.lang.Long"
  ^:private analyse-jvm-lsub "jvm;lsub" ::jvm-lsub "java.lang.Long"
  ^:private analyse-jvm-lmul "jvm;lmul" ::jvm-lmul "java.lang.Long"
  ^:private analyse-jvm-ldiv "jvm;ldiv" ::jvm-ldiv "java.lang.Long"
  ^:private analyse-jvm-lrem "jvm;lrem" ::jvm-lrem "java.lang.Long"

  ^:private analyse-jvm-iadd "jvm;fadd" ::jvm-fadd "java.lang.Float"
  ^:private analyse-jvm-isub "jvm;fsub" ::jvm-fsub "java.lang.Float"
  ^:private analyse-jvm-imul "jvm;fmul" ::jvm-fmul "java.lang.Float"
  ^:private analyse-jvm-idiv "jvm;fdiv" ::jvm-fdiv "java.lang.Float"
  ^:private analyse-jvm-irem "jvm;frem" ::jvm-frem "java.lang.Float"

  ^:private analyse-jvm-iadd "jvm;dadd" ::jvm-dadd "java.lang.Double"
  ^:private analyse-jvm-isub "jvm;dsub" ::jvm-dsub "java.lang.Double"
  ^:private analyse-jvm-imul "jvm;dmul" ::jvm-dmul "java.lang.Double"
  ^:private analyse-jvm-idiv "jvm;ddiv" ::jvm-ddiv "java.lang.Double"
  ^:private analyse-jvm-irem "jvm;drem" ::jvm-drem "java.lang.Double"
  )

(defn ^:private analyse-jvm-getstatic [analyse-ast ?class ?field]
  (exec [=class (full-class-name ?class)
         =type (lookup-static-field =class ?field)]
    (return (list [::Expression [::jvm-getstatic =class ?field] =type]))))

(defn ^:private analyse-jvm-getfield [analyse-ast ?class ?field ?object]
  (exec [=class (full-class-name ?class)
         =type (lookup-static-field =class ?field)
         =object (analyse-1 ?object)]
    (return (list [::Expression [::jvm-getfield =class ?field =object] =type]))))

(defn ^:private analyse-jvm-invokestatic [analyse-ast ?class ?method ?classes ?args]
  (exec [=class (full-class-name ?class)
         =classes (map-m extract-jvm-param ?classes)
         =return (lookup-virtual-method =class ?method =classes)
         =args (mapcat-m analyse-ast ?args)]
    (return (list [::Expression [::jvm-invokestatic =class ?method =classes =args] =return]))))

(defn ^:private analyse-jvm-invokevirtual [analyse-ast ?class ?method ?classes ?object ?args]
  (exec [=class (full-class-name ?class)
         =classes (map-m extract-jvm-param ?classes)
         =return (lookup-virtual-method =class ?method =classes)
         =object (analyse-1 ?object)
         =args (mapcat-m analyse-ast ?args)]
    (return (list [::Expression [::jvm-invokevirtual =class ?method =classes =object =args] =return]))))

(defn ^:private analyse-jvm-new [analyse-ast ?class ?classes ?args]
  (exec [=class (full-class-name ?class)
         =classes (map-m extract-jvm-param ?classes)
         =args (mapcat-m analyse-ast ?args)]
    (return (list [::Expression [::jvm-new =class =classes =args] [::&type/Data =class]]))))

(defn ^:private analyse-jvm-new-array [analyse-ast ?class ?length]
  (exec [=class (full-class-name ?class)]
    (return (list [::Expression [::jvm-new-array =class ?length] [::&type/Array [::&type/Data =class]]]))))

(defn ^:private analyse-jvm-aastore [analyse-ast ?array ?idx ?elem]
  (exec [[=array =elem] (analyse-2 ?array ?elem)
         =array-type (expr-type =array)]
    (return (list [::Expression [::jvm-aastore =array ?idx =elem] =array-type]))))

(defn ^:private analyse-jvm-aaload [analyse-ast ?array ?idx]
  (exec [=array (analyse-1 ?array)
         =array-type (expr-type =array)]
    (return (list [::Expression [::jvm-aaload =array ?idx] =array-type]))))

(defn ^:private analyse-jvm-class [analyse-ast ?name ?super-class ?fields]
  (exec [?fields (map-m (fn [?field]
                          (match ?field
                            [::&parser/tuple ([[::&parser/ident ?class] [::&parser/ident ?field-name]] :seq)]
                            (return [?class ?field-name])
                            
                            _
                            (fail "")))
                        ?fields)
         :let [=fields (into {} (for [[class field] ?fields]
                                  [field {:access :public
                                          :type class}]))]
         $module &util/get-module-name]
    (return (list [::Statement [::jvm-class [$module ?name] ?super-class =fields {}]]))))

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
         :let [=methods (into {} (for [[method [inputs output]] ?members]
                                   [method {:access :public
                                            :type [inputs output]}]))]
         $module &util/get-module-name]
    (return (list [::Statement [::jvm-interface [$module ?name] {} =methods]]))))

(defn ^:private analyse-basic-ast [analyse-ast token]
  (match token
    ;; Standard special forms
    [::&parser/bool ?value]
    (return (list [::Expression [::bool ?value] [::&type/Data "java.lang.Boolean"]]))

    [::&parser/int ?value]
    (return (list [::Expression [::int ?value]  [::&type/Data "java.lang.Long"]]))

    [::&parser/real ?value]
    (return (list [::Expression [::real ?value] [::&type/Data "java.lang.Double"]]))

    [::&parser/char ?value]
    (return (list [::Expression [::char ?value] [::&type/Data "java.lang.Character"]]))

    [::&parser/text ?value]
    (return (list [::Expression [::text ?value] [::&type/Data "java.lang.String"]]))

    [::&parser/tuple ?elems]
    (analyse-tuple analyse-ast ?elems)

    [::&parser/tag ?tag]
    (return (list [::Expression [::variant ?tag (list)] [::&type/Variant {?tag [::&type/Tuple (list)]}]]))

    [::&parser/ident ?ident]
    (analyse-ident analyse-ast ?ident)

    [::&parser/form ([[::&parser/ident "case'"] ?variant & ?branches] :seq)]
    (analyse-case analyse-ast ?variant ?branches)
    
    [::&parser/form ([[::&parser/ident "lambda'"] [::&parser/ident ?self] [::&parser/ident ?arg] ?body] :seq)]
    (analyse-lambda analyse-ast ?self ?arg ?body)

    [::&parser/form ([[::&parser/ident "def'"] [::&parser/ident ?name] ?value] :seq)]
    (analyse-def analyse-ast ?name ?value)

    [::&parser/form ([[::&parser/ident "declare-macro"] [::&parser/ident ?ident]] :seq)]
    (analyse-declare-macro ?ident)
    
    [::&parser/form ([[::&parser/ident "require"] [::&parser/text ?path]] :seq)]
    (analyse-require analyse-ast ?path)

    ;; Host special forms
    [::&parser/form ([[::&parser/ident "do"] & ?exprs] :seq)]
    (analyse-do analyse-ast ?exprs)

    ;; Integer arithmetic
    [::&parser/form ([[::&parser/ident "jvm;iadd"] ?x ?y] :seq)]
    (analyse-jvm-iadd analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;isub"] ?x ?y] :seq)]
    (analyse-jvm-isub analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;imul"] ?x ?y] :seq)]
    (analyse-jvm-imul analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;idiv"] ?x ?y] :seq)]
    (analyse-jvm-idiv analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;irem"] ?x ?y] :seq)]
    (analyse-jvm-irem analyse-ast ?x ?y)

    ;; Long arithmetic
    [::&parser/form ([[::&parser/ident "jvm;ladd"] ?x ?y] :seq)]
    (analyse-jvm-ladd analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;lsub"] ?x ?y] :seq)]
    (analyse-jvm-lsub analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;lmul"] ?x ?y] :seq)]
    (analyse-jvm-lmul analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;ldiv"] ?x ?y] :seq)]
    (analyse-jvm-ldiv analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;lrem"] ?x ?y] :seq)]
    (analyse-jvm-lrem analyse-ast ?x ?y)

    ;; Float arithmetic
    [::&parser/form ([[::&parser/ident "jvm;fadd"] ?x ?y] :seq)]
    (analyse-jvm-fadd analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;fsub"] ?x ?y] :seq)]
    (analyse-jvm-fsub analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;fmul"] ?x ?y] :seq)]
    (analyse-jvm-fmul analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;fdiv"] ?x ?y] :seq)]
    (analyse-jvm-fdiv analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;frem"] ?x ?y] :seq)]
    (analyse-jvm-frem analyse-ast ?x ?y)

    ;; Double arithmetic
    [::&parser/form ([[::&parser/ident "jvm;dadd"] ?x ?y] :seq)]
    (analyse-jvm-dadd analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;dsub"] ?x ?y] :seq)]
    (analyse-jvm-dsub analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;dmul"] ?x ?y] :seq)]
    (analyse-jvm-dmul analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;ddiv"] ?x ?y] :seq)]
    (analyse-jvm-ddiv analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;drem"] ?x ?y] :seq)]
    (analyse-jvm-drem analyse-ast ?x ?y)
    
    [::&parser/form ([[::&parser/ident "jvm;getstatic"] [::&parser/ident ?class] [::&parser/ident ?field]] :seq)]
    (analyse-jvm-getstatic analyse-ast ?class ?field)

    [::&parser/form ([[::&parser/ident "jvm;getfield"] [::&parser/ident ?class] [::&parser/ident ?field] ?object] :seq)]
    (analyse-jvm-getfield analyse-ast ?class ?field ?object)

    [::&parser/form ([[::&parser/ident "jvm;invokestatic"] [::&parser/ident ?class] [::&parser/text ?method] [::&parser/tuple ?classes] [::&parser/tuple ?args]] :seq)]
    (analyse-jvm-invokestatic analyse-ast ?class ?method ?classes ?args)

    [::&parser/form ([[::&parser/ident "jvm;invokevirtual"] [::&parser/ident ?class] [::&parser/text ?method] [::&parser/tuple ?classes] ?object [::&parser/tuple ?args]] :seq)]
    (analyse-jvm-invokevirtual analyse-ast ?class ?method ?classes ?object ?args)
    
    [::&parser/form ([[::&parser/ident "jvm;new"] [::&parser/ident ?class] [::&parser/tuple ?classes] [::&parser/tuple ?args]] :seq)]
    (analyse-jvm-new analyse-ast ?class ?classes ?args)

    [::&parser/form ([[::&parser/ident "jvm;new-array"] [::&parser/ident ?class] [::&parser/int ?length]] :seq)]
    (analyse-jvm-new-array analyse-ast ?class ?length)

    [::&parser/form ([[::&parser/ident "jvm;aastore"] ?array [::&parser/int ?idx] ?elem] :seq)]
    (analyse-jvm-aastore analyse-ast ?array ?idx ?elem)

    [::&parser/form ([[::&parser/ident "jvm;aaload"] ?array [::&parser/int ?idx]] :seq)]
    (analyse-jvm-aaload analyse-ast ?array ?idx)

    [::&parser/form ([[::&parser/ident "jvm;class"] [::&parser/ident ?name] [::&parser/ident ?super-class] [::&parser/tuple ?fields]] :seq)]
    (analyse-jvm-class analyse-ast ?name ?super-class ?fields)

    [::&parser/form ([[::&parser/ident "jvm;interface"] [::&parser/ident ?name] & ?members] :seq)]
    (analyse-jvm-interface analyse-ast ?name ?members)

    _
    (fail (str "[Analyser Error] Unmatched token: " token))))

(defn analyse-ast [token]
  ;; (prn 'analyse-ast token)
  (match token
    [::&parser/form ([[::&parser/tag ?tag] & ?data] :seq)]
    (exec [=data (mapcat-m analyse-ast ?data)
           ;; :let [_ (prn 'analyse-ast/variant+ ?tag '=data =data)]
           =data-types (map-m expr-type =data)]
      (return (list [::Expression [::variant ?tag =data] [::&type/Variant {?tag [::&type/Tuple =data-types]}]])))
    
    [::&parser/form ([?fn & ?args] :seq)]
    (try-all-m [(analyse-call analyse-ast ?fn ?args)
                (analyse-basic-ast analyse-ast token)])

    _
    (analyse-basic-ast analyse-ast token)))

(def analyse
  (exec [asts &parser/parse]
    (mapcat-m analyse-ast asts)))
