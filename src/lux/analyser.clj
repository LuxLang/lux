(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-all-m map-m mapcat-m reduce-m
                                         normalize-ident]]
                 [parser :as &parser]
                 [type :as &type]
                 [macro :as &macro]
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

(defn ^:private with-local [name mode type body]
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

(defn ^:private with-locals [locals monad]
  (reduce (fn [inner [label elem]]
            (with-local label :local elem inner))
          monad
          (reverse locals)))

(def ^:private captured-vars
  (fn [state]
    [::&util/ok [state (-> state ::&util/local-envs first :closure :mappings)]]))

(defn ^:private analyse-1 [elem]
  (exec [output (analyse-ast elem)]
    (match output
      ([x] :seq)
      (return x)

      :else
      (fail "[Analyser Error] Can't expand to other than 1 element."))))

(defn ^:private analyse-2 [el1 el2]
  (exec [output (mapcat-m analyse-ast (list el1 el2))]
    (match output
      ([x y] :seq)
      (return [x y])

      :else
      (fail "[Analyser Error] Can't expand to other than 2 elements."))))

(defn ^:private with-lambda [self self-type arg arg-type body]
  (fn [state]
    (let [body* (with-env (-> state ::&util/local-envs first :inner-closures str)
                  (exec [$scope &util/get-scope]
                    (with-local self :self self-type
                      (with-local arg :local arg-type
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
      [register* (update-in frame [:closure] #(-> %
                                                  (update-in [:counter] inc)
                                                  (assoc-in [:mappings ident] register*)))])))

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
            (if-let [global (get-in state [::&util/global-env ident])]
              [::&util/ok [state (list global)]]
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
            (let [macro-class (&host/location (list ?name ?module))
                  [macro-expansion state*] (&macro/expand loader macro-class ?args)]
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

(defn ^:private locals [member]
  (match member
    [::&parser/Ident ?name]
    (list ?name)

    [::&parser/Tuple ?submembers]
    (mapcat locals ?submembers)

    [::&parser/Form ([[::&parser/Tag _] & ?submembers] :seq)]
    (mapcat locals ?submembers)

    _
    (list)))

(defn ^:private analyse-branch [max-registers [bindings body]]
  (reduce (fn [body* name]
            (with-local name :local +dont-care-type+ body*))
          (reduce (fn [body* _]
                    (with-local "#" :local +dont-care-type+ body*))
                  (analyse-1 body)
                  (range (- max-registers (count bindings))))
          bindings))

(defn ^:private analyse-case [analyse-ast ?variant ?branches]
  (exec [=variant (analyse-1 ?variant)
         _ (assert! (and (> (count ?branches) 0) (even? (count ?branches)))
                    "Unbalanced branches in \"case'\" expression.")
         :let [branches (partition 2 ?branches)
               locals-per-branch (map locals (map first branches))
               max-locals (reduce max 0 (map count locals-per-branch))]
         base-register next-local-idx
         =bodies (map-m (partial analyse-branch max-locals)
                        (map vector locals-per-branch (map second branches)))
         =body-types (map-m expr-type =bodies)
         =case-type (reduce-m &type/merge [::&type/Nothing] =body-types)
         :let [=branches (map vector (map first branches) =bodies)]]
    (return (list [::Expression [::case =variant base-register max-locals =branches] =case-type]))))

(defn ^:private raise-expr [arg syntax]
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
      [::Expression [::tuple (map (partial raise-expr arg) ?members)] ?type]

      [::variant ?tag ?members]
      [::Expression [::variant ?tag (map (partial raise-expr arg) ?members)] ?type]
      
      [::local ?idx]
      [::Expression [::local (inc ?idx)] ?type]
      
      [::captured _ _ ?source]
      ?source

      [::self ?curried]
      [::Expression [::self (cons arg (map (partial raise-expr arg) ?curried))] ?type]

      [::global _ _]
      syntax

      [::case ?variant ?base ?num-bindings ?pm-struct]
      ...

      [::lambda ?scope ?captured ?args ?value]
      [::Expression [::lambda (pop ?scope)
                     (into {} (for [[?name ?sub-syntax] ?captured]
                                [?name (raise-expr arg ?sub-syntax)]))
                     ?args
                     ?value]
       ?type]

      [::call ?func ?args]
      [::Expression [::call (raise-expr arg ?func) (map (partial raise-expr arg) ?args)] ?type]

      [::do ?asts]
      [::Expression [::do (map (partial raise-expr arg) ?asts)] ?type]

      [::jvm-getstatic _ _]
      syntax
      
      [::jvm-invokevirtual ?class ?method ?arg-classes ?obj ?args]
      [::Expression [::jvm-invokevirtual ?class ?method ?arg-classes
                     (raise-expr arg ?obj)
                     (map (partial raise-expr arg) ?args)]
       ?type]

      ;; Integer arithmetic
      [::jvm-iadd ?x ?y]
      [::Expression [::jvm-iadd (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-isub ?x ?y]
      [::Expression [::jvm-isub (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-imul ?x ?y]
      [::Expression [::jvm-imul (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-idiv ?x ?y]
      [::Expression [::jvm-idiv (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-irem ?x ?y]
      [::Expression [::jvm-irem (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      ;; Long arithmetic
      [::jvm-ladd ?x ?y]
      [::Expression [::jvm-ladd (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-lsub ?x ?y]
      [::Expression [::jvm-lsub (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-lmul ?x ?y]
      [::Expression [::jvm-lmul (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-ldiv ?x ?y]
      [::Expression [::jvm-ldiv (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-lrem ?x ?y]
      [::Expression [::jvm-lrem (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      ;; Float arithmetic
      [::jvm-fadd ?x ?y]
      [::Expression [::jvm-fadd (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-fsub ?x ?y]
      [::Expression [::jvm-fsub (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-fmul ?x ?y]
      [::Expression [::jvm-fmul (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-fdiv ?x ?y]
      [::Expression [::jvm-fdiv (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-frem ?x ?y]
      [::Expression [::jvm-frem (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      ;; Double arithmetic
      [::jvm-dadd ?x ?y]
      [::Expression [::jvm-dadd (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-dsub ?x ?y]
      [::Expression [::jvm-dsub (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-dmul ?x ?y]
      [::Expression [::jvm-dmul (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-ddiv ?x ?y]
      [::Expression [::jvm-ddiv (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      [::jvm-drem ?x ?y]
      [::Expression [::jvm-drem (raise-expr arg ?x) (raise-expr arg ?y)] ?type]

      _
      (assert false syntax)
      )))

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
