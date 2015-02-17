(ns lux.analyser.host
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return fail
                                         try-all-m map-m mapcat-m reduce-m
                                         assert!]]
                 [parser :as &parser]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &])))

;; [Utils]
(defn ^:private extract-ident [ident]
  (match ident
    [::&parser/ident ?ident]
    (return ?ident)

    _
    (fail "")))

;; [Resources]
(do-template [<name> <ident> <output-tag> <wrapper-class>]
  (defn <name> [analyse ?x ?y]
    (exec [:let [=type [::&type/Data <wrapper-class>]]
           [=x =y] (&/analyse-2 analyse ?x ?y)
           =x-type (&/expr-type =x)
           =y-type (&/expr-type =y)
           _ (&type/solve =type =x-type)
           _ (&type/solve =type =y-type)]
      (return (list [::&/Expression [<output-tag> =x =y] =type]))))

  analyse-jvm-iadd "jvm;iadd" ::jvm-iadd "java.lang.Integer"
  analyse-jvm-isub "jvm;isub" ::jvm-isub "java.lang.Integer"
  analyse-jvm-imul "jvm;imul" ::jvm-imul "java.lang.Integer"
  analyse-jvm-idiv "jvm;idiv" ::jvm-idiv "java.lang.Integer"
  analyse-jvm-irem "jvm;irem" ::jvm-irem "java.lang.Integer"

  analyse-jvm-ladd "jvm;ladd" ::jvm-ladd "java.lang.Long"
  analyse-jvm-lsub "jvm;lsub" ::jvm-lsub "java.lang.Long"
  analyse-jvm-lmul "jvm;lmul" ::jvm-lmul "java.lang.Long"
  analyse-jvm-ldiv "jvm;ldiv" ::jvm-ldiv "java.lang.Long"
  analyse-jvm-lrem "jvm;lrem" ::jvm-lrem "java.lang.Long"

  analyse-jvm-fadd "jvm;fadd" ::jvm-fadd "java.lang.Float"
  analyse-jvm-fsub "jvm;fsub" ::jvm-fsub "java.lang.Float"
  analyse-jvm-fmul "jvm;fmul" ::jvm-fmul "java.lang.Float"
  analyse-jvm-fdiv "jvm;fdiv" ::jvm-fdiv "java.lang.Float"
  analyse-jvm-frem "jvm;frem" ::jvm-frem "java.lang.Float"

  analyse-jvm-dadd "jvm;dadd" ::jvm-dadd "java.lang.Double"
  analyse-jvm-dsub "jvm;dsub" ::jvm-dsub "java.lang.Double"
  analyse-jvm-dmul "jvm;dmul" ::jvm-dmul "java.lang.Double"
  analyse-jvm-ddiv "jvm;ddiv" ::jvm-ddiv "java.lang.Double"
  analyse-jvm-drem "jvm;drem" ::jvm-drem "java.lang.Double"
  )

(defn analyse-jvm-getstatic [analyse ?class ?field]
  (exec [=class (&host/full-class-name ?class)
         =type (&host/lookup-static-field =class ?field)]
    (return (list [::&/Expression [::jvm-getstatic =class ?field] =type]))))

(defn analyse-jvm-getfield [analyse ?class ?field ?object]
  (exec [=class (&host/full-class-name ?class)
         =type (&host/lookup-static-field =class ?field)
         =object (&/analyse-1 ?object)]
    (return (list [::&/Expression [::jvm-getfield =class ?field =object] =type]))))

(defn analyse-jvm-invokestatic [analyse ?class ?method ?classes ?args]
  (exec [=class (&host/full-class-name ?class)
         =classes (map-m &host/extract-jvm-param ?classes)
         =return (&host/lookup-virtual-method =class ?method =classes)
         =args (mapcat-m analyse ?args)]
    (return (list [::&/Expression [::jvm-invokestatic =class ?method =classes =args] =return]))))

(defn analyse-jvm-invokevirtual [analyse ?class ?method ?classes ?object ?args]
  (exec [=class (&host/full-class-name ?class)
         =classes (map-m &host/extract-jvm-param ?classes)
         =return (&host/lookup-virtual-method =class ?method =classes)
         =object (&/analyse-1 ?object)
         =args (mapcat-m analyse ?args)]
    (return (list [::&/Expression [::jvm-invokevirtual =class ?method =classes =object =args] =return]))))

(defn analyse-jvm-new [analyse ?class ?classes ?args]
  (exec [=class (&host/full-class-name ?class)
         =classes (map-m &host/extract-jvm-param ?classes)
         =args (mapcat-m analyse ?args)]
    (return (list [::&/Expression [::jvm-new =class =classes =args] [::&type/Data =class]]))))

(defn analyse-jvm-new-array [analyse ?class ?length]
  (exec [=class (&host/full-class-name ?class)]
    (return (list [::&/Expression [::jvm-new-array =class ?length] [::&type/Array [::&type/Data =class]]]))))

(defn analyse-jvm-aastore [analyse ?array ?idx ?elem]
  (exec [[=array =elem] (&/analyse-2 ?array ?elem)
         =array-type (&/expr-type =array)]
    (return (list [::&/Expression [::jvm-aastore =array ?idx =elem] =array-type]))))

(defn analyse-jvm-aaload [analyse ?array ?idx]
  (exec [=array (&/analyse-1 ?array)
         =array-type (&/expr-type =array)]
    (return (list [::&/Expression [::jvm-aaload =array ?idx] =array-type]))))

(defn analyse-jvm-class [analyse ?name ?super-class ?fields]
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
    (return (list [::&/Statement [::jvm-class [$module ?name] ?super-class =fields {}]]))))

(defn analyse-jvm-interface [analyse ?name ?members]
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
    (return (list [::&/Statement [::jvm-interface [$module ?name] {} =methods]]))))

(defn analyse-exec [analyse ?exprs]
  (exec [_ (assert! (count ?exprs) "\"exec\" expressions can't have empty bodies.")
         =exprs (mapcat-m analyse ?exprs)
         =exprs-types (map-m &/expr-type =exprs)]
    (return (list [::&/Expression [::do =exprs] (last =exprs-types)]))))
