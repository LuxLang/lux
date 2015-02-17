(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return fail
                                         try-all-m map-m mapcat-m reduce-m
                                         assert!]]
                 [parser :as &parser]
                 [type :as &type]
                 [macro :as &macro]
                 [host :as &host])
            (lux.analyser [base :as &]
                          [lux :as &&lux]
                          [host :as &&host])))

;; [Utils]
(defn ^:private analyse-basic-ast [analyse-ast token]
  (match token
    ;; Standard special forms
    [::&parser/bool ?value]
    (return (list [::&/Expression [::bool ?value] [::&type/Data "java.lang.Boolean"]]))

    [::&parser/int ?value]
    (return (list [::&/Expression [::int ?value]  [::&type/Data "java.lang.Long"]]))

    [::&parser/real ?value]
    (return (list [::&/Expression [::real ?value] [::&type/Data "java.lang.Double"]]))

    [::&parser/char ?value]
    (return (list [::&/Expression [::char ?value] [::&type/Data "java.lang.Character"]]))

    [::&parser/text ?value]
    (return (list [::&/Expression [::text ?value] [::&type/Data "java.lang.String"]]))

    [::&parser/tuple ?elems]
    (&&lux/analyse-tuple analyse-ast ?elems)

    [::&parser/tag ?tag]
    (return (list [::&/Expression [::variant ?tag (list)] [::&type/Variant {?tag [::&type/Tuple (list)]}]]))

    [::&parser/ident ?ident]
    (&&lux/analyse-ident analyse-ast ?ident)

    [::&parser/form ([[::&parser/ident "case'"] ?variant & ?branches] :seq)]
    (&&lux/analyse-case analyse-ast ?variant ?branches)
    
    [::&parser/form ([[::&parser/ident "lambda'"] [::&parser/ident ?self] [::&parser/ident ?arg] ?body] :seq)]
    (&&lux/analyse-lambda analyse-ast ?self ?arg ?body)

    [::&parser/form ([[::&parser/ident "def'"] [::&parser/ident ?name] ?value] :seq)]
    (&&lux/analyse-def analyse-ast ?name ?value)

    [::&parser/form ([[::&parser/ident "declare-macro"] [::&parser/ident ?ident]] :seq)]
    (&&lux/analyse-declare-macro ?ident)
    
    [::&parser/form ([[::&parser/ident "require"] [::&parser/text ?path]] :seq)]
    (&&lux/analyse-require analyse-ast ?path)

    ;; Host special forms
    [::&parser/form ([[::&parser/ident "exec"] & ?exprs] :seq)]
    (&&host/analyse-exec analyse-ast ?exprs)

    ;; Integer arithmetic
    [::&parser/form ([[::&parser/ident "jvm;iadd"] ?x ?y] :seq)]
    (&&host/analyse-jvm-iadd analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;isub"] ?x ?y] :seq)]
    (&&host/analyse-jvm-isub analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;imul"] ?x ?y] :seq)]
    (&&host/analyse-jvm-imul analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;idiv"] ?x ?y] :seq)]
    (&&host/analyse-jvm-idiv analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;irem"] ?x ?y] :seq)]
    (&&host/analyse-jvm-irem analyse-ast ?x ?y)

    ;; Long arithmetic
    [::&parser/form ([[::&parser/ident "jvm;ladd"] ?x ?y] :seq)]
    (&&host/analyse-jvm-ladd analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;lsub"] ?x ?y] :seq)]
    (&&host/analyse-jvm-lsub analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;lmul"] ?x ?y] :seq)]
    (&&host/analyse-jvm-lmul analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;ldiv"] ?x ?y] :seq)]
    (&&host/analyse-jvm-ldiv analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;lrem"] ?x ?y] :seq)]
    (&&host/analyse-jvm-lrem analyse-ast ?x ?y)

    ;; Float arithmetic
    [::&parser/form ([[::&parser/ident "jvm;fadd"] ?x ?y] :seq)]
    (&&host/analyse-jvm-fadd analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;fsub"] ?x ?y] :seq)]
    (&&host/analyse-jvm-fsub analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;fmul"] ?x ?y] :seq)]
    (&&host/analyse-jvm-fmul analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;fdiv"] ?x ?y] :seq)]
    (&&host/analyse-jvm-fdiv analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;frem"] ?x ?y] :seq)]
    (&&host/analyse-jvm-frem analyse-ast ?x ?y)

    ;; Double arithmetic
    [::&parser/form ([[::&parser/ident "jvm;dadd"] ?x ?y] :seq)]
    (&&host/analyse-jvm-dadd analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;dsub"] ?x ?y] :seq)]
    (&&host/analyse-jvm-dsub analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;dmul"] ?x ?y] :seq)]
    (&&host/analyse-jvm-dmul analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;ddiv"] ?x ?y] :seq)]
    (&&host/analyse-jvm-ddiv analyse-ast ?x ?y)

    [::&parser/form ([[::&parser/ident "jvm;drem"] ?x ?y] :seq)]
    (&&host/analyse-jvm-drem analyse-ast ?x ?y)
    
    [::&parser/form ([[::&parser/ident "jvm;getstatic"] [::&parser/ident ?class] [::&parser/ident ?field]] :seq)]
    (&&host/analyse-jvm-getstatic analyse-ast ?class ?field)

    [::&parser/form ([[::&parser/ident "jvm;getfield"] [::&parser/ident ?class] [::&parser/ident ?field] ?object] :seq)]
    (&&host/analyse-jvm-getfield analyse-ast ?class ?field ?object)

    [::&parser/form ([[::&parser/ident "jvm;invokestatic"] [::&parser/ident ?class] [::&parser/text ?method] [::&parser/tuple ?classes] [::&parser/tuple ?args]] :seq)]
    (&&host/analyse-jvm-invokestatic analyse-ast ?class ?method ?classes ?args)

    [::&parser/form ([[::&parser/ident "jvm;invokevirtual"] [::&parser/ident ?class] [::&parser/text ?method] [::&parser/tuple ?classes] ?object [::&parser/tuple ?args]] :seq)]
    (&&host/analyse-jvm-invokevirtual analyse-ast ?class ?method ?classes ?object ?args)
    
    [::&parser/form ([[::&parser/ident "jvm;new"] [::&parser/ident ?class] [::&parser/tuple ?classes] [::&parser/tuple ?args]] :seq)]
    (&&host/analyse-jvm-new analyse-ast ?class ?classes ?args)

    [::&parser/form ([[::&parser/ident "jvm;new-array"] [::&parser/ident ?class] [::&parser/int ?length]] :seq)]
    (&&host/analyse-jvm-new-array analyse-ast ?class ?length)

    [::&parser/form ([[::&parser/ident "jvm;aastore"] ?array [::&parser/int ?idx] ?elem] :seq)]
    (&&host/analyse-jvm-aastore analyse-ast ?array ?idx ?elem)

    [::&parser/form ([[::&parser/ident "jvm;aaload"] ?array [::&parser/int ?idx]] :seq)]
    (&&host/analyse-jvm-aaload analyse-ast ?array ?idx)

    [::&parser/form ([[::&parser/ident "jvm;class"] [::&parser/ident ?name] [::&parser/ident ?super-class] [::&parser/tuple ?fields]] :seq)]
    (&&host/analyse-jvm-class analyse-ast ?name ?super-class ?fields)

    [::&parser/form ([[::&parser/ident "jvm;interface"] [::&parser/ident ?name] & ?members] :seq)]
    (&&host/analyse-jvm-interface analyse-ast ?name ?members)

    _
    (fail (str "[Analyser Error] Unmatched token: " token))))

(defn ^:private analyse-ast [token]
  (match token
    [::&parser/form ([[::&parser/tag ?tag] & ?data] :seq)]
    (exec [=data (mapcat-m analyse-ast ?data)
           =data-types (map-m &/expr-type =data)]
      (return (list [::&/Expression [::variant ?tag =data] [::&type/Variant {?tag [::&type/Tuple =data-types]}]])))
    
    [::&parser/form ([?fn & ?args] :seq)]
    (try-all-m [(&&lux/analyse-call analyse-ast ?fn ?args)
                (analyse-basic-ast analyse-ast token)])

    _
    (analyse-basic-ast analyse-ast token)))

;; [Resources]
(def analyse
  (exec [asts &parser/parse]
    (mapcat-m analyse-ast asts)))
