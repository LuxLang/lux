(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return fail
                                     try-all-m map-m mapcat-m reduce-m
                                     assert!]]
                 [parser :as &parser]
                 [type :as &type]
                 [macro :as &macro]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lux :as &&lux]
                          [host :as &&host])))

;; [Utils]
(defn ^:private analyse-basic-ast [analyse-ast token]
  ;; (prn 'analyse-basic-ast token)
  (match token
    ;; Standard special forms
    [::&parser/Bool ?value]
    (return (list [::&&/Expression [::&&/bool ?value] [::&type/Data "java.lang.Boolean"]]))

    [::&parser/Int ?value]
    (return (list [::&&/Expression [::&&/int ?value]  [::&type/Data "java.lang.Long"]]))

    [::&parser/Real ?value]
    (return (list [::&&/Expression [::&&/real ?value] [::&type/Data "java.lang.Double"]]))

    [::&parser/Char ?value]
    (return (list [::&&/Expression [::&&/char ?value] [::&type/Data "java.lang.Character"]]))

    [::&parser/Text ?value]
    (return (list [::&&/Expression [::&&/text ?value] [::&type/Data "java.lang.String"]]))

    [::&parser/Tuple ?elems]
    (&&lux/analyse-tuple analyse-ast ?elems)

    [::&parser/Tag ?tag]
    (let [tuple-type [::&type/Tuple (list)]]
      (return (list [::&&/Expression [::&&/variant ?tag [::&&/Expression [::&&/tuple (list)] tuple-type]]
                     [::&type/Variant (list [?tag tuple-type])]])))

    [::&parser/Ident ?ident]
    (&&lux/analyse-ident analyse-ast ?ident)

    [::&parser/Form ([[::&parser/Ident "case'"] ?variant & ?branches] :seq)]
    (&&lux/analyse-case analyse-ast ?variant ?branches)
    
    [::&parser/Form ([[::&parser/Ident "lambda'"] [::&parser/Ident ?self] [::&parser/Ident ?arg] ?body] :seq)]
    (&&lux/analyse-lambda analyse-ast ?self ?arg ?body)

    [::&parser/Form ([[::&parser/Ident "def'"] [::&parser/Ident ?name] ?value] :seq)]
    (&&lux/analyse-def analyse-ast ?name ?value)

    [::&parser/Form ([[::&parser/Ident "declare-macro"] [::&parser/Ident ?ident]] :seq)]
    (&&lux/analyse-declare-macro ?ident)
    
    [::&parser/Form ([[::&parser/Ident "require"] [::&parser/Text ?path]] :seq)]
    (&&lux/analyse-require analyse-ast ?path)

    ;; Host special forms
    [::&parser/Form ([[::&parser/Ident "exec"] & ?exprs] :seq)]
    (&&host/analyse-exec analyse-ast ?exprs)

    ;; Integer arithmetic
    [::&parser/Form ([[::&parser/Ident "jvm-iadd"] ?x ?y] :seq)]
    (&&host/analyse-jvm-iadd analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-isub"] ?x ?y] :seq)]
    (&&host/analyse-jvm-isub analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-imul"] ?x ?y] :seq)]
    (&&host/analyse-jvm-imul analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-idiv"] ?x ?y] :seq)]
    (&&host/analyse-jvm-idiv analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-irem"] ?x ?y] :seq)]
    (&&host/analyse-jvm-irem analyse-ast ?x ?y)

    ;; Long arithmetic
    [::&parser/Form ([[::&parser/Ident "jvm-ladd"] ?x ?y] :seq)]
    (&&host/analyse-jvm-ladd analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-lsub"] ?x ?y] :seq)]
    (&&host/analyse-jvm-lsub analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-lmul"] ?x ?y] :seq)]
    (&&host/analyse-jvm-lmul analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-ldiv"] ?x ?y] :seq)]
    (&&host/analyse-jvm-ldiv analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-lrem"] ?x ?y] :seq)]
    (&&host/analyse-jvm-lrem analyse-ast ?x ?y)

    ;; Float arithmetic
    [::&parser/Form ([[::&parser/Ident "jvm-fadd"] ?x ?y] :seq)]
    (&&host/analyse-jvm-fadd analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-fsub"] ?x ?y] :seq)]
    (&&host/analyse-jvm-fsub analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-fmul"] ?x ?y] :seq)]
    (&&host/analyse-jvm-fmul analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-fdiv"] ?x ?y] :seq)]
    (&&host/analyse-jvm-fdiv analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-frem"] ?x ?y] :seq)]
    (&&host/analyse-jvm-frem analyse-ast ?x ?y)

    ;; Double arithmetic
    [::&parser/Form ([[::&parser/Ident "jvm-dadd"] ?x ?y] :seq)]
    (&&host/analyse-jvm-dadd analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-dsub"] ?x ?y] :seq)]
    (&&host/analyse-jvm-dsub analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-dmul"] ?x ?y] :seq)]
    (&&host/analyse-jvm-dmul analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-ddiv"] ?x ?y] :seq)]
    (&&host/analyse-jvm-ddiv analyse-ast ?x ?y)

    [::&parser/Form ([[::&parser/Ident "jvm-drem"] ?x ?y] :seq)]
    (&&host/analyse-jvm-drem analyse-ast ?x ?y)

    ;; Fields & methods
    [::&parser/Form ([[::&parser/Ident "jvm-getstatic"] [::&parser/Ident ?class] [::&parser/Text ?field]] :seq)]
    (&&host/analyse-jvm-getstatic analyse-ast ?class ?field)

    [::&parser/Form ([[::&parser/Ident "jvm-getfield"] [::&parser/Ident ?class] [::&parser/Text ?field] ?object] :seq)]
    (&&host/analyse-jvm-getfield analyse-ast ?class ?field ?object)

    [::&parser/Form ([[::&parser/Ident "jvm-invokestatic"] [::&parser/Ident ?class] [::&parser/Text ?method] [::&parser/Tuple ?classes] [::&parser/Tuple ?args]] :seq)]
    (&&host/analyse-jvm-invokestatic analyse-ast ?class ?method ?classes ?args)

    [::&parser/Form ([[::&parser/Ident "jvm-invokevirtual"] [::&parser/Ident ?class] [::&parser/Text ?method] [::&parser/Tuple ?classes] ?object [::&parser/Tuple ?args]] :seq)]
    (&&host/analyse-jvm-invokevirtual analyse-ast ?class ?method ?classes ?object ?args)

    ;; Arrays
    [::&parser/Form ([[::&parser/Ident "jvm-new"] [::&parser/Ident ?class] [::&parser/Tuple ?classes] [::&parser/Tuple ?args]] :seq)]
    (&&host/analyse-jvm-new analyse-ast ?class ?classes ?args)

    [::&parser/Form ([[::&parser/Ident "jvm-new-array"] [::&parser/Ident ?class] [::&parser/Int ?length]] :seq)]
    (&&host/analyse-jvm-new-array analyse-ast ?class ?length)

    [::&parser/Form ([[::&parser/Ident "jvm-aastore"] ?array [::&parser/Int ?idx] ?elem] :seq)]
    (&&host/analyse-jvm-aastore analyse-ast ?array ?idx ?elem)

    [::&parser/Form ([[::&parser/Ident "jvm-aaload"] ?array [::&parser/Int ?idx]] :seq)]
    (&&host/analyse-jvm-aaload analyse-ast ?array ?idx)

    ;; Classes & interfaces
    [::&parser/Form ([[::&parser/Ident "jvm-class"] [::&parser/Ident ?name] [::&parser/Ident ?super-class] [::&parser/Tuple ?fields]] :seq)]
    (&&host/analyse-jvm-class analyse-ast ?name ?super-class ?fields)

    [::&parser/Form ([[::&parser/Ident "jvm-interface"] [::&parser/Ident ?name] & ?members] :seq)]
    (&&host/analyse-jvm-interface analyse-ast ?name ?members)

    _
    (fail (str "[Analyser Error] Unmatched token: " (pr-str token)))))

(defn ^:private analyse-ast [token]
  ;; (prn 'analyse-ast token)
  (match token
    [::&parser/Form ([[::&parser/Tag ?tag] & ?values] :seq)]
    (exec [;; :let [_ (prn 'PRE-ASSERT)]
           :let [_ (assert (= 1 (count ?values)) (str "[Analyser Error] Can only tag 1 value: " (pr-str token)))]
           ;; :let [_ (prn 'POST-ASSERT)]
           :let [?value (first ?values)]
           =value (&&/analyse-1 analyse-ast ?value)
           =value-type (&&/expr-type =value)]
      (return (list [::&&/Expression [::&&/variant ?tag =value] [::&type/Variant (list [?tag =value-type])]])))
    
    [::&parser/Form ([?fn & ?args] :seq)]
    (fn [state]
      (match ((&&/analyse-1 analyse-ast ?fn) state)
        [::&/ok [state* =fn]]
        ((&&lux/analyse-call analyse-ast =fn ?args) state*)

        _
        ((analyse-basic-ast analyse-ast token) state)))
    
    _
    (analyse-basic-ast analyse-ast token)))

;; [Resources]
(def analyse
  (exec [asts &parser/parse
         ;; :let [_ (prn 'analyse/asts asts)]
         ]
    (mapcat-m analyse-ast asts)))
