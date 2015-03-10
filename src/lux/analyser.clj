(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
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
  ;; (prn 'analyse-basic-ast token (&/show-ast token))
  (matchv ::M/objects [token]
    ;; Standard special forms
    [["Bool" ?value]]
    (return (list [::&&/Expression [::&&/bool ?value] [::&type/Data "java.lang.Boolean"]]))

    [["Int" ?value]]
    (return (list [::&&/Expression [::&&/int ?value]  [::&type/Data "java.lang.Long"]]))

    [["Real" ?value]]
    (return (list [::&&/Expression [::&&/real ?value] [::&type/Data "java.lang.Double"]]))

    [["Char" ?value]]
    (return (list [::&&/Expression [::&&/char ?value] [::&type/Data "java.lang.Character"]]))

    [["Text" ?value]]
    (return (list [::&&/Expression [::&&/text ?value] [::&type/Data "java.lang.String"]]))

    [["Tuple" ?elems]]
    (&&lux/analyse-tuple analyse-ast (&/->seq ?elems))

    [["Record" ?elems]]
    (&&lux/analyse-record analyse-ast (&/->seq ?elems))

    [["Tag" ?tag]]
    (let [tuple-type [::&type/Tuple (list)]]
      (return (list [::&&/Expression [::&&/variant ?tag [::&&/Expression [::&&/tuple (list)] tuple-type]]
                     [::&type/Variant (list [?tag tuple-type])]])))

    [["Ident" ?ident]]
    (&&lux/analyse-ident analyse-ast ?ident)

    [["Form" ["Cons" [["Ident" "case'"]
                      ["Cons" [?variant ?branches]]]]]]
    (&&lux/analyse-case analyse-ast ?variant (&/->seq ?branches))
    
    [["Form" ["Cons" [["Ident" "lambda'"]
                      ["Cons" [["Ident" ?self]
                               ["Cons" [["Ident" ?arg]
                                        ["Cons" [?body
                                                 ["Nil" _]]]]]]]]]]]
    (&&lux/analyse-lambda analyse-ast ?self ?arg ?body)

    [["Form" ["Cons" [["Ident" "get@'"] ["Cons" [["Tag" ?slot] ["Cons" [?record ["Nil" _]]]]]]]]]
    (&&lux/analyse-get analyse-ast ?slot ?record)

    [["Form" ["Cons" [["Ident" "set@'"] ["Cons" [["Tag" ?slot] ["Cons" [?value ["Cons" [?record ["Nil" _]]]]]]]]]]]
    (&&lux/analyse-set analyse-ast ?slot ?value ?record)

    [["Form" ["Cons" [["Ident" "def'"] ["Cons" [["Ident" ?name] ["Cons" [?value ["Nil" _]]]]]]]]]
    (&&lux/analyse-def analyse-ast ?name ?value)

    [["Form" ["Cons" [["Ident" "declare-macro"] ["Cons" [["Ident" ?ident] ["Nil" _]]]]]]]
    (&&lux/analyse-declare-macro ?ident)
    
    [["Form" ["Cons" [["Ident" "import'"] ["Cons" [["Text" ?path] ["Nil" _]]]]]]]
    (&&lux/analyse-import analyse-ast ?path)

    ;; Host special forms
    [["Form" ["Cons" [["Ident" "exec"] ?exprs]]]]
    (&&host/analyse-exec analyse-ast (&/->seq ?exprs))

    ;; Integer arithmetic
    [["Form" ["Cons" [["Ident" "jvm-iadd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-iadd analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-isub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-isub analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-imul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-imul analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-idiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-idiv analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-irem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-irem analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-ieq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ieq analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-ilt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ilt analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-igt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-igt analyse-ast ?x ?y)

    ;; Long arithmetic
    [["Form" ["Cons" [["Ident" "jvm-ladd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ladd analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lsub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lsub analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lmul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lmul analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-ldiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ldiv analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lrem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lrem analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-leq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-leq analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-llt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-llt analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lgt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lgt analyse-ast ?x ?y)

    ;; Float arithmetic
    [["Form" ["Cons" [["Ident" "jvm-fadd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fadd analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-fsub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fsub analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-fmul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fmul analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-fdiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fdiv analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-frem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-frem analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-feq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-feq analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-flt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-flt analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-fgt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fgt analyse-ast ?x ?y)

    ;; Double arithmetic
    [["Form" ["Cons" [["Ident" "jvm-dadd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dadd analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-dsub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dsub analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-dmul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dmul analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-ddiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ddiv analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-drem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-drem analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-deq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-deq analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-dlt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dlt analyse-ast ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-dgt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dgt analyse-ast ?x ?y)

    ;; Objects
    [["Form" ["Cons" [["Ident" "jvm-new"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Tuple" ?classes]
                                        ["Cons" [["Tuple" ?args]
                                                 ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-new analyse-ast ?class ?classes ?args)

    
    [["Form" ["Cons" [["Ident" "jvm-getstatic"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-getstatic analyse-ast ?class ?field)

    [["Form" ["Cons" [["Ident" "jvm-getfield"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Cons" [?object
                                                 ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-getfield analyse-ast ?class ?field ?object)

    [["Form" ["Cons" [["Ident" "jvm-invokestatic"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [["Tuple" ?args]
                                                          ["Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokestatic analyse-ast ?class ?method (&/->seq ?classes) (&/->seq ?args))

    [["Form" ["Cons" [["Ident" "jvm-invokevirtual"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [?object
                                                          ["Cons" [["Tuple" ?args]
                                                                   ["Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokevirtual analyse-ast ?class ?method (&/->seq ?classes) ?object (&/->seq ?args))
    
    ;; Arrays
    [["Form" ["Cons" [["Ident" "jvm-new-array"] ["Cons" [["Ident" ?class] ["Cons" [["Int" ?length] ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-new-array analyse-ast ?class ?length)

    [["Form" ["Cons" [["Ident" "jvm-aastore"] ["Cons" [?array ["Cons" [["Int" ?idx] ["Cons" [?elem ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-aastore analyse-ast ?array ?idx ?elem)

    [["Form" ["Cons" [["Ident" "jvm-aaload"] ["Cons" [?array ["Cons" [["Int" ?idx] ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-aaload analyse-ast ?array ?idx)

    ;; Classes & interfaces
    [["Form" ["Cons" [["Ident" "jvm-class"] ["Cons" [["Ident" ?name] ["Cons" [["Ident" ?super-class] ["Cons" [["Tuple" ?fields] ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-class analyse-ast ?name ?super-class (&/->seq ?fields))

    [["Form" ["Cons" [["Ident" "jvm-interface"] ["Cons" [["Ident" ?name] ?members]]]]]]
    (&&host/analyse-jvm-interface analyse-ast ?name ?members)

    [_]
    (fail (str "[Analyser Error] Unmatched token: " (&/show-ast token)))))

(defn ^:private analyse-ast [token]
  ;; (prn 'analyse-ast token)
  (matchv ::M/objects [token]
    [["Form" ["Cons" [["Tag" ?tag] ?values]]]]
    (exec [;; :let [_ (prn 'PRE-ASSERT)]
           :let [?values (&/->seq ?values)]
           :let [_ (assert (= 1 (count ?values)) (str "[Analyser Error] Can only tag 1 value: " (pr-str token)))]
           ;; :let [_ (prn 'POST-ASSERT)]
           =value (&&/analyse-1 analyse-ast (first ?values))
           =value-type (&&/expr-type =value)]
      (return (list [::&&/Expression [::&&/variant ?tag =value] [::&type/Variant (list [?tag =value-type])]])))
    
    [["Form" ["Cons" [?fn ?args]]]]
    (fn [state]
      (match ((&&/analyse-1 analyse-ast ?fn) state)
        [::&/ok [state* =fn]]
        ((&&lux/analyse-call analyse-ast =fn ?args) state*)

        _
        ((analyse-basic-ast analyse-ast token) state)))
    
    [_]
    (analyse-basic-ast analyse-ast token)))

;; [Resources]
(def analyse
  (exec [asts &parser/parse
         ;; :let [_ (prn 'analyse/asts asts)]
         ]
    (mapcat-m analyse-ast asts)))
