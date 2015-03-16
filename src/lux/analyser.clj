(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return fail
                                     |list
                                     try-all-m map-m |flat-map% reduce-m
                                     assert!]]
                 [parser :as &parser]
                 [type :as &type]
                 [macro :as &macro]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [lux :as &&lux]
                          [host :as &&host])))

;; [Utils]
(defn ^:private parse-handler [[catch+ finally+] token]
  (matchv ::M/objects [token]
    [["Form" ["Cons" [["Ident" "jvm-catch"]
                      ["Cons" [["Ident" ?ex-class]
                               ["Cons" [["Ident" ?ex-arg]
                                        ["Cons" [?catch-body
                                                 ["Nil" _]]]]]]]]]]]
    [(concat catch+ (list [?ex-class ?ex-arg ?catch-body])) finally+]

    [["Form" ["Cons" [["Ident" "jvm-finally"]
                      ["Cons" [?finally-body
                               ["Nil" _]]]]]]]
    [catch+ ?finally-body]))

(defn ^:private analyse-basic-ast [analyse eval! token]
  ;; (prn 'analyse-basic-ast token (&/show-ast token))
  (matchv ::M/objects [token]
    ;; Standard special forms
    [["Bool" ?value]]
    (return (|list [::&&/Expression [::&&/bool ?value] (&/V "Data" (to-array ["java.lang.Boolean" (&/V "Nil" nil)]))]))

    [["Int" ?value]]
    (return (|list [::&&/Expression [::&&/int ?value]  (&/V "Data" (to-array ["java.lang.Long" (&/V "Nil" nil)]))]))

    [["Real" ?value]]
    (return (|list [::&&/Expression [::&&/real ?value] (&/V "Data" (to-array ["java.lang.Double" (&/V "Nil" nil)]))]))

    [["Char" ?value]]
    (return (|list [::&&/Expression [::&&/char ?value] (&/V "Data" (to-array ["java.lang.Character" (&/V "Nil" nil)]))]))

    [["Text" ?value]]
    (return (|list [::&&/Expression [::&&/text ?value] (&/V "Data" (to-array ["java.lang.String" (&/V "Nil" nil)]))]))

    [["Tuple" ?elems]]
    (&&lux/analyse-tuple analyse ?elems)

    [["Record" ?elems]]
    (&&lux/analyse-record analyse ?elems)

    [["Tag" ?tag]]
    (let [tuple-type (&/V "Tuple" (&/V "Nil" nil))]
      (return (|list [::&&/Expression [::&&/variant ?tag [::&&/Expression [::&&/tuple (list)] tuple-type]]
                     (&/V "Variant" (&/V "Cons" (to-array [(to-array [?tag tuple-type]) (&/V "Nil" nil)])))])))

    [["Ident" "jvm-null"]]
    (return (|list [::&&/Expression [::&&/jvm-null] (&/V "Data" (to-array ["null" (&/V "Nil" nil)]))]))
    
    [["Ident" ?ident]]
    (&&lux/analyse-ident analyse ?ident)

    [["Form" ["Cons" [["Ident" "case'"]
                      ["Cons" [?variant ?branches]]]]]]
    (&&lux/analyse-case analyse ?variant (&/->seq ?branches))
    
    [["Form" ["Cons" [["Ident" "lambda'"]
                      ["Cons" [["Ident" ?self]
                               ["Cons" [["Ident" ?arg]
                                        ["Cons" [?body
                                                 ["Nil" _]]]]]]]]]]]
    (&&lux/analyse-lambda analyse ?self ?arg ?body)

    [["Form" ["Cons" [["Ident" "get@'"] ["Cons" [["Tag" ?slot] ["Cons" [?record ["Nil" _]]]]]]]]]
    (&&lux/analyse-get analyse ?slot ?record)

    [["Form" ["Cons" [["Ident" "set@'"] ["Cons" [["Tag" ?slot] ["Cons" [?value ["Cons" [?record ["Nil" _]]]]]]]]]]]
    (&&lux/analyse-set analyse ?slot ?value ?record)

    [["Form" ["Cons" [["Ident" "def'"] ["Cons" [["Ident" ?name] ["Cons" [?value ["Nil" _]]]]]]]]]
    (&&lux/analyse-def analyse ?name ?value)

    [["Form" ["Cons" [["Ident" "declare-macro"] ["Cons" [["Ident" ?ident] ["Nil" _]]]]]]]
    (&&lux/analyse-declare-macro ?ident)
    
    [["Form" ["Cons" [["Ident" "import'"] ["Cons" [["Text" ?path] ["Nil" _]]]]]]]
    (&&lux/analyse-import analyse ?path)

    [["Form" ["Cons" [["Ident" ":"] ["Cons" [?value ["Cons" [?type ["Nil" _]]]]]]]]]
    (&&lux/analyse-check analyse eval! ?type ?value)

    [["Form" ["Cons" [["Ident" "coerce"] ["Cons" [?type ["Cons" [?value ["Nil" _]]]]]]]]]
    (&&lux/analyse-coerce analyse eval! ?type ?value)

    ;; Host special forms
    [["Form" ["Cons" [["Ident" "exec"] ?exprs]]]]
    (&&host/analyse-exec analyse (&/->seq ?exprs))

    ;; Integer arithmetic
    [["Form" ["Cons" [["Ident" "jvm-iadd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-iadd analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-isub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-isub analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-imul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-imul analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-idiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-idiv analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-irem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-irem analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-ieq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ieq analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-ilt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ilt analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-igt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-igt analyse ?x ?y)

    ;; Long arithmetic
    [["Form" ["Cons" [["Ident" "jvm-ladd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ladd analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lsub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lsub analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lmul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lmul analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-ldiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ldiv analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lrem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lrem analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-leq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-leq analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-llt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-llt analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lgt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lgt analyse ?x ?y)

    ;; Float arithmetic
    [["Form" ["Cons" [["Ident" "jvm-fadd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fadd analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-fsub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fsub analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-fmul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fmul analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-fdiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fdiv analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-frem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-frem analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-feq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-feq analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-flt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-flt analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-fgt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fgt analyse ?x ?y)

    ;; Double arithmetic
    [["Form" ["Cons" [["Ident" "jvm-dadd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dadd analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-dsub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dsub analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-dmul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dmul analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-ddiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ddiv analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-drem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-drem analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-deq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-deq analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-dlt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dlt analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-dgt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dgt analyse ?x ?y)

    ;; Objects
    [["Form" ["Cons" [["Ident" "jvm-null?"] ["Cons" [?object ["Nil" _]]]]]]]
    (&&host/analyse-jvm-null? analyse ?object)
    
    [["Form" ["Cons" [["Ident" "jvm-new"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Tuple" ?classes]
                                        ["Cons" [["Tuple" ?args]
                                                 ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-new analyse ?class ?classes ?args)

    [["Form" ["Cons" [["Ident" "jvm-getstatic"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-getstatic analyse ?class ?field)

    [["Form" ["Cons" [["Ident" "jvm-getfield"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Cons" [?object
                                                 ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-getfield analyse ?class ?field ?object)

    [["Form" ["Cons" [["Ident" "jvm-putstatic"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Cons" [?value
                                                 ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-putstatic analyse ?class ?field ?value)

    [["Form" ["Cons" [["Ident" "jvm-putfield"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Cons" [?object
                                                 ["Cons" [?value
                                                          ["Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-putfield analyse ?class ?field ?object ?value)

    [["Form" ["Cons" [["Ident" "jvm-invokestatic"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [["Tuple" ?args]
                                                          ["Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokestatic analyse ?class ?method (&/->seq ?classes) (&/->seq ?args))

    [["Form" ["Cons" [["Ident" "jvm-invokevirtual"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [?object
                                                          ["Cons" [["Tuple" ?args]
                                                                   ["Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokevirtual analyse ?class ?method (&/->seq ?classes) ?object (&/->seq ?args))

    [["Form" ["Cons" [["Ident" "jvm-invokeinterface"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [?object
                                                          ["Cons" [["Tuple" ?args]
                                                                   ["Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokeinterface analyse ?class ?method (&/->seq ?classes) ?object (&/->seq ?args))

    [["Form" ["Cons" [["Ident" "jvm-invokespecial"]
                      ["Cons" [["Ident" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [?object
                                                          ["Cons" [["Tuple" ?args]
                                                                   ["Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokespecial analyse ?class ?method (&/->seq ?classes) ?object (&/->seq ?args))
    
    ;; Exceptions
    [["Form" ["Cons" [["Ident" "jvm-try"]
                      ["Cons" [?body
                               ?handlers]]]]]]
    (&&host/analyse-jvm-try analyse ?body (reduce parse-handler [(list) nil] (&/->seq ?handlers)))

    [["Form" ["Cons" [["Ident" "jvm-throw"]
                      ["Cons" [?ex
                               ["Nil" _]]]]]]]
    (&&host/analyse-jvm-throw analyse ?ex)

    ;; Syncronization/monitos
    [["Form" ["Cons" [["Ident" "jvm-monitorenter"]
                      ["Cons" [?monitor
                               ["Nil" _]]]]]]]
    (&&host/analyse-jvm-monitorenter analyse ?monitor)

    [["Form" ["Cons" [["Ident" "jvm-monitorexit"]
                      ["Cons" [?monitor
                               ["Nil" _]]]]]]]
    (&&host/analyse-jvm-monitorexit analyse ?monitor)

    ;; Primitive conversions
    [["Form" ["Cons" [["Ident" "jvm-d2f"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-d2f analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-d2i"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-d2i analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-d2l"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-d2l analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-f2d"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-f2d analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-f2i"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-f2i analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-f2l"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-f2l analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-i2b"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2b analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-i2c"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2c analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-i2d"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2d analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-i2f"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2f analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-i2l"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2l analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-i2s"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2s analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-l2d"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-l2d analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-l2f"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-l2f analyse ?value)

    [["Form" ["Cons" [["Ident" "jvm-l2i"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-l2i analyse ?value)

    ;; Bitwise operators
    [["Form" ["Cons" [["Ident" "jvm-iand"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-iand analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-ior"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ior analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-land"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-land analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lor"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lor analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lxor"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lxor analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lshl"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lshl analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lshr"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lshr analyse ?x ?y)

    [["Form" ["Cons" [["Ident" "jvm-lushr"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lushr analyse ?x ?y)
    
    ;; Arrays
    [["Form" ["Cons" [["Ident" "jvm-new-array"] ["Cons" [["Ident" ?class] ["Cons" [["Int" ?length] ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-new-array analyse ?class ?length)

    [["Form" ["Cons" [["Ident" "jvm-aastore"] ["Cons" [?array ["Cons" [["Int" ?idx] ["Cons" [?elem ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-aastore analyse ?array ?idx ?elem)

    [["Form" ["Cons" [["Ident" "jvm-aaload"] ["Cons" [?array ["Cons" [["Int" ?idx] ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-aaload analyse ?array ?idx)

    ;; Classes & interfaces
    [["Form" ["Cons" [["Ident" "jvm-class"] ["Cons" [["Ident" ?name] ["Cons" [["Ident" ?super-class] ["Cons" [["Tuple" ?fields] ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-class analyse ?name ?super-class (&/->seq ?fields))

    [["Form" ["Cons" [["Ident" "jvm-interface"] ["Cons" [["Ident" ?name] ?members]]]]]]
    (&&host/analyse-jvm-interface analyse ?name ?members)

    ;; Programs
    [["Form" ["Cons" [["Ident" "jvm-program"] ["Cons" [["Ident" ?args] ["Cons" [?body ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-program analyse ?args ?body)
    
    [_]
    (fail (str "[Analyser Error] Unmatched token: " (&/show-ast token)))))

(defn ^:private analyse-ast [eval!]
  (fn [token]
    ;; (prn 'analyse-ast token)
    (matchv ::M/objects [token]
      [["Form" ["Cons" [["Tag" ?tag] ?values]]]]
      (exec [;; :let [_ (prn 'PRE-ASSERT)]
             :let [?values (&/->seq ?values)]
             :let [_ (assert (= 1 (count ?values)) (str "[Analyser Error] Can only tag 1 value: " (pr-str token)))]
             ;; :let [_ (prn 'POST-ASSERT)]
             =value (&&/analyse-1 (analyse-ast eval!) (first ?values))
             =value-type (&&/expr-type =value)]
        (return (|list [::&&/Expression [::&&/variant ?tag =value] (&/V "Variant" (&/V "Cons" (to-array [(to-array [?tag =value-type]) (&/V "Nil" nil)])))])))
      
      [["Form" ["Cons" [?fn ?args]]]]
      (fn [state]
        (match ((&&/analyse-1 (analyse-ast eval!) ?fn) state)
          [::&/ok [state* =fn]]
          ((&&lux/analyse-apply (analyse-ast eval!) =fn ?args) state*)

          _
          ((analyse-basic-ast (analyse-ast eval!) eval! token) state)))
      
      [_]
      (analyse-basic-ast (analyse-ast eval!) eval! token))))

;; [Resources]
(defn analyse [eval!]
  (exec [asts &parser/parse
         ;; :let [_ (prn 'analyse/asts asts)]
         ]
    (|flat-map% (analyse-ast eval!) asts)))
