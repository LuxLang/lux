(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return fail]]
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
    [["Form" ["Cons" [["Symbol" "jvm-catch"]
                      ["Cons" [["Symbol" ?ex-class]
                               ["Cons" [["Symbol" ?ex-arg]
                                        ["Cons" [?catch-body
                                                 ["Nil" _]]]]]]]]]]]
    [(concat catch+ (list [?ex-class ?ex-arg ?catch-body])) finally+]

    [["Form" ["Cons" [["Symbol" "jvm-finally"]
                      ["Cons" [?finally-body
                               ["Nil" _]]]]]]]
    [catch+ ?finally-body]))

(defn ^:private analyse-basic-ast [analyse eval! token]
  ;; (prn 'analyse-basic-ast token (&/show-ast token))
  (matchv ::M/objects [token]
    ;; Standard special forms
    [["Bool" ?value]]
    (return (&/|list (&/V "Expression" (&/T (&/V "bool" ?value) (&/V "Data" (&/T "java.lang.Boolean" (&/V "Nil" nil)))))))

    [["Int" ?value]]
    (return (&/|list (&/V "Expression" (&/T (&/V "int" ?value)  (&/V "Data" (&/T "java.lang.Long" (&/V "Nil" nil)))))))

    [["Real" ?value]]
    (return (&/|list (&/V "Expression" (&/T (&/V "real" ?value) (&/V "Data" (&/T "java.lang.Double" (&/V "Nil" nil)))))))

    [["Char" ?value]]
    (return (&/|list (&/V "Expression" (&/T (&/V "char" ?value) (&/V "Data" (&/T "java.lang.Character" (&/V "Nil" nil)))))))

    [["Text" ?value]]
    (return (&/|list (&/V "Expression" (&/T (&/V "text" ?value) (&/V "Data" (&/T "java.lang.String" (&/V "Nil" nil)))))))

    [["Tuple" ?elems]]
    (&&lux/analyse-tuple analyse ?elems)

    [["Record" ?elems]]
    (&&lux/analyse-record analyse ?elems)

    [["Tag" ?tag]]
    (let [tuple-type (&/V "Tuple" (&/V "Nil" nil))]
      (return (&/|list (&/V "Expression" (&/T (&/V "variant" (&/T ?tag (&/V "Expression" (&/T (&/V "tuple" (&/|list)) tuple-type))))
                                              (&/V "Variant" (&/V "Cons" (&/T (&/T ?tag tuple-type) (&/V "Nil" nil)))))))))

    [["Symbol" "jvm-null"]]
    (return (&/|list (&/V "Expression" (&/T (&/V "jvm-null" nil) (&/V "Data" (&/T "null" (&/V "Nil" nil)))))))
    
    [["Symbol" ?ident]]
    (&&lux/analyse-ident analyse ?ident)

    [["Form" ["Cons" [["Symbol" "case'"]
                      ["Cons" [?variant ?branches]]]]]]
    (&&lux/analyse-case analyse ?variant ?branches)
    
    [["Form" ["Cons" [["Symbol" "lambda'"]
                      ["Cons" [["Symbol" ?self]
                               ["Cons" [["Symbol" ?arg]
                                        ["Cons" [?body
                                                 ["Nil" _]]]]]]]]]]]
    (&&lux/analyse-lambda analyse ?self ?arg ?body)

    [["Form" ["Cons" [["Symbol" "get@'"] ["Cons" [["Tag" ?slot] ["Cons" [?record ["Nil" _]]]]]]]]]
    (&&lux/analyse-get analyse ?slot ?record)

    [["Form" ["Cons" [["Symbol" "set@'"] ["Cons" [["Tag" ?slot] ["Cons" [?value ["Cons" [?record ["Nil" _]]]]]]]]]]]
    (&&lux/analyse-set analyse ?slot ?value ?record)

    [["Form" ["Cons" [["Symbol" "def'"] ["Cons" [["Symbol" ?name] ["Cons" [?value ["Nil" _]]]]]]]]]
    (&&lux/analyse-def analyse ?name ?value)

    [["Form" ["Cons" [["Symbol" "declare-macro"] ["Cons" [["Symbol" ?ident] ["Nil" _]]]]]]]
    (&&lux/analyse-declare-macro ?ident)
    
    [["Form" ["Cons" [["Symbol" "import'"] ["Cons" [["Text" ?path] ["Nil" _]]]]]]]
    (&&lux/analyse-import analyse ?path)

    [["Form" ["Cons" [["Symbol" ":"] ["Cons" [?value ["Cons" [?type ["Nil" _]]]]]]]]]
    (&&lux/analyse-check analyse eval! ?type ?value)

    [["Form" ["Cons" [["Symbol" "coerce"] ["Cons" [?type ["Cons" [?value ["Nil" _]]]]]]]]]
    (&&lux/analyse-coerce analyse eval! ?type ?value)

    ;; Host special forms
    [["Form" ["Cons" [["Symbol" "exec"] ?exprs]]]]
    (&&host/analyse-exec analyse ?exprs)

    ;; Integer arithmetic
    [["Form" ["Cons" [["Symbol" "jvm-iadd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-iadd analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-isub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-isub analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-imul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-imul analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-idiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-idiv analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-irem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-irem analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-ieq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ieq analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-ilt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ilt analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-igt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-igt analyse ?x ?y)

    ;; Long arithmetic
    [["Form" ["Cons" [["Symbol" "jvm-ladd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ladd analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-lsub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lsub analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-lmul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lmul analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-ldiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ldiv analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-lrem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lrem analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-leq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-leq analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-llt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-llt analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-lgt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lgt analyse ?x ?y)

    ;; Float arithmetic
    [["Form" ["Cons" [["Symbol" "jvm-fadd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fadd analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-fsub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fsub analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-fmul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fmul analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-fdiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fdiv analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-frem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-frem analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-feq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-feq analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-flt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-flt analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-fgt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-fgt analyse ?x ?y)

    ;; Double arithmetic
    [["Form" ["Cons" [["Symbol" "jvm-dadd"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dadd analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-dsub"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dsub analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-dmul"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dmul analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-ddiv"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ddiv analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-drem"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-drem analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-deq"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-deq analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-dlt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dlt analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-dgt"] ["Cons" [?y ["Cons" [?x ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-dgt analyse ?x ?y)

    ;; Objects
    [["Form" ["Cons" [["Symbol" "jvm-null?"] ["Cons" [?object ["Nil" _]]]]]]]
    (&&host/analyse-jvm-null? analyse ?object)
    
    [["Form" ["Cons" [["Symbol" "jvm-new"]
                      ["Cons" [["Symbol" ?class]
                               ["Cons" [["Tuple" ?classes]
                                        ["Cons" [["Tuple" ?args]
                                                 ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-new analyse ?class ?classes ?args)

    [["Form" ["Cons" [["Symbol" "jvm-getstatic"]
                      ["Cons" [["Symbol" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-getstatic analyse ?class ?field)

    [["Form" ["Cons" [["Symbol" "jvm-getfield"]
                      ["Cons" [["Symbol" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Cons" [?object
                                                 ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-getfield analyse ?class ?field ?object)

    [["Form" ["Cons" [["Symbol" "jvm-putstatic"]
                      ["Cons" [["Symbol" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Cons" [?value
                                                 ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-putstatic analyse ?class ?field ?value)

    [["Form" ["Cons" [["Symbol" "jvm-putfield"]
                      ["Cons" [["Symbol" ?class]
                               ["Cons" [["Text" ?field]
                                        ["Cons" [?object
                                                 ["Cons" [?value
                                                          ["Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-putfield analyse ?class ?field ?object ?value)

    [["Form" ["Cons" [["Symbol" "jvm-invokestatic"]
                      ["Cons" [["Symbol" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [["Tuple" ?args]
                                                          ["Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokestatic analyse ?class ?method ?classes ?args)

    [["Form" ["Cons" [["Symbol" "jvm-invokevirtual"]
                      ["Cons" [["Symbol" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [?object
                                                          ["Cons" [["Tuple" ?args]
                                                                   ["Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokevirtual analyse ?class ?method ?classes ?object ?args)

    [["Form" ["Cons" [["Symbol" "jvm-invokeinterface"]
                      ["Cons" [["Symbol" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [?object
                                                          ["Cons" [["Tuple" ?args]
                                                                   ["Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokeinterface analyse ?class ?method ?classes ?object ?args)

    [["Form" ["Cons" [["Symbol" "jvm-invokespecial"]
                      ["Cons" [["Symbol" ?class]
                               ["Cons" [["Text" ?method]
                                        ["Cons" [["Tuple" ?classes]
                                                 ["Cons" [?object
                                                          ["Cons" [["Tuple" ?args]
                                                                   ["Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokespecial analyse ?class ?method ?classes ?object ?args)
    
    ;; Exceptions
    [["Form" ["Cons" [["Symbol" "jvm-try"]
                      ["Cons" [?body
                               ?handlers]]]]]]
    (&&host/analyse-jvm-try analyse ?body (&/fold parse-handler [(list) nil] ?handlers))

    [["Form" ["Cons" [["Symbol" "jvm-throw"]
                      ["Cons" [?ex
                               ["Nil" _]]]]]]]
    (&&host/analyse-jvm-throw analyse ?ex)

    ;; Syncronization/monitos
    [["Form" ["Cons" [["Symbol" "jvm-monitorenter"]
                      ["Cons" [?monitor
                               ["Nil" _]]]]]]]
    (&&host/analyse-jvm-monitorenter analyse ?monitor)

    [["Form" ["Cons" [["Symbol" "jvm-monitorexit"]
                      ["Cons" [?monitor
                               ["Nil" _]]]]]]]
    (&&host/analyse-jvm-monitorexit analyse ?monitor)

    ;; Primitive conversions
    [["Form" ["Cons" [["Symbol" "jvm-d2f"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-d2f analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-d2i"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-d2i analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-d2l"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-d2l analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-f2d"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-f2d analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-f2i"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-f2i analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-f2l"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-f2l analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-i2b"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2b analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-i2c"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2c analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-i2d"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2d analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-i2f"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2f analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-i2l"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2l analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-i2s"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-i2s analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-l2d"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-l2d analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-l2f"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-l2f analyse ?value)

    [["Form" ["Cons" [["Symbol" "jvm-l2i"] ["Cons" [?value ["Nil" _]]]]]]]
    (&&host/analyse-jvm-l2i analyse ?value)

    ;; Bitwise operators
    [["Form" ["Cons" [["Symbol" "jvm-iand"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-iand analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-ior"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-ior analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-land"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-land analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-lor"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lor analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-lxor"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lxor analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-lshl"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lshl analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-lshr"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lshr analyse ?x ?y)

    [["Form" ["Cons" [["Symbol" "jvm-lushr"] ["Cons" [?x ["Cons" [?y ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-lushr analyse ?x ?y)
    
    ;; Arrays
    [["Form" ["Cons" [["Symbol" "jvm-new-array"] ["Cons" [["Symbol" ?class] ["Cons" [["Int" ?length] ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-new-array analyse ?class ?length)

    [["Form" ["Cons" [["Symbol" "jvm-aastore"] ["Cons" [?array ["Cons" [["Int" ?idx] ["Cons" [?elem ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-aastore analyse ?array ?idx ?elem)

    [["Form" ["Cons" [["Symbol" "jvm-aaload"] ["Cons" [?array ["Cons" [["Int" ?idx] ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-aaload analyse ?array ?idx)

    ;; Classes & interfaces
    [["Form" ["Cons" [["Symbol" "jvm-class"] ["Cons" [["Symbol" ?name] ["Cons" [["Symbol" ?super-class] ["Cons" [["Tuple" ?fields] ["Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-class analyse ?name ?super-class ?fields)

    [["Form" ["Cons" [["Symbol" "jvm-interface"] ["Cons" [["Symbol" ?name] ?members]]]]]]
    (&&host/analyse-jvm-interface analyse ?name ?members)

    ;; Programs
    [["Form" ["Cons" [["Symbol" "jvm-program"] ["Cons" [["Symbol" ?args] ["Cons" [?body ["Nil" _]]]]]]]]]
    (&&host/analyse-jvm-program analyse ?args ?body)
    
    [_]
    (fail (str "[Analyser Error] Unmatched token: " (&/show-ast token)))))

(defn ^:private analyse-ast [eval!]
  (fn [token]
    ;; (prn 'analyse-ast token)
    (matchv ::M/objects [token]
      [["Form" ["Cons" [["Tag" ?tag] ?values]]]]
      (exec [;; :let [_ (prn 'PRE-ASSERT)]
             :let [_ (assert (= 1 (&/|length ?values)) (str "[Analyser Error] Can only tag 1 value: " (pr-str token)))]
             ;; :let [_ (prn 'POST-ASSERT)]
             =value (&&/analyse-1 (analyse-ast eval!) (&/|head ?values))
             =value-type (&&/expr-type =value)]
        (return (&/|list (&/V "Expression" (&/T (&/V "variant" (&/T ?tag =value)) (&/V "Variant" (&/V "Cons" (&/T (&/T ?tag =value-type) (&/V "Nil" nil)))))))))
      
      [["Form" ["Cons" [?fn ?args]]]]
      (fn [state]
        ;; (prn '(&/show-ast ?fn) (&/show-ast ?fn))
        (matchv ::M/objects [((&&/analyse-1 (analyse-ast eval!) ?fn) state)]
          [["Right" [state* =fn]]]
          ((&&lux/analyse-apply (analyse-ast eval!) =fn ?args) state*)

          [_]
          (do ;; (prn 'analyse-ast/token (aget token 0) (&/show-state state))
            ((analyse-basic-ast (analyse-ast eval!) eval! token) state))))
      
      [_]
      (analyse-basic-ast (analyse-ast eval!) eval! token))))

;; [Resources]
(defn analyse [eval!]
  (exec [asts &parser/parse
         ;; :let [_ (prn 'analyse/asts asts)]
         ]
    (&/flat-map% (analyse-ast eval!) asts)))
