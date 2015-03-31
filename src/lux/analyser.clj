(ns lux.analyser
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return fail]]
                 [reader :as &reader]
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
    [["lux;Meta" [meta ["Form" ["Cons" [["lux;Meta" [_ ["Symbol" [_ "jvm-catch"]]]]
                                        ["Cons" [["lux;Meta" [_ ["Symbol" [_ ?ex-class]]]]
                                                 ["Cons" [["lux;Meta" [_ ["Symbol" [_ ?ex-arg]]]]
                                                          ["Cons" [?catch-body
                                                                   ["Nil" _]]]]]]]]]]]]]
    [(concat catch+ (list [?ex-class ?ex-arg ?catch-body])) finally+]

    [["lux;Meta" [meta ["Form" ["Cons" [["lux;Meta" [_ ["Symbol" [_ "jvm-finally"]]]]
                                        ["Cons" [?finally-body
                                                 ["Nil" _]]]]]]]]]
    [catch+ ?finally-body]))

(defn ^:private analyse-basic-ast [analyse eval! token]
  ;; (prn 'analyse-basic-ast (aget token 0))
  ;; (when (= "lux;Tag" (aget token 0))
  ;;   (prn 'analyse-basic-ast/tag (aget token 1)))
  ;; (prn 'analyse-basic-ast token (&/show-ast token))
  (matchv ::M/objects [token]
    ;; Standard special forms
    [["lux;Meta" [meta ["lux;Bool" ?value]]]]
    (return (&/|list (&/V "Expression" (&/T (&/V "bool" ?value) (&/V "lux;DataT" (&/T "java.lang.Boolean" (&/V "lux;Nil" nil)))))))

    [["lux;Meta" [meta ["lux;Int" ?value]]]]
    (return (&/|list (&/V "Expression" (&/T (&/V "int" ?value)  (&/V "lux;DataT" (&/T "java.lang.Long" (&/V "lux;Nil" nil)))))))

    [["lux;Meta" [meta ["lux;Real" ?value]]]]
    (return (&/|list (&/V "Expression" (&/T (&/V "real" ?value) (&/V "lux;DataT" (&/T "java.lang.Double" (&/V "lux;Nil" nil)))))))

    [["lux;Meta" [meta ["lux;Char" ?value]]]]
    (return (&/|list (&/V "Expression" (&/T (&/V "char" ?value) (&/V "lux;DataT" (&/T "java.lang.Character" (&/V "lux;Nil" nil)))))))

    [["lux;Meta" [meta ["lux;Text" ?value]]]]
    (return (&/|list (&/V "Expression" (&/T (&/V "text" ?value) (&/V "lux;DataT" (&/T "java.lang.String" (&/V "lux;Nil" nil)))))))

    [["lux;Meta" [meta ["lux;Tuple" ?elems]]]]
    (&&lux/analyse-tuple analyse ?elems)

    [["lux;Meta" [meta ["lux;Record" ?elems]]]]
    (&&lux/analyse-record analyse ?elems)

    [["lux;Meta" [meta ["lux;Tag" [?module ?name]]]]]
    (let [tuple-type (&/V "lux;TupleT" (&/V "lux;Nil" nil))
          ?tag (str ?module ";" ?name)]
      (return (&/|list (&/V "Expression" (&/T (&/V "variant" (&/T ?tag (&/V "Expression" (&/T (&/V "tuple" (&/|list)) tuple-type))))
                                              (&/V "lux;VariantT" (&/V "lux;Cons" (&/T (&/T ?tag tuple-type) (&/V "lux;Nil" nil)))))))))

    [["lux;Meta" [meta ["lux;Symbol" [_ "jvm-null"]]]]]
    (return (&/|list (&/V "Expression" (&/T (&/V "jvm-null" nil) (&/V "lux;DataT" (&/T "null" (&/V "lux;Nil" nil)))))))
    
    [["lux;Meta" [meta ["lux;Symbol" ?ident]]]]
    (&&lux/analyse-ident analyse ?ident)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "case'"]]]]
                                                ["lux;Cons" [?variant ?branches]]]]]]]]
    (&&lux/analyse-case analyse ?variant ?branches)
    
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "lambda'"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?self]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?arg]]]]
                                                                          ["lux;Cons" [?body
                                                                                       ["lux;Nil" _]]]]]]]]]]]]]
    (&&lux/analyse-lambda analyse ?self ?arg ?body)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "def'"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?name]]]]
                                                             ["lux;Cons" [?value
                                                                          ["lux;Nil" _]]]]]]]]]]]
    (do ;; (when (= "if" ?name)
        ;;   (prn "if" (&/show-ast ?value)))
        (&&lux/analyse-def analyse ?name ?value))

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "declare-macro'"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" ?ident]]]
                                                             ["lux;Nil" _]]]]]]]]]
    (&&lux/analyse-declare-macro ?ident)
    
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "import'"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Text" ?path]]]
                                                             ["lux;Nil" _]]]]]]]]]
    (&&lux/analyse-import analyse ?path)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "check'"]]]]
                                                ["lux;Cons" [?type
                                                             ["lux;Cons" [?value
                                                                          ["lux;Nil" _]]]]]]]]]]]
    (&&lux/analyse-check analyse eval! ?type ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "coerce'"]]]]
                                                ["lux;Cons" [?type
                                                             ["lux;Cons" [?value
                                                                          ["lux;Nil" _]]]]]]]]]]]
    (&&lux/analyse-coerce analyse eval! ?type ?value)

    ;; Host special forms
    ;; Integer arithmetic
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-iadd"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-iadd analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-isub"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-isub analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-imul"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-imul analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-idiv"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-idiv analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-irem"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-irem analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-ieq"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-ieq analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-ilt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-ilt analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-igt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-igt analyse ?x ?y)

    ;; Long arithmetic
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-ladd"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-ladd analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-lsub"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-lsub analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-lmul"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-lmul analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-ldiv"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-ldiv analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-lrem"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-lrem analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-leq"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-leq analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-llt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-llt analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-lgt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-lgt analyse ?x ?y)

    ;; Float arithmetic
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-fadd"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-fadd analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-fsub"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-fsub analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-fmul"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-fmul analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-fdiv"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-fdiv analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-frem"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-frem analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-feq"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-feq analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-flt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-flt analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-fgt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-fgt analyse ?x ?y)

    ;; Double arithmetic
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-dadd"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-dadd analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-dsub"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-dsub analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-dmul"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-dmul analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-ddiv"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-ddiv analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-drem"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-drem analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-deq"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-deq analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-dlt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-dlt analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-dgt"]]]] ["lux;Cons" [?y ["lux;Cons" [?x ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-dgt analyse ?x ?y)

    ;; Objects
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-null?"]]]]
                                                ["lux;Cons" [?object
                                                             ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-null? analyse ?object)
    
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-new"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?classes]]]
                                                                          ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?args]]]
                                                                                       ["lux;Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-new analyse ?class ?classes ?args)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-getstatic"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Text" ?field]]]
                                                                          ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-getstatic analyse ?class ?field)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-getfield"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Text" ?field]]]
                                                                          ["lux;Cons" [?object
                                                                                       ["lux;Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-getfield analyse ?class ?field ?object)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-putstatic"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Text" ?field]]]
                                                                          ["lux;Cons" [?value
                                                                                       ["lux;Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-putstatic analyse ?class ?field ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-putfield"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Text" ?field]]]
                                                                          ["lux;Cons" [?object
                                                                                       ["lux;Cons" [?value
                                                                                                    ["lux;Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-putfield analyse ?class ?field ?object ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-invokestatic"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Text" ?method]]]
                                                                          ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?classes]]]
                                                                                       ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?args]]]
                                                                                                    ["lux;Nil" _]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokestatic analyse ?class ?method ?classes ?args)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-invokevirtual"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Text" ?method]]]
                                                                          ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?classes]]]
                                                                                       ["lux;Cons" [?object
                                                                                                    ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?args]]]
                                                                                                                 ["lux;Nil" _]]]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokevirtual analyse ?class ?method ?classes ?object ?args)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-invokeinterface"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Text" ?method]]]
                                                                          ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?classes]]]
                                                                                       ["lux;Cons" [?object
                                                                                                    ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?args]]]
                                                                                                                 ["lux;Nil" _]]]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokeinterface analyse ?class ?method ?classes ?object ?args)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-invokespecial"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Text" ?method]]]
                                                                          ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?classes]]]
                                                                                       ["lux;Cons" [?object
                                                                                                    ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?args]]]
                                                                                                                 ["lux;Nil" _]]]]]]]]]]]]]]]]]
    (&&host/analyse-jvm-invokespecial analyse ?class ?method ?classes ?object ?args)
    
    ;; Exceptions
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-try"]]]]
                                                ["lux;Cons" [?body
                                                             ?handlers]]]]]]]]
    (&&host/analyse-jvm-try analyse ?body (&/fold parse-handler [(list) nil] ?handlers))

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-throw"]]]]
                                                ["lux;Cons" [?ex
                                                             ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-throw analyse ?ex)

    ;; Syncronization/monitos
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-monitorenter"]]]]
                                                ["lux;Cons" [?monitor
                                                             ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-monitorenter analyse ?monitor)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-monitorexit"]]]]
                                                ["lux;Cons" [?monitor
                                                             ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-monitorexit analyse ?monitor)

    ;; Primitive conversions
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-d2f"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-d2f analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-d2i"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-d2i analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-d2l"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-d2l analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-f2d"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-f2d analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-f2i"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-f2i analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-f2l"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-f2l analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-i2b"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-i2b analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-i2c"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-i2c analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-i2d"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-i2d analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-i2f"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-i2f analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-i2l"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-i2l analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-i2s"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-i2s analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-l2d"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-l2d analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-l2f"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-l2f analyse ?value)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-l2i"]]]] ["lux;Cons" [?value ["lux;Nil" _]]]]]]]]]
    (&&host/analyse-jvm-l2i analyse ?value)

    ;; Bitwise operators
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-iand"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-iand analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-ior"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-ior analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-land"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-land analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-lor"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-lor analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-lxor"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-lxor analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-lshl"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-lshl analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-lshr"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-lshr analyse ?x ?y)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-lushr"]]]] ["lux;Cons" [?x ["lux;Cons" [?y ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-lushr analyse ?x ?y)
    
    ;; Arrays
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-new-array"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?class]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Int" ?length]]]
                                                                          ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-new-array analyse ?class ?length)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-aastore"]]]]
                                                ["lux;Cons" [?array
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Int" ?idx]]]
                                                                          ["lux;Cons" [?elem
                                                                                       ["lux;Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-aastore analyse ?array ?idx ?elem)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-aaload"]]]]
                                                ["lux;Cons" [?array
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Int" ?idx]]]
                                                                          ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-aaload analyse ?array ?idx)

    ;; Classes & interfaces
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-class"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?name]]]]
                                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?super-class]]]]
                                                                          ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?fields]]]
                                                                                       ["lux;Nil" _]]]]]]]]]]]]]
    (&&host/analyse-jvm-class analyse ?name ?super-class ?fields)

    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-interface"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?name]]]]
                                                             ?members]]]]]]]]
    (&&host/analyse-jvm-interface analyse ?name ?members)

    ;; Programs
    [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "jvm-program"]]]]
                                                ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?args]]]]
                                                             ["lux;Cons" [?body
                                                                          ["lux;Nil" _]]]]]]]]]]]
    (&&host/analyse-jvm-program analyse ?args ?body)
    
    [_]
    (fail (str "[Analyser Error] Unmatched token: " (&/show-ast token)))))

(defn ^:private analyse-ast [eval!]
  (fn [token]
    ;; (prn 'analyse-ast token)
    (matchv ::M/objects [token]
      [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Tag" [?module ?name]]]] ?values]]]]]]
      (exec [;; :let [_ (prn 'PRE-ASSERT)]
             :let [?tag (str ?module ";" ?name)]
             :let [_ (assert (= 1 (&/|length ?values)) (str "[Analyser Error] Can only tag 1 value: " (pr-str token)))]
             ;; :let [_ (prn 'POST-ASSERT)]
             =value (&&/analyse-1 (analyse-ast eval!) (&/|head ?values))
             =value-type (&&/expr-type =value)]
        (return (&/|list (&/V "Expression" (&/T (&/V "variant" (&/T ?tag =value)) (&/V "lux;VariantT" (&/V "lux;Cons" (&/T (&/T ?tag =value-type) (&/V "lux;Nil" nil)))))))))
      
      [["lux;Meta" [meta ["lux;Form" ["lux;Cons" [?fn ?args]]]]]]
      (fn [state]
        ;; (prn '(&/show-ast ?fn) (&/show-ast ?fn))
        (matchv ::M/objects [((&&/analyse-1 (analyse-ast eval!) ?fn) state)]
          [["lux;Right" [state* =fn]]]
          ((&&lux/analyse-apply (analyse-ast eval!) =fn ?args) state*)

          [_]
          (do ;; (prn 'analyse-ast/token (aget token 0) (&/show-state state))
              ((analyse-basic-ast (analyse-ast eval!) eval! token) state))))
      
      [_]
      (analyse-basic-ast (analyse-ast eval!) eval! token))))

;; [Resources]
(defn analyse [eval!]
  (exec [asts &parser/parse]
    (&/flat-map% (analyse-ast eval!) asts)))
