(ns lux.analyser.host
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return fail]]
                 [parser :as &parser]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [env :as &&env])))

;; [Utils]
(defn ^:private extract-ident [ident]
  (matchv ::M/objects [ident]
    [["lux;Meta" [_ ["lux;Symbol" [_ ?ident]]]]]
    (return ?ident)

    [_]
    (fail "[Analyser Error] Can't extract Symbol.")))

(defn ^:private analyse-1+ [analyse ?token]
  (&type/with-var
    (fn [$var]
      (|do [=expr (&&/analyse-1 analyse $var ?token)]
        (matchv ::M/objects [=expr]
          [[?item ?type]]
          (|do [=type (&type/clean $var ?type)]
            (return (&/T ?item =type)))
          )))))

;; [Resources]
(do-template [<name> <output-tag> <input-class> <output-class>]
  (let [input-type (&/V "lux;DataT" <input-class>)
        output-type (&/V "lux;DataT" <output-class>)]
    (defn <name> [analyse ?x ?y]
      (|do [=x (&&/analyse-1 analyse input-type ?x)
            =y (&&/analyse-1 analyse input-type ?y)]
        (return (&/|list (&/T (&/V <output-tag> (&/T =x =y)) output-type))))))

  analyse-jvm-iadd "jvm-iadd" "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-isub "jvm-isub" "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-imul "jvm-imul" "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-idiv "jvm-idiv" "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-irem "jvm-irem" "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-ieq  "jvm-ieq"  "java.lang.Integer" "java.lang.Boolean"
  analyse-jvm-ilt  "jvm-ilt"  "java.lang.Integer" "java.lang.Boolean"
  analyse-jvm-igt  "jvm-igt"  "java.lang.Integer" "java.lang.Boolean"

  analyse-jvm-ladd "jvm-ladd" "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lsub "jvm-lsub" "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lmul "jvm-lmul" "java.lang.Long"    "java.lang.Long"
  analyse-jvm-ldiv "jvm-ldiv" "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lrem "jvm-lrem" "java.lang.Long"    "java.lang.Long"
  analyse-jvm-leq  "jvm-leq"  "java.lang.Long"    "java.lang.Boolean"
  analyse-jvm-llt  "jvm-llt"  "java.lang.Long"    "java.lang.Boolean"
  analyse-jvm-lgt  "jvm-lgt"  "java.lang.Long"    "java.lang.Boolean"

  analyse-jvm-fadd "jvm-fadd" "java.lang.Float"   "java.lang.Float"
  analyse-jvm-fsub "jvm-fsub" "java.lang.Float"   "java.lang.Float"
  analyse-jvm-fmul "jvm-fmul" "java.lang.Float"   "java.lang.Float"
  analyse-jvm-fdiv "jvm-fdiv" "java.lang.Float"   "java.lang.Float"
  analyse-jvm-frem "jvm-frem" "java.lang.Float"   "java.lang.Float"
  analyse-jvm-feq  "jvm-feq"  "java.lang.Float"   "java.lang.Boolean"
  analyse-jvm-flt  "jvm-flt"  "java.lang.Float"   "java.lang.Boolean"
  analyse-jvm-fgt  "jvm-fgt"  "java.lang.Float"   "java.lang.Boolean"

  analyse-jvm-dadd "jvm-dadd" "java.lang.Double"  "java.lang.Double"
  analyse-jvm-dsub "jvm-dsub" "java.lang.Double"  "java.lang.Double"
  analyse-jvm-dmul "jvm-dmul" "java.lang.Double"  "java.lang.Double"
  analyse-jvm-ddiv "jvm-ddiv" "java.lang.Double"  "java.lang.Double"
  analyse-jvm-drem "jvm-drem" "java.lang.Double"  "java.lang.Double"
  analyse-jvm-deq  "jvm-deq"  "java.lang.Double"  "java.lang.Boolean"
  analyse-jvm-dlt  "jvm-dlt"  "java.lang.Double"  "java.lang.Boolean"
  analyse-jvm-dgt  "jvm-dgt"  "java.lang.Double"  "java.lang.Boolean"
  )

(defn analyse-jvm-getstatic [analyse ?class ?field]
  (|do [=class (&host/full-class-name ?class)
        =type (&host/lookup-static-field =class ?field)]
    (return (&/|list (&/T (&/V "jvm-getstatic" (&/T =class ?field)) =type)))))

(defn analyse-jvm-getfield [analyse ?class ?field ?object]
  (|do [=class (&host/full-class-name ?class)
        =type (&host/lookup-static-field =class ?field)
        =object (&&/analyse-1 analyse ?object)]
    (return (&/|list (&/T (&/V "jvm-getfield" (&/T =class ?field =object)) =type)))))

(defn analyse-jvm-putstatic [analyse ?class ?field ?value]
  (|do [=class (&host/full-class-name ?class)
        =type (&host/lookup-static-field =class ?field)
        =value (&&/analyse-1 analyse ?value)]
    (return (&/|list (&/T (&/V "jvm-putstatic" (&/T =class ?field =value)) =type)))))

(defn analyse-jvm-putfield [analyse ?class ?field ?object ?value]
  (|do [=class (&host/full-class-name ?class)
        =type (&host/lookup-static-field =class ?field)
        =object (&&/analyse-1 analyse ?object)
        =value (&&/analyse-1 analyse ?value)]
    (return (&/|list (&/T (&/V "jvm-putfield" (&/T =class ?field =object =value)) =type)))))

(defn analyse-jvm-invokestatic [analyse ?class ?method ?classes ?args]
  (|do [=class (&host/full-class-name ?class)
        =classes (&/map% &host/extract-jvm-param ?classes)
        =return (&host/lookup-static-method =class ?method =classes)
        =args (&/flat-map% analyse ?args)]
    (return (&/|list (&/T (&/V "jvm-invokestatic" (&/T =class ?method =classes =args)) =return)))))

(do-template [<name> <tag>]
  (defn <name> [analyse ?class ?method ?classes ?object ?args]
    (|do [=class (&host/full-class-name ?class)
          =classes (&/map% &host/extract-jvm-param ?classes)
          =return (&host/lookup-virtual-method =class ?method =classes)
          =object (&&/analyse-1 analyse (&/V "lux;DataT" ?class) ?object)
          =args (&/map% (fn [c+o]
                          (|let [[?c ?o] c+o]
                            (&&/analyse-1 analyse (&/V "lux;DataT" ?c) ?o)))
                        (&/zip2 =classes ?args))]
      (return (&/|list (&/T (&/V <tag> (&/T =class ?method =classes =object =args)) =return)))))

  analyse-jvm-invokevirtual   "jvm-invokevirtual"
  analyse-jvm-invokeinterface "jvm-invokeinterface"
  analyse-jvm-invokespecial   "jvm-invokespecial"
  )

(defn analyse-jvm-null? [analyse ?object]
  (|do [=object (&&/analyse-1 analyse ?object)]
    (return (&/|list (&/T (&/V "jvm-null?" =object) (&/V "lux;DataT" "java.lang.Boolean"))))))

(defn analyse-jvm-new [analyse ?class ?classes ?args]
  (|do [=class (&host/full-class-name ?class)
        =classes (&/map% &host/extract-jvm-param ?classes)
        =args (&/flat-map% analyse ?args)]
    (return (&/|list (&/T (&/V "jvm-new" (&/T =class =classes =args)) (&/V "lux;DataT" =class))))))

(defn analyse-jvm-new-array [analyse ?class ?length]
  (|do [=class (&host/full-class-name ?class)]
    (return (&/|list (&/T (&/V "jvm-new-array" (&/T =class ?length)) (&/V "array" (&/T (&/V "lux;DataT" =class)
                                                                                                         (&/V "lux;Nil" nil))))))))

(defn analyse-jvm-aastore [analyse ?array ?idx ?elem]
  (|do [=array (&&/analyse-1 analyse &type/$Void ?array)
        =elem (&&/analyse-1 analyse &type/$Void ?elem)
        =array-type (&&/expr-type =array)]
    (return (&/|list (&/T (&/V "jvm-aastore" (&/T =array ?idx =elem)) =array-type)))))

(defn analyse-jvm-aaload [analyse ?array ?idx]
  (|do [=array (&&/analyse-1 analyse ?array)
        =array-type (&&/expr-type =array)]
    (return (&/|list (&/T (&/V "jvm-aaload" (&/T =array ?idx)) =array-type)))))

(defn analyse-jvm-class [analyse ?name ?super-class ?fields]
  (|do [?fields (&/map% (fn [?field]
                          (matchv ::M/objects [?field]
                            [["lux;Meta" [_ ["lux;Tuple" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" ?class]]]
                                                                      ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" ?field-name]]]
                                                                                   ["lux;Nil" _]]]]]]]]]
                            (return [?class ?field-name])
                            
                            [_]
                            (fail "[Analyser Error] Fields must be Tuple2 of [Symbol, Symbol]")))
                        ?fields)
        :let [=fields (into {} (for [[class field] ?fields]
                                 [field {:access :public
                                         :type class}]))]
        $module &/get-module-name]
    (return (&/|list (&/V "jvm-class" (&/T $module ?name ?super-class =fields {}))))))

(defn analyse-jvm-interface [analyse ?name ?members]
  (|do [=members (&/map% (fn [member]
                           (matchv ::M/objects [member]
                             [["lux;Meta" [_ ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" ["" ":'"]]]]
                                                                      ["lux;Cons" [["lux;Meta" [_ ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "->"]]]]
                                                                                                                           ["lux;Cons" [["lux;Meta" [_ ["lux;Tuple" ?inputs]]]
                                                                                                                                        ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?output]]]]
                                                                                                                                                     ["lux;Nil" _]]]]]]]]]]
                                                                                   ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?member-name]]]]
                                                                                                ["lux;Nil" _]]]]]]]]]]]
                             (|do [inputs* (&/map% extract-ident ?inputs)]
                               (return [?member-name [inputs* ?output]]))
                             
                             [_]
                             (fail "[Analyser Error] Invalid method signature!")))
                         ?members)
        :let [=methods (into {} (for [[method [inputs output]] (&/->seq =members)]
                                  [method {:access :public
                                           :type [inputs output]}]))]
        $module &/get-module-name]
    (return (&/|list (&/V "jvm-interface" (&/T $module ?name =methods))))))

(defn analyse-jvm-try [analyse ?body [?catches ?finally]]
  (|do [=body (&&/analyse-1 analyse ?body)
        =catches (&/map% (fn [[?ex-class ?ex-arg ?catch-body]]
                           (&&env/with-local ?ex-arg (&/V "lux;DataT" ?ex-class)
                             (|do [=catch-body (&&/analyse-1 analyse ?catch-body)]
                               (return [?ex-class ?ex-arg =catch-body]))))
                         ?catches)
        =finally (&&/analyse-1 analyse ?finally)
        =body-type (&&/expr-type =body)]
    (return (&/|list (&/T (&/V "jvm-try" (&/T =body =catches =finally)) =body-type)))))

(defn analyse-jvm-throw [analyse ?ex]
  (|do [=ex (&&/analyse-1 analyse ?ex)]
    (return (&/|list (&/T (&/V "jvm-throw" =ex) &type/$Void)))))

(defn analyse-jvm-monitorenter [analyse ?monitor]
  (|do [=monitor (&&/analyse-1 analyse ?monitor)]
    (return (&/|list (&/T (&/V "jvm-monitorenter" =monitor) (&/V "lux;TupleT" (&/V "lux;Nil" nil)))))))

(defn analyse-jvm-monitorexit [analyse ?monitor]
  (|do [=monitor (&&/analyse-1 analyse ?monitor)]
    (return (&/|list (&/T (&/V "jvm-monitorexit" =monitor) (&/V "lux;TupleT" (&/V "lux;Nil" nil)))))))

(do-template [<name> <tag> <from-class> <to-class>]
  (defn <name> [analyse ?value]
    (|do [=value (&&/analyse-1 analyse (&/V "lux;DataT" <from-class>) ?value)]
      (return (&/|list (&/T (&/V <tag> =value) (&/V "lux;DataT" <to-class>))))))

  analyse-jvm-d2f "jvm-d2f" "java.lang.Double"  "java.lang.Float"
  analyse-jvm-d2i "jvm-d2i" "java.lang.Double"  "java.lang.Integer"
  analyse-jvm-d2l "jvm-d2l" "java.lang.Double"  "java.lang.Long"

  analyse-jvm-f2d "jvm-f2d" "java.lang.Float"   "java.lang.Double"
  analyse-jvm-f2i "jvm-f2i" "java.lang.Float"   "java.lang.Integer"
  analyse-jvm-f2l "jvm-f2l" "java.lang.Float"   "java.lang.Long"

  analyse-jvm-i2b "jvm-i2b" "java.lang.Integer" "java.lang.Byte"
  analyse-jvm-i2c "jvm-i2c" "java.lang.Integer" "java.lang.Character"
  analyse-jvm-i2d "jvm-i2d" "java.lang.Integer" "java.lang.Double"
  analyse-jvm-i2f "jvm-i2f" "java.lang.Integer" "java.lang.Float"
  analyse-jvm-i2l "jvm-i2l" "java.lang.Integer" "java.lang.Long"
  analyse-jvm-i2s "jvm-i2s" "java.lang.Integer" "java.lang.Short"

  analyse-jvm-l2d "jvm-l2d" "java.lang.Long"    "java.lang.Double"
  analyse-jvm-l2f "jvm-l2f" "java.lang.Long"    "java.lang.Float"
  analyse-jvm-l2i "jvm-l2i" "java.lang.Long"    "java.lang.Integer"
  )

(do-template [<name> <tag> <from-class> <to-class>]
  (defn <name> [analyse ?value]
    (|do [=value (&&/analyse-1 analyse (&/V "lux;DataT" <from-class>) ?value)]
      (return (&/|list (&/T (&/V <tag> =value) (&/V "lux;DataT" <to-class>))))))

  analyse-jvm-iand  "jvm-iand"  "java.lang.Integer" "java.lang.Integer"
  analyse-jvm-ior   "jvm-ior"   "java.lang.Integer" "java.lang.Integer"

  analyse-jvm-land  "jvm-land"  "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lor   "jvm-lor"   "java.lang.Long"    "java.lang.Long"
  analyse-jvm-lxor  "jvm-lxor"  "java.lang.Long"    "java.lang.Long"

  analyse-jvm-lshl  "jvm-lshl"  "java.lang.Long"    "java.lang.Integer"
  analyse-jvm-lshr  "jvm-lshr"  "java.lang.Long"    "java.lang.Integer"
  analyse-jvm-lushr "jvm-lushr" "java.lang.Long"    "java.lang.Integer"
  )

(defn analyse-jvm-program [analyse ?args ?body]
  (|do [=body (&/with-scope ""
                (&&env/with-local "" (&/V "lux;AppT" (&/T &type/List &type/Text))
                  (analyse-1+ analyse ?body)))]
    (return (&/|list (&/V "jvm-program" =body)))))
