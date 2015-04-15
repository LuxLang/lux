(ns lux.type
  (:refer-clojure :exclude [deref apply merge bound?])
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            [lux.base :as & :refer [|do return* return fail fail* assert! |let]]))

(declare show-type)

;; [Util]
(defn bound? [id]
  (fn [state]
    (if-let [type* (->> state (&/get$ "lux;types") (&/get$ "lux;mappings") (&/|get id))]
      (matchv ::M/objects [type*]
        [["lux;Some" _]]
        (return* state true)
        
        [["lux;None" _]]
        (return* state false))
      (fail* (str "Unknown type-var: " id)))))

(defn deref [id]
  (fn [state]
    (let [mappings (->> state (&/get$ "lux;types") (&/get$ "lux;mappings"))]
      (do (prn 'deref/mappings (&/->seq (&/|keys mappings)))
        (if-let [type* (->> mappings (&/|get id))]
          (do (prn 'deref/type* (aget type* 0))
            (matchv ::M/objects [type*]
              [["lux;Some" type]]
              (return* state type)
              
              [["lux;None" _]]
              (fail* (str "Unbound type-var: " id))))
          (fail* (str "Unknown type-var: " id)))))))

(defn set-var [id type]
  (fn [state]
    (if-let [tvar (->> state (&/get$ "lux;types") (&/get$ "lux;mappings") (&/|get id))]
      (do ;; (prn 'set-var (aget tvar 0))
          (matchv ::M/objects [tvar]
            [["lux;Some" bound]]
            (fail* (str "Can't rebind type var: " id " | Current type: " (show-type bound)))
            
            [["lux;None" _]]
            (return* (&/update$ "lux;types" (fn [ts] (&/update$ "lux;mappings" #(&/|put id (&/V "lux;Some" type) %)
                                                               ts))
                                state)
                     nil)))
      (fail* (str "Unknown type-var: " id)))))

;; [Exports]
(def fresh-var
  (fn [state]
    (let [id (->> state (&/get$ "lux;types") (&/get$ "lux;counter"))]
      (return* (&/update$ "lux;types" #(->> %
                                            (&/update$ "lux;counter" inc)
                                            (&/update$ "lux;mappings" (fn [ms] (&/|put id (&/V "lux;None" nil) ms))))
                          state)
               (&/V "lux;VarT" id)))))

(def fresh-lambda
  (|do [=arg fresh-var
        =return fresh-var]
    (return (&/V "lux;LambdaT" (&/T =arg =return)))))

(defn clean [tvar type]
  (matchv ::M/objects [tvar]
    [["lux;VarT" ?tid]]
    (matchv ::M/objects [type]
      [["lux;VarT" ?id]]
      (if (= ?tid ?id)
        (|do [=type (deref ?id)]
          (clean tvar =type))
        (return type))
      
      [["lux;LambdaT" [?arg ?return]]]
      (|do [=arg (clean tvar ?arg)
            =return (clean tvar ?return)]
        (return (&/V "lux;LambdaT" (&/T =arg =return))))

      [["lux;AppT" [?lambda ?param]]]
      (|do [=lambda (clean tvar ?lambda)
            =param (clean tvar ?param)]
        (return (&/V "lux;AppT" (&/T =lambda =param))))

      [["lux;TupleT" ?members]]
      (|do [=members (&/map% (partial clean tvar) ?members)]
        (return (&/V "lux;TupleT" =members)))
      
      [["lux;VariantT" ?members]]
      (|do [=members (&/map% (fn [[k v]]
                               (|do [=v (clean tvar v)]
                                 (return (&/T k =v))))
                             ?members)]
        (return (&/V "lux;VariantT" =members)))

      [["lux;RecordT" ?members]]
      (|do [=members (&/map% (fn [[k v]]
                               (|do [=v (clean tvar v)]
                                 (return (&/T k =v))))
                             ?members)]
        (return (&/V "lux;RecordT" =members)))

      [["lux;AllT" [?env ?name ?arg ?body]]]
      (|do [=env (&/map% (fn [[k v]]
                           (|do [=v (clean tvar v)]
                             (return (&/T k =v))))
                         ?env)]
        (return (&/V "lux;AllT" (&/T =env ?name ?arg ?body))))

      [_]
      (return type)
      )))

(defn show-type [type]
  ;; (prn 'show-type (aget type 0))
  (matchv ::M/objects [type]
    [["lux;AnyT" _]]
    "Any"

    [["lux;NothingT" _]]
    "Nothing"

    [["lux;DataT" name]]
    (str "(^ " name ")")
    
    [["lux;TupleT" elems]]
    (if (&/|empty? elems)
      "(,)"
      (str "(, " (->> elems (&/|map show-type) (&/|interpose " ") (&/fold str "")) ")"))

    [["lux;VariantT" cases]]
    (str "(| " (->> cases
                    (&/|map (fn [kv]
                              (matchv ::M/objects [kv]
                                [[k ["Tuple" ["Nil" _]]]]
                                (str "#" k)

                                [[k v]]
                                (str "(#" k " " (show-type v) ")"))))
                    (&/|interpose " ")
                    (&/fold str "")) ")")
    

    [["lux;RecordT" fields]]
    (str "(& " (->> fields
                    (&/|map (fn [kv]
                              (matchv ::M/objects [kv]
                                [[k v]]
                                (str "(#" k " " (show-type v) ")"))))
                    (&/|interpose " ")
                    (&/fold str "")) ")")

    [["lux;LambdaT" [input output]]]
    (str "(-> " (show-type input) " " (show-type output) ")")

    [["lux;VarT" id]]
    (str "⌈" id "⌋")

    [["lux;BoundT" name]]
    name

    [["lux;AppT" [?lambda ?param]]]
    (str "(" (show-type ?lambda) " " (show-type ?param) ")")
    
    [["lux;AllT" [?env ?name ?arg ?body]]]
    (str "(All " ?name " " ?arg " " (show-type ?body) ")")
    ))

(defn type= [x y]
  (matchv ::M/objects [x y]
    [["lux;AnyT" _] ["lux;AnyT" _]]
    true

    [["lux;NothingT" _] ["lux;NothingT" _]]
    true

    [["lux;DataT" xname] ["lux;DataT" yname]]
    (= xname yname)

    [["lux;TupleT" xelems] ["lux;TupleT" yelems]]
    (&/fold (fn [old xy] (and old (type= (aget xy 0) (aget xy 1))))
            true
            (&/zip2 xelems yelems))

    [["lux;VariantT" xcases] ["lux;VariantT" ycases]]
    (&/fold (fn [old cases]
              (matchv ::M/objects [cases]
                [[[xtag xtype] [ytag ytype]]]
                (and (= xtag ytag)
                     (type= xtype ytype))))
            true (&/zip2 xcases ycases))
    

    [["lux;RecordT" xfields] ["lux;RecordT" yfields]]
    (&/fold (fn [old cases]
              (matchv ::M/objects [cases]
                [[[xtag xtype] [ytag ytype]]]
                (and (= xtag ytag)
                     (type= xtype ytype))))
            true (&/zip2 xfields yfields))

    [["lux;LambdaT" [xinput xoutput]] ["lux;LambdaT" [yinput youtput]]]
    (and (type= xinput yinput)
         (type= xoutput youtput))

    [["lux;VarT" xid] ["lux;VarT" yid]]
    (= xid yid)

    [["lux;BoundT" xname] ["lux;BoundT" yname]]
    (= xname yname)

    [["lux;AppT" [xlambda xparam]] ["lux;AppT" [ylambda yparam]]]
    (and (type= xlambda ylambda)
         (type= xparam yparam))
    
    [["lux;AllT" [xenv xname xarg xbody]] ["lux;AllT" [yenv yname yarg ybody]]]
    (and (&/fold (fn [old cases]
                   (matchv ::M/objects [cases]
                     [[[xtag xtype] [ytag ytype]]]
                     (and (= xtag ytag)
                          (type= xtype ytype))))
                 true (&/zip2 xenv yenv))
         (= xname yname)
         (= xarg yarg)
         (type= xbody ybody))

    [_ _]
    (do ;; (prn 'type= (show-type x) (show-type y))
        false)
    ))

(defn ^:private fp-get [k xs]
  (matchv ::M/objects [k]
    [[e a]]
    (matchv ::M/objects [xs]
      [["lux;Nil" _]]
      (&/V "lux;None" nil)

      [["lux;Cons" [[[e* a*] v*] xs*]]]
      (if (and (type= e e*)
               (type= a a*))
        (&/V "lux;Some" v*)
        (fp-get k xs*))
      )))

(defn ^:private fp-put [k v fixpoints]
  (&/|cons (&/T k v) fixpoints))

(defn ^:private check-error [expected actual]
  (str "Type " (show-type expected) " does not subsume type " (show-type actual)))

(defn beta-reduce [env type]
  ;; (prn 'beta-reduce (aget type 0))
  (matchv ::M/objects [type]
    [["lux;VariantT" ?cases]]
    (&/V "lux;VariantT" (&/|map (fn [kv]
                                  (|let [[k v] kv]
                                    (&/T k (beta-reduce env v))))
                                ?cases))

    [["lux;RecordT" ?fields]]
    (&/V "lux;RecordT" (&/|map (fn [kv]
                                 (|let [[k v] kv]
                                   (&/T k (beta-reduce env v))))
                               ?fields))

    [["lux;TupleT" ?members]]
    (&/V "lux;TupleT" (&/|map (partial beta-reduce env) ?members))

    [["lux;AppT" [?type-fn ?type-arg]]]
    (&/V "lux;AppT" (&/T (beta-reduce env ?type-fn) (beta-reduce env ?type-arg)))

    [["lux;AllT" [?local-env ?local-name ?local-arg ?local-def]]]
    (&/V "lux;AllT" (&/T (&/|merge ?local-env env) ?local-name ?local-arg ?local-def))

    [["lux;LambdaT" [?input ?output]]]
    (&/V "lux;LambdaT" (&/T (beta-reduce env ?input) (beta-reduce env ?output)))

    [["lux;BoundT" ?name]]
    (if-let [bound (&/|get ?name env)]
      (do ;; (prn 'beta-reduce "lux;BoundT" ?name (->> (&/|keys env) (&/|interpose " ") (&/fold str ""))
          ;;      (show-type bound))
          (beta-reduce env bound))
      type)

    [_]
    type
    ))

(defn slot-type [record slot]
  (fn [state]
    (matchv ::M/objects [(&/|get slot record)]
      [["lux;Left" msg]]
      (fail* msg)

      [["lux;Right" type]]
      (return* state type))))

(defn apply-type [type-fn param]
  ;; (prn 'apply-type (aget type-fn 0) (aget param 0))
  (matchv ::M/objects [type-fn]
    [["lux;AllT" [local-env local-name local-arg local-def]]]
    (return (beta-reduce (->> local-env
                              (&/|put local-name type-fn)
                              (&/|put local-arg param))
                         local-def))

    [["lux;AppT" [F A]]]
    (|do [type-fn* (apply-type F A)]
      (apply-type type-fn* param))
    
    [_]
    (fail (str "[Type System] Can't apply type function " (show-type type-fn) " to type " (show-type param)))))

(def init-fixpoints (&/|list))

(defn ^:private check* [fixpoints expected actual]
  (prn 'check* (aget expected 0) (aget actual 0))
  ;; (prn 'check* (show-type expected) (show-type actual))
  (matchv ::M/objects [expected actual]
    [["lux;AnyT" _] _]
    (return (&/T fixpoints nil))

    [_ ["lux;NothingT" _]]
    (return (&/T fixpoints nil))

    [["lux;VarT" ?eid] ["lux;VarT" ?aid]]
    (if (= ?eid ?aid)
      (return (&/T fixpoints nil))
      (&/try-all% (&/|list (|do [ebound (deref ?eid)]
                             (check* fixpoints ebound actual))
                           (|do [abound (deref ?aid)]
                             (check* fixpoints expected abound))
                           (|do [_ (set-var ?eid actual)]
                             (return (&/T fixpoints nil))))))
    
    [["lux;VarT" ?id] _]
    (&/try-all% (&/|list (|do [_ (set-var ?id actual)]
                           (return (&/T fixpoints nil)))
                         (|do [bound (deref ?id)]
                           (check* fixpoints bound actual))))
    
    [_ ["lux;VarT" ?id]]
    (&/try-all% (&/|list (|do [_ (set-var ?id expected)]
                           (return (&/T fixpoints nil)))
                         (|do [bound (deref ?id)]
                           (check* fixpoints expected bound))))

    [["lux;AppT" [F A]] _]
    (|do [expected* (apply-type F A)
          :let [fp-pair (&/T expected actual)]]
      (matchv ::M/objects [(fp-get fp-pair fixpoints)]
        [["lux;Some" ?]]
        (if ?
          (return (&/T fixpoints nil))
          (fail (check-error expected actual)))

        [["lux;None" _]]
        (check* (fp-put fp-pair true fixpoints) expected* actual)))

    [_ ["lux;AppT" [F A]]]
    (|do [actual* (apply-type F A)]
      (check* fixpoints expected actual*))

    [["lux;AllT" _] _]
    (|do [$var fresh-var
          expected* (apply-type expected $var)]
      (check* fixpoints expected* actual))

    [_ ["lux;AllT" _]]
    (|do [$var fresh-var
          actual* (apply-type actual $var)]
      (check* fixpoints expected actual*))

    [["lux;DataT" e!name] ["lux;DataT" a!name]]
    (if (= e!name a!name)
      (return (&/T fixpoints nil))
      (fail (str "[Type Error] Names don't match: " e!name " & " a!name)))

    [["lux;LambdaT" [eI eO]] ["lux;LambdaT" [aI aO]]]
    (|do [[fixpoints* _] (check* fixpoints aI eI)]
      (check* fixpoints* eO aO))

    [["lux;TupleT" e!members] ["lux;TupleT" a!members]]
    (do (do (prn 'e!members (&/|length e!members))
          (prn 'a!members (&/|length a!members)))
      (if (= (&/|length e!members) (&/|length a!members))
        (|do [fixpoints* (&/fold% (fn [fixp ea]
                                    (|let [[e a] ea]
                                      (do ;; (prn "lux;TupleT" 'ITER (show-type e) (show-type a))
                                          (|do [[fixp* _] (check* fixp e a)]
                                            (return fixp*)))))
                                  fixpoints
                                  (&/zip2 e!members a!members))
              ;; :let [_ (prn "lux;TupleT" 'DONE)]
              ]
          (return (&/T fixpoints* nil)))
        (do ;; (prn "lux;TupleT" (&/|length e!members) (&/|length a!members))
            ;; (prn "lux;TupleT"
            ;;      (&/fold str "" (&/|interpose " " (&/|map show-type e!members)))
            ;;      (&/fold str "" (&/|interpose " " (&/|map show-type a!members))))
            ;; (prn "lux;TupleT#fail" (fail "[Type Error] Tuples don't match in size."))
            (fail "[Type Error] Tuples don't match in size."))))
    
    [["lux;VariantT" e!cases] ["lux;VariantT" a!cases]]
    (if (= (&/|length e!cases) (&/|length a!cases))
      (|do [fixpoints* (&/fold% (fn [fixp slot]
                                  (prn "lux;VariantT" slot)
                                  (if-let [e!type (&/|get slot e!cases)]
                                    (if-let [a!type (&/|get slot a!cases)]
                                      (|do [[fixp* _] (check* fixp e!type a!type)]
                                        (return fixp*))
                                      (fail (check-error expected actual)))
                                    (fail (check-error expected actual))))
                                fixpoints
                                (&/|keys e!cases))]
        (return (&/T fixpoints* nil)))
      (fail "[Type Error] Variants don't match in size."))

    [["lux;RecordT" e!fields] ["lux;RecordT" a!fields]]
    (if (= (&/|length e!fields) (&/|length a!fields))
      (|do [fixpoints* (&/fold% (fn [fixp slot]
                                  (prn "lux;RecordT" slot)
                                  (if-let [e!type (&/|get slot e!fields)]
                                    (if-let [a!type (&/|get slot a!fields)]
                                      (|do [[fixp* _] (check* fixp e!type a!type)]
                                        (return fixp*))
                                      (fail (check-error expected actual)))
                                    (fail (check-error expected actual))))
                                fixpoints
                                (&/|keys e!fields))]
        (return (&/T fixpoints* nil)))
      (fail "[Type Error] Records don't match in size."))

    [_ _]
    (do (prn (show-type expected) (show-type actual))
      (assert false))

    ;; [["lux;BoundT" name] _]
    ;; (do (prn "lux;BoundT" name)
    ;;   (assert false))
    ;; ...
    
    ;; [_ ["lux;BoundT" name]]
    ;; ...
    ))

(defn check [expected actual]
  (|do [_ (check* init-fixpoints expected actual)]
    (return nil)))

(defn apply-lambda [func param]
  (matchv ::M/objects [func]
    [["lux;LambdaT" [input output]]]
    (|do [_ (check* init-fixpoints input param)]
      (return output))

    [["lux;AllT" [local-env local-name local-arg local-def]]]
    (|do [$var fresh-var
          func* (apply-type func $var)]
      (apply-lambda func* param))

    [_]
    (fail (str "[Type System] Can't apply type " (show-type func) " to type " (show-type param)))
    ))

(defn actual-type [type]
  (matchv ::M/objects [type]
    [["lux;AppT" [?all ?param]]]
    (|do [type* (apply-type ?all ?param)]
      (actual-type type*))

    [_]
    (return type)
    ))

(def Any (&/V "lux;AnyT" nil))
(def Nothing (&/V "lux;NothingT" nil))
(def Bool (&/V "lux;DataT" "java.lang.Boolean"))
(def Int (&/V "lux;DataT" "java.lang.Long"))
(def Real (&/V "lux;DataT" "java.lang.Double"))
(def Char (&/V "lux;DataT" "java.lang.Character"))
(def Text (&/V "lux;DataT" "java.lang.String"))
(def Unit (&/V "lux;TupleT" (&/|list)))

(def List
  (&/V "lux;AllT" (&/T (&/|table) "List" "a"
                       (&/V "lux;VariantT" (&/|list (&/T "lux;Nil" (&/V "lux;TupleT" (&/|list)))
                                                    (&/T "lux;Cons" (&/V "lux;TupleT" (&/|list (&/V "lux;BoundT" "a")
                                                                                               (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "List")
                                                                                                                    (&/V "lux;BoundT" "a")))))))))))

(def Type
  (let [Type (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "Type") (&/V "lux;BoundT" "_")))
        TypeEnv (&/V "lux;AppT" (&/T List (&/V "lux;TupleT" (&/|list Text Type))))
        Unit (&/V "lux;TupleT" (&/|list))
        TypePair (&/V "lux;TupleT" (&/|list Type Type))]
    (&/V "lux;AppT" (&/T (&/V "lux;AllT" (&/T (&/|list) "Type" "_"
                                              (&/V "lux;VariantT" (&/|list (&/T "lux;AnyT" Unit)
                                                                           (&/T "lux;NothingT" Unit)
                                                                           (&/T "lux;DataT" Text)
                                                                           (&/T "lux;TupleT" (&/V "lux;AppT" (&/T List Type)))
                                                                           (&/T "lux;VariantT" TypeEnv)
                                                                           (&/T "lux;RecordT" TypeEnv)
                                                                           (&/T "lux;LambdaT" TypePair)
                                                                           (&/T "lux;BoundT" Text)
                                                                           (&/T "lux;VarT" Int)
                                                                           (&/T "lux;AllT" (&/V "lux;TupleT" (&/|list TypeEnv Text Text Type)))
                                                                           (&/T "lux;AppT" TypePair)
                                                                           ))))
                         (&/V "lux;NothingT" nil)))))
