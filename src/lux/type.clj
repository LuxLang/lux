(ns lux.type
  (:refer-clojure :exclude [deref apply merge])
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            [lux.base :as & :refer [|do return* return fail fail* assert! |let]]))

;; [Util]
(def ^:private success (return nil))

(defn lookup [type]
  (matchv ::M/objects [type]
    [["lux;VarT" id]]
    (fn [state]
      (if-let [type* (->> state (&/get$ "lux;types") (&/get$ "lux;mappings") (&/|get id))]
        (return* state type*)
        (fail* (str "Unknown type-var: " id))))

    [_]
    (fail "[Type Error] Can't lookup non-vars.")))

(defn deref [id]
  (fn [state]
    (if-let [type* (->> state (&/get$ "lux;types") (&/get$ "lux;mappings") (&/|get id))]
      (matchv ::M/objects [type*]
        [["lux;Some" type]]
        (return* state type)
        
        [["lux;None" _]]
        (fail* (str "Unbound type-var: " id)))
      (fail* (str "Unknown type-var: " id)))))

(defn reset [id type]
  (fn [state]
    (if-let [_ (->> state (&/get$ "lux;types") (&/get$ "lux;mappings") (&/|get id))]
      (return* (&/update$ "lux;types" (fn [ts] (&/update$ "lux;mappings" #(&/|put id (&/V "lux;Some" type) %)
                                                         ts))
                          state)
               nil)
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
        (&/try-all% (&/|list (|do [=type (deref ?id)]
                               (clean tvar =type))
                             (return type)))
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

    [["lux;DataT" [name params]]]
    (str "(^ " name " [" (->> params (&/|map show-type) (&/|interpose " ") (&/fold str "")) "])")
    
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

    [["lux;DataT" [xname xparams]] ["lux;DataT" [yname yparams]]]
    (&/fold (fn [old xy] (and old (type= (aget xy 0) (aget xy 1))))
            (= xname yname)
            (&/zip2 xparams yparams))

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

    [["lux;DataT" [?name ?params]]]
    (&/V "lux;DataT" (&/T ?name (&/|map (partial beta-reduce env) ?params)))

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
    success

    [_ ["lux;NothingT" _]]
    success

    [["lux;VarT" ?id] _]
    (&/try-all% (&/|list (|do [bound (deref ?id)]
                           (check* fixpoints bound actual))
                         (reset ?id actual)))
    
    [_ ["lux;VarT" ?id]]
    (&/try-all% (&/|list (|do [bound (deref ?id)]
                           (check* fixpoints expected bound))
                         (reset ?id expected)))

    [["lux;AppT" [F A]] _]
    (|do [expected* (apply-type F A)
           :let [fp-pair (&/T expected actual)]]
      (matchv ::M/objects [(fp-get fp-pair fixpoints)]
        [["lux;Some" ?]]
        (if ?
          success
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

    [["lux;DataT" [e!name e!params]] ["lux;DataT" [a!name a!params]]]
    (cond (not= e!name a!name)
          (fail (str "[Type Error] Names don't match: " e!name " & " a!name))
          
          (not= (&/|length e!params) (&/|length a!params))
          (fail "[Type Error] Params don't match in size.")

          :else
          (|do [_ (&/map% (fn [ea]
                             (|let [[e a] ea]
                               (check* fixpoints e a)))
                           (&/zip2 e!params a!params))]
            success))

    [["lux;LambdaT" [eI eO]] ["lux;LambdaT" [aI aO]]]
    (|do [_ (check* fixpoints aI eI)]
      (check* fixpoints eO aO))

    [["lux;TupleT" e!members] ["lux;TupleT" a!members]]
    (if (= (&/|length e!members) (&/|length a!members))
      (|do [_ (&/map% (fn [ea]
                         (|let [[e a] ea]
                           (do ;; (prn "lux;TupleT" 'ITER (show-type e) (show-type a))
                               (check* fixpoints e a))))
                       (&/zip2 e!members a!members))
             ;; :let [_ (prn "lux;TupleT" 'DONE)]
             ]
        success)
      (do ;; (prn "lux;TupleT" (&/|length e!members) (&/|length a!members))
          ;; (prn "lux;TupleT"
          ;;      (&/fold str "" (&/|interpose " " (&/|map show-type e!members)))
          ;;      (&/fold str "" (&/|interpose " " (&/|map show-type a!members))))
          ;; (prn "lux;TupleT#fail" (fail "[Type Error] Tuples don't match in size."))
          (fail "[Type Error] Tuples don't match in size.")))
    
    [["lux;VariantT" e!cases] ["lux;VariantT" a!cases]]
    (|do [_ (&/map% (fn [kv]
                       (|let [[k av] kv]
                         (if-let [ev (&/|get k e!cases)]
                           (check* fixpoints ev av)
                           (fail (str "[Type Error] The expected variant cannot handle case: #" k)))))
                     a!cases)]
      success)

    [["lux;RecordT" e!fields] ["lux;RecordT" a!fields]]
    (if (= (&/|length e!fields) (&/|length a!fields))
      (|do [_ (&/map% (fn [slot]
                         (if-let [e!type (&/|get e!fields slot)]
                           (if-let [a!type (&/|get a!fields slot)]
                             (check* fixpoints e!type a!type)
                             (fail (check-error expected actual)))
                           (fail (check-error expected actual))))
                       (&/|keys e!fields))]
        success)
      (fail "[Type Error] Records don't match in size."))

    ;; [["lux;BoundT" name] _]
    ;; (do (prn "lux;BoundT" name)
    ;;   (assert false))
    ;; ...
    
    ;; [_ ["lux;BoundT" name]]
    ;; ...
    ))

(def check (partial check* init-fixpoints))

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

(def Any (&/V "lux;AnyT" nil))
(def Nothing (&/V "lux;NothingT" nil))
(def Bool (&/V "lux;DataT" (&/T "java.lang.Boolean" (&/|list))))
(def Int (&/V "lux;DataT" (&/T "java.lang.Long" (&/|list))))
(def Real (&/V "lux;DataT" (&/T "java.lang.Double" (&/|list))))
(def Char (&/V "lux;DataT" (&/T "java.lang.Character" (&/|list))))
(def Text (&/V "lux;DataT" (&/T "java.lang.String" (&/|list))))
(def Unit (&/V "lux;TupleT" (&/|list)))

(def List
  (&/V "lux;AllT" (&/T (&/|table) "List" "a"
                       (&/V "lux;VariantT" (&/|list (&/T "lux;Nil" (&/V "lux;TupleT" (&/|list)))
                                                    (&/T "lux;Cons" (&/V "lux;TupleT" (&/|list (&/V "lux;BoundT" "a")
                                                                                               (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "List")
                                                                                                                    (&/V "lux;BoundT" "a")))))))))))

(def Type
  (let [Type (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "Type") (&/V "lux;BoundT" "")))
        TypeEnv (&/V "lux;AppT" (&/T List (&/V "lux;TupleT" (&/|list Text Type))))
        Unit (&/V "lux;TupleT" (&/|list))
        TypeList (&/V "lux;AppT" (&/T List Type))
        TypePair (&/V "lux;TupleT" (&/|list Type Type))]
    (&/V "lux;AppT" (&/T (&/V "lux;AllT" (&/T (&/|list) "Type" ""
                                              (&/V "lux;VariantT" (&/|list (&/T "lux;AnyT" Unit)
                                                                           (&/T "lux;NothingT" Unit)
                                                                           (&/T "lux;DataT" (&/V "lux;TupleT" (&/|list Text TypeList)))
                                                                           (&/T "lux;TupleT" TypeList)
                                                                           (&/T "lux;VariantT" TypeEnv)
                                                                           (&/T "lux;RecordT" TypeEnv)
                                                                           (&/T "lux;LambdaT" TypePair)
                                                                           (&/T "lux;BoundT" Text)
                                                                           (&/T "lux;VarT" Int)
                                                                           (&/T "lux;AllT" (&/V "lux;TupleT" (&/|list TypeEnv Text Text Type)))
                                                                           (&/T "lux;AppT" TypePair)
                                                                           ))))
                         (&/V "lux;NothingT" nil)))))

(let [&& #(and %1 %2)]
  (defn merge [x y]
    (matchv ::M/objects [x y]
      [_ ["lux;AnyT" _]]
      (return y)

      [["lux;AnyT" _] _]
      (return x)

      [_ ["lux;NothingT" _]]
      (return x)

      [["lux;NothingT" _] _]
      (return y)

      [["lux;DataT" [xname xparams]] ["lux;DataT" [yname yparams]]]
      (if (and (= xname yname)
               (= (&/|length xparams) (&/|length yparams)))
        (fail (str "[Type System Error] Can't merge types: " (show-type x) " and " (show-type y)))
        (|do [xyparams (&/map% (fn [xy]
                                 (|let [[xp yp] xy]
                                   (merge xp yp)))
                               (&/zip2 xparams yparams))]
          (return (&/V "lux;DataT" (&/T xname xyparams)))))

      [["lux;TupleT" xmembers] ["lux;TupleT" ymembers]]
      (if (= (&/|length xmembers) (&/|length ymembers))
        (fail (str "[Type System Error] Can't merge types: " (show-type x) " and " (show-type y)))
        (|do [xymembers (&/map% (fn [xy]
                                  (|let [[xp yp] xy]
                                    (merge xp yp)))
                                (&/zip2 xmembers ymembers))]
          (return (&/V "lux;TupleT" xymembers))))

      [["lux;VariantT" x!cases] ["lux;VariantT" y!cases]]
      (|do [cases (&/fold% (fn [cases kv]
                             (matchv ::M/objects [kv]
                               [[k v]]
                               (if-let [cv (&/|get k cases)]
                                 (|do [v* (merge cv v)]
                                   (return (&/|put k v* cases)))
                                 (return (&/|put k v cases)))))
                           x!cases
                           y!cases)]
        (return (&/V "lux;VariantT" cases)))
      
      [["lux;RecordT" x!fields] ["lux;RecordT" y!fields]]
      (if (= (&/|length x!fields) (&/|length y!fields))
        (|do [fields (&/fold% (fn [fields kv]
                                (matchv ::M/objects [kv]
                                  [[k v]]
                                  (if-let [cv (&/|get k fields)]
                                    (|do [v* (merge cv v)]
                                      (return (&/|put k v* fields)))
                                    (fail (str "[Type System Error] Incompatible records: " (show-type x) " and " (show-type y))))))
                              x!fields
                              y!fields)]
          (return (&/V "lux;RecordT" fields)))
        (fail (str "[Type System Error] Incompatible records: " (show-type x) " and " (show-type y))))

      [["lux;LambdaT" [xinput xoutput]] ["lux;LambdaT" [yinput youtput]]]
      (|do [xyinput (check xinput yinput)
            xyoutput (check xoutput youtput)]
        (return (&/V "lux;LambdaT" (&/T xyinput xyoutput))))
      
      [_ _]
      (fail (str "[Type System Error] Can't merge types: " (show-type x) " and " (show-type y))))))

(comment
  (do (def Real (&/V "lux;DataT" (&/T "java.lang.Long" (&/|list))))
    (def RealT (&/V "lux;VariantT" (&/|list (&/T "lux;DataT" (&/V "lux;TupleT" (&/|list Text
                                                                                        (&/V "lux;VariantT" (&/|list (&/T "lux;Nil" (&/V "lux;TupleT" (&/|list)))))))))))
    )
  
  (matchv ::M/objects [((check Type RealT)
                        (&/init-state nil))]
    [["lux;Left" ?msg]]
    (assert false ?msg)

    [_]
    (println "YEAH!"))

  (matchv ::M/objects [((check List (&/V "lux;AppT" (&/T List Real)))
                        (&/init-state nil))]
    [["lux;Left" ?msg]]
    (assert false ?msg)

    [_]
    (println "YEAH!"))
  )
