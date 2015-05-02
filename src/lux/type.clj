(ns lux.type
  (:refer-clojure :exclude [deref apply merge bound?])
  (:require [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            [lux.base :as & :refer [|do return* return fail fail* assert! |let]]))

(declare show-type)

;; [Util]
(def Bool (&/V "lux;DataT" "java.lang.Boolean"))
(def Int (&/V "lux;DataT" "java.lang.Long"))
(def Real (&/V "lux;DataT" "java.lang.Double"))
(def Char (&/V "lux;DataT" "java.lang.Character"))
(def Text (&/V "lux;DataT" "java.lang.String"))
(def Unit (&/V "lux;TupleT" (&/|list)))
(def $Void (&/V "lux;VariantT" (&/|list)))

(def List
  (&/V "lux;AllT" (&/T (&/V "lux;None" nil) "List" "a"
                       (&/V "lux;VariantT" (&/|list (&/T "lux;Nil" Unit)
                                                    (&/T "lux;Cons" (&/V "lux;TupleT" (&/|list (&/V "lux;BoundT" "a")
                                                                                               (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "List")
                                                                                                                    (&/V "lux;BoundT" "a")))))))))))

(def Maybe
  (&/V "lux;AllT" (&/T (&/V "lux;None" nil) "Maybe" "a"
                       (&/V "lux;VariantT" (&/|list (&/T "lux;None" Unit)
                                                    (&/T "lux;Some" (&/V "lux;BoundT" "a")))))))

(def Type
  (let [Type (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "Type") (&/V "lux;BoundT" "_")))
        TypeEnv (&/V "lux;AppT" (&/T List (&/V "lux;TupleT" (&/|list Text Type))))
        TypePair (&/V "lux;TupleT" (&/|list Type Type))]
    (&/V "lux;AppT" (&/T (&/V "lux;AllT" (&/T (&/V "lux;None" nil) "Type" "_"
                                              (&/V "lux;VariantT" (&/|list (&/T "lux;DataT" Text)
                                                                           (&/T "lux;TupleT" (&/V "lux;AppT" (&/T List Type)))
                                                                           (&/T "lux;VariantT" TypeEnv)
                                                                           (&/T "lux;RecordT" TypeEnv)
                                                                           (&/T "lux;LambdaT" TypePair)
                                                                           (&/T "lux;BoundT" Text)
                                                                           (&/T "lux;VarT" Int)
                                                                           (&/T "lux;AllT" (&/V "lux;TupleT" (&/|list (&/V "lux;AppT" (&/T Maybe TypeEnv)) Text Text Type)))
                                                                           (&/T "lux;AppT" TypePair)
                                                                           (&/T "lux;ExT" Int)
                                                                           ))))
                         $Void))))

(defn fAll [name arg body]
  (&/V "lux;AllT" (&/T (&/V "lux;None" nil) name arg body)))

(def Bindings
  (fAll "Bindings" "k"
        (fAll "" "v"
              (&/V "lux;RecordT" (&/|list (&/T "lux;counter" Int)
                                          (&/T "lux;mappings" (&/V "lux;AppT" (&/T List
                                                                                   (&/V "lux;TupleT" (&/|list (&/V "lux;BoundT" "k")
                                                                                                              (&/V "lux;BoundT" "v")))))))))))

(def Env
  (let [bindings (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T Bindings (&/V "lux;BoundT" "k")))
                                      (&/V "lux;BoundT" "v")))]
    (fAll "Env" "k"
          (fAll "" "v"
                (&/V "lux;RecordT"
                     (&/|list (&/T "lux;name" Text)
                              (&/T "lux;inner-closures" Int)
                              (&/T "lux;locals" bindings)
                              (&/T "lux;closure" bindings)
                              ))))))

(def Cursor
  (&/V "lux;TupleT" (&/|list Text Int Int)))

(def Meta
  (fAll "Meta" "m"
        (fAll "" "v"
              (&/V "lux;VariantT" (&/|list (&/T "lux;Meta" (&/V "lux;TupleT" (&/|list (&/V "lux;BoundT" "m")
                                                                                      (&/V "lux;BoundT" "v")))))))))

(def Syntax*
  (let [Syntax* (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "w")
                                     (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "Syntax'")
                                                          (&/V "lux;BoundT" "w")))))
        Syntax*List (&/V "lux;AppT" (&/T List Syntax*))
        Ident (&/V "lux;TupleT" (&/|list Text Text))]
    (fAll "Syntax'" "w"
          (&/V "lux;VariantT" (&/|list (&/T "lux;Bool" Bool)
                                       (&/T "lux;Int" Int)
                                       (&/T "lux;Real" Real)
                                       (&/T "lux;Char" Char)
                                       (&/T "lux;Text" Text)
                                       (&/T "lux;Symbol" Ident)
                                       (&/T "lux;Tag" Ident)
                                       (&/T "lux;Form" Syntax*List)
                                       (&/T "lux;Tuple" Syntax*List)
                                       (&/T "lux;Record" (&/V "lux;AppT" (&/T List (&/V "lux;TupleT" (&/|list Syntax* Syntax*))))))
               ))))

(def Syntax
  (let [w (&/V "lux;AppT" (&/T Meta Cursor))]
    (&/V "lux;AppT" (&/T w (&/V "lux;AppT" (&/T Syntax* w))))))

(def ^:private SyntaxList (&/V "lux;AppT" (&/T List Syntax)))

(def Either
  (fAll "_" "l"
        (fAll "" "r"
              (&/V "lux;VariantT" (&/|list (&/T "lux;Left" (&/V "lux;BoundT" "l"))
                                           (&/T "lux;Right" (&/V "lux;BoundT" "r")))))))

(def StateE
  (fAll "StateE" "s"
        (fAll "" "a"
              (&/V "lux;LambdaT" (&/T (&/V "lux;BoundT" "s")
                                      (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T Either Text))
                                                           (&/V "lux;TupleT" (&/|list (&/V "lux;BoundT" "s")
                                                                                      (&/V "lux;BoundT" "a"))))))))))

(def Reader
  (&/V "lux;AppT" (&/T List
                       (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T Meta Cursor))
                                            Text)))))

(def HostState
  (&/V "lux;RecordT"
       (&/|list (&/T "lux;writer" (&/V "lux;DataT" "org.objectweb.asm.ClassWriter"))
                (&/T "lux;loader" (&/V "lux;DataT" "java.lang.ClassLoader"))
                (&/T "lux;eval-ctor" Int))))

(def DefData*
  (fAll "DefData'" ""
        (&/V "lux;VariantT" (&/|list (&/T "lux;TypeD" Unit)
                                     (&/T "lux;ValueD" Type)
                                     (&/T "lux;MacroD" (&/V "lux;BoundT" ""))))))

(def CompilerState
  (&/V "lux;AppT" (&/T (fAll "CompilerState" ""
                             (&/V "lux;RecordT"
                                  (&/|list (&/T "lux;source" (&/V "lux;AppT" (&/T Maybe Reader)))
                                           (&/T "lux;modules" (&/V "lux;AppT" (&/T List (&/V "lux;TupleT"
                                                                                             (&/|list Text
                                                                                                      (&/V "lux;AppT" (&/T List (&/V "lux;TupleT"
                                                                                                                                     (&/|list Text
                                                                                                                                              (&/V "lux;AppT" (&/T DefData*
                                                                                                                                                                   (&/V "lux;LambdaT" (&/T SyntaxList
                                                                                                                                                                                           (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T StateE (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "CompilerState")
                                                                                                                                                                                                                                                                 (&/V "lux;BoundT" "")))))
                                                                                                                                                                                                                SyntaxList)))))))))))))))
                                           (&/T "lux;module-aliases" (&/V "lux;AppT" (&/T List $Void)))
                                           (&/T "lux;envs" (&/V "lux;AppT" (&/T List
                                                                                (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T Env Text))
                                                                                                     $Void)))))
                                           (&/T "lux;types" (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T Bindings Int)) Type)))
                                           (&/T "lux;host" HostState)
                                           (&/T "lux;seed" Int))))
                       $Void)))

(def Macro
  (&/V "lux;LambdaT" (&/T SyntaxList
                          (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T StateE CompilerState))
                                               SyntaxList)))))

(defn bound? [id]
  (fn [state]
    (if-let [type (->> state (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS) (&/|get id))]
      (matchv ::M/objects [type]
        [["lux;Some" type*]]
        (return* state true)
        
        [["lux;None" _]]
        (return* state false))
      (fail* (str "[Type Error] <bound?> Unknown type-var: " id)))))

(defn deref [id]
  (fn [state]
    (let [mappings (->> state (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS))]
      (if-let [type* (->> mappings (&/|get id))]
        (matchv ::M/objects [type*]
          [["lux;Some" type]]
          (return* state type)
          
          [["lux;None" _]]
          (fail* (str "[Type Error] Unbound type-var: " id)))
        (fail* (str "[Type Error] <deref> Unknown type-var: " id))))))

(defn set-var* [id type]
  (fn [state]
    (if-let [tvar (->> state (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS) (&/|get id))]
      (return* (&/update$ &/$TYPES (fn [ts] (&/update$ &/$MAPPINGS #(&/|put id (&/V "lux;Some" type) %)
                                                      ts))
                          state)
               nil)
      (fail* (str "[Type Error] <set-var> Unknown type-var: " id " | " (->> state (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS) &/|length))))))

(defn set-var [id type]
  (fn [state]
    (if-let [tvar (->> state (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS) (&/|get id))]
      (do ;; (prn 'set-var (aget tvar 0))
          (matchv ::M/objects [tvar]
            [["lux;Some" bound]]
            (fail* (str "[Type Error] Can't rebind type var: " id " | Current type: " (show-type bound)))
            
            [["lux;None" _]]
            (do ;; (prn 'set-var id (show-type type))
              (return* (&/update$ &/$TYPES (fn [ts] (&/update$ &/$MAPPINGS #(&/|put id (&/V "lux;Some" type) %)
                                                              ts))
                                  state)
                       nil))))
      (fail* (str "[Type Error] <set-var> Unknown type-var: " id " | " (->> state (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS) &/|length))))))

;; [Exports]
;; Type vars
(def ^:private create-var
  (fn [state]
    (let [id (->> state (&/get$ &/$TYPES) (&/get$ &/$COUNTER))]
      (return* (&/update$ &/$TYPES #(->> %
                                            (&/update$ &/$COUNTER inc)
                                            (&/update$ &/$MAPPINGS (fn [ms] (&/|put id (&/V "lux;None" nil) ms))))
                          state)
               id))))

(def existential
  (|do [seed &/gen-id]
    (return (&/V "lux;ExT" seed))))

(declare clean*)
(defn ^:private delete-var [id]
  (|do [? (bound? id)
        _ (if ?
            (return nil)
            (|do [ex existential]
              (set-var id ex)))]
    (fn [state]
      (&/run-state (|do [mappings* (&/map% (fn [binding]
                                             (|let [[?id ?type] binding]
                                               (if (= id ?id)
                                                 (return binding)
                                                 (matchv ::M/objects [?type]
                                                   [["lux;None" _]]
                                                   (return binding)

                                                   [["lux;Some" ?type*]]
                                                   (matchv ::M/objects [?type*]
                                                     [["lux;VarT" ?id*]]
                                                     (if (= id ?id*)
                                                       (return (&/T ?id (&/V "lux;None" nil)))
                                                       (return binding)
                                                       ;; (|do [?type** (clean* id ?type*)]
                                                       ;;   (return (&/T ?id (&/V "lux;Some" ?type**))))
                                                       )

                                                     [_]
                                                     (|do [?type** (clean* id ?type*)]
                                                       (return (&/T ?id (&/V "lux;Some" ?type**)))))
                                                   ))))
                                           (->> state (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS)))]
                     (fn [state]
                       (return* (&/update$ &/$TYPES #(->> %
                                                          (&/update$ &/$COUNTER dec)
                                                          (&/set$ &/$MAPPINGS (&/|remove id mappings*)))
                                           state)
                                nil)))
                   state))))

(defn with-var [k]
  (|do [id create-var
        output (k (&/V "lux;VarT" id))
        _ (delete-var id)]
    (return output)))

;; (def delete-vars
;;   (|do [vars #(->> % (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS) &/|keys (return* %))
;;         _ (&/map% delete-var vars)]
;;     (return nil)))

(defn with-vars [amount k]
  (|do [=vars (&/map% (constantly create-var) (&/|range amount))
        output (k (&/|map #(&/V "lux;VarT" %) =vars))
        _ (&/map% delete-var (&/|reverse =vars))]
    (return output)))

(defn ^:private clean* [?tid type]
  (matchv ::M/objects [type]
    [["lux;VarT" ?id]]
    (if (= ?tid ?id)
      (&/try-all% (&/|list (deref ?id)
                           (fail "##5##")))
      (return type))
    
    [["lux;LambdaT" [?arg ?return]]]
    (|do [=arg (clean* ?tid ?arg)
          =return (clean* ?tid ?return)]
      (return (&/V "lux;LambdaT" (&/T =arg =return))))

    [["lux;AppT" [?lambda ?param]]]
    (|do [=lambda (clean* ?tid ?lambda)
          =param (clean* ?tid ?param)]
      (return (&/V "lux;AppT" (&/T =lambda =param))))

    [["lux;TupleT" ?members]]
    (|do [=members (&/map% (partial clean* ?tid) ?members)]
      (return (&/V "lux;TupleT" =members)))
    
    [["lux;VariantT" ?members]]
    (|do [=members (&/map% (fn [[k v]]
                             (|do [=v (clean* ?tid v)]
                               (return (&/T k =v))))
                           ?members)]
      (return (&/V "lux;VariantT" =members)))

    [["lux;RecordT" ?members]]
    (|do [=members (&/map% (fn [[k v]]
                             (|do [=v (clean* ?tid v)]
                               (return (&/T k =v))))
                           ?members)]
      (return (&/V "lux;RecordT" =members)))

    [["lux;AllT" [?env ?name ?arg ?body]]]
    (|do [=env (matchv ::M/objects [?env]
                 [["lux;None" _]]
                 (return ?env)

                 [["lux;Some" ?env*]]
                 (|do [clean-env (&/map% (fn [[k v]]
                                           (|do [=v (clean* ?tid v)]
                                             (return (&/T k =v))))
                                         ?env*)]
                   (return (&/V "lux;Some" clean-env))))
          body* (clean* ?tid ?body)]
      (return (&/V "lux;AllT" (&/T =env ?name ?arg body*))))

    [_]
    (return type)
    ))

(defn clean [tvar type]
  ;; (prn "^^ clean ^^")
  (matchv ::M/objects [tvar]
    [["lux;VarT" ?id]]
    (clean* ?id type)
    
    [_]
    (fail (str "[Type Error] Not type-var: " (show-type tvar)))))

(defn show-type [type]
  ;; (prn 'show-type (aget type 0))
  (matchv ::M/objects [type]
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
                                (str "#" k " " (show-type v)))))
                    (&/|interpose " ")
                    (&/fold str "")) ")")

    [["lux;LambdaT" [input output]]]
    (str "(-> " (show-type input) " " (show-type output) ")")

    [["lux;VarT" id]]
    (str "⌈" id "⌋")

    [["lux;BoundT" name]]
    name

    [["lux;ExT" ?id]]
    (str "⟨" ?id "⟩")

    [["lux;AppT" [?lambda ?param]]]
    (str "(" (show-type ?lambda) " " (show-type ?param) ")")
    
    [["lux;AllT" [?env ?name ?arg ?body]]]
    (let [[args body] (loop [args (list ?arg)
                             body* ?body]
                        (matchv ::M/objects [body*]
                          [["lux;AllT" [?env* ?name* ?arg* ?body*]]]
                          (recur (cons ?arg* args) ?body*)

                          [_]
                          [args body*]))]
      (str "(All " ?name " [" (->> args reverse (interpose " ") (reduce str "")) "] " (show-type body) ")"))

    [_]
    (assert false (prn-str 'show-type (aget type 0) (class (aget type 1))))
    ))

(defn type= [x y]
  ;; (prn "^^ type= ^^")
  (let [output (matchv ::M/objects [x y]
                 [["lux;DataT" xname] ["lux;DataT" yname]]
                 (= xname yname)

                 [["lux;TupleT" xelems] ["lux;TupleT" yelems]]
                 (&/fold (fn [old xy]
                           (|let [[x* y*] xy]
                             (and old
                                  (type= x* y*))))
                         true
                         (&/zip2 xelems yelems))

                 [["lux;VariantT" xcases] ["lux;VariantT" ycases]]
                 (and (= (&/|length xcases) (&/|length ycases))
                      (&/fold (fn [old case]
                                (and old
                                     (type= (&/|get case xcases) (&/|get case ycases))))
                              true
                              (&/|keys xcases)))

                 [["lux;RecordT" xfields] ["lux;RecordT" yfields]]
                 (and (= (&/|length xfields) (&/|length yfields))
                      (&/fold (fn [old field]
                                (and old
                                     (type= (&/|get field xfields) (&/|get field yfields))))
                              true
                              (&/|keys xfields)))

                 [["lux;LambdaT" [xinput xoutput]] ["lux;LambdaT" [yinput youtput]]]
                 (and (type= xinput yinput)
                      (type= xoutput youtput))

                 [["lux;VarT" xid] ["lux;VarT" yid]]
                 (= xid yid)

                 [["lux;BoundT" xname] ["lux;BoundT" yname]]
                 (= xname yname)

                 [["lux;ExT" xid] ["lux;ExT" yid]]
                 (= xid yid)

                 [["lux;AppT" [xlambda xparam]] ["lux;AppT" [ylambda yparam]]]
                 (and (type= xlambda ylambda)
                      (type= xparam yparam))
                 
                 [["lux;AllT" [xenv xname xarg xbody]] ["lux;AllT" [yenv yname yarg ybody]]]
                 (do ;; (prn 'TESTING_ALLT
                     ;;      'NAME [xname yname] (= xname yname)
                     ;;      'ARG (= xarg yarg)
                     ;;      'LENGTH [(&/|length xenv) (&/|length yenv)] (= (&/|length xenv) (&/|length yenv)))
                     (and (= xname yname)
                          (= xarg yarg)
                          ;; (matchv ::M/objects [xenv yenv]
                          ;;   [["lux;None" _] ["lux;None" _]]
                          ;;   true

                          ;;   [["lux;Some" xenv*] ["lux;Some" yenv*]]
                          ;;   (&/fold (fn [old bname]
                          ;;             (and old
                          ;;                  (type= (&/|get bname xenv*) (&/|get bname yenv*))))
                          ;;           (= (&/|length xenv*) (&/|length yenv*))
                          ;;           (&/|keys xenv*))

                          ;;   [_ _]
                          ;;   false)
                          (type= xbody ybody)
                          ))

                 [_ _]
                 (do ;; (prn 'type= (show-type x) (show-type y))
                     false)
                 )]
    ;; (prn 'type= output (show-type x) (show-type y))
    output))

(defn ^:private fp-get [k fixpoints]
  (|let [[e a] k]
    (matchv ::M/objects [fixpoints]
      [["lux;Nil" _]]
      (&/V "lux;None" nil)

      [["lux;Cons" [[[e* a*] v*] fixpoints*]]]
      (if (and (type= e e*)
               (type= a a*))
        (&/V "lux;Some" v*)
        (fp-get k fixpoints*))
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
    (matchv ::M/objects [?local-env]
      [["lux;None" _]]
      (&/V "lux;AllT" (&/T (&/V "lux;Some" env) ?local-name ?local-arg ?local-def))

      [["lux;Some" _]]
      type)

    [["lux;LambdaT" [?input ?output]]]
    (&/V "lux;LambdaT" (&/T (beta-reduce env ?input) (beta-reduce env ?output)))

    [["lux;BoundT" ?name]]
    (if-let [bound (&/|get ?name env)]
      (beta-reduce env bound)
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
    (let [;; _ (prn 'apply-type/local-env (aget local-env 0) (show-type type-fn))
          local-env* (matchv ::M/objects [local-env]
                       [["lux;None" _]]
                       (&/|table)

                       [["lux;Some" local-env*]]
                       local-env*)]
      (return (beta-reduce (->> local-env*
                                (&/|put local-name type-fn)
                                (&/|put local-arg param))
                           local-def)))

    [["lux;AppT" [F A]]]
    (|do [type-fn* (apply-type F A)]
      (apply-type type-fn* param))
    
    [_]
    (fail (str "[Type System] Can't apply type function " (show-type type-fn) " to type " (show-type param)))))

(def init-fixpoints (&/|list))

(defn ^:private check* [fixpoints expected actual]
  ;; (prn "^^ check* ^^")
  ;; (prn 'check* (aget expected 0) (aget actual 0))
  ;; (prn 'check* (show-type expected) (show-type actual))
  (matchv ::M/objects [expected actual]
    [["lux;VarT" ?eid] ["lux;VarT" ?aid]]
    (if (= ?eid ?aid)
      (return (&/T fixpoints nil))
      (|do [ebound (&/try-all% (&/|list (|do [ebound (&/try-all% (&/|list (deref ?eid)
                                                                          (fail "##4##")))]
                                          (return (&/V "lux;Some" ebound)))
                                        (return (&/V "lux;None" nil))))
            abound (&/try-all% (&/|list (|do [abound (&/try-all% (&/|list (deref ?aid)
                                                                          (fail "##3##")))]
                                          (return (&/V "lux;Some" abound)))
                                        (return (&/V "lux;None" nil))))]
        (matchv ::M/objects [ebound abound]
          [["lux;None" _] ["lux;None" _]]
          ;; (|do [_ (set-var ?aid expected)]
          ;;   (return (&/T fixpoints nil)))
          (|do [_ (set-var ?eid actual)]
            (return (&/T fixpoints nil)))
          
          [["lux;Some" etype] ["lux;None" _]]
          (check* fixpoints etype actual)

          [["lux;None" _] ["lux;Some" atype]]
          (check* fixpoints expected atype)

          [["lux;Some" etype] ["lux;Some" atype]]
          (check* fixpoints etype atype)))
      )
    
    [["lux;VarT" ?id] _]
    (&/try-all% (&/|list (|do [_ (set-var ?id actual)]
                           (return (&/T fixpoints nil)))
                         (|do [bound (&/try-all% (&/|list (deref ?id)
                                                          (fail "##1##")))]
                           (check* fixpoints bound actual))))
    
    [_ ["lux;VarT" ?id]]
    (&/try-all% (&/|list (|do [_ (set-var ?id expected)]
                           (return (&/T fixpoints nil)))
                         (|do [bound (&/try-all% (&/|list (deref ?id)
                                                          (fail "##2##")))]
                           (check* fixpoints expected bound))))

    [["lux;AppT" [["lux;VarT" ?eid] A1]] ["lux;AppT" [["lux;VarT" ?aid] A2]]]
    (|do [_ (check* fixpoints (&/V "lux;VarT" ?eid) (&/V "lux;VarT" ?aid))
          _ (check* fixpoints A1 A2)]
      (return (&/T fixpoints nil)))
    
    ;; [["lux;AppT" [["lux;VarT" ?id] A1]] ["lux;AppT" [F2 A2]]]
    ;; (|do [[fixpoints* _] (check* fixpoints (&/V "lux;VarT" ?id) F2)
    ;;       [fixpoints** _] (check* fixpoints* A1 A2)]
    ;;   (return (&/T fixpoints** nil)))
    [["lux;AppT" [["lux;VarT" ?id] A1]] ["lux;AppT" [F2 A2]]]
    (|do [[fixpoints* _] (check* fixpoints (&/V "lux;VarT" ?id) F2)
          e* (apply-type F2 A1)
          a* (apply-type F2 A2)
          [fixpoints** _] (check* fixpoints* e* a*)]
      (return (&/T fixpoints** nil)))

    ;; [["lux;AppT" [F1 A1]] ["lux;AppT" [["lux;VarT" ?id] A2]]]
    ;; (|do [[fixpoints* _] (check* fixpoints F1 (&/V "lux;VarT" ?id))
    ;;       [fixpoints** _] (check* fixpoints* A1 A2)]
    ;;   (return (&/T fixpoints** nil)))
    [["lux;AppT" [F1 A1]] ["lux;AppT" [["lux;VarT" ?id] A2]]]
    (|do [[fixpoints* _] (check* fixpoints F1 (&/V "lux;VarT" ?id))
          e* (apply-type F1 A1)
          a* (apply-type F1 A2)
          [fixpoints** _] (check* fixpoints* e* a*)]
      (return (&/T fixpoints** nil)))

    ;; [["lux;AppT" [F1 A1]] ["lux;AppT" [F2 A2]]]
    ;; (|do [[fixpoints* _] (check* (fp-put fp-pair true fixpoints) F1 F2)
    ;;       [fixpoints** _] (check* fixpoints* A1 A2)]
    ;;   (return (&/T fixpoints** nil)))
    
    [["lux;AppT" [F A]] _]
    (let [fp-pair (&/T expected actual)
          ;; _ (prn 'LEFT_APP (&/|length fixpoints))
          _ (when (> (&/|length fixpoints) 40)
              (println 'FIXPOINTS (->> (&/|keys fixpoints)
                                       (&/|map (fn [pair]
                                                 (|let [[e a] pair]
                                                   (str (show-type e) ":+:"
                                                        (show-type a)))))
                                       (&/|interpose "\n\n")
                                       (&/fold str "")))
              (assert false))]
      (matchv ::M/objects [(fp-get fp-pair fixpoints)]
        [["lux;Some" ?]]
        (if ?
          (return (&/T fixpoints nil))
          (fail (check-error expected actual)))

        [["lux;None" _]]
        (|do [expected* (apply-type F A)]
          (check* (fp-put fp-pair true fixpoints) expected* actual))))

    [_ ["lux;AppT" [F A]]]
    (|do [actual* (apply-type F A)]
      (check* fixpoints expected actual*))
    ;; (let [fp-pair (&/T expected actual)
    ;;       _ (prn 'RIGHT_APP (&/|length fixpoints))
    ;;       _ (when (> (&/|length fixpoints) 10)
    ;;           (println 'FIXPOINTS (->> (&/|keys fixpoints)
    ;;                                    (&/|map (fn [pair]
    ;;                                              (|let [[e a] pair]
    ;;                                                (str (show-type e) ":+:"
    ;;                                                     (show-type a)))))
    ;;                                    (&/|interpose "\n\n")
    ;;                                    (&/fold str "")))
    ;;           (assert false))]
    ;;   (matchv ::M/objects [(fp-get fp-pair fixpoints)]
    ;;     [["lux;Some" ?]]
    ;;     (if ?
    ;;       (return (&/T fixpoints nil))
    ;;       (fail (check-error expected actual)))

    ;;     [["lux;None" _]]
    ;;     (|do [actual* (apply-type F A)]
    ;;       (check* (fp-put fp-pair true fixpoints) expected actual*))))

    [["lux;AllT" _] _]
    (with-var
      (fn [$arg]
        (|do [expected* (apply-type expected $arg)]
          (check* fixpoints expected* actual))))

    [_ ["lux;AllT" _]]
    (with-var
      (fn [$arg]
        (|do [actual* (apply-type actual $arg)]
          (check* fixpoints expected actual*))))

    [["lux;DataT" e!name] ["lux;DataT" a!name]]
    (if (or (= e!name a!name)
            (.isAssignableFrom (Class/forName e!name) (Class/forName a!name)))
      (return (&/T fixpoints nil))
      (fail (str "[Type Error] Names don't match: " e!name " & " a!name)))

    [["lux;LambdaT" [eI eO]] ["lux;LambdaT" [aI aO]]]
    (|do [[fixpoints* _] (check* fixpoints aI eI)]
      (check* fixpoints* eO aO))

    [["lux;TupleT" e!members] ["lux;TupleT" a!members]]
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
      (fail "[Type Error] Tuples don't match in size."))
    
    [["lux;VariantT" e!cases] ["lux;VariantT" a!cases]]
    (if (= (&/|length e!cases) (&/|length a!cases))
      (|do [fixpoints* (&/fold% (fn [fixp slot]
                                  ;; (prn 'VARIANT_CASE slot)
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
                                  ;; (prn 'RECORD_FIELD slot)
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

    [["lux;ExT" e!id] ["lux;ExT" a!id]]
    (if (= e!id a!id)
      (return (&/T fixpoints nil))
      (check-error expected actual))

    [_ _]
    (fail (println-str "[Type Error] Can't type-check: " (show-type expected) (show-type actual)))
    ))

(defn check [expected actual]
  ;; (prn "^^ check ^^")
  (|do [_ (check* init-fixpoints expected actual)]
    (return nil)))

(defn apply-lambda [func param]
  (matchv ::M/objects [func]
    [["lux;LambdaT" [input output]]]
    (|do [_ (check* init-fixpoints input param)]
      (return output))

    [["lux;AllT" _]]
    (with-var
      (fn [$var]
        (|do [func* (apply-type func $var)
              =return (apply-lambda func* param)]
          (clean $var =return))))

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

(defn variant-case [case type]
  (matchv ::M/objects [type]
    [["lux;VariantT" ?cases]]
    (if-let [case-type (&/|get case ?cases)]
      (return case-type)
      (fail (str "[Type Error] Variant lacks case: " case)))

    [_]
    (fail (str "[Type Error] Type is not a variant: " (show-type type)))))
