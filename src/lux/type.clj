;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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

(def IO
  (&/V "lux;AllT" (&/T (&/V "lux;Some" (&/V "lux;Nil" nil)) "IO" "a"
                       (&/V "lux;LambdaT" (&/T Unit (&/V "lux;BoundT" "a"))))))

(def List
  (&/V "lux;AllT" (&/T (&/V "lux;Some" (&/V "lux;Nil" nil)) "lux;List" "a"
                       (&/V "lux;VariantT" (&/|list (&/T "lux;Nil" Unit)
                                                    (&/T "lux;Cons" (&/V "lux;TupleT" (&/|list (&/V "lux;BoundT" "a")
                                                                                               (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "lux;List")
                                                                                                                    (&/V "lux;BoundT" "a")))))))))))

(def Maybe
  (&/V "lux;AllT" (&/T (&/V "lux;Some" (&/V "lux;Nil" nil)) "lux;Maybe" "a"
                       (&/V "lux;VariantT" (&/|list (&/T "lux;None" Unit)
                                                    (&/T "lux;Some" (&/V "lux;BoundT" "a")))))))

(def Type
  (let [Type (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "Type") (&/V "lux;BoundT" "_")))
        TypeEnv (&/V "lux;AppT" (&/T List (&/V "lux;TupleT" (&/|list Text Type))))
        TypePair (&/V "lux;TupleT" (&/|list Type Type))]
    (&/V "lux;AppT" (&/T (&/V "lux;AllT" (&/T (&/V "lux;Some" (&/V "lux;Nil" nil)) "Type" "_"
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
  (fAll "lux;Bindings" "k"
        (fAll "" "v"
              (&/V "lux;RecordT" (&/|list (&/T "lux;counter" Int)
                                          (&/T "lux;mappings" (&/V "lux;AppT" (&/T List
                                                                                   (&/V "lux;TupleT" (&/|list (&/V "lux;BoundT" "k")
                                                                                                              (&/V "lux;BoundT" "v")))))))))))

(def Env
  (let [bindings (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T Bindings (&/V "lux;BoundT" "k")))
                                      (&/V "lux;BoundT" "v")))]
    (fAll "lux;Env" "k"
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
  (fAll "lux;Meta" "m"
        (fAll "" "v"
              (&/V "lux;VariantT" (&/|list (&/T "lux;Meta" (&/V "lux;TupleT" (&/|list (&/V "lux;BoundT" "m")
                                                                                      (&/V "lux;BoundT" "v")))))))))

(def Ident (&/V "lux;TupleT" (&/|list Text Text)))

(def Syntax*
  (let [Syntax* (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "w")
                                     (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "lux;Syntax'")
                                                          (&/V "lux;BoundT" "w")))))
        Syntax*List (&/V "lux;AppT" (&/T List Syntax*))]
    (fAll "lux;Syntax'" "w"
          (&/V "lux;VariantT" (&/|list (&/T "lux;BoolS" Bool)
                                       (&/T "lux;IntS" Int)
                                       (&/T "lux;RealS" Real)
                                       (&/T "lux;CharS" Char)
                                       (&/T "lux;TextS" Text)
                                       (&/T "lux;SymbolS" Ident)
                                       (&/T "lux;TagS" Ident)
                                       (&/T "lux;FormS" Syntax*List)
                                       (&/T "lux;TupleS" Syntax*List)
                                       (&/T "lux;RecordS" (&/V "lux;AppT" (&/T List (&/V "lux;TupleT" (&/|list Syntax* Syntax*))))))
               ))))

(def Syntax
  (let [w (&/V "lux;AppT" (&/T Meta Cursor))]
    (&/V "lux;AppT" (&/T w (&/V "lux;AppT" (&/T Syntax* w))))))

(def ^:private SyntaxList (&/V "lux;AppT" (&/T List Syntax)))

(def Either
  (fAll "lux;Either" "l"
        (fAll "" "r"
              (&/V "lux;VariantT" (&/|list (&/T "lux;Left" (&/V "lux;BoundT" "l"))
                                           (&/T "lux;Right" (&/V "lux;BoundT" "r")))))))

(def StateE
  (fAll "lux;StateE" "s"
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
                (&/T "lux;classes" (&/V "lux;DataT" "clojure.lang.Atom")))))

(def DefData*
  (fAll "lux;DefData'" ""
        (&/V "lux;VariantT" (&/|list (&/T "lux;TypeD" Unit)
                                     (&/T "lux;ValueD" Type)
                                     (&/T "lux;MacroD" (&/V "lux;BoundT" ""))
                                     (&/T "lux;AliasD" Ident)))))

(def LuxVar
  (&/V "lux;VariantT" (&/|list (&/T "lux;Local" Int)
                               (&/T "lux;Global" Ident))))

(def $Module
  (fAll "lux;$Module" "Compiler"
        (&/V "lux;RecordT"
             (&/|list (&/T "lux;module-aliases" (&/V "lux;AppT" (&/T List (&/V "lux;TupleT" (&/|list Text Text)))))
                      (&/T "lux;defs" (&/V "lux;AppT" (&/T List (&/V "lux;TupleT"
                                                                     (&/|list Text
                                                                              (&/V "lux;TupleT" (&/|list Bool
                                                                                                         (&/V "lux;AppT" (&/T DefData*
                                                                                                                              (&/V "lux;LambdaT" (&/T SyntaxList
                                                                                                                                                      (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T StateE (&/V "lux;BoundT" "Compiler")))
                                                                                                                                                                           SyntaxList)))))))))))))
                      (&/T "lux;imports" (&/V "lux;AppT" (&/T List Text)))))))

(def $Compiler
  (&/V "lux;AppT" (&/T (fAll "lux;Compiler" ""
                             (&/V "lux;RecordT"
                                  (&/|list (&/T "lux;source" Reader)
                                           (&/T "lux;modules" (&/V "lux;AppT" (&/T List (&/V "lux;TupleT"
                                                                                             (&/|list Text
                                                                                                      (&/V "lux;AppT" (&/T $Module (&/V "lux;AppT" (&/T (&/V "lux;BoundT" "lux;Compiler") (&/V "lux;BoundT" ""))))))))))
                                           (&/T "lux;envs" (&/V "lux;AppT" (&/T List
                                                                                (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T Env Text))
                                                                                                     (&/V "lux;TupleT" (&/|list LuxVar Type)))))))
                                           (&/T "lux;types" (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T Bindings Int)) Type)))
                                           (&/T "lux;host" HostState)
                                           (&/T "lux;seed" Int)
                                           (&/T "lux;eval?" Bool))))
                       $Void)))

(def Macro
  (&/V "lux;LambdaT" (&/T SyntaxList
                          (&/V "lux;AppT" (&/T (&/V "lux;AppT" (&/T StateE $Compiler))
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
    (if-let [type* (->> state (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS) (&/|get id))]
      (matchv ::M/objects [type*]
        [["lux;Some" type]]
        (return* state type)
        
        [["lux;None" _]]
        (fail* (str "[Type Error] Unbound type-var: " id)))
      (fail* (str "[Type Error] <deref> Unknown type-var: " id)))))

(defn set-var [id type]
  (fn [state]
    (if-let [tvar (->> state (&/get$ &/$TYPES) (&/get$ &/$MAPPINGS) (&/|get id))]
      (matchv ::M/objects [tvar]
        [["lux;Some" bound]]
        (fail* (str "[Type Error] Can't rebind type var: " id " | Current type: " (show-type bound)))
        
        [["lux;None" _]]
        (return* (&/update$ &/$TYPES (fn [ts] (&/update$ &/$MAPPINGS #(&/|put id (&/V "lux;Some" type) %)
                                                        ts))
                            state)
                 nil))
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
      ((|do [mappings* (&/map% (fn [binding]
                                 (|let [[?id ?type] binding]
                                   (if (.equals ^Object id ?id)
                                     (return binding)
                                     (matchv ::M/objects [?type]
                                       [["lux;None" _]]
                                       (return binding)

                                       [["lux;Some" ?type*]]
                                       (matchv ::M/objects [?type*]
                                         [["lux;VarT" ?id*]]
                                         (if (.equals ^Object id ?id*)
                                           (return (&/T ?id (&/V "lux;None" nil)))
                                           (return binding))

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

(defn with-vars [amount k]
  (|do [=vars (&/map% (constantly create-var) (&/|range amount))
        output (k (&/|map #(&/V "lux;VarT" %) =vars))
        _ (&/map% delete-var (&/|reverse =vars))]
    (return output)))

(defn clean* [?tid type]
  (matchv ::M/objects [type]
    [["lux;VarT" ?id]]
    (if (.equals ^Object ?tid ?id)
      (deref ?id)
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
  (matchv ::M/objects [tvar]
    [["lux;VarT" ?id]]
    (clean* ?id type)
    
    [_]
    (fail (str "[Type Error] Not type-var: " (show-type tvar)))))

(defn ^:private unravel-fun [type]
  (matchv ::M/objects [type]
    [["lux;LambdaT" [?in ?out]]]
    (|let [[??out ?args] (unravel-fun ?out)]
      (&/T ??out (&/|cons ?in ?args)))

    [_]
    (&/T type (&/|list))))

(defn ^:private unravel-app [fun-type]
  (matchv ::M/objects [fun-type]
    [["lux;AppT" [?left ?right]]]
    (|let [[?fun-type ?args] (unravel-app ?left)]
      (&/T ?fun-type (&/|++ ?args (&/|list ?right))))

    [_]
    (&/T fun-type (&/|list))))

(defn show-type [^objects type]
  (matchv ::M/objects [type]
    [["lux;DataT" name]]
    (str "(^ " name ")")
    
    [["lux;TupleT" elems]]
    (if (&/|empty? elems)
      "(,)"
      (str "(, " (->> elems (&/|map show-type) (&/|interpose " ") (&/fold str "")) ")"))

    [["lux;VariantT" cases]]
    (if (&/|empty? cases)
      "(|)"
      (str "(| " (->> cases
                      (&/|map (fn [kv]
                                (matchv ::M/objects [kv]
                                  [[k ["lux;TupleT" ["lux;Nil" _]]]]
                                  (str "#" k)

                                  [[k v]]
                                  (str "(#" k " " (show-type v) ")"))))
                      (&/|interpose " ")
                      (&/fold str "")) ")"))
    

    [["lux;RecordT" fields]]
    (str "(& " (->> fields
                    (&/|map (fn [kv]
                              (matchv ::M/objects [kv]
                                [[k v]]
                                (str "#" k " " (show-type v)))))
                    (&/|interpose " ")
                    (&/fold str "")) ")")

    [["lux;LambdaT" [input output]]]
    (|let [[?out ?ins] (unravel-fun type)]
      (str "(-> " (->> ?ins (&/|map show-type) (&/|interpose " ") (&/fold str "")) " " (show-type ?out) ")"))

    [["lux;VarT" id]]
    (str "⌈" id "⌋")

    [["lux;ExT" ?id]]
    (str "⟨" ?id "⟩")

    [["lux;BoundT" name]]
    name

    [["lux;AppT" [_ _]]]
    (|let [[?call-fun ?call-args] (unravel-app type)]
      (str "(" (show-type ?call-fun) " " (->> ?call-args (&/|map show-type) (&/|interpose " ") (&/fold str "")) ")"))
    
    [["lux;AllT" [?env ?name ?arg ?body]]]
    (if (= "" ?name)
      (let [[args body] (loop [args (list ?arg)
                               body* ?body]
                          (matchv ::M/objects [body*]
                            [["lux;AllT" [?env* ?name* ?arg* ?body*]]]
                            (recur (cons ?arg* args) ?body*)

                            [_]
                            [args body*]))]
        (str "(All " ?name " [" (->> args reverse (interpose " ") (reduce str "")) "] " (show-type body) ")"))
      ?name)
    ))

(defn type= [x y]
  (or (clojure.lang.Util/identical x y)
      (let [output (matchv ::M/objects [x y]
                     [["lux;DataT" xname] ["lux;DataT" yname]]
                     (.equals ^Object xname yname)

                     [["lux;TupleT" xelems] ["lux;TupleT" yelems]]
                     (&/fold2 (fn [old x y]
                                (and old (type= x y)))
                              true
                              xelems yelems)

                     [["lux;VariantT" xcases] ["lux;VariantT" ycases]]
                     (&/fold2 (fn [old xcase ycase]
                                (|let [[xname xtype] xcase
                                       [yname ytype] ycase]
                                  (and old (.equals ^Object xname yname) (type= xtype ytype))))
                              true
                              xcases ycases)

                     [["lux;RecordT" xslots] ["lux;RecordT" yslots]]
                     (&/fold2 (fn [old xslot yslot]
                                (|let [[xname xtype] xslot
                                       [yname ytype] yslot]
                                  (and old (.equals ^Object xname yname) (type= xtype ytype))))
                              true
                              xslots yslots)

                     [["lux;LambdaT" [xinput xoutput]] ["lux;LambdaT" [yinput youtput]]]
                     (and (type= xinput yinput)
                          (type= xoutput youtput))

                     [["lux;VarT" xid] ["lux;VarT" yid]]
                     (.equals ^Object xid yid)

                     [["lux;BoundT" xname] ["lux;BoundT" yname]]
                     (.equals ^Object xname yname)

                     [["lux;ExT" xid] ["lux;ExT" yid]]
                     (.equals ^Object xid yid)

                     [["lux;AppT" [xlambda xparam]] ["lux;AppT" [ylambda yparam]]]
                     (and (type= xlambda ylambda) (type= xparam yparam))
                     
                     [["lux;AllT" [xenv xname xarg xbody]] ["lux;AllT" [yenv yname yarg ybody]]]
                     (and (.equals ^Object xname yname)
                          (.equals ^Object xarg yarg)
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
                          )

                     [_ _]
                     false
                     )]
        output)))

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
  (str "[Type Checker]\nExpected: " (show-type expected)
       "\n\nActual: " (show-type actual)
       "\n"))

(defn beta-reduce [env type]
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
  (matchv ::M/objects [type-fn]
    [["lux;AllT" [local-env local-name local-arg local-def]]]
    (let [local-env* (matchv ::M/objects [local-env]
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
    (fail (str "[Type System] Not type function:\n" (show-type type-fn) "\n"))))

(defn as-obj [class]
  (case class
    "boolean" "java.lang.Boolean"
    "byte"    "java.lang.Byte"
    "short"   "java.lang.Short"
    "int"     "java.lang.Integer"
    "long"    "java.lang.Long"
    "float"   "java.lang.Float"
    "double"  "java.lang.Double"
    "char"    "java.lang.Character"
    ;; else
    class))

(def ^:private primitive-types #{"boolean" "byte" "short" "int" "long" "float" "double" "char"})

(def ^:private init-fixpoints (&/|list))

(defn ^:private check* [class-loader fixpoints expected actual]
  (if (clojure.lang.Util/identical expected actual)
    (return (&/T fixpoints nil))
    (matchv ::M/objects [expected actual]
      [["lux;VarT" ?eid] ["lux;VarT" ?aid]]
      (if (.equals ^Object ?eid ?aid)
        (return (&/T fixpoints nil))
        (|do [ebound (fn [state]
                       (matchv ::M/objects [((deref ?eid) state)]
                         [["lux;Right" [state* ebound]]]
                         (return* state* (&/V "lux;Some" ebound))

                         [["lux;Left" _]]
                         (return* state (&/V "lux;None" nil))))
              abound (fn [state]
                       (matchv ::M/objects [((deref ?aid) state)]
                         [["lux;Right" [state* abound]]]
                         (return* state* (&/V "lux;Some" abound))

                         [["lux;Left" _]]
                         (return* state (&/V "lux;None" nil))))]
          (matchv ::M/objects [ebound abound]
            [["lux;None" _] ["lux;None" _]]
            (|do [_ (set-var ?eid actual)]
              (return (&/T fixpoints nil)))
            
            [["lux;Some" etype] ["lux;None" _]]
            (check* class-loader fixpoints etype actual)

            [["lux;None" _] ["lux;Some" atype]]
            (check* class-loader fixpoints expected atype)

            [["lux;Some" etype] ["lux;Some" atype]]
            (check* class-loader fixpoints etype atype))))
      
      [["lux;VarT" ?id] _]
      (fn [state]
        (matchv ::M/objects [((set-var ?id actual) state)]
          [["lux;Right" [state* _]]]
          (return* state* (&/T fixpoints nil))

          [["lux;Left" _]]
          ((|do [bound (deref ?id)]
             (check* class-loader fixpoints bound actual))
           state)))
      
      [_ ["lux;VarT" ?id]]
      (fn [state]
        (matchv ::M/objects [((set-var ?id expected) state)]
          [["lux;Right" [state* _]]]
          (return* state* (&/T fixpoints nil))

          [["lux;Left" _]]
          ((|do [bound (deref ?id)]
             (check* class-loader fixpoints expected bound))
           state)))

      [["lux;AppT" [["lux;VarT" ?eid] A1]] ["lux;AppT" [["lux;VarT" ?aid] A2]]]
      (fn [state]
        (matchv ::M/objects [((|do [F1 (deref ?eid)]
                                (fn [state]
                                  (matchv ::M/objects [((|do [F2 (deref ?aid)]
                                                          (check* class-loader fixpoints (&/V "lux;AppT" (&/T F1 A1)) (&/V "lux;AppT" (&/T F2 A2))))
                                                        state)]
                                    [["lux;Right" [state* output]]]
                                    (return* state* output)

                                    [["lux;Left" _]]
                                    ((check* class-loader fixpoints (&/V "lux;AppT" (&/T F1 A1)) actual)
                                     state))))
                              state)]
          [["lux;Right" [state* output]]]
          (return* state* output)

          [["lux;Left" _]]
          (matchv ::M/objects [((|do [F2 (deref ?aid)]
                                  (check* class-loader fixpoints expected (&/V "lux;AppT" (&/T F2 A2))))
                                state)]
            [["lux;Right" [state* output]]]
            (return* state* output)

            [["lux;Left" _]]
            ((|do [[fixpoints* _] (check* class-loader fixpoints (&/V "lux;VarT" ?eid) (&/V "lux;VarT" ?aid))
                   [fixpoints** _] (check* class-loader fixpoints* A1 A2)]
               (return (&/T fixpoints** nil)))
             state))))
      ;; (|do [_ (check* class-loader fixpoints (&/V "lux;VarT" ?eid) (&/V "lux;VarT" ?aid))
      ;;       _ (check* class-loader fixpoints A1 A2)]
      ;;   (return (&/T fixpoints nil)))
      
      [["lux;AppT" [["lux;VarT" ?id] A1]] ["lux;AppT" [F2 A2]]]
      (fn [state]
        (matchv ::M/objects [((|do [F1 (deref ?id)]
                                (check* class-loader fixpoints (&/V "lux;AppT" (&/T F1 A1)) actual))
                              state)]
          [["lux;Right" [state* output]]]
          (return* state* output)

          [["lux;Left" _]]
          ((|do [[fixpoints* _] (check* class-loader fixpoints (&/V "lux;VarT" ?id) F2)
                 e* (apply-type F2 A1)
                 a* (apply-type F2 A2)
                 [fixpoints** _] (check* class-loader fixpoints* e* a*)]
             (return (&/T fixpoints** nil)))
           state)))
      ;; [["lux;AppT" [["lux;VarT" ?id] A1]] ["lux;AppT" [F2 A2]]]
      ;; (|do [[fixpoints* _] (check* class-loader fixpoints (&/V "lux;VarT" ?id) F2)
      ;;       e* (apply-type F2 A1)
      ;;       a* (apply-type F2 A2)
      ;;       [fixpoints** _] (check* class-loader fixpoints* e* a*)]
      ;;   (return (&/T fixpoints** nil)))
      
      [["lux;AppT" [F1 A1]] ["lux;AppT" [["lux;VarT" ?id] A2]]]
      (fn [state]
        (matchv ::M/objects [((|do [F2 (deref ?id)]
                                (check* class-loader fixpoints expected (&/V "lux;AppT" (&/T F2 A2))))
                              state)]
          [["lux;Right" [state* output]]]
          (return* state* output)

          [["lux;Left" _]]
          ((|do [[fixpoints* _] (check* class-loader fixpoints F1 (&/V "lux;VarT" ?id))
                 e* (apply-type F1 A1)
                 a* (apply-type F1 A2)
                 [fixpoints** _] (check* class-loader fixpoints* e* a*)]
             (return (&/T fixpoints** nil)))
           state)))
      ;; [["lux;AppT" [F1 A1]] ["lux;AppT" [["lux;VarT" ?id] A2]]]
      ;; (|do [[fixpoints* _] (check* class-loader fixpoints F1 (&/V "lux;VarT" ?id))
      ;;       e* (apply-type F1 A1)
      ;;       a* (apply-type F1 A2)
      ;;       [fixpoints** _] (check* class-loader fixpoints* e* a*)]
      ;;   (return (&/T fixpoints** nil)))

      [["lux;AppT" [F A]] _]
      (let [fp-pair (&/T expected actual)
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
            (check* class-loader (fp-put fp-pair true fixpoints) expected* actual))))

      [_ ["lux;AppT" [F A]]]
      (|do [actual* (apply-type F A)]
        (check* class-loader fixpoints expected actual*))

      [["lux;AllT" _] _]
      (with-var
        (fn [$arg]
          (|do [expected* (apply-type expected $arg)]
            (check* class-loader fixpoints expected* actual))))

      [_ ["lux;AllT" _]]
      (with-var
        (fn [$arg]
          (|do [actual* (apply-type actual $arg)]
            (check* class-loader fixpoints expected actual*))))

      [["lux;DataT" e!name] ["lux;DataT" "null"]]
      (if (contains? primitive-types e!name)
        (fail (str "[Type Error] Can't use \"null\" with primitive types."))
        (return (&/T fixpoints nil)))

      [["lux;DataT" e!name] ["lux;DataT" a!name]]
      (let [e!name (as-obj e!name)
            a!name (as-obj a!name)]
        (if (or (.equals ^Object e!name a!name)
                (.isAssignableFrom (Class/forName e!name true class-loader) (Class/forName a!name true class-loader)))
          (return (&/T fixpoints nil))
          (fail (str "[Type Error] Names don't match: " e!name " =/= " a!name))))

      [["lux;LambdaT" [eI eO]] ["lux;LambdaT" [aI aO]]]
      (|do [[fixpoints* _] (check* class-loader fixpoints aI eI)]
        (check* class-loader fixpoints* eO aO))

      [["lux;TupleT" e!members] ["lux;TupleT" a!members]]
      (|do [fixpoints* (&/fold2% (fn [fp e a]
                                   (|do [[fp* _] (check* class-loader fp e a)]
                                     (return fp*)))
                                 fixpoints
                                 e!members a!members)]
        (return (&/T fixpoints* nil)))
      
      [["lux;VariantT" e!cases] ["lux;VariantT" a!cases]]
      (|do [fixpoints* (&/fold2% (fn [fp e!case a!case]
                                   (|let [[e!name e!type] e!case
                                          [a!name a!type] a!case]
                                     (if (.equals ^Object e!name a!name)
                                       (|do [[fp* _] (check* class-loader fp e!type a!type)]
                                         (return fp*))
                                       (fail (check-error expected actual)))))
                                 fixpoints
                                 e!cases a!cases)]
        (return (&/T fixpoints* nil)))

      [["lux;RecordT" e!slots] ["lux;RecordT" a!slots]]
      (|do [fixpoints* (&/fold2% (fn [fp e!slot a!slot]
                                   (|let [[e!name e!type] e!slot
                                          [a!name a!type] a!slot]
                                     (if (.equals ^Object e!name a!name)
                                       (|do [[fp* _] (check* class-loader fp e!type a!type)]
                                         (return fp*))
                                       (fail (check-error expected actual)))))
                                 fixpoints
                                 e!slots a!slots)]
        (return (&/T fixpoints* nil)))

      [["lux;ExT" e!id] ["lux;ExT" a!id]]
      (if (.equals ^Object e!id a!id)
        (return (&/T fixpoints nil))
        (fail (check-error expected actual)))

      [_ _]
      (fail (check-error expected actual))
      )))

(defn check [expected actual]
  (|do [class-loader &/loader
        _ (check* class-loader init-fixpoints expected actual)]
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
    (fail (str "[Type System] Not a function type:\n" (show-type func) "\n"))
    ))

(defn actual-type [type]
  (matchv ::M/objects [type]
    [["lux;AppT" [?all ?param]]]
    (|do [type* (apply-type ?all ?param)]
      (actual-type type*))

    [["lux;VarT" ?id]]
    (deref ?id)
    
    [_]
    (return type)
    ))

(defn variant-case [case type]
  (matchv ::M/objects [type]
    [["lux;VariantT" ?cases]]
    (if-let [case-type (&/|get case ?cases)]
      (return case-type)
      (fail (str "[Type Error] Variant lacks case: " case " | " (show-type type))))

    [_]
    (fail (str "[Type Error] Type is not a variant: " (show-type type)))))
