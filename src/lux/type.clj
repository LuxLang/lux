;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.type
  (:refer-clojure :exclude [deref apply merge bound?])
  (:require clojure.core.match
            clojure.core.match.array
            [lux.base :as & :refer [|do return* return fail fail* assert! |let |case $$]]))

(declare show-type)

;; [Utils]
(defn |list? [xs]
  (|case xs
    (&/$Nil)
    true

    (&/$Cons x xs*)
    (|list? xs*)

    _
    false))

(def ^:private empty-env (&/Some$ &/Nil$))
(def ^:private no-env &/None$)
(def Ident$ &/P)
(defn Data$ [name]
  (&/S &/$DataT name))
(defn Bound$ [name]
  (&/S &/$BoundT name))
(defn Var$ [id]
  (&/S &/$VarT id))
(defn Lambda$ [in out]
  (&/S &/$LambdaT (&/P in out)))
(defn App$ [fun arg]
  (&/S &/$AppT (&/P fun arg)))
(defn Prod$ [left right]
  ;; (assert (|list? members))
  (&/S &/$ProdT (&/P left right)))
(defn Sum$ [left right]
  ;; (assert (|list? members))
  (&/S &/$SumT (&/P left right)))
(defn All$ [env name arg body]
  (&/S &/$AllT ($$ &/P env name arg body)))
(defn Named$ [name type]
  (&/S &/$NamedT (&/P name type)))

(def Bool (Named$ (Ident$ &/prelude-name "Bool") (Data$ "java.lang.Boolean")))
(def Int (Named$ (Ident$ &/prelude-name "Int") (Data$ "java.lang.Long")))
(def Real (Named$ (Ident$ &/prelude-name "Real") (Data$ "java.lang.Double")))
(def Char (Named$ (Ident$ &/prelude-name "Char") (Data$ "java.lang.Character")))
(def Text (Named$ (Ident$ &/prelude-name "Text") (Data$ "java.lang.String")))
(def Unit (Named$ (Ident$ &/prelude-name "Unit") (&/S &/$UnitT nil)))
(def $Void (Named$ (Ident$ &/prelude-name "Void") (&/S &/$VoidT nil)))
(def Ident (Named$ (Ident$ &/prelude-name "Ident") (Prod$ Text Text)))

(def IO
  (Named$ (Ident$ "lux/data" "IO")
          (All$ empty-env "IO" "a"
                (Lambda$ Unit (Bound$ "a")))))

(def List
  (Named$ (Ident$ &/prelude-name "List")
          (All$ empty-env "lux;List" "a"
                (Sum$
                 ;; lux;Nil
                 Unit
                 ;; lux;Cons
                 (Prod$ (Bound$ "a")
                        (App$ (Bound$ "lux;List")
                              (Bound$ "a")))
                 ))))

(def Maybe
  (Named$ (Ident$ &/prelude-name "Maybe")
          (All$ empty-env "lux;Maybe" "a"
                (Sum$
                 ;; lux;None
                 Unit
                 ;; lux;Some
                 (Bound$ "a")
                 ))))

(def Type
  (Named$ (Ident$ &/prelude-name "Type")
          (let [Type (App$ (Bound$ "Type") (Bound$ "_"))
                TypeList (App$ List Type)
                TypeEnv (App$ List (Prod$ Text Type))
                TypePair (Prod$ Type Type)]
            (App$ (All$ empty-env "Type" "_"
                        ($$ Sum$
                            ;; VoidT
                            Unit
                            ;; UnitT
                            Unit
                            ;; SumT
                            TypePair
                            ;; ProdT
                            TypePair
                            ;; DataT
                            Text
                            ;; LambdaT
                            TypePair
                            ;; BoundT
                            Text
                            ;; VarT
                            Int
                            ;; ExT
                            Int
                            ;; AllT
                            ($$ Prod$ (App$ Maybe TypeEnv) Text Text Type)
                            ;; AppT
                            TypePair
                            ;; NamedT
                            (Prod$ Ident Type)
                            ))
                  $Void))))

(def Bindings
  (Named$ (Ident$ &/prelude-name "Bindings")
          (All$ empty-env "lux;Bindings" "k"
                (All$ no-env "" "v"
                      (Prod$
                       ;; "lux;counter"
                       Int
                       ;; "lux;mappings"
                       (App$ List
                             (Prod$ (Bound$ "k")
                                    (Bound$ "v"))))))))

(def Env
  (Named$ (Ident$ &/prelude-name "Env")
          (let [bindings (App$ (App$ Bindings (Bound$ "k"))
                               (Bound$ "v"))]
            (All$ empty-env "lux;Env" "k"
                  (All$ no-env "" "v"
                        ($$ Prod$
                            ;; "lux;name"
                            Text
                            ;; "lux;inner-closures"
                            Int
                            ;; "lux;locals"
                            bindings
                            ;; "lux;closure"
                            bindings
                            ))))))

(def Cursor
  (Named$ (Ident$ &/prelude-name "Cursor")
          ($$ Prod$ Text Int Int)))

(def Meta
  (Named$ (Ident$ &/prelude-name "Meta")
          (All$ empty-env "lux;Meta" "m"
                (All$ no-env "" "v"
                      (Prod$ (Bound$ "m")
                             (Bound$ "v"))))))

(def AST*
  (Named$ (Ident$ &/prelude-name "AST'")
          (let [AST* (App$ (Bound$ "w")
                           (App$ (Bound$ "lux;AST'")
                                 (Bound$ "w")))
                AST*List (App$ List AST*)]
            (All$ empty-env "lux;AST'" "w"
                  ($$ Sum$
                      ;; &/$BoolS
                      Bool
                      ;; &/$IntS
                      Int
                      ;; &/$RealS
                      Real
                      ;; &/$CharS
                      Char
                      ;; &/$TextS
                      Text
                      ;; &/$SymbolS
                      Ident
                      ;; &/$TagS
                      Ident
                      ;; &/$FormS
                      AST*List
                      ;; &/$TupleS
                      AST*List
                      ;; &/$RecordS
                      (App$ List (Prod$ AST* AST*))
                      )))))

(def AST
  (Named$ (Ident$ &/prelude-name "AST")
          (let [w (App$ Meta Cursor)]
            (App$ w (App$ AST* w)))))

(def ^:private ASTList (App$ List AST))

(def Either
  (Named$ (Ident$ &/prelude-name "Either")
          (All$ empty-env "lux;Either" "l"
                (All$ no-env "" "r"
                      (Sum$
                       ;; &/$Left
                       (Bound$ "l")
                       ;; &/$Right
                       (Bound$ "r"))))))

(def StateE
  (All$ empty-env "lux;StateE" "s"
        (All$ no-env "" "a"
              (Lambda$ (Bound$ "s")
                       (App$ (App$ Either Text)
                             (Prod$ (Bound$ "s")
                                    (Bound$ "a")))))))

(def Source
  (Named$ (Ident$ &/prelude-name "Source")
          (App$ List
                (App$ (App$ Meta Cursor)
                      Text))))

(def Host
  (Named$ (Ident$ &/prelude-name "Host")
          ($$ Prod$
              ;; "lux;writer"
              (Data$ "org.objectweb.asm.ClassWriter")
              ;; "lux;loader"
              (Data$ "java.lang.ClassLoader")
              ;; "lux;classes"
              (Data$ "clojure.lang.Atom"))))

(def DefData*
  (All$ empty-env "lux;DefData'" ""
        ($$ Sum$
            ;; "lux;ValueD"
            (Prod$ Type Unit)
            ;; "lux;TypeD"
            Type
            ;; "lux;MacroD"
            (Bound$ "")
            ;; "lux;AliasD"
            Ident
            )))

(def LuxVar
  (Named$ (Ident$ &/prelude-name "LuxVar")
          (Sum$
           ;; "lux;Local"
           Int
           ;; "lux;Global"
           Ident)))

(def $Module
  (All$ empty-env "lux;$Module" "Compiler"
        ($$ Prod$
            ;; "lux;module-aliases"
            (App$ List (Prod$ Text Text))
            ;; "lux;defs"
            (App$ List
                  (Prod$ Text
                         (Prod$ Bool
                                (App$ DefData*
                                      (Lambda$ ASTList
                                               (App$ (App$ StateE (Bound$ "Compiler"))
                                                     ASTList))))))
            ;; "lux;imports"
            (App$ List Text)
            ;; "lux;tags"
            ;; (List (, Text (, Int (List Ident) Type)))
            (App$ List
                  (Prod$ Text
                         ($$ Prod$ Int
                             (App$ List Ident)
                             Type)))
            ;; "lux;types"
            ;; (List (, Text (, (List Ident) Type)))
            (App$ List
                  (Prod$ Text
                         (Prod$ (App$ List Ident)
                                Type)))
            )))

(def $Compiler
  (Named$ (Ident$ &/prelude-name "Compiler")
          (App$ (All$ empty-env "lux;Compiler" ""
                      ($$ Prod$
                          ;; "lux;source"
                          Source
                          ;; "lux;cursor"
                          Cursor
                          ;; "lux;modules"
                          (App$ List (Prod$ Text
                                            (App$ $Module (App$ (Bound$ "lux;Compiler") (Bound$ "")))))
                          ;; "lux;envs"
                          (App$ List
                                (App$ (App$ Env Text)
                                      (Prod$ LuxVar Type)))
                          ;; "lux;types"
                          (App$ (App$ Bindings Int) Type)
                          ;; "lux;expected"
                          Type
                          ;; "lux;seed"
                          Int
                          ;; "lux;eval?"
                          Bool
                          ;; "lux;host"
                          Host
                          ))
                $Void)))

(def Macro
  (Named$ (Ident$ &/prelude-name "Macro")
          (Lambda$ ASTList
                   (App$ (App$ StateE $Compiler)
                         ASTList))))

(defn bound? [id]
  (fn [state]
    (if-let [type (->> state (&/$get-type-vars) (&/$get-mappings) (&/|get id))]
      (|case type
        (&/$Some type*)
        (return* state true)
        
        (&/$None)
        (return* state false))
      (fail* (str "[Type Error] <bound?> Unknown type-var: " id)))))

(defn deref [id]
  (fn [state]
    (if-let [type* (->> state (&/$get-type-vars) (&/$get-mappings) (&/|get id))]
      (|case type*
        (&/$Some type)
        (return* state type)
        
        (&/$None)
        (fail* (str "[Type Error] Unbound type-var: " id)))
      (fail* (str "[Type Error] <deref> Unknown type-var: " id)))))

(defn set-var [id type]
  (fn [state]
    (if-let [tvar (->> state (&/$get-type-vars) (&/$get-mappings) (&/|get id))]
      (|case tvar
        (&/$Some bound)
        (fail* (str "[Type Error] Can't rebind type var: " id " | Current type: " (show-type bound)))
        
        (&/$None)
        (return* (&/$update-type-vars (fn [ts] (&/$update-mappings #(&/|put id (&/Some$ type) %)
                                                                  ts))
                                      state)
                 nil))
      (fail* (str "[Type Error] <set-var> Unknown type-var: " id " | " (->> state (&/$get-type-vars) (&/$get-mappings) &/|length))))))

;; [Exports]
;; Type vars
(def ^:private create-var
  (fn [state]
    (let [id (->> state &/$get-type-vars &/$get-counter)]
      (return* (&/$update-type-vars #(do ;; (prn 'create-var/_0 (&/adt->text %))
                                       ;; (prn 'create-var/_1 (&/adt->text (->> % (&/$update-counter inc))))
                                       ;; (prn 'create-var/_2 (&/adt->text (->> %
                                       ;;                                       (&/$update-counter inc)
                                       ;;                                       (&/$update-mappings (fn [ms] (&/|put id &/None$ ms))))))
                                       (->> %
                                            (&/$update-counter inc)
                                            (&/$update-mappings (fn [ms] (&/|put id &/None$ ms)))))
                                    state)
               id))))

(def existential
  (|do [seed &/gen-id]
    (return (&/S &/$ExT seed))))

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
                                     (|case ?type
                                       (&/$None)
                                       (return binding)

                                       (&/$Some ?type*)
                                       (|case ?type*
                                         (&/$VarT ?id*)
                                         (if (.equals ^Object id ?id*)
                                           (return (&/P ?id &/None$))
                                           (return binding))

                                         _
                                         (|do [?type** (clean* id ?type*)]
                                           (return (&/P ?id (&/Some$ ?type**)))))
                                       ))))
                               (->> state (&/$get-type-vars) (&/$get-mappings)))]
         (fn [state]
           (return* (&/$update-type-vars #(->> %
                                               (&/$update-counter dec)
                                               (&/$set-mappings (&/|remove id mappings*)))
                                         state)
                    nil)))
       state))))

(defn with-var [k]
  (|do [id create-var
        output (k (Var$ id))
        _ (delete-var id)]
    (return output)))

(defn with-vars [amount k]
  (|do [=vars (&/map% (constantly create-var) (&/|range amount))
        output (k (&/|map #(Var$ %) =vars))
        _ (&/map% delete-var (&/|reverse =vars))]
    (return output)))

(defn clean* [?tid type]
  (|case type
    (&/$VarT ?id)
    (if (.equals ^Object ?tid ?id)
      (deref ?id)
      (return type))
    
    (&/$LambdaT ?arg ?return)
    (|do [=arg (clean* ?tid ?arg)
          =return (clean* ?tid ?return)]
      (return (Lambda$ =arg =return)))

    (&/$AppT ?lambda ?param)
    (|do [=lambda (clean* ?tid ?lambda)
          =param (clean* ?tid ?param)]
      (return (App$ =lambda =param)))

    (&/$SumT ?left ?right)
    (|do [=left (clean* ?tid ?left)
          =right (clean* ?tid ?right)]
      (return (Sum$ =left =right)))

    (&/$ProdT ?left ?right)
    (|do [=left (clean* ?tid ?left)
          =right (clean* ?tid ?right)]
      (return (Prod$ =left =right)))

    (&/$AllT ?env ?name ?arg ?body)
    (|do [=env (|case ?env
                 (&/$None)
                 (return ?env)

                 (&/$Some ?env*)
                 (|do [clean-env (&/map% (fn [[k v]]
                                           (|do [=v (clean* ?tid v)]
                                             (return (&/P k =v))))
                                         ?env*)]
                   (return (&/Some$ clean-env))))
          body* (clean* ?tid ?body)]
      (return (All$ =env ?name ?arg body*)))

    _
    (return type)
    ))

(defn clean [tvar type]
  (|case tvar
    (&/$VarT ?id)
    (clean* ?id type)
    
    _
    (fail (str "[Type Error] Not type-var: " (show-type tvar)))))

(defn ^:private unravel-fun [type]
  (|case type
    (&/$LambdaT ?in ?out)
    (|let [[??out ?args] (unravel-fun ?out)]
      (&/P ??out (&/Cons$ ?in ?args)))

    _
    (&/P type (&/|list))))

(defn ^:private unravel-app [fun-type]
  (|case fun-type
    (&/$AppT ?left ?right)
    (|let [[?fun-type ?args] (unravel-app ?left)]
      (&/P ?fun-type (&/|++ ?args (&/|list ?right))))

    _
    (&/P fun-type (&/|list))))

(defn show-type [^objects type]
  (|case type
    (&/$VoidT)
    "(|)"

    (&/$UnitT)
    "(,)"
    
    (&/$DataT name)
    (str "(^ " name ")")
    
    (&/$ProdT left right)
    (str "(, " (show-type left) " " (show-type right) ")")

    (&/$SumT left right)
    (str "(| " (show-type left) " " (show-type right) ")")
    
    (&/$LambdaT input output)
    (|let [[?out ?ins] (unravel-fun type)]
      (str "(-> " (->> ?ins (&/|map show-type) (&/|interpose " ") (&/fold str "")) " " (show-type ?out) ")"))

    (&/$VarT id)
    (str "⌈" id "⌋")

    (&/$ExT ?id)
    (str "⟨" ?id "⟩")

    (&/$BoundT name)
    name

    (&/$AppT _ _)
    (|let [[?call-fun ?call-args] (unravel-app type)]
      (str "(" (show-type ?call-fun) " " (->> ?call-args (&/|map show-type) (&/|interpose " ") (&/fold str "")) ")"))
    
    (&/$AllT ?env ?name ?arg ?body)
    (if (= "" ?name)
      (let [[args body] (loop [args (list ?arg)
                               body* ?body]
                          (|case body*
                            (&/$AllT ?env* ?name* ?arg* ?body*)
                            (recur (cons ?arg* args) ?body*)

                            _
                            [args body*]))]
        (str "(All " ?name " [" (->> args reverse (interpose " ") (reduce str "")) "] " (show-type body) ")"))
      ?name)

    (&/$NamedT ?name ?type)
    (&/ident->text ?name)

    _
    (assert false (prn-str 'show-type (&/adt->text type)))))

(defn type= [x y]
  (or (clojure.lang.Util/identical x y)
      (let [output (|case [x y]
                     [(&/$UnitT) (&/$UnitT)]
                     true

                     [(&/$VoidT) (&/$VoidT)]
                     true

                     [(&/$DataT xname) (&/$DataT yname)]
                     (.equals ^Object xname yname)

                     [(&/$ProdT xleft xright) (&/$ProdT yleft yright)]
                     (and (type= xleft yleft)
                          (type= xright yright))

                     [(&/$SumT xleft xright) (&/$SumT yleft yright)]
                     (and (type= xleft yleft)
                          (type= xright yright))

                     [(&/$LambdaT xinput xoutput) (&/$LambdaT yinput youtput)]
                     (and (type= xinput yinput)
                          (type= xoutput youtput))

                     [(&/$VarT xid) (&/$VarT yid)]
                     (.equals ^Object xid yid)

                     [(&/$BoundT xname) (&/$BoundT yname)]
                     (.equals ^Object xname yname)

                     [(&/$ExT xid) (&/$ExT yid)]
                     (.equals ^Object xid yid)

                     [(&/$AppT xlambda xparam) (&/$AppT ylambda yparam)]
                     (and (type= xlambda ylambda) (type= xparam yparam))
                     
                     [(&/$AllT xenv xname xarg xbody) (&/$AllT yenv yname yarg ybody)]
                     (and (.equals ^Object xname yname)
                          (.equals ^Object xarg yarg)
                          ;; (matchv ::M/objects [xenv yenv]
                          ;;   [[&/$None _] [&/$None _]]
                          ;;   true

                          ;;   [[&/$Some xenv*] [&/$Some yenv*]]
                          ;;   (&/fold (fn [old bname]
                          ;;             (and old
                          ;;                  (type= (&/|get bname xenv*) (&/|get bname yenv*))))
                          ;;           (= (&/|length xenv*) (&/|length yenv*))
                          ;;           (&/|keys xenv*))

                          ;;   [_ _]
                          ;;   false)
                          (type= xbody ybody)
                          )

                     [(&/$NamedT ?xname ?xtype) _]
                     (type= ?xtype y)

                     [_ (&/$NamedT ?yname ?ytype)]
                     (type= x ?ytype)
                     
                     [_ _]
                     false
                     )]
        output)))

(defn ^:private fp-get [k fixpoints]
  (|let [[e a] k]
    (|case fixpoints
      (&/$Nil)
      &/None$

      (&/$Cons [[e* a*] v*] fixpoints*)
      (if (and (type= e e*)
               (type= a a*))
        (&/Some$ v*)
        (fp-get k fixpoints*))
      )))

(defn ^:private fp-put [k v fixpoints]
  (&/Cons$ (&/P k v) fixpoints))

(defn ^:private check-error [expected actual]
  (str "[Type Checker]\nExpected: " (show-type expected)
       "\n\nActual: " (show-type actual)
       "\n"))

(defn beta-reduce [env type]
  (|case type
    (&/$SumT ?left ?right)
    (Sum$ (beta-reduce env ?left) (beta-reduce env ?right))

    (&/$ProdT ?left ?right)
    (Prod$ (beta-reduce env ?left) (beta-reduce env ?right))

    (&/$AppT ?type-fn ?type-arg)
    (App$ (beta-reduce env ?type-fn) (beta-reduce env ?type-arg))

    (&/$AllT ?local-env ?local-name ?local-arg ?local-def)
    (|case ?local-env
      (&/$None)
      (All$ (&/Some$ env) ?local-name ?local-arg ?local-def)

      (&/$Some _)
      type)

    (&/$LambdaT ?input ?output)
    (Lambda$ (beta-reduce env ?input) (beta-reduce env ?output))

    (&/$BoundT ?name)
    (if-let [bound (&/|get ?name env)]
      (beta-reduce env bound)
      type)

    _
    type
    ))

(defn apply-type [type-fn param]
  (|case type-fn
    (&/$AllT local-env local-name local-arg local-def)
    (let [local-env* (|case local-env
                       (&/$None)
                       (&/|table)

                       (&/$Some local-env*)
                       local-env*)]
      (return (beta-reduce (->> local-env*
                                (&/|put local-name type-fn)
                                (&/|put local-arg param))
                           local-def)))

    (&/$AppT F A)
    (|do [type-fn* (apply-type F A)]
      (apply-type type-fn* param))

    (&/$NamedT ?name ?type)
    (apply-type ?type param)
    
    _
    (fail (str "[Type Error] Not a type function:\n" (show-type type-fn) "\n"))))

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
  ;; (prn 'check*/_0 (&/adt->text expected) (&/adt->text actual))
  ;; (prn 'check*/_1 (show-type expected) (show-type actual))
  (if (clojure.lang.Util/identical expected actual)
    (return (&/P fixpoints nil))
    (|case [expected actual]
      [(&/$UnitT) (&/$UnitT)]
      (return (&/P fixpoints nil))

      [(&/$VarT ?eid) (&/$VarT ?aid)]
      (if (.equals ^Object ?eid ?aid)
        (return (&/P fixpoints nil))
        (|do [ebound (fn [state]
                       (|case ((deref ?eid) state)
                         (&/$Right state* ebound)
                         (return* state* (&/Some$ ebound))

                         (&/$Left _)
                         (return* state &/None$)))
              abound (fn [state]
                       (|case ((deref ?aid) state)
                         (&/$Right state* abound)
                         (return* state* (&/Some$ abound))

                         (&/$Left _)
                         (return* state &/None$)))]
          (|case [ebound abound]
            [(&/$None _) (&/$None _)]
            (|do [_ (set-var ?eid actual)]
              (return (&/P fixpoints nil)))
            
            [(&/$Some etype) (&/$None _)]
            (check* class-loader fixpoints etype actual)

            [(&/$None _) (&/$Some atype)]
            (check* class-loader fixpoints expected atype)

            [(&/$Some etype) (&/$Some atype)]
            (check* class-loader fixpoints etype atype))))
      
      [(&/$VarT ?id) _]
      (fn [state]
        (|case ((set-var ?id actual) state)
          (&/$Right state* _)
          (return* state* (&/P fixpoints nil))

          (&/$Left _)
          ((|do [bound (deref ?id)]
             (check* class-loader fixpoints bound actual))
           state)))
      
      [_ (&/$VarT ?id)]
      (fn [state]
        (|case ((set-var ?id expected) state)
          (&/$Right state* _)
          (return* state* (&/P fixpoints nil))

          (&/$Left _)
          ((|do [bound (deref ?id)]
             (check* class-loader fixpoints expected bound))
           state)))

      [(&/$AppT (&/$VarT ?eid) A1) (&/$AppT (&/$VarT ?aid) A2)]
      (fn [state]
        (|case ((|do [F1 (deref ?eid)]
                  (fn [state]
                    (|case ((|do [F2 (deref ?aid)]
                              (check* class-loader fixpoints (App$ F1 A1) (App$ F2 A2)))
                            state)
                      (&/$Right state* output)
                      (return* state* output)

                      (&/$Left _)
                      ((check* class-loader fixpoints (App$ F1 A1) actual)
                       state))))
                state)
          (&/$Right state* output)
          (return* state* output)

          (&/$Left _)
          (|case ((|do [F2 (deref ?aid)]
                    (check* class-loader fixpoints expected (App$ F2 A2)))
                  state)
            (&/$Right state* output)
            (return* state* output)

            (&/$Left _)
            ((|do [[fixpoints* _] (check* class-loader fixpoints (Var$ ?eid) (Var$ ?aid))
                   [fixpoints** _] (check* class-loader fixpoints* A1 A2)]
               (return (&/P fixpoints** nil)))
             state))))
      ;; (|do [_ (check* class-loader fixpoints (Var$ ?eid) (Var$ ?aid))
      ;;       _ (check* class-loader fixpoints A1 A2)]
      ;;   (return (&/P fixpoints nil)))
      
      [(&/$AppT (&/$VarT ?id) A1) (&/$AppT F2 A2)]
      (fn [state]
        (|case ((|do [F1 (deref ?id)]
                  (check* class-loader fixpoints (App$ F1 A1) actual))
                state)
          (&/$Right state* output)
          (return* state* output)

          (&/$Left _)
          ((|do [[fixpoints* _] (check* class-loader fixpoints (Var$ ?id) F2)
                 e* (apply-type F2 A1)
                 a* (apply-type F2 A2)
                 [fixpoints** _] (check* class-loader fixpoints* e* a*)]
             (return (&/P fixpoints** nil)))
           state)))
      ;; [[&/$AppT [[&/$VarT ?id] A1]] [&/$AppT [F2 A2]]]
      ;; (|do [[fixpoints* _] (check* class-loader fixpoints (Var$ ?id) F2)
      ;;       e* (apply-type F2 A1)
      ;;       a* (apply-type F2 A2)
      ;;       [fixpoints** _] (check* class-loader fixpoints* e* a*)]
      ;;   (return (&/P fixpoints** nil)))
      
      [(&/$AppT F1 A1) (&/$AppT (&/$VarT ?id) A2)]
      (fn [state]
        (|case ((|do [F2 (deref ?id)]
                  (check* class-loader fixpoints expected (App$ F2 A2)))
                state)
          (&/$Right state* output)
          (return* state* output)

          (&/$Left _)
          ((|do [[fixpoints* _] (check* class-loader fixpoints F1 (Var$ ?id))
                 e* (apply-type F1 A1)
                 a* (apply-type F1 A2)
                 [fixpoints** _] (check* class-loader fixpoints* e* a*)]
             (return (&/P fixpoints** nil)))
           state)))
      ;; [[&/$AppT [F1 A1]] [&/$AppT [[&/$VarT ?id] A2]]]
      ;; (|do [[fixpoints* _] (check* class-loader fixpoints F1 (Var$ ?id))
      ;;       e* (apply-type F1 A1)
      ;;       a* (apply-type F1 A2)
      ;;       [fixpoints** _] (check* class-loader fixpoints* e* a*)]
      ;;   (return (&/P fixpoints** nil)))

      [(&/$AppT F A) _]
      (let [fp-pair (&/P expected actual)
            _ (when (> (&/|length fixpoints) 40)
                (println 'FIXPOINTS (->> (&/|keys fixpoints)
                                         (&/|map (fn [pair]
                                                   (|let [[e a] pair]
                                                     (str (show-type e) " :+: "
                                                          (show-type a)))))
                                         (&/|interpose "\n\n")
                                         (&/fold str "")))
                (assert false (prn-str 'check* '[(&/$AppT F A) _] (&/|length fixpoints) (show-type expected) (show-type actual))))]
        (|case (fp-get fp-pair fixpoints)
          (&/$Some ?)
          (if ?
            (return (&/P fixpoints nil))
            (fail (check-error expected actual)))

          (&/$None)
          (|do [expected* (apply-type F A)]
            (check* class-loader (fp-put fp-pair true fixpoints) expected* actual))))

      [_ (&/$AppT F A)]
      (|do [actual* (apply-type F A)]
        (check* class-loader fixpoints expected actual*))

      [(&/$AllT _) _]
      (with-var
        (fn [$arg]
          (|do [expected* (apply-type expected $arg)]
            (check* class-loader fixpoints expected* actual))))

      [_ (&/$AllT _)]
      (with-var
        (fn [$arg]
          (|do [actual* (apply-type actual $arg)]
            (check* class-loader fixpoints expected actual*))))

      [(&/$DataT e!name) (&/$DataT "null")]
      (if (contains? primitive-types e!name)
        (fail (str "[Type Error] Can't use \"null\" with primitive types."))
        (return (&/P fixpoints nil)))

      [(&/$DataT e!name) (&/$DataT a!name)]
      (let [e!name (as-obj e!name)
            a!name (as-obj a!name)]
        (if (or (.equals ^Object e!name a!name)
                (.isAssignableFrom (Class/forName e!name true class-loader) (Class/forName a!name true class-loader)))
          (return (&/P fixpoints nil))
          (fail (str "[Type Error] Names don't match: " e!name " =/= " a!name))))

      [(&/$LambdaT eI eO) (&/$LambdaT aI aO)]
      (|do [[fixpoints* _] (check* class-loader fixpoints aI eI)]
        (check* class-loader fixpoints* eO aO))

      [(&/$ProdT e!left e!right) (&/$ProdT a!left a!right)]
      (|do [[fixpoints* _] (check* class-loader fixpoints e!left a!left)
            [fixpoints** _] (check* class-loader fixpoints* e!right a!right)]
        (return (&/P fixpoints** nil)))
      
      [(&/$SumT e!left e!right) (&/$SumT a!left a!right)]
      (|do [[fixpoints* _] (check* class-loader fixpoints e!left a!left)
            [fixpoints** _] (check* class-loader fixpoints* e!right a!right)]
        (return (&/P fixpoints** nil)))

      [(&/$ExT e!id) (&/$ExT a!id)]
      (if (.equals ^Object e!id a!id)
        (return (&/P fixpoints nil))
        (fail (check-error expected actual)))

      [(&/$NamedT ?ename ?etype) _]
      (check* class-loader fixpoints ?etype actual)

      [_ (&/$NamedT ?aname ?atype)]
      (check* class-loader fixpoints expected ?atype)

      [_ (&/$VoidT)]
      (return (&/P fixpoints nil))

      [_ _]
      (fail (check-error expected actual))
      )))

(defn check [expected actual]
  (|do [class-loader &/loader
        _ (check* class-loader init-fixpoints expected actual)]
    (return nil)))

(defn apply-lambda [func param]
  (|case func
    (&/$LambdaT input output)
    (|do [_ (check* init-fixpoints input param)]
      (return output))

    (&/$AllT _)
    (with-var
      (fn [$var]
        (|do [func* (apply-type func $var)
              =return (apply-lambda func* param)]
          (clean $var =return))))

    (&/$NamedT ?name ?type)
    (apply-lambda ?type param)

    _
    (fail (str "[Type Error] Not a function type:\n" (show-type func) "\n"))
    ))

(defn actual-type [type]
  "(-> Type (Lux Type))"
  (|case type
    (&/$AppT ?all ?param)
    (|do [type* (apply-type ?all ?param)]
      (actual-type type*))

    (&/$VarT ?id)
    (deref ?id)

    (&/$NamedT ?name ?type)
    (actual-type ?type)
    
    _
    (return type)
    ))

(defn variant-case [tag type]
  ;; (prn 'variant-case tag (show-type type))
  (|case type
    (&/$NamedT ?name ?type)
    (variant-case tag ?type)
    
    (&/$SumT ?left ?right)
    (case tag
      0
      (return ?left)

      1
      (|case ?right
        (&/$SumT ?left* _)
        (return ?left*)

        _
        (return ?right))

      ;; else
      (variant-case (dec tag) ?right))

    _
    (fail (str "[Type Error] Type is not a variant: " (show-type type)))
    ;; (assert false (str "[Type Error] Type is not a variant: " (show-type type)))
    ))

(defn type-name [type]
  "(-> Type (Lux Ident))"
  (|case type
    (&/$NamedT name _)
    (return name)
    
    _
    (fail (str "[Type Error] Type is not named: " (show-type type)))
    ))
