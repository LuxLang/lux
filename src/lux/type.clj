;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.type
  (:refer-clojure :exclude [deref apply merge bound?])
  (:require clojure.core.match
            clojure.core.match.array
            [lux.base :as & :refer [|do return* return fail fail* assert! |let |case]]))

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

(def ^:private empty-env (&/V &/$Nil nil))
(defn Data$ [name]
  (&/V &/$DataT name))
(defn Bound$ [name]
  (&/V &/$BoundT name))
(defn Var$ [id]
  (&/V &/$VarT id))
(defn Lambda$ [in out]
  (&/V &/$LambdaT (&/T in out)))
(defn App$ [fun arg]
  (&/V &/$AppT (&/T fun arg)))
(defn Tuple$ [members]
  ;; (assert (|list? members))
  (&/V &/$TupleT members))
(defn Variant$ [members]
  ;; (assert (|list? members))
  (&/V &/$VariantT members))
(defn Univ$ [env body]
  (&/V &/$UnivQ (&/T env body)))
(defn Named$ [name type]
  (&/V &/$NamedT (&/T name type)))


(def Bool (Named$ (&/T "lux" "Bool") (&/V &/$DataT "java.lang.Boolean")))
(def Int (Named$ (&/T "lux" "Int") (&/V &/$DataT "java.lang.Long")))
(def Real (Named$ (&/T "lux" "Real") (&/V &/$DataT "java.lang.Double")))
(def Char (Named$ (&/T "lux" "Char") (&/V &/$DataT "java.lang.Character")))
(def Text (Named$ (&/T "lux" "Text") (&/V &/$DataT "java.lang.String")))
(def Unit (Named$ (&/T "lux" "Unit") (&/V &/$TupleT (&/|list))))
(def $Void (Named$ (&/T "lux" "Void") (&/V &/$VariantT (&/|list))))
(def Ident (Named$ (&/T "lux" "Ident") (Tuple$ (&/|list Text Text))))

(def IO
  (Named$ (&/T "lux/data" "IO")
          (Univ$ empty-env
                 (Lambda$ Unit (Bound$ 1)))))

(def List
  (Named$ (&/T "lux" "List")
          (Univ$ empty-env
                 (Variant$ (&/|list
                            ;; lux;Nil
                            Unit
                            ;; lux;Cons
                            (Tuple$ (&/|list (Bound$ 1)
                                             (App$ (Bound$ 0)
                                                   (Bound$ 1))))
                            )))))

(def Maybe
  (Named$ (&/T "lux" "Maybe")
          (Univ$ empty-env
                 (Variant$ (&/|list
                            ;; lux;None
                            Unit
                            ;; lux;Some
                            (Bound$ 1)
                            )))))

(def Type
  (Named$ (&/T "lux" "Type")
          (let [Type (App$ (Bound$ 0) (Bound$ 1))
                TypeList (App$ List Type)
                TypePair (Tuple$ (&/|list Type Type))]
            (App$ (Univ$ empty-env
                         (Variant$ (&/|list
                                    ;; DataT
                                    Text
                                    ;; VariantT
                                    TypeList
                                    ;; TupleT
                                    TypeList
                                    ;; LambdaT
                                    TypePair
                                    ;; BoundT
                                    Int
                                    ;; VarT
                                    Int
                                    ;; ExT
                                    Int
                                    ;; UnivQ
                                    (Tuple$ (&/|list TypeList Type))
                                    ;; ExQ
                                    (Tuple$ (&/|list TypeList Type))
                                    ;; AppT
                                    TypePair
                                    ;; NamedT
                                    (Tuple$ (&/|list Ident Type))
                                    )))
                  $Void))))

(def Bindings
  (Named$ (&/T "lux" "Bindings")
          (Univ$ empty-env
                 (Univ$ empty-env
                        (Tuple$ (&/|list
                                 ;; "lux;counter"
                                 Int
                                 ;; "lux;mappings"
                                 (App$ List
                                       (Tuple$ (&/|list (Bound$ 3)
                                                        (Bound$ 1))))))))))

(def Env
  (Named$ (&/T "lux" "Env")
          (let [bindings (App$ (App$ Bindings (Bound$ 3))
                               (Bound$ 1))]
            (Univ$ empty-env
                   (Univ$ empty-env
                          (Tuple$
                           (&/|list
                            ;; "lux;name"
                            Text
                            ;; "lux;inner-closures"
                            Int
                            ;; "lux;locals"
                            bindings
                            ;; "lux;closure"
                            bindings
                            )))))))

(def Cursor
  (Named$ (&/T "lux" "Cursor")
          (Tuple$ (&/|list Text Int Int))))

(def Meta
  (Named$ (&/T "lux" "Meta")
          (Univ$ empty-env
                 (Univ$ empty-env
                        (Tuple$ (&/|list (Bound$ 3)
                                         (Bound$ 1)))))))

(def AST*
  (Named$ (&/T "lux" "AST'")
          (let [AST* (App$ (Bound$ 1)
                           (App$ (Bound$ 0)
                                 (Bound$ 1)))
                AST*List (App$ List AST*)]
            (Univ$ empty-env
                   (Variant$ (&/|list
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
                              (App$ List (Tuple$ (&/|list AST* AST*))))
                             )))))

(def AST
  (Named$ (&/T "lux" "AST")
          (let [w (App$ Meta Cursor)]
            (App$ w (App$ AST* w)))))

(def ^:private ASTList (App$ List AST))

(def Either
  (Named$ (&/T "lux" "Either")
          (Univ$ empty-env
                 (Univ$ empty-env
                        (Variant$ (&/|list
                                   ;; &/$Left
                                   (Bound$ 3)
                                   ;; &/$Right
                                   (Bound$ 1)))))))

(def StateE
  (Univ$ empty-env
         (Univ$ empty-env
                (Lambda$ (Bound$ 3)
                         (App$ (App$ Either Text)
                               (Tuple$ (&/|list (Bound$ 3)
                                                (Bound$ 1))))))))

(def Source
  (Named$ (&/T "lux" "Source")
          (App$ List
                (App$ (App$ Meta Cursor)
                      Text))))

(def Host
  (Named$ (&/T "lux" "Host")
          (Tuple$
           (&/|list
            ;; "lux;writer"
            (Data$ "org.objectweb.asm.ClassWriter")
            ;; "lux;loader"
            (Data$ "java.lang.ClassLoader")
            ;; "lux;classes"
            (Data$ "clojure.lang.Atom")))))

(def DefData*
  (Univ$ empty-env
         (Variant$ (&/|list
                    ;; "lux;ValueD"
                    (Tuple$ (&/|list Type Unit))
                    ;; "lux;TypeD"
                    Type
                    ;; "lux;MacroD"
                    (Bound$ 1)
                    ;; "lux;AliasD"
                    Ident
                    ))))

(def LuxVar
  (Named$ (&/T "lux" "LuxVar")
          (Variant$ (&/|list
                     ;; "lux;Local"
                     Int
                     ;; "lux;Global"
                     Ident))))

(def $Module
  (Univ$ empty-env
         (Tuple$
          (&/|list
           ;; "lux;module-aliases"
           (App$ List (Tuple$ (&/|list Text Text)))
           ;; "lux;defs"
           (App$ List
                 (Tuple$ (&/|list Text
                                  (Tuple$ (&/|list Bool
                                                   (App$ DefData*
                                                         (Lambda$ ASTList
                                                                  (App$ (App$ StateE (Bound$ 1))
                                                                        ASTList))))))))
           ;; "lux;imports"
           (App$ List Text)
           ;; "lux;tags"
           ;; (List (, Text (, Int (List Ident) Type)))
           (App$ List
                 (Tuple$ (&/|list Text
                                  (Tuple$ (&/|list Int
                                                   (App$ List Ident)
                                                   Type)))))
           ;; "lux;types"
           ;; (List (, Text (, (List Ident) Type)))
           (App$ List
                 (Tuple$ (&/|list Text
                                  (Tuple$ (&/|list (App$ List Ident)
                                                   Type)))))
           ))))

(def $Compiler
  (Named$ (&/T "lux" "Compiler")
          (App$ (Univ$ empty-env
                       (Tuple$
                        (&/|list
                         ;; "lux;source"
                         Source
                         ;; "lux;cursor"
                         Cursor
                         ;; "lux;modules"
                         (App$ List (Tuple$ (&/|list Text
                                                     (App$ $Module (App$ (Bound$ 0) (Bound$ 1))))))
                         ;; "lux;envs"
                         (App$ List
                               (App$ (App$ Env Text)
                                     (Tuple$ (&/|list LuxVar Type))))
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
                         )))
                $Void)))

(def Macro
  (Named$ (&/T "lux" "Macro")
          (Lambda$ ASTList
                   (App$ (App$ StateE $Compiler)
                         ASTList))))

(defn bound? [id]
  (fn [state]
    (if-let [type (->> state (&/get$ &/$type-vars) (&/get$ &/$mappings) (&/|get id))]
      (|case type
        (&/$Some type*)
        (return* state true)
        
        (&/$None)
        (return* state false))
      (fail* (str "[Type Error] <bound?> Unknown type-var: " id)))))

(defn deref [id]
  (fn [state]
    (if-let [type* (->> state (&/get$ &/$type-vars) (&/get$ &/$mappings) (&/|get id))]
      (|case type*
        (&/$Some type)
        (return* state type)
        
        (&/$None)
        (fail* (str "[Type Error] Unbound type-var: " id)))
      (fail* (str "[Type Error] <deref> Unknown type-var: " id)))))

(defn deref+ [type]
  (|case type
    (&/$VarT id)
    (deref id)

    _
    (fail (str "[Type Error] Type is not a variable: " (show-type type)))))

(defn set-var [id type]
  (fn [state]
    (if-let [tvar (->> state (&/get$ &/$type-vars) (&/get$ &/$mappings) (&/|get id))]
      (|case tvar
        (&/$Some bound)
        (fail* (str "[Type Error] Can't rebind type var: " id " | Current type: " (show-type bound)))
        
        (&/$None)
        (return* (&/update$ &/$type-vars (fn [ts] (&/update$ &/$mappings #(&/|put id (&/V &/$Some type) %)
                                                            ts))
                            state)
                 nil))
      (fail* (str "[Type Error] <set-var> Unknown type-var: " id " | " (->> state (&/get$ &/$type-vars) (&/get$ &/$mappings) &/|length))))))

;; [Exports]
;; Type vars
(def ^:private create-var
  (fn [state]
    (let [id (->> state (&/get$ &/$type-vars) (&/get$ &/$counter))]
      (return* (&/update$ &/$type-vars #(->> %
                                             (&/update$ &/$counter inc)
                                             (&/update$ &/$mappings (fn [ms] (&/|put id (&/V &/$None nil) ms))))
                          state)
               id))))

(def existential
  (|do [seed &/gen-id]
    (return (&/V &/$ExT seed))))

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
                                           (return (&/T ?id (&/V &/$None nil)))
                                           (return binding))

                                         _
                                         (|do [?type** (clean* id ?type*)]
                                           (return (&/T ?id (&/V &/$Some ?type**)))))
                                       ))))
                               (->> state (&/get$ &/$type-vars) (&/get$ &/$mappings)))]
         (fn [state]
           (return* (&/update$ &/$type-vars #(->> %
                                                  ;; (&/update$ &/$counter dec)
                                                  (&/set$ &/$mappings (&/|remove id mappings*)))
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

    (&/$TupleT ?members)
    (|do [=members (&/map% (partial clean* ?tid) ?members)]
      (return (Tuple$ =members)))
    
    (&/$VariantT ?members)
    (|do [=members (&/map% (partial clean* ?tid) ?members)]
      (return (Variant$ =members)))

    (&/$UnivQ ?env ?body)
    (|do [=env (&/map% (partial clean* ?tid) ?env)
          body* (clean* ?tid ?body)]
      (return (Univ$ =env body*)))

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
      (&/T ??out (&/Cons$ ?in ?args)))

    _
    (&/T type (&/|list))))

(defn ^:private unravel-app [fun-type]
  (|case fun-type
    (&/$AppT ?left ?right)
    (|let [[?fun-type ?args] (unravel-app ?left)]
      (&/T ?fun-type (&/|++ ?args (&/|list ?right))))

    _
    (&/T fun-type (&/|list))))

(defn show-type [^objects type]
  (|case type
    (&/$DataT name)
    (str "(^ " name ")")
    
    (&/$TupleT elems)
    (if (&/|empty? elems)
      "(,)"
      (str "(, " (->> elems (&/|map show-type) (&/|interpose " ") (&/fold str "")) ")"))

    (&/$VariantT cases)
    (if (&/|empty? cases)
      "(|)"
      (str "(| " (->> cases
                      (&/|map show-type)
                      (&/|interpose " ")
                      (&/fold str "")) ")"))
    
    (&/$LambdaT input output)
    (|let [[?out ?ins] (unravel-fun type)]
      (str "(-> " (->> ?ins (&/|map show-type) (&/|interpose " ") (&/fold str "")) " " (show-type ?out) ")"))

    (&/$VarT id)
    (str "⌈" id "⌋")

    (&/$ExT ?id)
    (str "⟨" ?id "⟩")

    (&/$BoundT idx)
    (str idx)

    (&/$AppT _ _)
    (|let [[?call-fun ?call-args] (unravel-app type)]
      (str "(" (show-type ?call-fun) " " (->> ?call-args (&/|map show-type) (&/|interpose " ") (&/fold str "")) ")"))
    
    (&/$UnivQ ?env ?body)
    (str "(All " (show-type ?body) ")")
    
    (&/$NamedT ?name ?type)
    (&/ident->text ?name)

    _
    (assert false (prn-str 'show-type (&/adt->text type)))))

(defn type= [x y]
  (or (clojure.lang.Util/identical x y)
      (let [output (|case [x y]
                     [(&/$NamedT [?xmodule ?xname] ?xtype) (&/$NamedT [?ymodule ?yname] ?ytype)]
                     (and (= ?xmodule ?ymodule)
                          (= ?xname ?yname))

                     [(&/$DataT xname) (&/$DataT yname)]
                     (.equals ^Object xname yname)

                     [(&/$TupleT xelems) (&/$TupleT yelems)]
                     (&/fold2 (fn [old x y] (and old (type= x y)))
                              true
                              xelems yelems)

                     [(&/$VariantT xcases) (&/$VariantT ycases)]
                     (&/fold2 (fn [old x y] (and old (type= x y)))
                              true
                              xcases ycases)

                     [(&/$LambdaT xinput xoutput) (&/$LambdaT yinput youtput)]
                     (and (type= xinput yinput)
                          (type= xoutput youtput))

                     [(&/$VarT xid) (&/$VarT yid)]
                     (.equals ^Object xid yid)

                     [(&/$BoundT xidx) (&/$BoundT yidx)]
                     (= xidx yidx)

                     [(&/$ExT xid) (&/$ExT yid)]
                     (.equals ^Object xid yid)

                     [(&/$AppT xlambda xparam) (&/$AppT ylambda yparam)]
                     (and (type= xlambda ylambda) (type= xparam yparam))
                     
                     [(&/$UnivQ xenv xbody) (&/$UnivQ yenv ybody)]
                     (type= xbody ybody)

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
      (&/V &/$None nil)

      (&/$Cons [[e* a*] v*] fixpoints*)
      (if (and (type= e e*)
               (type= a a*))
        (&/V &/$Some v*)
        (fp-get k fixpoints*))
      )))

(defn ^:private fp-put [k v fixpoints]
  (&/Cons$ (&/T k v) fixpoints))

(defn ^:private check-error [expected actual]
  (str "[Type Checker]\nExpected: " (show-type expected)
       "\n\nActual: " (show-type actual)
       "\n"))

;; (def !flag (atom false))

(defn beta-reduce [env type]
  ;; (when @!flag
  ;;   (prn 'beta-reduce (show-type type)))
  (|case type
    (&/$VariantT ?members)
    (Variant$ (&/|map (partial beta-reduce env) ?members))

    (&/$TupleT ?members)
    (Tuple$ (&/|map (partial beta-reduce env) ?members))

    (&/$AppT ?type-fn ?type-arg)
    (App$ (beta-reduce env ?type-fn) (beta-reduce env ?type-arg))

    (&/$UnivQ ?local-env ?local-def)
    (|case ?local-env
      (&/$Nil)
      (Univ$ env ?local-def)

      _
      type)

    (&/$LambdaT ?input ?output)
    (Lambda$ (beta-reduce env ?input) (beta-reduce env ?output))

    (&/$BoundT ?idx)
    (|case (&/|at ?idx env)
      (&/$Some bound)
      (beta-reduce env bound)

      _
      (assert false (str "[Type Error] Unknown var: " ?idx " | " (&/->seq (&/|map show-type env)))))

    _
    type
    ))

(defn apply-type [type-fn param]
  ;; (when @!flag
  ;;   (prn 'apply-type (show-type type-fn) (show-type param)))
  (|case type-fn
    (&/$UnivQ local-env local-def)
    (return (beta-reduce (->> local-env
                              (&/Cons$ param)
                              (&/Cons$ type-fn))
                         local-def))

    (&/$AppT F A)
    (|do [type-fn* (apply-type F A)]
      (apply-type type-fn* param))

    (&/$NamedT ?name ?type)
    (apply-type ?type param)
    
    _
    (fail (str "[Type System] Not a type function:\n" (show-type type-fn) "\n"))))

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
    (|case [expected actual]
      [(&/$VarT ?eid) (&/$VarT ?aid)]
      (if (.equals ^Object ?eid ?aid)
        (return (&/T fixpoints nil))
        (|do [ebound (fn [state]
                       (|case ((deref ?eid) state)
                         (&/$Right state* ebound)
                         (return* state* (&/V &/$Some ebound))

                         (&/$Left _)
                         (return* state (&/V &/$None nil))))
              abound (fn [state]
                       (|case ((deref ?aid) state)
                         (&/$Right state* abound)
                         (return* state* (&/V &/$Some abound))

                         (&/$Left _)
                         (return* state (&/V &/$None nil))))]
          (|case [ebound abound]
            [(&/$None _) (&/$None _)]
            (|do [_ (set-var ?eid actual)]
              (return (&/T fixpoints nil)))
            
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
          (return* state* (&/T fixpoints nil))

          (&/$Left _)
          ((|do [bound (deref ?id)]
             (check* class-loader fixpoints bound actual))
           state)))
      
      [_ (&/$VarT ?id)]
      (fn [state]
        (|case ((set-var ?id expected) state)
          (&/$Right state* _)
          (return* state* (&/T fixpoints nil))

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
               (return (&/T fixpoints** nil)))
             state))))
      ;; (|do [_ (check* class-loader fixpoints (Var$ ?eid) (Var$ ?aid))
      ;;       _ (check* class-loader fixpoints A1 A2)]
      ;;   (return (&/T fixpoints nil)))
      
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
             (return (&/T fixpoints** nil)))
           state)))
      ;; [[&/$AppT [[&/$VarT ?id] A1]] [&/$AppT [F2 A2]]]
      ;; (|do [[fixpoints* _] (check* class-loader fixpoints (Var$ ?id) F2)
      ;;       e* (apply-type F2 A1)
      ;;       a* (apply-type F2 A2)
      ;;       [fixpoints** _] (check* class-loader fixpoints* e* a*)]
      ;;   (return (&/T fixpoints** nil)))
      
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
             (return (&/T fixpoints** nil)))
           state)))
      ;; [[&/$AppT [F1 A1]] [&/$AppT [[&/$VarT ?id] A2]]]
      ;; (|do [[fixpoints* _] (check* class-loader fixpoints F1 (Var$ ?id))
      ;;       e* (apply-type F1 A1)
      ;;       a* (apply-type F1 A2)
      ;;       [fixpoints** _] (check* class-loader fixpoints* e* a*)]
      ;;   (return (&/T fixpoints** nil)))

      [(&/$AppT F A) _]
      (let [fp-pair (&/T expected actual)
            _ (when (> (&/|length fixpoints) 40)
                (println 'FIXPOINTS (->> (&/|keys fixpoints)
                                         (&/|map (fn [pair]
                                                   (|let [[e a] pair]
                                                     (str (show-type e) ":+:"
                                                          (show-type a)))))
                                         (&/|interpose "\n\n")
                                         (&/fold str "")))
                (assert false (prn-str 'check* '[(&/$AppT F A) _] (&/|length fixpoints) (show-type expected) (show-type actual))))]
        (|case (fp-get fp-pair fixpoints)
          (&/$Some ?)
          (if ?
            (return (&/T fixpoints nil))
            (fail (check-error expected actual)))

          (&/$None)
          (|do [expected* (apply-type F A)]
            (check* class-loader (fp-put fp-pair true fixpoints) expected* actual))))

      [_ (&/$AppT F A)]
      (|do [actual* (apply-type F A)]
        (check* class-loader fixpoints expected actual*))

      [(&/$UnivQ _) _]
      (with-var
        (fn [$arg]
          (|do [expected* (apply-type expected $arg)]
            (check* class-loader fixpoints expected* actual))))

      [_ (&/$UnivQ _)]
      (with-var
        (fn [$arg]
          (|do [actual* (apply-type actual $arg)]
            (check* class-loader fixpoints expected actual*))))

      [(&/$DataT e!name) (&/$DataT "null")]
      (if (contains? primitive-types e!name)
        (fail (str "[Type Error] Can't use \"null\" with primitive types."))
        (return (&/T fixpoints nil)))

      [(&/$DataT e!name) (&/$DataT a!name)]
      (let [e!name (as-obj e!name)
            a!name (as-obj a!name)]
        (if (or (.equals ^Object e!name a!name)
                (.isAssignableFrom (Class/forName e!name true class-loader) (Class/forName a!name true class-loader)))
          (return (&/T fixpoints nil))
          (fail (str "[Type Error] Names don't match: " e!name " =/= " a!name))))

      [(&/$LambdaT eI eO) (&/$LambdaT aI aO)]
      (|do [[fixpoints* _] (check* class-loader fixpoints aI eI)]
        (check* class-loader fixpoints* eO aO))

      [(&/$TupleT e!members) (&/$TupleT a!members)]
      (|do [fixpoints* (&/fold2% (fn [fp e a]
                                   (|do [[fp* _] (check* class-loader fp e a)]
                                     (return fp*)))
                                 fixpoints
                                 e!members a!members)]
        (return (&/T fixpoints* nil)))
      
      [(&/$VariantT e!cases) (&/$VariantT a!cases)]
      (|do [fixpoints* (&/fold2% (fn [fp e a]
                                   (|do [[fp* _] (check* class-loader fp e a)]
                                     (return fp*)))
                                 fixpoints
                                 e!cases a!cases)]
        (return (&/T fixpoints* nil)))

      [(&/$ExT e!id) (&/$ExT a!id)]
      (if (.equals ^Object e!id a!id)
        (return (&/T fixpoints nil))
        (fail (check-error expected actual)))

      [(&/$NamedT ?ename ?etype) _]
      (check* class-loader fixpoints ?etype actual)

      [_ (&/$NamedT ?aname ?atype)]
      (check* class-loader fixpoints expected ?atype)

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

    (&/$UnivQ _)
    (with-var
      (fn [$var]
        (|do [func* (apply-type func $var)
              =return (apply-lambda func* param)]
          (clean $var =return))))

    (&/$NamedT ?name ?type)
    (apply-lambda ?type param)

    _
    (fail (str "[Type System] Not a function type:\n" (show-type func) "\n"))
    ))

(defn actual-type [type]
  "(-> Type (Lux Type))"
  (|case type
    (&/$AppT ?all ?param)
    (|do [type* (apply-type ?all ?param)]
      (actual-type type*))

    (&/$VarT id)
    (|do [=type (deref id)]
      (actual-type =type))

    (&/$NamedT ?name ?type)
    (actual-type ?type)
    
    _
    (return type)
    ))

(defn variant-case [tag type]
  (|case type
    (&/$NamedT ?name ?type)
    (variant-case tag ?type)
    
    (&/$VariantT ?cases)
    (|case (&/|at tag ?cases)
      (&/$Some case-type)
      (return case-type)

      (&/$None)
      (fail (str "[Type Error] Variant lacks case: " tag " | " (show-type type))))

    _
    (fail (str "[Type Error] Type is not a variant: " (show-type type)))))

(defn type-name [type]
  "(-> Type (Lux Ident))"
  (|case type
    (&/$NamedT name _)
    (return name)
    
    _
    (fail (str "[Type Error] Type is not named: " (show-type type)))
    ))

(defn unknown? [type]
  "(-> Type (Lux Bool))"
  (|case type
    (&/$VarT id)
    (|do [? (bound? id)]
      (return (not ?)))

    _
    (return false)))
