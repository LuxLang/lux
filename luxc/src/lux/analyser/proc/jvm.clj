(ns lux.analyser.proc.jvm
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case assert!]]
                 [type :as &type]
                 [host :as &host]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [reader :as &reader])
            [lux.type.host :as &host-type]
            [lux.host.generics :as &host-generics]
            (lux.analyser [base :as &&]
                          [env :as &&env]
                          [parser :as &&a-parser])
            [lux.compiler.jvm.base :as &c!base])
  (:import (java.lang.reflect Type TypeVariable)))

;; [Utils]
(defn ^:private ensure-object [type]
  "(-> Type (Lux (, Text (List Type))))"
  (|case type
    (&/$Primitive payload)
    (return payload)

    (&/$Var id)
    (return (&/T ["java.lang.Object" (&/|list)]))

    (&/$Ex id)
    (return (&/T ["java.lang.Object" (&/|list)]))

    (&/$Named _ type*)
    (ensure-object type*)

    (&/$UnivQ _ type*)
    (ensure-object type*)

    (&/$ExQ _ type*)
    (ensure-object type*)

    (&/$Apply A F)
    (|do [type* (&type/apply-type F A)]
      (ensure-object type*))

    _
    (&/fail-with-loc (str "[Analyser Error] Was expecting object type. Instead got: " (&type/show-type type)))))

(defn ^:private as-object [type]
  "(-> Type Type)"
  (|case type
    (&/$Primitive class params)
    (&/$Primitive (&host-type/as-obj class) params)

    _
    type))

(defn ^:private as-otype [tname]
  (case tname
    "boolean" "java.lang.Boolean"
    "byte"    "java.lang.Byte"
    "short"   "java.lang.Short"
    "int"     "java.lang.Integer"
    "long"    "java.lang.Long"
    "float"   "java.lang.Float"
    "double"  "java.lang.Double"
    "char"    "java.lang.Character"
    ;; else
    tname
    ))

(defn ^:private as-otype+ [type]
  "(-> Type Type)"
  (|case type
    (&/$Primitive name params)
    (&/$Primitive (as-otype name) params)

    _
    type))

(defn ^:private clean-gtype-var [idx gtype-var]
  (|let [(&/$Var id) gtype-var]
    (|do [? (&type/bound? id)]
      (if ?
        (|do [real-type (&type/deref id)]
          (return (&/T [idx real-type])))
        (return (&/T [(+ 2 idx) (&/$Bound idx)]))))))

(defn ^:private clean-gtype-vars [gtype-vars]
  (|do [[_ clean-types] (&/fold% (fn [idx+types gtype-var]
                                   (|do [:let [[idx types] idx+types]
                                         [idx* real-type] (clean-gtype-var idx gtype-var)]
                                     (return (&/T [idx* (&/$Cons real-type types)]))))
                                 (&/T [1 &/$Nil])
                                 gtype-vars)]
    (return clean-types)))

(defn ^:private make-gtype [class-name type-args]
  "(-> Text (List Type) Type)"
  (&/fold (fn [base-type type-arg]
            (|case type-arg
              (&/$Bound _)
              (&/$UnivQ &type/empty-env base-type)
              
              _
              base-type))
          (&/$Primitive class-name type-args)
          type-args))

;; [Resources]
(defn ^:private analyse-field-access-helper [obj-type gvars gtype]
  "(-> Type (List (^ java.lang.reflect.Type)) (^ java.lang.reflect.Type) (Lux Type))"
  (|case obj-type
    (&/$Primitive class targs)
    (if (= (&/|length targs) (&/|length gvars))
      (|let [gtype-env (&/fold2 (fn [m ^TypeVariable g t] (&/$Cons (&/T [(.getName g) t]) m))
                                (&/|table)
                                gvars
                                targs)]
        (&host-type/instance-param &type/existential gtype-env gtype))
      (&/fail-with-loc (str "[Type Error] Mismatched number of type-parameters for " (&type/show-type obj-type) "\n"
                            "Expected: " (&/|length targs) "\n"
                            "  Actual: " (&/|length gvars))))

    _
    (&/fail-with-loc (str "[Type Error] Type is not an object type: " (&type/show-type obj-type)))))

(defn generic-class->simple-class [gclass]
  "(-> GenericClass Text)"
  (|case gclass
    (&/$GenericTypeVar var-name)
    "java.lang.Object"

    (&/$GenericWildcard _)
    "java.lang.Object"
    
    (&/$GenericClass name params)
    name

    (&/$GenericArray param)
    (|case param
      (&/$GenericArray _)
      (str "[" (generic-class->simple-class param))

      (&/$GenericClass "boolean" _)
      "[Z"
      
      (&/$GenericClass "byte" _)
      "[B"
      
      (&/$GenericClass "short" _)
      "[S"
      
      (&/$GenericClass "int" _)
      "[I"
      
      (&/$GenericClass "long" _)
      "[J"
      
      (&/$GenericClass "float" _)
      "[F"
      
      (&/$GenericClass "double" _)
      "[D"
      
      (&/$GenericClass "char" _)
      "[C"

      (&/$GenericClass name params)
      (str "[L" name ";")

      (&/$GenericTypeVar var-name)
      "[Ljava.lang.Object;"

      (&/$GenericWildcard _)
      "[Ljava.lang.Object;")
    ))

(defn generic-class->type [env gclass]
  "(-> (List (, TypeVar Type)) GenericClass (Lux Type))"
  (|case gclass
    (&/$GenericTypeVar var-name)
    (if-let [ex (&/|get var-name env)]
      (return ex)
      (&/fail-with-loc (str "[Analysis Error] Unknown type-var: " var-name)))
    
    (&/$GenericClass name params)
    (case name
      "boolean" (return (&/$Primitive "java.lang.Boolean" &/$Nil))
      "byte"    (return (&/$Primitive "java.lang.Byte" &/$Nil))
      "short"   (return (&/$Primitive "java.lang.Short" &/$Nil))
      "int"     (return (&/$Primitive "java.lang.Integer" &/$Nil))
      "long"    (return (&/$Primitive "java.lang.Long" &/$Nil))
      "float"   (return (&/$Primitive "java.lang.Float" &/$Nil))
      "double"  (return (&/$Primitive "java.lang.Double" &/$Nil))
      "char"    (return (&/$Primitive "java.lang.Character" &/$Nil))
      "void"    (return &/$Unit)
      ;; else
      (|do [=params (&/map% (partial generic-class->type env) params)]
        (return (&/$Primitive name =params))))

    (&/$GenericArray param)
    (|do [=param (generic-class->type env param)]
      (return (&/$Primitive &host-type/array-data-tag (&/|list =param))))

    (&/$GenericWildcard _)
    (return (&/$ExQ &/$Nil (&/$Bound 1)))
    ))

(defn gen-super-env [class-env supers class-decl]
  "(-> (List (, TypeVar Type)) (List SuperClassDecl) ClassDecl (Lux (List (, Text Type))))"
  (|let [[class-name class-vars] class-decl]
    (|case (&/|some (fn [super]
                      (|let [[super-name super-params] super]
                        (if (= class-name super-name)
                          (&/$Some (&/zip2 (&/|map &/|first class-vars) super-params))
                          &/$None)))
                    supers)
      (&/$None)
      (&/fail-with-loc (str "[Analyser Error] Unrecognized super-class: " class-name))

      (&/$Some vars+gtypes)
      (&/map% (fn [var+gtype]
                (|do [:let [[var gtype] var+gtype]
                      =gtype (generic-class->type class-env gtype)]
                  (return (&/T [var =gtype]))))
              vars+gtypes)
      )))

(defn ^:private make-type-env [type-params]
  "(-> (List TypeParam) (Lux (List [Text Type])))"
  (&/map% (fn [gvar]
            (|do [:let [[gvar-name _] gvar]
                  ex &type/existential]
              (return (&/T [gvar-name ex]))))
          type-params))

(defn ^:private double-register-gclass? [gclass]
  (|case gclass
    (&/$GenericClass name _)
    (|case name
      "long"   true
      "double" true
      _        false)

    _
    false))

(defn ^:private method-input-folder [full-env]
  (fn [body* input*]
    (|do [:let [[iname itype*] input*]
          itype (generic-class->type full-env itype*)]
      (if (double-register-gclass? itype*)
        (&&env/with-local iname itype
          (&&env/with-local "" &/$Void
            body*))
        (&&env/with-local iname itype
          body*)))))

(defn ^:private analyse-method [analyse class-decl class-env all-supers method]
  "(-> Analyser ClassDecl (List (, TypeVar Type)) (List SuperClassDecl) MethodSyntax (Lux MethodAnalysis))"
  (|let [[?cname ?cparams] class-decl
         class-type (&/$Primitive ?cname (&/|map &/|second class-env))]
    (|case method
      (&/$ConstructorMethodSyntax =privacy-modifier ?strict ?anns ?gvars ?exceptions ?inputs ?ctor-args ?body)
      (|do [method-env (make-type-env ?gvars)
            :let [full-env (&/|++ class-env method-env)]
            :let [output-type &/$Unit]
            =ctor-args (&/map% (fn [ctor-arg]
                                 (|do [:let [[ca-type ca-term] ctor-arg]
                                       =ca-type (generic-class->type full-env ca-type)
                                       =ca-term (&&/analyse-1 analyse =ca-type ca-term)]
                                   (return (&/T [ca-type =ca-term]))))
                               ?ctor-args)
            =body (&/with-type-env full-env
                    (&&env/with-local &&/jvm-this class-type
                      (&/fold (method-input-folder full-env)
                              (&&/analyse-1 analyse output-type ?body)
                              (&/|reverse ?inputs))))]
        (return (&/$ConstructorMethodAnalysis (&/T [=privacy-modifier ?strict ?anns ?gvars ?exceptions ?inputs =ctor-args =body]))))
      
      (&/$VirtualMethodSyntax ?name =privacy-modifier =final? ?strict ?anns ?gvars ?exceptions ?inputs ?output ?body)
      (|do [method-env (make-type-env ?gvars)
            :let [full-env (&/|++ class-env method-env)]
            output-type (generic-class->type full-env ?output)
            =body (&/with-type-env full-env
                    (&&env/with-local &&/jvm-this class-type
                      (&/fold (method-input-folder full-env)
                              (&&/analyse-1 analyse output-type ?body)
                              (&/|reverse ?inputs))))]
        (return (&/$VirtualMethodAnalysis (&/T [?name =privacy-modifier =final? ?strict ?anns ?gvars ?exceptions ?inputs ?output =body]))))
      
      (&/$OverridenMethodSyntax ?class-decl ?name ?strict ?anns ?gvars ?exceptions ?inputs ?output ?body)
      (|do [super-env (gen-super-env class-env all-supers ?class-decl)
            method-env (make-type-env ?gvars)
            :let [full-env (&/|++ super-env method-env)]
            output-type (generic-class->type full-env ?output)
            =body (&/with-type-env full-env
                    (&&env/with-local &&/jvm-this class-type
                      (&/fold (method-input-folder full-env)
                              (&&/analyse-1 analyse output-type ?body)
                              (&/|reverse ?inputs))))]
        (return (&/$OverridenMethodAnalysis (&/T [?class-decl ?name ?strict ?anns ?gvars ?exceptions ?inputs ?output =body]))))

      (&/$StaticMethodSyntax ?name =privacy-modifier ?strict ?anns ?gvars ?exceptions ?inputs ?output ?body)
      (|do [method-env (make-type-env ?gvars)
            :let [full-env method-env]
            output-type (generic-class->type full-env ?output)
            =body (&/with-type-env full-env
                    (&/fold (method-input-folder full-env)
                            (&&/analyse-1 analyse output-type ?body)
                            (&/|reverse ?inputs)))]
        (return (&/$StaticMethodAnalysis (&/T [?name =privacy-modifier ?strict ?anns ?gvars ?exceptions ?inputs ?output =body]))))

      (&/$AbstractMethodSyntax ?name =privacy-modifier ?anns ?gvars ?exceptions ?inputs ?output)
      (return (&/$AbstractMethodAnalysis (&/T [?name =privacy-modifier ?anns ?gvars ?exceptions ?inputs ?output])))

      (&/$NativeMethodSyntax ?name =privacy-modifier ?anns ?gvars ?exceptions ?inputs ?output)
      (return (&/$NativeMethodAnalysis (&/T [?name =privacy-modifier ?anns ?gvars ?exceptions ?inputs ?output])))
      )))

(defn ^:private mandatory-methods [supers]
  (|do [class-loader &/loader]
    (&/flat-map% (partial &host/abstract-methods class-loader) supers)))

(defn ^:private check-method-completion [supers methods]
  "(-> (List SuperClassDecl) (List (, MethodDecl Analysis)) (Lux Null))"
  (|do [abstract-methods (mandatory-methods supers)
        :let [methods-map (&/fold (fn [mmap mentry]
                                    (|case mentry
                                      (&/$ConstructorMethodAnalysis _)
                                      mmap
                                      
                                      (&/$VirtualMethodAnalysis _)
                                      mmap
                                      
                                      (&/$OverridenMethodAnalysis =class-decl =name ?strict =anns =gvars =exceptions =inputs =output body)
                                      (update-in mmap [=name] (fn [old-inputs] (if old-inputs (conj old-inputs =inputs) [=inputs])))

                                      (&/$StaticMethodAnalysis _)
                                      mmap

                                      (&/$AbstractMethodSyntax _)
                                      mmap

                                      (&/$NativeMethodSyntax _)
                                      mmap
                                      ))
                                  {}
                                  methods)
              missing-method (&/fold (fn [missing abs-meth]
                                       (or missing
                                           (|let [[am-name am-inputs] abs-meth]
                                             (if-let [meth-struct (get methods-map am-name)]
                                               (if (some (fn [=inputs]
                                                           (and (= (&/|length =inputs) (&/|length am-inputs))
                                                                (&/fold2 (fn [prev mi ai]
                                                                           (|let [[iname itype] mi]
                                                                             (and prev (= (generic-class->simple-class itype) ai))))
                                                                         true
                                                                         =inputs am-inputs)))
                                                         meth-struct)
                                                 nil
                                                 abs-meth)
                                               abs-meth))))
                                     nil
                                     abstract-methods)]]
    (if (nil? missing-method)
      (return nil)
      (|let [[am-name am-inputs] missing-method]
        (&/fail-with-loc (str "[Analyser Error] Missing method: " am-name " " "(" (->> am-inputs (&/|interpose " ") (&/fold str "")) ")"))))))

(defn ^:private analyse-field [analyse gtype-env field]
  "(-> Analyser GTypeEnv FieldSyntax (Lux FieldAnalysis))"
  (|case field
    (&/$ConstantFieldSyntax ?name ?anns ?gclass ?value)
    (|do [=gtype (&host-type/instance-gtype &type/existential gtype-env ?gclass)
          =value (&&/analyse-1 analyse =gtype ?value)]
      (return (&/$ConstantFieldAnalysis ?name ?anns ?gclass =value)))
    
    (&/$VariableFieldSyntax ?name ?privacy-modifier ?state-modifier ?anns ?type)
    (return (&/$VariableFieldAnalysis ?name ?privacy-modifier ?state-modifier ?anns ?type))
    ))

(do-template [<name> <proc> <from-class> <to-class>]
  (let [output-type (&/$Primitive <to-class> &/$Nil)]
    (defn <name> [analyse exo-type _?value]
      (|do [:let [(&/$Cons ?value (&/$Nil)) _?value]
            =value (&&/analyse-1 analyse (&/$Primitive <from-class> &/$Nil) ?value)
            _ (&type/check exo-type output-type)
            _cursor &/cursor]
        (return (&/|list (&&/|meta output-type _cursor (&&/$proc (&/T ["jvm" <proc>]) (&/|list =value) (&/|list))))))))

  ^:private analyse-jvm-double-to-float "double-to-float" "java.lang.Double"    "java.lang.Float"
  ^:private analyse-jvm-double-to-int "double-to-int" "java.lang.Double"    "java.lang.Integer"
  ^:private analyse-jvm-double-to-long "double-to-long" "java.lang.Double"    "java.lang.Long"

  ^:private analyse-jvm-float-to-double "float-to-double" "java.lang.Float"     "java.lang.Double"
  ^:private analyse-jvm-float-to-int "float-to-int" "java.lang.Float"     "java.lang.Integer"
  ^:private analyse-jvm-float-to-long "float-to-long" "java.lang.Float"     "java.lang.Long"

  ^:private analyse-jvm-int-to-byte "int-to-byte" "java.lang.Integer"   "java.lang.Byte"
  ^:private analyse-jvm-int-to-char "int-to-char" "java.lang.Integer"   "java.lang.Character"
  ^:private analyse-jvm-int-to-double "int-to-double" "java.lang.Integer"   "java.lang.Double"
  ^:private analyse-jvm-int-to-float "int-to-float" "java.lang.Integer"   "java.lang.Float"
  ^:private analyse-jvm-int-to-long "int-to-long" "java.lang.Integer"   "java.lang.Long"
  ^:private analyse-jvm-int-to-short "int-to-short" "java.lang.Integer"   "java.lang.Short"

  ^:private analyse-jvm-long-to-double "long-to-double" "java.lang.Long"      "java.lang.Double"
  ^:private analyse-jvm-long-to-float "long-to-float" "java.lang.Long"      "java.lang.Float"
  ^:private analyse-jvm-long-to-int "long-to-int" "java.lang.Long"      "java.lang.Integer"
  ^:private analyse-jvm-long-to-short "long-to-short" "java.lang.Long"      "java.lang.Short"
  ^:private analyse-jvm-long-to-byte "long-to-byte" "java.lang.Long"      "java.lang.Byte"

  ^:private analyse-jvm-char-to-byte "char-to-byte" "java.lang.Character" "java.lang.Byte"
  ^:private analyse-jvm-char-to-short "char-to-short" "java.lang.Character" "java.lang.Short"
  ^:private analyse-jvm-char-to-int "char-to-int" "java.lang.Character" "java.lang.Integer"
  ^:private analyse-jvm-char-to-long "char-to-long" "java.lang.Character" "java.lang.Long"

  ^:private analyse-jvm-short-to-long "short-to-long" "java.lang.Short"     "java.lang.Long"

  ^:private analyse-jvm-byte-to-long "byte-to-long" "java.lang.Byte"      "java.lang.Long"
  )

(do-template [<name> <proc> <v1-class> <v2-class> <to-class>]
  (let [output-type (&/$Primitive <to-class> &/$Nil)]
    (defn <name> [analyse exo-type ?values]
      (|do [:let [(&/$Cons ?value1 (&/$Cons ?value2 (&/$Nil))) ?values]
            =value1 (&&/analyse-1 analyse (&/$Primitive <v1-class> &/$Nil) ?value1)
            =value2 (&&/analyse-1 analyse (&/$Primitive <v2-class> &/$Nil) ?value2)
            _ (&type/check exo-type output-type)
            _cursor &/cursor]
        (return (&/|list (&&/|meta output-type _cursor (&&/$proc (&/T ["jvm" <proc>]) (&/|list =value1 =value2) (&/|list))))))))

  ^:private analyse-jvm-iand  "iand"  "java.lang.Integer" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-ior   "ior"   "java.lang.Integer" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-ixor  "ixor"  "java.lang.Integer" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-ishl  "ishl"  "java.lang.Integer" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-ishr  "ishr"  "java.lang.Integer" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-iushr "iushr" "java.lang.Integer" "java.lang.Integer" "java.lang.Integer"

  ^:private analyse-jvm-land  "land"  "java.lang.Long"    "java.lang.Long"    "java.lang.Long"
  ^:private analyse-jvm-lor   "lor"   "java.lang.Long"    "java.lang.Long"    "java.lang.Long"
  ^:private analyse-jvm-lxor  "lxor"  "java.lang.Long"    "java.lang.Long"    "java.lang.Long"
  ^:private analyse-jvm-lshl  "lshl"  "java.lang.Long"    "java.lang.Integer" "java.lang.Long"
  ^:private analyse-jvm-lshr  "lshr"  "java.lang.Long"    "java.lang.Integer" "java.lang.Long"
  ^:private analyse-jvm-lushr "lushr" "java.lang.Long"    "java.lang.Integer" "java.lang.Long"
  )

(do-template [<name> <proc> <input-class> <output-class>]
  (let [input-type (&/$Primitive <input-class> &/$Nil)
        output-type (&/$Primitive <output-class> &/$Nil)]
    (defn <name> [analyse exo-type ?values]
      (|do [:let [(&/$Cons x (&/$Cons y (&/$Nil))) ?values]
            =x (&&/analyse-1 analyse input-type x)
            =y (&&/analyse-1 analyse input-type y)
            _ (&type/check exo-type output-type)
            _cursor &/cursor]
        (return (&/|list (&&/|meta output-type _cursor
                                   (&&/$proc (&/T ["jvm" <proc>]) (&/|list =x =y) (&/|list))))))))

  ^:private analyse-jvm-iadd "iadd" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-isub "isub" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-imul "imul" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-idiv "idiv" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-irem "irem" "java.lang.Integer" "java.lang.Integer"
  ^:private analyse-jvm-ieq  "ieq"  "java.lang.Integer" "java.lang.Boolean"
  ^:private analyse-jvm-ilt  "ilt"  "java.lang.Integer" "java.lang.Boolean"
  ^:private analyse-jvm-igt  "igt"  "java.lang.Integer" "java.lang.Boolean"

  ^:private analyse-jvm-ceq  "ceq"  "java.lang.Character" "java.lang.Boolean"
  ^:private analyse-jvm-clt  "clt"  "java.lang.Character" "java.lang.Boolean"
  ^:private analyse-jvm-cgt  "cgt"  "java.lang.Character" "java.lang.Boolean"

  ^:private analyse-jvm-ladd "ladd" "java.lang.Long"    "java.lang.Long"
  ^:private analyse-jvm-lsub "lsub" "java.lang.Long"    "java.lang.Long"
  ^:private analyse-jvm-lmul "lmul" "java.lang.Long"    "java.lang.Long"
  ^:private analyse-jvm-ldiv "ldiv" "java.lang.Long"    "java.lang.Long"
  ^:private analyse-jvm-lrem "lrem" "java.lang.Long"    "java.lang.Long"
  ^:private analyse-jvm-leq  "leq"  "java.lang.Long"    "java.lang.Boolean"
  ^:private analyse-jvm-llt  "llt"  "java.lang.Long"    "java.lang.Boolean"
  ^:private analyse-jvm-lgt  "lgt"  "java.lang.Long"    "java.lang.Boolean"

  ^:private analyse-jvm-fadd "fadd" "java.lang.Float"   "java.lang.Float"
  ^:private analyse-jvm-fsub "fsub" "java.lang.Float"   "java.lang.Float"
  ^:private analyse-jvm-fmul "fmul" "java.lang.Float"   "java.lang.Float"
  ^:private analyse-jvm-fdiv "fdiv" "java.lang.Float"   "java.lang.Float"
  ^:private analyse-jvm-frem "frem" "java.lang.Float"   "java.lang.Float"
  ^:private analyse-jvm-feq  "feq"  "java.lang.Float"   "java.lang.Boolean"
  ^:private analyse-jvm-flt  "flt"  "java.lang.Float"   "java.lang.Boolean"
  ^:private analyse-jvm-fgt  "fgt"  "java.lang.Float"   "java.lang.Boolean"

  ^:private analyse-jvm-dadd "dadd" "java.lang.Double"  "java.lang.Double"
  ^:private analyse-jvm-dsub "dsub" "java.lang.Double"  "java.lang.Double"
  ^:private analyse-jvm-dmul "dmul" "java.lang.Double"  "java.lang.Double"
  ^:private analyse-jvm-ddiv "ddiv" "java.lang.Double"  "java.lang.Double"
  ^:private analyse-jvm-drem "drem" "java.lang.Double"  "java.lang.Double"
  ^:private analyse-jvm-deq  "deq"  "java.lang.Double"  "java.lang.Boolean"
  ^:private analyse-jvm-dlt  "dlt"  "java.lang.Double"  "java.lang.Boolean"
  ^:private analyse-jvm-dgt  "dgt"  "java.lang.Double"  "java.lang.Boolean"
  )

(let [length-type &type/Nat
      idx-type &type/Nat]
  (do-template [<elem-class> <array-class> <new-name> <new-tag> <load-name> <load-tag> <store-name> <store-tag>]
    (let [elem-type (&/$Primitive <elem-class> &/$Nil)
          array-type (&/$Primitive <array-class> &/$Nil)]
      (defn <new-name> [analyse exo-type ?values]
        (|do [:let [(&/$Cons length (&/$Nil)) ?values]
              =length (&&/analyse-1 analyse length-type length)
              _ (&type/check exo-type array-type)
              _cursor &/cursor]
          (return (&/|list (&&/|meta exo-type _cursor
                                     (&&/$proc (&/T ["jvm" <new-tag>]) (&/|list =length) (&/|list)))))))

      (defn <load-name> [analyse exo-type ?values]
        (|do [:let [(&/$Cons array (&/$Cons idx (&/$Nil))) ?values]
              =array (&&/analyse-1 analyse array-type array)
              =idx (&&/analyse-1 analyse idx-type idx)
              _ (&type/check exo-type elem-type)
              _cursor &/cursor]
          (return (&/|list (&&/|meta exo-type _cursor
                                     (&&/$proc (&/T ["jvm" <load-tag>]) (&/|list =array =idx) (&/|list)))))))

      (defn <store-name> [analyse exo-type ?values]
        (|do [:let [(&/$Cons array (&/$Cons idx (&/$Cons elem (&/$Nil)))) ?values]
              =array (&&/analyse-1 analyse array-type array)
              =idx (&&/analyse-1 analyse idx-type idx)
              =elem (&&/analyse-1 analyse elem-type elem)
              _ (&type/check exo-type array-type)
              _cursor &/cursor]
          (return (&/|list (&&/|meta exo-type _cursor
                                     (&&/$proc (&/T ["jvm" <store-tag>]) (&/|list =array =idx =elem) (&/|list)))))))
      )

    "java.lang.Boolean"   "[Z" ^:private analyse-jvm-znewarray "znewarray" analyse-jvm-zaload "zaload" analyse-jvm-zastore "zastore"
    "java.lang.Byte"      "[B" ^:private analyse-jvm-bnewarray "bnewarray" analyse-jvm-baload "baload" analyse-jvm-bastore "bastore"
    "java.lang.Short"     "[S" ^:private analyse-jvm-snewarray "snewarray" analyse-jvm-saload "saload" analyse-jvm-sastore "sastore"
    "java.lang.Integer"   "[I" ^:private analyse-jvm-inewarray "inewarray" analyse-jvm-iaload "iaload" analyse-jvm-iastore "iastore"
    "java.lang.Long"      "[J" ^:private analyse-jvm-lnewarray "lnewarray" analyse-jvm-laload "laload" analyse-jvm-lastore "lastore"
    "java.lang.Float"     "[F" ^:private analyse-jvm-fnewarray "fnewarray" analyse-jvm-faload "faload" analyse-jvm-fastore "fastore"
    "java.lang.Double"    "[D" ^:private analyse-jvm-dnewarray "dnewarray" analyse-jvm-daload "daload" analyse-jvm-dastore "dastore"
    "java.lang.Character" "[C" ^:private analyse-jvm-cnewarray "cnewarray" analyse-jvm-caload "caload" analyse-jvm-castore "castore"
    ))

(defn ^:private array-class? [class-name]
  (or (= &host-type/array-data-tag class-name)
      (case class-name
        ("[Z" "[B" "[S" "[I" "[J" "[F" "[D" "[C") true
        ;; else
        false)))

(let [length-type &type/Nat
      idx-type &type/Nat]
  (defn ^:private analyse-jvm-anewarray [analyse exo-type ?values]
    (|do [:let [(&/$Cons [_ (&/$Text _gclass)] (&/$Cons length (&/$Nil))) ?values]
          gclass (&reader/with-source "jvm-anewarray" _gclass
                   &&a-parser/parse-gclass)
          gtype-env &/get-type-env
          =gclass (&host-type/instance-gtype &type/existential gtype-env gclass)
          :let [array-type (&/$Primitive &host-type/array-data-tag (&/|list =gclass))]
          =length (&&/analyse-1 analyse length-type length)
          _ (&type/check exo-type array-type)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["jvm" "anewarray"]) (&/|list =length) (&/|list gclass gtype-env)))))))

  (defn ^:private analyse-jvm-aaload [analyse exo-type ?values]
    (|do [:let [(&/$Cons array (&/$Cons idx (&/$Nil))) ?values]
          =array (&&/analyse-1+ analyse array)
          [arr-class arr-params] (ensure-object (&&/expr-type* =array))
          _ (&/assert! (= &host-type/array-data-tag arr-class) (str "[Analyser Error] Expected array. Instead got: " arr-class))
          :let [(&/$Cons inner-arr-type (&/$Nil)) arr-params]
          =idx (&&/analyse-1 analyse idx-type idx)
          _ (&type/check exo-type inner-arr-type)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["jvm" "aaload"]) (&/|list =array =idx) (&/|list)))))))

  (defn ^:private analyse-jvm-aastore [analyse exo-type ?values]
    (|do [:let [(&/$Cons array (&/$Cons idx (&/$Cons elem (&/$Nil)))) ?values]
          =array (&&/analyse-1+ analyse array)
          :let [array-type (&&/expr-type* =array)]
          [arr-class arr-params] (ensure-object array-type)
          _ (&/assert! (= &host-type/array-data-tag arr-class) (str "[Analyser Error] Expected array. Instead got: " arr-class))
          :let [(&/$Cons inner-arr-type (&/$Nil)) arr-params]
          =idx (&&/analyse-1 analyse idx-type idx)
          =elem (&&/analyse-1 analyse inner-arr-type elem)
          _ (&type/check exo-type array-type)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["jvm" "aastore"]) (&/|list =array =idx =elem) (&/|list))))))))

(defn ^:private analyse-jvm-arraylength [analyse exo-type ?values]
  (|do [:let [(&/$Cons array (&/$Nil)) ?values]
        =array (&&/analyse-1+ analyse array)
        [arr-class arr-params] (ensure-object (&&/expr-type* =array))
        _ (&/assert! (array-class? arr-class) (str "[Analyser Error] Expected array. Instead got: " arr-class))
        _ (&type/check exo-type &type/Nat)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "arraylength"]) (&/|list =array) (&/|list))
                               )))))

(defn ^:private analyse-jvm-object-null? [analyse exo-type ?values]
  (|do [:let [(&/$Cons object (&/$Nil)) ?values]
        =object (&&/analyse-1+ analyse object)
        _ (ensure-object (&&/expr-type* =object))
        :let [output-type &type/Bool]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "object null?"]) (&/|list =object) (&/|list)))))))

(defn ^:private analyse-jvm-object-null [analyse exo-type ?values]
  (|do [:let [(&/$Nil) ?values]
        :let [output-type (&/$Primitive &host-type/null-data-tag &/$Nil)]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "object null"]) (&/|list) (&/|list)))))))

(defn analyse-jvm-object-synchronized [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?monitor (&/$Cons ?expr (&/$Nil))) ?values]
        =monitor (&&/analyse-1+ analyse ?monitor)
        _ (ensure-object (&&/expr-type* =monitor))
        =expr (&&/analyse-1 analyse exo-type ?expr)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "object synchronized"]) (&/|list =monitor =expr) (&/|list)))))))

(defn ^:private analyse-jvm-throw [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?ex (&/$Nil)) ?values]
        =ex (&&/analyse-1+ analyse ?ex)
        _ (&type/check (&/$Primitive "java.lang.Throwable" &/$Nil) (&&/expr-type* =ex))
        [throw-class throw-params] (ensure-object (&&/expr-type* =ex))
        _cursor &/cursor
        _ (&type/check exo-type &type/Bottom)]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "throw"]) (&/|list =ex) (&/|list)))))))

(defn ^:private analyse-jvm-getstatic [analyse exo-type class field ?values]
  (|do [!class! (&/de-alias-class class)
        :let [(&/$Nil) ?values]
        class-loader &/loader
        [gvars gtype] (&host/lookup-static-field class-loader !class! field)
        =type (&host-type/instance-param &type/existential &/$Nil gtype)
        :let [output-type =type]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "getstatic"]) (&/|list) (&/|list class field output-type)))))))

(defn ^:private analyse-jvm-getfield [analyse exo-type class field ?values]
  (|do [!class! (&/de-alias-class class)
        :let [(&/$Cons object (&/$Nil)) ?values]
        class-loader &/loader
        =object (&&/analyse-1+ analyse object)
        _ (ensure-object (&&/expr-type* =object))
        [gvars gtype] (&host/lookup-field class-loader !class! field)
        =type (analyse-field-access-helper (&&/expr-type* =object) gvars gtype)
        :let [output-type =type]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "getfield"]) (&/|list =object) (&/|list class field output-type)))))))

(defn ^:private analyse-jvm-putstatic [analyse exo-type class field ?values]
  (|do [!class! (&/de-alias-class class)
        :let [(&/$Cons value (&/$Nil)) ?values]
        class-loader &/loader
        [gvars gtype] (&host/lookup-static-field class-loader !class! field)
        :let [gclass (&host-type/gtype->gclass gtype)]
        =type (&host-type/instance-param &type/existential &/$Nil gtype)
        =value (&&/analyse-1 analyse =type value)
        :let [output-type &/$Unit]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "putstatic"]) (&/|list =value) (&/|list class field gclass)))))))

(defn ^:private analyse-jvm-putfield [analyse exo-type class field ?values]
  (|do [!class! (&/de-alias-class class)
        :let [(&/$Cons object (&/$Cons value (&/$Nil))) ?values]
        class-loader &/loader
        =object (&&/analyse-1+ analyse object)
        :let [obj-type (&&/expr-type* =object)]
        _ (ensure-object obj-type)
        [gvars gtype] (&host/lookup-field class-loader !class! field)
        :let [gclass (&host-type/gtype->gclass gtype)]
        =type (analyse-field-access-helper obj-type gvars gtype)
        =value (&&/analyse-1 analyse =type value)
        :let [output-type &/$Unit]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "putfield"]) (&/|list =object =value) (&/|list class field gclass =type)))))))

(defn ^:private analyse-method-call-helper [analyse exo-type gret gtype-env gtype-vars gtype-args args]
  (|case gtype-vars
    (&/$Nil)
    (|do [arg-types (&/map% (partial &host-type/instance-param &type/existential gtype-env) gtype-args)
          =arg-types (&/map% &type/show-type+ arg-types)
          =args (&/map2% (partial &&/analyse-1 analyse) arg-types args)
          =gret (&host-type/instance-param &type/existential gtype-env gret)
          _ (&type/check exo-type (as-otype+ =gret))]
      (return (&/T [=gret =args])))
    
    (&/$Cons ^TypeVariable gtv gtype-vars*)
    (&type/with-var
      (fn [$var]
        (|do [:let [(&/$Var _id) $var
                    gtype-env* (&/$Cons (&/T [(.getName gtv) $var]) gtype-env)]
              [=gret =args] (analyse-method-call-helper analyse exo-type gret gtype-env* gtype-vars* gtype-args args)
              ==gret (&type/clean $var =gret)
              ==args (&/map% (partial &&/clean-analysis $var) =args)]
          (return (&/T [==gret ==args])))))
    ))

(let [dummy-type-param (&/$Primitive "java.lang.Object" &/$Nil)]
  (do-template [<name> <tag> <only-interface?>]
    (defn <name> [analyse exo-type class method classes ?values]
      (|do [!class! (&/de-alias-class class)
            :let [(&/$Cons object args) ?values]
            class-loader &/loader
            _ (try (assert! (let [=class (Class/forName !class! true class-loader)]
                              (= <only-interface?> (.isInterface =class)))
                            (if <only-interface?>
                              (str "[Analyser Error] Can only invoke method \"" method "\"" " on interface.")
                              (str "[Analyser Error] Can only invoke method \"" method "\"" " on class.")))
                (catch Exception e
                  (&/fail-with-loc (str "[Analyser Error] Unknown class: " class))))
            [gret exceptions parent-gvars gvars gargs] (if (= "<init>" method)
                                                         (return (&/T [Void/TYPE &/$Nil &/$Nil &/$Nil &/$Nil]))
                                                         (&host/lookup-virtual-method class-loader !class! method classes))
            =object (&&/analyse-1+ analyse object)
            [sub-class sub-params] (ensure-object (&&/expr-type* =object))
            (&/$Primitive super-class* super-params*) (&host-type/->super-type &type/existential class-loader !class! (if (= sub-class class)
                                                                                                                        !class!
                                                                                                                        sub-class)
                                                                               sub-params)
            :let [gtype-env (&/fold2 (fn [m ^TypeVariable g t] (&/$Cons (&/T [(.getName g) t]) m))
                                     (&/|table)
                                     parent-gvars
                                     super-params*)]
            [output-type =args] (analyse-method-call-helper analyse exo-type gret gtype-env gvars gargs args)
            _cursor &/cursor]
        (return (&/|list (&&/|meta exo-type _cursor
                                   (&&/$proc (&/T ["jvm" <tag>]) (&/$Cons =object =args) (&/|list class method classes output-type gret)))))))

    ^:private analyse-jvm-invokevirtual   "invokevirtual"   false
    ^:private analyse-jvm-invokespecial   "invokespecial"   false
    ^:private analyse-jvm-invokeinterface "invokeinterface" true
    ))

(defn ^:private analyse-jvm-invokestatic [analyse exo-type class method classes ?values]
  (|do [!class! (&/de-alias-class class)
        :let [args ?values]
        class-loader &/loader
        [gret exceptions parent-gvars gvars gargs] (&host/lookup-static-method class-loader !class! method classes)
        :let [gtype-env (&/|table)]
        [output-type =args] (analyse-method-call-helper analyse exo-type gret gtype-env gvars gargs args)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "invokestatic"]) =args (&/|list class method classes output-type gret)))))))

(defn ^:private analyse-jvm-new-helper [analyse gtype gtype-env gtype-vars gtype-args args]
  (|case gtype-vars
    (&/$Nil)
    (|do [arg-types (&/map% (partial &host-type/instance-param &type/existential gtype-env) gtype-args)
          =args (&/map2% (partial &&/analyse-1 analyse) arg-types args)
          gtype-vars* (->> gtype-env (&/|map &/|second) (clean-gtype-vars))]
      (return (&/T [(make-gtype gtype gtype-vars*)
                    =args])))
    
    (&/$Cons ^TypeVariable gtv gtype-vars*)
    (&type/with-var
      (fn [$var]
        (|do [:let [gtype-env* (&/$Cons (&/T [(.getName gtv) $var]) gtype-env)]
              [=gret =args] (analyse-jvm-new-helper analyse gtype gtype-env* gtype-vars* gtype-args args)
              ==gret (&type/clean $var =gret)
              ==args (&/map% (partial &&/clean-analysis $var) =args)]
          (return (&/T [==gret ==args])))))
    ))

(defn ^:private analyse-jvm-new [analyse exo-type class classes ?values]
  (|do [!class! (&/de-alias-class class)
        :let [args ?values]
        class-loader &/loader
        [exceptions gvars gargs] (&host/lookup-constructor class-loader !class! classes)
        [output-type =args] (analyse-jvm-new-helper analyse class (&/|table) gvars gargs args)
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "new"]) =args (&/|list class classes)))))))

(defn ^:private analyse-jvm-instanceof [analyse exo-type class ?values]
  (|do [:let [(&/$Cons object (&/$Nil)) ?values]
        =object (&&/analyse-1+ analyse object)
        _ (ensure-object (&&/expr-type* =object))
        :let [output-type &type/Bool]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&&/$proc (&/T ["jvm" "instanceof"]) (&/|list =object) (&/|list class)))))))

(defn ^:private analyse-jvm-object-class [analyse exo-type ?values]
  (|do [:let [(&/$Cons [_ (&/$Text _class-name)] (&/$Nil)) ?values]
        ^ClassLoader class-loader &/loader
        _ (try (do (.loadClass class-loader _class-name)
                 (return nil))
            (catch Exception e
              (&/fail-with-loc (str "[Analyser Error] Unknown class: " _class-name))))
        :let [output-type (&/$Primitive "java.lang.Class" (&/|list (&/$Primitive _class-name (&/|list))))]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&&/$proc (&/T ["jvm" "object class"]) (&/|list) (&/|list _class-name output-type)))))))

(defn ^:private analyse-jvm-interface [analyse compile-interface interface-decl supers =anns =methods]
  (|do [module &/get-module-name
        _ (compile-interface interface-decl supers =anns =methods)
        :let [_ (println 'INTERFACE (str module "." (&/|first interface-decl)))]
        _cursor &/cursor]
    (return (&/|list (&&/|meta &/$Unit _cursor
                               (&&/$tuple (&/|list)))))))

(defn ^:private analyse-jvm-class [analyse compile-class class-decl super-class interfaces =inheritance-modifier =anns ?fields methods]
  (&/with-closure
    (|do [module &/get-module-name
          :let [[?name ?params] class-decl
                full-name (str (string/replace module "/" ".") "." ?name)
                class-decl* (&/T [full-name ?params])
                all-supers (&/$Cons super-class interfaces)]
          class-env (make-type-env ?params)
          =fields (&/map% (partial analyse-field analyse class-env) ?fields)
          _ (&host/use-dummy-class class-decl super-class interfaces &/$None =fields methods)
          =methods (&/map% (partial analyse-method analyse class-decl* class-env all-supers) methods)
          _ (check-method-completion all-supers =methods)
          _ (compile-class class-decl super-class interfaces =inheritance-modifier =anns =fields =methods &/$Nil &/$None)
          _ &/pop-dummy-name
          :let [_ (println 'CLASS full-name)]
          _cursor &/cursor]
      (return (&/|list (&&/|meta &/$Unit _cursor
                                 (&&/$tuple (&/|list))))))))

(defn ^:private captured-source [env-entry]
  (|case env-entry
    [name [_ (&&/$captured _ _ source)]]
    source))

(let [default-<init> (&/$ConstructorMethodSyntax (&/T [&/$PublicPM
                                                       false
                                                       &/$Nil
                                                       &/$Nil
                                                       &/$Nil
                                                       &/$Nil
                                                       &/$Nil
                                                       (&/$Tuple &/$Nil)]))
      captured-slot-class "java.lang.Object"
      captured-slot-type (&/$GenericClass captured-slot-class &/$Nil)]
  (defn ^:private analyse-jvm-anon-class [analyse compile-class exo-type super-class interfaces ctor-args methods]
    (&/with-closure
      (|do [module &/get-module-name
            scope &/get-scope-name
            :let [name (->> scope &/|reverse &/|tail &host/location)
                  class-decl (&/T [name &/$Nil])
                  anon-class (str (string/replace module "/" ".") "." name)
                  anon-class-type (&/$Primitive anon-class &/$Nil)]
            =ctor-args (&/map% (fn [ctor-arg]
                                 (|let [[arg-type arg-term] ctor-arg]
                                   (|do [=arg-term (&&/analyse-1+ analyse arg-term)]
                                     (return (&/T [arg-type =arg-term])))))
                               ctor-args)
            _ (->> methods
                   (&/$Cons default-<init>)
                   (&host/use-dummy-class class-decl super-class interfaces (&/$Some =ctor-args) &/$Nil))
            :let [all-supers (&/$Cons super-class interfaces)
                  class-env &/$Nil]
            =methods (&/map% (partial analyse-method analyse class-decl class-env all-supers) methods)
            _ (check-method-completion all-supers =methods)
            =captured &&env/captured-vars
            :let [=fields (&/|map (fn [^objects idx+capt]
                                    (|let [[idx _] idx+capt]
                                      (&/$VariableFieldAnalysis (str &c!base/closure-prefix idx)
                                                                &/$PublicPM
                                                                &/$FinalSM
                                                                &/$Nil
                                                                captured-slot-type)))
                                  (&/enumerate =captured))]
            :let [sources (&/|map captured-source =captured)]
            _ (compile-class class-decl super-class interfaces &/$DefaultIM &/$Nil =fields =methods =captured (&/$Some =ctor-args))
            _ &/pop-dummy-name
            _cursor &/cursor]
        (return (&/|list (&&/|meta anon-class-type _cursor
                                   (&&/$proc (&/T ["jvm" "new"]) sources (&/|list anon-class (&/|repeat (&/|length sources) captured-slot-class)))
                                   )))
        ))))

(defn analyse-host [analyse exo-type compilers proc ?values]
  (|let [[_ _ _ compile-class compile-interface] compilers]
    (try (case proc
           "jvm object synchronized" (analyse-jvm-object-synchronized analyse exo-type ?values)
           "jvm object class"   (analyse-jvm-object-class analyse exo-type ?values)
           "jvm throw"        (analyse-jvm-throw analyse exo-type ?values)
           "jvm object null?"        (analyse-jvm-object-null? analyse exo-type ?values)
           "jvm object null"         (analyse-jvm-object-null analyse exo-type ?values)
           "jvm anewarray"    (analyse-jvm-anewarray analyse exo-type ?values)
           "jvm aaload"       (analyse-jvm-aaload analyse exo-type ?values)
           "jvm aastore"      (analyse-jvm-aastore analyse exo-type ?values)
           "jvm arraylength"  (analyse-jvm-arraylength analyse exo-type ?values)
           "jvm znewarray"    (analyse-jvm-znewarray analyse exo-type ?values)
           "jvm bnewarray"    (analyse-jvm-bnewarray analyse exo-type ?values)
           "jvm snewarray"    (analyse-jvm-snewarray analyse exo-type ?values)
           "jvm inewarray"    (analyse-jvm-inewarray analyse exo-type ?values)
           "jvm lnewarray"    (analyse-jvm-lnewarray analyse exo-type ?values)
           "jvm fnewarray"    (analyse-jvm-fnewarray analyse exo-type ?values)
           "jvm dnewarray"    (analyse-jvm-dnewarray analyse exo-type ?values)
           "jvm cnewarray"    (analyse-jvm-cnewarray analyse exo-type ?values)
           "jvm zaload" (analyse-jvm-zaload analyse exo-type ?values)
           "jvm zastore" (analyse-jvm-zastore analyse exo-type ?values)
           "jvm baload" (analyse-jvm-baload analyse exo-type ?values)
           "jvm bastore" (analyse-jvm-bastore analyse exo-type ?values)
           "jvm saload" (analyse-jvm-saload analyse exo-type ?values)
           "jvm sastore" (analyse-jvm-sastore analyse exo-type ?values)
           "jvm iaload" (analyse-jvm-iaload analyse exo-type ?values)
           "jvm iastore" (analyse-jvm-iastore analyse exo-type ?values)
           "jvm laload" (analyse-jvm-laload analyse exo-type ?values)
           "jvm lastore" (analyse-jvm-lastore analyse exo-type ?values)
           "jvm faload" (analyse-jvm-faload analyse exo-type ?values)
           "jvm fastore" (analyse-jvm-fastore analyse exo-type ?values)
           "jvm daload" (analyse-jvm-daload analyse exo-type ?values)
           "jvm dastore" (analyse-jvm-dastore analyse exo-type ?values)
           "jvm caload" (analyse-jvm-caload analyse exo-type ?values)
           "jvm castore" (analyse-jvm-castore analyse exo-type ?values)
           "jvm iadd"         (analyse-jvm-iadd analyse exo-type ?values)
           "jvm isub"         (analyse-jvm-isub analyse exo-type ?values)
           "jvm imul"         (analyse-jvm-imul analyse exo-type ?values)
           "jvm idiv"         (analyse-jvm-idiv analyse exo-type ?values)
           "jvm irem"         (analyse-jvm-irem analyse exo-type ?values)
           "jvm ieq"          (analyse-jvm-ieq analyse exo-type ?values)
           "jvm ilt"          (analyse-jvm-ilt analyse exo-type ?values)
           "jvm igt"          (analyse-jvm-igt analyse exo-type ?values)
           "jvm ceq"          (analyse-jvm-ceq analyse exo-type ?values)
           "jvm clt"          (analyse-jvm-clt analyse exo-type ?values)
           "jvm cgt"          (analyse-jvm-cgt analyse exo-type ?values)
           "jvm ladd"         (analyse-jvm-ladd analyse exo-type ?values)
           "jvm lsub"         (analyse-jvm-lsub analyse exo-type ?values)
           "jvm lmul"         (analyse-jvm-lmul analyse exo-type ?values)
           "jvm ldiv"         (analyse-jvm-ldiv analyse exo-type ?values)
           "jvm lrem"         (analyse-jvm-lrem analyse exo-type ?values)
           "jvm leq"          (analyse-jvm-leq analyse exo-type ?values)
           "jvm llt"          (analyse-jvm-llt analyse exo-type ?values)
           "jvm lgt"          (analyse-jvm-lgt analyse exo-type ?values)
           "jvm fadd"         (analyse-jvm-fadd analyse exo-type ?values)
           "jvm fsub"         (analyse-jvm-fsub analyse exo-type ?values)
           "jvm fmul"         (analyse-jvm-fmul analyse exo-type ?values)
           "jvm fdiv"         (analyse-jvm-fdiv analyse exo-type ?values)
           "jvm frem"         (analyse-jvm-frem analyse exo-type ?values)
           "jvm feq"          (analyse-jvm-feq analyse exo-type ?values)
           "jvm flt"          (analyse-jvm-flt analyse exo-type ?values)
           "jvm fgt"          (analyse-jvm-fgt analyse exo-type ?values)
           "jvm dadd"         (analyse-jvm-dadd analyse exo-type ?values)
           "jvm dsub"         (analyse-jvm-dsub analyse exo-type ?values)
           "jvm dmul"         (analyse-jvm-dmul analyse exo-type ?values)
           "jvm ddiv"         (analyse-jvm-ddiv analyse exo-type ?values)
           "jvm drem"         (analyse-jvm-drem analyse exo-type ?values)
           "jvm deq"          (analyse-jvm-deq analyse exo-type ?values)
           "jvm dlt"          (analyse-jvm-dlt analyse exo-type ?values)
           "jvm dgt"          (analyse-jvm-dgt analyse exo-type ?values)
           "jvm iand"         (analyse-jvm-iand analyse exo-type ?values)
           "jvm ior"          (analyse-jvm-ior analyse exo-type ?values)
           "jvm ixor"         (analyse-jvm-ixor analyse exo-type ?values)
           "jvm ishl"         (analyse-jvm-ishl analyse exo-type ?values)
           "jvm ishr"         (analyse-jvm-ishr analyse exo-type ?values)
           "jvm iushr"        (analyse-jvm-iushr analyse exo-type ?values)
           "jvm land"         (analyse-jvm-land analyse exo-type ?values)
           "jvm lor"          (analyse-jvm-lor analyse exo-type ?values)
           "jvm lxor"         (analyse-jvm-lxor analyse exo-type ?values)
           "jvm lshl"         (analyse-jvm-lshl analyse exo-type ?values)
           "jvm lshr"         (analyse-jvm-lshr analyse exo-type ?values)
           "jvm lushr"        (analyse-jvm-lushr analyse exo-type ?values)
           "jvm convert double-to-float"          (analyse-jvm-double-to-float analyse exo-type ?values)
           "jvm convert double-to-int"          (analyse-jvm-double-to-int analyse exo-type ?values)
           "jvm convert double-to-long"          (analyse-jvm-double-to-long analyse exo-type ?values)
           "jvm convert float-to-double"          (analyse-jvm-float-to-double analyse exo-type ?values)
           "jvm convert float-to-int"          (analyse-jvm-float-to-int analyse exo-type ?values)
           "jvm convert float-to-long"          (analyse-jvm-float-to-long analyse exo-type ?values)
           "jvm convert int-to-byte"          (analyse-jvm-int-to-byte analyse exo-type ?values)
           "jvm convert int-to-char"          (analyse-jvm-int-to-char analyse exo-type ?values)
           "jvm convert int-to-double"          (analyse-jvm-int-to-double analyse exo-type ?values)
           "jvm convert int-to-float"          (analyse-jvm-int-to-float analyse exo-type ?values)
           "jvm convert int-to-long"          (analyse-jvm-int-to-long analyse exo-type ?values)
           "jvm convert int-to-short"          (analyse-jvm-int-to-short analyse exo-type ?values)
           "jvm convert long-to-double"          (analyse-jvm-long-to-double analyse exo-type ?values)
           "jvm convert long-to-float"          (analyse-jvm-long-to-float analyse exo-type ?values)
           "jvm convert long-to-int"          (analyse-jvm-long-to-int analyse exo-type ?values)
           "jvm convert long-to-short"          (analyse-jvm-long-to-short analyse exo-type ?values)
           "jvm convert long-to-byte"          (analyse-jvm-long-to-byte analyse exo-type ?values)
           "jvm convert char-to-byte"          (analyse-jvm-char-to-byte analyse exo-type ?values)
           "jvm convert char-to-short"          (analyse-jvm-char-to-short analyse exo-type ?values)
           "jvm convert char-to-int"          (analyse-jvm-char-to-int analyse exo-type ?values)
           "jvm convert char-to-long"          (analyse-jvm-char-to-long analyse exo-type ?values)
           "jvm convert byte-to-long"          (analyse-jvm-byte-to-long analyse exo-type ?values)
           "jvm convert short-to-long"          (analyse-jvm-short-to-long analyse exo-type ?values)
           ;; else
           (->> (&/fail-with-loc (str "[Analyser Error] Unknown host procedure: " ["jvm" proc]))
                (if-let [[_ _def-code] (re-find #"^jvm interface:(.*)$" proc)]
                  (|do [[_module _line _column] &/cursor]
                    (&reader/with-source (str "interface@" "(" _module "," _line "," _column ")") _def-code
                      (|do [[=gclass-decl =supers =anns =methods] &&a-parser/parse-interface-def]
                        (analyse-jvm-interface analyse compile-interface =gclass-decl =supers =anns =methods)))))
                
                (if-let [[_ _def-code] (re-find #"^jvm class:(.*)$" proc)]
                  (|do [[_module _line _column] &/cursor]
                    (&reader/with-source (str "class@" "(" _module "," _line "," _column ")") _def-code
                      (|do [[=gclass-decl =super-class =interfaces =inheritance-modifier =anns =fields =methods] &&a-parser/parse-class-def]
                        (analyse-jvm-class analyse compile-class =gclass-decl =super-class =interfaces =inheritance-modifier =anns =fields =methods)))))
                
                (if-let [[_ _def-code] (re-find #"^jvm anon-class:(.*)$" proc)]
                  (|do [[_module _line _column] &/cursor]
                    (&reader/with-source (str "anon-class@" "(" _module "," _line "," _column ")") _def-code
                      (|do [[=super-class =interfaces =ctor-args =methods] &&a-parser/parse-anon-class-def]
                        (analyse-jvm-anon-class analyse compile-class exo-type =super-class =interfaces =ctor-args =methods)))))
                
                (if-let [[_ _class] (re-find #"^jvm instanceof:([^:]+)$" proc)]
                  (analyse-jvm-instanceof analyse exo-type _class ?values))
                
                (if-let [[_ _class _arg-classes] (re-find #"^jvm new:([^:]+):([^:]*)$" proc)]
                  (analyse-jvm-new analyse exo-type _class (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
                
                (if-let [[_ _class _method _arg-classes] (re-find #"^jvm invokestatic:([^:]+):([^:]+):([^:]*)$" proc)]
                  (analyse-jvm-invokestatic analyse exo-type _class _method (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
                
                (if-let [[_ _class _method _arg-classes] (re-find #"^jvm invokeinterface:([^:]+):([^:]+):([^:]*)$" proc)]
                  (analyse-jvm-invokeinterface analyse exo-type _class _method (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
                
                (if-let [[_ _class _method _arg-classes] (re-find #"^jvm invokevirtual:([^:]+):([^:]+):([^:]*)$" proc)]
                  (analyse-jvm-invokevirtual analyse exo-type _class _method (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
                
                (if-let [[_ _class _method _arg-classes] (re-find #"^jvm invokespecial:([^:]+):([^:]+):([^:]*)$" proc)]
                  (analyse-jvm-invokespecial analyse exo-type _class _method (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
                
                (if-let [[_ _class _field] (re-find #"^jvm getstatic:([^:]+):([^:]+)$" proc)]
                  (analyse-jvm-getstatic analyse exo-type _class _field ?values))
                
                (if-let [[_ _class _field] (re-find #"^jvm getfield:([^:]+):([^:]+)$" proc)]
                  (analyse-jvm-getfield analyse exo-type _class _field ?values))
                
                (if-let [[_ _class _field] (re-find #"^jvm putstatic:([^:]+):([^:]+)$" proc)]
                  (analyse-jvm-putstatic analyse exo-type _class _field ?values))
                
                (if-let [[_ _class _field] (re-find #"^jvm putfield:([^:]+):([^:]+)$" proc)]
                  (analyse-jvm-putfield analyse exo-type _class _field ?values))))
      (catch Exception ex
        (&/fail-with-loc (str "[Analyser Error] Invalid syntax for procedure: " proc))))
    ))
