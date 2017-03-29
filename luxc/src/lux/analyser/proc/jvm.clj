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
                          [lambda :as &&lambda]
                          [env :as &&env]
                          [parser :as &&a-parser])
            [lux.compiler.jvm.base :as &c!base])
  (:import (java.lang.reflect Type TypeVariable)))

;; [Utils]
(defn ^:private ensure-object [type]
  "(-> Type (Lux (, Text (List Type))))"
  (|case type
    (&/$HostT payload)
    (return payload)

    (&/$VarT id)
    (return (&/T ["java.lang.Object" (&/|list)]))

    (&/$ExT id)
    (return (&/T ["java.lang.Object" (&/|list)]))

    (&/$NamedT _ type*)
    (ensure-object type*)

    (&/$UnivQ _ type*)
    (ensure-object type*)

    (&/$ExQ _ type*)
    (ensure-object type*)

    (&/$AppT F A)
    (|do [type* (&type/apply-type F A)]
      (ensure-object type*))

    _
    (&/fail-with-loc (str "[Analyser Error] Expecting object: " (&type/show-type type)))))

(defn ^:private as-object [type]
  "(-> Type Type)"
  (|case type
    (&/$HostT class params)
    (&/$HostT (&host-type/as-obj class) params)

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
    (&/$HostT name params)
    (&/$HostT (as-otype name) params)

    _
    type))

(defn ^:private clean-gtype-var [idx gtype-var]
  (|let [(&/$VarT id) gtype-var]
    (|do [? (&type/bound? id)]
      (if ?
        (|do [real-type (&type/deref id)]
          (return (&/T [idx real-type])))
        (return (&/T [(+ 2 idx) (&/$BoundT idx)]))))))

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
              (&/$BoundT _)
              (&/$UnivQ &type/empty-env base-type)
              
              _
              base-type))
          (&/$HostT class-name type-args)
          type-args))

;; [Resources]
(defn ^:private analyse-field-access-helper [obj-type gvars gtype]
  "(-> Type (List (^ java.lang.reflect.Type)) (^ java.lang.reflect.Type) (Lux Type))"
  (|case obj-type
    (&/$HostT class targs)
    (if (= (&/|length targs) (&/|length gvars))
      (|let [gtype-env (&/fold2 (fn [m ^TypeVariable g t] (&/$Cons (&/T [(.getName g) t]) m))
                                (&/|table)
                                gvars
                                targs)]
        (&host-type/instance-param &type/existential gtype-env gtype))
      (&/fail-with-loc (str "[Type Error] Mismatched number of type-parameters: " (&/|length gvars) " - " (&type/show-type obj-type))))

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
      (&/fail-with-loc (str "[Analysis Error] Unknown type var: " var-name)))
    
    (&/$GenericClass name params)
    (case name
      "boolean" (return (&/$HostT "java.lang.Boolean" &/$Nil))
      "byte"    (return (&/$HostT "java.lang.Byte" &/$Nil))
      "short"   (return (&/$HostT "java.lang.Short" &/$Nil))
      "int"     (return (&/$HostT "java.lang.Integer" &/$Nil))
      "long"    (return (&/$HostT "java.lang.Long" &/$Nil))
      "float"   (return (&/$HostT "java.lang.Float" &/$Nil))
      "double"  (return (&/$HostT "java.lang.Double" &/$Nil))
      "char"    (return (&/$HostT "java.lang.Character" &/$Nil))
      "void"    (return &/$UnitT)
      ;; else
      (|do [=params (&/map% (partial generic-class->type env) params)]
        (return (&/$HostT name =params))))

    (&/$GenericArray param)
    (|do [=param (generic-class->type env param)]
      (return (&/$HostT &host-type/array-data-tag (&/|list =param))))

    (&/$GenericWildcard _)
    (return (&/$ExQ &/$Nil (&/$BoundT 1)))
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
          (&&env/with-local "" &/$VoidT
            body*))
        (&&env/with-local iname itype
          body*)))))

(defn ^:private analyse-method [analyse class-decl class-env all-supers method]
  "(-> Analyser ClassDecl (List (, TypeVar Type)) (List SuperClassDecl) MethodSyntax (Lux MethodAnalysis))"
  (|let [[?cname ?cparams] class-decl
         class-type (&/$HostT ?cname (&/|map &/|second class-env))]
    (|case method
      (&/$ConstructorMethodSyntax =privacy-modifier ?strict ?anns ?gvars ?exceptions ?inputs ?ctor-args ?body)
      (|do [method-env (make-type-env ?gvars)
            :let [full-env (&/|++ class-env method-env)]
            :let [output-type &/$UnitT]
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
  (let [output-type (&/$HostT <to-class> &/$Nil)]
    (defn <name> [analyse exo-type _?value]
      (|do [:let [(&/$Cons ?value (&/$Nil)) _?value]
            =value (&&/analyse-1 analyse (&/$HostT <from-class> &/$Nil) ?value)
            _ (&type/check exo-type output-type)
            _cursor &/cursor]
        (return (&/|list (&&/|meta output-type _cursor (&&/$proc (&/T ["jvm" <proc>]) (&/|list =value) (&/|list))))))))

  ^:private analyse-jvm-d2f "d2f" "java.lang.Double"    "java.lang.Float"
  ^:private analyse-jvm-d2i "d2i" "java.lang.Double"    "java.lang.Integer"
  ^:private analyse-jvm-d2l "d2l" "java.lang.Double"    "java.lang.Long"

  ^:private analyse-jvm-f2d "f2d" "java.lang.Float"     "java.lang.Double"
  ^:private analyse-jvm-f2i "f2i" "java.lang.Float"     "java.lang.Integer"
  ^:private analyse-jvm-f2l "f2l" "java.lang.Float"     "java.lang.Long"

  ^:private analyse-jvm-i2b "i2b" "java.lang.Integer"   "java.lang.Byte"
  ^:private analyse-jvm-i2c "i2c" "java.lang.Integer"   "java.lang.Character"
  ^:private analyse-jvm-i2d "i2d" "java.lang.Integer"   "java.lang.Double"
  ^:private analyse-jvm-i2f "i2f" "java.lang.Integer"   "java.lang.Float"
  ^:private analyse-jvm-i2l "i2l" "java.lang.Integer"   "java.lang.Long"
  ^:private analyse-jvm-i2s "i2s" "java.lang.Integer"   "java.lang.Short"

  ^:private analyse-jvm-l2d "l2d" "java.lang.Long"      "java.lang.Double"
  ^:private analyse-jvm-l2f "l2f" "java.lang.Long"      "java.lang.Float"
  ^:private analyse-jvm-l2i "l2i" "java.lang.Long"      "java.lang.Integer"
  ^:private analyse-jvm-l2s "l2s" "java.lang.Long"      "java.lang.Short"
  ^:private analyse-jvm-l2b "l2b" "java.lang.Long"      "java.lang.Byte"

  ^:private analyse-jvm-c2b "c2b" "java.lang.Character" "java.lang.Byte"
  ^:private analyse-jvm-c2s "c2s" "java.lang.Character" "java.lang.Short"
  ^:private analyse-jvm-c2i "c2i" "java.lang.Character" "java.lang.Integer"
  ^:private analyse-jvm-c2l "c2l" "java.lang.Character" "java.lang.Long"

  ^:private analyse-jvm-s2l "s2l" "java.lang.Short"     "java.lang.Long"

  ^:private analyse-jvm-b2l "b2l" "java.lang.Byte"      "java.lang.Long"
  )

(do-template [<name> <proc> <v1-class> <v2-class> <to-class>]
  (let [output-type (&/$HostT <to-class> &/$Nil)]
    (defn <name> [analyse exo-type ?values]
      (|do [:let [(&/$Cons ?value1 (&/$Cons ?value2 (&/$Nil))) ?values]
            =value1 (&&/analyse-1 analyse (&/$HostT <v1-class> &/$Nil) ?value1)
            =value2 (&&/analyse-1 analyse (&/$HostT <v2-class> &/$Nil) ?value2)
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
  (let [input-type (&/$HostT <input-class> &/$Nil)
        output-type (&/$HostT <output-class> &/$Nil)]
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
    (let [elem-type (&/$HostT <elem-class> &/$Nil)
          array-type (&/$HostT <array-class> &/$Nil)]
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
    (|do [:let [(&/$Cons [_ (&/$TextS _gclass)] (&/$Cons length (&/$Nil))) ?values]
          gclass (&reader/with-source "jvm-anewarray" _gclass
                   &&a-parser/parse-gclass)
          gtype-env &/get-type-env
          =gclass (&host-type/instance-gtype &type/existential gtype-env gclass)
          :let [array-type (&/$HostT &host-type/array-data-tag (&/|list =gclass))]
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

(defn ^:private analyse-jvm-null? [analyse exo-type ?values]
  (|do [:let [(&/$Cons object (&/$Nil)) ?values]
        =object (&&/analyse-1+ analyse object)
        _ (ensure-object (&&/expr-type* =object))
        :let [output-type &type/Bool]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "null?"]) (&/|list =object) (&/|list)))))))

(defn ^:private analyse-jvm-null [analyse exo-type ?values]
  (|do [:let [(&/$Nil) ?values]
        :let [output-type (&/$HostT &host-type/null-data-tag &/$Nil)]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "null"]) (&/|list) (&/|list)))))))

(defn analyse-jvm-synchronized [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?monitor (&/$Cons ?expr (&/$Nil))) ?values]
        =monitor (&&/analyse-1+ analyse ?monitor)
        _ (ensure-object (&&/expr-type* =monitor))
        =expr (&&/analyse-1 analyse exo-type ?expr)
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["jvm" "synchronized"]) (&/|list =monitor =expr) (&/|list)))))))

(defn ^:private analyse-jvm-throw [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?ex (&/$Nil)) ?values]
        =ex (&&/analyse-1+ analyse ?ex)
        _ (&type/check (&/$HostT "java.lang.Throwable" &/$Nil) (&&/expr-type* =ex))
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
        :let [output-type &/$UnitT]
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
        :let [output-type &/$UnitT]
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
        (|do [:let [(&/$VarT _id) $var
                    gtype-env* (&/$Cons (&/T [(.getName gtv) $var]) gtype-env)]
              [=gret =args] (analyse-method-call-helper analyse exo-type gret gtype-env* gtype-vars* gtype-args args)
              ==gret (&type/clean $var =gret)
              ==args (&/map% (partial &&/clean-analysis $var) =args)]
          (return (&/T [==gret ==args])))))
    ))

(let [dummy-type-param (&/$HostT "java.lang.Object" &/$Nil)]
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
            (&/$HostT super-class* super-params*) (&host-type/->super-type &type/existential class-loader !class! (if (= sub-class class)
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

(defn ^:private analyse-jvm-load-class [analyse exo-type ?values]
  (|do [:let [(&/$Cons [_ (&/$TextS _class-name)] (&/$Nil)) ?values]
        ^ClassLoader class-loader &/loader
        _ (try (do (.loadClass class-loader _class-name)
                 (return nil))
            (catch Exception e
              (&/fail-with-loc (str "[Analyser Error] Unknown class: " _class-name))))
        :let [output-type (&/$HostT "java.lang.Class" (&/|list (&/$HostT _class-name (&/|list))))]
        _ (&type/check exo-type output-type)
        _cursor &/cursor]
    (return (&/|list (&&/|meta output-type _cursor
                               (&&/$proc (&/T ["jvm" "load-class"]) (&/|list) (&/|list _class-name output-type)))))))

(defn ^:private analyse-jvm-interface [analyse compile-interface interface-decl supers =anns =methods]
  (|do [module &/get-module-name
        _ (compile-interface interface-decl supers =anns =methods)
        :let [_ (println 'INTERFACE (str module "." (&/|first interface-decl)))]
        _cursor &/cursor]
    (return (&/|list (&&/|meta &/$UnitT _cursor
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
      (return (&/|list (&&/|meta &/$UnitT _cursor
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
                                                       (&/$TupleS &/$Nil)]))
      captured-slot-class "java.lang.Object"
      captured-slot-type (&/$GenericClass captured-slot-class &/$Nil)]
  (defn ^:private analyse-jvm-anon-class [analyse compile-class exo-type super-class interfaces ctor-args methods]
    (&/with-closure
      (|do [module &/get-module-name
            scope &/get-scope-name
            :let [name (->> scope &/|reverse &/|tail &host/location)
                  class-decl (&/T [name &/$Nil])
                  anon-class (str (string/replace module "/" ".") "." name)
                  anon-class-type (&/$HostT anon-class &/$Nil)]
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
    (case proc
      "synchronized" (analyse-jvm-synchronized analyse exo-type ?values)
      "load-class"   (analyse-jvm-load-class analyse exo-type ?values)
      "throw"        (analyse-jvm-throw analyse exo-type ?values)
      "null?"        (analyse-jvm-null? analyse exo-type ?values)
      "null"         (analyse-jvm-null analyse exo-type ?values)
      "anewarray"    (analyse-jvm-anewarray analyse exo-type ?values)
      "aaload"       (analyse-jvm-aaload analyse exo-type ?values)
      "aastore"      (analyse-jvm-aastore analyse exo-type ?values)
      "arraylength"  (analyse-jvm-arraylength analyse exo-type ?values)
      "znewarray"    (analyse-jvm-znewarray analyse exo-type ?values)
      "bnewarray"    (analyse-jvm-bnewarray analyse exo-type ?values)
      "snewarray"    (analyse-jvm-snewarray analyse exo-type ?values)
      "inewarray"    (analyse-jvm-inewarray analyse exo-type ?values)
      "lnewarray"    (analyse-jvm-lnewarray analyse exo-type ?values)
      "fnewarray"    (analyse-jvm-fnewarray analyse exo-type ?values)
      "dnewarray"    (analyse-jvm-dnewarray analyse exo-type ?values)
      "cnewarray"    (analyse-jvm-cnewarray analyse exo-type ?values)
      "iadd"         (analyse-jvm-iadd analyse exo-type ?values)
      "isub"         (analyse-jvm-isub analyse exo-type ?values)
      "imul"         (analyse-jvm-imul analyse exo-type ?values)
      "idiv"         (analyse-jvm-idiv analyse exo-type ?values)
      "irem"         (analyse-jvm-irem analyse exo-type ?values)
      "ieq"          (analyse-jvm-ieq analyse exo-type ?values)
      "ilt"          (analyse-jvm-ilt analyse exo-type ?values)
      "igt"          (analyse-jvm-igt analyse exo-type ?values)
      "ceq"          (analyse-jvm-ceq analyse exo-type ?values)
      "clt"          (analyse-jvm-clt analyse exo-type ?values)
      "cgt"          (analyse-jvm-cgt analyse exo-type ?values)
      "ladd"         (analyse-jvm-ladd analyse exo-type ?values)
      "lsub"         (analyse-jvm-lsub analyse exo-type ?values)
      "lmul"         (analyse-jvm-lmul analyse exo-type ?values)
      "ldiv"         (analyse-jvm-ldiv analyse exo-type ?values)
      "lrem"         (analyse-jvm-lrem analyse exo-type ?values)
      "leq"          (analyse-jvm-leq analyse exo-type ?values)
      "llt"          (analyse-jvm-llt analyse exo-type ?values)
      "lgt"          (analyse-jvm-lgt analyse exo-type ?values)
      "fadd"         (analyse-jvm-fadd analyse exo-type ?values)
      "fsub"         (analyse-jvm-fsub analyse exo-type ?values)
      "fmul"         (analyse-jvm-fmul analyse exo-type ?values)
      "fdiv"         (analyse-jvm-fdiv analyse exo-type ?values)
      "frem"         (analyse-jvm-frem analyse exo-type ?values)
      "feq"          (analyse-jvm-feq analyse exo-type ?values)
      "flt"          (analyse-jvm-flt analyse exo-type ?values)
      "fgt"          (analyse-jvm-fgt analyse exo-type ?values)
      "dadd"         (analyse-jvm-dadd analyse exo-type ?values)
      "dsub"         (analyse-jvm-dsub analyse exo-type ?values)
      "dmul"         (analyse-jvm-dmul analyse exo-type ?values)
      "ddiv"         (analyse-jvm-ddiv analyse exo-type ?values)
      "drem"         (analyse-jvm-drem analyse exo-type ?values)
      "deq"          (analyse-jvm-deq analyse exo-type ?values)
      "dlt"          (analyse-jvm-dlt analyse exo-type ?values)
      "dgt"          (analyse-jvm-dgt analyse exo-type ?values)
      "iand"         (analyse-jvm-iand analyse exo-type ?values)
      "ior"          (analyse-jvm-ior analyse exo-type ?values)
      "ixor"         (analyse-jvm-ixor analyse exo-type ?values)
      "ishl"         (analyse-jvm-ishl analyse exo-type ?values)
      "ishr"         (analyse-jvm-ishr analyse exo-type ?values)
      "iushr"        (analyse-jvm-iushr analyse exo-type ?values)
      "land"         (analyse-jvm-land analyse exo-type ?values)
      "lor"          (analyse-jvm-lor analyse exo-type ?values)
      "lxor"         (analyse-jvm-lxor analyse exo-type ?values)
      "lshl"         (analyse-jvm-lshl analyse exo-type ?values)
      "lshr"         (analyse-jvm-lshr analyse exo-type ?values)
      "lushr"        (analyse-jvm-lushr analyse exo-type ?values)
      "d2f"          (analyse-jvm-d2f analyse exo-type ?values)
      "d2i"          (analyse-jvm-d2i analyse exo-type ?values)
      "d2l"          (analyse-jvm-d2l analyse exo-type ?values)
      "f2d"          (analyse-jvm-f2d analyse exo-type ?values)
      "f2i"          (analyse-jvm-f2i analyse exo-type ?values)
      "f2l"          (analyse-jvm-f2l analyse exo-type ?values)
      "i2b"          (analyse-jvm-i2b analyse exo-type ?values)
      "i2c"          (analyse-jvm-i2c analyse exo-type ?values)
      "i2d"          (analyse-jvm-i2d analyse exo-type ?values)
      "i2f"          (analyse-jvm-i2f analyse exo-type ?values)
      "i2l"          (analyse-jvm-i2l analyse exo-type ?values)
      "i2s"          (analyse-jvm-i2s analyse exo-type ?values)
      "l2d"          (analyse-jvm-l2d analyse exo-type ?values)
      "l2f"          (analyse-jvm-l2f analyse exo-type ?values)
      "l2i"          (analyse-jvm-l2i analyse exo-type ?values)
      "l2s"          (analyse-jvm-l2s analyse exo-type ?values)
      "l2b"          (analyse-jvm-l2b analyse exo-type ?values)
      "c2b"          (analyse-jvm-c2b analyse exo-type ?values)
      "c2s"          (analyse-jvm-c2s analyse exo-type ?values)
      "c2i"          (analyse-jvm-c2i analyse exo-type ?values)
      "c2l"          (analyse-jvm-c2l analyse exo-type ?values)
      "b2l"          (analyse-jvm-b2l analyse exo-type ?values)
      "s2l"          (analyse-jvm-s2l analyse exo-type ?values)
      ;; else
      (->> (&/fail-with-loc (str "[Analyser Error] Unknown JVM procedure: " proc))
           (if-let [[_ _def-code] (re-find #"^interface:(.*)$" proc)]
             (|do [[_module _line _column] &/cursor]
               (&reader/with-source (str "interface@" "(" _module "," _line "," _column ")") _def-code
                 (|do [[=gclass-decl =supers =anns =methods] &&a-parser/parse-interface-def]
                   (analyse-jvm-interface analyse compile-interface =gclass-decl =supers =anns =methods)))))
           
           (if-let [[_ _def-code] (re-find #"^class:(.*)$" proc)]
             (|do [[_module _line _column] &/cursor]
               (&reader/with-source (str "class@" "(" _module "," _line "," _column ")") _def-code
                 (|do [[=gclass-decl =super-class =interfaces =inheritance-modifier =anns =fields =methods] &&a-parser/parse-class-def]
                   (analyse-jvm-class analyse compile-class =gclass-decl =super-class =interfaces =inheritance-modifier =anns =fields =methods)))))
           
           (if-let [[_ _def-code] (re-find #"^anon-class:(.*)$" proc)]
             (|do [[_module _line _column] &/cursor]
               (&reader/with-source (str "anon-class@" "(" _module "," _line "," _column ")") _def-code
                 (|do [[=super-class =interfaces =ctor-args =methods] &&a-parser/parse-anon-class-def]
                   (analyse-jvm-anon-class analyse compile-class exo-type =super-class =interfaces =ctor-args =methods)))))
           
           (if-let [[_ _class] (re-find #"^instanceof:([^:]+)$" proc)]
             (analyse-jvm-instanceof analyse exo-type _class ?values))
           
           (if-let [[_ _class _arg-classes] (re-find #"^new:([^:]+):([^:]*)$" proc)]
             (analyse-jvm-new analyse exo-type _class (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
           
           (if-let [[_ _class _method _arg-classes] (re-find #"^invokestatic:([^:]+):([^:]+):([^:]*)$" proc)]
             (analyse-jvm-invokestatic analyse exo-type _class _method (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
           
           (if-let [[_ _class _method _arg-classes] (re-find #"^invokeinterface:([^:]+):([^:]+):([^:]*)$" proc)]
             (analyse-jvm-invokeinterface analyse exo-type _class _method (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
           
           (if-let [[_ _class _method _arg-classes] (re-find #"^invokevirtual:([^:]+):([^:]+):([^:]*)$" proc)]
             (analyse-jvm-invokevirtual analyse exo-type _class _method (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
           
           (if-let [[_ _class _method _arg-classes] (re-find #"^invokespecial:([^:]+):([^:]+):([^:]*)$" proc)]
             (analyse-jvm-invokespecial analyse exo-type _class _method (if (= "" _arg-classes) (&/|list) (&/->list (string/split _arg-classes #","))) ?values))
           
           (if-let [[_ _class _field] (re-find #"^getstatic:([^:]+):([^:]+)$" proc)]
             (analyse-jvm-getstatic analyse exo-type _class _field ?values))
           
           (if-let [[_ _class _field] (re-find #"^getfield:([^:]+):([^:]+)$" proc)]
             (analyse-jvm-getfield analyse exo-type _class _field ?values))
           
           (if-let [[_ _class _field] (re-find #"^putstatic:([^:]+):([^:]+)$" proc)]
             (analyse-jvm-putstatic analyse exo-type _class _field ?values))
           
           (if-let [[_ _class _field] (re-find #"^putfield:([^:]+):([^:]+)$" proc)]
             (analyse-jvm-putfield analyse exo-type _class _field ?values))))
    ))
