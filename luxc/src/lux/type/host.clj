(ns lux.type.host
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return assert! |let |case]])
            [lux.host.generics :as &host-generics])
  (:import (java.lang.reflect GenericArrayType
                              ParameterizedType
                              TypeVariable
                              WildcardType)))

(defn ^:private type= [x y]
  (or (clojure.lang.Util/identical x y)
      (let [output (|case [x y]
                     [(&/$Named [?xmodule ?xname] ?xtype) (&/$Named [?ymodule ?yname] ?ytype)]
                     (and (= ?xmodule ?ymodule)
                          (= ?xname ?yname))

                     [(&/$Primitive xname xparams) (&/$Primitive yname yparams)]
                     (and (.equals ^Object xname yname)
                          (= (&/|length xparams) (&/|length yparams))
                          (&/fold2 #(and %1 (type= %2 %3)) true xparams yparams))

                     [(&/$Product xL xR) (&/$Product yL yR)]
                     (and (type= xL yL)
                          (type= xR yR))

                     [(&/$Sum xL xR) (&/$Sum yL yR)]
                     (and (type= xL yL)
                          (type= xR yR))

                     [(&/$Function xinput xoutput) (&/$Function yinput youtput)]
                     (and (type= xinput yinput)
                          (type= xoutput youtput))

                     [(&/$Var xid) (&/$Var yid)]
                     (= xid yid)

                     [(&/$Parameter xidx) (&/$Parameter yidx)]
                     (= xidx yidx)

                     [(&/$Ex xid) (&/$Ex yid)]
                     (= xid yid)

                     [(&/$Apply xparam xlambda) (&/$Apply yparam ylambda)]
                     (and (type= xparam yparam) (type= xlambda ylambda))
                     
                     [(&/$UnivQ xenv xbody) (&/$UnivQ yenv ybody)]
                     (type= xbody ybody)

                     [(&/$Named ?xname ?xtype) _]
                     (type= ?xtype y)

                     [_ (&/$Named ?yname ?ytype)]
                     (type= x ?ytype)
                     
                     [_ _]
                     false
                     )]
        output)))

(def ^:private Any
  (&/$Named (&/T ["lux" "Any"])
            (&/$ExQ (&/|list)
                    (&/$Parameter 1))))

;; [Exports]
(def array-data-tag "#Array")
(def null-data-tag "#Null")
(def i64-data-tag "#I64")
(def nat-data-tag "#Nat")
(def int-data-tag "#Int")
(def deg-data-tag "#Deg")

;; [Utils]
(defn ^:private trace-lineage* [^Class super-class ^Class sub-class]
  "(-> Class Class (List Class))"
  ;; Either they're both interfaces, of they're both classes
  (let [valid-sub? #(if (or (= super-class %)
                            (.isAssignableFrom super-class %))
                      %
                      nil)]
    (cond (.isInterface sub-class)
          (loop [sub-class sub-class
                 stack (&/|list)]
            (let [super-interface (some valid-sub? (.getInterfaces sub-class))]
              (if (= super-class super-interface)
                (&/$Cons super-interface stack)
                (recur super-interface (&/$Cons super-interface stack)))))

          (.isInterface super-class)
          (loop [sub-class sub-class
                 stack (&/|list)]
            (if-let [super-interface (some valid-sub? (.getInterfaces sub-class))]
              (if (= super-class super-interface)
                (&/$Cons super-interface stack)
                (recur super-interface (&/$Cons super-interface stack)))
              (let [super* (.getSuperclass sub-class)]
                (recur super* (&/$Cons super* stack)))))

          :else
          (loop [sub-class sub-class
                 stack (&/|list)]
            (let [super* (.getSuperclass sub-class)]
              (if (= super* super-class)
                (&/$Cons super* stack)
                (recur super* (&/$Cons super* stack))))))))

(defn ^:private trace-lineage [^Class sub-class ^Class super-class]
  "(-> Class Class (List Class))"
  (if (= sub-class super-class)
    (&/|list)
    (&/|reverse (trace-lineage* super-class sub-class))))

(let [matcher (fn [m ^TypeVariable jt lt] (&/$Cons (&/T [(.getName jt) lt]) m))]
  (defn ^:private match-params [sub-type-params params]
    (assert (and (= (&/|length sub-type-params) (&/|length params))
                 (&/|every? (partial instance? TypeVariable) sub-type-params)))
    (&/fold2 matcher (&/|table) sub-type-params params)))

;; [Exports]
(let [class-name-re #"((\[+)L([^\s]+);|([^\s]+)|(\[+)([ZBSIJFDC]))"
      jprim->lprim (fn [prim]
                     (case prim
                       "Z" "boolean"
                       "B" "byte"
                       "S" "short"
                       "I" "int"
                       "J" "long"
                       "F" "float"
                       "D" "double"
                       "C" "char"))]
  (defn class->type [^Class class]
    "(-> Class Type)"
    (let [gclass-name (.getName class)]
      (case gclass-name
        ("[Z" "[B" "[S" "[I" "[J" "[F" "[D" "[C")
        (&/$Primitive gclass-name (&/|list))
        ;; else
        (if-let [[_ _ arr-obrackets arr-obase simple-base arr-pbrackets arr-pbase] (re-find class-name-re gclass-name)]
          (let [base (or arr-obase simple-base (jprim->lprim arr-pbase))]
            (if (.equals "void" base)
              Any
              (reduce (fn [inner _] (&/$Primitive array-data-tag (&/|list inner)))
                      (&/$Primitive base (try (-> (Class/forName base) .getTypeParameters
                                                  seq count (repeat (&/$Primitive "java.lang.Object" &/$Nil))
                                                  &/->list)
                                           (catch Exception e
                                             (&/|list))))
                      (range (count (or arr-obrackets arr-pbrackets "")))))
            ))))))

(defn instance-param [existential matchings refl-type]
  "(-> (Lux Type) (List (, Text Type)) (^ java.lang.reflect.Type) (Lux Type))"
  (cond (instance? Class refl-type)
        (return (class->type refl-type))

        (instance? GenericArrayType refl-type)
        (|do [inner-type (instance-param existential matchings (.getGenericComponentType ^GenericArrayType refl-type))]
          (return (&/$Primitive array-data-tag (&/|list inner-type))))
        
        (instance? ParameterizedType refl-type)
        (|do [:let [refl-type* ^ParameterizedType refl-type]
              params* (->> refl-type*
                           .getActualTypeArguments
                           seq &/->list
                           (&/map% (partial instance-param existential matchings)))]
          (return (&/$Primitive (->> refl-type* ^Class (.getRawType) .getName)
                                params*)))
        
        (instance? TypeVariable refl-type)
        (let [gvar (.getName ^TypeVariable refl-type)]
          (if-let [m-type (&/|get gvar matchings)]
            (return m-type)
            (&/fail-with-loc (str "[Host Error] Unknown generic type-variable: " gvar "\n"
                                  "Available type-variables: " (->> matchings
                                                                    (&/|map &/|first)
                                                                    &/->seq)))))
        
        (instance? WildcardType refl-type)
        (if-let [bound (->> ^WildcardType refl-type .getUpperBounds seq first)]
          (instance-param existential matchings bound)
          existential)))

(defn principal-class [refl-type]
  (cond (instance? Class refl-type)
        (let [class-type (class->type refl-type)]
          (if (type= Any class-type)
            "V"
            (|case class-type
              (&/$Primitive "#Array" (&/$Cons (&/$Primitive class-name _) (&/$Nil)))
              (str "[" (&host-generics/->type-signature class-name))

              (&/$Primitive class-name _)
              (&host-generics/->type-signature class-name))))

        (instance? GenericArrayType refl-type)
        (str "[" (principal-class (.getGenericComponentType ^GenericArrayType refl-type)))
        
        (instance? ParameterizedType refl-type)
        (&host-generics/->type-signature (->> ^ParameterizedType refl-type ^Class (.getRawType) .getName))
        
        (instance? TypeVariable refl-type)
        (if-let [bound (->> ^TypeVariable refl-type .getBounds seq first)]
          (principal-class bound)
          (&host-generics/->type-signature "java.lang.Object"))
        
        (instance? WildcardType refl-type)
        (if-let [bound (->> ^WildcardType refl-type .getUpperBounds seq first)]
          (principal-class bound)
          (&host-generics/->type-signature "java.lang.Object"))))

(defn instance-gtype [existential matchings gtype]
  "(-> (Lux Type) (List (, Text Type)) GenericType (Lux Type))"
  (|case gtype
    (&/$GenericArray component-type)
    (|do [inner-type (instance-gtype existential matchings component-type)]
      (return (&/$Primitive array-data-tag (&/|list inner-type))))
    
    (&/$GenericClass type-name type-params)
    ;; When referring to type-parameters during class or method
    ;; definition, a type-environment is set for storing the names
    ;; of such parameters.
    ;; When a "class" shows up with the name of one of those
    ;; parameters, it must be detected, and the bytecode class-name
    ;; must correspond to Object's.
    
    (if-let [m-type (&/|get type-name matchings)]
      (return m-type)
      (|do [params* (&/map% (partial instance-gtype existential matchings)
                            type-params)]
        (return (&/$Primitive type-name params*))))
    
    (&/$GenericTypeVar var-name)
    (if-let [m-type (&/|get var-name matchings)]
      (return m-type)
      (&/fail-with-loc (str "[Host Error] Unknown generic type-variable: " var-name "\n"
                            "Available type-variables: " (->> matchings
                                                              (&/|map &/|first)
                                                              &/->seq))))
    
    (&/$GenericWildcard)
    existential))

;; [Utils]
(defn ^:private translate-params [existential super-type-params sub-type-params params]
  "(-> (List (^ java.lang.reflect.Type)) (List (^ java.lang.reflect.Type)) (List Type) (Lux (List Type)))"
  (|let [matchings (match-params sub-type-params params)]
    (&/map% (partial instance-param existential matchings) super-type-params)))

(defn ^:private raise* [existential sub+params ^Class super]
  "(-> (, Class (List Type)) Class (Lux (, Class (List Type))))"
  (|let [[^Class sub params] sub+params]
    (if (.isInterface super)
      (|do [:let [super-params (->> sub
                                    .getGenericInterfaces
                                    (some #(if (= super (if (instance? Class %) % (.getRawType ^ParameterizedType %)))
                                             (if (instance? Class %)
                                               (&/|list)
                                               (->> ^ParameterizedType % .getActualTypeArguments seq &/->list))
                                             nil)))]
            params* (translate-params existential
                                      (or super-params (&/|list))
                                      (->> sub .getTypeParameters seq &/->list)
                                      params)]
        (return (&/T [super params*])))
      (let [super* (.getGenericSuperclass sub)]
        (cond (instance? Class super*)
              (return (&/T [super* (&/|list)]))

              (instance? ParameterizedType super*)
              (|do [params* (translate-params existential
                                              (->> ^ParameterizedType super* .getActualTypeArguments seq &/->list)
                                              (->> sub .getTypeParameters seq &/->list)
                                              params)]
                (return (&/T [super params*])))
              
              :else
              (assert false (prn-str super* (class super*) [sub super])))))))

(defn ^:private raise [existential lineage class params]
  "(-> (List Class) Class (List Type) (Lux (, Class (List Type))))"
  (&/fold% (partial raise* existential) (&/T [class params]) lineage))

;; [Exports]
(defn ->super-type [existential class-loader super-class sub-class sub-params]
  "(-> Text Text (List Type) (Lux Type))"
  (let [super-class+ (Class/forName super-class true class-loader)
        sub-class+ (Class/forName sub-class true class-loader)]
    (if (.isAssignableFrom super-class+ sub-class+)
      (let [lineage (trace-lineage sub-class+ super-class+)]
        (|do [[^Class sub-class* sub-params*] (raise existential lineage sub-class+ sub-params)]
          (return (&/$Primitive (.getName sub-class*) sub-params*))))
      (&/fail-with-loc (str "[Host Error] Classes do not have a subtyping relationship: " sub-class " </= " super-class)))))

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

(let [primitive-types #{"boolean" "byte" "short" "int" "long" "float" "double" "char"}]
  (defn primitive-type? [type-name]
    (contains? primitive-types type-name)))

(def ^:private lux-jvm-type-combos
  #{#{"java.lang.Boolean" "#Bool"}
    #{"java.lang.Long" i64-data-tag}
    #{"java.lang.Double" "#Frac"}
    #{"java.lang.String" "#Text"}})

(defn ^:private lux-type? [^String class-name]
  (.startsWith class-name "#"))

(defn check-host-types [check check-error fixpoints existential class-loader invariant?? expected actual]
  (|let [[^String e!name e!params] expected
         [^String a!name a!params] actual]
    (try (let [e!name (as-obj e!name)
               a!name (as-obj a!name)]
           (cond (= e!name a!name)
                 (if (= (&/|length e!params) (&/|length a!params))
                   (|do [_ (&/map2% check e!params a!params)]
                     (return fixpoints))
                   (check-error "" (&/$Primitive e!name e!params) (&/$Primitive a!name a!params)))

                 (or (lux-type? e!name)
                     (lux-type? a!name))
                 (if (or (= "java.lang.Object" e!name)
                         (contains? lux-jvm-type-combos #{e!name a!name})
                         (and (not (primitive-type? e!name))
                              (= null-data-tag a!name)))
                   (return fixpoints)
                   (check-error "" (&/$Primitive e!name e!params) (&/$Primitive a!name a!params)))

                 (not invariant??)
                 (|do [actual* (->super-type existential class-loader e!name a!name a!params)]
                   (check (&/$Primitive e!name e!params) actual*))

                 :else
                 (check-error "" (&/$Primitive e!name e!params) (&/$Primitive a!name a!params))))
      (catch Exception e
        (throw e)))))

(defn gtype->gclass [gtype]
  "(-> GenericType GenericClass)"
  (cond (instance? Class gtype)
        (&/$GenericClass (.getName ^Class gtype) &/$Nil)

        (instance? GenericArrayType gtype)
        (&/$GenericArray (gtype->gclass (.getGenericComponentType ^GenericArrayType gtype)))

        (instance? ParameterizedType gtype)
        (let [type-name (->> ^ParameterizedType gtype ^Class (.getRawType) .getName)
              type-params (->> ^ParameterizedType gtype
                               .getActualTypeArguments
                               seq &/->list
                               (&/|map gtype->gclass))]
          (&/$GenericClass type-name type-params))

        (instance? TypeVariable gtype)
        (&/$GenericTypeVar (.getName ^TypeVariable gtype))

        (instance? WildcardType gtype)
        (if-let [bound (->> ^WildcardType gtype .getUpperBounds seq first)]
          (&/$GenericWildcard (&/$Some (&/T &/$UpperBound (gtype->gclass bound))))
          (if-let [bound (->> ^WildcardType gtype .getLowerBounds seq first)]
            (&/$GenericWildcard (&/$Some (&/T &/$LowerBound (gtype->gclass bound))))
            (&/$GenericWildcard &/$None)))))

(let [generic-type-sig "Ljava/lang/Object;"]
  (defn gclass->sig [gclass]
    "(-> GenericClass Text)"
    (|case gclass
      (&/$GenericClass gclass-name (&/$Nil))
      (case gclass-name
        "void"    "V"
        "boolean" "Z"
        "byte"    "B"
        "short"   "S"
        "int"     "I"
        "long"    "J"
        "float"   "F"
        "double"  "D"
        "char"    "C"
        ("[Z" "[B" "[S" "[I" "[J" "[F" "[D" "[C") gclass-name
        ;; else
        (str "L" (clojure.string/replace gclass-name #"\." "/") ";"))

      (&/$GenericArray inner-gtype)
      (str "[" (gclass->sig inner-gtype))

      (&/$GenericTypeVar ?vname)
      generic-type-sig

      (&/$GenericWildcard _)
      generic-type-sig
      )))
