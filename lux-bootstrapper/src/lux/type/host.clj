;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

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

                     [(&/$Nominal xname xparams) (&/$Nominal yname yparams)]
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

                     [(&/$Opaque xid) (&/$Opaque yid)]
                     (= xid yid)

                     [(&/$Apply xparam xlambda) (&/$Apply yparam ylambda)]
                     (and (type= xparam yparam) (type= xlambda ylambda))
                     
                     [(&/$Universal xenv xbody) (&/$Universal yenv ybody)]
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
  (&/$Named (&/T [&/prelude "Any"])
            (&/$Existential (&/|list)
                            (&/$Parameter 1))))

;; [Exports]
(def mutable-data-tag "#Mutable")
(def array-data-tag "#Array")
(defn Array [item]
  (&/$Nominal array-data-tag (&/|list (&/$Nominal mutable-data-tag (&/|list (&/$Function item item))))))

(def null-data-tag "#Null")
(def i64-data-tag "#I64")
(def nat-data-tag "#Nat")
(def int-data-tag "#Int")
(def rev-data-tag "#Rev")

;; [Utils]
(defn ^:private trace-lineage*
  "(-> Class Class (List Class))"
  [^Class super-class ^Class sub-class]
  ;; Either they're both interfaces, or they're both classes
  (let [valid-sub? #(if (or (= super-class %)
                            (.isAssignableFrom super-class %))
                      %
                      nil)]
    (if (or (.isInterface sub-class)
            (.isInterface super-class))
      (loop [sub-class sub-class
             stack (&/|list)]
        (if-let [super-interface (some valid-sub? (.getInterfaces sub-class))]
          (if (= super-class super-interface)
            (&/$Item super-interface stack)
            (recur super-interface (&/$Item super-interface stack)))
          (if-let [super* (.getSuperclass sub-class)]
            (recur super* (&/$Item super* stack))
            stack)))
      (loop [sub-class sub-class
             stack (&/|list)]
        (let [super* (.getSuperclass sub-class)]
          (if (= super* super-class)
            (&/$Item super* stack)
            (recur super* (&/$Item super* stack))))))))

(defn ^:private trace-lineage
  "(-> Class Class (List Class))"
  [^Class sub-class ^Class super-class]
  (if (= sub-class super-class)
    (&/|list)
    (&/|reverse (trace-lineage* super-class sub-class))))

(let [matcher (fn [m ^TypeVariable jt lt] (&/$Item (&/T [(.getName jt) lt]) m))]
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
  (defn class->type
    "(-> Class Type)"
    [^Class class]
    (let [gclass-name (.getName class)]
      (case gclass-name
        ("[Z" "[B" "[S" "[I" "[J" "[F" "[D" "[C")
        (&/$Nominal gclass-name (&/|list))
        ;; else
        (if-let [[_ _ arr-obrackets arr-obase simple-base arr-pbrackets arr-pbase] (re-find class-name-re gclass-name)]
          (let [base (or arr-obase simple-base (jprim->lprim arr-pbase))]
            (if (.equals "void" base)
              Any
              (reduce (fn [inner _] (Array inner))
                      (&/$Nominal base (try (-> (Class/forName base) .getTypeParameters
                                                seq count (repeat (&/$Nominal "java.lang.Object" &/$End))
                                                &/->list)
                                         (catch Exception e
                                           (&/|list))))
                      (range (count (or arr-obrackets arr-pbrackets "")))))
            ))))))

(defn instance-param
  "(-> (Lux Type) (List (, Text Type)) (^ java.lang.reflect.Type) (Lux Type))"
  [existential matchings refl-type]
  (cond (instance? Class refl-type)
        (return (class->type refl-type))

        (instance? GenericArrayType refl-type)
        (|do [inner-type (instance-param existential matchings (.getGenericComponentType ^GenericArrayType refl-type))]
          (return (Array inner-type)))
        
        (instance? ParameterizedType refl-type)
        (|do [:let [refl-type* ^ParameterizedType refl-type]
              params* (->> refl-type*
                           .getActualTypeArguments
                           seq &/->list
                           (&/map% (partial instance-param existential matchings)))]
          (return (&/$Nominal (->> refl-type* ^Class (.getRawType) .getName)
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
              (&/$Nominal "#Array"
                          (&/$Item (&/$Nominal "#Mutable"
                                               (&/$Item (&/$Function _ (&/$Nominal class-name _))
                                                        (&/$End)))
                                   (&/$End)))
              (str "[" (&host-generics/->type-signature class-name))

              (&/$Nominal class-name _)
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

(defn instance-gtype
  "(-> (Lux Type) (List (, Text Type)) GenericType (Lux Type))"
  [existential matchings gtype]
  (|case gtype
    (&/$GenericArray component-type)
    (|do [inner-type (instance-gtype existential matchings component-type)]
      (return (Array inner-type)))
    
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
        (return (&/$Nominal type-name params*))))
    
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
(defn ^:private translate-params
  "(-> (List (^ java.lang.reflect.Type)) (List (^ java.lang.reflect.Type)) (List Type) (Lux (List Type)))"
  [existential super-type-params sub-type-params params]
  (|let [matchings (match-params sub-type-params params)]
    (&/map% (partial instance-param existential matchings) super-type-params)))

(defn ^:private raise*
  "(-> (, Class (List Type)) Class (Lux (, Class (List Type))))"
  [existential sub+params ^Class super]
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

(defn- raise
  "(-> (List Class) Class (List Type) (Lux (, Class (List Type))))"
  [existential lineage class params]
  (&/fold% (partial raise* existential) (&/T [class params]) lineage))

;; [Exports]
(defn find-class! [class class-loader]
  (try (return (Class/forName class true class-loader))
    (catch java.lang.ClassNotFoundException ex
      (&/fail-with-loc (str "[Host Error] Cannot find class: " (pr-str class))))))

(defn ->super-type
  "(-> Text Text (List Type) (Lux Type))"
  [existential class-loader super-class sub-class sub-params]
  (|do [^Class super-class+ (find-class! super-class class-loader)
        ^Class sub-class+ (find-class! sub-class class-loader)]
    (if (.isAssignableFrom super-class+ sub-class+)
      (let [lineage (trace-lineage sub-class+ super-class+)]
        (|do [[^Class sub-class* sub-params*] (raise existential lineage sub-class+ sub-params)]
          (return (&/$Nominal (.getName sub-class*) sub-params*))))
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
  #{#{"java.lang.Boolean" "#Bit"}
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
                   (check-error "" (&/$Nominal e!name e!params) (&/$Nominal a!name a!params)))

                 (or (lux-type? e!name)
                     (lux-type? a!name))
                 (if (or (= "java.lang.Object" e!name)
                         (contains? lux-jvm-type-combos #{e!name a!name})
                         (and (not (primitive-type? e!name))
                              (= null-data-tag a!name)))
                   (return fixpoints)
                   (check-error "" (&/$Nominal e!name e!params) (&/$Nominal a!name a!params)))

                 (not invariant??)
                 (|do [actual* (->super-type existential class-loader e!name a!name a!params)]
                   (check (&/$Nominal e!name e!params) actual*))

                 :else
                 (check-error "" (&/$Nominal e!name e!params) (&/$Nominal a!name a!params))))
      (catch Exception e
        (throw e)))))

(defn gtype->gclass
  "(-> GenericType GenericClass)"
  [gtype]
  (cond (instance? Class gtype)
        (&/$GenericClass (.getName ^Class gtype) &/$End)

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
  (defn gclass->sig
    "(-> GenericClass Text)"
    [gclass]
    (|case gclass
      (&/$GenericClass gclass-name (&/$End))
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
