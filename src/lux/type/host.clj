;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.type.host
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* assert! |let |case]]))
  (:import (java.lang.reflect GenericArrayType
                              ParameterizedType
                              TypeVariable
                              WildcardType)))

;; [Exports]
(def array-data-tag "#Array")
(def null-data-tag "#Null")

;; [Utils]
(defn ^:private trace-lineage* [^Class super-class ^Class sub-class]
  "(-> Class Class (List Class))"
  ;; Either they're both interfaces, of they're both classes
  (cond (.isInterface sub-class)
        (let [interface<=interface? #(if (or (= super-class %)
                                             (.isAssignableFrom super-class %))
                                       %
                                       nil)]
          (loop [sub-class sub-class
                 stack (&/|list)]
            (let [super-interface (some interface<=interface?
                                        (.getInterfaces sub-class))]
              (if (= super-class super-interface)
                (&/Cons$ super-interface stack)
                (let [super* (.getSuperclass sub-class)]
                  (recur super* (&/Cons$ super* stack)))))))

        (.isInterface super-class)
        (let [class<=interface? #(if (= super-class %) % nil)]
          (loop [sub-class sub-class
                 stack (&/|list)]
            (if-let [super-interface (some class<=interface? (.getInterfaces sub-class))]
              (&/Cons$ super-interface stack)
              (let [super* (.getSuperclass sub-class)]
                (recur super* (&/Cons$ super* stack))))))

        :else
        (loop [sub-class sub-class
               stack (&/|list)]
          (let [super* (.getSuperclass sub-class)]
            (if (= super* super-class)
              (&/Cons$ super* stack)
              (recur super* (&/Cons$ super* stack)))))))

(defn ^:private trace-lineage [^Class sub-class ^Class super-class]
  "(-> Class Class (List Class))"
  (if (= sub-class super-class)
    (&/|list)
    (&/|reverse (trace-lineage* super-class sub-class))))

(let [matcher (fn [m ^TypeVariable jt lt] (&/Cons$ (&/T (.getName jt) lt) m))]
  (defn ^:private match-params [sub-type-params params]
    (assert (and (= (&/|length sub-type-params) (&/|length params))
                 (&/|every? (partial instance? TypeVariable) sub-type-params)))
    (&/fold2 matcher (&/|table) sub-type-params params)))

;; [Exports]
(let [class-name-re #"((\[+)L([\.a-zA-Z0-9]+);|([\.a-zA-Z0-9]+)|(\[+)([ZBSIJFDC]))"
      Unit (&/V &/$TupleT (&/|list))
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
    (if-let [[_ _ arr-obrackets arr-obase simple-base arr-pbrackets arr-pbase] (re-find class-name-re (.getName class))]
      (let [base (or arr-obase simple-base (jprim->lprim arr-pbase))]
        (if (.equals "void" base)
          Unit
          (reduce (fn [inner _] (&/V &/$DataT (&/T array-data-tag (&/|list inner))))
                  (&/V &/$DataT (&/T base &/Nil$))
                  (range (count (or arr-obrackets arr-pbrackets "")))))
        ))))

(defn instance-param [existential matchings refl-type]
  "(-> (Lux Type) (List (, Text Type)) (^ java.lang.reflect.Type) (Lux Type))"
  (cond (instance? Class refl-type)
        (return (class->type refl-type))

        (instance? GenericArrayType refl-type)
        (let [inner-type (instance-param existential matchings (.getGenericComponentType ^GenericArrayType refl-type))]
          (return (&/V &/$DataT (&/T array-data-tag (&/|list inner-type)))))
        
        (instance? ParameterizedType refl-type)
        (|do [:let [refl-type* ^ParameterizedType refl-type]
              params* (->> refl-type*
                           .getActualTypeArguments
                           seq &/->list
                           (&/map% (partial instance-param existential matchings)))]
          (return (&/V &/$DataT (&/T (->> refl-type* ^Class (.getRawType) .getName)
                                     params*))))
        
        (instance? TypeVariable refl-type)
        (let [gvar (.getName ^TypeVariable refl-type)]
          (if-let [m-type (&/|get gvar matchings)]
            (return m-type)
            (fail (str "[Type Error] Unknown generic type variable: " gvar " -- " (->> matchings
                                                                                       (&/|map &/|first)
                                                                                       &/->seq)))))
        
        (instance? WildcardType refl-type)
        (if-let [bound (->> ^WildcardType refl-type .getUpperBounds seq first)]
          (instance-param existential matchings bound)
          existential)))

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
                                      super-params
                                      (->> sub .getTypeParameters seq &/->list)
                                      params)]
        (return (&/T super params*)))
      (let [super* (.getGenericSuperclass sub)]
        (cond (instance? Class super*)
              (return (&/T super* (&/|list)))

              (instance? ParameterizedType super*)
              (|do [params* (translate-params existential
                                              (->> ^ParameterizedType super* .getActualTypeArguments seq &/->list)
                                              (->> sub .getTypeParameters seq &/->list)
                                              params)]
                (return (&/T super params*)))
              
              :else
              (assert false (prn-str super* (class super*) [sub super])))))))

(defn ^:private raise [existential lineage class params]
  "(-> (List Class) Class (List Type) (Lux (, Class (List Type))))"
  (&/fold% (partial raise* existential) (&/T class params) lineage))

;; [Exports]
(defn ->super-type [existential class-loader super-class sub-class sub-params]
  "(-> Text Text (List Type) (Lux Type))"
  (let [super-class+ (Class/forName super-class true class-loader)
        sub-class+ (Class/forName sub-class true class-loader)]
    (if (.isAssignableFrom super-class+ sub-class+)
      (let [lineage (trace-lineage sub-class+ super-class+)]
        (|do [[^Class sub-class* sub-params*] (raise existential lineage sub-class+ sub-params)]
          (return (&/V &/$DataT (&/T (.getName sub-class*) sub-params*)))))
      (fail (str "[Type Error] Classes don't have a subtyping relationship: " sub-class " </= " super-class)))))

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

(defn check-host-types [check check-error fixpoints existential class-loader invariant?? expected actual]
  (|let [[e!name e!params] expected
         [a!name a!params] actual]
    (try (cond (= "java.lang.Object" e!name)
               (return (&/T fixpoints nil))

               (= null-data-tag a!name)
               (if (not (primitive-type? e!name))
                 (return (&/T fixpoints nil))
                 (fail (check-error (&/V &/$DataT expected) (&/V &/$DataT actual))))

               (= null-data-tag e!name)
               (if (= null-data-tag a!name)
                 (return (&/T fixpoints nil))
                 (fail (check-error (&/V &/$DataT expected) (&/V &/$DataT actual))))

               (and (= array-data-tag e!name)
                    (not= array-data-tag a!name))
               (fail (check-error (&/V &/$DataT expected) (&/V &/$DataT actual)))
               
               :else
               (let [e!name (as-obj e!name)
                     a!name (as-obj a!name)]
                 (cond (.equals ^Object e!name a!name)
                       (if (= (&/|length e!params) (&/|length a!params))
                         (|do [_ (&/map2% check e!params a!params)]
                           (return (&/T fixpoints nil)))
                         (fail (str "[Type Error] Amounts of generic parameters don't match: " e!name "(" (&/|length e!params) ")" " vs " a!name "(" (&/|length a!params) ")")))

                       (not invariant??)
                       (|do [actual* (->super-type existential class-loader e!name a!name a!params)]
                         (check (&/V &/$DataT expected) actual*))

                       :else
                       (fail (str "[Type Error] Names don't match: " e!name " =/= " a!name)))))
      (catch Exception e
        (prn 'check-host-types e [e!name a!name])
        (throw e)))))

(let [Void$ (&/V &/$VariantT (&/|list))
      gen-type (constantly Void$)]
  (defn dummy-gtype [class]
    (|do [class-loader &/loader]
      (try (|let [=class (Class/forName class true class-loader)
                  params (->> =class .getTypeParameters seq &/->list (&/|map gen-type))]
             (return (&/V &/$DataT (&/T class params))))
        (catch Exception e
          (fail (str "[Type Error] Unknown type: " class)))))))

(defn class-name->type [class-name]
  (case class-name
    "[Z" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "java.lang.Boolean" (&/|list))))))
    "[B" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "java.lang.Byte" (&/|list))))))
    "[S" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "java.lang.Short" (&/|list))))))
    "[I" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "java.lang.Integer" (&/|list))))))
    "[J" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "java.lang.Long" (&/|list))))))
    "[F" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "java.lang.Float" (&/|list))))))
    "[D" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "java.lang.Double" (&/|list))))))
    "[C" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "java.lang.Character" (&/|list))))))
    ;; "[Z" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "boolean" (&/|list))))))
    ;; "[B" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "byte" (&/|list))))))
    ;; "[S" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "short" (&/|list))))))
    ;; "[I" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "int" (&/|list))))))
    ;; "[J" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "long" (&/|list))))))
    ;; "[F" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "float" (&/|list))))))
    ;; "[D" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "double" (&/|list))))))
    ;; "[C" (&/V &/$DataT (&/T array-data-tag (&/|list (&/V &/$DataT (&/T "char" (&/|list))))))
    ;; else
    (&/V &/$DataT (&/T class-name (&/|list)))))
