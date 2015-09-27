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
(defn ^:private Data$ [name params]
  (&/V &/$DataT (&/T name params)))

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
  (&/|reverse (trace-lineage* super-class sub-class)))

(let [matcher (fn [m ^TypeVariable jt lt] (&/Cons$ (&/T (.getName jt) lt) m))]
  (defn ^:private match-params [sub-type-params params]
    (assert (and (= (&/|length sub-type-params) (&/|length params))
                 (&/|every? (partial instance? TypeVariable) sub-type-params)))
    (&/fold2 matcher (&/|table) sub-type-params params)))

;; [Exports]
(defn instance-param [existential matchings refl-type]
  "(-> (List (, Text Type)) (^ java.lang.reflect.Type) (Lux Type))"
  ;; (prn 'instance-param refl-type (class refl-type))
  (cond (instance? Class refl-type)
        (return (Data$ (.getName ^Class refl-type) (&/|list)))

        (instance? GenericArrayType refl-type)
        (let [inner-type (instance-param existential matchings (.getGenericComponentType ^GenericArrayType refl-type))]
          (return (Data$ array-data-tag (&/|list inner-type))))
        
        (instance? ParameterizedType refl-type)
        (|do [:let [refl-type* ^ParameterizedType refl-type]
              params* (->> refl-type*
                           .getActualTypeArguments
                           seq &/->list
                           (&/map% (partial instance-param existential matchings)))]
          (return (Data$ (->> refl-type* ^Class (.getRawType) .getName)
                         params*)))
        
        (instance? TypeVariable refl-type)
        (let [gvar (.getName ^TypeVariable refl-type)]
          (if-let [m-type (&/|get gvar matchings)]
            (return m-type)
            (fail (str "[Type Error] Unknown generic type variable: " gvar))))
        
        (instance? WildcardType refl-type)
        (if-let [bound (->> ^WildcardType refl-type .getUpperBounds seq first)]
          (instance-param existential matchings bound)
          existential)))

;; [Utils]
(defn ^:private translate-params [existential super-type-params sub-type-params params]
  "(-> (List (^ java.lang.reflect.Type)) (List (^ java.lang.reflect.Type)) (List Type) (Lux (List Type)))"
  (|let [matchings (match-params sub-type-params params)]
    (&/map% (partial instance-param existential matchings) super-type-params)))

(defn ^:private raise* [existential sub+params super]
  "(-> (, Class (List Type)) Class (Lux (, Class (List Type))))"
  (|let [[^Class sub params] sub+params]
    (if (.isInterface super)
      (|do [:let [super-params (->> sub
                                    .getGenericInterfaces
                                    (some #(if (= super (if (instance? Class %) % (.getRawType ^ParameterizedType %)))
                                             (if (instance? Class %) (&/|list) (->> % .getActualTypeArguments seq &/->list))
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
        (|do [[sub-class* sub-params*] (raise existential lineage sub-class+ sub-params)]
          (return (Data$ (.getName sub-class*) sub-params*))))
      (fail (str "[Type Error] Classes don't have a subtyping relationship: " sub-class " </=" super-class)))))

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
