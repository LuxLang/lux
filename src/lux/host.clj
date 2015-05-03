(ns lux.host
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail* |let]]
                 [type :as &type])))

;; [Constants]
(def prefix "lux.")
(def function-class (str prefix "Function"))

;; [Utils]
(defn ^:private class->type [class]
  (if-let [[_ base arr-level] (re-find #"^([^\[]+)(\[\])*$"
                                       (str (if-let [pkg (.getPackage class)]
                                              (str (.getName pkg) ".")
                                              "")
                                            (.getSimpleName class)))]
    (if (= "void" base)
      (return &type/$Void)
      (return (&/V "lux;DataT" (str (reduce str "" (repeat (int (/ (count arr-level) 2)) "["))
                                    base)))
      )))

(defn ^:private method->type [method]
  (|do [;; =args (&/map% class->type (&/->list (seq (.getParameterTypes method))))
         =return (class->type (.getReturnType method))]
    (return =return)))

;; [Resources]
(defn full-class [class-name]
  (case class
    "boolean" (return Boolean/TYPE)
    "byte"    (return Byte/TYPE)
    "short"   (return Short/TYPE)
    "int"     (return Integer/TYPE)
    "long"    (return Long/TYPE)
    "float"   (return Float/TYPE)
    "double"  (return Double/TYPE)
    "char"    (return Character/TYPE)
    ;; else
    (try (return (Class/forName class-name))
      (catch Exception e
        (fail "[Analyser Error] Unknown class.")))))

(defn full-class-name [class-name]
  ;; (prn 'full-class-name class-name)
  (|do [=class (full-class class-name)]
    (return (.getName =class))))

(defn ->class [class]
  (string/replace class #"\." "/"))

(def ->package ->class)

(defn ->type-signature [class]
  (assert (string? class))
  (case class
    "void"    "V"
    "boolean" "Z"
    "byte"    "B"
    "short"   "S"
    "int"     "I"
    "long"    "J"
    "float"   "F"
    "double"  "D"
    "char"    "C"
    ;; else
    (let [class* (->class class)]
      (if (.startsWith class* "[")
        class*
        (str "L" class* ";")))
    ))

(defn ->java-sig [type]
  (matchv ::M/objects [type]
    [["lux;DataT" ?name]]
    (->type-signature ?name)

    [["lux;LambdaT" [_ _]]]
    (->type-signature function-class)

    [["lux;VariantT" ["lux;Nil" _]]]
    "V"
    
    [_]
    (assert false (prn-str '->java-sig (aget type 0)))))

(defn extract-jvm-param [token]
  (matchv ::M/objects [token]
    [["lux;Meta" [_ ["lux;Symbol" [_ ?ident]]]]]
    (full-class-name ?ident)

    [["lux;Meta" [_ ["lux;Form" ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ "Array"]]]]
                                             ["lux;Cons" [["lux;Meta" [_ ["lux;Symbol" [_ ?inner]]]]
                                                          ["lux;Nil" _]]]]]]]]]
    (|do [=inner (full-class-name ?inner)]
      (return (str "[L" (->class =inner) ";")))

    [_]
    (fail (str "[Host] Unknown JVM param: " (pr-str token)))))

(do-template [<name> <static?>]
  (defn <name> [target field]
    (let [target (Class/forName target)]
      (if-let [type* (first (for [=field (.getFields target)
                                  :when (and (= target (.getDeclaringClass =field))
                                             (= field (.getName =field))
                                             (= <static?> (java.lang.reflect.Modifier/isStatic (.getModifiers =field))))]
                              (.getType =field)))]
        (|do [=type (class->type type*)]
          (return =type))
        (fail (str "[Analyser Error] Field does not exist: " target field)))))

  lookup-static-field true
  lookup-field        false
  )

(do-template [<name> <static?>]
  (defn <name> [target method-name args]
    (let [target (Class/forName target)]
      (if-let [method (first (for [=method (.getMethods target)
                                   ;; :let [_ (prn '<name> '=method =method (mapv #(.getName %) (.getParameterTypes =method)))]
                                   :when (and (= target (.getDeclaringClass =method))
                                              (= method-name (.getName =method))
                                              (= <static?> (java.lang.reflect.Modifier/isStatic (.getModifiers =method)))
                                              (&/fold #(and %1 %2)
                                                      true
                                                      (&/|map (fn [xy]
                                                                (|let [[x y] xy]
                                                                  (= x y)))
                                                              (&/zip2 args
                                                                      (&/|map #(.getName %) (&/->list (seq (.getParameterTypes =method))))))))]
                               =method))]
        (method->type method)
        (fail (str "[Analyser Error] Method does not exist: " target method-name)))))

  lookup-static-method  true
  lookup-virtual-method false
  )

(defn location [scope]
  (->> scope (&/|map &/normalize-ident) (&/|interpose "$") (&/fold str "")))
