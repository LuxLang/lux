(ns lux.type
  (:refer-clojure :exclude [resolve])
  (:require [clojure.core.match :refer [match]]
            [lux.util :as &util :refer [exec return* return fail fail*
                                        repeat-m try-m try-all-m map-m
                                        apply-m assert!]]))

;; [Util]
(def ^:private success (return nil))

(defn ^:private resolve [id]
  (fn [state]
    (if-let [top+bottom (get-in state [::mappings id])]
      [::&util/ok [state top+bottom]]
      [::&util/failure (str "Unknown type-var: " id)])))

(defn ^:private update [id top bottom]
  (fn [state]
    (if-let [top+bottom (get-in state [::mappings id])]
      [::&util/ok [(assoc-in state [::mappings id] [top bottom]) nil]]
      [::&util/failure (str "Unknown type-var: " id)])))

;; [Interface]
(def +init+ {::counter 0
             ::mappings {}})

(def fresh-var
  (fn [state]
    (let [id (::counter state)]
      [::&util/ok [(-> state
                       (update-in [::counter] inc)
                       (assoc-in [::mappings id] [::any ::nothing]))
                   [::var id]]])))

(defn fresh-function [num-args]
  (exec [=args (map-m (constantly fresh-var) (range num-args))
         =return fresh-var
         :let [=function [::function =args =return]]]
    (return [=function =args =return])))

(defn solve [expected actual]
  ;; (prn 'solve expected actual)
  (match [expected actual]
    [::any _]
    success

    [_ ::nothing]
    success

    [_ [::var ?id]]
    (exec [[=top =bottom] (resolve ?id)]
      (try-all-m [(exec [_ (solve expected =top)]
                    success)
                  (exec [_ (solve =top expected)
                         _ (solve expected =bottom)
                         _ (update ?id expected =bottom)]
                    success)]))

    [[::var ?id] _]
    (exec [[=top =bottom] (resolve ?id)]
      (try-all-m [(exec [_ (solve =bottom actual)]
                    success)
                  (exec [_ (solve actual =bottom)
                         _ (solve =top actual)
                         _ (update ?id =top actual)]
                    success)]))

    [[::primitive ?prim] _]
    (let [as-obj (case ?prim
                   "boolean" [:lang.type/object "java.lang.Boolean" []]
                   "int"     [:lang.type/object "java.lang.Integer" []]
                   "long"    [:lang.type/object "java.lang.Long" []]
                   "char"    [:lang.type/object "java.lang.Character" []]
                   "float"   [:lang.type/object "java.lang.Float" []]
                   "double"  [:lang.type/object "java.lang.Double" []])]
      (solve as-obj actual))

    [[::object ?eclass []] [::object ?aclass []]]
    (if (.isAssignableFrom (Class/forName ?eclass) (Class/forName ?aclass))
      success
      (fail (str "Can't solve types: " (pr-str expected actual))))

    [_ _]
    (fail (str "Can't solve types: " (pr-str expected actual)))
    ))

(defn pick-matches [methods args]
  (if (empty? methods)
    (fail "No matches.")
    (try-all-m [(match (-> methods first second)
                  [::function ?args ?return]
                  (exec [_ (assert! (= (count ?args) (count args)) "Args-size doesn't match.")
                         _ (map-m (fn [[e a]] (solve e a)) (map vector ?args args))]
                    (return (first methods))))
                (pick-matches (rest methods) args)])))

(defn clean [type]
  (match type
    [::var ?id]
    (exec [[=top =bottom] (resolve ?id)]
      (clean =top))

    [::function ?args ?return]
    (exec [=args (map-m clean ?args)
           =return (clean ?return)]
      (return [::function =args =return]))

    ;; ::any
    ;; (return [::object "java.lang.Object" []])
    
    _
    (return type)))

;; Java Reflection
(defn class->type [class]
  (if-let [[_ base arr-level] (re-find #"^([^\[]+)(\[\])*$"
                                       (str (if-let [pkg (.getPackage class)]
                                              (str (.getName pkg) ".")
                                              "")
                                            (.getSimpleName class)))]
    (if (= "void" base)
      (return ::nothing)
      (let [base* (case base
                    ("boolean" "byte" "short" "int" "long" "float" "double" "char")
                    [::primitive base]
                    ;; else
                    [::object base []])]
        (if arr-level
          (return (reduce (fn [inner _]
                            [::array inner])
                          base*
                          (range (/ (count arr-level) 2.0))))
          (return base*)))
      
      )))

(defn method->type [method]
  (exec [=args (map-m class->type (seq (.getParameterTypes method)))
         =return (class->type (.getReturnType method))]
    (return [::function (vec =args) =return])))

(defn return-type [func]
  (match func
    [::function _ ?return]
    (return ?return)

    _
    (fail (str "Type is not a function: " (pr-str func)))))
