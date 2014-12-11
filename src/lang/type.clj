(ns lang.type
  (:refer-clojure :exclude [resolve])
  (:require [clojure.core.match :refer [match]]
            [lang.util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-m try-all-m map-m
                                         apply-m]]))

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

    [_ _]
    (fail (str "Can't solve types: " (pr-str expected actual)))
    ))

(defn clean [type]
  (match type
    [::var ?id]
    (exec [[=top =bottom] (resolve ?id)]
      (clean =top))

    [::function ?args ?return]
    (exec [=args (map-m clean ?args)
           =return (clean ?return)]
      (return [::function =args =return]))

    _
    (return type)))
