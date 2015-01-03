(ns lang.util
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]))

;; [Interface]
;; [Interface/Utils]
(defn fail* [message]
  [::failure message])

(defn return* [state value]
  [::ok [state value]])

;; [Interface/Monads]
(defn fail [message]
  (fn [_]
    [::failure message]))

(defn return [value]
  (fn [state]
    [::ok [state value]]))

(defn bind [m-value step]
  #(let [inputs (m-value %)]
     ;; (prn 'bind/inputs inputs)
     (match inputs
       [::ok [?state ?datum]]
       ((step ?datum) ?state)
       
       [::failure _]
       inputs)))

(defmacro exec [steps return]
  (assert (not= 0 (count steps)) "The steps can't be empty!")
  (assert (= 0 (rem (count steps) 2)) "The number of steps must be even!")
  (reduce (fn [inner [label computation]]
            (case label
              :let `(let ~computation ~inner)
              ;; :when (assert false "Can't use :when")
              :when `(if ~computation
                       ~inner
                       zero)
              ;; else
              `(bind ~computation (fn [~label] ~inner))))
          return
          (reverse (partition 2 steps))))

;; [Interface/Combinators]
(defn try-m [monad]
  (fn [state]
    (match (monad state)
      [::ok [?state ?datum]]
      (return* ?state ?datum)
      
      [::failure _]
      (return* state nil))))

(defn repeat-m [monad]
  (fn [state]
    (match (monad state)
      [::ok [?state ?head]]
      (do ;; (prn 'repeat-m/?state ?state)
          (match ((repeat-m monad) ?state)
            [::ok [?state* ?tail]]
            (do ;; (prn 'repeat-m/?state* ?state*)
                (return* ?state* (cons ?head ?tail)))))
      
      [::failure ?message]
      (do ;; (println "Failed at last:" ?message)
          (return* state '())))))

(defn try-all-m [monads]
  (fn [state]
    (if (empty? monads)
      (fail* "No alternative worked!")
      (let [output ((first monads) state)]
        (match output
          [::ok _]
          output
          :else
          (if-let [monads* (seq (rest monads))]
            ((try-all-m monads*) state)
            output)
          )))))

(defn map-m [f inputs]
  (if (empty? inputs)
    (return '())
    (exec [output (f (first inputs))
           outputs (map-m f (rest inputs))]
      (return (conj outputs output)))))

(defn reduce-m [f init inputs]
  (if (empty? inputs)
    (return init)
    (exec [init* (f init (first inputs))]
      (reduce-m f init* (rest inputs)))))

(defn apply-m [monad call-state]
  (fn [state]
    ;; (prn 'apply-m monad call-state)
    (let [output (monad call-state)]
      ;; (prn 'apply-m/output output)
      (match output
        [::ok [?state ?datum]]
        [::ok [state ?datum]]
        
        [::failure _]
        output))))

(defn assert! [test message]
  (if test
    (return nil)
    (fail message)))

(defn comp-m [f-m g-m]
  (exec [temp g-m]
    (f-m temp)))

(defn pass [m-value]
  (fn [state]
    m-value))

(def get-state
  (fn [state]
    (return* state state)))

(defn within [slot monad]
  (fn [state]
    (let [=return (monad (get state slot))]
      (match =return
        [::ok [?state ?value]]
        [::ok [(assoc state slot ?state) ?value]]
        _
        =return))))

(defn ^:private normalize-char [char]
  (case char
    \* "_ASTER_"
    \+ "_PLUS_"
    \- "_DASH_"
    \/ "_SLASH_"
    \_ "_UNDERS_"
    \% "_PERCENT_"
    \$ "_DOLLAR_"
    \! "_BANG_"
    \' "_QUOTE_"
    \` "_BQUOTE_"
    \@ "_AT_"
    \^ "_CARET_"
    \& "_AMPERS_"
    \= "_EQ_"
    ;; default
    char))

(defn normalize-ident [ident]
  (reduce str "" (map normalize-char ident)))

(defonce loader (doto (-> (java.io.File. "./") .toURL vector into-array java.net.URLClassLoader.)
                  (->> (prn 'loader))))
