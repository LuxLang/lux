(ns lux.base
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [match matchv]]))

;; [Resources]
;; [Resources/Contants]
(def +name-separator+ ";")

;; [Resources/Utils]
(defn fail* [message]
  [::failure message])

(defn return* [state value]
  [::ok [state value]])

;; [Resources/Monads]
(defn fail [message]
  (fn [_]
    [::failure message]))

(defn return [value]
  (fn [state]
    [::ok [state value]]))

(defn bind [m-value step]
  (fn [state]
    (let [inputs (m-value state)]
      (match inputs
        [::ok [?state ?datum]]
        ((step ?datum) ?state)
        
        [::failure _]
        inputs))))

(defmacro exec [steps return]
  (assert (not= 0 (count steps)) "The steps can't be empty!")
  (assert (= 0 (rem (count steps) 2)) "The number of steps must be even!")
  (reduce (fn [inner [label computation]]
            (case label
              :let `(let ~computation ~inner)
              ;; else
              `(bind ~computation (fn [~label] ~inner))))
          return
          (reverse (partition 2 steps))))

;; [Resources/Combinators]
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

(def source-consumed?
  (fn [state]
    [::ok [state (empty? (::source state))]]))

(defn exhaust-m [monad]
  (exec [output-h monad
         ? source-consumed?
         output-t (if ?
                    (return (list))
                    (exhaust-m monad))]
    (return (cons output-h output-t))))

(defn try-all-m [monads]
  (if (empty? monads)
    (fail "Can't try no alternatives!")
    (fn [state]
      (let [output ((first monads) state)]
        (match output
          [::ok _]
          output
          
          _
          (if-let [monads* (seq (rest monads))]
            ((try-all-m monads*) state)
            output)
          )))))

(defn if-m [text-m then-m else-m]
  (exec [? text-m]
    (if ?
      then-m
      else-m)))

(do-template [<name> <joiner>]
  (defn <name> [f inputs]
    (if (empty? inputs)
      (return '())
      (exec [output (f (first inputs))
             outputs (<name> f (rest inputs))]
        (return (<joiner> output outputs)))))

  map-m    cons
  mapcat-m concat)

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

(defn sequence-m [m-values]
  (match m-values
    ([head & tail] :seq)
    (exec [_ head]
      (sequence-m tail))

    _
    (return nil)))

(defn ^:private normalize-char [char]
  (case char
    \* "_ASTER_"
    \+ "_PLUS_"
    \- "_DASH_"
    \/ "_SLASH_"
    \\ "_BSLASH_"
    \_ "_UNDERS_"
    \% "_PERCENT_"
    \$ "_DOLLAR_"
    \' "_QUOTE_"
    \` "_BQUOTE_"
    \@ "_AT_"
    \^ "_CARET_"
    \& "_AMPERS_"
    \= "_EQ_"
    \! "_BANG_"
    \? "_QM_"
    \: "_COLON_"
    \; "_SCOLON_"
    \. "_PERIOD_"
    \, "_COMMA_"
    \< "_LT_"
    \> "_GT_"
    \~ "_TILDE_"
    ;; default
    char))

(defn normalize-ident [ident]
  (reduce str "" (map normalize-char ident)))

(def loader
  (fn [state]
    (return* state (::loader state))))

(def +init-bindings+
  {:counter 0
   :mappings {}})

(defn env [name]
  {:name name
   :inner-closures 0
   :locals  +init-bindings+
   :closure +init-bindings+})

(defn init-state []
  {::source nil
   ::modules {}
   ::global-env nil
   ::local-envs (list)
   ::types +init-bindings+
   ::writer nil
   ::loader (-> (java.io.File. "./output/") .toURL vector into-array java.net.URLClassLoader.)})

(def get-writer
  (fn [state]
    (if-let [datum (::writer state)]
      [::ok [state datum]]
      [::failure "Writer hasn't been set."])))

(def get-top-local-env
  (fn [state]
    (if-let [datum (first (::local-envs state))]
      [::ok [state datum]]
      [::failure "Module hasn't been set."])))

(def get-current-module-env
  (fn [state]
    (if-let [datum (::global-env state)]
      [::ok [state datum]]
      [::failure "Module hasn't been set."])))

(def get-module-name
  (exec [module get-current-module-env]
    (return (:name module))))

(defn ^:private with-scope [name body]
  (fn [state]
    (let [output (body (update-in state [::local-envs] conj (env name)))]
      (match output
        [::ok [state* datum]]
        [::ok [(update-in state* [::local-envs] rest) datum]]
        
        _
        output))))

(defn with-closure [body]
  (exec [[local? closure-name] (try-all-m (list (exec [top get-top-local-env]
                                                  (return [true (-> top :inner-closures str)]))
                                                (exec [global get-current-module-env]
                                                  (return [false (-> global :inner-closures str)]))))]
    (fn [state]
      (let [body* (with-scope closure-name
                    body)]
        (body* (if local?
                 (update-in state [::local-envs]
                            #(cons (update-in (first %) [:inner-closures] inc)
                                   (rest %)))
                 (update-in state [::global-env :inner-closures] inc)))))))

(def get-scope-name
  (exec [module-name get-module-name]
    (fn [state]
      [::ok [state (->> state ::local-envs (map :name) reverse (cons module-name))]])))

(defn with-writer [writer body]
  (fn [state]
    (let [output (body (assoc state ::writer writer))]
      (match output
        [::ok [?state ?value]]
        [::ok [(assoc ?state ::writer (::writer state)) ?value]]

        _
        output))))

(defn run-state [monad state]
  (monad state))

(defn V [tag value]
  (to-array [tag value]))

(defn ->seq [xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    (list)

    [["Cons" [x xs*]]]
    (cons x (->seq xs*))))

(defn show-ast [ast]
  (matchv ::M/objects [ast]
    [["Bool" ?value]]
    (pr-str ?value)

    [["Int" ?value]]
    (pr-str ?value)

    [["Real" ?value]]
    (pr-str ?value)

    [["Char" ?value]]
    (pr-str ?value)

    [["Text" ?value]]
    (str "\"" ?value "\"")

    [["Tag" ?tag]]
    (str "#" ?tag)

    [["Ident" ?ident]]
    ?ident

    [["Tuple" ?elems]]
    (str "[" (->> (->seq ?elems) (map show-ast) (interpose " ") (apply str)) "]")

    [["Form" ?elems]]
    (str "(" (->> (->seq ?elems) (map show-ast) (interpose " ") (apply str)) ")")
    ))
