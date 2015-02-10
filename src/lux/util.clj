(ns lux.util
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :refer [match]]))

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
  #(let [inputs (m-value %)]
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

(defn exhaust-m [monad]
  (fn [state]
    (let [result (monad state)]
      (match result
        [::ok [?state ?head]]
        (if (empty? (:forms ?state))
          (return* ?state (list ?head))
          (let [result* ((exhaust-m monad) ?state)]
            (match result*
              [::ok [?state* ?tail]]
              (return* ?state* (cons ?head ?tail))

              _
              result*)))
        
        _
        result))))

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

(do-template [<name> <joiner>]
  (defn <name> [f inputs]
    (if (empty? inputs)
      (return '())
      (exec [output (f (first inputs))
             outputs (map-m f (rest inputs))]
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
  (if (empty? m-values)
    (return nil)
    (exec [head (first m-values)]
      (sequence-m (rest monads)))))

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

(defn class-loader! []
  (-> (java.io.File. "./output/") .toURL vector into-array java.net.URLClassLoader.))

(def loader
  (fn [state]
    (return* state (::loader state))))

(def +init-env+
  {:counter 0
   :mappings {}})

(defn scope [name]
  {:name name
   :inner-lambdas 0
   :locals  +init-env+
   :closure +init-env+})

(defn init-state []
  {::source nil
   ::current-module nil
   ::modules {}
   ::global-env {}
   ::local-envs (list)
   ::types +init-env+
   ::writer nil
   ::loader (class-loader!)})

(do-template [<name> <tag>]
  (def <name>
    (fn [state]
      (if-let [datum (<tag> state)]
        [::ok [state datum]]
        [::failure (str "Data does not exist: " <tag>)])))

  get-module-name ::current-module
  get-writer      ::writer
  )

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
