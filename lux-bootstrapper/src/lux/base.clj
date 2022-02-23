(ns lux.base
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array))

(def prelude
  "library/lux")

(def !log! (atom false))
(defn flag-prn! [& args]
  (when @!log!
    (apply prn args)))

;; [Tags]
(def unit-tag
  (.intern ""))

(defn T [elems]
  (case (count elems)
    0
    unit-tag

    1
    (first elems)

    ;; else
    (to-array elems)))

(defmacro defvariant [& names]
  (assert (> (count names) 1))
  `(do ~@(for [[[name num-params] idx] (map vector names (range (count names)))
               :let [last-idx (dec (count names))
                     [lefts right?] (if (= idx last-idx)
                                      [(dec idx) ""]
                                      [idx nil])
                     def-name (with-meta (symbol (str "$" name))
                                {::lefts lefts
                                 ::right? right?})]]
           (cond (= 0 num-params)
                 `(def ~def-name
                    (to-array [(int ~lefts) ~right? unit-tag]))

                 (= 1 num-params)
                 `(defn ~def-name [arg#]
                    (to-array [(int ~lefts) ~right? arg#]))

                 :else
                 (let [g!args (map (fn [_] (gensym "arg"))
                                   (range num-params))]
                   `(defn ~def-name [~@g!args]
                      (to-array [(int ~lefts) ~right? (T [~@g!args])])))
                 ))))

(defmacro deftuple [names]
  (assert (vector? names))
  `(do ~@(for [[name idx] (map vector names (range (count names)))]
           `(def ~(symbol (str "$" name))
              (int ~idx)))))

;; List
(defvariant
  ("End" 0)
  ("Item" 2))

;; Maybe
(defvariant
  ("None" 0)
  ("Some" 1))

;; Either
(defvariant
  ("Left" 1)
  ("Right" 1))

;; Code
(defvariant
  ("Bit" 1)
  ("Nat" 1)
  ("Int" 1)
  ("Rev" 1)
  ("Frac" 1)
  ("Text" 1)
  ("Identifier" 1)
  ("Form" 1)
  ("Variant" 1)
  ("Tuple" 1))

;; Type
(defvariant
  ("Primitive" 2)
  ("Sum" 2)
  ("Product" 2)
  ("Function" 2)
  ("Parameter" 1)
  ("Var" 1)
  ("Ex" 1)
  ("UnivQ" 2)
  ("ExQ" 2)
  ("Apply" 2)
  ("Named" 2))

;; Vars
(defvariant
  ("Local" 1)
  ("Captured" 1))

;; Binding
(deftuple
  ["counter"
   "mappings"])

;; Type-Context
(deftuple
  ["ex-counter"
   "var-counter"
   "var-bindings"])

;; Env
(deftuple
  ["name"
   "inner"
   "locals"
   "captured"])

;; Host
(deftuple
  ["writer"
   "loader"
   "classes"
   "type-env"
   "dummy-mappings"
   ])

(defvariant
  ("Build" 0)
  ("Eval" 0)
  ("REPL" 0))

(deftuple
  ["target"
   "version"
   "mode"
   "configuration"])

;; Hosts
(defvariant
  ("Jvm" 1)
  ("Js" 1))

(defvariant
  ("DefinitionG" 1)
  ("TypeG" 1)
  ("TagG" 1)
  ("SlotG" 1)
  ("AliasG" 1))

(deftuple
  ["info"
   "source"
   "location"
   "current-module"
   "modules"
   "scopes"
   "type-context"
   "expected"
   "seed"
   "scope-type-vars"
   "extensions"
   "eval"
   "host"])

(defvariant
  ("UpperBound" 0)
  ("LowerBound" 0))

(defvariant
  ("GenericTypeVar" 1)
  ("GenericClass" 2)
  ("GenericArray" 1)
  ("GenericWildcard" 1))

;; Privacy Modifiers
(defvariant
  ("DefaultPM" 0)
  ("PublicPM" 0)
  ("PrivatePM" 0)
  ("ProtectedPM" 0))

;; State Modifiers
(defvariant
  ("DefaultSM" 0)
  ("VolatileSM" 0)
  ("FinalSM" 0))

;; Inheritance Modifiers
(defvariant
  ("DefaultIM" 0)
  ("AbstractIM" 0)
  ("FinalIM" 0))

;; Fields
(defvariant
  ("ConstantFieldSyntax" 4)
  ("VariableFieldSyntax" 5))

(defvariant
  ("ConstantFieldAnalysis" 4)
  ("VariableFieldAnalysis" 5))

;; Methods
(defvariant
  ("ConstructorMethodSyntax" 1)
  ("VirtualMethodSyntax" 1)
  ("OverridenMethodSyntax" 1)
  ("StaticMethodSyntax" 1)
  ("AbstractMethodSyntax" 1)
  ("NativeMethodSyntax" 1))

(defvariant
  ("ConstructorMethodAnalysis" 1)
  ("VirtualMethodAnalysis" 1)
  ("OverridenMethodAnalysis" 1)
  ("StaticMethodAnalysis" 1)
  ("AbstractMethodAnalysis" 1)
  ("NativeMethodAnalysis" 1))

;; [Exports]
(def ^:const value-field "_value")
(def ^:const module-class-name "_")
(def ^:const +name-separator+ ".")

(def ^:const ^String version "00.07.00")

;; Constructors
(def empty-location (T ["" -1 -1]))

(defn get$ [slot ^objects record]
  (aget record slot))

(defn set$ [slot value ^objects record]
  (doto (aclone ^objects record)
    (aset slot value)))

(defmacro update$ [slot f record]
  `(let [record# ~record]
     (set$ ~slot (~f (get$ ~slot record#))
           record#)))

(defn fail* [message]
  ($Left message))

(defn return* [state value]
  ($Right (T [state value])))

(defn transform-pattern [pattern]
  (cond (vector? pattern) (case (count pattern)
                            0
                            unit-tag

                            1
                            (transform-pattern (first pattern))

                            ;; else
                            (mapv transform-pattern pattern))
        (seq? pattern) (if-let [tag-var (ns-resolve *ns* (first pattern))]
                         [(-> tag-var meta ::lefts)
                          (-> tag-var meta ::right?)
                          (transform-pattern (vec (rest pattern)))]
                         (assert false (str "Unknown var: " (first pattern))))
        :else pattern))

(defmacro |case [value & branches]
  (assert (even? (count branches)))
  (let [value* (if (vector? value)
                 [`(T [~@value])]
                 [value])]
    `(matchv ::M/objects ~value*
       ~@(mapcat (fn [[pattern body]]
                   (list [(transform-pattern pattern)]
                         body))
                 (partition 2 branches)))))

(defmacro |let [bindings body]
  (reduce (fn [inner [left right]]
            `(|case ~right
               ~left
               ~inner))
          body
          (reverse (partition 2 bindings))))

(defmacro |list [& elems]
  (reduce (fn [tail head]
            `($Item ~head ~tail))
          `$End
          (reverse elems)))

(defmacro |table [& elems]
  (reduce (fn [table [k v]]
            `(|put ~k ~v ~table))
          `$End
          (reverse (partition 2 elems))))

(defn |get [slot table]
  (|case table
    ($End)
    nil
    
    ($Item [k v] table*)
    (if (= k slot)
      v
      (recur slot table*))))

(defn |put [slot value table]
  (|case table
    ($End)
    ($Item (T [slot value]) $End)
    
    ($Item [k v] table*)
    (if (= k slot)
      ($Item (T [slot value]) table*)
      ($Item (T [k v]) (|put slot value table*)))
    ))

(defn |remove [slot table]
  (|case table
    ($End)
    table
    
    ($Item [k v] table*)
    (if (= k slot)
      table*
      ($Item (T [k v]) (|remove slot table*)))))

(defn |update [k f table]
  (|case table
    ($End)
    table

    ($Item [k* v] table*)
    (if (= k k*)
      ($Item (T [k* (f v)]) table*)
      ($Item (T [k* v]) (|update k f table*)))))

(defn |head [xs]
  (|case xs
    ($End)
    (assert false (prn-str '|head))

    ($Item x _)
    x))

(defn |tail [xs]
  (|case xs
    ($End)
    (assert false (prn-str '|tail))

    ($Item _ xs*)
    xs*))

;; [Resources/Monads]
(defn fail [message]
  (fn [_]
    ($Left message)))

(defn return [value]
  (fn [state]
    ($Right (T [state value]))))

(defn bind [m-value step]
  (fn [state]
    (let [inputs (m-value state)]
      (|case inputs
        ($Right ?state ?datum)
        ((step ?datum) ?state)
        
        ($Left _)
        inputs
        ))))

(defmacro |do [steps return]
  (assert (even? (count steps)) "The number of steps must be even!")
  (reduce (fn [inner [label computation]]
            (case label
              :let `(|let ~computation ~inner)
              ;; else
              `(bind ~computation
                     (fn [val#]
                       (|case val#
                         ~label
                         ~inner)))))
          return
          (reverse (partition 2 steps))))

;; [Resources/Combinators]
(let [array-class (class (to-array []))]
  (defn adt->text [adt]
    (if (= array-class (class adt))
      (str "[" (->> adt (map adt->text) (interpose " ") (reduce str "")) "]")
      (pr-str adt))))

(defn |++ [xs ys]
  (|case xs
    ($End)
    ys

    ($Item x xs*)
    ($Item x (|++ xs* ys))))

(defn |map [f xs]
  (|case xs
    ($End)
    xs

    ($Item x xs*)
    ($Item (f x) (|map f xs*))

    _
    (assert false (prn-str '|map f (adt->text xs)))))

(defn |empty?
  "(All [a] (-> (List a) Bit))"
  [xs]
  (|case xs
    ($End)
    true

    ($Item _ _)
    false))

(defn |filter
  "(All [a] (-> (-> a Bit) (List a) (List a)))"
  [p xs]
  (|case xs
    ($End)
    xs

    ($Item x xs*)
    (if (p x)
      ($Item x (|filter p xs*))
      (|filter p xs*))))

(defn flat-map
  "(All [a b] (-> (-> a (List b)) (List a) (List b)))"
  [f xs]
  (|case xs
    ($End)
    xs

    ($Item x xs*)
    (|++ (f x) (flat-map f xs*))))

(defn |split-with [p xs]
  (|case xs
    ($End)
    (T [xs xs])

    ($Item x xs*)
    (if (p x)
      (|let [[pre post] (|split-with p xs*)]
        (T [($Item x pre) post]))
      (T [$End xs]))))

(defn |contains? [k table]
  (|case table
    ($End)
    false

    ($Item [k* _] table*)
    (or (= k k*)
        (|contains? k table*))))

(defn |member? [x xs]
  (|case xs
    ($End)
    false

    ($Item x* xs*)
    (or (= x x*) (|member? x xs*))))

(defn fold [f init xs]
  (|case xs
    ($End)
    init

    ($Item x xs*)
    (recur f (f init x) xs*)))

(defn fold% [f init xs]
  (|case xs
    ($End)
    (return init)

    ($Item x xs*)
    (|do [init* (f init x)]
      (fold% f init* xs*))))

(defn folds [f init xs]
  (|case xs
    ($End)
    (|list init)

    ($Item x xs*)
    ($Item init (folds f (f init x) xs*))))

(defn |length [xs]
  (fold (fn [acc _] (inc acc)) 0 xs))

(defn |range* [from to]
  (if (<= from to)
    ($Item from (|range* (inc from) to))
    $End))

(let [|range* (fn |range* [from to]
                (if (< from to)
                  ($Item from (|range* (inc from) to))
                  $End))]
  (defn |range [n]
    (|range* 0 n)))

(defn |first [pair]
  (|let [[_1 _2] pair]
    _1))

(defn |second [pair]
  (|let [[_1 _2] pair]
    _2))

(defn zip2 [xs ys]
  (|case [xs ys]
    [($Item x xs*) ($Item y ys*)]
    ($Item (T [x y]) (zip2 xs* ys*))

    [_ _]
    $End))

(defn |keys [plist]
  (|case plist
    ($End)
    $End
    
    ($Item [k v] plist*)
    ($Item k (|keys plist*))))

(defn |vals [plist]
  (|case plist
    ($End)
    $End
    
    ($Item [k v] plist*)
    ($Item v (|vals plist*))))

(defn |interpose [sep xs]
  (|case xs
    ($End)
    xs

    ($Item _ ($End))
    xs
    
    ($Item x xs*)
    ($Item x ($Item sep (|interpose sep xs*)))))

(do-template [<name> <joiner>]
  (defn <name> [f xs]
    (|case xs
      ($End)
      (return xs)

      ($Item x xs*)
      (|do [y (f x)
            ys (<name> f xs*)]
        (return (<joiner> y ys)))))

  map%      $Item
  flat-map% |++)

(defn list-join [xss]
  (fold |++ $End xss))

(defn |reverse [xs]
  (fold (fn [tail head]
          ($Item head tail))
        $End
        xs))

(defn |as-pairs [xs]
  (loop [input xs
         output $End]
    (|case input
      ($Item headL ($Item headR tail))
      (recur tail ($Item (T [headL headR]) output))

      _
      (|reverse output))))

(defn add-loc [meta ^String msg]
  (if (.startsWith msg "@")
    msg
    (|let [[file line col] meta]
      (str "@ " file "," line "," col "\n" msg))))

(defn fail-with-loc [^String msg]
  (fn [state]
    (fail* (add-loc (get$ $location state) msg))))

(defn assert! [test ^String message]
  (if test
    (return unit-tag)
    (fail-with-loc message)))

(def get-state
  (fn [state]
    (return* state state)))

(defn try% [action]
  (fn [state]
    (|case (action state)
      ($Right output)
      ($Right (T [state ($Some output)]))

      ($Left _)
      ($Right (T [state $None])))))

(defn try-all% [monads]
  (|case monads
    ($End)
    (fail "[Error] There are no alternatives to try!")

    ($Item m monads*)
    (fn [state]
      (let [output (m state)]
        (|case [output monads*]
          [($Right _) _]
          output

          [_ ($End)]
          output
          
          [_ _]
          ((try-all% monads*) state)
          )))
    ))

(defn try-all-% [prefix monads]
  (|case monads
    ($End)
    (fail "[Error] There are no alternatives to try!")

    ($Item m monads*)
    (fn [state]
      (let [output (m state)]
        (|case [output monads*]
          [($Right _) _]
          output

          [_ ($End)]
          output

          [($Left ^String error) _]
          (if (.contains error prefix)
            ((try-all-% prefix monads*) state)
            output)
          )))
    ))

(defn exhaust% [step]
  (fn [state]
    (|case (step state)
      ($Right state* _)
      ((exhaust% step) state*)

      ($Left ^String msg)
      (if (.contains msg "[Reader Error] EOF")
        (return* state unit-tag)
        (fail* msg)))))

(defn |some
  "(All [a b] (-> (-> a (Maybe b)) (List a) (Maybe b)))"
  [f xs]
  (|case xs
    ($End)
    $None

    ($Item x xs*)
    (|case (f x)
      ($None) (|some f xs*)
      output  output)
    ))

(defn ^:private normalize-char [char]
  (case char
    \* "_AS"
    \+ "_PL"
    \- "_DS"
    \/ "_SL"
    \\ "_BS"
    \_ "_US"
    \% "_PC"
    \$ "_DL"
    \' "_QU"
    \` "_BQ"
    \@ "_AT"
    \^ "_CR"
    \& "_AA"
    \= "_EQ"
    \! "_BG"
    \? "_QM"
    \: "_CO"
    \; "_SC"
    \. "_PD"
    \, "_CM"
    \< "_LT"
    \> "_GT"
    \~ "_TI"
    \| "_PI"
    ;; default
    char))

(defn normalize-name [ident]
  (reduce str "" (map normalize-char ident)))

(def +init-bindings+
  (T [;; "lux;counter"
      0
      ;; "lux;mappings"
      (|table)]))

(def +init-type-context+
  (T [;; ex-counter
      0
      ;; var-counter
      0
      ;; var-bindings
      (|table)]))

(defn env [name old-name]
  (T [;; name
      ($Item name old-name)
      ;; inner
      0
      ;; locals
      +init-bindings+
      ;; captured
      +init-bindings+]
     ))

(do-template [<tag> <host-desc> <host> <ask> <change> <with>]
  (do (def <host>
        (fn [compiler]
          (|case (get$ $host compiler)
            (<tag> host-data)
            (return* compiler host-data)

            _
            ((fail-with-loc (str "[Error] Wrong host.\nExpected: " <host-desc>))
             compiler))))

    (def <ask>
      (fn [compiler]
        (|case (get$ $host compiler)
          (<tag> host-data)
          (return* compiler true)

          _
          (return* compiler false))))

    (defn <change> [slot updater]
      (|do [host <host>]
        (fn [compiler]
          (return* (set$ $host (<tag> (update$ slot updater host)) compiler)
                   (get$ slot host)))))

    (defn <with> [slot updater body]
      (|do [old-val (<change> slot updater)
            ?output-val body
            new-val (<change> slot (fn [_] old-val))]
        (return ?output-val))))

  $Jvm "JVM" jvm-host jvm? change-jvm-host-slot with-jvm-host-slot
  $Js  "JS"  js-host  js?  change-js-host-slot  with-js-host-slot
  )

(do-template [<name> <slot>]
  (def <name>
    (|do [host jvm-host]
      (return (get$ <slot> host))))

  loader       $loader
  classes      $classes
  get-type-env $type-env
  )

(def get-writer
  (|do [host jvm-host]
    (|case (get$ $writer host)
      ($Some writer)
      (return writer)

      _
      (fail-with-loc "[Error] Writer has not been set."))))

(defn with-writer [writer body]
  (with-jvm-host-slot $writer (fn [_] ($Some writer)) body))

(defn with-type-env
  "(All [a] (-> TypeEnv (Meta a) (Meta a)))"
  [type-env body]
  (with-jvm-host-slot $type-env (partial |++ type-env) body))

(defn push-dummy-name [real-name store-name]
  (change-jvm-host-slot $dummy-mappings (partial $Item (T [real-name store-name]))))

(def pop-dummy-name
  (change-jvm-host-slot $dummy-mappings |tail))

(defn de-alias-class [class-name]
  (|do [host jvm-host]
    (return (|case (|some #(|let [[real-name store-name] %]
                             (if (= real-name class-name)
                               ($Some store-name)
                               $None))
                          (get$ $dummy-mappings host))
              ($Some store-name)
              store-name

              _
              class-name))))

(defn default-info [target mode]
  (T [;; target
      target
      ;; version
      version
      ;; mode
      mode
      ;; configuration
      $End]
     ))

(defn init-state [name mode host-data]
  (T [;; "lux;info"
      (default-info name mode)
      ;; "lux;source"
      $End
      ;; "lux;location"
      (T ["" -1 -1])
      ;; "current-module"
      $None
      ;; "lux;modules"
      (|table)
      ;; "lux;scopes"
      $End
      ;; "lux;type-context"
      +init-type-context+
      ;; "lux;expected"
      $None
      ;; "lux;seed"
      0
      ;; scope-type-vars
      $End
      ;; extensions
      "" ;; This is an invalid value. But I don't expect extensions to be used with the bootstrapping compiler.
      ;; eval
      "" ;; This is an invalid value. But I don't expect eval to be used with the bootstrapping compiler.
      ;; "lux;host"
      host-data]
     ))

(defn save-module [body]
  (fn [state]
    (|case (body state)
      ($Right state* output)
      (return* (->> state*
                    (set$ $scopes (get$ $scopes state))
                    (set$ $source (get$ $source state)))
               output)

      ($Left msg)
      (fail* msg))))

(do-template [<name> <tag>]
  (defn <name>
    "(-> CompilerMode Bit)"
    [mode]
    (|case mode
      (<tag>) true
      _       false))

  in-eval? $Eval
  in-repl? $REPL
  )

(defn with-eval [body]
  (fn [state]
    (let [old-mode (->> state (get$ $info) (get$ $mode))]
      (|case (body (update$ $info #(set$ $mode $Eval %) state))
        ($Right state* output)
        (return* (update$ $info #(set$ $mode old-mode %) state*) output)

        ($Left msg)
        (fail* msg)))))

(def get-eval
  (fn [state]
    (return* state (->> state (get$ $info) (get$ $mode) in-eval?))))

(def get-mode
  (fn [state]
    (return* state (->> state (get$ $info) (get$ $mode)))))

(def get-top-local-env
  (fn [state]
    (try (let [top (|head (get$ $scopes state))]
           (return* state top))
      (catch Throwable _
        ((fail-with-loc "[Error] No local environment.")
         state)))))

(def gen-id
  (fn [state]
    (let [seed (get$ $seed state)]
      (return* (set$ $seed (inc seed) state) seed))))

(defn ->seq [xs]
  (|case xs
    ($End)
    (list)

    ($Item x xs*)
    (cons x (->seq xs*))))

(defn ->list [seq]
  (if (empty? seq)
    $End
    ($Item (first seq) (->list (rest seq)))))

(defn |repeat [n x]
  (if (> n 0)
    ($Item x (|repeat (dec n) x))
    $End))

(def get-module-name
  (fn [state]
    (|case (get$ $current-module state)
      ($None)
      ((fail-with-loc "[Analyser Error] Cannot get the module-name without a module.")
       state)

      ($Some module-name)
      (return* state module-name))))

(defn find-module
  "(-> Text (Meta (Module Lux)))"
  [name]
  (fn [state]
    (if-let [module (|get name (get$ $modules state))]
      (return* state module)
      ((fail-with-loc (str "[Error] Unknown module: " name))
       state))))

(def ^{:doc "(Meta (Module Lux))"}
  get-current-module
  (|do [module-name get-module-name]
    (find-module module-name)))

(defn with-scope [name body]
  (fn [state]
    (let [old-name (->> state (get$ $scopes) |head (get$ $name))
          output (body (update$ $scopes #($Item (env name old-name) %) state))]
      (|case output
        ($Right state* datum)
        (return* (update$ $scopes |tail state*) datum)
        
        _
        output))))

(defn without-scope [body]
  (fn [state]
    (|case (body (set$ $scopes (|list (env "WITHOUT-SCOPE" "")) state))
      ($Right state* datum)
      (return* (set$ $scopes (get$ $scopes state) state*)
               datum)
      
      output
      output)))

(defn run-state [monad state]
  (monad state))

(defn with-closure [body]
  (|do [closure-name (|do [top get-top-local-env]
                       (return (->> top (get$ $inner) str)))]
    (fn [state]
      (let [body* (with-scope closure-name body)]
        (run-state body* (update$ $scopes #($Item (update$ $inner inc (|head %))
                                                  (|tail %))
                                  state))))))

(let [!out! *out*]
  (defn |log! [& parts]
    (binding [*out* !out!]
      (do (print (str (apply str parts) "\n"))
        (flush)))))

(defn |last [xs]
  (|case xs
    ($Item x ($End))
    x

    ($Item x xs*)
    (|last xs*)

    _
    (assert false (adt->text xs))))

(def get-scope-name
  (fn [state]
    (return* state (->> state (get$ $scopes) |head (get$ $name)))))

(defn without-repl-closure [body]
  (|do [_mode get-mode
        current-scope get-scope-name]
    (fn [state]
      (let [output (body (if (and (in-repl? _mode)
                                  (->> current-scope |last (= "REPL")))
                           (update$ $scopes |tail state)
                           state))]
        (|case output
          ($Right state* datum)
          (return* (set$ $scopes (get$ $scopes state) state*) datum)
          
          _
          output)))))

(defn without-repl [body]
  (|do [_mode get-mode]
    (fn [state]
      (let [output (body (if (in-repl? _mode)
                           (update$ $info #(set$ $mode $Build %) state)
                           state))]
        (|case output
          ($Right state* datum)
          (return* (update$ $info #(set$ $mode _mode %) state*) datum)
          
          _
          output)))))

(defn with-expected-type
  "(All [a] (-> Type (Meta a)))"
  [type body]
  (fn [state]
    (let [output (body (set$ $expected ($Some type) state))]
      (|case output
        ($Right ?state ?value)
        (return* (set$ $expected (get$ $expected state) ?state)
                 ?value)

        _
        output))))

(defn with-location
  "(All [a] (-> Location (Meta a)))"
  [^objects location body]
  (|let [[_file-name _ _] location]
    (if (= "" _file-name)
      body
      (fn [state]
        (let [output (body (set$ $location location state))]
          (|case output
            ($Right ?state ?value)
            (return* (set$ $location (get$ $location state) ?state)
                     ?value)

            _
            output))))))

(defn with-analysis-meta
  "(All [a] (-> Location Type (Meta a)))"
  [^objects location type body]
  (|let [[_file-name _ _] location]
    (if (= "" _file-name)
      (fn [state]
        (let [output (body (->> state
                                (set$ $expected ($Some type))))]
          (|case output
            ($Right ?state ?value)
            (return* (->> ?state
                          (set$ $expected (get$ $expected state)))
                     ?value)

            _
            output)))
      (fn [state]
        (let [output (body (->> state
                                (set$ $location location)
                                (set$ $expected ($Some type))))]
          (|case output
            ($Right ?state ?value)
            (return* (->> ?state
                          (set$ $location (get$ $location state))
                          (set$ $expected (get$ $expected state)))
                     ?value)

            _
            output))))))

(def ^{:doc "(Meta Any)"}
  ensure-directive
  (fn [state]
    (|case (get$ $expected state)
      ($None)
      (return* state unit-tag)

      ($Some _)
      ((fail-with-loc "[Error] All directives must be top-level forms.")
       state))))

(def location
  ;; (Meta Location)
  (fn [state]
    (return* state (get$ $location state))))

(def rev-bits 64)

(let [clean-separators (fn [^String input]
                         (.replaceAll input "_" ""))
      rev-text-to-digits (fn [^String input]
                           (loop [output (vec (repeat rev-bits 0))
                                  index (dec (.length input))]
                             (if (>= index 0)
                               (let [digit (Byte/parseByte (.substring input index (inc index)))]
                                 (recur (assoc output index digit)
                                        (dec index)))
                               output)))
      times5 (fn [index digits]
               (loop [index index
                      carry 0
                      digits digits]
                 (if (>= index 0)
                   (let [raw (->> (get digits index) (* 5) (+ carry))]
                     (recur (dec index)
                            (int (/ raw 10))
                            (assoc digits index (rem raw 10))))
                   digits)))
      rev-digit-power (fn [level]
                        (loop [output (-> (vec (repeat rev-bits 0))
                                          (assoc level 1))
                               times level]
                          (if (>= times 0)
                            (recur (times5 level output)
                                   (dec times))
                            output)))
      rev-digits-lt (fn rev-digits-lt
                      ([subject param index]
                       (and (< index rev-bits)
                            (or (< (get subject index)
                                   (get param index))
                                (and (= (get subject index)
                                        (get param index))
                                     (rev-digits-lt subject param (inc index))))))
                      ([subject param]
                       (rev-digits-lt subject param 0)))
      rev-digits-sub-once (fn [subject param-digit index]
                            (if (>= (get subject index)
                                    param-digit)
                              (update-in subject [index] #(- % param-digit))
                              (recur (update-in subject [index] #(- 10 (- param-digit %)))
                                     1
                                     (dec index))))
      rev-digits-sub (fn [subject param]
                       (loop [target subject
                              index (dec rev-bits)]
                         (if (>= index 0)
                           (recur (rev-digits-sub-once target (get param index) index)
                                  (dec index))
                           target)))
      rev-digits-to-text (fn [digits]
                           (loop [output ""
                                  index (dec rev-bits)]
                             (if (>= index 0)
                               (recur (-> (get digits index)
                                          (Character/forDigit 10)
                                          (str output))
                                      (dec index))
                               output)))
      add-rev-digit-powers (fn [dl dr]
                             (loop [index (dec rev-bits)
                                    output (vec (repeat rev-bits 0))
                                    carry 0]
                               (if (>= index 0)
                                 (let [raw (+ carry
                                              (get dl index)
                                              (get dr index))]
                                   (recur (dec index)
                                          (assoc output index (rem raw 10))
                                          (int (/ raw 10))))
                                 output)))]
  ;; Based on the Runtime.encode_rev method
  (defn encode-rev [input]
    (if (= 0 input)
      ".0"
      (loop [index (dec rev-bits)
             output (vec (repeat rev-bits 0))]
        (if (>= index 0)
          (recur (dec index)
                 (if (bit-test input index)
                   (->> (- (dec rev-bits) index)
                        rev-digit-power
                        (add-rev-digit-powers output))
                   output))
          (-> output rev-digits-to-text
              (->> (str "."))
              (.split "0*$")
              (aget 0))))))

  ;; Based on the Runtime.decode_rev method
  (defn decode-rev [^String input]
    (if (and (.startsWith input ".")
             (<= (.length input) (inc rev-bits)))
      (loop [digits-left (-> input
                             (.substring 1)
                             clean-separators
                             rev-text-to-digits)
             index 0
             ouput 0]
        (if (< index rev-bits)
          (let [power-slice (rev-digit-power index)]
            (if (not (rev-digits-lt digits-left power-slice))
              (recur (rev-digits-sub digits-left power-slice)
                     (inc index)
                     (bit-set ouput (- (dec rev-bits) index)))
              (recur digits-left
                     (inc index)
                     ouput)))
          ouput))
      (throw (new java.lang.Exception (str "Bad format for Rev number: " input)))))
  )

(defn show-ast [ast]
  (|case ast
    [_ ($Bit ?value)]
    (pr-str ?value)

    [_ ($Nat ?value)]
    (Long/toUnsignedString ?value)

    [_ ($Int ?value)]
    (if (< ?value 0)
      (pr-str ?value)
      (str "+" (pr-str ?value)))

    [_ ($Rev ?value)]
    (encode-rev ?value)

    [_ ($Frac ?value)]
    (pr-str ?value)

    [_ ($Text ?value)]
    (str "\"" ?value "\"")

    [_ ($Identifier ?module ?name)]
    (if (.equals "" ?module)
      ?name
      (str ?module +name-separator+ ?name))

    [_ ($Variant ?elems)]
    (str "{" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) "}")

    [_ ($Tuple ?elems)]
    (str "[" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) "]")

    [_ ($Form ?elems)]
    (str "(" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) ")")

    _
    (assert false (prn-str 'show-ast (adt->text ast)))
    ))

(defn ident->text [ident]
  (|let [[?module ?name] ident]
    (if (= "" ?module)
      ?name
      (str ?module +name-separator+ ?name))))

(defn fold2% [f init xs ys]
  (|case [xs ys]
    [($Item x xs*) ($Item y ys*)]
    (|do [init* (f init x y)]
      (fold2% f init* xs* ys*))

    [($End) ($End)]
    (return init)

    [_ _]
    (assert false "Lists do not match in size.")))

(defn map2% [f xs ys]
  (|case [xs ys]
    [($Item x xs*) ($Item y ys*)]
    (|do [z (f x y)
          zs (map2% f xs* ys*)]
      (return ($Item z zs)))

    [($End) ($End)]
    (return $End)

    [_ _]
    (assert false "Lists do not match in size.")))

(defn map2 [f xs ys]
  (|case [xs ys]
    [($Item x xs*) ($Item y ys*)]
    ($Item (f x y) (map2 f xs* ys*))

    [_ _]
    $End))

(defn fold2 [f init xs ys]
  (|case [xs ys]
    [($Item x xs*) ($Item y ys*)]
    (and init
         (fold2 f (f init x y) xs* ys*))

    [($End) ($End)]
    init

    [_ _]
    init
    ;; (assert false)
    ))

(defn ^:private enumerate*
  "(All [a] (-> Int (List a) (List (, Int a))))"
  [idx xs]
  (|case xs
    ($Item x xs*)
    ($Item (T [idx x])
           (enumerate* (inc idx) xs*))

    ($End)
    xs
    ))

(defn enumerate
  "(All [a] (-> (List a) (List (, Int a))))"
  [xs]
  (enumerate* 0 xs))

(def ^{:doc "(Meta (List Text))"}
  modules
  (fn [state]
    (return* state (|keys (get$ $modules state)))))

(defn when%
  "(-> Bit (Meta Any) (Meta Any))"
  [test body]
  (if test
    body
    (return unit-tag)))

(defn |at
  "(All [a] (-> Int (List a) (Maybe a)))"
  [idx xs]
  (|case xs
    ($Item x xs*)
    (cond (< idx 0)
          $None

          (= idx 0)
          ($Some x)

          :else ;; > 1
          (|at (dec idx) xs*))

    ($End)
    $None))

(defn normalize
  "(-> Ident (Meta Ident))"
  [ident]
  (|case ident
    ["" name] (|do [module get-module-name]
                (return (T [module name])))
    _ (return ident)))

(defn ident= [x y]
  (|let [[xmodule xname] x
         [ymodule yname] y]
    (and (= xmodule ymodule)
         (= xname yname))))

(defn |list-put [idx val xs]
  (|case xs
    ($End)
    $None
    
    ($Item x xs*)
    (if (= idx 0)
      ($Some ($Item val xs*))
      (|case (|list-put (dec idx) val xs*)
        ($None)      $None
        ($Some xs**) ($Some ($Item x xs**)))
      )))

(do-template [<name> <default> <op>]
  (defn <name>
    "(All [a] (-> (-> a Bit) (List a) Bit))"
    [p xs]
    (|case xs
      ($End)
      <default>

      ($Item x xs*)
      (<op> (p x) (<name> p xs*))))

  |every? true  and
  |any?   false or)

(defn m-comp
  "(All [a b c] (-> (-> b (Meta c)) (-> a (Meta b)) (-> a (Meta c))))"
  [f g]
  (fn [x]
    (|do [y (g x)]
      (f y))))

(defn with-attempt
  "(All [a] (-> (Meta a) (-> Text (Meta a)) (Meta a)))"
  [m-value on-error]
  (fn [state]
    (|case (m-value state)
      ($Left msg)
      ((on-error msg) state)
      
      output
      output)))

(defn |take [n xs]
  (|case (T [n xs])
    [0 _]             $End
    [_ ($End)]        $End
    [_ ($Item x xs*)] ($Item x (|take (dec n) xs*))
    ))

(defn |drop [n xs]
  (|case (T [n xs])
    [0 _]             xs
    [_ ($End)]        $End
    [_ ($Item x xs*)] (|drop (dec n) xs*)
    ))

(defn |but-last [xs]
  (|case xs
    ($End)
    $End
    
    ($Item x ($End))
    $End

    ($Item x xs*)
    ($Item x (|but-last xs*))

    _
    (assert false (adt->text xs))))

(defn |partition [n xs]
  (->> xs ->seq (partition-all n) (map ->list) ->list))

(defn with-scope-type-var [id body]
  (fn [state]
    (|case (body (set$ $scope-type-vars
                       ($Item id (get$ $scope-type-vars state))
                       state))
      ($Right [state* output])
      ($Right (T [(set$ $scope-type-vars
                        (get$ $scope-type-vars state)
                        state*)
                  output]))

      ($Left msg)
      ($Left msg))))

(defn with-module [name body]
  (fn [state]
    (|case (body (set$ $current-module ($Some name) state))
      ($Right [state* output])
      ($Right (T [(set$ $current-module (get$ $current-module state) state*)
                  output]))

      ($Left msg)
      ($Left msg))))

(defn |eitherL [left right]
  (fn [compiler]
    (|case (run-state left compiler)
      ($Left _error)
      (run-state right compiler)

      _output
      _output)))

(defn timed% [what when operation]
  (fn [state]
    (let [pre (System/currentTimeMillis)]
      (|case (operation state)
        ($Right state* output)
        (let [post (System/currentTimeMillis)
              duration (- post pre)
              _ (|log! (str what " [" when "]: +" duration "ms"))]
          ($Right (T [state* output])))

        ($Left ^String msg)
        (fail* msg)))))
