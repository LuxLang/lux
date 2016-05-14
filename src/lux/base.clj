;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.base
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array))

;; [Tags]
(def unit-tag (.intern (str (char 0) "unit" (char 0))))

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
                     is-last? (if (= idx last-idx)
                                ""
                                nil)
                     def-name (with-meta (symbol (str "$" name))
                                {::idx idx
                                 ::is-last? is-last?})]]
           (cond (= 0 num-params)
                 `(def ~def-name
                    (to-array [(int ~idx) ~is-last? unit-tag]))

                 (= 1 num-params)
                 `(defn ~def-name [arg#]
                    (to-array [(int ~idx) ~is-last? arg#]))

                 :else
                 (let [g!args (map (fn [_] (gensym "arg"))
                                   (range num-params))]
                   `(defn ~def-name [~@g!args]
                      (to-array [(int ~idx) ~is-last? (T [~@g!args])])))
                 ))))

(defmacro deftuple [names]
  (assert (vector? names))
  `(do ~@(for [[name idx] (map vector names (range (count names)))]
           `(def ~(symbol (str "$" name))
              (int ~idx)))))

;; List
(defvariant
  ("Nil" 0)
  ("Cons" 2))

;; Maybe
(defvariant
  ("None" 0)
  ("Some" 1))

;; Either
(defvariant
  ("Left" 1)
  ("Right" 1))

;; AST
(defvariant
  ("BoolS" 1)
  ("IntS" 1)
  ("RealS" 1)
  ("CharS" 1)
  ("TextS" 1)
  ("SymbolS" 1)
  ("TagS" 1)
  ("FormS" 1)
  ("TupleS" 1)
  ("RecordS" 1))

;; Type
(defvariant
  ("HostT" 2)
  ("VoidT" 0)
  ("UnitT" 0)
  ("SumT" 2)
  ("ProdT" 2)
  ("LambdaT" 2)
  ("BoundT" 1)
  ("VarT" 1)
  ("ExT" 1)
  ("UnivQ" 2)
  ("ExQ" 2)
  ("AppT" 2)
  ("NamedT" 2))

;; Vars
(defvariant
  ("Local" 1)
  ("Global" 1))

;; Binding
(deftuple
  ["counter"
   "mappings"])

;; Env
(deftuple
  ["name"
   "inner-closures"
   "locals"
   "closure"])

;; ModuleState
(defvariant
  ("Active" 0)
  ("Compiled" 0)
  ("Cached" 0))

;; Host
(deftuple
  ["writer"
   "loader"
   "classes"
   "catching"
   "module-states"
   "type-env"])

;; Compiler
(defvariant
  ("Release" 0)
  ("Debug" 0)
  ("Eval" 0))

(deftuple
  ["compiler-name"
   "compiler-version"
   "compiler-mode"])

(deftuple
  ["info"
   "source"
   "cursor"
   "modules"
   "envs"
   "type-vars"
   "expected"
   "seed"
   "host"])

;; Compiler
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

;; Meta-data
(defvariant
  ("BoolM" 1)
  ("IntM" 1)
  ("RealM" 1)
  ("CharM" 1)
  ("TextM" 1)
  ("IdentM" 1)
  ("ListM" 1)
  ("DictM" 1))

;; [Exports]
(def name-field "_name")
(def hash-field "_hash")
(def type-field "_type")
(def meta-field "_meta")
(def value-field "_value")
(def compiler-field "_compiler")
(def imports-field "_imports")
(def defs-field "_defs")
(def eval-field "_eval")
(def tags-field "_tags")
(def module-class-name "_")
(def +name-separator+ ";")

(def ^String compiler-name "Lux/JVM")
(def ^String compiler-version "0.4.0")

;; Constructors
(def empty-cursor (T ["" -1 -1]))

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
        (seq? pattern) [(if-let [tag-var (ns-resolve *ns* (first pattern))]
                          (-> tag-var
                              meta
                              ::idx)
                          (assert false (str "Unknown var: " (first pattern))))
                        '_
                        (transform-pattern (vec (rest pattern)))]
        :else pattern
        ))

(defmacro |case [value & branches]
  (assert (= 0 (mod (count branches) 2)))
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
            `($Cons ~head ~tail))
          `$Nil
          (reverse elems)))

(defmacro |table [& elems]
  (reduce (fn [table [k v]]
            `(|put ~k ~v ~table))
          `$Nil
          (reverse (partition 2 elems))))

(defn |get [slot table]
  (|case table
    ($Nil)
    nil
    
    ($Cons [k v] table*)
    (if (.equals ^Object k slot)
      v
      (recur slot table*))))

(defn |put [slot value table]
  (|case table
    ($Nil)
    ($Cons (T [slot value]) $Nil)
    
    ($Cons [k v] table*)
    (if (.equals ^Object k slot)
      ($Cons (T [slot value]) table*)
      ($Cons (T [k v]) (|put slot value table*)))
    ))

(defn |remove [slot table]
  (|case table
    ($Nil)
    table
    
    ($Cons [k v] table*)
    (if (.equals ^Object k slot)
      table*
      ($Cons (T [k v]) (|remove slot table*)))))

(defn |update [k f table]
  (|case table
    ($Nil)
    table

    ($Cons [k* v] table*)
    (if (.equals ^Object k k*)
      ($Cons (T [k* (f v)]) table*)
      ($Cons (T [k* v]) (|update k f table*)))))

(defn |head [xs]
  (|case xs
    ($Nil)
    (assert false (prn-str '|head))

    ($Cons x _)
    x))

(defn |tail [xs]
  (|case xs
    ($Nil)
    (assert false (prn-str '|tail))

    ($Cons _ xs*)
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
  (assert (= 0 (rem (count steps) 2)) "The number of steps must be even!")
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
    ($Nil)
    ys

    ($Cons x xs*)
    ($Cons x (|++ xs* ys))))

(defn |map [f xs]
  (|case xs
    ($Nil)
    xs

    ($Cons x xs*)
    ($Cons (f x) (|map f xs*))))

(defn |empty? [xs]
  "(All [a] (-> (List a) Bool))"
  (|case xs
    ($Nil)
    true

    ($Cons _ _)
    false))

(defn |filter [p xs]
  "(All [a] (-> (-> a Bool) (List a) (List a)))"
  (|case xs
    ($Nil)
    xs

    ($Cons x xs*)
    (if (p x)
      ($Cons x (|filter p xs*))
      (|filter p xs*))))

(defn flat-map [f xs]
  "(All [a b] (-> (-> a (List b)) (List a) (List b)))"
  (|case xs
    ($Nil)
    xs

    ($Cons x xs*)
    (|++ (f x) (flat-map f xs*))))

(defn |split-with [p xs]
  (|case xs
    ($Nil)
    (T [xs xs])

    ($Cons x xs*)
    (if (p x)
      (|let [[pre post] (|split-with p xs*)]
        (T [($Cons x pre) post]))
      (T [$Nil xs]))))

(defn |contains? [k table]
  (|case table
    ($Nil)
    false

    ($Cons [k* _] table*)
    (or (.equals ^Object k k*)
        (|contains? k table*))))

(defn |member? [x xs]
  (|case xs
    ($Nil)
    false

    ($Cons x* xs*)
    (or (= x x*) (|member? x xs*))))

(defn fold [f init xs]
  (|case xs
    ($Nil)
    init

    ($Cons x xs*)
    (recur f (f init x) xs*)))

(defn fold% [f init xs]
  (|case xs
    ($Nil)
    (return init)

    ($Cons x xs*)
    (|do [init* (f init x)]
      (fold% f init* xs*))))

(defn folds [f init xs]
  (|case xs
    ($Nil)
    (|list init)

    ($Cons x xs*)
    ($Cons init (folds f (f init x) xs*))))

(defn |length [xs]
  (fold (fn [acc _] (inc acc)) 0 xs))

(defn |range* [from to]
  (if (<= from to)
    ($Cons from (|range* (inc from) to))
    $Nil))

(let [|range* (fn |range* [from to]
                (if (< from to)
                  ($Cons from (|range* (inc from) to))
                  $Nil))]
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
    [($Cons x xs*) ($Cons y ys*)]
    ($Cons (T [x y]) (zip2 xs* ys*))

    [_ _]
    $Nil))

(defn |keys [plist]
  (|case plist
    ($Nil)
    $Nil
    
    ($Cons [k v] plist*)
    ($Cons k (|keys plist*))))

(defn |vals [plist]
  (|case plist
    ($Nil)
    $Nil
    
    ($Cons [k v] plist*)
    ($Cons v (|vals plist*))))

(defn |interpose [sep xs]
  (|case xs
    ($Nil)
    xs

    ($Cons _ ($Nil))
    xs
    
    ($Cons x xs*)
    ($Cons x ($Cons sep (|interpose sep xs*)))))

(do-template [<name> <joiner>]
  (defn <name> [f xs]
    (|case xs
      ($Nil)
      (return xs)

      ($Cons x xs*)
      (|do [y (f x)
            ys (<name> f xs*)]
        (return (<joiner> y ys)))))

  map%      $Cons
  flat-map% |++)

(defn list-join [xss]
  (fold |++ $Nil xss))

(defn |as-pairs [xs]
  (|case xs
    ($Cons x ($Cons y xs*))
    ($Cons (T [x y]) (|as-pairs xs*))

    _
    $Nil))

(defn |reverse [xs]
  (fold (fn [tail head]
          ($Cons head tail))
        $Nil
        xs))

(defn assert! [test message]
  (if test
    (return unit-tag)
    (fail message)))

(def get-state
  (fn [state]
    (return* state state)))

(defn try-all% [monads]
  (|case monads
    ($Nil)
    (fail "There are no alternatives to try!")

    ($Cons m monads*)
    (fn [state]
      (let [output (m state)]
        (|case [output monads*]
          [($Right _) _]
          output

          [_ ($Nil)]
          output
          
          [_ _]
          ((try-all% monads*) state)
          )))
    ))

(defn try-all-% [prefix monads]
  (|case monads
    ($Nil)
    (fail "There are no alternatives to try!")

    ($Cons m monads*)
    (fn [state]
      (let [output (m state)]
        (|case [output monads*]
          [($Right _) _]
          output

          [_ ($Nil)]
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

      ($Left msg)
      (if (.equals "[Reader Error] EOF" msg)
        (return* state unit-tag)
        (fail* msg)))))

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
    \. "_PERIOD_"
    \, "_COMMA_"
    \< "_LT_"
    \> "_GT_"
    \~ "_TILDE_"
    \| "_PIPE_"
    ;; default
    char))

(defn normalize-name [ident]
  (reduce str "" (map normalize-char ident)))

(def loader
  (fn [state]
    (return* state (->> state (get$ $host) (get$ $loader)))))

(def classes
  (fn [state]
    (return* state (->> state (get$ $host) (get$ $classes)))))

(def +init-bindings+
  (T [;; "lux;counter"
      0
      ;; "lux;mappings"
      (|table)]))

(defn env [name]
  (T [;; "lux;name"
      name
      ;; "lux;inner-closures"
      0
      ;; "lux;locals"
      +init-bindings+
      ;; "lux;closure"
      +init-bindings+]
     ))

(let [define-class (doto (.getDeclaredMethod java.lang.ClassLoader "defineClass" (into-array [String
                                                                                              (class (byte-array []))
                                                                                              Integer/TYPE
                                                                                              Integer/TYPE]))
                     (.setAccessible true))]
  (defn memory-class-loader [store]
    (proxy [java.lang.ClassLoader]
      []
      (findClass [^String class-name]
        (if-let [^bytes bytecode (get @store class-name)]
          (.invoke define-class this (to-array [class-name bytecode (int 0) (int (alength bytecode))]))
          (throw (IllegalStateException. (str "[Class Loader] Unknown class: " class-name))))))))

(defn host [_]
  (let [store (atom {})]
    (T [;; "lux;writer"
        $None
        ;; "lux;loader"
        (memory-class-loader store)
        ;; "lux;classes"
        store
        ;; "lux;catching"
        $Nil
        ;; "lux;module-states"
        (|table)
        ;; lux;type-env
        (|table)])))

(defn default-compiler-info [mode]
  (T [;; compiler-name
      compiler-name
      ;; compiler-version
      compiler-version
      ;; compiler-mode
      mode]
     ))

(defn init-state [mode]
  (T [;; "lux;info"
      (default-compiler-info mode)
      ;; "lux;source"
      $Nil
      ;; "lux;cursor"
      (T ["" -1 -1])
      ;; "lux;modules"
      (|table)
      ;; "lux;envs"
      $Nil
      ;; "lux;types"
      +init-bindings+
      ;; "lux;expected"
      $None
      ;; "lux;seed"
      0
      ;; "lux;host"
      (host nil)]
     ))

(defn save-module [body]
  (fn [state]
    (|case (body state)
      ($Right state* output)
      (return* (->> state*
                    (set$ $envs (get$ $envs state))
                    (set$ $source (get$ $source state)))
               output)

      ($Left msg)
      (fail* msg))))

(defn ^:private is-eval? [mode]
  "(-> CompilerMode Bool)"
  (|case mode
    ($Eval) true
    _       false))

(defn with-eval [body]
  (fn [state]
    (let [old-mode (->> state (get$ $info) (get$ $compiler-mode))]
      (|case (body (update$ $info #(set$ $compiler-mode $Eval %) state))
        ($Right state* output)
        (return* (update$ $info #(set$ $compiler-mode old-mode %) state*) output)

        ($Left msg)
        (fail* msg)))))

(def get-eval
  (fn [state]
    (return* state (->> state (get$ $info) (get$ $compiler-mode) is-eval?))))

(def get-writer
  (fn [state]
    (let [writer* (->> state (get$ $host) (get$ $writer))]
      (|case writer*
        ($Some datum)
        (return* state datum)

        _
        (fail* "Writer hasn't been set.")))))

(def get-top-local-env
  (fn [state]
    (try (let [top (|head (get$ $envs state))]
           (return* state top))
      (catch Throwable _
        (fail* "No local environment.")))))

(def gen-id
  (fn [state]
    (let [seed (get$ $seed state)]
      (return* (set$ $seed (inc seed) state) seed))))

(defn ->seq [xs]
  (|case xs
    ($Nil)
    (list)

    ($Cons x xs*)
    (cons x (->seq xs*))))

(defn ->list [seq]
  (if (empty? seq)
    $Nil
    ($Cons (first seq) (->list (rest seq)))))

(defn |repeat [n x]
  (if (> n 0)
    ($Cons x (|repeat (dec n) x))
    $Nil))

(def get-module-name
  (fn [state]
    (|case (|reverse (get$ $envs state))
      ($Nil)
      (fail* "[Analyser Error] Can't get the module-name without a module.")

      ($Cons ?global _)
      (return* state (get$ $name ?global)))))

(defn find-module [name]
  "(-> Text (Lux (Module Compiler)))"
  (fn [state]
    (if-let [module (|get name (get$ $modules state))]
      (return* state module)
      (fail* (str "Unknown module: " name)))))

(def get-current-module
  "(Lux (Module Compiler))"
  (|do [module-name get-module-name]
    (find-module module-name)))

(defn with-scope [name body]
  (fn [state]
    (let [output (body (update$ $envs #($Cons (env name) %) state))]
      (|case output
        ($Right state* datum)
        (return* (update$ $envs |tail state*) datum)
        
        _
        output))))

(defn run-state [monad state]
  (monad state))

(defn with-closure [body]
  (|do [closure-name (|do [top get-top-local-env]
                       (return (->> top (get$ $inner-closures) str)))]
    (fn [state]
      (let [body* (with-scope closure-name body)]
        (run-state body* (update$ $envs #($Cons (update$ $inner-closures inc (|head %))
                                                (|tail %))
                                  state))))))

(def get-scope-name
  (fn [state]
    (return* state (->> state (get$ $envs) (|map #(get$ $name %)) |reverse))))

(defn with-writer [writer body]
  (fn [state]
    (let [old-writer (->> state (get$ $host) (get$ $writer))
          output (body (update$ $host #(set$ $writer ($Some writer) %) state))]
      (|case output
        ($Right ?state ?value)
        (return* (update$ $host #(set$ $writer old-writer %) ?state)
                 ?value)

        _
        output))))

(defn with-expected-type [type body]
  "(All [a] (-> Type (Lux a)))"
  (fn [state]
    (let [output (body (set$ $expected ($Some type) state))]
      (|case output
        ($Right ?state ?value)
        (return* (set$ $expected (get$ $expected state) ?state)
                 ?value)

        _
        output))))

(defn with-cursor [^objects cursor body]
  "(All [a] (-> Cursor (Lux a)))"
  (|let [[_file-name _ _] cursor]
    (if (= "" _file-name)
      body
      (fn [state]
        (let [output (body (set$ $cursor cursor state))]
          (|case output
            ($Right ?state ?value)
            (return* (set$ $cursor (get$ $cursor state) ?state)
                     ?value)

            _
            output))))))

(defn with-analysis-meta [^objects cursor type body]
  "(All [a] (-> Cursor Type (Lux a)))"
  (|let [[_file-name _ _] cursor]
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
                                (set$ $cursor cursor)
                                (set$ $expected ($Some type))))]
          (|case output
            ($Right ?state ?value)
            (return* (->> ?state
                          (set$ $cursor (get$ $cursor state))
                          (set$ $expected (get$ $expected state)))
                     ?value)

            _
            output))))))

(def ensure-statement
  "(Lux Unit)"
  (fn [state]
    (|case (get$ $expected state)
      ($None)
      (return* state unit-tag)

      ($Some _)
      (fail* "[Error] All statements must be top-level forms."))))

(def cursor
  ;; (Lux Cursor)
  (fn [state]
    (return* state (get$ $cursor state))))

(defn show-ast [ast]
  (|case ast
    [_ ($BoolS ?value)]
    (pr-str ?value)

    [_ ($IntS ?value)]
    (pr-str ?value)

    [_ ($RealS ?value)]
    (pr-str ?value)

    [_ ($CharS ?value)]
    (str "#\"" (pr-str ?value) "\"")

    [_ ($TextS ?value)]
    (str "\"" ?value "\"")

    [_ ($TagS ?module ?tag)]
    (if (.equals "" ?module)
      (str "#" ?tag)
      (str "#" ?module ";" ?tag))

    [_ ($SymbolS ?module ?name)]
    (if (.equals "" ?module)
      ?name
      (str ?module ";" ?name))

    [_ ($TupleS ?elems)]
    (str "[" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) "]")

    [_ ($RecordS ?elems)]
    (str "{" (->> ?elems
                  (|map (fn [elem]
                          (|let [[k v] elem]
                            (str (show-ast k) " " (show-ast v)))))
                  (|interpose " ") (fold str "")) "}")

    [_ ($FormS ?elems)]
    (str "(" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) ")")

    _
    (assert false (prn-str 'show-ast (adt->text ast)))
    ))

(defn ident->text [ident]
  (|let [[?module ?name] ident]
    (if (= "" ?module)
      ?name
      (str ?module ";" ?name))))

(defn fold2% [f init xs ys]
  (|case [xs ys]
    [($Cons x xs*) ($Cons y ys*)]
    (|do [init* (f init x y)]
      (fold2% f init* xs* ys*))

    [($Nil) ($Nil)]
    (return init)

    [_ _]
    (fail "Lists don't match in size.")))

(defn map2% [f xs ys]
  (|case [xs ys]
    [($Cons x xs*) ($Cons y ys*)]
    (|do [z (f x y)
          zs (map2% f xs* ys*)]
      (return ($Cons z zs)))

    [($Nil) ($Nil)]
    (return $Nil)

    [_ _]
    (fail "Lists don't match in size.")))

(defn map2 [f xs ys]
  (|case [xs ys]
    [($Cons x xs*) ($Cons y ys*)]
    ($Cons (f x y) (map2 f xs* ys*))

    [_ _]
    $Nil))

(defn fold2 [f init xs ys]
  (|case [xs ys]
    [($Cons x xs*) ($Cons y ys*)]
    (and init
         (fold2 f (f init x y) xs* ys*))

    [($Nil) ($Nil)]
    init

    [_ _]
    init
    ;; (assert false)
    ))

(defn ^:private enumerate* [idx xs]
  "(All [a] (-> Int (List a) (List (, Int a))))"
  (|case xs
    ($Cons x xs*)
    ($Cons (T [idx x])
           (enumerate* (inc idx) xs*))

    ($Nil)
    xs
    ))

(defn enumerate [xs]
  "(All [a] (-> (List a) (List (, Int a))))"
  (enumerate* 0 xs))

(def modules
  "(Lux (List Text))"
  (fn [state]
    (return* state (|keys (get$ $modules state)))))

(defn when% [test body]
  "(-> Bool (Lux Unit) (Lux Unit))"
  (if test
    body
    (return unit-tag)))

(defn |at [idx xs]
  "(All [a] (-> Int (List a) (Maybe a)))"
  (|case xs
    ($Cons x xs*)
    (cond (< idx 0)
          $None

          (= idx 0)
          ($Some x)

          :else ;; > 1
          (|at (dec idx) xs*))

    ($Nil)
    $None
    ))

(defn normalize [ident]
  "(-> Ident (Lux Ident))"
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
    ($Nil)
    $None
    
    ($Cons x xs*)
    (if (= idx 0)
      ($Some ($Cons val xs*))
      (|case (|list-put (dec idx) val xs*)
        ($None)      $None
        ($Some xs**) ($Some ($Cons x xs**)))
      )))

(do-template [<flagger> <asker> <tag>]
  (do (defn <flagger> [module]
        "(-> Text (Lux Unit))"
        (fn [state]
          (let [state* (update$ $host (fn [host]
                                        (update$ $module-states
                                                 (fn [module-states]
                                                   (|put module <tag> module-states))
                                                 host))
                                state)]
            ($Right (T [state* unit-tag])))))
    (defn <asker> [module]
      "(-> Text (Lux Bool))"
      (fn [state]
        (if-let [module-state (->> state (get$ $host) (get$ $module-states) (|get module))]
          ($Right (T [state (|case module-state
                              (<tag>) true
                              _       false)]))
          ($Right (T [state false])))
        )))

  flag-active-module   active-module?   $Active
  flag-compiled-module compiled-module? $Compiled
  flag-cached-module   cached-module?   $Cached
  )

(do-template [<name> <default> <op>]
  (defn <name> [p xs]
    "(All [a] (-> (-> a Bool) (List a) Bool))"
    (|case xs
      ($Nil)
      <default>

      ($Cons x xs*)
      (<op> (p x) (|every? p xs*))))

  |every? true  and
  |any?   false or)

(defn m-comp [f g]
  "(All [a b c] (-> (-> b (Lux c)) (-> a (Lux b)) (-> a (Lux c))))"
  (fn [x]
    (|do [y (g x)]
      (f y))))

(defn with-attempt [m-value on-error]
  "(All [a] (-> (Lux a) (-> Text (Lux a)) (Lux a)))"
  (fn [state]
    (|case (m-value state)
      ($Left msg)
      ((on-error msg) state)
      
      output
      output)))

(defn |some [f xs]
  "(All [a b] (-> (-> a (Maybe b)) (List a) (Maybe b)))"
  (|case xs
    ($Nil)
    $None

    ($Cons x xs*)
    (|case (f x)
      ($None) (|some f xs*)
      output  output)
    ))

(def get-type-env
  "(Lux TypeEnv)"
  (fn [state]
    (return* state (->> state (get$ $host) (get$ $type-env)))))

(defn with-type-env [type-env body]
  "(All [a] (-> TypeEnv (Lux a) (Lux a)))"
  (fn [state]
    (|let [state* (update$ $host #(update$ $type-env (partial |++ type-env) %)
                           state)]
      (|case (body state*)
        ($Right [state** output])
        ($Right (T [(update$ $host
                             #(set$ $type-env
                                    (->> state (get$ $host) (get$ $type-env))
                                    %)
                             state**)
                    output]))

        ($Left msg)
        ($Left msg)))))

(defn |take [n xs]
  (|case (T [n xs])
    [0 _]             $Nil
    [_ ($Nil)]        $Nil
    [_ ($Cons x xs*)] ($Cons x (|take (dec n) xs*))
    ))

(defn |drop [n xs]
  (|case (T [n xs])
    [0 _]             xs
    [_ ($Nil)]        $Nil
    [_ ($Cons x xs*)] (|drop (dec n) xs*)
    ))

(defn |but-last [xs]
  (|case xs
    ($Nil)
    $Nil
    
    ($Cons x ($Nil))
    $Nil

    ($Cons x xs*)
    ($Cons x (|but-last xs*))

    _
    (assert false (adt->text xs))))

(defn |partition [n xs]
  (->> xs ->seq (partition-all n) (map ->list) ->list))

(defn add-loc [meta ^String msg]
  (if (.startsWith msg "@")
    msg
    (|let [[file line col] meta]
      (str "@ " file "," line "," col "\n" msg))))

(defn fail-with-loc [msg]
  (fn [state]
    (fail* (add-loc (get$ $cursor state) msg))))
