;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.base
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array))

;; [Tags]
(defmacro deftags [names]
  (assert (vector? names))
  `(do ~@(for [[name idx] (map vector names (range (count names)))]
           `(def ~(symbol (str "$" name)) ~idx))))

;; List
(deftags
  ["Nil"
   "Cons"])

;; Maybe
(deftags
  ["None"
   "Some"])

;; Either
(deftags
  ["Left"
   "Right"])

;; AST
(deftags
  ["BoolS"
   "IntS"
   "RealS"
   "CharS"
   "TextS"
   "SymbolS"
   "TagS"
   "FormS"
   "TupleS"
   "RecordS"])

;; Type
(deftags
  ["DataT"
   "VariantT"
   "TupleT"
   "LambdaT"
   "BoundT"
   "VarT"
   "ExT"
   "UnivQ"
   "ExQ"
   "AppT"
   "NamedT"])

;; Vars
(deftags
  ["Local"
   "Global"])

;; Definitions
(deftags
  ["ValueD"
   "TypeD"
   "MacroD"
   "AliasD"])

;; Binding
(deftags
  ["counter"
   "mappings"])

;; Env
(deftags
  ["name"
   "inner-closures"
   "locals"
   "closure"])

;; ModuleState
(deftags
  ["Active"
   "Compiled"
   "Cached"])

;; Host
(deftags
  ["writer"
   "loader"
   "classes"
   "catching"
   "module-states"])

;; Compiler
(deftags
  ["source"
   "cursor"
   "modules"
   "envs"
   "type-vars"
   "expected"
   "seed"
   "eval?"
   "host"])

;; Compiler
(deftags
  ["GenericTypeVar"
   "GenericClass"
   "GenericArray"])

;; Methods
(deftags
  ["ConstructorMethodSyntax"
   "VirtualMethodSyntax"
   "OverridenMethodSyntax"])

(deftags
  ["ConstructorMethodAnalysis"
   "VirtualMethodAnalysis"
   "OverridenMethodAnalysis"])

;; [Exports]
(def datum-field "_datum")
(def meta-field "_meta")
(def name-field "_name")
(def hash-field "_hash")
(def compiler-field "_compiler")
(def imports-field "_imports")
(def defs-field "_defs")
(def eval-field "_eval")
(def tags-field "_tags")
(def module-class-name "_")
(def +name-separator+ ";")
(def lib-dir "lib")

(defn T [& elems]
  (to-array elems))

(defn V [^Long tag value]
  (to-array [tag value]))

;; Constructors
(def None$ (V $None nil))
(defn Some$ [x] (V $Some x))

(def Nil$ (V $Nil nil))
(defn Cons$ [h t] (V $Cons (T h t)))

(def empty-cursor (T "" -1 -1))

(defn get$ [slot ^objects record]
  (aget record slot))

(defn set$ [slot value ^objects record]
  (let [record* (aclone record)
        size (alength record)]
    (aset record* slot value)
    record*))

(defmacro update$ [slot f record]
  `(let [record# ~record]
     (set$ ~slot (~f (get$ ~slot record#))
           record#)))

(defn fail* [message]
  (V $Left message))

(defn return* [state value]
  (V $Right (T state value)))

(defn transform-pattern [pattern]
  (cond (vector? pattern) (mapv transform-pattern pattern)
        (seq? pattern) (let [parts (mapv transform-pattern (rest pattern))]
                         (vec (cons (eval (first pattern))
                                    (list (case (count parts)
                                            0 '_
                                            1 (first parts)
                                            ;; else
                                            `[~@parts])))))
        :else pattern
        ))

(defmacro |case [value & branches]
  (assert (= 0 (mod (count branches) 2)))
  (let [value* (if (vector? value)
                 [`(T ~@value)]
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
            `(V $Cons (T ~head ~tail)))
          `Nil$
          (reverse elems)))

(defmacro |table [& elems]
  (reduce (fn [table [k v]]
            `(|put ~k ~v ~table))
          `Nil$
          (reverse (partition 2 elems))))

(defn |get [slot table]
  (|case table
    ($Nil)
    nil
    
    ($Cons [k v] table*)
    (if (.equals ^Object k slot)
      v
      (|get slot table*))))

(defn |put [slot value table]
  (|case table
    ($Nil)
    (V $Cons (T (T slot value) Nil$))
    
    ($Cons [k v] table*)
    (if (.equals ^Object k slot)
      (V $Cons (T (T slot value) table*))
      (V $Cons (T (T k v) (|put slot value table*))))
    ))

(defn |remove [slot table]
  (|case table
    ($Nil)
    table
    
    ($Cons [k v] table*)
    (if (.equals ^Object k slot)
      table*
      (V $Cons (T (T k v) (|remove slot table*))))))

(defn |update [k f table]
  (|case table
    ($Nil)
    table

    ($Cons [k* v] table*)
    (if (.equals ^Object k k*)
      (V $Cons (T (T k* (f v)) table*))
      (V $Cons (T (T k* v) (|update k f table*))))))

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
    (V $Left message)))

(defn return [value]
  (fn [state]
    (V $Right (T state value))))

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
(defn |++ [xs ys]
  (|case xs
    ($Nil)
    ys

    ($Cons x xs*)
    (V $Cons (T x (|++ xs* ys)))))

(let [array-class (class (to-array []))]
  (defn adt->text [adt]
    (if (= array-class (class adt))
      (str "[" (->> adt (map adt->text) (interpose " ") (reduce str "")) "]")
      (pr-str adt))))

(defn |map [f xs]
  (|case xs
    ($Nil)
    xs

    ($Cons x xs*)
    (V $Cons (T (f x) (|map f xs*)))

    _
    (assert false (prn-str '|map f (adt->text xs)))
    ))

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
      (V $Cons (T x (|filter p xs*)))
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
    (T xs xs)

    ($Cons x xs*)
    (if (p x)
      (|let [[pre post] (|split-with p xs*)]
        (T (Cons$ x pre) post))
      (T Nil$ xs))))

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
    (Cons$ init (folds f (f init x) xs*))))

(defn |length [xs]
  (fold (fn [acc _] (inc acc)) 0 xs))

(let [|range* (fn |range* [from to]
                (if (< from to)
                  (V $Cons (T from (|range* (inc from) to)))
                  Nil$))]
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
    (V $Cons (T (T x y) (zip2 xs* ys*)))

    [_ _]
    Nil$))

(defn |keys [plist]
  (|case plist
    ($Nil)
    Nil$
    
    ($Cons [k v] plist*)
    (Cons$ k (|keys plist*))))

(defn |vals [plist]
  (|case plist
    ($Nil)
    Nil$
    
    ($Cons [k v] plist*)
    (Cons$ v (|vals plist*))))

(defn |interpose [sep xs]
  (|case xs
    ($Nil)
    xs

    ($Cons _ ($Nil))
    xs
    
    ($Cons x xs*)
    (V $Cons (T x (V $Cons (T sep (|interpose sep xs*)))))))

(do-template [<name> <joiner>]
  (defn <name> [f xs]
    (|case xs
      ($Nil)
      (return xs)

      ($Cons x xs*)
      (|do [y (f x)
            ys (<name> f xs*)]
        (return (<joiner> y ys)))))

  map%      Cons$
  flat-map% |++)

(defn list-join [xss]
  (fold |++ Nil$ xss))

(defn |as-pairs [xs]
  (|case xs
    ($Cons x ($Cons y xs*))
    (V $Cons (T (T x y) (|as-pairs xs*)))

    _
    Nil$))

(defn |reverse [xs]
  (fold (fn [tail head]
          (Cons$ head tail))
        Nil$
        xs))

(defn assert! [test message]
  (if test
    (return nil)
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

(defn repeat% [monad]
  (try-all% (|list (|do [head monad
                         tail (repeat% monad)]
                     (return (Cons$ head tail)))
                   (return Nil$))))

(defn exhaust% [step]
  (fn [state]
    (|case (step state)
      ($Right state* _)
      ((exhaust% step) state*)

      ($Left msg)
      (if (.equals "[Reader Error] EOF" msg)
        (return* state nil)
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
  (T ;; "lux;counter"
   0
   ;; "lux;mappings"
   (|table)))

(defn env [name]
  (T ;; "lux;name"
   name
   ;; "lux;inner-closures"
   0
   ;; "lux;locals"
   +init-bindings+
   ;; "lux;closure"
   +init-bindings+
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
        ;; (prn 'findClass class-name)
        (if-let [^bytes bytecode (get @store class-name)]
          (try (.invoke define-class this (to-array [class-name bytecode (int 0) (int (alength bytecode))]))
            (catch java.lang.reflect.InvocationTargetException e
              ;; (prn 'InvocationTargetException (.getCause e))
              ;; (prn 'InvocationTargetException (.getTargetException e))
              ;; (prn 'memory-class-loader/findClass class-name (get @store class-name))
              (throw e)))
          (do ;; (prn 'memory-class-loader/store class-name (keys @store))
              (throw (IllegalStateException. (str "[Class Loader] Unknown class: " class-name)))))))))

;; (deftype Host
;;   (& #writer         (^ org.objectweb.asm.ClassWriter)
;;      #loader         (^ java.net.URLClassLoader)
;;      #classes        (^ clojure.lang.Atom)
;;      #catching       (List Text)
;;      #module-states  (List (, Text ModuleState))))
(defn host [_]
  (let [store (atom {})]
    (T ;; "lux;writer"
     (V $None nil)
     ;; "lux;loader"
     (memory-class-loader store)
     ;; "lux;classes"
     store
     ;; "lux;catching"
     Nil$
     ;; "lux;module-states"
     (|table)
     )))

(defn init-state [_]
  (T ;; "lux;source"
   (V $None nil)
   ;; "lux;cursor"
   (T "" -1 -1)
   ;; "lux;modules"
   (|table)
   ;; "lux;envs"
   Nil$
   ;; "lux;types"
   +init-bindings+
   ;; "lux;expected"
   (V $VariantT Nil$)
   ;; "lux;seed"
   0
   ;; "lux;eval?"
   false
   ;; "lux;host"
   (host nil)
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

(defn with-eval [body]
  (fn [state]
    (|case (body (set$ $eval? true state))
      ($Right state* output)
      (return* (set$ $eval? (get$ $eval? state) state*) output)

      ($Left msg)
      (fail* msg))))

(def get-eval
  (fn [state]
    (return* state (get$ $eval? state))))

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
    Nil$
    (Cons$ (first seq) (->list (rest seq)))))

(defn |repeat [n x]
  (if (> n 0)
    (Cons$ x (|repeat (dec n) x))
    Nil$))

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
    (let [output (body (update$ $envs #(Cons$ (env name) %) state))]
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
        (run-state body* (update$ $envs #(Cons$ (update$ $inner-closures inc (|head %))
                                                (|tail %))
                                  state))))))

(def get-scope-name
  (fn [state]
    (return* state (->> state (get$ $envs) (|map #(get$ $name %)) |reverse))))

(defn with-writer [writer body]
  (fn [state]
    (let [old-writer (->> state (get$ $host) (get$ $writer))
          output (body (update$ $host #(set$ $writer (V $Some writer) %) state))]
      (|case output
        ($Right ?state ?value)
        (return* (update$ $host #(set$ $writer old-writer %) ?state)
                 ?value)

        _
        output))))

(defn with-expected-type [type body]
  "(All [a] (-> Type (Lux a)))"
  (fn [state]
    (let [output (body (set$ $expected type state))]
      (|case output
        ($Right ?state ?value)
        (return* (set$ $expected (get$ $expected state) ?state)
                 ?value)

        _
        output))))

(defn with-cursor [^objects cursor body]
  "(All [a] (-> Cursor (Lux a)))"
  (if (= "" (aget cursor 0))
    body
    (fn [state]
      (let [output (body (set$ $cursor cursor state))]
        (|case output
          ($Right ?state ?value)
          (return* (set$ $cursor (get$ $cursor state) ?state)
                   ?value)

          _
          output)))))

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
    (pr-str ?value)

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
    (str ?module ";" ?name)))

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
      (return (Cons$ z zs)))

    [($Nil) ($Nil)]
    (return Nil$)

    [_ _]
    (fail "Lists don't match in size.")))

(defn map2 [f xs ys]
  (|case [xs ys]
    [($Cons x xs*) ($Cons y ys*)]
    (Cons$ (f x y) (map2 f xs* ys*))

    [_ _]
    Nil$))

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
    (V $Cons (T (T idx x)
                (enumerate* (inc idx) xs*)))

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
  "(-> Bool (Lux (,)) (Lux (,)))"
  (if test
    body
    (return nil)))

(defn |at [idx xs]
  "(All [a] (-> Int (List a) (Maybe a)))"
  ;; (prn '|at idx (aget idx 0))
  (|case xs
    ($Cons x xs*)
    (cond (< idx 0)
          (V $None nil)

          (= idx 0)
          (V $Some x)

          :else ;; > 1
          (|at (dec idx) xs*))

    ($Nil)
    (V $None nil)
    ))

(defn normalize [ident]
  "(-> Ident (Lux Ident))"
  (|case ident
    ["" name] (|do [module get-module-name]
                (return (T module name)))
    _ (return ident)))

(defn ident= [x y]
  (|let [[xmodule xname] x
         [ymodule yname] y]
    (and (= xmodule ymodule)
         (= xname yname))))

(defn |list-put [idx val xs]
  (|case xs
    ($Nil)
    (V $None nil)
    
    ($Cons x xs*)
    (if (= idx 0)
      (V $Some (V $Cons (T val xs*)))
      (|case (|list-put (dec idx) val xs*)
        ($None)      (V $None nil)
        ($Some xs**) (V $Some (V $Cons (T x xs**))))
      )))

(do-template [<flagger> <asker> <tag>]
  (do (defn <flagger> [module]
        "(-> Text (Lux (,)))"
        (fn [state]
          (let [state* (update$ $host (fn [host]
                                        (update$ $module-states
                                                 (fn [module-states]
                                                   (|put module (V <tag> nil) module-states))
                                                 host))
                                state)]
            (V $Right (T state* nil)))))
    (defn <asker> [module]
      "(-> Text (Lux Bool))"
      (fn [state]
        (if-let [module-state (->> state (get$ $host) (get$ $module-states) (|get module))]
          (V $Right (T state (|case module-state
                               (<tag>) true
                               _       false)))
          (V $Right (T state false)))
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
    None$

    ($Cons x xs*)
    (|case (f x)
      ($None) (|some f xs*)
      output  output)
    ))
