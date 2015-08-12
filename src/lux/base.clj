;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.base
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array))

;; [Tags]
(defmacro deftags [prefix & names]
  `(do ~@(for [[name idx] (map vector names (range (count names)))]
           `(def ~(symbol (str "$" name)) ~idx))))

;; List
(deftags ""
  "Nil"
  "Cons")

;; Maybe
(deftags ""
  "None"
  "Some")

;; Meta
(deftags ""
  "Meta")

;; Either
(deftags ""
  "Left"
  "Right")

;; AST
(deftags ""
  "BoolS"
  "IntS"
  "RealS"
  "CharS"
  "TextS"
  "SymbolS"
  "TagS"
  "FormS"
  "TupleS"
  "RecordS")

;; Type
(deftags ""
  "DataT"
  "TupleT"
  "VariantT"
  "RecordT"
  "LambdaT"
  "BoundT"
  "VarT"
  "ExT"
  "AllT"
  "AppT")

;; [Fields]
;; Binding
(deftags ""
  "counter"
  "mappings")

;; Env
(deftags ""
  "name"
  "inner-closures"
  "locals"
  "closure")

;; Host
(deftags ""
  "writer"
  "loader"
  "classes")

;; Compiler
(deftags ""
  "source"
  "cursor"
  "modules"
  "envs"
  "types"
  "expected"
  "seed"
  "eval?"
  "host")

;; Vars
(deftags "lux;"
  "Local"
  "Global")

;; Definitions
(deftags "lux;"
  "ValueD"
  "TypeD"
  "MacroD"
  "AliasD")

;; [Exports]
(def +name-separator+ ";")

(defn T [& elems]
  (to-array elems))

(defn V [^Long tag value]
  (to-array [tag value]))

(defn R [& kvs]
  (to-array kvs))

;; Constructors
(def None$ (V $None nil))
(defn Some$ [x] (V $Some x))

(def Nil$ (V $Nil nil))
(defn Cons$ [h t] (V $Cons (T h t)))

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
          `(V $Nil nil)
          (reverse elems)))

(defmacro |table [& elems]
  (reduce (fn [table [k v]]
            `(|put ~k ~v ~table))
          `(|list)
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
    (V $Cons (T (T slot value) (V $Nil nil)))
    
    ($Cons [k v] table*)
    (if (.equals ^Object k slot)
      (V $Cons (T (T slot value) table*))
      (V $Cons (T (T k v) (|put slot value table*))))

    ;; _
    ;; (assert false (prn-str '|put (aget table 0)))
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
    (assert false)

    ($Cons x _)
    x))

(defn |tail [xs]
  (|case xs
    ($Nil)
    (assert false)

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
(defn |cons [head tail]
  (V $Cons (T head tail)))

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
  (|case xs
    ($Nil)
    true

    ($Cons _ _)
    false))

(defn |filter [p xs]
  (|case xs
    ($Nil)
    xs

    ($Cons x xs*)
    (if (p x)
      (V $Cons (T x (|filter p xs*)))
      (|filter p xs*))))

(defn flat-map [f xs]
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
        (T (|cons x pre) post))
      (T (V $Nil nil) xs))))

(defn |contains? [k table]
  (|case table
    ($Nil)
    false

    ($Cons [k* _] table*)
    (or (.equals ^Object k k*)
        (|contains? k table*))))

(defn fold [f init xs]
  (|case xs
    ($Nil)
    init

    ($Cons x xs*)
    (fold f (f init x) xs*)))

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
    (|cons init (folds f (f init x) xs*))))

(defn |length [xs]
  (fold (fn [acc _] (inc acc)) 0 xs))

(let [|range* (fn |range* [from to]
                (if (< from to)
                  (V $Cons (T from (|range* (inc from) to)))
                  (V $Nil nil)))]
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
    (V $Nil nil)))

(defn |keys [plist]
  (|case plist
    ($Nil)
    (|list)
    
    ($Cons [k v] plist*)
    (|cons k (|keys plist*))))

(defn |vals [plist]
  (|case plist
    ($Nil)
    (|list)
    
    ($Cons [k v] plist*)
    (|cons v (|vals plist*))))

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

  map%      |cons
  flat-map% |++)

(defn list-join [xss]
  (fold |++ (V $Nil nil) xss))

(defn |as-pairs [xs]
  (|case xs
    ($Cons x ($Cons y xs*))
    (V $Cons (T (T x y) (|as-pairs xs*)))

    _
    (V $Nil nil)))

(defn |reverse [xs]
  (fold (fn [tail head]
          (|cons head tail))
        (|list)
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
                     (return (|cons head tail)))
                   (return (|list)))))

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
  (R ;; "lux;counter"
   0
   ;; "lux;mappings"
   (|table)))

(defn env [name]
  (R ;; "lux;name"
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
              (prn 'InvocationTargetException (.getCause e))
              (throw e)))
          (do (prn 'memory-class-loader/store class-name (keys @store))
            (throw (IllegalStateException. (str "[Class Loader] Unknown class: " class-name)))))))))

(defn host [_]
  (let [store (atom {})]
    (R ;; "lux;writer"
     (V $None nil)
     ;; "lux;loader"
     (memory-class-loader store)
     ;; "lux;classes"
     store)))

(defn init-state [_]
  (R ;; "lux;source"
   (V $None nil)
   ;; "lux;cursor"
   (T "" -1 -1)
   ;; "lux;modules"
   (|table)
   ;; "lux;envs"
   (|list)
   ;; "lux;types"
   +init-bindings+
   ;; "lux;expected"
   (V $VariantT (|list))
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
    (|list)
    (|cons (first seq) (->list (rest seq)))))

(defn |repeat [n x]
  (if (> n 0)
    (|cons x (|repeat (dec n) x))
    (|list)))

(def get-module-name
  (fn [state]
    (|case (|reverse (get$ $envs state))
      ($Nil)
      (fail* "[Analyser Error] Can't get the module-name without a module.")

      ($Cons ?global _)
      (return* state (get$ $name ?global)))))

(defn with-scope [name body]
  (fn [state]
    (let [output (body (update$ $envs #(|cons (env name) %) state))]
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
        (run-state body* (update$ $envs #(|cons (update$ $inner-closures inc (|head %))
                                                (|tail %))
                                  state))))))

(def get-scope-name
  (fn [state]
    (return* state (->> state (get$ $envs) (|map #(get$ $name %)) |reverse))))

(defn with-writer [writer body]
  (fn [state]
    (let [output (body (update$ $host #(set$ $writer (V $Some writer) %) state))]
      (|case output
        ($Right ?state ?value)
        (return* (update$ $host #(set$ $writer (->> state (get$ $host) (get$ $writer)) %) ?state)
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

(defn show-ast [ast]
  ;; (prn 'show-ast/GOOD (aget ast 0) (aget ast 1 1 0))
  (|case ast
    ($Meta _ ($BoolS ?value))
    (pr-str ?value)

    ($Meta _ ($IntS ?value))
    (pr-str ?value)

    ($Meta _ ($RealS ?value))
    (pr-str ?value)

    ($Meta _ ($CharS ?value))
    (pr-str ?value)

    ($Meta _ ($TextS ?value))
    (str "\"" ?value "\"")

    ($Meta _ ($TagS ?module ?tag))
    (str "#" ?module ";" ?tag)

    ($Meta _ ($SymbolS ?module ?ident))
    (if (.equals "" ?module)
      ?ident
      (str ?module ";" ?ident))

    ($Meta _ ($TupleS ?elems))
    (str "[" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) "]")

    ($Meta _ ($RecordS ?elems))
    (str "{" (->> ?elems
                  (|map (fn [elem]
                          (|let [[k v] elem]
                            (str (show-ast k) " " (show-ast v)))))
                  (|interpose " ") (fold str "")) "}")

    ($Meta _ ($FormS ?elems))
    (str "(" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) ")")

    ;; _
    ;; (assert false (prn-str 'show-ast (aget ast 0) (aget ast 1 1 0)))
    ;; (assert false (prn-str 'show-ast (aget ast 0) (aget ast 1 1 0)))
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
      (return (|cons z zs)))

    [($Nil) ($Nil)]
    (return (V $Nil nil))

    [_ _]
    (fail "Lists don't match in size.")))

(defn map2 [f xs ys]
  (|case [xs ys]
    [($Cons x xs*) ($Cons y ys*)]
    (|cons (f x y) (map2 f xs* ys*))

    [_ _]
    (V $Nil nil)))

(defn fold2 [f init xs ys]
  (|case [xs ys]
    [($Cons x xs*) ($Cons y ys*)]
    (and init
         (fold2 f (f init x y) xs* ys*))

    [($Nil) ($Nil)]
    init

    [_ _]
    false))

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

;; (defn |list-put [idx val xs]
;;   (|case [idx xs]
;;     [_ ($Nil)]
;;     (V $None nil)
    
;;     [0 ($Cons x xs*)]
;;     (V $Some (V $Cons (T val xs*)))
    
;;     [_ ($Cons x xs*)]
;;     (|case (|list-put idx val xs*)
;;       ($None)      (V $None nil)
;;       ($Some xs**) (V $Some (V $Cons (T x xs**))))))

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
