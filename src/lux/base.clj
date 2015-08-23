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

;; [ADTs]
(let [array-class (class (to-array []))]
  (defn adt->text [adt]
    (if (= array-class (class adt))
      (str "[" (->> adt (map adt->text) (interpose " ") (reduce str "")) "]")
      (pr-str adt))))

(defmacro deftags [names]
  (assert (vector? names))
  `(do ~@(for [[name idx] (map vector names (range (count names)))]
           `(def ~(symbol (str "$" name)) (int ~idx)))))

(defn ^:private unfold-accesses
  ([elems]
     (unfold-accesses 1 (count elems) elems))
  ([begin end elems]
     (if (= begin end)
       (list elems)
       (cons (take begin elems)
             (unfold-accesses (inc begin) end elems)))))

(defmacro defrtags [tags]
  (let [num-tags (count tags)
        normals (butlast tags)
        special (last tags)
        tags+locs (cons [special (repeat (dec num-tags) 1)]
                        (map #(vector %1 (concat (repeat %2 1) [0]))
                             normals
                             (range num-tags)))]
    `(do ~@(for [[tag loc] tags+locs
                 :let [getter (symbol (str "$get-" tag))
                       setter (symbol (str "$set-" tag))
                       updater (symbol (str "$update-" tag))
                       record (gensym "record")
                       value (gensym "value")]]
             `(do (defn ~getter [~record]
                    ;; (if (= '~'$get-source '~getter)
                    ;;   (prn '~getter '~loc ~record (aget ~record ~@loc))
                    ;;   (prn '~getter '~loc ~record (adt->text (aget ~record ~@loc))))
                    (aget ~record ~@loc))
                (defn ~setter [~value ~record]
                  ;; (if (= '~'$set-source '~setter)
                  ;;   (prn '~setter '_1 '~loc ~record)
                  ;;   (prn '~setter '_2 '~loc ~record (adt->text ~value)))
                  ;; (doto record#
                  ;;   (aset ~@loc value#))
                  ;; (doto record#
                  ;;   (aset 1 (doto (aget record# 1)
                  ;;             (aset 1 ...))))
                  ~(reduce (fn [inner indices]
                             `(doto (aclone ~(if (= 1 (count indices))
                                               record
                                               `(aget ~record ~@(butlast indices))))
                                (aset ~(last indices) ~inner)))
                           value
                           (reverse (unfold-accesses loc)))
                  )
                (defn ~updater [f# ~record]
                  ;; (prn '~updater '~loc ~record)
                  ;; (doto record#
                  ;;   (aset ~@loc (f# (aget record# ~@loc))))
                  (~setter (f# (~getter ~record)) ~record)))))
    ))

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
  ["VoidT"
   "UnitT"
   "SumT"
   "ProdT"
   "DataT"
   "LambdaT"
   "BoundT"
   "VarT"
   "ExT"
   "AllT"
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
(defrtags
  ["counter"
   "mappings"])

;; Env
(defrtags
  ["name"
   "inner-closures"
   "locals"
   "closure"])

;; Host
(defrtags
  ["writer"
   "loader"
   "classes"])

;; Compiler
(defrtags
  ["source"
   "cursor"
   "modules"
   "envs"
   "type-vars"
   "expected"
   "seed"
   "eval?"
   "host"])

;; [Exports]
;; Class fields
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

(def prelude-name "lux")

(defmacro $$ [op & args]
  (assert (> (count args) 1)
          (prn-str '$$ op args))
  (let [[last & others] (reverse args)]
    (reduce (fn [right left] `(~op ~left ~right))
            last
            others)))

(defn S [^Long tag value]
  (to-array [tag value]))

(defn P [left right]
  (to-array [left right]))

;; Constructors
(def None$ (S $None nil))
(defn Some$ [x] (S $Some x))

(def Nil$ (S $Nil nil))
(defn Cons$ [h t] (S $Cons (P h t)))

(defn fail* [message]
  (S $Left message))

(defn return* [state value]
  (S $Right (P state value)))

(defn ^:private transform-tuple-pattern [pattern]
  (case (count pattern)
    0 '_
    1 (assert false "Can't have singleton tuples.")
    2 pattern
    ;; else
    (let [[last & others] (reverse pattern)]
      (reduce (fn [r l] [l r]) last others))))

(defn transform-pattern [pattern]
  (cond (vector? pattern) (transform-tuple-pattern (mapv transform-pattern pattern))
        (seq? pattern) (let [parts (mapv transform-pattern (rest pattern))]
                         (vec (cons (eval (first pattern))
                                    (list (case (count parts)
                                            1 (first parts)
                                            ;; else
                                            (transform-tuple-pattern parts))))))
        :else pattern
        ))

(defmacro |case [value & branches]
  (assert (= 0 (mod (count branches) 2)))
  (let [value* (if (vector? value)
                 [`($$ P ~@value)]
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
            `(Cons$ ~head ~tail))
          `Nil$
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
  ;; (prn '|put slot (adt->text value) (adt->text table))
  (|case table
    ($Nil)
    (Cons$ (P slot value) Nil$)
    
    ($Cons [k v] table*)
    (if (.equals ^Object k slot)
      (Cons$ (P slot value) table*)
      (Cons$ (P k v) (|put slot value table*)))

    ;; _
    ;; (assert false (prn-str '|put slot (adt->text value) (adt->text table)))
    ))

(defn |remove [slot table]
  (|case table
    ($Nil)
    table
    
    ($Cons [k v] table*)
    (if (.equals ^Object k slot)
      table*
      (Cons$ (P k v) (|remove slot table*)))))

(defn |update [k f table]
  (|case table
    ($Nil)
    table

    ($Cons [k* v] table*)
    (if (.equals ^Object k k*)
      (Cons$ (P k* (f v)) table*)
      (Cons$ (P k* v) (|update k f table*)))))

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
    (S $Left message)))

(defn return [value]
  (fn [state]
    (S $Right (P state value))))

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
    (Cons$ x (|++ xs* ys))))

(defn |map [f xs]
  (|case xs
    ($Nil)
    xs

    ($Cons x xs*)
    (Cons$ (f x) (|map f xs*))

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
      (Cons$ x (|filter p xs*))
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
    (P xs xs)

    ($Cons x xs*)
    (if (p x)
      (|let [[pre post] (|split-with p xs*)]
        (P (Cons$ x pre) post))
      (P Nil$ xs))))

(defn |contains? [k table]
  (|case table
    ($Nil)
    false

    ($Cons [k* _] table*)
    (or (.equals ^Object k k*)
        (|contains? k table*))

    _
    (assert false (prn-str '|contains? k (adt->text table)))))

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
    (Cons$ init (folds f (f init x) xs*))))

(defn |length [xs]
  (fold (fn [acc _] (inc acc)) 0 xs))

(let [|range* (fn |range* [from to]
                (if (< from to)
                  (Cons$ from (|range* (inc from) to))
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
    (Cons$ (P x y) (zip2 xs* ys*))

    [_ _]
    Nil$))

(defn |keys [plist]
  (|case plist
    ($Nil)
    (|list)
    
    ($Cons [k v] plist*)
    (Cons$ k (|keys plist*))))

(defn |vals [plist]
  (|case plist
    ($Nil)
    (|list)
    
    ($Cons [k v] plist*)
    (Cons$ v (|vals plist*))))

(defn |interpose [sep xs]
  (|case xs
    ($Nil)
    xs

    ($Cons _ ($Nil))
    xs
    
    ($Cons x xs*)
    (Cons$ x (Cons$ sep (|interpose sep xs*)))))

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
    (Cons$ (P x y) (|as-pairs xs*))

    _
    Nil$))

(defn |reverse [xs]
  (fold (fn [tail head]
          (Cons$ head tail))
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
                     (return (Cons$ head tail)))
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
    (return* state (->> state $get-host ($get-loader)))))

(def classes
  (fn [state]
    (return* state (->> state $get-host ($get-classes)))))

(def +init-bindings+
  (P ;; "lux;counter"
   0
   ;; "lux;mappings"
   (|table)))

(defn env [name]
  ($$ P ;; "lux;name"
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
    ($$ P ;; "lux;writer"
        None$
        ;; "lux;loader"
        (memory-class-loader store)
        ;; "lux;classes"
        store)))

(defn init-state [_]
  ($$ P ;; "lux;source"
      None$
      ;; "lux;cursor"
      ($$ P "" -1 -1)
      ;; "lux;modules"
      (|table)
      ;; "lux;envs"
      (|list)
      ;; "lux;types"
      +init-bindings+
      ;; "lux;expected"
      (S $VoidT nil)
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
                    ($set-envs ($get-envs state))
                    ($set-source ($get-source state)))
               output)

      ($Left msg)
      (fail* msg))))

(defn with-eval [body]
  (fn [state]
    (|case (body ($set-eval? true state))
      ($Right state* output)
      (return* ($set-eval? ($get-eval? state) state*) output)

      ($Left msg)
      (fail* msg))))

(def get-eval
  (fn [state]
    (return* state ($get-eval? state))))

(def get-writer
  (fn [state]
    (let [writer* (->> state ($get-host) ($get-writer))]
      (|case writer*
        ($Some datum)
        (return* state datum)

        _
        (fail* "Writer hasn't been set.")))))

(def get-top-local-env
  (fn [state]
    (try (let [top (|head ($get-envs state))]
           (return* state top))
      (catch Throwable _
        (fail* "No local environment.")))))

(def gen-id
  (fn [state]
    (let [seed ($get-seed state)]
      (return* ($set-seed (inc seed) state) seed))))

(defn ->seq [xs]
  (|case xs
    ($Nil)
    (list)

    ($Cons x xs*)
    (cons x (->seq xs*))))

(defn ->list [seq]
  (if (empty? seq)
    (|list)
    (Cons$ (first seq) (->list (rest seq)))))

(defn |repeat [n x]
  (if (> n 0)
    (Cons$ x (|repeat (dec n) x))
    (|list)))

(def get-module-name
  (fn [state]
    (|case (|reverse ($get-envs state))
      ($Nil)
      (fail* "[Analyser Error] Can't get the module-name without a module.")

      ($Cons ?global _)
      (return* state ($get-name ?global)))))

(defn find-module [name]
  "(-> Text (Lux (Module Compiler)))"
  (fn [state]
    (if-let [module (|get name ($get-modules state))]
      (return* state module)
      (fail* (str "Unknown module: " name)))))

(def get-current-module
  "(Lux (Module Compiler))"
  (|do [module-name get-module-name]
    (find-module module-name)))

(defn with-scope [name body]
  (fn [state]
    (let [output (body ($update-envs #(Cons$ (env name) %) state))]
      (|case output
        ($Right state* datum)
        (return* ($update-envs |tail state*) datum)
        
        _
        output))))

(defn run-state [monad state]
  (monad state))

(defn with-closure [body]
  (|do [closure-name (|do [top get-top-local-env]
                       (return (->> top ($get-inner-closures) str)))]
    (fn [state]
      (let [body* (with-scope closure-name body)]
        (run-state body* ($update-envs #(Cons$ ($update-inner-closures inc (|head %))
                                               (|tail %))
                                       state))))))

(def get-scope-name
  (fn [state]
    (return* state (->> state ($get-envs) (|map #($get-name %)) |reverse))))

(defn with-writer [writer body]
  (fn [state]
    ;; (prn 'with-writer writer body)
    (let [output (body ($update-host #($set-writer (Some$ writer) %) state))]
      (|case output
        ($Right ?state ?value)
        (return* ($update-host #($set-writer (->> state ($get-host) ($get-writer)) %) ?state)
                 ?value)

        _
        output))))

(defn with-expected-type [type body]
  "(All [a] (-> Type (Lux a)))"
  (fn [state]
    ;; (prn 'with-expected-type type state)
    (let [output (body ($set-expected type state))]
      (|case output
        ($Right ?state ?value)
        (return* ($set-expected ($get-expected state) ?state)
                 ?value)

        _
        output))))

(defn with-cursor [^objects cursor body]
  "(All [a] (-> Cursor (Lux a)))"
  ;; (prn 'with-cursor/_0 (adt->text cursor))
  (if (= "" (aget cursor 0))
    body
    (fn [state]
      (let [;; _ (prn 'with-cursor/_1 cursor)
            state* ($set-cursor cursor state)
            ;; _ (prn 'with-cursor/_2 state*)
            output (body state*)]
        (|case output
          ($Right ?state ?value)
          (let [?state* ($set-cursor ($get-cursor state) ?state)]
            ;; (prn 'with-cursor/_3 ?state*)
            (return* ?state*
                     ?value))

          _
          output)))))

(defn show-ast [ast]
  ;; (prn 'show-ast/GOOD (aget ast 0) (aget ast 1 1 0))
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
    (str "#" ?module ";" ?tag)

    [_ ($SymbolS ?module ?ident)]
    (if (.equals "" ?module)
      ?ident
      (str ?module ";" ?ident))

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
    false))

(defn ^:private enumerate* [idx xs]
  "(All [a] (-> Int (List a) (List (, Int a))))"
  (|case xs
    ($Cons x xs*)
    (Cons$ (P idx x)
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
    (return* state (|keys ($get-modules state)))))

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
          None$

          (= idx 0)
          (Some$ x)

          :else ;; > 1
          (|at (dec idx) xs*))

    ($Nil)
    None$
    ))

(defn normalize [ident]
  "(-> Ident (Lux Ident))"
  (|case ident
    ["" name] (|do [module get-module-name]
                (return (P module name)))
    _ (return ident)))

(defn ident= [x y]
  (|let [[xmodule xname] x
         [ymodule yname] y]
    (and (= xmodule ymodule)
         (= xname yname))))

(defn |list-put [idx val xs]
  (|case xs
    ($Nil)
    None$
    
    ($Cons x xs*)
    (if (= idx 0)
      (Some$ (Cons$ val xs*))
      (|case (|list-put (dec idx) val xs*)
        ($None)      None$
        ($Some xs**) (Some$ (Cons$ x xs**)))
      )))

(defn ensure-1 [m-value]
  (|do [output m-value]
    (|case output
      ($Cons x ($Nil))
      (return x)

      _
      (fail "[Error] Can't expand to other than 1 element."))))

(defn cursor$ [file-name line-num column-num]
  ($$ P file-name line-num column-num))
