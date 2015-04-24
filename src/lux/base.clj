(ns lux.base
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array))

;; [Fields]
;; Binding
(def $COUNTER 0)
(def $MAPPINGS 1)

;; Env
(def $CLOSURE 0)
(def $INNER-CLOSURES 1)
(def $LOCALS 2)
(def $NAME 3)

;; Host
(def $EVAL-CTOR 0)
(def $LOADER 1)
(def $WRITER 2)

;; CompilerState
(def $ENVS 0)
(def $HOST 1)
(def $MODULE-ALIASES 2)
(def $MODULES 3)
(def $SOURCE 4)
(def $TYPES 5)

;; [Exports]
(def +name-separator+ ";")

(defn T [& elems]
  (to-array elems))

(defn V [tag value]
  (to-array [tag value]))

(defn R [& kvs]
  (to-array kvs))

(defn get$ [slot record]
  (aget record slot))

(defn set$ [slot value record]
  (let [record* (aclone record)
        size (alength record)]
    (aset record* slot value)
    record*))

(defmacro update$ [slot f record]
  `(let [record# ~record]
     (set$ ~slot (~f (get$ ~slot record#))
           record#)))

(defn fail* [message]
  (V "lux;Left" message))

(defn return* [state value]
  (V "lux;Right" (T state value)))

(defmacro |let [bindings body]
  (reduce (fn [inner [left right]]
            `(matchv ::M/objects [~right]
               [~left]
               ~inner))
          body
          (reverse (partition 2 bindings))))

(defmacro |list [& elems]
  (reduce (fn [tail head]
            `(V "lux;Cons" (T ~head ~tail)))
          `(V "lux;Nil" nil)
          (reverse elems)))

(defmacro |table [& elems]
  (reduce (fn [table [k v]]
            `(|put ~k ~v ~table))
          `(|list)
          (reverse (partition 2 elems))))

(defn |get [slot table]
  ;; (prn '|get slot (aget table 0))
  (matchv ::M/objects [table]
    [["lux;Nil" _]]
    nil
    
    [["lux;Cons" [[k v] table*]]]
    (if (= k slot)
      v
      (|get slot table*))))

(defn |put [slot value table]
  (matchv ::M/objects [table]
    [["lux;Nil" _]]
    (V "lux;Cons" (T (T slot value) (V "lux;Nil" nil)))
    
    [["lux;Cons" [[k v] table*]]]
    (if (= k slot)
      (V "lux;Cons" (T (T slot value) table*))
      (V "lux;Cons" (T (T k v) (|put slot value table*))))))

(defn |remove [slot table]
  (matchv ::M/objects [table]
    [["lux;Nil" _]]
    table
    
    [["lux;Cons" [[k v] table*]]]
    (if (= k slot)
      table*
      (V "lux;Cons" (T (T k v) (|remove slot table*))))))

(defn |merge [table1 table2]
  ;; (prn '|merge (aget table1 0) (aget table2 0))
  (matchv ::M/objects [table2]
    [["lux;Nil" _]]
    table1

    [["lux;Cons" [[k v] table2*]]]
    (|merge (|put k v table1) table2*)))

(defn |update [k f table]
  (matchv ::M/objects [table]
    [["lux;Nil" _]]
    table

    [["lux;Cons" [[k* v] table*]]]
    (if (= k k*)
      (V "lux;Cons" (T (T k (f v)) table*))
      (|update k f table*))))

(defn |head [xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    (assert false)

    [["lux;Cons" [x _]]]
    x))

(defn |tail [xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    (assert false)

    [["lux;Cons" [_ xs*]]]
    xs*))

;; [Resources/Monads]
(defn fail [message]
  (fn [_]
    ;; (prn 'FAIL message)
    (V "lux;Left" message)))

(defn return [value]
  (fn [state]
    (V "lux;Right" (T state value))))

(defn bind [m-value step]
  (when (not (fn? m-value))
    (prn 'bind (aget m-value 0)))
  (when (not (fn? step))
    (prn 'bind (aget step 0)))
  ;; (prn 'bind m-value step)
  (fn [state]
    (let [inputs (m-value state)]
      (matchv ::M/objects [inputs]
        [["lux;Right" [?state ?datum]]]
        (let [next-fn (step ?datum)]
          (when (not (fn? next-fn))
            (prn 'bind (aget next-fn 0) (aget next-fn 1)))
          (next-fn ?state))
        
        [["lux;Left" _]]
        inputs

        ;; [_]
        ;; (assert false (pr-str 'bind/inputs (aget inputs 0)))
        ))))

(defmacro |do [steps return]
  (assert (not= 0 (count steps)) "The steps can't be empty!")
  (assert (= 0 (rem (count steps) 2)) "The number of steps must be even!")
  (reduce (fn [inner [label computation]]
            (case label
              :let `(|let ~computation ~inner)
              ;; else
              `(bind ~computation
                     (fn [val#]
                       (matchv ::M/objects [val#]
                         [~label]
                         ~inner)))
              ;; `(bind ~computation
              ;;        (fn [~label] ~inner))
              ))
          return
          (reverse (partition 2 steps))))

;; [Resources/Combinators]
(defn try% [monad]
  (fn [state]
    (matchv ::M/objects [(monad state)]
      [["lux;Right" [?state ?datum]]]
      (return* ?state ?datum)
      
      [_]
      (return* state nil))))

(defn |cons [head tail]
  (V "lux;Cons" (T head tail)))

(defn |++ [xs ys]
  ;; (prn '|++ (and xs (aget xs 0)) (and ys (aget ys 0)))
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    ys

    [["lux;Cons" [x xs*]]]
    (V "lux;Cons" (T x (|++ xs* ys)))))

(defn |map [f xs]
  ;; (prn '|map (aget xs 0))
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    xs

    [["lux;Cons" [x xs*]]]
    (V "lux;Cons" (T (f x) (|map f xs*)))))

(defn |empty? [xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    true

    [["lux;Cons" [_ _]]]
    false))

(defn |filter [p xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    xs

    [["lux;Cons" [x xs*]]]
    (if (p x)
      (V "lux;Cons" (T x (|filter p xs*)))
      (|filter p xs*))))

(defn flat-map [f xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    xs

    [["lux;Cons" [x xs*]]]
    (|++ (f x) (flat-map f xs*))))

(defn |split-with [p xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    (T xs xs)

    [["lux;Cons" [x xs*]]]
    (if (p x)
      (|let [[pre post] (|split-with p xs*)]
        (T (|cons x pre) post))
      (T (V "lux;Nil" nil) xs))))

(defn |contains? [k table]
  (matchv ::M/objects [table]
    [["lux;Nil" _]]
    false

    [["lux;Cons" [[k* _] table*]]]
    (or (= k k*)
        (|contains? k table*))))

(defn fold [f init xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    init

    [["lux;Cons" [x xs*]]]
    (fold f (f init x) xs*)))

(defn fold% [f init xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    (return init)

    [["lux;Cons" [x xs*]]]
    (|do [init* (f init x)]
      (fold% f init* xs*))))

(defn folds [f init xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    (|list init)

    [["lux;Cons" [x xs*]]]
    (|cons init (folds f (f init x) xs*))))

(defn |length [xs]
  ;; (prn '|length (aget xs 0))
  (fold (fn [acc _] (inc acc)) 0 xs))

(let [|range* (fn |range* [from to]
                (if (< from to)
                  (V "lux;Cons" (T from (|range* (inc from) to)))
                  (V "lux;Nil" nil)))]
  (defn |range [n]
    (|range* 0 n)))

(defn |first [pair]
  (|let [[_1 _2] pair]
    _1))

(defn |second [pair]
  (|let [[_1 _2] pair]
    _2))

(defn zip2 [xs ys]
  (matchv ::M/objects [xs ys]
    [["lux;Cons" [x xs*]] ["lux;Cons" [y ys*]]]
    (V "lux;Cons" (T (T x y) (zip2 xs* ys*)))

    [_ _]
    (V "lux;Nil" nil)))

(defn |keys [plist]
  (matchv ::M/objects [plist]
    [["lux;Nil" _]]
    (|list)
    
    [["lux;Cons" [[k v] plist*]]]
    (|cons k (|keys plist*))))

(defn |vals [plist]
  (matchv ::M/objects [plist]
    [["lux;Nil" _]]
    (|list)
    
    [["lux;Cons" [[k v] plist*]]]
    (|cons v (|vals plist*))))

(defn |interpose [sep xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    xs

    [["lux;Cons" [_ ["lux;Nil" _]]]]
    xs
    
    [["lux;Cons" [x xs*]]]
    (V "lux;Cons" (T x (V "lux;Cons" (T sep (|interpose sep xs*)))))))

(do-template [<name> <joiner>]
  (defn <name> [f xs]
    ;; (prn '<name> 0 (aget xs 0))
    (matchv ::M/objects [xs]
      [["lux;Nil" _]]
      (return xs)

      [["lux;Cons" [x xs*]]]
      (|do [y (f x)
             ;; :let [_ (prn '<name> 1 (class y))
             ;;       _ (prn '<name> 2 (aget y 0))]
             ys (<name> f xs*)]
        (return (<joiner> y ys)))))

  map%      |cons
  flat-map% |++)

(defn |as-pairs [xs]
  (matchv ::M/objects [xs]
    [["lux;Cons" [x ["lux;Cons" [y xs*]]]]]
    (V "lux;Cons" (T (T x y) (|as-pairs xs*)))

    [_]
    (V "lux;Nil" nil)))

(defn |reverse [xs]
  (fold (fn [tail head]
          (|cons head tail))
        (|list)
        xs))

(defn show-table [table]
  ;; (prn 'show-table (aget table 0))
  (str "{{"
       (->> table
            (|map (fn [kv] (|let [[k v] kv] (str k " = ???"))))
            (|interpose " ")
            (fold str ""))
       "}}"))

(defn apply% [monad call-state]
  (fn [state]
    ;; (prn 'apply-m monad call-state)
    (let [output (monad call-state)]
      ;; (prn 'apply-m/output output)
      (matchv ::M/objects [output]
        [["lux;Right" [?state ?datum]]]
        (return* state ?datum)
        
        [_]
        output))))

(defn assert! [test message]
  (if test
    (return nil)
    (fail message)))

(defn comp% [f-m g-m]
  (|do [temp g-m]
    (f-m temp)))

(defn pass [m-value]
  (fn [state]
    m-value))

(def get-state
  (fn [state]
    (return* state state)))

(defn sequence% [m-values]
  (matchv ::M/objects [m-values]
    [["lux;Cons" [head tail]]]
    (|do [_ head]
      (sequence% tail))

    [_]
    (return nil)))

(defn repeat% [monad]
  (fn [state]
    (matchv ::M/objects [(monad state)]
      [["lux;Right" [?state ?head]]]
      (do ;; (prn 'repeat-m/?state ?state)
          (matchv ::M/objects [((repeat% monad) ?state)]
            [["lux;Right" [?state* ?tail]]]
            (do ;; (prn 'repeat-m/?state* ?state*)
                (return* ?state* (|cons ?head ?tail)))))
      
      [["lux;Left" ?message]]
      (do ;; (println "Failed at last:" ?message)
          (return* state (V "lux;Nil" nil))))))

(def source-consumed?
  (fn [state]
    (matchv ::M/objects [(get$ $SOURCE state)]
      [["lux;None" _]]
      (fail* "No source code.")

      [["lux;Some" ["lux;Nil" _]]]
      (return* state true)

      [["lux;Some" _]]
      (return* state false))))

(defn try-all% [monads]
  (matchv ::M/objects [monads]
    [["lux;Nil" _]]
    (fail "There are no alternatives to try!")

    [["lux;Cons" [m monads*]]]
    (fn [state]
      (let [output (m state)]
        (matchv ::M/objects [output monads*]
          [["lux;Right" _] _]
          output

          [_ ["lux;Nil" _]]
          output
          
          [_ _]
          ((try-all% monads*) state)
          )))
    ))

(defn exhaust% [step]
  (fn [state]
    (matchv ::M/objects [(step state)]
      [["lux;Right" [state* _]]]
      ((exhaust% step) state*)

      [["lux;Left" msg]]
      ((|do [? source-consumed?]
         (if ?
           (return nil)
           (fail* msg)))
       state)
      ;; (if (= "[Reader Error] EOF" msg)
      ;;   ((|do [? source-consumed?
      ;;           :let [_ (prn '? ?)]]
      ;;      (return nil))
      ;;    state)
      ;;   (fail* msg))
      )))

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
    ;; default
    char))

(defn normalize-ident [ident]
  (reduce str "" (map normalize-char ident)))

(def loader
  (fn [state]
    (return* state (->> state (get$ $HOST) (get$ $LOADER)))))

(def +init-bindings+
  (R ;; "lux;counter"
   0
   ;; "lux;mappings"
   (|table)))

(defn env [name]
  (R ;; "lux;closure"
   +init-bindings+
   ;; "lux;inner-closures"
   0
   ;; "lux;locals"
   +init-bindings+
   ;; "lux;name"
   name
   ))

(defn host [_]
  (R ;; "lux;eval-ctor"
   0
   ;; "lux;loader"
   (-> (java.io.File. "./output/") .toURL vector into-array java.net.URLClassLoader.)
   ;; "lux;writer"
   (V "lux;None" nil)))

(defn init-state [_]
  (R ;; "lux;envs"
   (|list)
   ;; "lux;host"
   (host nil)
   ;; "lux;module-aliases"
   (|table)
   ;; "lux;modules"
   (|table)
   ;; "lux;source"
   (V "lux;None" nil)
   ;; "lux;types"
   +init-bindings+
   ))

(defn from-some [some]
  (matchv ::M/objects [some]
    [["lux;Some" datum]]
    datum

    [_]
    (assert false)))

(def get-eval-ctor
  (fn [state]
    (return* (update$ $HOST #(update$ $EVAL-CTOR inc %) state)
             (get$ $EVAL-CTOR (get$ $HOST state)))))

(def get-writer
  (fn [state]
    (let [writer* (->> state (get$ $HOST) (get$ $WRITER))]
      ;; (prn 'get-writer (class writer*))
      ;; (prn 'get-writer (aget writer* 0))
      (matchv ::M/objects [writer*]
        [["lux;Some" datum]]
        (return* state datum)

        [_]
        (fail* "Writer hasn't been set.")))))

(def get-top-local-env
  (fn [state]
    (try (let [top (|head (get$ $ENVS state))]
           (return* state top))
      (catch Throwable _
        (fail "No local environment.")))))

(defn ->seq [xs]
  (matchv ::M/objects [xs]
    [["lux;Nil" _]]
    (list)

    [["lux;Cons" [x xs*]]]
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
    (matchv ::M/objects [(|reverse (get$ $ENVS state))]
      [["lux;Nil"]]
      (fail* "[Analyser Error] Can't get the module-name without a module.")

      [["lux;Cons" [?global _]]]
      (return* state (get$ $NAME ?global)))))

(defn with-scope [name body]
  (fn [state]
    (let [output (body (update$ $ENVS #(|cons (env name) %) state))]
      (matchv ::M/objects [output]
        [["lux;Right" [state* datum]]]
        (return* (update$ $ENVS |tail state*) datum)
        
        [_]
        output))))

(defn run-state [monad state]
  (monad state))

(defn with-closure [body]
  (|do [closure-name (|do [top get-top-local-env]
                       (return (->> top (get$ $INNER-CLOSURES) str)))]
    (fn [state]
      (let [body* (with-scope closure-name
                    body)]
        (run-state body* (update$ $ENVS #(|cons (update$ $INNER-CLOSURES inc (|head %))
                                                (|tail %))
                                  state))))))

(def get-scope-name
  (|do [module-name get-module-name]
    (fn [state]
      (return* state (->> state (get$ $ENVS) (|map #(get$ $NAME %)) |reverse (|cons module-name))))))

(defn with-writer [writer body]
  (fn [state]
    (let [output (body (update$ $HOST #(set$ $WRITER (V "lux;Some" writer) %) state))]
      (matchv ::M/objects [output]
        [["lux;Right" [?state ?value]]]
        (return* (update$ $HOST #(set$ $WRITER (->> state (get$ $HOST) (get$ $WRITER)) %) ?state)
                 ?value)

        [_]
        output))))

(defn show-ast [ast]
  ;; (prn 'show-ast (aget ast 0))
  ;; (prn 'show-ast (aget ast 1 1 0))
  ;; (cond (= "lux;Meta" (aget ast 1 1 0))
  ;;       (prn 'EXTRA 'show-ast (aget ast 1 1 1 1 0))

  ;;       (= "lux;Symbol" (aget ast 1 1 0))
  ;;       (prn 'EXTRA 'show-ast (aget ast 1 1 1 1))

  ;;       :else
  ;;       nil)
  (matchv ::M/objects [ast]
    [["lux;Meta" [_ ["lux;Bool" ?value]]]]
    (pr-str ?value)

    [["lux;Meta" [_ ["lux;Int" ?value]]]]
    (pr-str ?value)

    [["lux;Meta" [_ ["lux;Real" ?value]]]]
    (pr-str ?value)

    [["lux;Meta" [_ ["lux;Char" ?value]]]]
    (pr-str ?value)

    [["lux;Meta" [_ ["lux;Text" ?value]]]]
    (str "\"" ?value "\"")

    [["lux;Meta" [_ ["lux;Tag" [?module ?tag]]]]]
    (str "#" ?module ";" ?tag)

    [["lux;Meta" [_ ["lux;Symbol" [?module ?ident]]]]]
    (if (= "" ?module)
      ?ident
      (str ?module ";" ?ident))

    [["lux;Meta" [_ ["lux;Tuple" ?elems]]]]
    (str "[" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) "]")

    [["lux;Meta" [_ ["lux;Form" ?elems]]]]
    (str "(" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) ")")
    ))

(defn ident->text [ident]
  (|let [[?module ?name] ident]
    (str ?module ";" ?name)))
