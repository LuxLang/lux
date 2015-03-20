(ns lux.base
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array))

;; [Exports]
(def +name-separator+ ";")

(defn T [& elems]
  (to-array elems))

(defn V [tag value]
  (to-array [tag value]))

(defn R [& kvs]
  (to-array kvs))

(defn get$ [slot record]
  (let [size (alength record)]
    (loop [idx 0]
      (if (< idx size)
        (if (= slot (aget record idx))
          (aget record (+ 1 idx))
          (recur (+ 2 idx)))
        (assert false)))))

(defn set$ [slot value record]
  (let [record (aclone record)
        size (alength record)]
    (loop [idx 0]
      (if (< idx size)
        (if (= slot (aget record idx))
          (doto record
            (aset (+ 1 idx) value))
          (recur (+ 2 idx)))
        (assert false)))))

(defmacro update$ [slot f record]
  `(let [record# ~record]
     (set$ ~slot (~f (get$ ~slot record#))
           record#)))

(defn fail* [message]
  (V "Left" message))

(defn return* [state value]
  (V "Right" (T state value)))

(defmacro |let [bindings body]
  (reduce (fn [inner [left right]]
            `(matchv ::M/objects [~right]
               [~left]
               ~inner))
          body
          (reverse (partition 2 bindings))))

(defmacro |list [& elems]
  (reduce (fn [tail head]
            `(V "Cons" (T ~head ~tail)))
          `(V "Nil" nil)
          elems))

(defmacro |table [& elems]
  (reduce (fn [table [k v]]
            `(|put ~k ~v ~table))
          `(|list)
          (partition 2 elems)))

(defn |get [slot table]
  (prn '|get slot (aget table 0))
  (matchv ::M/objects [table]
    [["Nil" _]]
    nil
    
    [["Cons" [[k v] table*]]]
    (if (= k slot)
      v
      (|get slot table*))))

(defn |put [slot value table]
  (matchv ::M/objects [table]
    [["Nil" _]]
    (V "Cons" (T (T slot value) (V "Nil" nil)))
    
    [["Cons" [[k v] table*]]]
    (if (= k slot)
      (V "Cons" (T (T slot value) table*))
      (V "Cons" (T (T k v) (|put slot value table*))))))

(defn |merge [table1 table2]
  (prn '|merge (aget table1 0) (aget table2 0))
  (matchv ::M/objects [table2]
    [["Nil" _]]
    table1

    [["Cons" [[k v] table2*]]]
    (|merge (|put k v table1) table2*)))

(defn |update [k f table]
  (matchv ::M/objects [table]
    [["Nil" _]]
    table

    [["Cons" [[k* v] table*]]]
    (if (= k k*)
      (V "Cons" (T (T k (f v)) table*))
      (|update k f table*))))

(defn |head [xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    (assert false)

    [["Cons" [x _]]]
    x))

(defn |tail [xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    (assert false)

    [["Cons" [_ xs*]]]
    xs*))

;; [Resources/Monads]
(defn fail [message]
  (fn [_]
    (V "Left" message)))

(defn return [value]
  (fn [state]
    (V "Right" (T state value))))

(defn bind [m-value step]
  ;; (prn 'bind m-value step)
  (fn [state]
    (let [inputs (m-value state)]
      ;; (prn 'bind/inputs (aget inputs 0))
      (matchv ::M/objects [inputs]
        [["Right" [?state ?datum]]]
        ((step ?datum) ?state)
        
        [_]
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
(defn try% [monad]
  (fn [state]
    (matchv ::M/objects [(monad state)]
      [["Right" [?state ?datum]]]
      (return* ?state ?datum)
      
      [_]
      (return* state nil))))

(defn |cons [head tail]
  (V "Cons" (T head tail)))

(defn |++ [xs ys]
  (prn '|++ (and xs (aget xs 0)) (and ys (aget ys 0)))
  (matchv ::M/objects [xs]
    [["Nil" _]]
    ys

    [["Cons" [x xs*]]]
    (V "Cons" (T x (|++ xs* ys)))))

(defn |map [f xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    xs

    [["Cons" [x xs*]]]
    (V "Cons" (T (f x) (|map f xs*)))))

(defn flat-map [f xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    xs

    [["Cons" [x xs*]]]
    (|++ (f x) (flat-map f xs*))))

(defn |split-with [p xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    (T xs xs)

    [["Cons" [x xs*]]]
    (if (p x)
      (|let [[pre post] (|split-with p xs*)]
            (T (|cons x pre) post))
      (T (V "Nil" nil) xs))))

(defn |contains? [k table]
  (matchv ::M/objects [table]
    [["Nil" _]]
    false

    [["Cons" [[k* _] table*]]]
    (or (= k k*)
        (|contains? k table*))))

(defn fold [f init xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    init

    [["Cons" [x xs*]]]
    (fold f (f init x) xs*)))

(defn folds [f init xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    (|list init)

    [["Cons" [x xs*]]]
    (|cons init (folds f (f init x) xs*))))

(defn |length [xs]
  (prn '|length (aget xs 0))
  (fold (fn [acc _] (inc acc)) 0 xs))

(let [|range* (fn |range* [from to]
                (if (< from to)
                  (V "Cons" (T from (|range* (inc from) to)))
                  (V "Nil" nil)))]
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
    [["Cons" [x xs*]] ["Cons" [y ys*]]]
    (V "Cons" (T (T x y) (zip2 xs* ys*)))

    [_ _]
    (V "Nil" nil)))

(defn |keys [plist]
  (matchv ::M/objects [plist]
    [["Nil" _]]
    (|list)
    
    [["Cons" [[k v] plist*]]]
    (|cons k (|keys plist*))))

(defn |interpose [sep xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    xs

    [["Cons" [_ ["Nil" _]]]]
    xs
    
    [["Cons" [x xs*]]]
    (V "Cons" (T x (V "Cons" (T sep (|interpose sep xs*)))))))

(do-template [<name> <joiner>]
  (defn <name> [f xs]
    (matchv ::M/objects [xs]
      [["Nil" _]]
      (return xs)

      [["Cons" [x xs*]]]
      (exec [y (f x)
             ys (<name> f xs*)]
        (return (<joiner> y ys)))))

  map%      |cons
  flat-map% |++)

(defn |as-pairs [xs]
  (matchv ::M/objects [xs]
    [["Cons" [x [["Cons" [y xs*]]]]]]
    (V "Cons" (T (T x y) (|as-pairs xs*)))

    [_]
    (V "Nil" nil)))

(defn |reverse [xs]
  (fold (fn [tail head]
          (|cons head tail))
        (|list)
        xs))

(defn if% [text-m then-m else-m]
  (exec [? text-m]
    (if ?
      then-m
      else-m)))

(defn apply% [monad call-state]
  (fn [state]
    ;; (prn 'apply-m monad call-state)
    (let [output (monad call-state)]
      ;; (prn 'apply-m/output output)
      (matchv ::M/objects [output]
        [["Right" [?state ?datum]]]
        (return* state ?datum)
        
        [_]
        output))))

(defn assert! [test message]
  (if test
    (return nil)
    (fail message)))

(defn comp% [f-m g-m]
  (exec [temp g-m]
    (f-m temp)))

(defn pass [m-value]
  (fn [state]
    m-value))

(def get-state
  (fn [state]
    (return* state state)))

(defn sequence% [m-values]
  (matchv ::M/objects [m-values]
    [["Cons" [head tail]]]
    (exec [_ head]
      (sequence% tail))

    [_]
    (return nil)))

(defn repeat% [monad]
  (fn [state]
    (matchv ::M/objects [(monad state)]
      [["Right" [?state ?head]]]
      (do ;; (prn 'repeat-m/?state ?state)
          (matchv ::M/objects [((repeat% monad) ?state)]
            [["Right" [?state* ?tail]]]
            (do ;; (prn 'repeat-m/?state* ?state*)
                (return* ?state* (|cons ?head ?tail)))))
      
      [["Left" ?message]]
      (do ;; (println "Failed at last:" ?message)
          (return* state (V "Nil" nil))))))

(def source-consumed?
  (fn [state]
    (return* state (empty? (get$ "source" state)))))

(defn exhaust% [monad]
  (exec [output-h monad
         ? source-consumed?
         output-t (if ?
                    (return (|list))
                    (exhaust% monad))]
    (return (|cons output-h output-t))))

(defn try-all% [monads]
  (matchv ::M/objects [monads]
    [["Nil" _]]
    (fail "There are no alternatives to try!")

    [["Cons" [m monads*]]]
    (fn [state]
      (let [output (m state)]
        (matchv ::M/objects [output monads*]
          [["Right" _] _]
          output

          [_ ["Nil" _]]
          output
          
          [_ _]
          ((try-all% monads*) state)
          )))
    ))

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
    (return* state (get$ "loader" state))))

(def +init-bindings+
  (R "counter" 0
     "mappings" (|list)))

(defn env [name]
  (R "name" name
     "inner-closures" 0
     "locals"  +init-bindings+
     "closure" +init-bindings+))

(defn init-state [_]
  (R "source"     (V "None" nil)
     "modules"    (|list)
     "global-env" (V "None" nil)
     "local-envs" (|list)
     "types"      +init-bindings+
     "writer"     (V "None" nil)
     "loader"     (-> (java.io.File. "./output/") .toURL vector into-array java.net.URLClassLoader.)
     "eval-ctor"  0))

(defn from-some [some]
  (matchv ::M/objects [some]
    [["Some" datum]]
    datum

    [_]
    (assert false)))

(defn show-state [state]
  (let [source (get$ "source" state)
        modules (get$ "modules" state)
        global-env (get$ "global-env" state)
        local-envs (get$ "local-envs" state)
        types (get$ "types" state)
        writer (get$ "writer" state)
        loader (get$ "loader" state)
        eval-ctor (get$ "eval-ctor" state)]
    (str "{"
         (->> (for [slot ["source", "modules", "global-env", "local-envs", "types", "writer", "loader", "eval-ctor"]
                    :let [value (get$ slot state)]]
                (str "#" slot " " (case slot
                                    "source" "???"
                                    "modules" "???"
                                    "global-env" "???"
                                    "local-envs" (|length value)
                                    "types" "???"
                                    "writer" "???"
                                    "loader" "???"
                                    "eval-ctor" value)))
              (interpose " ")
              (reduce str ""))
         "}")))

(def get-eval-ctor
  (fn [state]
    (return* (update$ "eval-ctor" inc state) (get$ "eval-ctor" state))))

(def get-writer
  (fn [state]
    (let [writer* (get$ "writer" state)]
      (prn 'get-writer (class writer*))
      (prn 'get-writer (aget writer* 0))
      (matchv ::M/objects [writer*]
        [["Some" datum]]
        (return* state datum)

        [_]
        (fail* "Writer hasn't been set.")))))

(def get-top-local-env
  (fn [state]
    (return* state (|head (get$ "local-envs" state)))))

(def get-current-module-env
  (fn [state]
    (let [global-env* (get$ "global-env" state)]
      (prn 'get-current-module-env (aget global-env* 0))
      (matchv ::M/objects [global-env*]
        [["Some" datum]]
        (return* state datum)

        [_]
        (fail* "Module hasn't been set.")))))

(defn ->seq [xs]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    (list)

    [["Cons" [x xs*]]]
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
  (exec [module get-current-module-env]
    (return (get$ "name" module))))

(defn ^:private with-scope [name body]
  (fn [state]
    (let [output (body (update$ "local-envs" #(|cons (env name) %) state))]
      (matchv ::M/objects [output]
        [["Right" [state* datum]]]
        (return* (update$ "local-envs" |tail state*) datum)
        
        [_]
        output))))

(defn with-closure [body]
  (exec [closure-info (try-all% (|list (exec [top get-top-local-env]
                                         (return (T true (->> top (get$ "inner-closures") str))))
                                       (exec [global get-current-module-env]
                                         (return (T false (->> global (get$ "inner-closures") str))))))]
    (matchv ::M/objects [closure-info]
      [[local? closure-name]]
      (fn [state]
        (let [body* (with-scope closure-name
                      body)]
          (body* (if local?
                   (update$ "local-envs" #(|cons (update$ "inner-closures" inc (|head %))
                                                 (|tail %))
                            state)
                   (update$ "global-env" #(matchv ::M/objects [%]
                                            [["Some" global-env]]
                                            (V "Some" (update$ "inner-closures" inc global-env))

                                            [_]
                                            %)
                            state)))))
      )))

(def get-scope-name
  (exec [module-name get-module-name]
    (fn [state]
      (return* state (->> state (get$ "local-envs") (|map #(get$ "name" %)) |reverse (|cons module-name))))))

(defn with-writer [writer body]
  (fn [state]
    (let [output (body (set$ "writer" (V "Some" writer) state))]
      (matchv ::M/objects [output]
        [["Right" [?state ?value]]]
        (return* (set$ "writer" (get$ "writer" state) ?state) ?value)

        [_]
        output))))

(defn run-state [monad state]
  (monad state))

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

    [["Symbol" ?ident]]
    ?ident

    [["Tuple" ?elems]]
    (str "[" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) "]")

    [["Form" ?elems]]
    (str "(" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) ")")
    ))
