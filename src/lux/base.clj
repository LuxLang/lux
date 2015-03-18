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
          (aset record (+ 1 idx) value)
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
  (matchv ::M/objects [table]
    [["Nil" _]]
    (V "Left" (str "Not found: " slot))
    
    [["Cons" [[k v] table*]]]
    (if (= k slot)
      (V "Right" v)
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

;; [Resources/Monads]
(defn fail [message]
  (fn [_]
    (V "Left" message)))

(defn return [value]
  (fn [state]
    (V "Right" (T state value))))

(defn bind [m-value step]
  (fn [state]
    (let [inputs (m-value state)]
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

(defn |concat [xs ys]
  (matchv ::M/objects [xs]
    [["Nil" _]]
    ys

    [["Cons" [x xs*]]]
    (V "Cons" (T x (|concat xs* ys)))))

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
    (|concat (f x) (flat-map f xs*))))

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

(defn |length [xs]
  (fold (fn [acc _] (inc acc)) 0 xs))

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

(let [cons% (fn [head tail]
              (V "Cons" (T head tail)))
      ++% (fn ++% [xs ys]
            (matchv ::M/objects [xs]
              [["Nil" _]]
              ys

              [["Cons" [x xs*]]]
              (V "Cons" (T x (++% xs* ys)))))]
  (do-template [<name> <joiner>]
    (defn <name> [f xs]
      (matchv ::M/objects [xs]
        [["Nil" _]]
        (return xs)

        [["Cons" [x xs*]]]
        (exec [y (f x)
               ys (<name> f xs*)]
          (return (<joiner> y ys)))))

    map%      cons%
    flat-map% ++%))

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

(defn init-state []
  (R "source"     (V "None" nil)
     "modules"    (|list)
     "global-env" (V "None" nil)
     "local-envs" (|list)
     "types"      +init-bindings+
     "writer"     (V "None" nil)
     "loader"     (-> (java.io.File. "./output/") .toURL vector into-array java.net.URLClassLoader.)
     "eval-ctor"  0))

(def get-eval-ctor
  (fn [state]
    (return* (update$ "eval-ctor" inc state) (get$ "eval-ctor" state))))

(def get-writer
  (fn [state]
    (matchv ::M/objects [(get$ "writer" state)]
      [["Some" datum]]
      (return* state datum)

      [_]
      (fail* "Writer hasn't been set."))))

(def get-top-local-env
  (fn [state]
    (return* state (|head (get$ "local-envs" state)))))

(def get-current-module-env
  (fn [state]
    (matchv ::M/objects [(get$ "global-env" state)]
      [["Some" datum]]
      (return* state datum)

      [_]
      (fail* "Module hasn't been set."))))

(def get-module-name
  (exec [module get-current-module-env]
    (return (get$ "name" module))))

(defn ^:private with-scope [name body]
  (fn [state]
    (let [output (body (update$ "local-envs" #(conj % (env name)) state))]
      (matchv ::M/objects [output]
        [["Right" [state* datum]]]
        (return* (update$ "local-envs" rest state*) datum)
        
        [_]
        output))))

(defn with-closure [body]
  (exec [[local? closure-name] (try-all% (list (exec [top get-top-local-env]
                                                 (return [true (->> top (get$ "inner-closures") str)]))
                                               (exec [global get-current-module-env]
                                                 (return [false (->> global (get$ "inner-closures") str)]))))]
    (fn [state]
      (let [body* (with-scope closure-name
                    body)]
        (body* (if local?
                 (update$ "local-envs" #(cons (update$ "inner-closures" inc (first %))
                                              (rest %))
                          state)
                 (update$ "global-env" #(update$ "inner-closures" inc %) state)))))))

(def get-scope-name
  (exec [module-name get-module-name]
    (fn [state]
      (return* state (->> state (get$ "local-envs") (map #(get$ "name" %)) reverse (cons module-name))))))

(defn with-writer [writer body]
  (fn [state]
    (let [output (body (set$ "writer" writer state))]
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

    [["Ident" ?ident]]
    ?ident

    [["Tuple" ?elems]]
    (str "[" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) "]")

    [["Form" ?elems]]
    (str "(" (->> ?elems (|map show-ast) (|interpose " ") (fold str "")) ")")
    ))
