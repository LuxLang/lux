(ns lux.compiler.js.proc.host
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |let |case]])))

(defn ^:private compile-js-ref [compile ?values special-args]
  (|do [:let [(&/$Cons ?name (&/$Nil)) special-args]]
    (return ?name)))

(defn ^:private compile-js-new [compile ?values special-args]
  (|do [:let [(&/$Cons ?function ?args) ?values]
        =function (compile ?function)
        =args (&/map% compile ?args)]
    (return (str "new (" =function ")("
                 (->> =args
                      (&/|interpose ",")
                      (&/fold str ""))
                 ")"))))

(defn ^:private compile-js-call [compile ?values special-args]
  (|do [:let [(&/$Cons ?function ?args) ?values]
        =function (compile ?function)
        =args (&/map% compile ?args)]
    (return (str "(" =function ")("
                 (->> =args
                      (&/|interpose ",")
                      (&/fold str ""))
                 ")"))))

(defn ^:private compile-js-object-call [compile ?values special-args]
  (|do [:let [(&/$Cons ?object (&/$Cons ?field ?args)) ?values]
        =object (compile ?object)
        =field (compile ?field)
        =args (&/map% compile ?args)]
    (return (str "LuxRT$" "jsObjectCall"
                 "(" =object
                 "," =field
                 "," (str "[" (->> =args (&/|interpose ",") (&/fold str "")) "]")
                 ")"))))

(defn ^:private compile-js-object [compile ?values special-args]
  (|do [:let [(&/$Nil) ?values]]
    (return "{}")))

(defn ^:private compile-js-get-field [compile ?values special-args]
  (|do [:let [(&/$Cons ?object (&/$Cons ?field (&/$Nil))) ?values]
        =object (compile ?object)
        =field (compile ?field)]
    (return (str "(" =object ")" "[" =field "]"))))

(defn ^:private compile-js-set-field [compile ?values special-args]
  (|do [:let [(&/$Cons ?object (&/$Cons ?field (&/$Cons ?input (&/$Nil)))) ?values]
        =object (compile ?object)
        =field (compile ?field)
        =input (compile ?input)]
    (return (str "LuxRT$" "jsSetField" "(" =object "," =field "," =input ")"))))

(defn ^:private compile-js-delete-field [compile ?values special-args]
  (|do [:let [(&/$Cons ?object (&/$Cons ?field (&/$Nil))) ?values]
        =object (compile ?object)
        =field (compile ?field)]
    (return (str "LuxRT$" "jsDeleteField" "(" =object "," =field ")"))))

(do-template [<name> <value>]
  (defn <name> [compile ?values special-args]
    (return <value>))

  ^:private compile-js-null      "null"
  ^:private compile-js-undefined "undefined"
  )

(defn compile-proc [compile proc-name ?values special-args]
  (case proc-name
    "new"             (compile-js-new compile ?values special-args)
    "call"            (compile-js-call compile ?values special-args)
    "object-call"     (compile-js-object-call compile ?values special-args)
    "ref"             (compile-js-ref compile ?values special-args)
    "object"          (compile-js-object compile ?values special-args)
    "get-field"       (compile-js-get-field compile ?values special-args)
    "set-field"       (compile-js-set-field compile ?values special-args)
    "delete-field"    (compile-js-delete-field compile ?values special-args)
    "null"            (compile-js-null compile ?values special-args)
    "undefined"       (compile-js-undefined compile ?values special-args)
    ;; else
    (&/fail-with-loc (str "[Compiler Error] Unknown host procedure: " ["js" proc-name]))))
