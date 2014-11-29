(ns lang.parser
  (:require [clojure.core.match :refer [match]]
            (lang [util :as &util :refer [exec return* return fail fail*
                                          repeat-m try-m try-all-m map-m
                                          apply-m]]
                  [lexer :as &lexer])))

(declare parse-form)

;; [Utils]
(defmacro ^:private defparser [name match return]
  `(def ~name
     (fn [[token# & left#]]
       (match token#
         ~match
         (~return left#)
         _#
         (fail* (str "Unmatched token: " token#))))))

;; [Parsers]
(defparser ^:private parse-int
  [::&lexer/int ?int]
  (return [::int (Long/parseLong ?int)]))

(defparser ^:private parse-float
  [::&lexer/float ?float]
  (return [::float (Double/parseDouble ?float)]))

(defparser ^:private parse-ident
  [::&lexer/ident ?ident]
  (return [::ident ?ident]))

(defparser ^:private parse-tuple
  [::&lexer/tuple ?parts]
  (exec [=parts (map-m (fn [arg] (apply-m parse-form (list arg)))
                       ?parts)]
    (return [::tuple =parts])))

(defparser ^:private parse-def
  [::&lexer/list ([[::&lexer/ident "def"] ?name ?body] :seq)]
  (exec [=name (apply-m parse-form (list ?name))
         =body (apply-m parse-form (list ?body))]
    (return [::def =name =body])))

(defparser ^:private parse-defdata
  [::&lexer/list ([[::&lexer/ident "defdata"] ?type & ?cases] :seq)]
  (exec [=type (apply-m parse-form (list ?type))
         =cases (map-m (fn [arg]
                         (match arg
                           [::&lexer/list ([[::&lexer/tag ?tag] ?data] :seq)]
                           (exec [=data (apply-m parse-form (list ?data))]
                             (return [::tagged ?tag =data]))
                           ))
                       ?cases)]
    (return [::defdata =type =cases])))

(defparser ^:private parse-if
  [::&lexer/list ([[::&lexer/ident "if"] ?test ?then ?else] :seq)]
  (exec [=test (apply-m parse-form (list ?test))
         =then (apply-m parse-form (list ?then))
         =else (apply-m parse-form (list ?else))]
    (return [::if =test =then =else])))

(defparser ^:private parse-case
  [::&lexer/list ([[::&lexer/ident "case"] ?variant & cases] :seq)]
  (exec [=variant (apply-m parse-form (list ?variant))
         =branches (do (assert (even? (count cases)))
                     (map-m (fn [[destruct expr]]
                              (exec [=destruct (apply-m parse-form (list destruct))
                                     =expr (apply-m parse-form (list expr))]
                                (return [::case-branch =destruct =expr])))
                            (partition 2 cases)))]
    (return [::case =variant =branches])))

(defparser ^:private parse-tagged
  [::&lexer/list ([[::&lexer/tag ?tag] ?data] :seq)]
  (exec [=data (apply-m parse-form (list ?data))]
    (return [::tagged ?tag =data])))

(defparser ^:private parse-fn-call
  [::&lexer/list ([?f & ?args] :seq)]
  (exec [=f (apply-m parse-form (list ?f))
         =args (map-m (fn [arg] (apply-m parse-form (list arg)))
                      ?args)]
    (return [::fn-call =f =args])))

(def ^:private parse-form
  (try-all-m [parse-int
              parse-float
              parse-ident
              parse-tuple
              parse-def
              parse-defdata
              parse-if
              parse-case
              parse-tagged
              parse-fn-call]))

;; [Interface]
(defn parse [text]
  (match ((repeat-m parse-form) text)
    [::&util/ok [?state ?forms]]
    (if (empty? ?state)
      ?forms
      (assert false (str "Unconsumed input: " ?state)))
    
    [::&util/failure ?message]
    (assert false ?message)))

(comment
  ((comp parse list &lexer/lex) (slurp "src/example/test1.lang"))
  
  (&lexer/lex (slurp "src/example/test1.lang"))
  "\n(def (** base exp)\n  (reduce * 1 (repeat exp base)))\n"
  
  [::list ([::ident "def"]
             [::list ([::ident "**"] [::ident "base"] [::ident "exp"])]
               [::list ([::ident "reduce"]
                          [::ident "*"]
                            [::int "1"]
                              [::list ([::ident "repeat"]
                                         [::ident "exp"]
                                           [::ident "base"])])])]

  (re-find #"^([a-zA-Z!@$%^&*<>\.,/\\\|][a-zA-Z0-9!@$%^&*<>\.,/\\\|]*)" "a9")
  (re-find #"^([1-9][0-9]*)" "9")
  )
