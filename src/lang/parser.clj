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
         (fail* "Unmatched token.")))))

;; [Parsers]
(defparser ^:private parse-ident
  [::&lexer/ident ?ident]
  (return [::ident ?ident]))

(defparser ^:private parse-int
  [::&lexer/int ?int]
  (return [::int (Long/parseLong ?int)]))

(defparser ^:private parse-def
  [::&lexer/list ([[::&lexer/ident "def"] ?name ?body] :seq)]
  (exec [=name (apply-m parse-form (list ?name))
         =body (apply-m parse-form (list ?body))]
    (return [::def =name =body])))

(defparser ^:private parse-fn-call
  [::&lexer/list ([?f & ?args] :seq)]
  (exec [=f (apply-m parse-form (list ?f))
         =args (map-m (fn [arg] (apply-m parse-form (list arg)))
                      ?args)]
    (return [::fn-call =f =args])))

(def ^:private parse-form
  (try-all-m [parse-ident
              parse-int
              parse-def
              parse-fn-call]))

;; [Interface]
(defn parse [tokens]
  (match (parse-form tokens)
    [::&util/ok [?state ?datum]]
    (if (empty? ?state)
      ?datum
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
