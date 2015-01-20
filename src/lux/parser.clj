(ns lux.parser
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-m try-all-m map-m
                                         apply-m]]
                 [lexer :as &lexer])))

(declare parse)

;; [Utils]
(defmacro ^:private defparser [name match return]
  `(defn ~name [token#]
     (match token#
       ~match
       ~return

       _#
       (fail (str "[Parser Error] Unmatched token: " token#)))))

;; [Parsers]
(let [first-char #(.charAt % 0)]
  (do-template [<name> <input-tag> <output-tag> <method>]
    (defparser <name>
      [<input-tag> ?value]
      (return [<output-tag> (<method> ?value)]))

    ^:private parse-bool  ::&lexer/bool  ::bool  Boolean/parseBoolean
    ^:private parse-int   ::&lexer/int   ::int   Integer/parseInt
    ^:private parse-real  ::&lexer/real  ::real  Float/parseFloat
    ^:private parse-char  ::&lexer/char  ::char  first-char
    ^:private parse-text  ::&lexer/text  ::text  identity
    ^:private parse-ident ::&lexer/ident ::ident identity
    ))

(defparser parse-comment
  [::&lexer/comment _]
  (return nil))

(defparser parse-whitespace
  [::&lexer/white-space _]
  (return nil))

(defparser ^:private parse-tag
  [::&lexer/tag ?tag]
  (return [::tag ?tag]))

(defparser ^:private parse-form
  [::&lexer/open-paren]
  (exec [elems (repeat-m parse)
         token &lexer/lex]
    (if (= [::&lexer/close-paren] token)
      (return [::form (filter identity elems)])
      (fail "[Parser Error] Unbalanced parantheses."))))

(do-template [<name> <open-tag> <close-tag> <description> <ast>]
  (defparser <name>
    [<open-tag>]
    (exec [elems (repeat-m parse)
           token &lexer/lex]
      (if (= [<close-tag>] token)
        (return [<ast> (filter identity elems)])
        (fail (str "[Parser Error] Unbalanced " <description> ".")))))

  ^:private parse-form  ::&lexer/open-paren   ::&lexer/close-paren   "parantheses" ::form
  ^:private parse-tuple ::&lexer/open-bracket ::&lexer/close-bracket "brackets"    ::tuple
  )

(defparser ^:private parse-record
  [::&lexer/open-brace]
  (exec [elems* (repeat-m parse)
         token &lexer/lex
         :let [elems (filter identity elems*)]]
    (cond (not= [::&lexer/close-brace] token)
          (fail (str "[Parser Error] Unbalanced braces."))

          (odd? (count elems))
          (fail (str "[Parser Error] Records must have an even number of elements."))

          :else
          (return [::record (filter identity elems)]))))

(let [parsers [parse-comment
               parse-whitespace
               parse-bool
               parse-int
               parse-real
               parse-char
               parse-text
               parse-tag
               parse-ident
               parse-form
               parse-tuple
               parse-record]]
  (defn ^:private parse-token [token]
    (try-all-m (map #(% token) parsers))))

(def ^:private parse
  (exec [token &lexer/lex]
    (parse-token token)))

(defn parse-all []
  (exec [ast parse]
    (fn [state]
      (if (empty? (::&lexer/source state))
        (return* state (if ast (list ast) '()))
        ((exec [asts (parse-all)]
           (return (cons ast asts)))
         state)))))
