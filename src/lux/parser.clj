(ns lux.parser
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return fail repeat-m]]
                 [lexer :as &lexer])))

;; [Utils]
(do-template [<name> <close-token> <description> <ast>]
  (defn <name> [parse]
    (exec [elems (repeat-m parse)
           token &lexer/lex]
      (if (= <close-token> token)
        (return (list [<ast> (apply concat elems)]))
        (fail (str "[Parser Error] Unbalanced " <description> ".")))))

  ^:private parse-form  [::&lexer/close-paren]   "parantheses" ::form
  ^:private parse-tuple [::&lexer/close-bracket] "brackets"    ::tuple
  )

(defn ^:private parse-record [parse]
  (exec [elems* (repeat-m parse)
         token &lexer/lex
         :let [elems (apply concat elems*)]]
    (cond (not= [::&lexer/close-brace] token)
          (fail (str "[Parser Error] Unbalanced braces."))

          (odd? (count elems))
          (fail (str "[Parser Error] Records must have an even number of elements."))

          :else
          (return (list [::record elems])))))

;; [Interface]
(def parse
  (exec [token &lexer/lex]
    (match token
      [::&lexer/white-space _]
      (return '())

      [::&lexer/comment _]
      (return '())
      
      [::&lexer/bool ?value]
      (return (list [::bool (Boolean/parseBoolean ?value)]))

      [::&lexer/int ?value]
      (return (list [::int (Integer/parseInt ?value)]))

      [::&lexer/real ?value]
      (return (list [::real (Float/parseFloat ?value)]))

      [::&lexer/char ?value]
      (return (list [::char (.charAt ?value 0)]))

      [::&lexer/text ?value]
      (return (list [::text ?value]))

      [::&lexer/ident ?value]
      (return (list [::ident ?value]))

      [::&lexer/tag ?value]
      (return (list [::tag ?value]))

      [::&lexer/open-paren]
      (parse-form parse)
      
      [::&lexer/open-bracket]
      (parse-tuple parse)

      [::&lexer/open-brace]
      (parse-record parse)

      _
      (fail (str "[Parser Error] Unmatched token: " token)))))
