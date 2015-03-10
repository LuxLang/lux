(ns lux.parser
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return fail repeat-m]]
                 [lexer :as &lexer])))

;; [Utils]
(do-template [<name> <close-token> <description> <tag>]
  (defn <name> [parse]
    (exec [elems (repeat-m parse)
           token &lexer/lex]
      (if (= <close-token> token)
        (return (list (&/V <tag> (reduce #(&/V "Cons" (to-array [%2 %1]))
                                         (&/V "Nil" nil)
                                         (reverse (apply concat elems))))))
        (fail (str "[Parser Error] Unbalanced " <description> ".")))))

  ^:private parse-form  [::&lexer/close-paren]   "parantheses" "Form"
  ^:private parse-tuple [::&lexer/close-bracket] "brackets"    "Tuple"
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
          (return (list (&/V "Record" (reduce #(&/V "Cons" (to-array [%2 %1]))
                                              (&/V "Nil" nil)
                                              (reverse elems))))))))

;; [Interface]
(def parse
  (exec [token &lexer/lex
         ;; :let [_ (prn 'parse/token token)]
         ]
    (match token
      [::&lexer/white-space _]
      (return (list))

      [::&lexer/comment _]
      (return (list))
      
      [::&lexer/bool ?value]
      (return (list (&/V "Bool" (Boolean/parseBoolean ?value))))

      [::&lexer/int ?value]
      (return (list (&/V "Int" (Integer/parseInt ?value))))

      [::&lexer/real ?value]
      (return (list (&/V "Real" (Float/parseFloat ?value))))

      [::&lexer/char ?value]
      (return (list (&/V "Char" (.charAt ?value 0))))

      [::&lexer/text ?value]
      (return (list (&/V "Text" ?value)))

      [::&lexer/ident ?value]
      (return (list (&/V "Ident" ?value)))

      [::&lexer/tag ?value]
      (return (list (&/V "Tag" ?value)))

      [::&lexer/open-paren]
      (parse-form parse)
      
      [::&lexer/open-bracket]
      (parse-tuple parse)

      [::&lexer/open-brace]
      (parse-record parse)

      _
      (fail (str "[Parser Error] Unmatched token: " token)))))
