(ns lux.parser
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return fail]]
                 [lexer :as &lexer])))

;; [Utils]
(do-template [<name> <close-tag> <description> <tag>]
  (defn <name> [parse]
    (exec [elems (&/repeat% parse)
           token &lexer/lex]
      (matchv ::M/objects [token]
        [[<close-token> _]]
        (return (&/|list (&/V <tag> (&/fold &/|++ (&/|list) elems))))
        
        [_]
        (fail (str "[Parser Error] Unbalanced " <description> ".")))))

  ^:private parse-form  "Close_Paren"   "parantheses" "lux;Form"
  ^:private parse-tuple "Close_Bracket" "brackets"    "lux;Tuple"
  )

(defn ^:private parse-record [parse]
  (exec [elems* (&/repeat% parse)
         token &lexer/lex
         :let [elems (&/fold &/|++ (&/|list) elems*)]]
    (matchv ::M/objects [token]
      [["Close_Brace" _]]
      (fail (str "[Parser Error] Unbalanced braces."))

      [_]
      (if (even? (&/|length elems))
        (return (&/|list (&/V "lux;Record" (&/|as-pairs elems))))
        (fail (str "[Parser Error] Records must have an even number of elements."))))))

;; [Interface]
(def parse
  (exec [token &lexer/lex
         ;; :let [_ (prn 'parse/token token)]
         ;; :let [_ (prn 'parse (aget token 0))]
         ]
    (matchv ::M/objects [token]
      [["White_Space" _]]
      (return (&/|list))

      [["Comment" _]]
      (return (&/|list))
      
      [["Bool" ?value]]
      (return (&/|list (&/V "lux;Bool" (Boolean/parseBoolean ?value))))

      [["Int" ?value]]
      (return (&/|list (&/V "lux;Int" (Integer/parseInt ?value))))

      [["Real" ?value]]
      (return (&/|list (&/V "lux;Real" (Float/parseFloat ?value))))

      [["Char" ?value]]
      (return (&/|list (&/V "lux;Char" (.charAt ?value 0))))

      [["Text" ?value]]
      (return (&/|list (&/V "lux;Text" ?value)))

      [["Symbol" ?ident]]
      (return (&/|list (&/V "lux;Symbol" ?ident)))

      [["Tag" ?ident]]
      (return (&/|list (&/V "lux;Tag" ?ident)))

      [["Open_Paren" _]]
      (parse-form parse)
      
      [["Open_Bracket" _]]
      (parse-tuple parse)

      [["Open_Brace" _]]
      (parse-record parse)

      [_]
      (fail "[Parser Error] Unknown lexer token.")
      )))
