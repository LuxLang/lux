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
        [["lux;Meta" [meta [<close-token> _]]]]
        (return (&/V <tag> (&/fold &/|++ (&/|list) elems)))
        
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
      [["lux;Meta" [meta ["Close_Brace" _]]]]
      (if (even? (&/|length elems))
        (return (&/V "lux;Record" (&/|as-pairs elems)))
        (fail (str "[Parser Error] Records must have an even number of elements.")))
      
      [_]
      (fail (str "[Parser Error] Unbalanced braces.")))))

;; [Interface]
(def parse
  (exec [token &lexer/lex
         ;; :let [_ (prn 'parse/token token)]
         ;; :let [_ (prn 'parse (aget token 0))]
         ]
    (matchv ::M/objects [token]
      [["lux;Meta" [meta ["White_Space" _]]]]
      (return (&/|list))

      [["lux;Meta" [meta ["Comment" _]]]]
      (return (&/|list))
      
      [["lux;Meta" [meta ["Bool" ?value]]]]
      (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;Bool" (Boolean/parseBoolean ?value))))))

      [["lux;Meta" [meta ["Int" ?value]]]]
      (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;Int" (Integer/parseInt ?value))))))

      [["lux;Meta" [meta ["Real" ?value]]]]
      (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;Real" (Float/parseFloat ?value))))))

      [["lux;Meta" [meta ["Char" ?value]]]]
      (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;Char" (.charAt ?value 0))))))

      [["lux;Meta" [meta ["Text" ?value]]]]
      (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;Text" ?value)))))

      [["lux;Meta" [meta ["Symbol" ?ident]]]]
      (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;Symbol" ?ident)))))

      [["lux;Meta" [meta ["Tag" ?ident]]]]
      (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;Tag" ?ident)))))

      [["lux;Meta" [meta ["Open_Paren" _]]]]
      (exec [syntax (parse-form parse)]
        (return (&/|list (&/V "lux;Meta" (&/T meta syntax)))))
      
      [["lux;Meta" [meta ["Open_Bracket" _]]]]
      (exec [syntax (parse-tuple parse)]
        (return (&/|list (&/V "lux;Meta" (&/T meta syntax)))))

      [["lux;Meta" [meta ["Open_Brace" _]]]]
      (exec [syntax (parse-record parse)]
        (return (&/|list (&/V "lux;Meta" (&/T meta syntax)))))

      [_]
      (fail "[Parser Error] Unknown lexer token.")
      )))
