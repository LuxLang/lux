(ns lux.parser
  (:require [clojure.template :refer [do-template]]
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return |case]]
                 [lexer :as &lexer])))

;; [Utils]
(def ^:private base-uneven-record-error
  "[Parser Error] Records must have an even number of elements.")

(defn ^:private repeat% [action]
  (fn [state]
    (|case (action state)
      (&/$Left ^String error)
      (if (or (.contains error base-uneven-record-error)
              (not (.contains error "[Parser Error]")))
        (&/$Left error)
        (&/$Right (&/T [state &/$Nil])))

      (&/$Right state* head)
      ((|do [tail (repeat% action)]
         (return (&/$Cons head tail)))
       state*))))

(do-template [<name> <close-tag> <description> <tag>]
  (defn <name> [parse]
    (|do [elems (repeat% parse)
          token &lexer/lex]
      (|case token
        [meta (<close-tag> _)]
        (return (<tag> (&/fold &/|++ &/$Nil elems)))
        
        _
        (&/fail-with-loc (str "[Parser Error] Unbalanced " <description> "."))
        )))

  ^:private parse-form  &lexer/$Close_Paren   "parantheses" &/$Form
  ^:private parse-tuple &lexer/$Close_Bracket "brackets"    &/$Tuple
  )

(defn ^:private parse-record [parse]
  (|do [elems* (repeat% parse)
        token &lexer/lex
        :let [elems (&/fold &/|++ &/$Nil elems*)]]
    (|case token
      [meta (&lexer/$Close_Brace _)]
      (|do [_ (&/assert! (even? (&/|length elems))
                         (&/fail-with-loc base-uneven-record-error))]
        (return (&/$Record (&/|as-pairs elems))))
      
      _
      (&/fail-with-loc "[Parser Error] Unbalanced braces.")
      )))

;; [Interface]
(def parse
  (|do [token &lexer/lex
        :let [[meta token*] token]]
    (|case token*
      (&lexer/$White_Space _)
      (return &/$Nil)

      (&lexer/$Comment _)
      (return &/$Nil)
      
      (&lexer/$Bit ?value)
      (return (&/|list (&/T [meta (&/$Bit (.equals ^String ?value "#1"))])))

      (&lexer/$Nat ?value)
      (return (&/|list (&/T [meta (&/$Nat (Long/parseUnsignedLong ?value))])))

      (&lexer/$Int ?value)
      (return (&/|list (&/T [meta (&/$Int (Long/parseLong ?value))])))

      (&lexer/$Rev ?value)
      (return (&/|list (&/T [meta (&/$Rev (&/decode-rev ?value))])))

      (&lexer/$Frac ?value)
      (return (&/|list (&/T [meta (&/$Frac (Double/parseDouble ?value))])))

      (&lexer/$Text ?value)
      (return (&/|list (&/T [meta (&/$Text ?value)])))

      (&lexer/$Identifier ?ident)
      (return (&/|list (&/T [meta (&/$Identifier ?ident)])))

      (&lexer/$Tag ?ident)
      (return (&/|list (&/T [meta (&/$Tag ?ident)])))

      (&lexer/$Open_Paren _)
      (|do [syntax (parse-form parse)]
        (return (&/|list (&/T [meta syntax]))))
      
      (&lexer/$Open_Bracket _)
      (|do [syntax (parse-tuple parse)]
        (return (&/|list (&/T [meta syntax]))))

      (&lexer/$Open_Brace _)
      (|do [syntax (parse-record parse)]
        (return (&/|list (&/T [meta syntax]))))

      _
      (&/fail-with-loc "[Parser Error] Unknown lexer token.")
      )))
