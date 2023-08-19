;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns lux.parser
  (:require [clojure.template :refer [do-template]]
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return |case]]
                 [lexer :as &lexer])))

;; [Utils]
(defn ^:private repeat% [action]
  (fn [state]
    (|case (action state)
      (&/$Left ^String error)
      (if (.contains error "[Parser Error]")
        (&/$Right (&/T [state &/$End]))
        (&/$Left error))

      (&/$Right state* head)
      ((|do [tail (repeat% action)]
         (return (&/$Item head tail)))
       state*))))

(do-template [<name> <close-tag> <description> <tag>]
  (defn <name> [parse]
    (|do [elems (repeat% parse)
          token &lexer/lex]
      (|case token
        [meta (<close-tag> _)]
        (return (<tag> (&/fold &/|++ &/$End elems)))
        
        _
        (&/fail-with-loc (str "[Parser Error] Unbalanced " <description> "."))
        )))

  ^:private parse-form    &lexer/$Close_Paren   "parantheses" &/$Form
  ^:private parse-variant &lexer/$Close_Brace   "braces"      &/$Variant
  ^:private parse-tuple   &lexer/$Close_Bracket "brackets"    &/$Tuple
  )

;; [Interface]
(def parse
  (|do [token &lexer/lex
        :let [[meta token*] token]]
    (|case token*
      (&lexer/$White_Space _)
      (return &/$End)

      (&lexer/$Comment _)
      (return &/$End)
      
      (&lexer/$Bit ?value)
      (return (&/|list (&/T [meta (&/$Bit (.equals ^String ?value "#1"))])))

      (&lexer/$Nat ?value)
      (return (&/|list (&/T [meta (&/$Nat (Long/parseUnsignedLong ?value))])))

      (&lexer/$Int ?value)
      (return (&/|list (&/T [meta (&/$Int (Long/parseLong ?value))])))

      (&lexer/$Rev ?value)
      (return (&/|list (&/T [meta (&/$Rev (&/decode-rev ?value))])))

      (&lexer/$Dec ?value)
      (return (&/|list (&/T [meta (&/$Dec (Double/parseDouble ?value))])))

      (&lexer/$Text ?value)
      (return (&/|list (&/T [meta (&/$Text ?value)])))

      (&lexer/$Identifier ?ident)
      (return (&/|list (&/T [meta (&/$Identifier ?ident)])))

      (&lexer/$Open_Paren _)
      (|do [syntax (parse-form parse)]
        (return (&/|list (&/T [meta syntax]))))
      
      (&lexer/$Open_Brace _)
      (|do [syntax (parse-variant parse)]
        (return (&/|list (&/T [meta syntax]))))

      (&lexer/$Open_Bracket _)
      (|do [syntax (parse-tuple parse)]
        (return (&/|list (&/T [meta syntax]))))

      _
      (&/fail-with-loc "[Parser Error] Unknown lexer token.")
      )))
