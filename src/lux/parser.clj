;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

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
      (cond (.contains error base-uneven-record-error)
            (&/$Left error)

            (not (.contains error "[Parser Error]"))
            (&/$Left error)

            :else
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

  ^:private parse-form  &lexer/$Close_Paren   "parantheses" &/$FormS
  ^:private parse-tuple &lexer/$Close_Bracket "brackets"    &/$TupleS
  )

(defn ^:private parse-record [parse]
  (|do [elems* (repeat% parse)
        token &lexer/lex
        :let [elems (&/fold &/|++ &/$Nil elems*)]]
    (|case token
      [meta (&lexer/$Close_Brace _)]
      (if (even? (&/|length elems))
        (return (&/$RecordS (&/|as-pairs elems)))
        (&/fail-with-loc base-uneven-record-error))
      
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
      
      (&lexer/$Bool ?value)
      (return (&/|list (&/T [meta (&/$BoolS (Boolean/parseBoolean ?value))])))

      (&lexer/$Int ?value)
      (return (&/|list (&/T [meta (&/$IntS (Long/parseLong ?value))])))

      (&lexer/$Real ?value)
      (return (&/|list (&/T [meta (&/$RealS (Double/parseDouble ?value))])))

      (&lexer/$Char ^String ?value)
      (return (&/|list (&/T [meta (&/$CharS (.charAt ?value 0))])))

      (&lexer/$Text ?value)
      (return (&/|list (&/T [meta (&/$TextS ?value)])))

      (&lexer/$Symbol ?ident)
      (return (&/|list (&/T [meta (&/$SymbolS ?ident)])))

      (&lexer/$Tag ?ident)
      (return (&/|list (&/T [meta (&/$TagS ?ident)])))

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
