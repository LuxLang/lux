;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.parser
  (:require [clojure.template :refer [do-template]]
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftags |do return fail |case]]
                 [lexer :as &lexer])))

;; [Tags]
(deftags ""
  "White_Space"
  "Comment"
  "Bool"
  "Int"
  "Real"
  "Char"
  "Text"
  "Symbol"
  "Tag"
  "Open_Paren"
  "Close_Paren"
  "Open_Bracket"
  "Close_Bracket"
  "Open_Brace"
  "Close_Brace"
  )

;; [Utils]
(do-template [<name> <close-tag> <description> <tag>]
  (defn <name> [parse]
    (|do [elems (&/repeat% parse)
          token &lexer/lex]
      (|case token
        (&/$Meta meta [<close-token> _])
        (return (&/V <tag> (&/fold &/|++ (&/|list) elems)))
        
        _
        (fail (str "[Parser Error] Unbalanced " <description> ".")))))

  ^:private parse-form  $Close_Paren   "parantheses" &/$FormS
  ^:private parse-tuple $Close_Bracket "brackets"    &/$TupleS
  )

(defn ^:private parse-record [parse]
  (|do [elems* (&/repeat% parse)
        token &lexer/lex
        :let [elems (&/fold &/|++ (&/|list) elems*)]]
    (|case token
      (&/$Meta meta ($Close_Brace _))
      (if (even? (&/|length elems))
        (return (&/V &/$RecordS (&/|as-pairs elems)))
        (fail (str "[Parser Error] Records must have an even number of elements.")))
      
      _
      (fail (str "[Parser Error] Unbalanced braces.")))))

;; [Interface]
(def parse
  (|do [token &lexer/lex
        :let [(&/$Meta meta token*) token]]
    (|case token*
      ($White_Space _)
      (return (&/|list))

      ($Comment _)
      (return (&/|list))
      
      ($Bool ?value)
      (return (&/|list (&/V &/$Meta (&/T meta (&/V &/$BoolS (Boolean/parseBoolean ?value))))))

      ($Int ?value)
      (return (&/|list (&/V &/$Meta (&/T meta (&/V &/$IntS (Long/parseLong ?value))))))

      ($Real ?value)
      (return (&/|list (&/V &/$Meta (&/T meta (&/V &/$RealS (Double/parseDouble ?value))))))

      ($Char ^String ?value)
      (return (&/|list (&/V &/$Meta (&/T meta (&/V &/$CharS (.charAt ?value 0))))))

      ($Text ?value)
      (return (&/|list (&/V &/$Meta (&/T meta (&/V &/$TextS ?value)))))

      ($Symbol ?ident)
      (return (&/|list (&/V &/$Meta (&/T meta (&/V &/$SymbolS ?ident)))))

      ($Tag ?ident)
      (return (&/|list (&/V &/$Meta (&/T meta (&/V &/$TagS ?ident)))))

      ($Open_Paren _)
      (|do [syntax (parse-form parse)]
        (return (&/|list (&/V &/$Meta (&/T meta syntax)))))
      
      ($Open_Bracket _)
      (|do [syntax (parse-tuple parse)]
        (return (&/|list (&/V &/$Meta (&/T meta syntax)))))

      ($Open_Brace _)
      (|do [syntax (parse-record parse)]
        (return (&/|list (&/V &/$Meta (&/T meta syntax)))))

      _
      (fail "[Parser Error] Unknown lexer token.")
      )))
