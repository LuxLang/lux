;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.parser
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return fail]]
                 [lexer :as &lexer])))

;; [Utils]
(do-template [<name> <close-tag> <description> <tag>]
  (defn <name> [parse]
    (|do [elems (&/repeat% parse)
          token &lexer/lex]
      (matchv ::M/objects [token]
        [["lux;Meta" [meta [<close-token> _]]]]
        (return (&/V <tag> (&/fold &/|++ (&/|list) elems)))
        
        [_]
        (fail (str "[Parser Error] Unbalanced " <description> ".")))))

  ^:private parse-form  "Close_Paren"   "parantheses" "lux;FormS"
  ^:private parse-tuple "Close_Bracket" "brackets"    "lux;TupleS"
  )

(defn ^:private parse-record [parse]
  (|do [elems* (&/repeat% parse)
        token &lexer/lex
        :let [elems (&/fold &/|++ (&/|list) elems*)]]
    (matchv ::M/objects [token]
      [["lux;Meta" [meta ["Close_Brace" _]]]]
      (if (even? (&/|length elems))
        (return (&/V "lux;RecordS" (&/|as-pairs elems)))
        (fail (str "[Parser Error] Records must have an even number of elements.")))
      
      [_]
      (fail (str "[Parser Error] Unbalanced braces.")))))

;; [Interface]
(def parse
  (|do [token &lexer/lex]
    (matchv ::M/objects [token]
      [["lux;Meta" [meta token*]]]
      (matchv ::M/objects [token*]
        [["White_Space" _]]
        (return (&/|list))

        [["Comment" _]]
        (return (&/|list))
        
        [["Bool" ?value]]
        (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;BoolS" (Boolean/parseBoolean ?value))))))

        [["Int" ?value]]
        (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;IntS" (Integer/parseInt ?value))))))

        [["Real" ?value]]
        (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;RealS" (Float/parseFloat ?value))))))

        [["Char" ^String ?value]]
        (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;CharS" (.charAt ?value 0))))))

        [["Text" ?value]]
        (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;TextS" ?value)))))

        [["Symbol" ?ident]]
        (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;SymbolS" ?ident)))))

        [["Tag" ?ident]]
        (return (&/|list (&/V "lux;Meta" (&/T meta (&/V "lux;TagS" ?ident)))))

        [["Open_Paren" _]]
        (|do [syntax (parse-form parse)]
          (return (&/|list (&/V "lux;Meta" (&/T meta syntax)))))
        
        [["Open_Bracket" _]]
        (|do [syntax (parse-tuple parse)]
          (return (&/|list (&/V "lux;Meta" (&/T meta syntax)))))

        [["Open_Brace" _]]
        (|do [syntax (parse-record parse)]
          (return (&/|list (&/V "lux;Meta" (&/T meta syntax)))))

        [_]
        (fail "[Parser Error] Unknown lexer token.")
        ))))
