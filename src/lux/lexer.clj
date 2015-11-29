;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.lexer
  (:require [clojure.template :refer [do-template]]
            (lux [base :as & :refer [deftags |do return* return fail fail*]]
                 [reader :as &reader])
            [lux.analyser.module :as &module]))

;; [Tags]
(deftags
  ["White_Space"
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
   "Close_Brace"]
  )

;; [Utils]
(defn ^:private escape-char [escaped]
  (cond (.equals ^Object escaped "\\t")  (return "\t")
        (.equals ^Object escaped "\\b")  (return "\b")
        (.equals ^Object escaped "\\n")  (return "\n")
        (.equals ^Object escaped "\\r")  (return "\r")
        (.equals ^Object escaped "\\f")  (return "\f")
        (.equals ^Object escaped "\\\"") (return "\"")
        (.equals ^Object escaped "\\\\") (return "\\")
        :else
        (fail (str "[Lexer Error] Unknown escape character: " escaped))))

(defn ^:private lex-text-body [_]
  (&/try-all% (&/|list (|do [[_ [prefix escaped]] (&reader/read-regex2 #"(?s)^([^\"\\]*)(\\.)")
                             unescaped (escape-char escaped)
                             postfix (lex-text-body nil)]
                         (return (str prefix unescaped postfix)))
                       (|do [[_ body] (&reader/read-regex #"(?s)^([^\"\\]*)")]
                         (return body)))))

(def ^:private +ident-re+ #"^([a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?][0-9a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?]*)"
  ;; #"^([^0-9\[\]\(\)\{\};#\s\"][^\[\]\(\)\{\};#\s\"]*)"
  )

;; [Lexers]
(def ^:private lex-white-space
  (|do [[meta white-space] (&reader/read-regex #"^(\s+|$)")]
    (return (&/T meta (&/V $White_Space white-space)))))

(def ^:private lex-single-line-comment
  (|do [_ (&reader/read-text "##")
        [meta comment] (&reader/read-regex #"^(.*)$")]
    (return (&/T meta (&/V $Comment comment)))))

(defn ^:private lex-multi-line-comment [_]
  (|do [_ (&reader/read-text "#(")
        [meta comment] (&/try-all% (&/|list (|do [[meta comment] (&reader/read-regex+ #"(?is)^(?!#\()((?!\)#).)*")]
                                              (return (&/T meta comment)))
                                            (|do [[meta pre] (&reader/read-regex+ #"(?is)^((?!#\().)*")
                                                  [_ ($Comment inner)] (lex-multi-line-comment nil)
                                                  [_ post] (&reader/read-regex+ #"(?is)^((?!\)#).)*")]
                                              (return (&/T meta (str pre "#(" inner ")#" post))))))
        _ (&reader/read-text ")#")]
    (return (&/T meta (&/V $Comment comment)))))

(def ^:private lex-comment
  (&/try-all% (&/|list lex-single-line-comment
                       (lex-multi-line-comment nil))))

(do-template [<name> <tag> <regex>]
  (def <name>
    (|do [[meta token] (&reader/read-regex <regex>)]
      (return (&/T meta (&/V <tag> token)))))

  ^:private lex-bool  $Bool  #"^(true|false)"
  ^:private lex-int   $Int   #"^-?(0|[1-9][0-9]*)"
  ^:private lex-real  $Real  #"^-?(0\.[0-9]+|[1-9][0-9]*\.[0-9]+)"
  )

(def ^:private lex-char
  (|do [[meta _] (&reader/read-text "#\"")
        token (&/try-all% (&/|list (|do [[_ escaped] (&reader/read-regex #"^(\\.)")]
                                     (escape-char escaped))
                                   (|do [[_ char] (&reader/read-regex #"^(.)")]
                                     (return char))))
        _ (&reader/read-text "\"")]
    (return (&/T meta (&/V $Char token)))))

(def ^:private lex-text
  (|do [[meta _] (&reader/read-text "\"")
        token (lex-text-body nil)
        _ (&reader/read-text "\"")]
    (return (&/T meta (&/V $Text token)))))

(def ^:private lex-ident
  (&/try-all% (&/|list (|do [[meta token] (&reader/read-regex +ident-re+)]
                         (&/try-all% (&/|list (|do [_ (&reader/read-text ";")
                                                    [_ local-token] (&reader/read-regex +ident-re+)
                                                    ? (&module/exists? token)]
                                                (if ?
                                                  (return (&/T meta (&/T token local-token)))
                                                  (|do [unaliased (&module/dealias token)]
                                                    (return (&/T meta (&/T unaliased local-token))))))
                                              (return (&/T meta (&/T "" token)))
                                              )))
                       (|do [[meta _] (&reader/read-text ";;")
                             [_ token] (&reader/read-regex +ident-re+)
                             module-name &/get-module-name]
                         (return (&/T meta (&/T module-name token))))
                       (|do [[meta _] (&reader/read-text ";")
                             [_ token] (&reader/read-regex +ident-re+)]
                         (return (&/T meta (&/T "lux" token))))
                       )))

(def ^:private lex-symbol
  (|do [[meta ident] lex-ident]
    (return (&/T meta (&/V $Symbol ident)))))

(def ^:private lex-tag
  (|do [[meta _] (&reader/read-text "#")
        [_ ident] lex-ident]
    (return (&/T meta (&/V $Tag ident)))))

(do-template [<name> <text> <tag>]
  (def <name>
    (|do [[meta _] (&reader/read-text <text>)]
      (return (&/T meta (&/V <tag> nil)))))

  ^:private lex-open-paren    "(" $Open_Paren
  ^:private lex-close-paren   ")" $Close_Paren
  ^:private lex-open-bracket  "[" $Open_Bracket
  ^:private lex-close-bracket "]" $Close_Bracket
  ^:private lex-open-brace    "{" $Open_Brace
  ^:private lex-close-brace   "}" $Close_Brace
  )

(def ^:private lex-delimiter
  (&/try-all% (&/|list lex-open-paren
                       lex-close-paren
                       lex-open-bracket
                       lex-close-bracket
                       lex-open-brace
                       lex-close-brace)))

;; [Exports]
(def lex
  (&/try-all% (&/|list lex-white-space
                       lex-comment
                       lex-bool
                       lex-real
                       lex-int
                       lex-char
                       lex-text
                       lex-symbol
                       lex-tag
                       lex-delimiter)))
