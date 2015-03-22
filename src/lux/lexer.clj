(ns lux.lexer
  (:require [clojure.template :refer [do-template]]
            (lux [base :as & :refer [exec return* return fail fail*]]
                 [reader :as &reader])
            [lux.analyser.def :as &def]))

;; [Utils]
(defn ^:private escape-char [escaped]
  (condp = escaped
    "\\t"  (return "\t")
    "\\b"  (return "\b")
    "\\n"  (return "\n")
    "\\r"  (return "\r")
    "\\f"  (return "\f")
    "\\\"" (return "\"")
    "\\\\" (return "\\")
    ;; else
    (fail (str "[Lexer Error] Unknown escape character: " escaped))))

(defn ^:private lex-text-body [_____]
  (&/try-all% (&/|list (exec [[_ [_ [prefix escaped]]] (&reader/read-regex2 #"(?s)^([^\"\\]*)(\\.)")
                              unescaped (escape-char escaped)
                              [_ [_ postfix]] (lex-text-body nil)]
                         (return (str prefix unescaped postfix)))
                       (exec [[_ [_ body]] (&reader/read-regex #"(?s)^([^\"\\]*)")]
                         (return body)))))

(def ^:private +ident-re+ #"^([a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?][0-9a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?]*)")

;; [Lexers]
(def ^:private lex-white-space
  (exec [[_ [meta white-space]] (&reader/read-regex #"^(\s+)")]
    (return (&/V "lux;Meta" (&/T meta (&/V "White_Space" white-space))))))

(def ^:private lex-single-line-comment
  (exec [[_ [meta _]] (&reader/read-text "##")
         [_ [_ comment]] (&reader/read-regex #"^([^\n]*)")
         _ (&reader/read-regex #"^(\n?)")]
    (return (&/V "lux;Meta" (&/T meta (&/V "Comment" comment))))))

(defn ^:private lex-multi-line-comment [___]
  (exec [_ (&reader/read-text "#(")
         [meta comment] (&/try-all% (&/|list (exec [[_ [meta comment]] (&reader/read-regex #"(?is)^((?!#\().)*?(?=\)#)")]
                                               (return comment))
                                             (exec [[_ [meta pre]] (&reader/read-regex #"(?is)^(.+?(?=#\())")
                                                    [_ inner] (lex-multi-line-comment nil)
                                                    [_ [_ post]] (&reader/read-regex #"(?is)^(.+?(?=\)#))")]
                                               (return (str pre "#(" inner ")#" post)))))
         _ (&reader/read-text ")#")]
    (return (&/V "lux;Meta" (&/T meta (&/V "Comment" comment))))))

(def ^:private lex-comment
  (&/try-all% (&/|list lex-single-line-comment
                       ;; (lex-multi-line-comment nil)
                       )))

(do-template [<name> <tag> <regex>]
  (def <name>
    (exec [[_ [meta token]] (&reader/read-regex <regex>)]
      (return (&/V "lux;Meta" (&/T meta (&/V <tag> token))))))

  ^:private lex-bool  "Bool"  #"^(true|false)"
  ^:private lex-int   "Int"   #"^-?(0|[1-9][0-9]*)"
  ^:private lex-real  "Real"  #"^-?(0|[1-9][0-9]*)\.[0-9]+"
  )

(def ^:private lex-char
  (exec [[_ [meta _]] (&reader/read-text "#\"")
         token (&/try-all% (&/|list (exec [escaped (&reader/read-regex #"^(\\.)")]
                                      (escape-char escaped))
                                    (exec [[_ [_ char]] (&reader/read-regex #"^(.)")]
                                      (return char))))
         _ (&reader/read-text "\"")]
    (return (&/V "lux;Meta" (&/T meta (&/V "Char" token))))))

(def ^:private lex-text
  (exec [[_ [meta _]] (&reader/read-text "\"")
         token (lex-text-body nil)
         _ (&reader/read-text "\"")]
    (return (&/V "lux;Meta" (&/T meta (&/V "Text" token))))))

(def ^:private lex-ident
  (&/try-all% (&/|list (exec [[_ [meta _]] (&reader/read-text ";")
                              [_ [_ token]] (&reader/read-regex +ident-re+)
                              module-name &/get-module-name]
                         (return (&/V "lux;Meta" (&/T meta (&/T module-name token)))))
                       (exec [[_ [metma token]] (&reader/read-regex +ident-re+)]
                         (&/try-all% (&/|list (exec [_ (&reader/read-text ";")
                                                     [_ [_ local-token]] (&reader/read-regex +ident-re+)]
                                                (&/try-all% (&/|list (exec [unaliased (&def/unalias-module token)]
                                                                       (return (&/V "lux;Meta" (&/T meta (&/T unaliased local-token)))))
                                                                     (exec [? (&def/module-exists? token)]
                                                                       (if ?
                                                                         (return (&/V "lux;Meta" (&/T meta (&/T token local-token))))
                                                                         (fail (str "[Lexer Error] Unknown module: " token))))
                                                                     )))
                                              (exec [module-name &/get-module-name]
                                                (return (&/V "lux;Meta" (&/T meta (&/T module-name token)))))
                                              )))
                       )))

(def ^:private lex-symbol
  (exec [[_ [meta ident]] lex-ident]
    (return (&/V "lux;Meta" (&/T meta (&/V "Symbol" ident))))))

(def ^:private lex-tag
  (exec [[_ [meta _]] (&reader/read-text "#")
         [_ [_ ident]] lex-ident]
    (return (&/V "lux;Meta" (&/T meta (&/V "Tag" ident))))))

(do-template [<name> <text> <tag>]
  (def <name>
    (exec [[_ [meta _]] (&reader/read-text <text>)]
      (return (&/V "lux;Meta" (&/T meta (&/V <tag> nil))))))

  ^:private lex-open-paren    "(" "Open_Paren"
  ^:private lex-close-paren   ")" "Close_Paren"
  ^:private lex-open-bracket  "[" "Open_Bracket"
  ^:private lex-close-bracket "]" "Close_Bracket"
  ^:private lex-open-brace    "{" "Open_Brace"
  ^:private lex-close-brace   "}" "Close_Brace"
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
