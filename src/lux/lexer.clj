(ns lux.lexer
  (:require [clojure.template :refer [do-template]]
            [lux.base :as & :refer [exec return* return fail fail*]]))

;; [Utils]
(defn ^:private lex-regex [regex]
  (fn [state]
    (if-let [[match] (re-find regex (&/get$ "source" state))]
      (return* (&/update$ "source" #(.substring % (.length match)) state) match)
      (fail* (str "[Lexer Error] Pattern failed: " regex)))))

(defn ^:private lex-regex2 [regex]
  (fn [state]
    (if-let [[match tok1 tok2] (re-find regex (&/get$ "source" state))]
      (return* (&/update$ "source" #(.substring % (.length match)) state) [tok1 tok2])
      (fail* (str "[Lexer Error] Pattern failed: " regex)))))

(defn ^:private lex-prefix [prefix]
  (fn [state]
    (if (.startsWith (&/get$ "source" state) prefix)
      (return* (&/update$ "source" #(.substring % (.length prefix)) state) prefix)
      (fail* (str "[Lexer Error] Text failed: " prefix)))))

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

(def ^:private lex-text-body
  (&/try-all% (&/|list (exec [[prefix escaped] (lex-regex2 #"(?s)^([^\"\\]*)(\\.)")
                          unescaped (escape-char escaped)
                          postfix lex-text-body]
                     (return (str prefix unescaped postfix)))
                   (lex-regex #"(?s)^([^\"\\]*)"))))

(def ^:private +ident-re+ #"^([a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?][0-9a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?]*)(;[0-9a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?]+)?")

;; [Lexers]
(def ^:private lex-white-space
  (exec [white-space (lex-regex #"^(\s+)")]
    (return (&/V "White_Space" white-space))))

(def ^:private lex-single-line-comment
  (exec [_ (lex-prefix "##")
         comment (lex-regex #"^([^\n]*)")
         _ (lex-regex #"^(\n?)")]
    (return (&/V "Comment" comment))))

(def ^:private lex-multi-line-comment
  (exec [_ (lex-prefix "#(")
         comment (&/try-all% (&/|list (lex-regex #"(?is)^((?!#\().)*?(?=\)#)")
                                  (exec [pre (lex-regex #"(?is)^(.+?(?=#\())")
                                         [_ inner] lex-multi-line-comment
                                         post (lex-regex #"(?is)^(.+?(?=\)#))")]
                                    (return (str pre "#(" inner ")#" post)))))
         _ (lex-prefix ")#")]
    (return (&/V "Comment" comment))))

(def ^:private lex-comment
  (&/try-all% (&/|list lex-single-line-comment
                   lex-multi-line-comment)))

(do-template [<name> <tag> <regex>]
  (def <name>
    (exec [token (lex-regex <regex>)]
      (return (&/V <tag> token))))

  ^:private lex-bool  "Bool"  #"^(true|false)"
  ^:private lex-real  "Real"  #"^-?(0|[1-9][0-9]*)\.[0-9]+"
  ^:private lex-int   "Int"   #"^-?(0|[1-9][0-9]*)"
  ^:private lex-ident "Ident" +ident-re+)

(def ^:private lex-char
  (exec [_ (lex-prefix "#\"")
         token (&/try-all% (&/|list (exec [escaped (lex-regex #"^(\\.)")]
                                  (escape-char escaped))
                                (lex-regex #"^(.)")))
         _ (lex-prefix "\"")]
    (return (&/V "Char" token))))

(def ^:private lex-text
  (exec [_ (lex-prefix "\"")
         token lex-text-body
         _ (lex-prefix "\"")]
    (return (&/V "Text" token))))

(def ^:private lex-tag
  (exec [_ (lex-prefix "#")
         token (lex-regex +ident-re+)]
    (return (&/V "Tag" token))))

(do-template [<name> <text> <tag>]
  (def <name>
    (exec [_ (lex-prefix <text>)]
      (return (&/V <tag> nil))))

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
                   lex-ident
                   lex-tag
                   lex-delimiter)))
