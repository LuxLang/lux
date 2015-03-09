(ns lux.lexer
  (:require [clojure.template :refer [do-template]]
            [lux.base :as & :refer [exec return* return fail fail*
                                    repeat-m try-m try-all-m]]))

;; [Utils]
(defn ^:private lex-regex [regex]
  (fn [state]
    (if-let [[match] (re-find regex (::&/source state))]
      (return* (update-in state [::&/source] #(.substring % (.length match))) match)
      (fail* (str "[Lexer Error] Pattern failed: " regex)))))

(defn ^:private lex-regex2 [regex]
  (fn [state]
    (if-let [[match tok1 tok2] (re-find regex (::&/source state))]
      (return* (update-in state [::&/source] #(.substring % (.length match))) [tok1 tok2])
      (fail* (str "[Lexer Error] Pattern failed: " regex)))))

(defn ^:private lex-prefix [prefix]
  (fn [state]
    (if (.startsWith (::&/source state) prefix)
      (return* (update-in state [::&/source] #(.substring % (.length prefix))) prefix)
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
  (try-all-m [(exec [[prefix escaped] (lex-regex2 #"(?s)^([^\"\\]*)(\\.)")
                     unescaped (escape-char escaped)
                     postfix lex-text-body]
                (return (str prefix unescaped postfix)))
              (lex-regex #"(?s)^([^\"\\]*)")]))

(def ^:private +ident-re+ #"^([a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?][0-9a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?]*)(;[0-9a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'`:\~\?]+)?")

;; [Lexers]
(def ^:private lex-white-space
  (exec [white-space (lex-regex #"^(\s+)")]
    (return [::white-space white-space])))

(def ^:private lex-single-line-comment
  (exec [_ (lex-prefix "##")
         comment (lex-regex #"^([^\n]*)")
         _ (lex-regex #"^(\n?)")]
    (return [::comment comment])))

(def ^:private lex-multi-line-comment
  (exec [_ (lex-prefix "#(")
         comment (try-all-m [(lex-regex #"(?is)^((?!#\().)*?(?=\)#)")
                             (exec [pre (lex-regex #"(?is)^(.+?(?=#\())")
                                    [_ inner] lex-multi-line-comment
                                    post (lex-regex #"(?is)^(.+?(?=\)#))")]
                               (return (str pre "#(" inner ")#" post)))])
         _ (lex-prefix ")#")]
    (return [::comment comment])))

(def ^:private lex-comment
  (try-all-m [lex-single-line-comment
              lex-multi-line-comment]))

(do-template [<name> <tag> <regex>]
  (def <name>
    (exec [token (lex-regex <regex>)]
      (return [<tag> token])))

  ^:private lex-bool  ::bool  #"^(true|false)"
  ^:private lex-real  ::real  #"^-?(0|[1-9][0-9]*)\.[0-9]+"
  ^:private lex-int   ::int   #"^-?(0|[1-9][0-9]*)"
  ^:private lex-ident ::ident +ident-re+)

(def ^:private lex-char
  (exec [_ (lex-prefix "#\"")
         token (try-all-m [(exec [escaped (lex-regex #"^(\\.)")]
                             (escape-char escaped))
                           (lex-regex #"^(.)")])
         _ (lex-prefix "\"")]
    (return [::char token])))

(def ^:private lex-text
  (exec [_ (lex-prefix "\"")
         token lex-text-body
         _ (lex-prefix "\"")]
    (return [::text token])))

(def ^:private lex-tag
  (exec [_ (lex-prefix "#")
         token (lex-regex +ident-re+)]
    (return [::tag token])))

(do-template [<name> <text> <tag>]
  (def <name>
    (exec [_ (lex-prefix <text>)]
      (return [<tag>])))

  ^:private lex-open-paren    "(" ::open-paren
  ^:private lex-close-paren   ")" ::close-paren
  ^:private lex-open-bracket  "[" ::open-bracket
  ^:private lex-close-bracket "]" ::close-bracket
  ^:private lex-open-brace    "{" ::open-brace
  ^:private lex-close-brace   "}" ::close-brace
  )

(def ^:private lex-delimiter
  (try-all-m [lex-open-paren
              lex-close-paren
              lex-open-bracket
              lex-close-bracket
              lex-open-brace
              lex-close-brace]))

;; [Exports]
(def lex
  (try-all-m [lex-white-space
              lex-comment
              lex-bool
              lex-real
              lex-int
              lex-char
              lex-text
              lex-ident
              lex-tag
              lex-delimiter]))
