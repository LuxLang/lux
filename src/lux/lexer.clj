(ns lux.lexer
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :refer [match]]
            [lux.util :as &util :refer [exec return* return fail fail*
                                        repeat-m try-m try-all-m]]))

(declare lex-forms lex-list lex-tuple lex-record lex-tag)

;; [Utils]
(defn ^:private lex-regex [regex]
  (fn [text]
    (if-let [[match] (re-find regex text)]
      (return* (.substring text (.length match)) match)
      (fail* (str "Pattern failed: " regex " -- " text)))))

(defn ^:private lex-regex2 [regex]
  (fn [text]
    (if-let [[match tok1 tok2] (re-find regex text)]
      (return* (.substring text (.length match)) [tok1 tok2])
      (fail* (str "Pattern failed: " regex " -- " text)))))

(defn ^:private lex-str [prefix]
  (fn [text]
    (if (.startsWith text prefix)
      (return* (.substring text (.length prefix)) prefix)
      (fail* (str "String failed: " prefix " -- " text)))))

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
    (fail (str "Unknown escape character: " escaped))))

(def ^:private lex-string-body
  (try-all-m [(exec [[prefix escaped] (lex-regex2 #"(?s)^([^\"\\]*)(\\.)")
                     unescaped (escape-char escaped)
                     postfix lex-string-body]
                (return (str prefix unescaped postfix)))
              (lex-regex #"(?s)^([^\"\\]*)")]))

(def ^:private +ident-re+ #"^([a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|':\~\?][0-9a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|':\~\?]*)")

;; [Lexers]
(def ^:private lex-white-space (lex-regex #"^(\s+)"))

(do-template [<name> <tag> <regex>]
  (def <name>
    (exec [token (lex-regex <regex>)]
      (return [<tag> token])))

  ^:private lex-bool  ::bool  #"^(true|false)"
  ^:private lex-real  ::real  #"^(0|[1-9][0-9]*)\.[0-9]+"
  ^:private lex-int   ::int   #"^(0|[1-9][0-9]*)"
  ^:private lex-ident ::ident +ident-re+)

(def ^:private lex-char
  (exec [_ (lex-str "#\"")
         token (try-all-m [(exec [escaped (lex-regex #"^(\\.)")]
                             (escape-char escaped))
                           (lex-regex #"^(.)")])
         _ (lex-str "\"")]
    (return [::char token])))

(def ^:private lex-text
  (exec [_ (lex-str "\"")
         token lex-string-body
         _ (lex-str "\"")]
    (return [::text token])))

(def ^:private lex-single-line-comment
  (exec [_ (lex-str "##")
         comment (lex-regex #"^([^\n]*)")
         _ (lex-regex #"^(\n?)")]
    (return [::comment comment])))

(def ^:private lex-multi-line-comment
  (exec [_ (lex-str "#(")
         comment (try-all-m [(lex-regex #"(?is)^((?!#\().)*?(?=\)#)")
                             (exec [pre (lex-regex #"(?is)^(.+?(?=#\())")
                                    [_ inner] lex-multi-line-comment
                                    post (lex-regex #"(?is)^(.+?(?=\)#))")]
                               (return (str pre "#(" inner ")#" post)))])
         _ (lex-str ")#")]
    (return [::comment comment])))

(def ^:private lex-comment
  (try-all-m [lex-single-line-comment
              lex-multi-line-comment]))

(def ^:private lex-tag
  (exec [_ (lex-str "#")
         token (lex-regex +ident-re+)]
    (return [::tag token])))

(def ^:private lex-form
  (exec [_ (try-m lex-white-space)
         form (try-all-m [lex-bool
                          lex-real
                          lex-int
                          lex-char
                          lex-text
                          lex-ident
                          lex-tag
                          lex-list
                          lex-tuple
                          lex-record
                          lex-comment])
         _ (try-m lex-white-space)]
    (return form)))

(def lex-forms
  (exec [forms (repeat-m lex-form)]
    (return (filter #(match %
                       [::comment _]
                       false
                       _
                       true)
                    forms))))

(def ^:private lex-list
  (exec [_ (lex-str "(")
         members lex-forms
         _ (lex-str ")")]
    (return [::list members])))

(def ^:private lex-tuple
  (exec [_ (lex-str "[")
         members lex-forms
         _ (lex-str "]")]
    (return [::tuple members])))

(def ^:private lex-record
  (exec [_ (lex-str "{")
         members lex-forms
         _ (lex-str "}")]
    (return [::record members])))

;; [Interface]
(defn lex [text]
  (match (lex-forms text)
    [::&util/ok [?state ?forms]]
    (if (empty? ?state)
      ?forms
      (assert false (str "Unconsumed input: " ?state)))
    
    [::&util/failure ?message]
    (assert false ?message)))
