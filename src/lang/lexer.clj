(ns lang.lexer
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :refer [match]]
            [lang.util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-m try-all-m]]))

(declare lex-forms lex-list lex-tuple lex-record lex-tag)

;; [Utils]
(defn ^:private lex-regex [regex]
  (fn [text]
    (if-let [[match] (re-find regex text)]
      (return* (.substring text (.length match)) match)
      (fail* (str "Pattern failed: " regex " -- " text)))))

(defn ^:private lex-str [prefix]
  (fn [text]
    (if (.startsWith text prefix)
      (return* (.substring text (.length prefix)) prefix)
      (fail* (str "String failed: " prefix " -- " text)))))

;; [Lexers]
(def ^:private lex-white-space (lex-regex #"^(\s+)"))

(def +ident-re+ #"^([a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|':][0-9a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|':]*)")

(do-template [<name> <tag> <regex>]
  (def <name>
    (exec [token (lex-regex <regex>)]
      (return [<tag> token])))

  ^:private lex-boolean ::boolean #"^(true|false)"
  ^:private lex-float   ::float   #"^(0|[1-9][0-9]*)\.[0-9]+"
  ^:private lex-int     ::int     #"^(0|[1-9][0-9]*)"
  ^:private lex-ident   ::ident   +ident-re+)

(def lex-string
  (exec [_ (lex-str "\"")
         token (lex-regex #"^(.+?(?=\"))")
         _ (lex-str "\"")
         ]
    (return [::string token])))

(def ^:private lex-single-line-comment
  (exec [_ (lex-str "##")
         comment (lex-regex #"^([^\n]*)")
         _ (lex-regex #"^(\n?)")
         ;; :let [_ (prn 'comment comment)]
         ]
    (return [::comment comment])))

(def ^:private lex-multi-line-comment
  (exec [_ (lex-str "#(")
         ;; :let [_ (prn 'OPEN)]
         ;; comment (lex-regex #"^(#\(.*\)#)")
         comment (try-all-m [(lex-regex #"(?is)^((?!#\().)*?(?=\)#)")
                             (exec [pre (lex-regex #"(?is)^(.+?(?=#\())")
                                    ;; :let [_ (prn 'PRE pre)]
                                    [_ inner] lex-multi-line-comment
                                    ;; :let [_ (prn 'INNER inner)]
                                    post (lex-regex #"(?is)^(.+?(?=\)#))")
                                    ;:let [_ (prn 'POST post)]
                                    ]
                               (return (str pre "#(" inner ")#" post)))])
         ;; :let [_ (prn 'COMMENT comment)]
         _ (lex-str ")#")
         ;; :let [_ (prn 'CLOSE)]
         :let [_ (prn 'multi-comment comment)]
         ]
    (return [::comment comment])))

(def ^:private lex-tag
  (exec [_ (lex-str "#")
         token (lex-regex +ident-re+)]
    (return [::tag token])))

(def ^:private lex-form
  (exec [_ (try-m lex-white-space)
         form (try-all-m [lex-boolean
                          lex-float
                          lex-int
                          lex-string
                          lex-ident
                          lex-tag
                          lex-list
                          lex-tuple
                          lex-record
                          lex-single-line-comment
                          lex-multi-line-comment])
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
