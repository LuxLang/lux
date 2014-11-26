(ns lang.lexer
  (:require [clojure.template :refer [do-template]]
            [clojure.core.match :refer [match]]
            [lang.util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-m try-all-m]]))

(declare lex-form)

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

(def ^:private lex-list
  (exec [_ (lex-str "(")
         members (repeat-m lex-form)
         _ (lex-str ")")]
    (return [::list members])))

(def ^:private lex-tuple
  (exec [_ (lex-str "[")
         members (repeat-m lex-form)
         _ (lex-str "]")]
    (return [::tuple members])))

(def +ident-re+ #"^([a-zA-Z\-\+\_\=!@$%^&*<>\.,/\\\|'][a-zA-Z0-9\-\+\_\=!@$%^&*<>\.,/\\\|']*)")

(do-template [<name> <tag> <regex>]
  (def <name>
    (exec [token (lex-regex <regex>)]
      (return [<tag> token])))

  ^:private lex-int   ::int    #"^(0|[1-9][0-9]*)"
  ^:private lex-ident ::ident  +ident-re+)

(def ^:private lex-tag
  (exec [_ (lex-str "#")
         token (lex-regex +ident-re+)]
    (return [::tag token])))

(def ^:private lex-form
  (exec [_ (try-m lex-white-space)
         form (try-all-m [lex-int
                          lex-ident
                          lex-tag
                          lex-list
                          lex-tuple])
         _ (try-m lex-white-space)]
    (return form)))

;; [Interface]
(defn lex [text]
  (match ((repeat-m lex-form) text)
    [::&util/ok [?state ?forms]]
    (if (empty? ?state)
      ?forms
      (assert false (str "Unconsumed input: " ?state)))
    
    [::&util/failure ?message]
    (assert false ?message)))
