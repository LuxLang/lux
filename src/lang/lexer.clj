(ns lang.lexer
  (:require [clojure.core.match :refer [match]]
            [lang.util :as &util :refer [exec return* return fail fail*
                                         repeat-m try-m try-all-m]]))

(declare lex-form)

;; [Utils]
(defn ^:private lex-regex [regex]
  (fn [text]
    (if-let [[match] (re-find regex text)]
      (return* (.substring text (.length match)) match)
      (fail* "Pattern failed."))))

(defn ^:private lex-str [str]
  (fn [text]
    (if (.startsWith text str)
      (return* (.substring text (.length str)) str)
      (fail* "String failed."))))

;; [Lexers]
(def ^:private lex-white-space (lex-regex #"^(\s+)"))

(def ^:private lex-list
  (exec [_ (lex-str "(")
         members (repeat-m lex-form)
         _ (lex-str ")")]
    (return [::list members])))

(def ^:private lex-ident
  (exec [token (lex-regex #"^([a-zA-Z!@$%^&*<>\.,/\\\|][a-zA-Z0-9!@$%^&*<>\.,/\\\|]*)")]
    (return [::ident token])))

(def ^:private lex-int
  (exec [token (lex-regex #"^([1-9][0-9]*)")]
    (return [::int token])))

(def ^:private lex-form
  (exec [_ (try-m lex-white-space)
         form (try-all-m [lex-list
                          lex-ident
                          lex-int
                          ])
         _ (try-m lex-white-space)]
    (return form)))

;; [Interface]
(defn lex [text]
  (match ((exec [_ (try-m lex-white-space)
                 form lex-list
                 _ (try-m lex-white-space)]
            (return form))
          text)
    [::&util/ok [?state ?datum]]
    (if (empty? ?state)
      ?datum
      (assert false (str "Unconsumed input: " ?state)))
    
    [::&util/failure ?message]
    (assert false ?message)))
