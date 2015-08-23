;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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
  (|do [[meta white-space] (&reader/read-regex #"^(\s+)")]
    (return (&/P meta (&/S $White_Space white-space)))))

(def ^:private lex-single-line-comment
  (|do [_ (&reader/read-text "##")
        [meta comment] (&reader/read-regex #"^(.*)$")]
    (return (&/P meta (&/S $Comment comment)))))

(defn ^:private lex-multi-line-comment [_]
  (|do [_ (&reader/read-text "#(")
        [meta comment] (&/try-all% (&/|list (|do [[meta comment] (&reader/read-regex #"(?is)^(?!#\()(.*?(?=\)#))")
                                                  ;; :let [_ (prn 'immediate comment)]
                                                  _ (&reader/read-text ")#")]
                                              (return (&/P meta comment)))
                                            (|do [;; :let [_ (prn 'pre/_0)]
                                                  [meta pre] (&reader/read-regex+ #"(?is)^(.*?)(#\(|$)")
                                                  ;; :let [_ (prn 'pre pre)]
                                                  [_ inner] (lex-multi-line-comment nil)
                                                  ;; :let [_ (prn 'inner inner)]
                                                  [_ post] (&reader/read-regex #"(?is)^(.+?(?=\)#))")
                                                  ;; :let [_ (prn 'post post (str pre "#(" inner ")#" post))]
                                                  ]
                                              (return (&/P meta (str pre "#(" inner ")#" post))))))
        ;; :let [_ (prn 'lex-multi-line-comment (str comment ")#"))]
        _ (&reader/read-text ")#")]
    (return (&/P meta (&/S $Comment comment)))))

(def ^:private lex-comment
  (&/try-all% (&/|list lex-single-line-comment
                       (lex-multi-line-comment nil))))

(do-template [<name> <tag> <regex>]
  (def <name>
    (|do [[meta token] (&reader/read-regex <regex>)]
      (return (&/P meta (&/S <tag> token)))))

  ^:private lex-bool  $Bool  #"^(true|false)"
  ^:private lex-int   $Int   #"^(-?0|-?[1-9][0-9]*)"
  ^:private lex-real  $Real  #"^-?(-?0\.[0-9]+|-?[1-9][0-9]*\.[0-9]+)"
  )

(def ^:private lex-char
  (|do [[meta _] (&reader/read-text "#\"")
        token (&/try-all% (&/|list (|do [[_ escaped] (&reader/read-regex #"^(\\.)")]
                                     (escape-char escaped))
                                   (|do [[_ char] (&reader/read-regex #"^(.)")]
                                     (return char))))
        _ (&reader/read-text "\"")]
    (return (&/P meta (&/S $Char token)))))

(def ^:private lex-text
  (|do [[meta _] (&reader/read-text "\"")
        token (lex-text-body nil)
        _ (&reader/read-text "\"")]
    (return (&/P meta (&/S $Text token)))))

(def ^:private lex-ident
  (&/try-all% (&/|list (|do [[meta token] (&reader/read-regex +ident-re+)]
                         (&/try-all% (&/|list (|do [_ (&reader/read-text ";")
                                                    [_ local-token] (&reader/read-regex +ident-re+)
                                                    ? (&module/exists? token)]
                                                (if ?
                                                  (return (&/P meta (&/P token local-token)))
                                                  (|do [unaliased (do ;; (prn "Unaliasing: " token ";" local-token)
                                                                      (&module/dealias token))]
                                                    (do ;; (prn "Unaliased: " unaliased ";" local-token)
                                                        (return (&/P meta (&/P unaliased local-token)))))))
                                              (return (&/P meta (&/P "" token)))
                                              )))
                       (|do [[meta _] (&reader/read-text ";;")
                             [_ token] (&reader/read-regex +ident-re+)
                             module-name &/get-module-name]
                         (return (&/P meta (&/P module-name token))))
                       (|do [[meta _] (&reader/read-text ";")
                             [_ token] (&reader/read-regex +ident-re+)]
                         (return (&/P meta (&/P &/prelude-name token))))
                       )))

(def ^:private lex-symbol
  (|do [[meta ident] lex-ident]
    (return (&/P meta (&/S $Symbol ident)))))

(def ^:private lex-tag
  (|do [[meta _] (&reader/read-text "#")
        [_ ident] lex-ident]
    (return (&/P meta (&/S $Tag ident)))))

(do-template [<name> <text> <tag>]
  (def <name>
    (|do [[meta _] (&reader/read-text <text>)]
      (return (&/P meta (&/S <tag> nil)))))

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
