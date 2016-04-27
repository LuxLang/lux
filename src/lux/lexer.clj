;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.lexer
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            (lux [base :as & :refer [defvariant |do return* return fail fail*]]
                 [reader :as &reader])
            [lux.analyser.module :as &module]))

;; [Tags]
(defvariant
  ("White_Space" 1)
  ("Comment" 1)
  ("Bool" 1)
  ("Int" 1)
  ("Real" 1)
  ("Char" 1)
  ("Text" 1)
  ("Symbol" 1)
  ("Tag" 1)
  ("Open_Paren" 0)
  ("Close_Paren" 0)
  ("Open_Bracket" 0)
  ("Close_Bracket" 0)
  ("Open_Brace" 0)
  ("Close_Brace" 0)
  )

;; [Utils]
(defn ^:private escape-char [escaped]
  "(-> Text (Lux Text))"
  (cond (.equals ^Object escaped "\\t")  (return "\t")
        (.equals ^Object escaped "\\b")  (return "\b")
        (.equals ^Object escaped "\\n")  (return "\n")
        (.equals ^Object escaped "\\r")  (return "\r")
        (.equals ^Object escaped "\\f")  (return "\f")
        (.equals ^Object escaped "\\\"") (return "\"")
        (.equals ^Object escaped "\\\\") (return "\\")
        :else
        (fail (str "[Lexer Error] Unknown escape character: " escaped))))

(defn ^:private escape-char* [escaped]
  "(-> Text Text)"
  (cond (.equals ^Object escaped "\\t")  "\t"
        (.equals ^Object escaped "\\b")  "\b"
        (.equals ^Object escaped "\\n")  "\n"
        (.equals ^Object escaped "\\r")  "\r"
        (.equals ^Object escaped "\\f")  "\f"
        (.equals ^Object escaped "\\\"") "\""
        (.equals ^Object escaped "\\\\") "\\"
        :else
        (assert false (str "[Lexer Error] Unknown escape character: " escaped))))

(defn ^:private clean-line [^String raw-line]
  "(-> Text Text)"
  (let [line-length (.length raw-line)
        buffer (new StringBuffer line-length)]
    (loop [idx 0]
      (if (< idx line-length)
        (let [current-char (.charAt raw-line idx)]
          (if (= \\ current-char)
            (do (assert (< (+ 1 idx) line-length) (str "[Lexer] Text is too short for escaping: " raw-line " " idx))
              (case (.charAt raw-line (+ 1 idx))
                \t (do (.append buffer "\t")
                     (recur (+ 2 idx)))
                \b (do (.append buffer "\b")
                     (recur (+ 2 idx)))
                \n (do (.append buffer "\n")
                     (recur (+ 2 idx)))
                \r (do (.append buffer "\r")
                     (recur (+ 2 idx)))
                \f (do (.append buffer "\f")
                     (recur (+ 2 idx)))
                \" (do (.append buffer "\"")
                     (recur (+ 2 idx)))
                \\ (do (.append buffer "\\")
                     (recur (+ 2 idx)))
                \u (do (assert (< (+ 5 idx) line-length) (str "[Lexer] Text is too short for unicode-escaping: " raw-line " " idx))
                     (.append buffer (char (Integer/valueOf (.substring raw-line (+ 2 idx) (+ 6 idx)) 16)))
                     (recur (+ 6 idx)))
                ;; else
                (assert false (str "[Lexer] Invalid escaping syntax: " raw-line " " idx))))
            (do (.append buffer current-char)
              (recur (+ 1 idx)))))
        (.toString buffer)))))

(defn ^:private lex-text-body [multi-line? offset]
  (|do [[_ eol? ^String pre-quotes**] (&reader/read-regex #"^([^\"]*)")
        ^String pre-quotes* (if multi-line?
                      (|do [:let [empty-line? (and eol? (= "" pre-quotes**))]
                            _ (&/assert! (or empty-line?
                                             (>= (.length pre-quotes**) offset))
                                         "Each line of a multi-line text must have an appropriate offset!")]
                        (return (if empty-line?
                                  "\n"
                                  (str "\n" (.substring pre-quotes** offset)))))
                      (return pre-quotes**))
        [pre-quotes post-quotes] (if (.endsWith pre-quotes* "\\")
                                   (if eol?
                                     (fail "[Lexer Error] Can't leave dangling back-slash \\")
                                     (if (if-let [^String back-slashes (re-find #"\\+$" pre-quotes*)]
                                           (odd? (.length back-slashes)))
                                       (|do [[_ eol?* _] (&reader/read-regex #"^([\"])")
                                             next-part (lex-text-body eol?* offset)]
                                         (return (&/T [(.substring pre-quotes* 0 (dec (.length pre-quotes*)))
                                                       (str "\"" next-part)])))
                                       (|do [post-quotes* (lex-text-body false offset)]
                                         (return (&/T [pre-quotes* post-quotes*])))))
                                   (if eol?
                                     (|do [next-part (lex-text-body true offset)]
                                       (return (&/T [pre-quotes*
                                                     next-part])))
                                     (return (&/T [pre-quotes* ""]))))]
    (return (str (clean-line pre-quotes) post-quotes))))

(def ^:private lex-text
  (|do [[meta _ _] (&reader/read-text "\"")
        :let [[_ _ _column] meta]
        token (lex-text-body false (inc _column))
        _ (&reader/read-text "\"")]
    (return (&/T [meta ($Text token)]))))

(def ^:private +ident-re+
  #"^([^0-9\[\]\{\}\(\)\s\"#;][^\[\]\{\}\(\)\s\"#;]*)")

;; [Lexers]
(def ^:private lex-white-space
  (|do [[meta _ white-space] (&reader/read-regex #"^(\s+|$)")]
    (return (&/T [meta ($White_Space white-space)]))))

(def ^:private lex-single-line-comment
  (|do [_ (&reader/read-text "##")
        [meta _ comment] (&reader/read-regex #"^(.*)$")]
    (return (&/T [meta ($Comment comment)]))))

(defn ^:private lex-multi-line-comment [_]
  (|do [_ (&reader/read-text "#(")
        [meta comment] (&/try-all% (&/|list (|do [[meta comment] (&reader/read-regex+ #"(?is)^(?!#\()((?!\)#).)*")]
                                              (return (&/T [meta comment])))
                                            (|do [[meta pre] (&reader/read-regex+ #"(?is)^((?!#\().)*")
                                                  [_ ($Comment inner)] (lex-multi-line-comment nil)
                                                  [_ post] (&reader/read-regex+ #"(?is)^((?!\)#).)*")]
                                              (return (&/T [meta (str pre "#(" inner ")#" post)])))))
        _ (&reader/read-text ")#")]
    (return (&/T [meta ($Comment comment)]))))

(def ^:private lex-comment
  (&/try-all% (&/|list lex-single-line-comment
                       (lex-multi-line-comment nil))))

(do-template [<name> <tag> <regex>]
  (def <name>
    (|do [[meta _ token] (&reader/read-regex <regex>)]
      (return (&/T [meta (<tag> token)]))))

  ^:private lex-bool  $Bool  #"^(true|false)"
  ^:private lex-int   $Int   #"^-?(0|[1-9][0-9]*)"
  ^:private lex-real  $Real  #"^-?(0\.[0-9]+|[1-9][0-9]*\.[0-9]+)(e-?[1-9][0-9]*)?"
  )

(def ^:private lex-char
  (|do [[meta _ _] (&reader/read-text "#\"")
        token (&/try-all% (&/|list (|do [[_ _ escaped] (&reader/read-regex #"^(\\.)")]
                                     (escape-char escaped))
                                   (|do [[_ _ ^String unicode] (&reader/read-regex #"^(\\u[0-9a-fA-F]{4})")]
                                     (return (str (char (Integer/valueOf (.substring unicode 2) 16)))))
                                   (|do [[_ _ char] (&reader/read-regex #"^(.)")]
                                     (return char))))
        _ (&reader/read-text "\"")]
    (return (&/T [meta ($Char token)]))))

(def ^:private lex-ident
  (&/try-all% (&/|list (|do [[meta _ token] (&reader/read-regex +ident-re+)]
                         (&/try-all% (&/|list (|do [_ (&reader/read-text ";")
                                                    [_ _ local-token] (&reader/read-regex +ident-re+)
                                                    ? (&module/exists? token)]
                                                (if ?
                                                  (return (&/T [meta (&/T [token local-token])]))
                                                  (|do [unaliased (&module/dealias token)]
                                                    (return (&/T [meta (&/T [unaliased local-token])])))))
                                              (return (&/T [meta (&/T ["" token])]))
                                              )))
                       (|do [[meta _ _] (&reader/read-text ";;")
                             [_ _ token] (&reader/read-regex +ident-re+)
                             module-name &/get-module-name]
                         (return (&/T [meta (&/T [module-name token])])))
                       (|do [[meta _ _] (&reader/read-text ";")
                             [_ _ token] (&reader/read-regex +ident-re+)]
                         (return (&/T [meta (&/T ["lux" token])])))
                       )))

(def ^:private lex-symbol
  (|do [[meta ident] lex-ident]
    (return (&/T [meta ($Symbol ident)]))))

(def ^:private lex-tag
  (|do [[meta _ _] (&reader/read-text "#")
        [_ ident] lex-ident]
    (return (&/T [meta ($Tag ident)]))))

(do-template [<name> <text> <tag>]
  (def <name>
    (|do [[meta _ _] (&reader/read-text <text>)]
      (return (&/T [meta <tag>]))))

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
