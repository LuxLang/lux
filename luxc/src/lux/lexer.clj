(ns lux.lexer
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            (lux [base :as & :refer [defvariant |do return* return |case]]
                 [reader :as &reader])
            [lux.analyser.module :as &module]))

;; [Tags]
(defvariant
  ("White_Space" 1)
  ("Comment" 1)
  ("Bit" 1)
  ("Nat" 1)
  ("Int" 1)
  ("Rev" 1)
  ("Frac" 1)
  ("Text" 1)
  ("Identifier" 1)
  ("Tag" 1)
  ("Open_Paren" 0)
  ("Close_Paren" 0)
  ("Open_Bracket" 0)
  ("Close_Bracket" 0)
  ("Open_Brace" 0)
  ("Close_Brace" 0)
  )

;; [Utils]
(defn- clean-line
  "(-> Text Text)"
  [^String raw-line]
  (let [line-length (.length raw-line)
        buffer (new StringBuffer line-length)]
    (loop [idx 0]
      (if (< idx line-length)
        (let [current-char (.charAt raw-line idx)]
          (if (= \\ current-char)
            (do (assert (< (+ 1 idx) line-length) (str "[Lexer Error] Text is too short for escaping: " raw-line " " idx))
              (case (.charAt raw-line (+ 1 idx))
                \v (do (.append buffer "\u000B")
                     (recur (+ 2 idx)))
                \n (do (.append buffer "\n")
                     (recur (+ 2 idx)))
                \r (do (.append buffer "\r")
                     (recur (+ 2 idx)))
                \" (do (.append buffer "\"")
                     (recur (+ 2 idx)))
                \\ (do (.append buffer "\\")
                     (recur (+ 2 idx)))
                \u (do (assert (< (+ 5 idx) line-length) (str "[Lexer Error] Text is too short for unicode-escaping: " raw-line " " idx))
                     (.append buffer (char (Integer/valueOf (.substring raw-line (+ 2 idx) (+ 6 idx)) 16)))
                     (recur (+ 6 idx)))
                ;; else
                (assert false (str "[Lexer Error] Invalid escaping syntax: " raw-line " " idx))))
            (do (.append buffer current-char)
              (recur (+ 1 idx)))))
        (.toString buffer)))))

(defn- lex-text-body [_]
  (|do [[_ _ ^String pre-quotes*] (&reader/read-regex #"^([^\"]*)")
        [pre-quotes post-quotes] (if (.endsWith pre-quotes* "\\")
                                   (if (if-let [^String back-slashes (re-find #"\\+$" pre-quotes*)]
                                         (odd? (.length back-slashes)))
                                     (|do [[_ _ _] (&reader/read-regex #"^([\"])")
                                           next-part (lex-text-body nil)]
                                       (return (&/T [(.substring pre-quotes* 0 (dec (.length pre-quotes*)))
                                                     (str "\"" next-part)])))
                                     (|do [post-quotes* (lex-text-body nil)]
                                       (return (&/T [pre-quotes* post-quotes*]))))
                                   (return (&/T [pre-quotes* ""])))]
    (return (str (clean-line pre-quotes) post-quotes))))

(def lex-text
  (|do [[meta _ _] (&reader/read-text "\"")
        :let [[_ _ _column] meta]
        token (lex-text-body nil)
        _ (&reader/read-text "\"")]
    (return (&/T [meta ($Text token)]))))

(def +ident-re+
  #"^([^0-9\[\]\{\}\(\)\s\"#.][^\[\]\{\}\(\)\s\"#.]*)")

;; [Lexers]
(def ^:private lex-white-space
  (|do [[meta _ white-space] (&reader/read-regex #"^(\s+|$)")]
    (return (&/T [meta ($White_Space white-space)]))))

(def ^:private lex-comment
  (|do [_ (&reader/read-text "##")
        [meta _ comment] (&reader/read-regex #"^(.*)$")]
    (return (&/T [meta ($Comment comment)]))))

(do-template [<name> <tag> <regex>]
  (def <name>
    (|do [[meta _ token] (&reader/read-regex <regex>)]
      (return (&/T [meta (<tag> token)]))))

  lex-bit $Bit #"^#(0|1)"
  )

(do-template [<name> <tag> <regex>]
  (def <name>
    (|do [[meta _ token] (&reader/read-regex <regex>)]
      (return (&/T [meta (<tag> (string/replace token #"_" ""))]))))

  lex-nat  $Nat  #"^[0-9][0-9_]*"
  lex-int  $Int  #"^(-|\+)[0-9][0-9_]*"
  lex-rev  $Rev  #"^\.[0-9][0-9_]*"
  lex-frac $Frac #"^(-|\+)[0-9][0-9_]*\.[0-9][0-9_]*((e|E)(-|\+)[0-9][0-9_]*)?"
  )

(def +same-module-mark+ (str &/+name-separator+ &/+name-separator+))

(def ^:private lex-ident
  (&/try-all-% "[Reader Error]"
               (&/|list (|do [[meta _ token] (&reader/read-regex +ident-re+)
                              [_ _ got-it?] (&reader/read-text? &/+name-separator+)]
                          (|case got-it?
                            (&/$Some _)
                            (|do [[_ _ local-token] (&reader/read-regex +ident-re+)
                                  ? (&module/exists? token)]
                              (if ?
                                (return (&/T [meta (&/T [token local-token])]))
                                (|do [unaliased (&module/dealias token)]
                                  (return (&/T [meta (&/T [unaliased local-token])])))))

                            (&/$None)
                            (return (&/T [meta (&/T ["" token])]))))
                        (|do [[meta _ _] (&reader/read-text +same-module-mark+)
                              [_ _ token] (&reader/read-regex +ident-re+)
                              module-name &/get-module-name]
                          (return (&/T [meta (&/T [module-name token])])))
                        (|do [[meta _ _] (&reader/read-text &/+name-separator+)
                              [_ _ token] (&reader/read-regex +ident-re+)]
                          (return (&/T [meta (&/T ["lux" token])])))
                        )))

(def ^:private lex-identifier
  (|do [[meta ident] lex-ident]
    (return (&/T [meta ($Identifier ident)]))))

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
  (&/try-all-% "[Reader Error]"
               (&/|list lex-white-space
                        lex-comment
                        lex-bit
                        lex-nat
                        lex-frac
                        lex-rev
                        lex-int
                        lex-text
                        lex-identifier
                        lex-tag
                        lex-delimiter)))
