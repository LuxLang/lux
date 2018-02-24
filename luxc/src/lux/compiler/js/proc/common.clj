(ns lux.compiler.js.proc.common
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |let |case]]
                 [type :as &type]
                 [analyser :as &analyser]
                 [optimizer :as &o])
            [lux.analyser.base :as &a]
            (lux.compiler.js [base :as &&]
                             [rt :as &&rt]
                             [lux :as &&lux])))

;; [Resources]
(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?input (&/$Cons ?param (&/$Nil))) ?values]
          =input (compile ?input)
          =param (compile ?param)]
      (return (str "LuxRT$" <op> "(" =input "," =param ")"))))

  ^:private compile-bit-and "andI64"
  ^:private compile-bit-or  "orI64"
  ^:private compile-bit-xor "xorI64"
  )

(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?input (&/$Cons ?param (&/$Nil))) ?values]
          =input (compile ?input)
          =param (compile ?param)]
      (return (str "LuxRT$" <op> "(" =input "," =param ".L)"))))

  ^:private compile-bit-shift-left           "shlI64"
  ^:private compile-bit-shift-right          "shrI64"
  ^:private compile-bit-unsigned-shift-right "ushrI64"
  )

(defn ^:private compile-bit-count [compile ?values special-args]
  (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
        =input (compile ?input)]
    (return (str "LuxRT$countI64(" =input ")"))))

(defn ^:private compile-lux-is [compile ?values special-args]
  (|do [:let [(&/$Cons ?left (&/$Cons ?right (&/$Nil))) ?values]
        =left (compile ?left)
        =right (compile ?right)]
    (return (str "(" =left " === " =right ")"))))

(defn ^:private compile-lux-try [compile ?values special-args]
  (|do [:let [(&/$Cons ?op (&/$Nil)) ?values]
        =op (compile ?op)]
    (return (str "LuxRT$runTry(" =op ")"))))

(defn ^:private compile-array-new [compile ?values special-args]
  (|do [:let [(&/$Cons ?length (&/$Nil)) ?values]
        =length (compile ?length)]
    (return (str "new Array(" (str "LuxRT$toNumberI64(" =length ")") ")"))))

(defn ^:private compile-array-get [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Nil))) ?values
              ;; (&/$Nil) special-args
              ]
        =array (compile ?array)
        =idx (compile ?idx)]
    (return (str "LuxRT$arrayGet(" =array "," =idx ")"))))

(defn ^:private compile-array-put [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Cons ?elem (&/$Nil)))) ?values
              ;; (&/$Nil) special-args
              ]
        =array (compile ?array)
        =idx (compile ?idx)
        =elem (compile ?elem)]
    (return (str "LuxRT$arrayPut(" =array "," =idx "," =elem ")"))))

(defn ^:private compile-array-remove [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Cons ?idx (&/$Nil))) ?values
              ;; (&/$Nil) special-args
              ]
        =array (compile ?array)
        =idx (compile ?idx)]
    (return (str "LuxRT$arrayRemove(" =array "," =idx ")"))))

(defn ^:private compile-array-size [compile ?values special-args]
  (|do [:let [(&/$Cons ?array (&/$Nil)) ?values
              ;; (&/$Nil) special-args
              ]
        =array (compile ?array)]
    (return (str "LuxRT$fromNumberI64(" =array ".length" ")"))))

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          =x (compile ?x)
          =y (compile ?y)]
      (return (str "LuxRT$" <method> "(" =x "," =y ")"))))

  ^:private compile-nat-add   "addI64"
  ^:private compile-nat-sub   "subI64"
  ^:private compile-nat-mul   "mulI64"
  ^:private compile-nat-div   "divN64"
  ^:private compile-nat-rem   "remN64"
  ^:private compile-nat-eq    "eqI64"
  ^:private compile-nat-lt    "ltN64"

  ^:private compile-int-add   "addI64"
  ^:private compile-int-sub   "subI64"
  ^:private compile-int-mul   "mulI64"
  ^:private compile-int-div   "divI64"
  ^:private compile-int-rem   "remI64"
  ^:private compile-int-eq    "eqI64"
  ^:private compile-int-lt    "ltI64"

  ^:private compile-deg-add   "addI64"
  ^:private compile-deg-sub   "subI64"
  ^:private compile-deg-mul   "mulD64"
  ^:private compile-deg-div   "divD64"
  ^:private compile-deg-rem   "subI64"
  ^:private compile-deg-eq    "eqI64"
  ^:private compile-deg-lt    "ltD64"
  ^:private compile-deg-scale "mulI64"
  ^:private compile-deg-reciprocal "divI64"
  )

(do-template [<name> <opcode>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          =x (compile ?x)
          =y (compile ?y)]
      (return (str "(" =x " " <opcode> " " =y ")"))))

  ^:private compile-frac-add   "+"
  ^:private compile-frac-sub   "-"
  ^:private compile-frac-mul   "*"
  ^:private compile-frac-div   "/"
  ^:private compile-frac-rem   "%"
  ^:private compile-frac-eq    "==="
  ^:private compile-frac-lt    "<"
  )

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
          =x (compile ?x)]
      (return (str "LuxRT$" <method> "(" =x ")"))
      ))

  ^:private compile-frac-decode "decodeFrac"
  )

(do-template [<name> <compiler> <value>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Nil) ?values]]
      (<compiler> <value>)))

  ^:private compile-nat-min &&lux/compile-nat  0
  ^:private compile-nat-max &&lux/compile-nat -1

  ^:private compile-int-min &&lux/compile-int Long/MIN_VALUE
  ^:private compile-int-max &&lux/compile-int Long/MAX_VALUE
  
  ^:private compile-deg-min &&lux/compile-deg  0
  ^:private compile-deg-max &&lux/compile-deg -1

  ^:private compile-frac-smallest &&lux/compile-frac Double/MIN_VALUE
  ^:private compile-frac-min &&lux/compile-frac (* -1.0 Double/MAX_VALUE)
  ^:private compile-frac-max &&lux/compile-frac Double/MAX_VALUE

  ^:private compile-frac-not-a-number      &&lux/compile-frac "NaN" 
  ^:private compile-frac-positive-infinity &&lux/compile-frac "Infinity"
  ^:private compile-frac-negative-infinity &&lux/compile-frac "-Infinity"
  )

(defn ^:private compile-frac-encode [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "(" =x ")" ".toString()"))))

(do-template [<name>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]]
      (compile ?x)))

  ^:private compile-nat-to-int
  ^:private compile-int-to-nat
  )

(defn ^:private compile-int-to-frac [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "LuxRT$toNumberI64(" =x ")"))))

(defn ^:private compile-frac-to-int [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "LuxRT$fromNumberI64(" =x ")"))))

(defn ^:private compile-deg-to-frac [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "LuxRT$degToFrac(" =x ")"))))

(defn ^:private compile-frac-to-deg [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "LuxRT$fracToDeg(" =x ")"))))

(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          =x (compile ?x)
          =y (compile ?y)]
      (return (str "(" =x <op> =y ")"))))

  ^:private compile-text-eq "==="
  ^:private compile-text-lt "<"
  )

(do-template [<name> <op>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          =x (compile ?x)
          =y (compile ?y)]
      (return (str "(" =x ".C" " " <op> " " =y ".C" ")"))))

  ^:private compile-char-eq "==="
  ^:private compile-char-lt "<"
  )

(defn ^:private compile-text-concat [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
        =x (compile ?x)
        =y (compile ?y)]
    (return (str =x ".concat(" =y ")"))))

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?text (&/$Cons ?part (&/$Cons ?start (&/$Nil)))) ?values]
          =text (compile ?text)
          =part (compile ?part)
          =start (compile ?start)]
      (return (str "LuxRT$" <method> "(" =text "," =part "," =start ")"))))

  ^:private compile-text-last-index "lastIndex"
  ^:private compile-text-index      "index"
  )

(defn ^:private compile-text-contains? [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?part (&/$Nil))) ?values]
        =text (compile ?text)
        =part (compile ?part)]
    (return (str "(" (str (str "(" =text ")")
                          ".indexOf"
                          (str "(" =part ")"))
                 " !== " "-1"
                 ")"))))

(defn ^:private compile-text-clip [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?from (&/$Cons ?to (&/$Nil)))) ?values]
        =text (compile ?text)
        =from (compile ?from)
        =to (compile ?to)]
    (return (str "LuxRT$clip(" (str =text "," =from "," =to) ")"))))

(defn ^:private compile-text-replace-all [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?to-find (&/$Cons ?replace-with (&/$Nil)))) ?values]
        =text (compile ?text)
        =to-find (compile ?to-find)
        =replace-with (compile ?replace-with)]
    (return (str "LuxRT$replaceAll(" (str =text "," =to-find "," =replace-with) ")"))))

(defn ^:private compile-text-size [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Nil)) ?values]
        =text (compile ?text)]
    (return (str "LuxRT$fromNumberI64(" =text ".length" ")"))))

(defn ^:private compile-text-hash [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Nil)) ?values]
        =text (compile ?text)]
    (return (str "LuxRT$textHash(" =text ")"))))

(defn ^:private compile-text-char [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?idx (&/$Nil))) ?values]
        =text (compile ?text)
        =idx (compile ?idx)]
    (return (str "LuxRT$textChar(" (str =text "," =idx) ")"))))

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?text (&/$Nil)) ?values]
          =text (compile ?text)]
      (return (str "(" =text ")." <method> "()"))))

  ^:private compile-text-trim       "trim"
  ^:private compile-text-upper "toUpperCase"
  ^:private compile-text-lower "toLowerCase"
  )

(defn ^:private compile-char-to-text [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "(" =x ").C"))))

(defn ^:private compile-char-to-nat [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "LuxRT$fromNumberI64(" (str "(" =x ").C" ".charCodeAt(0)") ")"))))

(defn ^:private compile-nat-to-char [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "{C:"
                 (str "String.fromCharCode("
                      (str "LuxRT$toNumberI64(" =x ")")
                      ")")
                 "}"))))

(defn ^:private compile-io-log [compile ?values special-args]
  (|do [:let [(&/$Cons ?message (&/$Nil)) ?values]
        =message (compile ?message)]
    (return (str "LuxRT$log(" =message ")"))))

(defn ^:private compile-io-error [compile ?values special-args]
  (|do [:let [(&/$Cons ?message (&/$Nil)) ?values]
        =message (compile ?message)]
    (return (str "LuxRT$error(" =message ")"))))

(defn ^:private compile-io-exit [compile ?values special-args]
  (|do [:let [(&/$Cons ?code (&/$Nil)) ?values]
        =code (compile ?code)]
    (return (str "(process && process.exit && process.exit(LuxRT$toNumberI64(" =code ")))"))))

(defn ^:private compile-io-current-time [compile ?values special-args]
  (|do [:let [(&/$Nil) ?values]]
    (return (str "LuxRT$fromNumberI64(" "(new Date()).getTime()" ")"))))

(defn ^:private compile-atom-new [compile ?values special-args]
  (|do [:let [(&/$Cons ?init (&/$Nil)) ?values]
        =init (compile ?init)]
    (return (str "{V: " =init "}"))))

(defn ^:private compile-atom-get [compile ?values special-args]
  (|do [:let [(&/$Cons ?atom (&/$Nil)) ?values]
        =atom (compile ?atom)]
    (return (str =atom ".V"))))

(defn ^:private compile-atom-compare-and-swap [compile ?values special-args]
  (|do [:let [(&/$Cons ?atom (&/$Cons ?old (&/$Cons ?new (&/$Nil)))) ?values]
        =atom (compile ?atom)
        =old (compile ?old)
        =new (compile ?new)]
    (return (str "(function() {"
                 (str "var atom = " =atom ";")
                 (str "if(" (str "(atom.V === " =old ")") ") {"
                      (str "atom.V = " =new ";")
                      "return true;"
                      "}"
                      "else {"
                      "return false;"
                      "}")
                 "})()"))))

(defn ^:private compile-process-concurrency-level [compile ?values special-args]
  (|do [:let [(&/$Nil) ?values]]
    (return (str "LuxRT$ONE"))))

(defn ^:private compile-process-future [compile ?values special-args]
  (|do [:let [(&/$Cons ?procedure (&/$Nil)) ?values]
        =procedure (compile ?procedure)]
    (return (str "setTimeout("
                 (str "function() {" =procedure "(null)" "}")
                 ",0)"))))

(defn ^:private compile-process-schedule [compile ?values special-args]
  (|do [:let [(&/$Cons ?milliseconds (&/$Cons ?procedure (&/$Nil))) ?values]
        =milliseconds (compile ?milliseconds)
        =procedure (compile ?procedure)]
    (return (str "setTimeout("
                 (str "function() {" =procedure "(null)" "}")
                 ","
                 (str "LuxRT$toNumberI64(" =milliseconds ")")
                 ")"))))

(do-template [<name> <field>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Nil) ?values]]
      (return (str "Math." <field>))))

  ^:private compile-math-e  "E"
  ^:private compile-math-pi "PI"
  )

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
          =input (compile ?input)]
      (return (str "Math." <method> "(" =input ")"))))

  ^:private compile-math-cos "cos"
  ^:private compile-math-sin "sin"
  ^:private compile-math-tan "tan"
  ^:private compile-math-acos "acos"
  ^:private compile-math-asin "asin"
  ^:private compile-math-atan "atan"
  ^:private compile-math-cosh "cosh"
  ^:private compile-math-sinh "sinh"
  ^:private compile-math-tanh "tanh"
  ^:private compile-math-exp "exp"
  ^:private compile-math-log "log"
  ^:private compile-math-ceil "ceil"
  ^:private compile-math-floor "floor"
  ^:private compile-math-round "round"
  )

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?input (&/$Cons ?param (&/$Nil))) ?values]
          =input (compile ?input)
          =param (compile ?param)]
      (return (str "Math." <method> "(" =input "," =param ")"))))

  ^:private compile-math-atan2 "atan2"
  ^:private compile-math-pow "pow"
  )

(defn compile-proc [compile category proc ?values special-args]
  (case category
    "lux"
    (case proc
      "is"                   (compile-lux-is compile ?values special-args)
      "try"                  (compile-lux-try compile ?values special-args))

    "io"
    (case proc
      "log"                  (compile-io-log compile ?values special-args)
      "error"                (compile-io-error compile ?values special-args)
      "exit"                 (compile-io-exit compile ?values special-args)
      "current-time"         (compile-io-current-time compile ?values special-args))

    "text"
    (case proc
      "="                    (compile-text-eq compile ?values special-args)
      "<"                    (compile-text-lt compile ?values special-args)
      "concat"               (compile-text-concat compile ?values special-args)
      "clip"                 (compile-text-clip compile ?values special-args)
      "index"                (compile-text-index compile ?values special-args)
      "last-index"           (compile-text-last-index compile ?values special-args)
      "size"                 (compile-text-size compile ?values special-args)
      "hash"                 (compile-text-hash compile ?values special-args)
      "replace-all"          (compile-text-replace-all compile ?values special-args)
      "trim"                 (compile-text-trim compile ?values special-args)
      "char"                 (compile-text-char compile ?values special-args)
      "upper"           (compile-text-upper compile ?values special-args)
      "lower"           (compile-text-lower compile ?values special-args)
      "contains?"            (compile-text-contains? compile ?values special-args)
      )
    
    "bit"
    (case proc
      "count"                (compile-bit-count compile ?values special-args)
      "and"                  (compile-bit-and compile ?values special-args)
      "or"                   (compile-bit-or compile ?values special-args)
      "xor"                  (compile-bit-xor compile ?values special-args)
      "shift-left"           (compile-bit-shift-left compile ?values special-args)
      "shift-right"          (compile-bit-shift-right compile ?values special-args)
      "unsigned-shift-right" (compile-bit-unsigned-shift-right compile ?values special-args))
    
    "array"
    (case proc
      "new" (compile-array-new compile ?values special-args)
      "get" (compile-array-get compile ?values special-args)
      "put" (compile-array-put compile ?values special-args)
      "remove" (compile-array-remove compile ?values special-args)
      "size" (compile-array-size compile ?values special-args))

    "nat"
    (case proc
      "+"         (compile-nat-add compile ?values special-args)
      "-"         (compile-nat-sub compile ?values special-args)
      "*"         (compile-nat-mul compile ?values special-args)
      "/"         (compile-nat-div compile ?values special-args)
      "%"         (compile-nat-rem compile ?values special-args)
      "="         (compile-nat-eq compile ?values special-args)
      "<"         (compile-nat-lt compile ?values special-args)
      "max" (compile-nat-max compile ?values special-args)
      "min" (compile-nat-min compile ?values special-args)
      "to-int"    (compile-nat-to-int compile ?values special-args)
      "to-char"   (compile-nat-to-char compile ?values special-args)
      )

    "int"
    (case proc
      "+"         (compile-int-add compile ?values special-args)
      "-"         (compile-int-sub compile ?values special-args)
      "*"         (compile-int-mul compile ?values special-args)
      "/"         (compile-int-div compile ?values special-args)
      "%"         (compile-int-rem compile ?values special-args)
      "="         (compile-int-eq compile ?values special-args)
      "<"         (compile-int-lt compile ?values special-args)
      "max" (compile-int-max compile ?values special-args)
      "min" (compile-int-min compile ?values special-args)
      "to-nat"    (compile-int-to-nat compile ?values special-args)
      "to-frac"   (compile-int-to-frac compile ?values special-args)
      )
    
    "deg"
    (case proc
      "+"         (compile-deg-add compile ?values special-args)
      "-"         (compile-deg-sub compile ?values special-args)
      "*"         (compile-deg-mul compile ?values special-args)
      "/"         (compile-deg-div compile ?values special-args)
      "%"         (compile-deg-rem compile ?values special-args)
      "="         (compile-deg-eq compile ?values special-args)
      "<"         (compile-deg-lt compile ?values special-args)
      "max" (compile-deg-max compile ?values special-args)
      "min" (compile-deg-min compile ?values special-args)
      "to-frac"   (compile-deg-to-frac compile ?values special-args)
      "scale"     (compile-deg-scale compile ?values special-args)
      "reciprocal" (compile-deg-reciprocal compile ?values special-args)
      )

    "frac"
    (case proc
      "+"         (compile-frac-add compile ?values special-args)
      "-"         (compile-frac-sub compile ?values special-args)
      "*"         (compile-frac-mul compile ?values special-args)
      "/"         (compile-frac-div compile ?values special-args)
      "%"         (compile-frac-rem compile ?values special-args)
      "="         (compile-frac-eq compile ?values special-args)
      "<"         (compile-frac-lt compile ?values special-args)
      "encode"    (compile-frac-encode compile ?values special-args)
      "decode"    (compile-frac-decode compile ?values special-args)
      "smallest" (compile-frac-smallest compile ?values special-args)
      "max" (compile-frac-max compile ?values special-args)
      "min" (compile-frac-min compile ?values special-args)
      "not-a-number" (compile-frac-not-a-number compile ?values special-args)
      "positive-infinity" (compile-frac-positive-infinity compile ?values special-args)
      "negative-infinity" (compile-frac-negative-infinity compile ?values special-args)
      "to-deg"    (compile-frac-to-deg compile ?values special-args)
      "to-int"    (compile-frac-to-int compile ?values special-args)
      )

    "char"
    (case proc
      "="         (compile-char-eq compile ?values special-args)
      "<"         (compile-char-lt compile ?values special-args)
      "to-text"   (compile-char-to-text compile ?values special-args)
      "to-nat"    (compile-char-to-nat compile ?values special-args)
      )

    "math"
    (case proc
      "e" (compile-math-e compile ?values special-args)
      "pi" (compile-math-pi compile ?values special-args)
      "cos" (compile-math-cos compile ?values special-args)
      "sin" (compile-math-sin compile ?values special-args)
      "tan" (compile-math-tan compile ?values special-args)
      "acos" (compile-math-acos compile ?values special-args)
      "asin" (compile-math-asin compile ?values special-args)
      "atan" (compile-math-atan compile ?values special-args)
      "cosh" (compile-math-cosh compile ?values special-args)
      "sinh" (compile-math-sinh compile ?values special-args)
      "tanh" (compile-math-tanh compile ?values special-args)
      "exp" (compile-math-exp compile ?values special-args)
      "log" (compile-math-log compile ?values special-args)
      "ceil" (compile-math-ceil compile ?values special-args)
      "floor" (compile-math-floor compile ?values special-args)
      "round" (compile-math-round compile ?values special-args)
      "atan2" (compile-math-atan2 compile ?values special-args)
      "pow" (compile-math-pow compile ?values special-args)
      )

    "atom"
    (case proc
      "new" (compile-atom-new compile ?values special-args)
      "get" (compile-atom-get compile ?values special-args)
      "compare-and-swap" (compile-atom-compare-and-swap compile ?values special-args)
      )

    "process"
    (case proc
      "concurrency-level" (compile-process-concurrency-level compile ?values special-args)
      "future" (compile-process-future compile ?values special-args)
      "schedule" (compile-process-schedule compile ?values special-args)
      )
    
    ;; else
    (&/fail-with-loc (str "[Compiler Error] Unknown procedure: " [category proc]))))
