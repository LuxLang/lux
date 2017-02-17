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
;; (do-template [<name> <op>]
;;   (defn <name> [compile ?values special-args]
;;     (|do [:let [(&/$Cons ?input (&/$Cons ?mask (&/$Nil))) ?values]
;;           ^MethodVisitor *writer* &/get-writer
;;           _ (compile ?input)
;;           :let [_ (&&/unwrap-long *writer*)]
;;           _ (compile ?mask)
;;           :let [_ (&&/unwrap-long *writer*)]
;;           :let [_ (doto *writer*
;;                     (.visitInsn <op>)
;;                     &&/wrap-long)]]
;;       (return nil)))

;;   ^:private compile-bit-and Opcodes/LAND
;;   ^:private compile-bit-or  Opcodes/LOR
;;   ^:private compile-bit-xor Opcodes/LXOR
;;   )

;; (defn ^:private compile-bit-count [compile ?values special-args]
;;   (|do [:let [(&/$Cons ?input (&/$Nil)) ?values]
;;         ^MethodVisitor *writer* &/get-writer
;;         _ (compile ?input)
;;         :let [_ (&&/unwrap-long *writer*)]
;;         :let [_ (doto *writer*
;;                   (.visitMethodInsn Opcodes/INVOKESTATIC "java/lang/Long" "bitCount" "(J)I")
;;                   (.visitInsn Opcodes/I2L)
;;                   &&/wrap-long)]]
;;     (return nil)))

;; (do-template [<name> <op>]
;;   (defn <name> [compile ?values special-args]
;;     (|do [:let [(&/$Cons ?input (&/$Cons ?shift (&/$Nil))) ?values]
;;           ^MethodVisitor *writer* &/get-writer
;;           _ (compile ?input)
;;           :let [_ (&&/unwrap-long *writer*)]
;;           _ (compile ?shift)
;;           :let [_ (doto *writer*
;;                     &&/unwrap-long
;;                     (.visitInsn Opcodes/L2I))]
;;           :let [_ (doto *writer*
;;                     (.visitInsn <op>)
;;                     &&/wrap-long)]]
;;       (return nil)))

;;   ^:private compile-bit-shift-left           Opcodes/LSHL
;;   ^:private compile-bit-shift-right          Opcodes/LSHR
;;   ^:private compile-bit-unsigned-shift-right Opcodes/LUSHR
;;   )

(defn ^:private compile-lux-is [compile ?values special-args]
  (|do [:let [(&/$Cons ?left (&/$Cons ?right (&/$Nil))) ?values]
        =left (compile ?left)
        =right (compile ?right)]
    (return (str "(" =left " === " =right ")"))))

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          =x (compile ?x)
          =y (compile ?y)]
      (return (str &&rt/LuxRT "." <method> "(" =x "," =y ")"))))

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
  )

(do-template [<name> <opcode>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
          =x (compile ?x)
          =y (compile ?y)]
      (return (str "(" =x " " <opcode> " " =y ")"))))

  ^:private compile-real-add   "+"
  ^:private compile-real-sub   "-"
  ^:private compile-real-mul   "*"
  ^:private compile-real-div   "/"
  ^:private compile-real-rem   "%"
  ^:private compile-real-eq    "==="
  ^:private compile-real-lt    "<"
  )

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
          =x (compile ?x)]
      (return (str &&rt/LuxRT "." <method> "(" =x ")"))
      ))

  ^:private compile-int-encode "encodeI64"
  ^:private compile-nat-encode "encodeN64"
  ^:private compile-deg-encode "encodeD64"

  ^:private compile-int-decode "decodeI64"
  ^:private compile-nat-decode "decodeN64"
  ^:private compile-deg-decode "decodeD64"

  ^:private compile-real-decode "decodeReal"

  ^:private compile-real-hash "hashReal"
  )

(do-template [<name> <compiler> <value>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Nil) ?values]]
      (<compiler> <value>)))

  ^:private compile-nat-min-value &&lux/compile-nat  0
  ^:private compile-nat-max-value &&lux/compile-nat -1

  ^:private compile-int-min-value &&lux/compile-int Long/MIN_VALUE
  ^:private compile-int-max-value &&lux/compile-int Long/MAX_VALUE
  
  ^:private compile-deg-min-value &&lux/compile-deg  0
  ^:private compile-deg-max-value &&lux/compile-deg -1

  ^:private compile-real-min-value &&lux/compile-real (* -1.0 Double/MAX_VALUE)
  ^:private compile-real-max-value &&lux/compile-real Double/MAX_VALUE

  ^:private compile-real-not-a-number      &&lux/compile-real "NaN" 
  ^:private compile-real-positive-infinity &&lux/compile-real "Infinity"
  ^:private compile-real-negative-infinity &&lux/compile-real "-Infinity"
  )

(defn ^:private compile-real-encode [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "(" =x ")" ".toString()"))))

;; (defn ^:private compile-nat-lt [compile ?values special-args]
;;   (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
;;         ^MethodVisitor *writer* &/get-writer
;;         _ (compile ?x)
;;         :let [_ (doto *writer*
;;                   &&/unwrap-long)]
;;         _ (compile ?y)
;;         :let [_ (doto *writer*
;;                   &&/unwrap-long)
;;               $then (new Label)
;;               $end (new Label)
;;               _ (doto *writer*
;;                   (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "_compareUnsigned" "(JJ)I")
;;                   (.visitLdcInsn (int -1))
;;                   (.visitJumpInsn Opcodes/IF_ICMPEQ $then)
;;                   (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "FALSE"  (&host-generics/->type-signature "java.lang.Boolean"))
;;                   (.visitJumpInsn Opcodes/GOTO $end)
;;                   (.visitLabel $then)
;;                   (.visitFieldInsn Opcodes/GETSTATIC (&host-generics/->bytecode-class-name "java.lang.Boolean") "TRUE" (&host-generics/->type-signature "java.lang.Boolean"))
;;                   (.visitLabel $end))]]
;;     (return nil)))

;; (do-template [<name> <method>]
;;   (defn <name> [compile ?values special-args]
;;     (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
;;           ^MethodVisitor *writer* &/get-writer
;;           _ (compile ?x)
;;           :let [_ (doto *writer*
;;                     &&/unwrap-long)]
;;           _ (compile ?y)
;;           :let [_ (doto *writer*
;;                     &&/unwrap-long)]
;;           :let [_ (doto *writer*
;;                     (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <method> "(JJ)J")
;;                     &&/wrap-long)]]
;;       (return nil)))

;;   ^:private compile-deg-mul   "mul_deg"
;;   ^:private compile-deg-div   "div_deg"
;;   )

;; (do-template [<name> <class> <method> <sig> <unwrap> <wrap>]
;;   (let [+wrapper-class+ (&host-generics/->bytecode-class-name <class>)]
;;     (defn <name> [compile ?values special-args]
;;       (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
;;             ^MethodVisitor *writer* &/get-writer
;;             _ (compile ?x)
;;             :let [_ (doto *writer*
;;                       <unwrap>
;;                       (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" <method> <sig>)
;;                       <wrap>)]]
;;         (return nil))))

;;   ^:private compile-deg-to-real "java.lang.Long"   "deg-to-real" "(J)D" &&/unwrap-long   &&/wrap-double
;;   ^:private compile-real-to-deg "java.lang.Double" "real-to-deg" "(D)J" &&/unwrap-double &&/wrap-long
;;   )

;; (let [widen (fn [^MethodVisitor *writer*]
;;               (doto *writer*
;;                 (.visitInsn Opcodes/I2L)))
;;       shrink (fn [^MethodVisitor *writer*]
;;                (doto *writer*
;;                  (.visitInsn Opcodes/L2I)
;;                  (.visitInsn Opcodes/I2C)))]
;;   (do-template [<name> <unwrap> <wrap> <adjust>]
;;     (defn <name> [compile ?values special-args]
;;       (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
;;             ^MethodVisitor *writer* &/get-writer
;;             _ (compile ?x)
;;             :let [_ (doto *writer*
;;                       <unwrap>
;;                       <adjust>
;;                       <wrap>)]]
;;         (return nil)))

;;     ^:private compile-nat-to-char &&/unwrap-long &&/wrap-char shrink
;;     ^:private compile-char-to-nat &&/unwrap-char &&/wrap-long widen
;;     ))

(do-template [<name>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]]
      (compile ?x)))

  ^:private compile-nat-to-int
  ^:private compile-int-to-nat
  )

(defn ^:private compile-int-to-real [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "LuxRT.toNumberI64(" =x ")"))))

(defn ^:private compile-real-to-int [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "LuxRT.fromNumberI64(" =x ")"))))

(defn ^:private compile-deg-to-real [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "LuxRT.degToReal(" =x ")"))))

(defn ^:private compile-real-to-deg [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "LuxRT.realToDeg(" =x ")"))))

(defn ^:private compile-text-eq [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
        =x (compile ?x)
        =y (compile ?y)]
    (return (str "(" =x "===" =y ")"))))

(defn ^:private compile-text-append [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Cons ?y (&/$Nil))) ?values]
        =x (compile ?x)
        =y (compile ?y)]
    (return (str =x ".concat(" =y ")"))))

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?text (&/$Cons ?part (&/$Nil))) ?values]
          =text (compile ?text)
          =part (compile ?part)]
      (return (str "LuxRT" "." <method> "(" =text "," =part ")"))))

  ^:private compile-text-last-index "lastIndex"
  ^:private compile-text-index      "index"
  )

(defn ^:private compile-text-clip [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?from (&/$Cons ?to (&/$Nil)))) ?values]
        =text (compile ?text)
        =from (compile ?from)
        =to (compile ?to)]
    (return (str "LuxRT.clip(" (str =text "," =from "," =to) ")"))))

(defn ^:private compile-text-replace-all [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?to-find (&/$Cons ?replace-with (&/$Nil)))) ?values]
        =text (compile ?text)
        =to-find (compile ?to-find)
        =replace-with (compile ?replace-with)]
    (return (str "LuxRT.replaceAll(" (str =text "," =to-find "," =replace-with) ")"))))

(defn ^:private compile-text-size [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Nil)) ?values]
        =text (compile ?text)]
    (return (str "LuxRT.fromNumberI64(" =text ".length" ")"))))

(defn ^:private compile-text-char [compile ?values special-args]
  (|do [:let [(&/$Cons ?text (&/$Cons ?idx (&/$Nil))) ?values]
        =text (compile ?text)
        =idx (compile ?idx)]
    (return (str "LuxRT.textChar(" (str =text "," =idx) ")"))))

(do-template [<name> <method>]
  (defn <name> [compile ?values special-args]
    (|do [:let [(&/$Cons ?text (&/$Nil)) ?values]
          =text (compile ?text)]
      (return (str "(" =text ")." <method> "()"))))

  ^:private compile-text-trim       "trim"
  ^:private compile-text-upper-case "toUpperCase"
  ^:private compile-text-lower-case "toLowerCase"
  )

(defn ^:private compile-char-to-text [compile ?values special-args]
  (|do [:let [(&/$Cons ?x (&/$Nil)) ?values]
        =x (compile ?x)]
    (return (str "(" =x ").C"))))

(defn ^:private compile-lux-log [compile ?values special-args]
  (|do [:let [(&/$Cons ?message (&/$Nil)) ?values]
        =message (compile ?message)]
    (return (str "LuxRT.log(" =message ")"))))

(defn ^:private compile-lux-error [compile ?values special-args]
  (|do [:let [(&/$Cons ?message (&/$Nil)) ?values]
        =message (compile ?message)]
    (return (str "LuxRT.error(" =message ")"))))

(defn compile-proc [compile proc-category proc-name ?values special-args]
  (case proc-category
    "lux"
    (case proc-name
      "is"                   (compile-lux-is compile ?values special-args))

    "io"
    (case proc-name
      "log"                  (compile-lux-log compile ?values special-args)
      "error"                (compile-lux-error compile ?values special-args))

    "text"
    (case proc-name
      "="                    (compile-text-eq compile ?values special-args)
      "append"               (compile-text-append compile ?values special-args)
      "clip"                 (compile-text-clip compile ?values special-args)
      "index"                (compile-text-index compile ?values special-args)
      "last-index"           (compile-text-last-index compile ?values special-args)
      "size"                 (compile-text-size compile ?values special-args)
      "replace-all"          (compile-text-replace-all compile ?values special-args)
      "trim"                 (compile-text-trim compile ?values special-args)
      "char"                 (compile-text-char compile ?values special-args)
      "upper-case"           (compile-text-upper-case compile ?values special-args)
      "lower-case"           (compile-text-lower-case compile ?values special-args)
      )
    
    ;; "bit"
    ;; (case proc-name
    ;;   "count"                (compile-bit-count compile ?values special-args)
    ;;   "and"                  (compile-bit-and compile ?values special-args)
    ;;   "or"                   (compile-bit-or compile ?values special-args)
    ;;   "xor"                  (compile-bit-xor compile ?values special-args)
    ;;   "shift-left"           (compile-bit-shift-left compile ?values special-args)
    ;;   "shift-right"          (compile-bit-shift-right compile ?values special-args)
    ;;   "unsigned-shift-right" (compile-bit-unsigned-shift-right compile ?values special-args))
    
    ;; "array"
    ;; (case proc-name
    ;;   "get" (compile-array-get compile ?values special-args))

    "nat"
    (case proc-name
      "+"         (compile-nat-add compile ?values special-args)
      "-"         (compile-nat-sub compile ?values special-args)
      "*"         (compile-nat-mul compile ?values special-args)
      "/"         (compile-nat-div compile ?values special-args)
      "%"         (compile-nat-rem compile ?values special-args)
      "="         (compile-nat-eq compile ?values special-args)
      "<"         (compile-nat-lt compile ?values special-args)
      "encode"    (compile-nat-encode compile ?values special-args)
      "decode"    (compile-nat-decode compile ?values special-args)
      "max-value" (compile-nat-max-value compile ?values special-args)
      "min-value" (compile-nat-min-value compile ?values special-args)
      "to-int"    (compile-nat-to-int compile ?values special-args)
      ;; "to-char"   (compile-nat-to-char compile ?values special-args)
      )

    "int"
    (case proc-name
      "+"         (compile-int-add compile ?values special-args)
      "-"         (compile-int-sub compile ?values special-args)
      "*"         (compile-int-mul compile ?values special-args)
      "/"         (compile-int-div compile ?values special-args)
      "%"         (compile-int-rem compile ?values special-args)
      "="         (compile-int-eq compile ?values special-args)
      "<"         (compile-int-lt compile ?values special-args)
      "encode"    (compile-int-encode compile ?values special-args)
      "decode"    (compile-int-decode compile ?values special-args)
      "max-value" (compile-int-max-value compile ?values special-args)
      "min-value" (compile-int-min-value compile ?values special-args)
      "to-nat"    (compile-int-to-nat compile ?values special-args)
      "to-real"   (compile-int-to-real compile ?values special-args)
      )
    
    "deg"
    (case proc-name
      "+"         (compile-deg-add compile ?values special-args)
      "-"         (compile-deg-sub compile ?values special-args)
      "*"         (compile-deg-mul compile ?values special-args)
      "/"         (compile-deg-div compile ?values special-args)
      "%"         (compile-deg-rem compile ?values special-args)
      "="         (compile-deg-eq compile ?values special-args)
      "<"         (compile-deg-lt compile ?values special-args)
      "encode"    (compile-deg-encode compile ?values special-args)
      "decode"    (compile-deg-decode compile ?values special-args)
      "max-value" (compile-deg-max-value compile ?values special-args)
      "min-value" (compile-deg-min-value compile ?values special-args)
      "to-real"   (compile-deg-to-real compile ?values special-args)
      "scale"     (compile-deg-scale compile ?values special-args)
      )

    "real"
    (case proc-name
      "+"         (compile-real-add compile ?values special-args)
      "-"         (compile-real-sub compile ?values special-args)
      "*"         (compile-real-mul compile ?values special-args)
      "/"         (compile-real-div compile ?values special-args)
      "%"         (compile-real-rem compile ?values special-args)
      "="         (compile-real-eq compile ?values special-args)
      "<"         (compile-real-lt compile ?values special-args)
      "encode"    (compile-real-encode compile ?values special-args)
      "decode"    (compile-real-decode compile ?values special-args)
      "max-value" (compile-real-max-value compile ?values special-args)
      "min-value" (compile-real-min-value compile ?values special-args)
      "not-a-number" (compile-real-not-a-number compile ?values special-args)
      "positive-infinity" (compile-real-positive-infinity compile ?values special-args)
      "negative-infinity" (compile-real-negative-infinity compile ?values special-args)
      "to-deg"    (compile-real-to-deg compile ?values special-args)
      "to-int"    (compile-real-to-int compile ?values special-args)
      "hash"      (compile-real-hash compile ?values special-args)
      )

    "char"
    (case proc-name
      "to-text" (compile-char-to-text compile ?values special-args)
      ;; "to-nat"    (compile-char-to-nat compile ?values special-args)
      )
    
    ;; else
    (&/fail-with-loc (str "[Compiler Error] Unknown host procedure: " [proc-category proc-name]))))
