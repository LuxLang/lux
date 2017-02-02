(ns lux.compiler.js.lux
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |let |case]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host]
                 [optimizer :as &o])
            (lux.analyser [base :as &a]
                          [module :as &a-module]
                          [meta :as &a-meta])
            (lux.compiler.js [base :as &&]
                             [rt :as &&rt])
            ))

;; [Utils]
(defn ^:private js-var-name [module name]
  (str (string/replace module "/" "$") "$" (&host/def-name name)))

(defn ^:private register-name [register]
  (str "_" register))

;; [Exports]
(defn compile-bool [?value]
  (return (str ?value)))

(do-template [<name>]
  (defn <name> [value]
    (return (str value "|0")))

  compile-nat
  compile-int
  compile-deg
  )

(defn compile-real [value]
  (return (str value)))

(defn compile-char [value]
  (return (str "\"" value "\"")))

(defn compile-text [?value]
  (return (pr-str ?value)))

(defn compile-tuple [compile ?elems]
  (|do [:let [num-elems (&/|length ?elems)]]
    (|case num-elems
      0
      (return &&/unit)

      1
      (compile (&/|head ?elems))

      _
      (|do [=elems (&/map% compile ?elems)]
        (return (str "[" (->> =elems (&/|interpose ",") (&/fold str "")) "]"))))))

(defn compile-variant [compile tag tail? value]
  (|do [value-expr (compile value)]
    (return (str "[" tag
                 "," (if tail? "\"\"" "null")
                 "," value-expr
                 "]"))))

(defn compile-local [compile register]
  (return (register-name register)))

;; (defn compile-captured [compile ?scope ?captured-id ?source]
;;   (|do [:let [??scope (&/|reverse ?scope)]
;;         ^MethodVisitor *writer* &/get-writer
;;         :let [_ (doto *writer*
;;                   (.visitVarInsn Opcodes/ALOAD 0)
;;                   (.visitFieldInsn Opcodes/GETFIELD
;;                                    (str (&host/->module-class (&/|head ??scope)) "/" (&host/location (&/|tail ??scope)))
;;                                    (str &&/closure-prefix ?captured-id)
;;                                    "Ljava/lang/Object;"))]]
;;     (return nil)))

(defn compile-global [module name]
  (return (js-var-name module name)))

(defn compile-apply [compile ?fn ?args]
  (|do [=fn (compile ?fn)
        =args (&/map% compile ?args)]
    (return (str =fn "(" (->> =args (&/|interpose ",") (&/fold str "")) ")"))))

;; (defn compile-loop [compile-expression register-offset inits body]
;;   (|do [^MethodVisitor *writer* &/get-writer
;;         :let [idxs+inits (&/zip2 (&/|range* 0 (dec (&/|length inits)))
;;                                  inits)]
;;         _ (&/map% (fn [idx+_init]
;;                     (|do [:let [[idx _init] idx+_init
;;                                 idx+ (+ register-offset idx)]
;;                           _ (compile-expression nil _init)
;;                           :let [_ (.visitVarInsn *writer* Opcodes/ASTORE idx+)]]
;;                       (return nil)))
;;                   idxs+inits)
;;         :let [$begin (new Label)
;;               _ (.visitLabel *writer* $begin)]]
;;     (compile-expression $begin body)
;;     ))

;; (defn compile-iter [compile $begin register-offset ?args]
;;   (|do [^MethodVisitor *writer* &/get-writer
;;         :let [idxs+args (&/zip2 (&/|range* 0 (dec (&/|length ?args)))
;;                                 ?args)]
;;         _ (&/map% (fn [idx+?arg]
;;                     (|do [:let [[idx ?arg] idx+?arg
;;                                 idx+ (+ register-offset idx)
;;                                 already-set? (|case ?arg
;;                                                [_ (&o/$var (&/$Local l-idx))]
;;                                                (= idx+ l-idx)

;;                                                _
;;                                                false)]]
;;                       (if already-set?
;;                         (return nil)
;;                         (compile ?arg))))
;;                   idxs+args)
;;         _ (&/map% (fn [idx+?arg]
;;                     (|do [:let [[idx ?arg] idx+?arg
;;                                 idx+ (+ register-offset idx)
;;                                 already-set? (|case ?arg
;;                                                [_ (&o/$var (&/$Local l-idx))]
;;                                                (= idx+ l-idx)

;;                                                _
;;                                                false)]
;;                           :let [_ (when (not already-set?)
;;                                     (.visitVarInsn *writer* Opcodes/ASTORE idx+))]]
;;                       (return nil)))
;;                   (&/|reverse idxs+args))
;;         :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $begin)]]
;;     (return nil)))

(defn compile-let [compile _value _register _body]
  (|do [=value (compile _value)
        =body (compile _body)]
    (return (str "(function() {"
                 "var " (register-name _register) " = " =value ";"
                 " return " =body
                 ";})()"))))

;; (defn compile-record-get [compile _value _path]
;;   (|do [^MethodVisitor *writer* &/get-writer
;;         _ (compile _value)
;;         :let [_ (&/|map (fn [step]
;;                           (|let [[idx tail?] step]
;;                             (doto *writer*
;;                               (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
;;                               (.visitLdcInsn (int idx))
;;                               (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT"
;;                                                 (if tail? "product_getRight" "product_getLeft")
;;                                                 "([Ljava/lang/Object;I)Ljava/lang/Object;"))))
;;                         _path)]]
;;     (return nil)))

;; (defn compile-if [compile _test _then _else]
;;   (|do [^MethodVisitor *writer* &/get-writer
;;         _ (compile _test)
;;         :let [$else (new Label)
;;               $end (new Label)
;;               _ (doto *writer*
;;                   &&/unwrap-boolean
;;                   (.visitJumpInsn Opcodes/IFEQ $else))]
;;         _ (compile _then)
;;         :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)]
;;         :let [_ (.visitLabel *writer* $else)]
;;         _ (compile _else)
;;         :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)
;;               _ (.visitLabel *writer* $end)]]
;;     (return nil)))

(def ^:private original "pm_stack_original")
(def ^:private stack "pm_stack")
(defn ^:private stack-push [value]
  (str stack ".push(" value ");"))
(def ^:private stack-init (str stack " = " original ".slice();"))
(def ^:private stack-peek (str stack "[" stack ".length - 1]"))
(def ^:private stack-pop (str stack ".pop();"))
(def ^:private pm-error (.intern (pr-str (str (char 0) "PM-ERROR" (char 0)))))
(def ^:private pm-fail (str "throw " pm-error ";"))

(defn ^:private compile-pm* [compile pm bodies]
  "(-> Case-Pattern (List Analysis) (Lux JS))"
  (|case pm
    (&o/$ExecPM _body-idx)
    (|case (&/|at _body-idx bodies)
      (&/$Some body)
      (|do [=body (compile body)]
        (return (str "return " =body ";")))

      (&/$None)
      (assert false))

    (&o/$PopPM)
    (return stack-pop)

    (&o/$BindPM _register)
    (return (str "var " (register-name _register) " = " stack-peek ";"
                 stack-pop))

    (&o/$BoolPM _value)
    (return (str "if(" stack-peek "!== " _value ") { " pm-fail " }"))

    (&o/$NatPM _value)
    (return (str "if(" stack-peek "!== " _value ") { " pm-fail " }"))

    (&o/$IntPM _value)
    (return (str "if(" stack-peek "!== " _value ") { " pm-fail " }"))

    (&o/$DegPM _value)
    (return (str "if(" stack-peek "!== " _value ") { " pm-fail " }"))

    (&o/$RealPM _value)
    (return (str "if(" stack-peek "!== " _value ") { " pm-fail " }"))

    (&o/$CharPM _value)
    (return (str "if(" stack-peek "!== \"" _value "\") { " pm-fail " }"))

    (&o/$TextPM _value)
    (return (str "if(" stack-peek "!== \"" _value "\") { " pm-fail " }"))

    (&o/$TuplePM _idx+)
    (|let [[_idx is-tail?] (|case _idx+
                             (&/$Left _idx)
                             (&/T [_idx false])

                             (&/$Right _idx)
                             (&/T [_idx true]))
           getter (if is-tail? "product_getRight" "product_getLeft")]
      (return (str (stack-push (str &&rt/LuxRT "." getter "(" stack-peek "," _idx ")")))))

    (&o/$VariantPM _idx+)
    (|let [[_idx is-last] (|case _idx+
                            (&/$Left _idx)
                            (&/T [_idx false])

                            (&/$Right _idx)
                            (&/T [_idx true]))
           temp-assignment (str "temp = " &&rt/LuxRT "." "sum_get(" stack-peek "," _idx "," (if is-last "\"\"" "null") ");")]
      (return (str temp-assignment
                   (str "if(temp) {"
                        (stack-push "temp")
                        "}"
                        "else {"
                        pm-fail
                        "}"))))

    (&o/$SeqPM _left-pm _right-pm)
    (|do [=left (compile-pm* compile _left-pm bodies)
          =right (compile-pm* compile _right-pm bodies)]
      (return (str =left =right)))

    (&o/$AltPM _left-pm _right-pm)
    (|do [=left (compile-pm* compile _left-pm bodies)
          =right (compile-pm* compile _right-pm bodies)]
      (return (str "try {" =left "}"
                   "catch(ex) {"
                   "if(ex === " pm-error ") {"
                   stack-init
                   =right
                   "}"
                   "else {"
                   "throw ex;"
                   "}"
                   "}")))
    ))

(defn ^:private compile-pm [compile pm bodies]
  (|do [raw (compile-pm* compile pm bodies)]
    (return (str "try {" raw "}"
                 "catch(ex) {"
                 "if(ex === " pm-error ") {"
                 "throw \"Invalid expression for pattern-matching.\";"
                 "}"
                 "else {"
                 "throw ex;"
                 "}"
                 "}"))))

;; [Resources]
(defn compile-case [compile ?value ?pm ?bodies]
  (|do [=value (compile ?value)
        =pm (compile-pm compile ?pm ?bodies)]
    (return (str "(function() {"
                 "\"use strict\";"
                 "var temp;"
                 "var " original " = [" =value "];"
                 "var " stack-init
                 =pm
                 "})()"))))

(defn compile-function [compile arity ?scope ?env ?body]
  (|do [:let [??scope (&/|reverse ?scope)
              function-name (str (&host/->module-class (&/|head ??scope))
                                 "$" (&host/location (&/|tail ??scope)))
              func-args (->> (&/|range* 0 (dec arity))
                             (&/|map (fn [register] (str "var " (register-name (inc register)) " = arguments[" register "];")))
                             (&/fold str ""))]
        =body (compile ?body)]
    (return (str "(function " function-name "() {"
                 "\"use strict\";"
                 "var num_args = arguments.length;"
                 "if(num_args == " arity ") {"
                 "var " (register-name 0) " = " function-name ";"
                 func-args
                 "return " =body ";"
                 "}"
                 "else if(num_args > " arity ") {"
                 "return " function-name ".apply(null, [].slice.call(arguments,0," arity "))"
                 ".apply(null, [].slice.call(arguments," arity "));"
                 "}"
                 ;; Less than arity
                 "else {"
                 "var curried = [].slice.call(arguments);"
                 "return function() { "
                 "return " function-name ".apply(null, curried.concat([].slice.call(arguments)));"
                 " };"
                 "}"
                 "})"))))

(defn compile-def [compile ?name ?body def-meta]
  (|do [module-name &/get-module-name
        class-loader &/loader
        :let [var-name (js-var-name module-name ?name)]]
    (|case (&a-meta/meta-get &a-meta/alias-tag def-meta)
      (&/$Some (&/$IdentA [r-module r-name]))
      (if (= 1 (&/|length def-meta))
        (|do [def-value (&&/run-js! var-name)
              def-type (&a-module/def-type r-module r-name)
              _ (&/without-repl-closure
                 (&a-module/define module-name ?name def-type def-meta def-value))]
          (return nil))
        (&/fail-with-loc (str "[Compilation Error] Aliases cannot contain meta-data: " module-name ";" ?name)))

      (&/$Some _)
      (&/fail-with-loc "[Compilation Error] Invalid syntax for lux;alias meta-data. Must be an Ident.")
      
      _
      (|do [=body (compile ?body)
            :let [def-js (str "var " var-name " = " =body ";")
                  is-type? (|case (&a-meta/meta-get &a-meta/type?-tag def-meta)
                             (&/$Some (&/$BoolA true))
                             true

                             _
                             false)
                  def-type (&a/expr-type* ?body)
                  _ (&/|log! (str "def-js >>\n"
                                  (string/replace def-js " " "^@")))]
            _ (&&/run-js! def-js)
            def-value (&&/run-js!+ var-name)
            _ (&/without-repl-closure
               (&a-module/define module-name ?name def-type def-meta def-value))
            _ (|case (&/T [is-type? (&a-meta/meta-get &a-meta/tags-tag def-meta)])
                [true (&/$Some (&/$ListA tags*))]
                (|do [:let [was-exported? (|case (&a-meta/meta-get &a-meta/export?-tag def-meta)
                                            (&/$Some _)
                                            true

                                            _
                                            false)]
                      tags (&/map% (fn [tag*]
                                     (|case tag*
                                       (&/$TextA tag)
                                       (return tag)

                                       _
                                       (&/fail-with-loc "[Compiler Error] Incorrect format for tags.")))
                                   tags*)
                      _ (&a-module/declare-tags module-name tags was-exported? def-value)]
                  (return nil))

                [false (&/$Some _)]
                (&/fail-with-loc "[Compiler Error] Can't define tags for non-type.")

                [true (&/$Some _)]
                (&/fail-with-loc "[Compiler Error] Incorrect format for tags.")

                [_ (&/$None)]
                (return nil))
            :let [_ (println 'DEF (str module-name ";" ?name))]]
        (return nil))
      ))
  )

(defn compile-program [compile ?body]
  (assert false "compile-program")
  ;; (|do [module-name &/get-module-name
  ;;       ^ClassWriter *writer* &/get-writer]
  ;;   (&/with-writer (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "main" "([Ljava/lang/String;)V" nil nil)
  ;;                    (.visitCode))
  ;;     (|do [^MethodVisitor main-writer &/get-writer
  ;;           :let [$loop (new Label)
  ;;                 $end (new Label)
  ;;                 _ (doto main-writer
  ;;                     ;; Tail: Begin
  ;;                     (.visitLdcInsn (->> #'&/$Nil meta ::&/idx int)) ;; I
  ;;                     (.visitInsn Opcodes/ACONST_NULL) ;; I?
  ;;                     (.visitLdcInsn &/unit-tag) ;; I?U
  ;;                     (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;") ;; V
  ;;                     ;; Tail: End
  ;;                     ;; Size: Begin
  ;;                     (.visitVarInsn Opcodes/ALOAD 0) ;; VA
  ;;                     (.visitInsn Opcodes/ARRAYLENGTH) ;; VI
  ;;                     ;; Size: End
  ;;                     ;; Loop: Begin
  ;;                     (.visitLabel $loop)
  ;;                     (.visitLdcInsn (int 1)) ;; VII
  ;;                     (.visitInsn Opcodes/ISUB) ;; VI
  ;;                     (.visitInsn Opcodes/DUP) ;; VII
  ;;                     (.visitJumpInsn Opcodes/IFLT $end) ;; VI
  ;;                     ;; Head: Begin
  ;;                     (.visitInsn Opcodes/DUP) ;; VII
  ;;                     (.visitVarInsn Opcodes/ALOAD 0) ;; VIIA
  ;;                     (.visitInsn Opcodes/SWAP) ;; VIAI
  ;;                     (.visitInsn Opcodes/AALOAD) ;; VIO
  ;;                     (.visitInsn Opcodes/SWAP) ;; VOI
  ;;                     (.visitInsn Opcodes/DUP_X2) ;; IVOI
  ;;                     (.visitInsn Opcodes/POP) ;; IVO
  ;;                     ;; Head: End
  ;;                     ;; Tuple: Begin
  ;;                     (.visitLdcInsn (int 2)) ;; IVOS
  ;;                     (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; IVO2
  ;;                     (.visitInsn Opcodes/DUP_X1) ;; IV2O2
  ;;                     (.visitInsn Opcodes/SWAP) ;; IV22O
  ;;                     (.visitLdcInsn (int 0)) ;; IV22OI
  ;;                     (.visitInsn Opcodes/SWAP) ;; IV22IO
  ;;                     (.visitInsn Opcodes/AASTORE) ;; IV2
  ;;                     (.visitInsn Opcodes/DUP_X1) ;; I2V2
  ;;                     (.visitInsn Opcodes/SWAP) ;; I22V
  ;;                     (.visitLdcInsn (int 1)) ;; I22VI
  ;;                     (.visitInsn Opcodes/SWAP) ;; I22IV
  ;;                     (.visitInsn Opcodes/AASTORE) ;; I2
  ;;                     ;; Tuple: End
  ;;                     ;; Cons: Begin
  ;;                     (.visitLdcInsn (->> #'&/$Cons meta ::&/idx int)) ;; I2I
  ;;                     (.visitLdcInsn "") ;; I2I?
  ;;                     (.visitInsn Opcodes/DUP2_X1) ;; II?2I?
  ;;                     (.visitInsn Opcodes/POP2) ;; II?2
  ;;                     (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;") ;; IV
  ;;                     ;; Cons: End
  ;;                     (.visitInsn Opcodes/SWAP) ;; VI
  ;;                     (.visitJumpInsn Opcodes/GOTO $loop)
  ;;                     ;; Loop: End
  ;;                     (.visitLabel $end) ;; VI
  ;;                     (.visitInsn Opcodes/POP) ;; V
  ;;                     (.visitVarInsn Opcodes/ASTORE (int 0)) ;;
  ;;                     )
  ;;                 ]
  ;;           _ (compile ?body)
  ;;           :let [_ (doto main-writer
  ;;                     (.visitTypeInsn Opcodes/CHECKCAST &&/function-class)
  ;;                     (.visitInsn Opcodes/ACONST_NULL)
  ;;                     (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature 1)))]
  ;;           :let [_ (doto main-writer
  ;;                     (.visitInsn Opcodes/POP)
  ;;                     (.visitInsn Opcodes/RETURN)
  ;;                     (.visitMaxs 0 0)
  ;;                     (.visitEnd))]]
  ;;       (return nil))))
  )
