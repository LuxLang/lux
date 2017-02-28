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
(defn ^:private captured-name [register]
  (str "$" register))

(defn ^:private register-name [register]
  (str "_" register))

;; [Exports]
(defn compile-bool [?value]
  (return (str ?value)))

(def mask-4b (dec (bit-shift-left 1 32)))

(do-template [<name>]
  (defn <name> [value]
    (let [high (-> value (bit-shift-right 32) int)
          low (-> value (bit-and mask-4b) (bit-shift-left 32) (bit-shift-right 32) int)]
      (return (str &&rt/LuxRT "." "makeI64" "(" high "," low ")"))))

  compile-nat
  compile-int
  compile-deg
  )

(defn compile-real [value]
  (return (str value)))

(defn compile-char [value]
  (return (str "{C:" (pr-str (str value)) "}")))

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

(defn compile-captured [compile ?scope ?captured-id ?source]
  (return (captured-name ?captured-id)))

(defn compile-global [module name]
  (return (&&/js-var-name module name)))

(defn compile-apply [compile ?fn ?args]
  (|do [=fn (compile ?fn)
        =args (&/map% compile ?args)]
    (return (str =fn "(" (->> =args (&/|interpose ",") (&/fold str "")) ")"))))

(defn compile-loop [compile register-offset inits body]
  (|do [:let [registers (&/|map #(->> % (+ register-offset) register-name)
                                (&/|range* 0 (dec (&/|length inits))))]
        register-inits (&/map% compile inits)
        =body (compile body)]
    (return (str "(function _loop(" (->> registers (&/|interpose ",") (&/fold str "")) ") {"
                 =body
                 "})(" (->> register-inits (&/|interpose ",") (&/fold str "")) ")"))
    ))

(defn compile-iter [compile register-offset ?args]
  ;; Can only optimize if it is a simple expression.
  ;; Won't work if it's inside an 'if', unlike on the JVM.
  ;; (|do [[updates _] (&/fold% (fn [updates+offset ?arg]
  ;;                              (|let [[updates offset] updates+offset
  ;;                                     already-set? (|case ?arg
  ;;                                                    [_ (&o/$var (&/$Local l-idx))]
  ;;                                                    (= offset l-idx)

  ;;                                                    _
  ;;                                                    false)]
  ;;                                (if already-set?
  ;;                                  (return (&/T [updates (inc offset)]))
  ;;                                  (|do [=arg (compile ?arg)]
  ;;                                    (return (&/T [(str updates
  ;;                                                       (register-name offset) " = " =arg ";")
  ;;                                                  (inc offset)]))))))
  ;;                            (&/T ["" register-offset])
  ;;                            ?args)]
  ;;   (return updates))
  (|do [=args (&/map% compile ?args)]
    (return (str "_loop("
                 (->> =args (&/|interpose ",") (&/fold str ""))
                 ")")))
  )

(defn compile-let [compile _value _register _body]
  (|do [=value (compile _value)
        =body (compile _body)]
    (return (str "(function() {"
                 "var " (register-name _register) " = " =value ";"
                 " return " =body
                 ";})()"))))

(defn compile-record-get [compile _value _path]
  (|do [=value (compile _value)]
    (return (&/fold (fn [source step]
                      (|let [[idx tail?] step
                             method (if tail? "product_getRight" "product_getLeft")]
                        (str &&rt/LuxRT "." method "(" source "," idx ")")))
                    (str "(" =value ")")
                    _path))))

(defn compile-if [compile _test _then _else]
  (|do [=test (compile _test)
        =then (compile _then)
        =else (compile _else)]
    (return (str "(" =test " ? " =then " : " =else ")"))))

(def ^:private savepoint "pm_cursor_savepoint")
(def ^:private cursor "pm_cursor")
(defn ^:private cursor-push [value]
  (str cursor ".push(" value ");"))
(def ^:private cursor-save (str savepoint ".push(" cursor ".slice());"))
(def ^:private cursor-restore (str cursor " = " savepoint ".pop();"))
(def ^:private cursor-peek (str cursor "[" cursor ".length - 1]"))
(def ^:private cursor-pop (str cursor ".pop();"))
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
    (return cursor-pop)

    (&o/$BindPM _register)
    (return (str "var " (register-name _register) " = " cursor-peek ";"
                 cursor-pop))

    (&o/$BoolPM _value)
    (return (str "if(" cursor-peek " !== " _value ") { " pm-fail " }"))

    (&o/$NatPM _value)
    (|do [=value (compile-nat _value)]
      (return (str "if(!" (str "LuxRT.eqI64(" cursor-peek "," =value ")") ") { " pm-fail " }")))

    (&o/$IntPM _value)
    (|do [=value (compile-int _value)]
      (return (str "if(!" (str "LuxRT.eqI64(" cursor-peek "," =value ")") ") { " pm-fail " }")))

    (&o/$DegPM _value)
    (|do [=value (compile-deg _value)]
      (return (str "if(!" (str "LuxRT.eqI64(" cursor-peek "," =value ")") ") { " pm-fail " }")))

    (&o/$RealPM _value)
    (return (str "if(" cursor-peek " !== " _value ") { " pm-fail " }"))

    (&o/$CharPM _value)
    (|do [=value (compile-char _value)]
      (return (str "if(" (str "(" cursor-peek ").C") " !== " (str "(" =value ").C") ") { " pm-fail " }")))

    (&o/$TextPM _value)
    (|do [=value (compile-text _value)]
      (return (str "if(" cursor-peek " !== " =value ") { " pm-fail " }")))

    (&o/$TuplePM _idx+)
    (|let [[_idx is-tail?] (|case _idx+
                             (&/$Left _idx)
                             (&/T [_idx false])

                             (&/$Right _idx)
                             (&/T [_idx true]))
           getter (if is-tail? "product_getRight" "product_getLeft")]
      (return (str (cursor-push (str &&rt/LuxRT "." getter "(" cursor-peek "," _idx ")")))))

    (&o/$VariantPM _idx+)
    (|let [[_idx is-last] (|case _idx+
                            (&/$Left _idx)
                            (&/T [_idx false])

                            (&/$Right _idx)
                            (&/T [_idx true]))
           temp-assignment (str "temp = " &&rt/LuxRT "." "sum_get(" cursor-peek "," _idx "," (if is-last "\"\"" "null") ");")]
      (return (str temp-assignment
                   (str "if(temp !== null) {"
                        (cursor-push "temp")
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
      (return (str "try {"
                   cursor-save
                   =left
                   "}"
                   "catch(ex) {"
                   "if(ex === " pm-error ") {"
                   cursor-restore
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
                 "var " cursor " = [" =value "];"
                 "var " savepoint " = [];"
                 =pm
                 "})()"))))

(defn compile-function [compile arity ?scope ?env ?body]
  (|do [:let [??scope (&/|reverse ?scope)
              function-name (str (&&/js-module (&/|head ??scope))
                                 "$" (&host/location (&/|tail ??scope)))
              func-args (->> (&/|range* 0 (dec arity))
                             (&/|map (fn [register] (str "var " (register-name (inc register)) " = arguments[" register "];")))
                             (&/fold str ""))]
        =env-vars (&/map% (fn [=captured]
                            (|case =captured
                              [_ (&o/$captured ?scope ?captured-id ?source)]
                              (return (captured-name ?captured-id))))
                          (&/|vals ?env))
        =env-values (&/map% (fn [=captured]
                              (|case =captured
                                [_ (&o/$captured ?scope ?captured-id ?source)]
                                (compile ?source)))
                            (&/|vals ?env))
        =body (compile ?body)]
    (return (str "(function(" (->> =env-vars (&/|interpose ",") (&/fold str "")) ") {"
                 "return "
                 (str "(function " function-name "() {"
                      "\"use strict\";"
                      "var num_args = arguments.length;"
                      "if(num_args == " arity ") {"
                      (str "var " (register-name 0) " = " function-name ";")
                      (str "var _loop = " function-name ";")
                      func-args
                      (str "while(true) {"
                           "return " =body ";"
                           "}")
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
                      "})")
                 ";})(" (->> =env-values (&/|interpose ",") (&/fold str "")) ")"))))

(defn compile-def [compile ?name ?body def-meta]
  (|do [module-name &/get-module-name]
    (|case (&a-meta/meta-get &a-meta/alias-tag def-meta)
      (&/$Some (&/$IdentA [r-module r-name]))
      (if (= 1 (&/|length def-meta))
        (|do [def-value (&&/run-js! (&&/js-var-name r-module r-name))
              def-type (&a-module/def-type r-module r-name)
              _ (&/without-repl-closure
                 (&a-module/define module-name ?name def-type def-meta def-value))]
          (return nil))
        (&/fail-with-loc (str "[Compilation Error] Aliases cannot contain meta-data: " module-name ";" ?name)))

      (&/$Some _)
      (&/fail-with-loc "[Compilation Error] Invalid syntax for lux;alias meta-data. Must be an Ident.")
      
      _
      (|do [:let [var-name (&&/js-var-name module-name ?name)]
            =body (compile ?body)
            :let [def-js (str "var " var-name " = " =body ";")
                  is-type? (|case (&a-meta/meta-get &a-meta/type?-tag def-meta)
                             (&/$Some (&/$BoolA true))
                             true

                             _
                             false)
                  def-type (&a/expr-type* ?body)
                  ;; _ (&/|log! (string/replace def-js " " "^@"))
                  ]
            _ (&&/save-js! ?name def-js)
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
