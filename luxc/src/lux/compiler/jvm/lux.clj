(ns lux.compiler.jvm.lux
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
            [lux.host.generics :as &host-generics]
            (lux.analyser [base :as &a]
                          [module :as &a-module]
                          [meta :as &a-meta])
            (lux.compiler.jvm [base :as &&]
                              [function :as &&function]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)
           java.lang.reflect.Field))

;; [Exports]
(defn compile-bit [?value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC "java/lang/Boolean" (if ?value "TRUE" "FALSE") "Ljava/lang/Boolean;")]]
    (return nil)))

(do-template [<name> <class> <prim> <caster>]
  (defn <name> [value]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [_ (doto *writer*
                    (.visitLdcInsn (<caster> value))
                    (.visitMethodInsn Opcodes/INVOKESTATIC <class> "valueOf" (str "(" <prim> ")" (&host-generics/->type-signature <class>))))]]
      (return nil)))

  compile-nat  "java/lang/Long"      "J" long
  compile-int  "java/lang/Long"      "J" long
  compile-rev "java/lang/Long"      "J" long
  compile-frac "java/lang/Double"    "D" double
  )

(defn compile-text [?value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn compile-tuple [compile ?elems]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [num-elems (&/|length ?elems)]]
    (|case num-elems
      0
      (|do [:let [_ (.visitLdcInsn *writer* &/unit-tag)]]
        (return nil))

      1
      (compile (&/|head ?elems))
      
      _
      (|do [:let [_ (doto *writer*
                      (.visitLdcInsn (int num-elems))
                      (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object"))]
            _ (&/map2% (fn [idx elem]
                         (|do [:let [_ (doto *writer*
                                         (.visitInsn Opcodes/DUP)
                                         (.visitLdcInsn (int idx)))]
                               ret (compile elem)
                               :let [_ (.visitInsn *writer* Opcodes/AASTORE)]]
                           (return ret)))
                       (&/|range num-elems) ?elems)]
        (return nil)))))

(defn compile-variant [compile tag tail? value]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitLdcInsn *writer* (int tag))
              _ (if tail?
                  (.visitLdcInsn *writer* "")
                  (.visitInsn *writer* Opcodes/ACONST_NULL))]
        _ (compile value)
        :let [_ (.visitMethodInsn *writer* Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;")]]
    (return nil)))

(defn compile-local [compile ?idx]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitVarInsn *writer* Opcodes/ALOAD (int ?idx))]]
    (return nil)))

(defn compile-captured [compile ?scope ?captured-id ?source]
  (|do [:let [??scope (&/|reverse ?scope)]
        ^MethodVisitor *writer* &/get-writer
        :let [_ (doto *writer*
                  (.visitVarInsn Opcodes/ALOAD 0)
                  (.visitFieldInsn Opcodes/GETFIELD
                                   (str (&host/->module-class (&/|head ??scope)) "/" (&host/location (&/|tail ??scope)))
                                   (str &&/closure-prefix ?captured-id)
                                   "Ljava/lang/Object;"))]]
    (return nil)))

(defn compile-global [compile ?owner-class ?name]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (str (&host/->module-class ?owner-class) "/" (&host/def-name ?name)) &/value-field "Ljava/lang/Object;")]]
    (return nil)))

(defn ^:private compile-apply* [compile ?args]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (&/map% (fn [?args]
                    (|do [:let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST &&/function-class)]
                          _ (&/map% compile ?args)
                          :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature (&/|length ?args)))]]
                      (return nil)))
                  (&/|partition &&/num-apply-variants ?args))]
    (return nil)))

(defn compile-apply [compile ?fn ?args]
  (|case ?fn
    [_ (&o/$def ?module ?name)]
    (|do [[_ [_ _ _ func-obj]] (&a-module/find-def! ?module ?name)
          class-loader &/loader
          :let [func-class (class func-obj)
                func-arity (.get ^Field (.getDeclaredField func-class &&/arity-field) nil)
                func-partials (.get ^Field (.getDeclaredField (Class/forName "lux.Function" true class-loader) &&/partials-field) func-obj)
                num-args (&/|length ?args)
                func-class-name (->> func-class .getName &host-generics/->bytecode-class-name)]]
      (if (and (= 0 func-partials)
               (>= num-args func-arity))
        (|do [_ (compile ?fn)
              ^MethodVisitor *writer* &/get-writer
              :let [_ (.visitTypeInsn *writer* Opcodes/CHECKCAST func-class-name)]
              _ (&/map% compile (&/|take func-arity ?args))
              :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEVIRTUAL func-class-name (if (= 1 func-arity) &&/apply-method "impl") (&&/apply-signature func-arity))]
              _ (if (= num-args func-arity)
                  (return nil)
                  (compile-apply* compile (&/|drop func-arity ?args)))]
          (return nil))
        (|do [_ (compile ?fn)]
          (compile-apply* compile ?args))))
    
    _
    (|do [_ (compile ?fn)]
      (compile-apply* compile ?args))
    ))

(defn compile-loop [compile-expression register-offset inits body]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [idxs+inits (&/zip2 (&/|range* 0 (dec (&/|length inits)))
                                 inits)]
        _ (&/map% (fn [idx+_init]
                    (|do [:let [[idx _init] idx+_init
                                idx+ (+ register-offset idx)]
                          _ (compile-expression nil _init)
                          :let [_ (.visitVarInsn *writer* Opcodes/ASTORE idx+)]]
                      (return nil)))
                  idxs+inits)
        :let [$begin (new Label)
              _ (.visitLabel *writer* $begin)]]
    (compile-expression $begin body)
    ))

(defn compile-iter [compile $begin register-offset ?args]
  (|do [^MethodVisitor *writer* &/get-writer
        :let [idxs+args (&/zip2 (&/|range* 0 (dec (&/|length ?args)))
                                ?args)]
        _ (&/map% (fn [idx+?arg]
                    (|do [:let [[idx ?arg] idx+?arg
                                idx+ (+ register-offset idx)
                                already-set? (|case ?arg
                                               [_ (&o/$var (&/$Local l-idx))]
                                               (= idx+ l-idx)

                                               _
                                               false)]]
                      (if already-set?
                        (return nil)
                        (compile ?arg))))
                  idxs+args)
        _ (&/map% (fn [idx+?arg]
                    (|do [:let [[idx ?arg] idx+?arg
                                idx+ (+ register-offset idx)
                                already-set? (|case ?arg
                                               [_ (&o/$var (&/$Local l-idx))]
                                               (= idx+ l-idx)

                                               _
                                               false)]
                          :let [_ (when (not already-set?)
                                    (.visitVarInsn *writer* Opcodes/ASTORE idx+))]]
                      (return nil)))
                  (&/|reverse idxs+args))
        :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $begin)]]
    (return nil)))

(defn compile-let [compile _value _register _body]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile _value)
        :let [_ (.visitVarInsn *writer* Opcodes/ASTORE _register)]
        _ (compile _body)]
    (return nil)))

(defn compile-record-get [compile _value _path]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile _value)
        :let [_ (&/|map (fn [step]
                          (|let [[idx tail?] step]
                            (doto *writer*
                              (.visitTypeInsn Opcodes/CHECKCAST "[Ljava/lang/Object;")
                              (.visitLdcInsn (int (if tail?
                                                    (dec idx)
                                                    idx)))
                              (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT"
                                                (if tail? "tuple_right" "tuple_left")
                                                "([Ljava/lang/Object;I)Ljava/lang/Object;"))))
                        _path)]]
    (return nil)))

(defn compile-if [compile _test _then _else]
  (|do [^MethodVisitor *writer* &/get-writer
        _ (compile _test)
        :let [$else (new Label)
              $end (new Label)
              _ (doto *writer*
                  &&/unwrap-boolean
                  (.visitJumpInsn Opcodes/IFEQ $else))]
        _ (compile _then)
        :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)]
        :let [_ (.visitLabel *writer* $else)]
        _ (compile _else)
        :let [_ (.visitJumpInsn *writer* Opcodes/GOTO $end)
              _ (.visitLabel *writer* $end)]]
    (return nil)))

(defn ^:private de-ann [optim]
  (|case optim
    [_ (&o/$ann value-expr _)]
    value-expr

    _
    optim))

(defn ^:private throwable->text [^Throwable t]
  (let [base (->> t
                  .getStackTrace
                  (map str)
                  (cons (.getMessage t))
                  (interpose "\n")
                  (apply str))]
    (if-let [cause (.getCause t)]
      (str base "\n\n" "Caused by: " (throwable->text cause))
      base)))

(defn ^:private install-def! [class-loader current-class module-name ?name ?body ?meta exported?]
  (|do [_ (return nil)
        :let [def-class (&&/load-class! class-loader (&host-generics/->class-name current-class))
              def-type (&a/expr-type* ?body)]
        def-value (try (return (-> def-class (.getField &/value-field) (.get nil)))
                    (catch Throwable t
                      (&/assert! false
                                 (str "Error during value initialization:\n"
                                      (throwable->text t)))))
        _ (&/without-repl-closure
           (&a-module/define module-name ?name exported? def-type ?meta def-value))]
    (return def-value)))

(let [class-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
      field-flags (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC)]
  (defn compile-def [compile ?name ?body ?meta exported?]
    (|do [module-name &/get-module-name
          class-loader &/loader]
      (|case (&a-meta/meta-get &a-meta/alias-tag ?meta)
        (&/$Some [_ (&/$Identifier [r-module r-name])])
        (|case ?meta
          [_ (&/$Record ?meta*)]
          (if (= 1 (&/|length ?meta*))
            (|do [:let [current-class (&host-generics/->class-name (str (&host/->module-class r-module) "/" (&host/def-name r-name)))
                        def-class (&&/load-class! class-loader current-class)
                        def-value (-> def-class (.getField &/value-field) (.get nil))]
                  def-type (&a-module/def-type r-module r-name)
                  _ (&/without-repl-closure
                     (&a-module/define module-name ?name false def-type ?meta def-value))]
              (return nil))
            (&/fail-with-loc (str "[Compilation Error] Aliases cannot contain meta-data: " (str module-name &/+name-separator+ ?name)))))

        (&/$Some _)
        (&/fail-with-loc "[Compilation Error] Invalid syntax for lux;alias meta-data. Must be an identifier.")
        
        _
        (|case (de-ann ?body)
          [_ (&o/$function _ _ __scope _ _)]
          (|let [[_ (&o/$function _ _arity _scope _captured ?body+)] (&o/shift-function-body (&/|tail __scope) __scope
                                                                                             false
                                                                                             (de-ann ?body))]
            (|do [[file-name _ _] &/cursor
                  :let [datum-sig "Ljava/lang/Object;"
                        def-name (&host/def-name ?name)
                        current-class (str (&host/->module-class module-name) "/" def-name)
                        =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                                 (.visit &host/bytecode-version class-flags
                                         current-class nil &&/function-class (into-array String []))
                                 (-> (.visitField field-flags &/value-field datum-sig nil nil)
                                     (doto (.visitEnd)))
                                 (.visitSource file-name nil))]
                  instancer (&&function/compile-function compile (&/$Some =class) _arity _scope _captured ?body+)
                  _ (&/with-writer (.visitMethod =class Opcodes/ACC_STATIC "<clinit>" "()V" nil nil)
                      (|do [^MethodVisitor **writer** &/get-writer
                            :let [_ (.visitCode **writer**)]
                            _ instancer
                            :let [_ (.visitFieldInsn **writer** Opcodes/PUTSTATIC current-class &/value-field datum-sig)]
                            :let [_ (doto **writer**
                                      (.visitInsn Opcodes/RETURN)
                                      (.visitMaxs 0 0)
                                      (.visitEnd))]]
                        (return nil)))
                  :let [_ (.visitEnd =class)]
                  _ (&&/save-class! def-name (.toByteArray =class))
                  def-value (install-def! class-loader current-class module-name ?name ?body ?meta exported?)
                  :let [_ (println 'DEF (str module-name &/+name-separator+ ?name))]]
              (return def-value)))

          _
          (|do [[file-name _ _] &/cursor
                :let [datum-sig "Ljava/lang/Object;"
                      def-name (&host/def-name ?name)
                      current-class (str (&host/->module-class module-name) "/" def-name)
                      =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                               (.visit &host/bytecode-version class-flags
                                       current-class nil "java/lang/Object" (into-array String []))
                               (-> (.visitField field-flags &/value-field datum-sig nil nil)
                                   (doto (.visitEnd)))
                               (.visitSource file-name nil))]
                _ (&/with-writer (.visitMethod =class Opcodes/ACC_STATIC "<clinit>" "()V" nil nil)
                    (|do [^MethodVisitor **writer** &/get-writer
                          :let [_ (.visitCode **writer**)]
                          _ (compile nil ?body)
                          :let [_ (.visitFieldInsn **writer** Opcodes/PUTSTATIC current-class &/value-field datum-sig)]
                          :let [_ (doto **writer**
                                    (.visitInsn Opcodes/RETURN)
                                    (.visitMaxs 0 0)
                                    (.visitEnd))]]
                      (return nil)))
                :let [_ (.visitEnd =class)]
                _ (&&/save-class! def-name (.toByteArray =class))
                def-value (install-def! class-loader current-class module-name ?name ?body ?meta exported?)
                :let [_ (println 'DEF (str module-name &/+name-separator+ ?name))]]
            (return def-value)))
        ))))

(defn compile-program [compile ?program]
  (|do [module-name &/get-module-name
        ^ClassWriter *writer* &/get-writer]
    (&/with-writer (doto (.visitMethod *writer* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "main" "([Ljava/lang/String;)V" nil nil)
                     (.visitCode))
      (|do [^MethodVisitor main-writer &/get-writer
            _ (compile ?program)
            :let [_ (.visitTypeInsn main-writer Opcodes/CHECKCAST &&/function-class)]
            :let [$loop (new Label)
                  $end (new Label)
                  _ (doto main-writer
                      ;; Tail: Begin
                      (.visitLdcInsn (->> #'&/$Nil meta ::&/idx int)) ;; I
                      (.visitInsn Opcodes/ACONST_NULL) ;; I?
                      (.visitLdcInsn &/unit-tag) ;; I?U
                      (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;") ;; V
                      ;; Tail: End
                      ;; Size: Begin
                      (.visitVarInsn Opcodes/ALOAD 0) ;; VA
                      (.visitInsn Opcodes/ARRAYLENGTH) ;; VI
                      ;; Size: End
                      ;; Loop: Begin
                      (.visitLabel $loop)
                      (.visitLdcInsn (int 1)) ;; VII
                      (.visitInsn Opcodes/ISUB) ;; VI
                      (.visitInsn Opcodes/DUP) ;; VII
                      (.visitJumpInsn Opcodes/IFLT $end) ;; VI
                      ;; Head: Begin
                      (.visitInsn Opcodes/DUP) ;; VII
                      (.visitVarInsn Opcodes/ALOAD 0) ;; VIIA
                      (.visitInsn Opcodes/SWAP) ;; VIAI
                      (.visitInsn Opcodes/AALOAD) ;; VIO
                      (.visitInsn Opcodes/SWAP) ;; VOI
                      (.visitInsn Opcodes/DUP_X2) ;; IVOI
                      (.visitInsn Opcodes/POP) ;; IVO
                      ;; Head: End
                      ;; Tuple: Begin
                      (.visitLdcInsn (int 2)) ;; IVOS
                      (.visitTypeInsn Opcodes/ANEWARRAY "java/lang/Object") ;; IVO2
                      (.visitInsn Opcodes/DUP_X1) ;; IV2O2
                      (.visitInsn Opcodes/SWAP) ;; IV22O
                      (.visitLdcInsn (int 0)) ;; IV22OI
                      (.visitInsn Opcodes/SWAP) ;; IV22IO
                      (.visitInsn Opcodes/AASTORE) ;; IV2
                      (.visitInsn Opcodes/DUP_X1) ;; I2V2
                      (.visitInsn Opcodes/SWAP) ;; I22V
                      (.visitLdcInsn (int 1)) ;; I22VI
                      (.visitInsn Opcodes/SWAP) ;; I22IV
                      (.visitInsn Opcodes/AASTORE) ;; I2
                      ;; Tuple: End
                      ;; Cons: Begin
                      (.visitLdcInsn (->> #'&/$Cons meta ::&/idx int)) ;; I2I
                      (.visitLdcInsn "") ;; I2I?
                      (.visitInsn Opcodes/DUP2_X1) ;; II?2I?
                      (.visitInsn Opcodes/POP2) ;; II?2
                      (.visitMethodInsn Opcodes/INVOKESTATIC "lux/LuxRT" "sum_make" "(ILjava/lang/Object;Ljava/lang/Object;)[Ljava/lang/Object;") ;; IV
                      ;; Cons: End
                      (.visitInsn Opcodes/SWAP) ;; VI
                      (.visitJumpInsn Opcodes/GOTO $loop)
                      ;; Loop: End
                      (.visitLabel $end) ;; VI
                      (.visitInsn Opcodes/POP) ;; V
                      )]
            :let [_ (doto main-writer
                      (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature 1))
                      (.visitTypeInsn Opcodes/CHECKCAST &&/function-class)
                      (.visitInsn Opcodes/ACONST_NULL)
                      (.visitMethodInsn Opcodes/INVOKEVIRTUAL &&/function-class &&/apply-method (&&/apply-signature 1)))]
            :let [_ (doto main-writer
                      (.visitInsn Opcodes/POP)
                      (.visitInsn Opcodes/RETURN)
                      (.visitMaxs 0 0)
                      (.visitEnd))]]
        (return nil)))))
