(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m exhaust-m try-m try-all-m map-m reduce-m
                                         apply-m within
                                         normalize-ident]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils/General]
(defn ^:private write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(defn ^:private write-class [name data]
  (write-file (str "output/" name ".class") data))

(defn ^:private load-class! [loader name]
  (.loadClass loader name))

(defn ^:private save-class! [name bytecode]
  (exec [loader &util/loader
         :let [_ (write-class name bytecode)
               _ (load-class! loader (string/replace name #"/" "."))]]
    (return nil)))

(def ^:private +prefix+ "lux.")
(def ^:private +variant-class+  (str +prefix+ "Variant"))
(def ^:private +tuple-class+    (str +prefix+ "Tuple"))
(def ^:private +function-class+ (str +prefix+ "Function"))
(def ^:private +local-prefix+ "l")
(def ^:private +partial-prefix+ "p")
(def ^:private +closure-prefix+ "c")
(def ^:private +apply-signature+ "(Ljava/lang/Object;)Ljava/lang/Object;")

(def ^:private ->package ->class)

(defn ^:private ->type-signature [class]
  (case class
    "void"    "V"
    "boolean" "Z"
    "byte"    "B"
    "short"   "S"
    "int"     "I"
    "long"    "J"
    "float"   "F"
    "double"  "D"
    "char"    "C"
    ;; else
    (let [class* (->class class)]
      (if (.startsWith class* "[")
        class*
        (str "L" class* ";")))
    ))

(defn ^:private ->java-sig [type]
  (match type
    ::&type/Any
    (->type-signature "java.lang.Object")
    
    [::&type/Data ?name]
    (->type-signature ?name)

    [::&type/Array ?elem]
    (str "[" (->java-sig ?elem))

    [::&type/variant ?tag ?value]
    (->type-signature +variant-class+)

    [::&type/Lambda _ _]
    (->type-signature +function-class+)))

;; [Utils/Compilers]
(let [+class+ (->class "java.lang.Boolean")
      +sig+ (->type-signature "java.lang.Boolean")]
  (defn ^:private compile-bool [compile *type* ?value]
    (exec [*writer* &util/get-writer
           :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class "java.lang.Boolean") (if ?value "TRUE" "FALSE") (->type-signature "java.lang.Boolean"))]]
      (return nil))))

(do-template [<name> <class> <sig>]
  (let [+class+ (->class <class>)]
    (defn <name> [compile *type* ?value]
      (exec [*writer* &util/get-writer
             :let [_ (doto *writer*
                       (.visitTypeInsn Opcodes/NEW <class>)
                       (.visitInsn Opcodes/DUP)
                       (.visitLdcInsn ?literal)
                       (.visitMethodInsn Opcodes/INVOKESPECIAL <class> "<init>" <sig>))]]
        (return nil))))

  ^:private compile-int  "java.lang.Long"      "(J)V"
  ^:private compile-real "java.lang.Double"    "(D)V"
  ^:private compile-char "java.lang.Character" "(C)V"
  )

(defn ^:private compile-text [compile *type* ?value]
  (exec [*writer* &util/get-writer
         :let [_ (.visitLdcInsn *writer* ?value)]]
    (return nil)))

(defn ^:private compile-tuple [compile *type* ?elems]
  (exec [*writer* &util/get-writer
         :let [num-elems (count ?elems)
               tuple-class (->class (str +tuple-class+ num-elems))
               _ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW tuple-class)
                   (.visitInsn Opcodes/DUP)
                   (.visitMethodInsn Opcodes/INVOKESPECIAL tuple-class "<init>" "()V"))]
         _ (map-m (fn [idx]
                    (exec [:let [_ (.visitInsn *writer* Opcodes/DUP)]
                           ret (compile (nth ?elems idx))
                           :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD tuple-class (str +partial-prefix+ idx) "Ljava/lang/Object;")]]
                      (return ret)))
                  (range num-elems))]
    (return nil)))

(defn ^:private compile-variant [compile *type* ?tag ?members]
  (exec [*writer* &util/get-writer
         :let [variant-class* (str (->class +variant-class+) (count ?members))
               _ (doto *writer*
                   (.visitTypeInsn Opcodes/NEW variant-class*)
                   (.visitInsn Opcodes/DUP)
                   (.visitMethodInsn Opcodes/INVOKESPECIAL variant-class* "<init>" "()V")
                   (.visitInsn Opcodes/DUP)
                   (.visitLdcInsn ?tag)
                   (.visitFieldInsn Opcodes/PUTFIELD variant-class* "tag" (->type-signature "java.lang.String")))]
         _ (map-m (fn [[?tfield ?member]]
                    (exec [:let [_ (.visitInsn *writer* Opcodes/DUP)]
                           ret (compile ?member)
                           :let [_ (.visitFieldInsn *writer* Opcodes/PUTFIELD variant-class* (str +partial-prefix+ ?tfield) "Ljava/lang/Object;")]]
                      (return ret)))
                  (map vector (range (count ?members)) ?members))]
    (return nil)))

(defn ^:private compile-local [compile *type* ?idx]
  (exec [*writer* &util/get-writer
         :let [_ (.visitVarInsn *writer* Opcodes/ALOAD (int ?idx))]]
    (return nil)))

(defn ^:private compile-captured [compile *type* ?scope ?captured-id ?source]
  (exec [*writer* &util/get-writer
         :let [_ (doto *writer*
                   (.visitVarInsn Opcodes/ALOAD 0)
                   (.visitFieldInsn Opcodes/GETFIELD
                                    (normalize-ident ?scope)
                                    (str +closure-prefix+ ?captured-id)
                                    "Ljava/lang/Object;"))]]
    (return nil)))

(defn ^:private compile-global [compile *type* ?owner-class ?name]
  (exec [*writer* &util/get-writer
         :let [_ (.visitFieldInsn *writer* Opcodes/GETSTATIC (->class (&host/location (list ?name ?owner-class))) "_datum" "Ljava/lang/Object;")]]
    (return nil)))

(defn ^:private compile-call [compile *type* ?fn ?args]
  (exec [*writer* &util/get-writer
         _ (compile ?fn)
         _ (map-m (fn [arg]
                    (exec [ret (compile arg)
                           :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (->class +function-class+) "apply" +apply-signature+)]]
                      (return ret)))
                  ?args)]
    (return nil)))

(defn ^:private compile-static-call [compile *type* ?needs-num ?fn ?args]
  (assert false (pr-str 'compile-static-call))
  (exec [*writer* &util/get-writer
         :let [_ (match (:form ?fn)
                   [::&analyser/global ?owner-class ?fn-name]
                   (let [arg-sig (->type-signature "java.lang.Object")
                         call-class (&host/location (list ?fn-name ?owner-class))
                         provides-num (count ?args)]
                     (if (>= provides-num ?needs-num)
                       (let [impl-sig (str "(" (reduce str "" (repeat ?needs-num arg-sig)) ")" arg-sig)]
                         (doto *writer*
                           (-> (do (compile arg))
                               (->> (doseq [arg (take ?needs-num ?args)])))
                           (.visitMethodInsn Opcodes/INVOKESTATIC call-class "impl" impl-sig)
                           (-> (doto (do (compile arg))
                                 (.visitMethodInsn Opcodes/INVOKEINTERFACE (->class +function-class+) "apply" +apply-signature+))
                               (->> (doseq [arg (drop ?needs-num ?args)])))))
                       (let [counter-sig "I"
                             init-signature (str "(" (apply str counter-sig (repeat (dec ?needs-num) arg-sig)) ")" "V")]
                         (doto *writer*
                           (.visitTypeInsn Opcodes/NEW call-class)
                           (.visitInsn Opcodes/DUP)
                           (.visitLdcInsn (int provides-num))
                           (-> (do (compile arg))
                               (->> (doseq [arg ?args])))
                           (add-nulls (dec (- ?needs-num provides-num)))
                           (.visitMethodInsn Opcodes/INVOKESPECIAL call-class "<init>" init-signature)))
                       ))
                   )]]
    (return nil)))

(defn ^:private compile-do [compile *type* ?exprs]
  (exec [*writer* &util/get-writer
         _ (map-m (fn [expr]
                    (exec [ret (compile expr)
                           :let [_ (.visitInsn *writer* Opcodes/POP)]]
                      (return ret)))
                  (butlast ?exprs))
         _ (compile (last ?exprs))]
    (return nil)))

(defn ^:private compile-field [compile *type* ?name body]
  (exec [*writer* &util/get-writer
         module-name &analyser/module-name
         :let [outer-class (->class module-name)
               datum-sig (->type-signature "java.lang.Object")
               current-class (&host/location (list ?name outer-class))
               _ (.visitInnerClass *writer* current-class outer-class nil (+ Opcodes/ACC_STATIC Opcodes/ACC_SYNTHETIC))
               =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                        (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
                                current-class nil "java/lang/Object" (into-array [(->class +function-class+)]))
                        (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_datum" datum-sig nil nil)
                            (doto (.visitEnd))))]
         _ (&util/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
             (exec [*writer* &util/get-writer
                    :let [_ (.visitCode *writer*)]
                    _ (compile body)
                    :let [_ (doto *writer*
                              (.visitFieldInsn Opcodes/PUTSTATIC current-class "_datum" datum-sig)
                              (.visitInsn Opcodes/RETURN)
                              (.visitMaxs 0 0)
                              (.visitEnd))]]
               (return nil)))
         :let [_ (.visitEnd *writer*)]
         _ (save-class! current-class (.toByteArray =class))]
    (return nil)))

(defn ^:private compile-def [compile *type* name value]
  (match value
    [::&analyser/Expression ?form _]
    (match ?form
      [::&analyser/lambda ?scope ?captured ?args ?body]
      (compile-lambda compile *type* ?scope ?closure ?args ?body true false)

      _
      (compile-field compile *type* name value))
    
    _
    (fail "Can only define expressions.")))

(defn ^:private compile-self-call [compile ?assumed-args]
  (exec [*writer* &util/get-writer
         :let [_ (.visitVarInsn *writer* Opcodes/ALOAD 0)]
         _ (map-m (fn [arg]
                    (exec [ret (compile arg)
                           :let [_ (.visitMethodInsn *writer* Opcodes/INVOKEINTERFACE (->class +function-class+) "apply" +apply-signature+)]]
                      (return ret)))
                  ?assumed-args)]
    (return nil)))

(defn ^:private compile-expression [syntax]
  (match (:form syntax)
    [::&analyser/bool ?value]
    (compile-bool compile-expression (:type syntax) ?value)

    [::&analyser/int ?value]
    (compile-int compile-expression (:type syntax) ?value)

    [::&analyser/real ?value]
    (compile-real compile-expression (:type syntax) ?value)

    [::&analyser/char ?value]
    (compile-char compile-expression (:type syntax) ?value)

    [::&analyser/text ?value]
    (compile-text compile-expression (:type syntax) ?value)

    [::&analyser/tuple ?elems]
    (compile-tuple compile-expression (:type syntax) ?elems)

    [::&analyser/local ?idx]
    (compile-local compile-expression (:type syntax) ?idx)

    [::&analyser/captured ?scope ?captured-id ?source]
    (compile-captured compile-expression (:type syntax) ?scope ?captured-id ?source)

    [::&analyser/global ?owner-class ?name]
    (compile-global compile-expression (:type syntax) ?owner-class ?name)

    [::&analyser/call ?fn ?args]
    (compile-call compile-expression (:type syntax) ?fn ?args)

    [::&analyser/static-call ?needs-num ?fn ?args]
    (compile-static-call compile-expression (:type syntax) ?needs-num ?fn ?args)

    [::&analyser/variant ?tag ?members]
    (compile-variant compile-expression (:type syntax) ?tag ?members)

    [::&analyser/case ?variant ?base-register ?num-registers ?branches]
    (compile-case compile-expression (:type syntax) ?variant ?base-register ?num-registers ?branches)

    [::&analyser/lambda ?scope ?frame ?args ?body]
    (compile-lambda compile-expression (:type syntax) ?scope ?frame ?args ?body false true)

    ;; Integer arithmetic
    [::&analyser/jvm-iadd ?x ?y]
    (compile-jvm-iadd compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-isub ?x ?y]
    (compile-jvm-isub compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-imul ?x ?y]
    (compile-jvm-imul compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-idiv ?x ?y]
    (compile-jvm-idiv compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-irem ?x ?y]
    (compile-jvm-irem compile-expression (:type syntax) ?x ?y)

    ;; Long arithmetic
    [::&analyser/jvm-ladd ?x ?y]
    (compile-jvm-ladd compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-lsub ?x ?y]
    (compile-jvm-lsub compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-lmul ?x ?y]
    (compile-jvm-lmul compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-ldiv ?x ?y]
    (compile-jvm-ldiv compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-lrem ?x ?y]
    (compile-jvm-lrem compile-expression (:type syntax) ?x ?y)

    ;; Float arithmetic
    [::&analyser/jvm-fadd ?x ?y]
    (compile-jvm-fadd compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-fsub ?x ?y]
    (compile-jvm-fsub compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-fmul ?x ?y]
    (compile-jvm-fmul compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-fdiv ?x ?y]
    (compile-jvm-fdiv compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-frem ?x ?y]
    (compile-jvm-frem compile-expression (:type syntax) ?x ?y)

    ;; Double arithmetic
    [::&analyser/jvm-dadd ?x ?y]
    (compile-jvm-dadd compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-dsub ?x ?y]
    (compile-jvm-dsub compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-dmul ?x ?y]
    (compile-jvm-dmul compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-ddiv ?x ?y]
    (compile-jvm-ddiv compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/jvm-drem ?x ?y]
    (compile-jvm-drem compile-expression (:type syntax) ?x ?y)
    
    [::&analyser/do ?exprs]
    (compile-do compile-expression (:type syntax) ?exprs)

    [::&analyser/jvm-new ?class ?classes ?args]
    (compile-jvm-new compile-expression (:type syntax) ?class ?classes ?args)

    [::&analyser/jvm-getstatic ?class ?field]
    (compile-jvm-getstatic compile-expression (:type syntax) ?class ?field)

    [::&analyser/jvm-getfield ?class ?field ?object]
    (compile-jvm-getfield compile-expression (:type syntax) ?class ?field ?object)
    
    [::&analyser/jvm-invokestatic ?class ?method ?classes ?args]
    (compile-jvm-invokestatic compile-expression (:type syntax) ?class ?method ?classes ?args)

    [::&analyser/jvm-invokevirtual ?class ?method ?classes ?object ?args]
    (compile-jvm-invokevirtual compile-expression (:type syntax) ?class ?method ?classes ?object ?args)

    [::&analyser/jvm-new-array ?class ?length]
    (compile-jvm-new-array compile-expression (:type syntax) ?class ?length)

    [::&analyser/jvm-aastore ?array ?idx ?elem]
    (compile-jvm-aastore compile-expression (:type syntax) ?array ?idx ?elem)

    [::&analyser/jvm-aaload ?array ?idx]
    (compile-jvm-aaload compile-expression (:type syntax) ?array ?idx)

    [::&analyser/self ?assumed-args]
    (compile-self-call compile-expression ?assumed-args)

    _
    (fail "[Compiler Error] Can't compile expressions as top-level forms.")
    ))

(defn ^:private compile-statement [syntax]
  (match (:form syntax)
    [::&analyser/def ?form ?body]
    (compile-def compile-expression (:type syntax) ?form ?body)
    
    [::&analyser/jvm-interface [?package ?name] ?members]
    (compile-jvm-interface compile-expression (:type syntax) ?package ?name ?members)

    [::&analyser/jvm-class [?package ?name] ?super-class ?members]
    (compile-jvm-class compile-expression (:type syntax) ?package ?name ?super-class ?members)

    _
    (fail "[Compiler Error] Can't compile expressions as top-level forms.")
    ))

;; [Interface]
(let [compiler-step (exec [analysis+ &analyser/analyse]
                      (map-m compile-statement analysis+))]
  (defn compile-module [name]
    (exec [loader &util/loader]
      (fn [state]
        (if (-> state ::&util/modules (contains? name))
          (fail "[Compiler Error] Can't redefine a module!")
          (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 (->class name) nil "java/lang/Object" nil))]
            (match (&util/run-state (exhaust-m compiler-step) (assoc state
                                                                ::&util/source (slurp (str "source/" name ".lux"))
                                                                ::&util/current-module name
                                                                ::&util/writer =class))
              [::&util/ok [?state _]]
              (do (.visitEnd =class)
                (&util/run-state (save-class! name (.toByteArray =class)) ?state))
              
              [::&util/failure ?message]
              (fail* ?message))))))))

(defn compile-all [modules]
  (.mkdir (java.io.File. "output"))
  (match (&util/run-state (map-m compile-module modules) (&util/init-state))
    [::&util/ok [?state _]]
    (println (str "Compilation complete! " (pr-str modules)))

    [::&util/failure ?message]
    (assert false ?message)))

(comment
  (compile-all ["lux"])
  )
