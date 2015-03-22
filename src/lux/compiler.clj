(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return* return fail fail*]]
                 [type :as &type]
                 [reader :as &reader]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [optimizer :as &optimizer]
                 [host :as &host])
            [lux.analyser.base :as &a]
            [lux.analyser.def :as &a-def]
            (lux.compiler [base :as &&]
                          [lux :as &&lux]
                          [host :as &&host]
                          [case :as &&case]
                          [lambda :as &&lambda])
            ;; :reload
            )
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils/Compilers]
(defn ^:private compile-expression [syntax]
  ;; (prn 'compile-expression (aget syntax 0))
  (matchv ::M/objects [syntax]
    [["Expression" [?form ?type]]]
    (do ;; (prn 'compile-expression2 (aget ?form 0))
        (matchv ::M/objects [?form]
          [["bool" ?value]]
          (&&lux/compile-bool compile-expression ?type ?value)

          [["int" ?value]]
          (&&lux/compile-int compile-expression ?type ?value)

          [["real" ?value]]
          (&&lux/compile-real compile-expression ?type ?value)

          [["char" ?value]]
          (&&lux/compile-char compile-expression ?type ?value)

          [["text" ?value]]
          (&&lux/compile-text compile-expression ?type ?value)

          [["tuple" ?elems]]
          (&&lux/compile-tuple compile-expression ?type ?elems)

          [["record" ?elems]]
          (&&lux/compile-record compile-expression ?type ?elems)

          [["local" ?idx]]
          (&&lux/compile-local compile-expression ?type ?idx)

          [["captured" [?scope ?captured-id ?source]]]
          (&&lux/compile-captured compile-expression ?type ?scope ?captured-id ?source)

          [["global" [?owner-class ?name]]]
          (&&lux/compile-global compile-expression ?type ?owner-class ?name)

          [["apply" [?fn ?arg]]]
          (&&lux/compile-apply compile-expression ?type ?fn ?arg)

          [["variant" [?tag ?members]]]
          (&&lux/compile-variant compile-expression ?type ?tag ?members)

          [["case" [?variant ?base-register ?num-registers ?branches]]]
          (&&case/compile-case compile-expression ?type ?variant ?base-register ?num-registers ?branches)

          [["lambda" [?scope ?env ?args ?body]]]
          (&&lambda/compile-lambda compile-expression ?scope ?env ?args ?body)

          [["get" [?slot ?record]]]
          (&&lux/compile-get compile-expression ?type ?slot ?record)

          [["set" [?slot ?value ?record]]]
          (&&lux/compile-set compile-expression ?type ?slot ?value ?record)

          ;; Integer arithmetic
          [["jvm-iadd" [?x ?y]]]
          (&&host/compile-jvm-iadd compile-expression ?type ?x ?y)

          [["jvm-isub" [?x ?y]]]
          (&&host/compile-jvm-isub compile-expression ?type ?x ?y)
          
          [["jvm-imul" [?x ?y]]]
          (&&host/compile-jvm-imul compile-expression ?type ?x ?y)
          
          [["jvm-idiv" [?x ?y]]]
          (&&host/compile-jvm-idiv compile-expression ?type ?x ?y)
          
          [["jvm-irem" [?x ?y]]]
          (&&host/compile-jvm-irem compile-expression ?type ?x ?y)

          [["jvm-ieq" [?x ?y]]]
          (&&host/compile-jvm-ieq compile-expression ?type ?x ?y)

          [["jvm-ilt" [?x ?y]]]
          (&&host/compile-jvm-ilt compile-expression ?type ?x ?y)

          [["jvm-igt" [?x ?y]]]
          (&&host/compile-jvm-igt compile-expression ?type ?x ?y)

          ;; Long arithmetic
          [["jvm-ladd" [?x ?y]]]
          (&&host/compile-jvm-ladd compile-expression ?type ?x ?y)
          
          [["jvm-lsub" [?x ?y]]]
          (&&host/compile-jvm-lsub compile-expression ?type ?x ?y)
          
          [["jvm-lmul" [?x ?y]]]
          (&&host/compile-jvm-lmul compile-expression ?type ?x ?y)
          
          [["jvm-ldiv" [?x ?y]]]
          (&&host/compile-jvm-ldiv compile-expression ?type ?x ?y)
          
          [["jvm-lrem" [?x ?y]]]
          (&&host/compile-jvm-lrem compile-expression ?type ?x ?y)

          [["jvm-leq" [?x ?y]]]
          (&&host/compile-jvm-leq compile-expression ?type ?x ?y)

          [["jvm-llt" [?x ?y]]]
          (&&host/compile-jvm-llt compile-expression ?type ?x ?y)

          [["jvm-lgt" [?x ?y]]]
          (&&host/compile-jvm-lgt compile-expression ?type ?x ?y)

          ;; Float arithmetic
          [["jvm-fadd" [?x ?y]]]
          (&&host/compile-jvm-fadd compile-expression ?type ?x ?y)
          
          [["jvm-fsub" [?x ?y]]]
          (&&host/compile-jvm-fsub compile-expression ?type ?x ?y)
          
          [["jvm-fmul" [?x ?y]]]
          (&&host/compile-jvm-fmul compile-expression ?type ?x ?y)
          
          [["jvm-fdiv" [?x ?y]]]
          (&&host/compile-jvm-fdiv compile-expression ?type ?x ?y)
          
          [["jvm-frem" [?x ?y]]]
          (&&host/compile-jvm-frem compile-expression ?type ?x ?y)

          [["jvm-feq" [?x ?y]]]
          (&&host/compile-jvm-feq compile-expression ?type ?x ?y)

          [["jvm-flt" [?x ?y]]]
          (&&host/compile-jvm-flt compile-expression ?type ?x ?y)

          [["jvm-fgt" [?x ?y]]]
          (&&host/compile-jvm-fgt compile-expression ?type ?x ?y)

          ;; Double arithmetic
          [["jvm-dadd" [?x ?y]]]
          (&&host/compile-jvm-dadd compile-expression ?type ?x ?y)
          
          [["jvm-dsub" [?x ?y]]]
          (&&host/compile-jvm-dsub compile-expression ?type ?x ?y)
          
          [["jvm-dmul" [?x ?y]]]
          (&&host/compile-jvm-dmul compile-expression ?type ?x ?y)
          
          [["jvm-ddiv" [?x ?y]]]
          (&&host/compile-jvm-ddiv compile-expression ?type ?x ?y)
          
          [["jvm-drem" [?x ?y]]]
          (&&host/compile-jvm-drem compile-expression ?type ?x ?y)

          [["jvm-deq" [?x ?y]]]
          (&&host/compile-jvm-deq compile-expression ?type ?x ?y)

          [["jvm-dlt" [?x ?y]]]
          (&&host/compile-jvm-dlt compile-expression ?type ?x ?y)

          [["jvm-dgt" [?x ?y]]]
          (&&host/compile-jvm-dgt compile-expression ?type ?x ?y)
          
          [["exec" ?exprs]]
          (&&host/compile-exec compile-expression ?type ?exprs)

          [["jvm-null" _]]
          (&&host/compile-jvm-null compile-expression ?type)

          [["jvm-null?" ?object]]
          (&&host/compile-jvm-null? compile-expression ?type ?object)
          
          [["jvm-new" [?class ?classes ?args]]]
          (&&host/compile-jvm-new compile-expression ?type ?class ?classes ?args)

          [["jvm-getstatic" [?class ?field]]]
          (&&host/compile-jvm-getstatic compile-expression ?type ?class ?field)

          [["jvm-getfield" [?class ?field ?object]]]
          (&&host/compile-jvm-getfield compile-expression ?type ?class ?field ?object)

          [["jvm-putstatic" [?class ?field ?value]]]
          (&&host/compile-jvm-putstatic compile-expression ?type ?class ?field ?value)

          [["jvm-putfield" [?class ?field ?object ?value]]]
          (&&host/compile-jvm-putfield compile-expression ?type ?class ?field ?object ?value)

          [["jvm-invokestatic" [?class ?method ?classes ?args]]]
          (&&host/compile-jvm-invokestatic compile-expression ?type ?class ?method ?classes ?args)

          [["jvm-invokevirtual" [?class ?method ?classes ?object ?args]]]
          (&&host/compile-jvm-invokevirtual compile-expression ?type ?class ?method ?classes ?object ?args)

          [["jvm-invokeinterface" [?class ?method ?classes ?object ?args]]]
          (&&host/compile-jvm-invokeinterface compile-expression ?type ?class ?method ?classes ?object ?args)

          [["jvm-invokespecial" [?class ?method ?classes ?object ?args]]]
          (&&host/compile-jvm-invokespecial compile-expression ?type ?class ?method ?classes ?object ?args)
          
          [["jvm-new-array" [?class ?length]]]
          (&&host/compile-jvm-new-array compile-expression ?type ?class ?length)

          [["jvm-aastore" [?array ?idx ?elem]]]
          (&&host/compile-jvm-aastore compile-expression ?type ?array ?idx ?elem)

          [["jvm-aaload" [?array ?idx]]]
          (&&host/compile-jvm-aaload compile-expression ?type ?array ?idx)

          [["jvm-try" [?body ?catches ?finally]]]
          (&&host/compile-jvm-try compile-expression ?type ?body ?catches ?finally)

          [["jvm-throw" ?ex]]
          (&&host/compile-jvm-throw compile-expression ?type ?ex)

          [["jvm-monitorenter" ?monitor]]
          (&&host/compile-jvm-monitorenter compile-expression ?type ?monitor)

          [["jvm-monitorexit" ?monitor]]
          (&&host/compile-jvm-monitorexit compile-expression ?type ?monitor)

          [["jvm-d2f" ?value]]
          (&&host/compile-jvm-d2f compile-expression ?type ?value)

          [["jvm-d2i" ?value]]
          (&&host/compile-jvm-d2i compile-expression ?type ?value)

          [["jvm-d2l" ?value]]
          (&&host/compile-jvm-d2l compile-expression ?type ?value)
          
          [["jvm-f2d" ?value]]
          (&&host/compile-jvm-f2d compile-expression ?type ?value)

          [["jvm-f2i" ?value]]
          (&&host/compile-jvm-f2i compile-expression ?type ?value)

          [["jvm-f2l" ?value]]
          (&&host/compile-jvm-f2l compile-expression ?type ?value)
          
          [["jvm-i2b" ?value]]
          (&&host/compile-jvm-i2b compile-expression ?type ?value)

          [["jvm-i2c" ?value]]
          (&&host/compile-jvm-i2c compile-expression ?type ?value)

          [["jvm-i2d" ?value]]
          (&&host/compile-jvm-i2d compile-expression ?type ?value)

          [["jvm-i2f" ?value]]
          (&&host/compile-jvm-i2f compile-expression ?type ?value)

          [["jvm-i2l" ?value]]
          (&&host/compile-jvm-i2l compile-expression ?type ?value)

          [["jvm-i2s" ?value]]
          (&&host/compile-jvm-i2s compile-expression ?type ?value)

          [["jvm-l2d" ?value]]
          (&&host/compile-jvm-l2d compile-expression ?type ?value)

          [["jvm-l2f" ?value]]
          (&&host/compile-jvm-l2f compile-expression ?type ?value)

          [["jvm-l2i" ?value]]
          (&&host/compile-jvm-l2i compile-expression ?type ?value)

          [["jvm-iand" [?x ?y]]]
          (&&host/compile-jvm-iand compile-expression ?type ?x ?y)

          [["jvm-ior" [?x ?y]]]
          (&&host/compile-jvm-ior compile-expression ?type ?x ?y)

          [["jvm-land" [?x ?y]]]
          (&&host/compile-jvm-land compile-expression ?type ?x ?y)

          [["jvm-lor" [?x ?y]]]
          (&&host/compile-jvm-lor compile-expression ?type ?x ?y)

          [["jvm-lxor" [?x ?y]]]
          (&&host/compile-jvm-lxor compile-expression ?type ?x ?y)

          [["jvm-lshl" [?x ?y]]]
          (&&host/compile-jvm-lshl compile-expression ?type ?x ?y)

          [["jvm-lshr" [?x ?y]]]
          (&&host/compile-jvm-lshr compile-expression ?type ?x ?y)

          [["jvm-lushr" [?x ?y]]]
          (&&host/compile-jvm-lushr compile-expression ?type ?x ?y)

          [["jvm-program" ?body]]
          (&&host/compile-jvm-program compile-expression ?type ?body)
          ))

    [_]
    (fail "[Compiler Error] Can't compile statements as expressions.")))

(defn ^:private compile-statement [syntax]
  ;; (prn 'compile-statement syntax)
  (matchv ::M/objects [syntax]
    [["Statement" ?form]]
    (do ;; (prn 'compile-statement (aget syntax 0) (aget ?form 0))
        (matchv ::M/objects [?form]
          [["def" [?name ?body]]]
          (&&lux/compile-def compile-expression ?name ?body)
          
          [["jvm-interface" [?package ?name ?methods]]]
          (&&host/compile-jvm-interface compile-expression ?package ?name ?methods)

          [["jvm-class" [?package ?name ?super-class ?fields ?methods]]]
          (&&host/compile-jvm-class compile-expression ?package ?name ?super-class ?fields ?methods)))

    [_]
    (fail "[Compiler Error] Can't compile expressions as top-level forms.")))

(defn ^:private eval! [expr]
  (exec [eval-ctor &/get-eval-ctor
         :let [class-name (str eval-ctor)
               class-file (str class-name ".class")
               =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                        (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                class-name nil "java/lang/Object" nil)
                        (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_eval" "Ljava/lang/Object;" nil nil)
                            (doto (.visitEnd))))]
         _ (&/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
             (exec [*writer* &/get-writer
                    :let [_ (.visitCode *writer*)]
                    _ (compile-expression expr)
                    :let [_ (doto *writer*
                              (.visitFieldInsn Opcodes/PUTSTATIC class-name "_eval" "Ljava/lang/Object;")
                              (.visitInsn Opcodes/RETURN)
                              (.visitMaxs 0 0)
                              (.visitEnd))]]
               (return nil)))
         :let [bytecode (.toByteArray (doto =class
                                        .visitEnd))]
         _ (&&/save-class! class-file bytecode)
         loader &/loader]
    (-> (.loadClass loader class-name)
        (.getField "_eval")
        (.get nil)
        return)))

(let [compiler-step (exec [analysis+ (&optimizer/optimize eval!)
                           ;; :let [_ (prn 'analysis+ analysis+)]
                           ]
                      (&/map% compile-statement analysis+)
                      ;; (if (&/|empty? analysis+)
                      ;;   (fail "[Compiler Error] No more to compile.")
                      ;;   (&/map% compile-statement analysis+))
                      )]
  (defn ^:private compile-module [name]
    (fn [state]
      (if (->> state (&/get$ "lux;modules") (&/|contains? name))
        (fail "[Compiler Error] Can't redefine a module!")
        (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                               (&host/->class name) nil "java/lang/Object" nil))]
          (matchv ::M/objects [(&/run-state (&/exhaust% compiler-step) (->> state
                                                                            (&/set$ "lux;source" (&/V "lux;Some" (&reader/from (str "source/" name ".lux"))))
                                                                            (&/set$ "lux;global-env" (&/V "lux;Some" (&/env name)))
                                                                            (&/set$ "lux;writer" (&/V "lux;Some" =class))
                                                                            (&/update$ "lux;modules" #(&/|put name &a-def/init-module %))))]
            [["lux;Right" [?state _]]]
            (do (.visitEnd =class)
              ;; (prn 'compile-module 'DONE name)
              ;; (prn 'compile-module/?vals ?vals)
              (&/run-state (&&/save-class! name (.toByteArray =class)) ?state))
            
            [["lux;Left" ?message]]
            (fail* ?message)))))))

;; [Resources]
(defn compile-all [modules]
  (.mkdir (java.io.File. "output"))
  (matchv ::M/objects [(&/run-state (&/map% compile-module modules) (&/init-state nil))]
    [["lux;Right" [?state _]]]
    (println (str "Compilation complete! " (str "[" (->> modules
                                                         (&/|interpose " ")
                                                         (&/fold str ""))
                                                "]")))

    [["lux;Left" ?message]]
    (do (prn 'compile-all '?message ?message)
      (assert false ?message))))

(comment
  (compile-all ["lux"])
  )
