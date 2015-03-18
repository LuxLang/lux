(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return* return fail fail* assert!
                                     repeat% exhaust% try% try-all% map% flat-map% fold% sequence%
                                     apply%
                                     normalize-ident
                                     |get |list]]
                 [type :as &type]
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
            :reload)
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils/Compilers]
(defn ^:private compile-expression [syntax]
  ;; (prn 'compile-expression syntax)
  (match syntax
    [::&a/Expression ?form ?type]
    (match ?form
      [::&a/bool ?value]
      (&&lux/compile-bool compile-expression ?type ?value)

      [::&a/int ?value]
      (&&lux/compile-int compile-expression ?type ?value)

      [::&a/real ?value]
      (&&lux/compile-real compile-expression ?type ?value)

      [::&a/char ?value]
      (&&lux/compile-char compile-expression ?type ?value)

      [::&a/text ?value]
      (&&lux/compile-text compile-expression ?type ?value)

      [::&a/tuple ?elems]
      (&&lux/compile-tuple compile-expression ?type ?elems)

      [::&a/record ?elems]
      (&&lux/compile-record compile-expression ?type ?elems)

      [::&a/local ?idx]
      (&&lux/compile-local compile-expression ?type ?idx)

      [::&a/captured ?scope ?captured-id ?source]
      (&&lux/compile-captured compile-expression ?type ?scope ?captured-id ?source)

      [::&a/global ?owner-class ?name]
      (&&lux/compile-global compile-expression ?type ?owner-class ?name)

      [::&a/call ?fn ?args]
      (&&lux/compile-call compile-expression ?type ?fn ?args)

      [::&a/variant ?tag ?members]
      (&&lux/compile-variant compile-expression ?type ?tag ?members)

      [::&a/case ?variant ?base-register ?num-registers ?branches]
      (&&case/compile-case compile-expression ?type ?variant ?base-register ?num-registers ?branches)

      [::&a/lambda ?scope ?env ?args ?body]
      (&&lambda/compile-lambda compile-expression ?scope ?env ?args ?body)

      [::&a/get ?slot ?record]
      (&&lux/compile-get compile-expression ?type ?slot ?record)

      [::&a/set ?slot ?value ?record]
      (&&lux/compile-set compile-expression ?type ?slot ?value ?record)

      ;; Integer arithmetic
      [::&a/jvm-iadd ?x ?y]
      (&&host/compile-jvm-iadd compile-expression ?type ?x ?y)

      [::&a/jvm-isub ?x ?y]
      (&&host/compile-jvm-isub compile-expression ?type ?x ?y)
      
      [::&a/jvm-imul ?x ?y]
      (&&host/compile-jvm-imul compile-expression ?type ?x ?y)
      
      [::&a/jvm-idiv ?x ?y]
      (&&host/compile-jvm-idiv compile-expression ?type ?x ?y)
      
      [::&a/jvm-irem ?x ?y]
      (&&host/compile-jvm-irem compile-expression ?type ?x ?y)

      [::&a/jvm-ieq ?x ?y]
      (&&host/compile-jvm-ieq compile-expression ?type ?x ?y)

      [::&a/jvm-ilt ?x ?y]
      (&&host/compile-jvm-ilt compile-expression ?type ?x ?y)

      [::&a/jvm-igt ?x ?y]
      (&&host/compile-jvm-igt compile-expression ?type ?x ?y)

      ;; Long arithmetic
      [::&a/jvm-ladd ?x ?y]
      (&&host/compile-jvm-ladd compile-expression ?type ?x ?y)
      
      [::&a/jvm-lsub ?x ?y]
      (&&host/compile-jvm-lsub compile-expression ?type ?x ?y)
      
      [::&a/jvm-lmul ?x ?y]
      (&&host/compile-jvm-lmul compile-expression ?type ?x ?y)
      
      [::&a/jvm-ldiv ?x ?y]
      (&&host/compile-jvm-ldiv compile-expression ?type ?x ?y)
      
      [::&a/jvm-lrem ?x ?y]
      (&&host/compile-jvm-lrem compile-expression ?type ?x ?y)

      [::&a/jvm-leq ?x ?y]
      (&&host/compile-jvm-leq compile-expression ?type ?x ?y)

      [::&a/jvm-llt ?x ?y]
      (&&host/compile-jvm-llt compile-expression ?type ?x ?y)

      [::&a/jvm-lgt ?x ?y]
      (&&host/compile-jvm-lgt compile-expression ?type ?x ?y)

      ;; Float arithmetic
      [::&a/jvm-fadd ?x ?y]
      (&&host/compile-jvm-fadd compile-expression ?type ?x ?y)
      
      [::&a/jvm-fsub ?x ?y]
      (&&host/compile-jvm-fsub compile-expression ?type ?x ?y)
      
      [::&a/jvm-fmul ?x ?y]
      (&&host/compile-jvm-fmul compile-expression ?type ?x ?y)
      
      [::&a/jvm-fdiv ?x ?y]
      (&&host/compile-jvm-fdiv compile-expression ?type ?x ?y)
      
      [::&a/jvm-frem ?x ?y]
      (&&host/compile-jvm-frem compile-expression ?type ?x ?y)

      [::&a/jvm-feq ?x ?y]
      (&&host/compile-jvm-feq compile-expression ?type ?x ?y)

      [::&a/jvm-flt ?x ?y]
      (&&host/compile-jvm-flt compile-expression ?type ?x ?y)

      [::&a/jvm-fgt ?x ?y]
      (&&host/compile-jvm-fgt compile-expression ?type ?x ?y)

      ;; Double arithmetic
      [::&a/jvm-dadd ?x ?y]
      (&&host/compile-jvm-dadd compile-expression ?type ?x ?y)
      
      [::&a/jvm-dsub ?x ?y]
      (&&host/compile-jvm-dsub compile-expression ?type ?x ?y)
      
      [::&a/jvm-dmul ?x ?y]
      (&&host/compile-jvm-dmul compile-expression ?type ?x ?y)
      
      [::&a/jvm-ddiv ?x ?y]
      (&&host/compile-jvm-ddiv compile-expression ?type ?x ?y)
      
      [::&a/jvm-drem ?x ?y]
      (&&host/compile-jvm-drem compile-expression ?type ?x ?y)

      [::&a/jvm-deq ?x ?y]
      (&&host/compile-jvm-deq compile-expression ?type ?x ?y)

      [::&a/jvm-dlt ?x ?y]
      (&&host/compile-jvm-dlt compile-expression ?type ?x ?y)

      [::&a/jvm-dgt ?x ?y]
      (&&host/compile-jvm-dgt compile-expression ?type ?x ?y)
      
      [::&a/exec ?exprs]
      (&&host/compile-exec compile-expression ?type ?exprs)

      [::&a/jvm-null]
      (&&host/compile-jvm-null compile-expression ?type)

      [::&a/jvm-null? ?object]
      (&&host/compile-jvm-null? compile-expression ?type ?object)
      
      [::&a/jvm-new ?class ?classes ?args]
      (&&host/compile-jvm-new compile-expression ?type ?class ?classes ?args)

      [::&a/jvm-getstatic ?class ?field]
      (&&host/compile-jvm-getstatic compile-expression ?type ?class ?field)

      [::&a/jvm-getfield ?class ?field ?object]
      (&&host/compile-jvm-getfield compile-expression ?type ?class ?field ?object)

      [::&a/jvm-putstatic ?class ?field ?value]
      (&&host/compile-jvm-putstatic compile-expression ?type ?class ?field ?value)

      [::&a/jvm-putfield ?class ?field ?object ?value]
      (&&host/compile-jvm-putfield compile-expression ?type ?class ?field ?object ?value)

      [::&a/jvm-invokestatic ?class ?method ?classes ?args]
      (&&host/compile-jvm-invokestatic compile-expression ?type ?class ?method ?classes ?args)

      [::&a/jvm-invokevirtual ?class ?method ?classes ?object ?args]
      (&&host/compile-jvm-invokevirtual compile-expression ?type ?class ?method ?classes ?object ?args)

      [::&a/jvm-invokeinterface ?class ?method ?classes ?object ?args]
      (&&host/compile-jvm-invokeinterface compile-expression ?type ?class ?method ?classes ?object ?args)

      [::&a/jvm-invokespecial ?class ?method ?classes ?object ?args]
      (&&host/compile-jvm-invokespecial compile-expression ?type ?class ?method ?classes ?object ?args)
      
      [::&a/jvm-new-array ?class ?length]
      (&&host/compile-jvm-new-array compile-expression ?type ?class ?length)

      [::&a/jvm-aastore ?array ?idx ?elem]
      (&&host/compile-jvm-aastore compile-expression ?type ?array ?idx ?elem)

      [::&a/jvm-aaload ?array ?idx]
      (&&host/compile-jvm-aaload compile-expression ?type ?array ?idx)

      [::&a/jvm-try ?body ?catches ?finally]
      (&&host/compile-jvm-try compile-expression ?type ?body ?catches ?finally)

      [::&a/jvm-throw ?ex]
      (&&host/compile-jvm-throw compile-expression ?type ?ex)

      [::&a/jvm-monitorenter ?monitor]
      (&&host/compile-jvm-monitorenter compile-expression ?type ?monitor)

      [::&a/jvm-monitorexit ?monitor]
      (&&host/compile-jvm-monitorexit compile-expression ?type ?monitor)

      [::&a/jvm-d2f ?value]
      (&&host/compile-jvm-d2f compile-expression ?type ?value)

      [::&a/jvm-d2i ?value]
      (&&host/compile-jvm-d2i compile-expression ?type ?value)

      [::&a/jvm-d2l ?value]
      (&&host/compile-jvm-d2l compile-expression ?type ?value)
      
      [::&a/jvm-f2d ?value]
      (&&host/compile-jvm-f2d compile-expression ?type ?value)

      [::&a/jvm-f2i ?value]
      (&&host/compile-jvm-f2i compile-expression ?type ?value)

      [::&a/jvm-f2l ?value]
      (&&host/compile-jvm-f2l compile-expression ?type ?value)
      
      [::&a/jvm-i2b ?value]
      (&&host/compile-jvm-i2b compile-expression ?type ?value)

      [::&a/jvm-i2c ?value]
      (&&host/compile-jvm-i2c compile-expression ?type ?value)

      [::&a/jvm-i2d ?value]
      (&&host/compile-jvm-i2d compile-expression ?type ?value)

      [::&a/jvm-i2f ?value]
      (&&host/compile-jvm-i2f compile-expression ?type ?value)

      [::&a/jvm-i2l ?value]
      (&&host/compile-jvm-i2l compile-expression ?type ?value)

      [::&a/jvm-i2s ?value]
      (&&host/compile-jvm-i2s compile-expression ?type ?value)

      [::&a/jvm-l2d ?value]
      (&&host/compile-jvm-l2d compile-expression ?type ?value)

      [::&a/jvm-l2f ?value]
      (&&host/compile-jvm-l2f compile-expression ?type ?value)

      [::&a/jvm-l2i ?value]
      (&&host/compile-jvm-l2i compile-expression ?type ?value)

      [::&a/jvm-iand ?x y]
      (&&host/compile-jvm-iand compile-expression ?type ?x y)

      [::&a/jvm-ior ?x y]
      (&&host/compile-jvm-ior compile-expression ?type ?x y)

      [::&a/jvm-land ?x y]
      (&&host/compile-jvm-land compile-expression ?type ?x y)

      [::&a/jvm-lor ?x y]
      (&&host/compile-jvm-lor compile-expression ?type ?x y)

      [::&a/jvm-lxor ?x y]
      (&&host/compile-jvm-lxor compile-expression ?type ?x y)

      [::&a/jvm-lshl ?x y]
      (&&host/compile-jvm-lshl compile-expression ?type ?x y)

      [::&a/jvm-lshr ?x y]
      (&&host/compile-jvm-lshr compile-expression ?type ?x y)

      [::&a/jvm-lushr ?x y]
      (&&host/compile-jvm-lushr compile-expression ?type ?x y)

      [::&a/jvm-program ?body]
      (&&host/compile-jvm-program compile-expression ?type ?body)
      )

    _
    (fail "[Compiler Error] Can't compile statements as expressions.")))

(defn ^:private compile-statement [syntax]
  ;; (prn 'compile-statement syntax)
  (match syntax
    [::&a/Statement ?form]
    (match ?form
      [::&a/def ?name ?body]
      (&&lux/compile-def compile-expression ?name ?body)
      
      [::&a/jvm-interface ?package ?name ?methods]
      (&&host/compile-jvm-interface compile-expression ?package ?name ?methods)

      [::&a/jvm-class ?package ?name ?super-class ?fields ?methods]
      (&&host/compile-jvm-class compile-expression ?package ?name ?super-class ?fields ?methods))

    _
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
                      (flat-map% compile-statement analysis+))]
  (defn ^:private compile-module [name]
    (fn [state]
      (if (->> state (get$ "modules") (|contains? name))
        (fail "[Compiler Error] Can't redefine a module!")
        (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                       (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                               (&host/->class name) nil "java/lang/Object" nil))]
          (matchv ::M/objects [(&/run-state (exhaust% compiler-step) (-> state
                                                                         (set$ "source" (slurp (str "source/" name ".lux")))
                                                                         (set$ "global-env" (&/env name))
                                                                         (set$ "writer" =class)
                                                                         (update$ "modules" #(|put name &a-def/init-module %))))]
            [["Right" [?state ?vals]]]
            (do (.visitEnd =class)
              ;; (prn 'compile-module/?vals ?vals)
              (&/run-state (&&/save-class! name (.toByteArray =class)) ?state))
            
            [["Left" ?message]]
            (fail* ?message)))))))

;; [Resources]
(defn compile-all [modules]
  (.mkdir (java.io.File. "output"))
  (matchv ::M/objects [(&/run-state (map% compile-module modules) (&/init-state))]
    [["Right" [?state _]]]
    (println (str "Compilation complete! " (pr-str modules)))

    [["Left" ?message]]
    (do (prn 'compile-all '?message ?message)
      (assert false ?message))))

(comment
  (compile-all ["lux"])
  )
