(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :refer [match]]
            (lux [util :as &util :refer [exec return* return fail fail*
                                         repeat-m exhaust-m try-m try-all-m map-m reduce-m
                                         apply-m
                                         normalize-ident]]
                 [type :as &type]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [host :as &host])
            [lux.analyser.base :as &a]
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

      [::&a/local ?idx]
      (&&lux/compile-local compile-expression ?type ?idx)

      [::&a/captured ?scope ?captured-id ?source]
      (&&lux/compile-captured compile-expression ?type ?scope ?captured-id ?source)

      [::&a/global ?owner-class ?name]
      (&&lux/compile-global compile-expression ?type ?owner-class ?name)

      [::&a/call ?fn ?args]
      (&&lux/compile-call compile-expression ?type ?fn ?args)

      [::&a/static-call ?needs-num ?fn ?args]
      (&&lux/compile-static-call compile-expression ?type ?needs-num ?fn ?args)

      [::&a/variant ?tag ?members]
      (&&lux/compile-variant compile-expression ?type ?tag ?members)

      [::&a/case ?variant ?base-register ?num-registers ?branches]
      (&&case/compile-case compile-expression ?type ?variant ?base-register ?num-registers ?branches)

      [::&a/lambda ?scope ?frame ?args ?body]
      (&&lambda/compile-lambda compile-expression ?type ?scope ?frame ?args ?body false true)

      [::&a/self ?assumed-args]
      (&&lux/compile-self-call compile-expression ?assumed-args)

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
      
      [::&a/exec ?exprs]
      (&&host/compile-exec compile-expression ?type ?exprs)

      [::&a/jvm-new ?class ?classes ?args]
      (&&host/compile-jvm-new compile-expression ?type ?class ?classes ?args)

      [::&a/jvm-getstatic ?class ?field]
      (&&host/compile-jvm-getstatic compile-expression ?type ?class ?field)

      [::&a/jvm-getfield ?class ?field ?object]
      (&&host/compile-jvm-getfield compile-expression ?type ?class ?field ?object)
      
      [::&a/jvm-invokestatic ?class ?method ?classes ?args]
      (&&host/compile-jvm-invokestatic compile-expression ?type ?class ?method ?classes ?args)

      [::&a/jvm-invokevirtual ?class ?method ?classes ?object ?args]
      (&&host/compile-jvm-invokevirtual compile-expression ?type ?class ?method ?classes ?object ?args)

      [::&a/jvm-new-array ?class ?length]
      (&&host/compile-jvm-new-array compile-expression ?type ?class ?length)

      [::&a/jvm-aastore ?array ?idx ?elem]
      (&&host/compile-jvm-aastore compile-expression ?type ?array ?idx ?elem)

      [::&a/jvm-aaload ?array ?idx]
      (&&host/compile-jvm-aaload compile-expression ?type ?array ?idx)

      _
      (fail "[Compiler Error] Can't compile expressions as top-level forms.")
      )))

(defn ^:private compile-statement [syntax]
  (match syntax
    [::&a/Expression ?form ?type]
    (match ?form
      [::&a/def ?form ?body]
      (&&lux/compile-def compile-expression ?type ?form ?body)
      
      [::&a/jvm-interface [?package ?name] ?members]
      (&&host/compile-jvm-interface compile-expression ?type ?package ?name ?members)

      [::&a/jvm-class [?package ?name] ?super-class ?members]
      (&&host/compile-jvm-class compile-expression ?type ?package ?name ?super-class ?members)

      _
      (fail "[Compiler Error] Can't compile expressions as top-level forms.")
      )))

(let [compiler-step (exec [analysis+ &analyser/analyse]
                      (map-m compile-statement analysis+))]
  (defn ^:private compile-module [name]
    (exec [loader &util/loader]
      (fn [state]
        (if (-> state ::&util/modules (contains? name))
          (fail "[Compiler Error] Can't redefine a module!")
          (let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 (&host/->class name) nil "java/lang/Object" nil))]
            (match (&util/run-state (exhaust-m compiler-step) (assoc state
                                                                ::&util/source (slurp (str "source/" name ".lux"))
                                                                ::&util/current-module name
                                                                ::&util/writer =class))
              [::&util/ok [?state _]]
              (do (.visitEnd =class)
                (&util/run-state (&&/save-class! name (.toByteArray =class)) ?state))
              
              [::&util/failure ?message]
              (fail* ?message))))))))

;; [Resources]
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
