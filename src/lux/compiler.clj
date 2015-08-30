;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail* |case]]
                 [type :as &type]
                 [reader :as &reader]
                 [lexer :as &lexer]
                 [parser :as &parser]
                 [analyser :as &analyser]
                 [optimizer :as &optimizer]
                 [host :as &host])
            [lux.analyser.base :as &a]
            [lux.analyser.module :as &a-module]
            (lux.compiler [base :as &&]
                          [cache :as &&cache]
                          [lux :as &&lux]
                          [host :as &&host]
                          [case :as &&case]
                          [lambda :as &&lambda]
                          [package :as &&package]
                          [module :as &&module]
                          [io :as &&io]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils/Compilers]
(defn ^:private compile-expression [syntax]
  (|let [[?form ?type] syntax]
    (|case ?form
      (&a/$bool ?value)
      (&&lux/compile-bool compile-expression ?type ?value)

      (&a/$int ?value)
      (&&lux/compile-int compile-expression ?type ?value)

      (&a/$real ?value)
      (&&lux/compile-real compile-expression ?type ?value)

      (&a/$char ?value)
      (&&lux/compile-char compile-expression ?type ?value)

      (&a/$text ?value)
      (&&lux/compile-text compile-expression ?type ?value)

      (&a/$tuple ?elems)
      (&&lux/compile-tuple compile-expression ?type ?elems)

      (&a/$var (&/$Local ?idx))
      (&&lux/compile-local compile-expression ?type ?idx)

      (&a/$captured ?scope ?captured-id ?source)
      (&&lux/compile-captured compile-expression ?type ?scope ?captured-id ?source)

      (&a/$var (&/$Global ?owner-class ?name))
      (&&lux/compile-global compile-expression ?type ?owner-class ?name)

      (&a/$apply ?fn ?args)
      (&&lux/compile-apply compile-expression ?type ?fn ?args)

      (&a/$variant ?tag ?members)
      (&&lux/compile-variant compile-expression ?type ?tag ?members)

      (&a/$case ?value ?match)
      (&&case/compile-case compile-expression ?type ?value ?match)

      (&a/$lambda ?scope ?env ?body)
      (&&lambda/compile-lambda compile-expression ?scope ?env ?body)

      (&a/$ann ?value-ex ?type-ex)
      (&&lux/compile-ann compile-expression ?type ?value-ex ?type-ex)

      ;; Characters
      (&a/$jvm-ceq ?x ?y)
      (&&host/compile-jvm-ceq compile-expression ?type ?x ?y)

      (&a/$jvm-clt ?x ?y)
      (&&host/compile-jvm-clt compile-expression ?type ?x ?y)

      (&a/$jvm-cgt ?x ?y)
      (&&host/compile-jvm-cgt compile-expression ?type ?x ?y)
      
      ;; Integer arithmetic
      (&a/$jvm-iadd ?x ?y)
      (&&host/compile-jvm-iadd compile-expression ?type ?x ?y)

      (&a/$jvm-isub ?x ?y)
      (&&host/compile-jvm-isub compile-expression ?type ?x ?y)
      
      (&a/$jvm-imul ?x ?y)
      (&&host/compile-jvm-imul compile-expression ?type ?x ?y)
      
      (&a/$jvm-idiv ?x ?y)
      (&&host/compile-jvm-idiv compile-expression ?type ?x ?y)
      
      (&a/$jvm-irem ?x ?y)
      (&&host/compile-jvm-irem compile-expression ?type ?x ?y)

      (&a/$jvm-ieq ?x ?y)
      (&&host/compile-jvm-ieq compile-expression ?type ?x ?y)

      (&a/$jvm-ilt ?x ?y)
      (&&host/compile-jvm-ilt compile-expression ?type ?x ?y)

      (&a/$jvm-igt ?x ?y)
      (&&host/compile-jvm-igt compile-expression ?type ?x ?y)

      ;; Long arithmetic
      (&a/$jvm-ladd ?x ?y)
      (&&host/compile-jvm-ladd compile-expression ?type ?x ?y)
      
      (&a/$jvm-lsub ?x ?y)
      (&&host/compile-jvm-lsub compile-expression ?type ?x ?y)
      
      (&a/$jvm-lmul ?x ?y)
      (&&host/compile-jvm-lmul compile-expression ?type ?x ?y)
      
      (&a/$jvm-ldiv ?x ?y)
      (&&host/compile-jvm-ldiv compile-expression ?type ?x ?y)
      
      (&a/$jvm-lrem ?x ?y)
      (&&host/compile-jvm-lrem compile-expression ?type ?x ?y)

      (&a/$jvm-leq ?x ?y)
      (&&host/compile-jvm-leq compile-expression ?type ?x ?y)

      (&a/$jvm-llt ?x ?y)
      (&&host/compile-jvm-llt compile-expression ?type ?x ?y)

      (&a/$jvm-lgt ?x ?y)
      (&&host/compile-jvm-lgt compile-expression ?type ?x ?y)

      ;; Float arithmetic
      (&a/$jvm-fadd ?x ?y)
      (&&host/compile-jvm-fadd compile-expression ?type ?x ?y)
      
      (&a/$jvm-fsub ?x ?y)
      (&&host/compile-jvm-fsub compile-expression ?type ?x ?y)
      
      (&a/$jvm-fmul ?x ?y)
      (&&host/compile-jvm-fmul compile-expression ?type ?x ?y)
      
      (&a/$jvm-fdiv ?x ?y)
      (&&host/compile-jvm-fdiv compile-expression ?type ?x ?y)
      
      (&a/$jvm-frem ?x ?y)
      (&&host/compile-jvm-frem compile-expression ?type ?x ?y)

      (&a/$jvm-feq ?x ?y)
      (&&host/compile-jvm-feq compile-expression ?type ?x ?y)

      (&a/$jvm-flt ?x ?y)
      (&&host/compile-jvm-flt compile-expression ?type ?x ?y)

      (&a/$jvm-fgt ?x ?y)
      (&&host/compile-jvm-fgt compile-expression ?type ?x ?y)

      ;; Double arithmetic
      (&a/$jvm-dadd ?x ?y)
      (&&host/compile-jvm-dadd compile-expression ?type ?x ?y)
      
      (&a/$jvm-dsub ?x ?y)
      (&&host/compile-jvm-dsub compile-expression ?type ?x ?y)
      
      (&a/$jvm-dmul ?x ?y)
      (&&host/compile-jvm-dmul compile-expression ?type ?x ?y)
      
      (&a/$jvm-ddiv ?x ?y)
      (&&host/compile-jvm-ddiv compile-expression ?type ?x ?y)
      
      (&a/$jvm-drem ?x ?y)
      (&&host/compile-jvm-drem compile-expression ?type ?x ?y)

      (&a/$jvm-deq ?x ?y)
      (&&host/compile-jvm-deq compile-expression ?type ?x ?y)

      (&a/$jvm-dlt ?x ?y)
      (&&host/compile-jvm-dlt compile-expression ?type ?x ?y)

      (&a/$jvm-dgt ?x ?y)
      (&&host/compile-jvm-dgt compile-expression ?type ?x ?y)
      
      (&a/$jvm-null _)
      (&&host/compile-jvm-null compile-expression ?type)

      (&a/$jvm-null? ?object)
      (&&host/compile-jvm-null? compile-expression ?type ?object)
      
      (&a/$jvm-new ?class ?classes ?args)
      (&&host/compile-jvm-new compile-expression ?type ?class ?classes ?args)

      (&a/$jvm-getstatic ?class ?field)
      (&&host/compile-jvm-getstatic compile-expression ?type ?class ?field)

      (&a/$jvm-getfield ?class ?field ?object)
      (&&host/compile-jvm-getfield compile-expression ?type ?class ?field ?object)

      (&a/$jvm-putstatic ?class ?field ?value)
      (&&host/compile-jvm-putstatic compile-expression ?type ?class ?field ?value)

      (&a/$jvm-putfield ?class ?field ?object ?value)
      (&&host/compile-jvm-putfield compile-expression ?type ?class ?field ?object ?value)

      (&a/$jvm-invokestatic ?class ?method ?classes ?args)
      (&&host/compile-jvm-invokestatic compile-expression ?type ?class ?method ?classes ?args)

      (&a/$jvm-invokevirtual ?class ?method ?classes ?object ?args)
      (&&host/compile-jvm-invokevirtual compile-expression ?type ?class ?method ?classes ?object ?args)

      (&a/$jvm-invokeinterface ?class ?method ?classes ?object ?args)
      (&&host/compile-jvm-invokeinterface compile-expression ?type ?class ?method ?classes ?object ?args)

      (&a/$jvm-invokespecial ?class ?method ?classes ?object ?args)
      (&&host/compile-jvm-invokespecial compile-expression ?type ?class ?method ?classes ?object ?args)
      
      (&a/$jvm-new-array ?class ?length)
      (&&host/compile-jvm-new-array compile-expression ?type ?class ?length)

      (&a/$jvm-aastore ?array ?idx ?elem)
      (&&host/compile-jvm-aastore compile-expression ?type ?array ?idx ?elem)

      (&a/$jvm-aaload ?array ?idx)
      (&&host/compile-jvm-aaload compile-expression ?type ?array ?idx)

      (&a/$jvm-try ?body ?catches ?finally)
      (&&host/compile-jvm-try compile-expression ?type ?body ?catches ?finally)

      (&a/$jvm-throw ?ex)
      (&&host/compile-jvm-throw compile-expression ?type ?ex)

      (&a/$jvm-monitorenter ?monitor)
      (&&host/compile-jvm-monitorenter compile-expression ?type ?monitor)

      (&a/$jvm-monitorexit ?monitor)
      (&&host/compile-jvm-monitorexit compile-expression ?type ?monitor)

      (&a/$jvm-d2f ?value)
      (&&host/compile-jvm-d2f compile-expression ?type ?value)

      (&a/$jvm-d2i ?value)
      (&&host/compile-jvm-d2i compile-expression ?type ?value)

      (&a/$jvm-d2l ?value)
      (&&host/compile-jvm-d2l compile-expression ?type ?value)
      
      (&a/$jvm-f2d ?value)
      (&&host/compile-jvm-f2d compile-expression ?type ?value)

      (&a/$jvm-f2i ?value)
      (&&host/compile-jvm-f2i compile-expression ?type ?value)

      (&a/$jvm-f2l ?value)
      (&&host/compile-jvm-f2l compile-expression ?type ?value)
      
      (&a/$jvm-i2b ?value)
      (&&host/compile-jvm-i2b compile-expression ?type ?value)

      (&a/$jvm-i2c ?value)
      (&&host/compile-jvm-i2c compile-expression ?type ?value)

      (&a/$jvm-i2d ?value)
      (&&host/compile-jvm-i2d compile-expression ?type ?value)

      (&a/$jvm-i2f ?value)
      (&&host/compile-jvm-i2f compile-expression ?type ?value)

      (&a/$jvm-i2l ?value)
      (&&host/compile-jvm-i2l compile-expression ?type ?value)

      (&a/$jvm-i2s ?value)
      (&&host/compile-jvm-i2s compile-expression ?type ?value)

      (&a/$jvm-l2d ?value)
      (&&host/compile-jvm-l2d compile-expression ?type ?value)

      (&a/$jvm-l2f ?value)
      (&&host/compile-jvm-l2f compile-expression ?type ?value)

      (&a/$jvm-l2i ?value)
      (&&host/compile-jvm-l2i compile-expression ?type ?value)

      (&a/$jvm-iand ?x ?y)
      (&&host/compile-jvm-iand compile-expression ?type ?x ?y)

      (&a/$jvm-ior ?x ?y)
      (&&host/compile-jvm-ior compile-expression ?type ?x ?y)

      (&a/$jvm-ixor ?x ?y)
      (&&host/compile-jvm-ixor compile-expression ?type ?x ?y)

      (&a/$jvm-ishl ?x ?y)
      (&&host/compile-jvm-ishl compile-expression ?type ?x ?y)

      (&a/$jvm-ishr ?x ?y)
      (&&host/compile-jvm-ishr compile-expression ?type ?x ?y)

      (&a/$jvm-iushr ?x ?y)
      (&&host/compile-jvm-iushr compile-expression ?type ?x ?y)

      (&a/$jvm-land ?x ?y)
      (&&host/compile-jvm-land compile-expression ?type ?x ?y)

      (&a/$jvm-lor ?x ?y)
      (&&host/compile-jvm-lor compile-expression ?type ?x ?y)

      (&a/$jvm-lxor ?x ?y)
      (&&host/compile-jvm-lxor compile-expression ?type ?x ?y)

      (&a/$jvm-lshl ?x ?y)
      (&&host/compile-jvm-lshl compile-expression ?type ?x ?y)

      (&a/$jvm-lshr ?x ?y)
      (&&host/compile-jvm-lshr compile-expression ?type ?x ?y)

      (&a/$jvm-lushr ?x ?y)
      (&&host/compile-jvm-lushr compile-expression ?type ?x ?y)

      (&a/$jvm-instanceof ?class ?object)
      (&&host/compile-jvm-instanceof compile-expression ?type ?class ?object)
      )
    ))

(defn ^:private compile-statement [syntax]
  (|case syntax
    (&a/$def ?name ?body)
    (&&lux/compile-def compile-expression ?name ?body)

    (&a/$declare-macro ?module ?name)
    (&&lux/compile-declare-macro compile-expression ?module ?name)

    (&a/$jvm-program ?body)
    (&&host/compile-jvm-program compile-expression ?body)
    
    (&a/$jvm-interface ?name ?supers ?methods)
    (&&host/compile-jvm-interface compile-expression ?name ?supers ?methods)

    (&a/$jvm-class ?name ?super-class ?interfaces ?fields ?methods)
    (&&host/compile-jvm-class compile-expression ?name ?super-class ?interfaces ?fields ?methods)))

(defn ^:private compile-token [syntax]
  (|case syntax
    (&a/$def ?name ?body)
    (&&lux/compile-def compile-expression ?name ?body)

    (&a/$declare-macro ?module ?name)
    (&&lux/compile-declare-macro compile-expression ?module ?name)

    (&a/$jvm-program ?body)
    (&&host/compile-jvm-program compile-expression ?body)
    
    (&a/$jvm-interface ?name ?supers ?methods)
    (&&host/compile-jvm-interface compile-expression ?name ?supers ?methods)

    (&a/$jvm-class ?name ?super-class ?interfaces ?fields ?methods)
    (&&host/compile-jvm-class compile-expression ?name ?super-class ?interfaces ?fields ?methods)

    _
    (compile-expression syntax)))

(defn ^:private eval! [expr]
  (&/with-eval
    (|do [module &/get-module-name
          id &/gen-id
          :let [class-name (str (&host/->module-class module) "/" id)
                ;; _ (prn 'eval! id class-name)
                =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 class-name nil "java/lang/Object" nil)
                         (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/eval-field "Ljava/lang/Object;" nil nil)
                             (doto (.visitEnd))))]
          _ (&/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
              (|do [^MethodVisitor *writer* &/get-writer
                    :let [_ (.visitCode *writer*)]
                    _ (compile-expression expr)
                    :let [_ (doto *writer*
                              (.visitFieldInsn Opcodes/PUTSTATIC class-name &/eval-field "Ljava/lang/Object;")
                              (.visitInsn Opcodes/RETURN)
                              (.visitMaxs 0 0)
                              (.visitEnd))]]
                (return nil)))
          :let [bytecode (.toByteArray (doto =class
                                         .visitEnd))]
          _ (&&/save-class! (str id) bytecode)
          loader &/loader]
      (-> (.loadClass ^ClassLoader loader (str (&host/->module-class module) "." id))
          (.getField &/eval-field)
          (.get nil)
          return))))

(defn ^:private compile-module [name]
  ;; (prn 'compile-module name (&&cache/cached? name))
  (let [file-name (str &&/input-dir "/" name ".lux")]
    (|do [file-content (&&io/read-file file-name)
          :let [file-hash (hash file-content)]]
      (if (&&cache/cached? name)
        (&&cache/load name file-hash compile-module)
        (let [compiler-step (&optimizer/optimize eval! compile-module compile-token)]
          (|do [module-exists? (&a-module/exists? name)]
            (if module-exists?
              (fail "[Compiler Error] Can't redefine a module!")
              (|do [_ (&a-module/enter-module name)
                    :let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                                   (.visit Opcodes/V1_6 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                           (str (&host/->module-class name) "/_") nil "java/lang/Object" nil)
                                   (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/hash-field "I" nil file-hash)
                                       .visitEnd)
                                   (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/compiler-field "Ljava/lang/String;" nil &&/version)
                                       .visitEnd))
                          ;; _ (prn 'compile-module name =class)
                          ]]
                (fn [state]
                  (|case ((&/with-writer =class
                            (&/exhaust% compiler-step))
                          (&/set$ &/$source (&reader/from name file-content) state))
                    (&/$Right ?state _)
                    (&/run-state (|do [defs &a-module/defs
                                       imports &a-module/imports
                                       tag-groups &&module/tag-groups
                                       :let [_ (doto =class
                                                 (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/defs-field "Ljava/lang/String;" nil
                                                                  (->> defs
                                                                       (&/|map (fn [_def]
                                                                                 (|let [[?exported ?name ?ann] _def]
                                                                                   (str (if ?exported &&/exported-true &&/exported-false)
                                                                                        &&/exported-separator
                                                                                        ?name
                                                                                        &&/exported-separator
                                                                                        ?ann))))
                                                                       (&/|interpose &&/def-separator)
                                                                       (&/fold str "")))
                                                     .visitEnd)
                                                 (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/imports-field "Ljava/lang/String;" nil
                                                                  (->> imports (&/|interpose &&/import-separator) (&/fold str "")))
                                                     .visitEnd)
                                                 (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/tags-field "Ljava/lang/String;" nil
                                                                  (->> tag-groups
                                                                       (&/|map (fn [group]
                                                                                 (|let [[type tags] group]
                                                                                   (->> tags (&/|interpose &&/tag-separator) (&/fold str "")
                                                                                        (str type &&/type-separator)))))
                                                                       (&/|interpose &&/tag-group-separator)
                                                                       (&/fold str "")))
                                                     .visitEnd)
                                                 (.visitEnd))
                                             ;; _ (prn 'CLOSED name =class)
                                             ]]
                                   (&&/save-class! &/module-class-name (.toByteArray =class)))
                                 ?state)
                    
                    (&/$Left ?message)
                    (fail* ?message)))))))
        ))
    ))

(defn ^:private init! []
  (.mkdirs (java.io.File. &&/output-dir)))

;; [Resources]
(defn compile-program [program-module]
  (init!)
  (|case ((&/map% compile-module (&/|list "lux" program-module)) (&/init-state nil))
    (&/$Right ?state _)
    (do (println "Compilation complete!")
      (&&cache/clean ?state)
      (&&package/package program-module))

    (&/$Left ?message)
    (assert false ?message)))
