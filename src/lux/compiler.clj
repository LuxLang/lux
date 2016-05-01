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
            [lux.host.generics :as &host-generics]
            [lux.optimizer :as &o]
            [lux.analyser.base :as &a]
            [lux.analyser.module :as &a-module]
            (lux.compiler [base :as &&]
                          [cache :as &&cache]
                          [lux :as &&lux]
                          [host :as &&host]
                          [case :as &&case]
                          [lambda :as &&lambda]
                          [module :as &&module]
                          [io :as &&io])
            [lux.packager.program :as &packager-program])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Resources]
(def ^:private !source->last-line (atom nil))

(defn compile-expression [syntax]
  (|let [[[?type [_file-name _line _column]] ?form] syntax]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [debug-label (new Label)
                _ (when (not= _line (get @!source->last-line _file-name))
                    (doto *writer*
                      (.visitLabel debug-label)
                      (.visitLineNumber (int _line) debug-label))
                    (swap! !source->last-line assoc _file-name _line))]]
      (|case ?form
        (&o/$bool ?value)
        (&&lux/compile-bool compile-expression ?value)

        (&o/$int ?value)
        (&&lux/compile-int compile-expression ?value)

        (&o/$real ?value)
        (&&lux/compile-real compile-expression ?value)

        (&o/$char ?value)
        (&&lux/compile-char compile-expression ?value)

        (&o/$text ?value)
        (&&lux/compile-text compile-expression ?value)

        (&o/$tuple ?elems)
        (&&lux/compile-tuple compile-expression ?elems)

        (&o/$var (&/$Local ?idx))
        (&&lux/compile-local compile-expression ?idx)

        (&o/$captured ?scope ?captured-id ?source)
        (&&lux/compile-captured compile-expression ?scope ?captured-id ?source)

        (&o/$var (&/$Global ?owner-class ?name))
        (&&lux/compile-global compile-expression ?owner-class ?name)

        (&o/$apply ?fn ?args)
        (&&lux/compile-apply compile-expression ?fn ?args)

        (&o/$variant ?tag ?tail ?members)
        (&&lux/compile-variant compile-expression ?tag ?tail ?members)

        (&o/$case ?value ?match)
        (&&case/compile-case compile-expression ?value ?match)

        (&o/$lambda ?scope ?env ?body)
        (&&lambda/compile-lambda compile-expression ?scope ?env ?body)

        (&o/$ann ?value-ex ?type-ex ?value-type)
        (&&lux/compile-ann compile-expression ?value-ex ?type-ex ?value-type)

        (&o/$host [?proc-category ?proc-name] ?args)
        (&&host/compile-host compile-expression ?proc-category ?proc-name ?args)
        
        ;; JVM
        (&o/$jvm-new ?class ?classes ?args)
        (&&host/compile-jvm-new compile-expression ?class ?classes ?args)

        (&o/$jvm-getstatic ?class ?field ?output-type)
        (&&host/compile-jvm-getstatic compile-expression ?class ?field ?output-type)

        (&o/$jvm-getfield ?class ?field ?object ?output-type)
        (&&host/compile-jvm-getfield compile-expression ?class ?field ?object ?output-type)

        (&o/$jvm-putstatic ?class ?field ?value input-gclass)
        (&&host/compile-jvm-putstatic compile-expression ?class ?field ?value input-gclass)

        (&o/$jvm-putfield ?class ?field ?value input-gclass ?object ?input-type)
        (&&host/compile-jvm-putfield compile-expression ?class ?field ?object ?value input-gclass ?input-type)

        (&o/$jvm-invokestatic ?class ?method ?classes ?args ?output-type)
        (&&host/compile-jvm-invokestatic compile-expression ?class ?method ?classes ?args ?output-type)

        (&o/$jvm-invokevirtual ?class ?method ?classes ?object ?args ?output-type)
        (&&host/compile-jvm-invokevirtual compile-expression ?class ?method ?classes ?object ?args ?output-type)

        (&o/$jvm-invokeinterface ?class ?method ?classes ?object ?args ?output-type)
        (&&host/compile-jvm-invokeinterface compile-expression ?class ?method ?classes ?object ?args ?output-type)

        (&o/$jvm-invokespecial ?class ?method ?classes ?object ?args ?output-type)
        (&&host/compile-jvm-invokespecial compile-expression ?class ?method ?classes ?object ?args ?output-type)
        
        (&o/$jvm-try ?body ?catches ?finally)
        (&&host/compile-jvm-try compile-expression ?body ?catches ?finally)

        (&o/$jvm-instanceof ?class ?object)
        (&&host/compile-jvm-instanceof compile-expression ?class ?object)

        _
        (assert false (prn-str 'compile-expression (&/adt->text syntax)))
        ))
    ))

(defn compile-statement [syntax]
  (|case syntax
    (&o/$def ?name ?body ?meta)
    (&&lux/compile-def compile-expression ?name ?body ?meta)

    (&o/$jvm-program ?body)
    (&&host/compile-jvm-program compile-expression ?body)
    
    (&o/$jvm-interface ?name ?supers ?anns ?methods)
    (&&host/compile-jvm-interface compile-expression ?name ?supers ?anns ?methods)

    (&o/$jvm-class ?name ?super-class ?interfaces ?anns ?inheritance-modifier ?fields ?methods ??env ??ctor-args)
    (&&host/compile-jvm-class compile-expression ?name ?super-class ?interfaces ?anns ?inheritance-modifier ?fields ?methods ??env ??ctor-args)
    ))

(defn init! []
  (reset! !source->last-line {})
  (.mkdirs (java.io.File. &&/output-dir))
  (doto (.getDeclaredMethod java.net.URLClassLoader "addURL" (into-array [java.net.URL]))
    (.setAccessible true)
    (.invoke (ClassLoader/getSystemClassLoader) (to-array [(-> (new java.io.File "./resources") .toURI .toURL)]))))

(defn eval! [expr]
  (&/with-eval
    (|do [module &/get-module-name
          id &/gen-id
          [file-name _ _] &/cursor
          :let [class-name (str (&host/->module-class module) "/" id)
                =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 class-name nil "java/lang/Object" nil)
                         (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/eval-field "Ljava/lang/Object;" nil nil)
                             (doto (.visitEnd)))
                         (.visitSource file-name nil))]
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
      (-> (.loadClass ^ClassLoader loader (str (&host-generics/->class-name module) "." id))
          (.getField &/eval-field)
          (.get nil)
          return))))

(defn compile-module [source-dirs name]
  (let [file-name (str name ".lux")]
    (|do [file-content (&&io/read-file source-dirs file-name)
          :let [file-hash (hash file-content)]]
      (if (&&cache/cached? name)
        (&&cache/load source-dirs name file-hash compile-module)
        (let [compiler-step (&optimizer/optimize eval! (partial compile-module source-dirs) compile-statement)]
          (|do [module-exists? (&a-module/exists? name)]
            (if module-exists?
              (fail "[Compiler Error] Can't redefine a module!")
              (|do [_ (&&cache/delete name)
                    _ (&a-module/enter-module name)
                    _ (&/flag-active-module name)
                    :let [=class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                                   (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                           (str (&host/->module-class name) "/_") nil "java/lang/Object" nil)
                                   (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/hash-field "I" nil file-hash)
                                       .visitEnd)
                                   (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/compiler-field "Ljava/lang/String;" nil &/compiler-version)
                                       .visitEnd)
                                   (.visitSource file-name nil))]
                    _ (if (= "lux" name)
                        (|do [_ &&host/compile-Function-class
                              _ &&host/compile-LuxUtils-class]
                          (return nil))
                        (return nil))]
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
                                                                                 (|let [[?name ?alias] _def]
                                                                                   (str ?name
                                                                                        &&/exported-separator
                                                                                        ?alias))))
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
                                             ]
                                       _ (&/flag-compiled-module name)]
                                   (&&/save-class! &/module-class-name (.toByteArray =class)))
                                 ?state)
                    
                    (&/$Left ?message)
                    (fail* ?message)))))))
        ))
    ))

(defn compile-program [mode program-module source-dirs]
  (init!)
  (let [m-action (&/map% (partial compile-module source-dirs) (&/|list "lux" program-module))]
    (|case (m-action (&/init-state mode))
      (&/$Right ?state _)
      (do (println "Compilation complete!")
        (&&cache/clean ?state)
        (&packager-program/package program-module))

      (&/$Left ?message)
      (assert false ?message))))
