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

(defn compile-expression [$begin syntax]
  (|let [[[?type [_file-name _line _]] ?form] syntax]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [debug-label (new Label)
                _ (when (not= _line (get @!source->last-line _file-name))
                    (doto *writer*
                      (.visitLabel debug-label)
                      (.visitLineNumber (int _line) debug-label))
                    (swap! !source->last-line assoc _file-name _line))]]
      (|case ?form
        (&o/$bool ?value)
        (&&lux/compile-bool ?value)

        (&o/$nat ?value)
        (&&lux/compile-nat ?value)

        (&o/$int ?value)
        (&&lux/compile-int ?value)

        (&o/$frac ?value)
        (&&lux/compile-frac ?value)

        (&o/$real ?value)
        (&&lux/compile-real ?value)

        (&o/$char ?value)
        (&&lux/compile-char ?value)

        (&o/$text ?value)
        (&&lux/compile-text ?value)

        (&o/$tuple ?elems)
        (&&lux/compile-tuple (partial compile-expression $begin) ?elems)

        (&o/$var (&/$Local ?idx))
        (&&lux/compile-local (partial compile-expression $begin) ?idx)

        (&o/$captured ?scope ?captured-id ?source)
        (&&lux/compile-captured (partial compile-expression $begin) ?scope ?captured-id ?source)

        (&o/$var (&/$Global ?owner-class ?name))
        (&&lux/compile-global (partial compile-expression $begin) ?owner-class ?name)

        (&o/$apply ?fn ?args)
        (&&lux/compile-apply (partial compile-expression $begin) ?fn ?args)

        (&o/$iter ?args)
        (&&lux/compile-iter (partial compile-expression $begin) $begin ?args)

        (&o/$variant ?tag ?tail ?members)
        (&&lux/compile-variant (partial compile-expression $begin) ?tag ?tail ?members)

        (&o/$case ?value [?pm ?bodies])
        (&&case/compile-case (partial compile-expression $begin) ?value ?pm ?bodies)

        (&o/$let _value _register _body)
        (&&lux/compile-let (partial compile-expression $begin) _value _register _body)

        (&o/$record-get _value _path)
        (&&lux/compile-record-get (partial compile-expression $begin) _value _path)

        (&o/$if _test _then _else)
        (&&lux/compile-if (partial compile-expression $begin) _test _then _else)
        
        (&o/$function ?arity ?scope ?env ?body)
        (&&lambda/compile-function compile-expression &/$None ?arity ?scope ?env ?body)

        ;; TODO: Must get rid of this one...
        (&o/$ann ?value-ex ?type-ex ?value-type)
        (compile-expression $begin ?value-ex)

        (&o/$proc [?proc-category ?proc-name] ?args special-args)
        (&&host/compile-host (partial compile-expression $begin) ?proc-category ?proc-name ?args special-args)
        
        _
        (assert false (prn-str 'compile-expression (&/adt->text syntax)))
        ))
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
                    _ (compile-expression nil expr)
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

(def all-compilers
  (let [compile-expression* (partial compile-expression nil)]
    (&/T [(partial &&lux/compile-def compile-expression)
          (partial &&lux/compile-program compile-expression*)
          (partial &&host/compile-jvm-class compile-expression*)
          &&host/compile-jvm-interface])))

(defn compile-module [source-dirs name]
  (let [file-name (str name ".lux")]
    (|do [file-content (&&io/read-file source-dirs file-name)
          :let [file-hash (hash file-content)]]
      (if (&&cache/cached? name)
        (&&cache/load source-dirs name file-hash compile-module)
        (let [compiler-step (&analyser/analyse &optimizer/optimize eval! (partial compile-module source-dirs) all-compilers)]
          (|do [module-exists? (&a-module/exists? name)]
            (if module-exists?
              (fail "[Compiler Error] Can't redefine a module!")
              (|do [_ (&&cache/delete name)
                    _ (&a-module/create-module name file-hash)
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
                              _ &&host/compile-LuxRT-class]
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
                                                                  (->> imports
                                                                       (&/|map (fn [import]
                                                                                 (|let [[_module _hash] import]
                                                                                   (str _module &&/field-separator _hash))))
                                                                       (&/|interpose &&/entry-separator)
                                                                       (&/fold str "")))
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
                                                 (.visitEnd))]
                                       _ (&/flag-compiled-module name)
                                       _ (&&/save-class! &/module-class-name (.toByteArray =class))]
                                   (return file-hash))
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
