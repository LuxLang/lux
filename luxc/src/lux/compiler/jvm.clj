(ns lux.compiler.jvm
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case]]
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
            (lux.compiler [core :as &&core]
                          [io :as &&io]
                          [cache :as &&cache]
                          [parallel :as &&parallel])
            (lux.compiler.jvm [base :as &&]
                              [lux :as &&lux]
                              [case :as &&case]
                              [function :as &&function]
                              [rt :as &&rt]
                              [cache :as &&jvm-cache])
            (lux.compiler.jvm.proc [common :as &&proc-common]
                                   [host :as &&proc-host]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Resources]
(def ^:private !source->last-line (atom nil))

(defn ^:private compile-expression [$begin syntax]
  (|let [[[?type [_file-name _line _]] ?form] syntax]
    (|do [^MethodVisitor *writer* &/get-writer
          :let [debug-label (new Label)
                _ (when (not= _line (get @!source->last-line _file-name))
                    (doto *writer*
                      (.visitLabel debug-label)
                      (.visitLineNumber (int _line) debug-label))
                    (swap! !source->last-line assoc _file-name _line))]]
      (|case ?form
        (&o/$bit ?value)
        (&&lux/compile-bit ?value)

        (&o/$nat ?value)
        (&&lux/compile-nat ?value)

        (&o/$int ?value)
        (&&lux/compile-int ?value)

        (&o/$rev ?value)
        (&&lux/compile-rev ?value)

        (&o/$frac ?value)
        (&&lux/compile-frac ?value)

        (&o/$text ?value)
        (&&lux/compile-text ?value)

        (&o/$tuple ?elems)
        (&&lux/compile-tuple (partial compile-expression $begin) ?elems)

        (&o/$var (&/$Local ?idx))
        (&&lux/compile-local (partial compile-expression $begin) ?idx)

        (&o/$captured ?scope ?captured-id ?source)
        (&&lux/compile-captured (partial compile-expression $begin) ?scope ?captured-id ?source)

        (&o/$def ?owner-class ?name)
        (&&lux/compile-global (partial compile-expression $begin) ?owner-class ?name)

        (&o/$apply ?fn ?args)
        (&&lux/compile-apply (partial compile-expression $begin) ?fn ?args)

        (&o/$loop _register-offset _inits _body)
        (&&lux/compile-loop compile-expression _register-offset _inits _body)
        
        (&o/$iter _register-offset ?args)
        (&&lux/compile-iter (partial compile-expression $begin) $begin _register-offset ?args)

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
        
        (&o/$function _register-offset ?arity ?scope ?env ?body)
        (&&function/compile-function compile-expression &/$None ?arity ?scope ?env ?body)

        (&o/$ann ?value-ex ?type-ex)
        (compile-expression $begin ?value-ex)

        (&o/$proc [?proc-category ?proc-name] ?args special-args)
        (if (= "jvm" ?proc-category)
          (&&proc-host/compile-proc (partial compile-expression $begin) ?proc-name ?args special-args)
          (&&proc-common/compile-proc (partial compile-expression $begin) ?proc-category ?proc-name ?args special-args))
        
        _
        (assert false (prn-str 'compile-expression (&/adt->text syntax)))
        ))
    ))

(defn init!
  "(-> (List Text) Null)"
  [resources-dirs ^String target-dir]
  (do (reset! !source->last-line {})
    (let [class-loader (ClassLoader/getSystemClassLoader)
          addURL (doto (.getDeclaredMethod java.net.URLClassLoader "addURL" (into-array [java.net.URL]))
                   (.setAccessible true))]
      (doseq [^String resources-dir (&/->seq resources-dirs)]
        (.invoke addURL class-loader
                 (to-array [(->> resources-dir (new java.io.File) .toURI .toURL)]))))))

(defn eval! [expr]
  (&/with-eval
    (|do [module &/get-module-name
          id &/gen-id
          [file-name _ _] &/cursor
          :let [class-name (str (&host/->module-class module) "/" id)
                =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 class-name nil "java/lang/Object" nil)
                         (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) &/value-field "Ljava/lang/Object;" nil nil)
                             (doto (.visitEnd)))
                         (.visitSource file-name nil))]
          _ (&/with-writer (.visitMethod =class Opcodes/ACC_STATIC "<clinit>" "()V" nil nil)
              (|do [^MethodVisitor *writer* &/get-writer
                    :let [_ (.visitCode *writer*)]
                    _ (compile-expression nil expr)
                    :let [_ (doto *writer*
                              (.visitFieldInsn Opcodes/PUTSTATIC class-name &/value-field "Ljava/lang/Object;")
                              (.visitInsn Opcodes/RETURN)
                              (.visitMaxs 0 0)
                              (.visitEnd))]]
                (return nil)))
          :let [bytecode (.toByteArray (doto =class
                                         .visitEnd))]
          _ (&&/save-class! (str id) bytecode)
          loader &/loader]
      (-> (.loadClass ^ClassLoader loader (str (&host-generics/->class-name module) "." id))
          (.getField &/value-field)
          (.get nil)
          return))))

(def all-compilers
  (let [compile-expression* (partial compile-expression nil)]
    (&/T [(partial &&lux/compile-def compile-expression)
          (partial &&lux/compile-program compile-expression*)
          (fn [macro args state] (-> macro (.apply args) (.apply state)))
          (partial &&proc-host/compile-jvm-class compile-expression*)
          &&proc-host/compile-jvm-interface])))

(defn ^:private activate-module! [name file-hash]
  (|do [_ (&&cache/delete name)
        _ (&a-module/create-module name file-hash)]
    (&a-module/flag-active-module name)))

(defn ^:private save-module! [name file-hash class-bytes]
  (|do [_ (&a-module/flag-compiled-module name)
        _ (&&/save-class! &/module-class-name class-bytes)
        module-descriptor (&&core/generate-module-descriptor file-hash)]
    (&&core/write-module-descriptor! name module-descriptor)))

(let [+field-flags+ (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC)
      +datum-sig+ "Ljava/lang/Object;"]
  (defn compile-module [source-dirs name]
    (|do [[file-name file-content] (&&io/read-file source-dirs name)
          :let [file-hash (hash file-content)
                compile-module!! (&&parallel/parallel-compilation (partial compile-module source-dirs))]]
      (&/|eitherL (&&cache/load name)
                  (let [compiler-step (&analyser/analyse &optimizer/optimize eval! compile-module!! all-compilers)]
                    (|do [module-exists? (&a-module/exists? name)]
                      (if module-exists?
                        (&/fail-with-loc (str "[Compiler Error] Cannot re-define a module: " name))
                        (|do [_ (activate-module! name file-hash)
                              :let [module-class-name (str (&host/->module-class name) "/_")
                                    =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                                             (.visit &host/bytecode-version (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                                     module-class-name nil "java/lang/Object" nil)
                                             (.visitSource file-name nil))]
                              _ (if (= "lux" name)
                                  (|do [_ &&rt/compile-Function-class
                                        _ &&rt/compile-LuxRT-class]
                                    (return nil))
                                  (return nil))]
                          (fn [state]
                            (|case ((&/with-writer =class
                                      (&/exhaust% compiler-step))
                                    (&/set$ &/$source (&reader/from name file-content) state))
                              (&/$Right ?state _)
                              (&/run-state (|do [:let [_ (.visitEnd =class)]
                                                 _ (save-module! name file-hash (.toByteArray =class))]
                                             (return file-hash))
                                           ?state)
                              
                              (&/$Left ?message)
                              (&/fail* ?message))))))))
      )))

(let [define-class (doto (.getDeclaredMethod java.lang.ClassLoader "defineClass" (into-array [String
                                                                                              (class (byte-array []))
                                                                                              Integer/TYPE
                                                                                              Integer/TYPE]))
                     (.setAccessible true))]
  (defn memory-class-loader [store]
    (proxy [java.lang.ClassLoader]
      []
      (findClass [^String class-name]
        (if-let [^bytes bytecode (get @store class-name)]
          (.invoke define-class this (to-array [class-name bytecode (int 0) (int (alength bytecode))]))
          (throw (IllegalStateException. (str "[Class Loader] Unknown class: " class-name))))))))

(defn jvm-host []
  (let [store (atom {})]
    (&/$Jvm (&/T [;; "lux;writer"
                  &/$None
                  ;; "lux;loader"
                  (memory-class-loader store)
                  ;; "lux;classes"
                  store
                  ;; lux;type-env
                  (&/|table)
                  ;; lux;dummy-mappings
                  (&/|table)
                  ]))))

(let [!err! *err*]
  (defn compile-program [mode program-module resources-dir source-dirs target-dir]
    (let [m-action (|do [_ (&&cache/pre-load-cache! source-dirs
                                                    &&jvm-cache/load-def-value
                                                    &&jvm-cache/install-all-defs-in-module
                                                    &&jvm-cache/uninstall-all-defs-in-module)
                         _ (compile-module source-dirs "lux")]
                     (compile-module source-dirs program-module))]
      (|case (m-action (&/init-state "{old}" mode (jvm-host)))
        (&/$Right ?state _)
        (do (println "Compilation complete!")
          (&&cache/clean ?state))

        (&/$Left ?message)
        (binding [*out* !err!]
          (do (println (str "Compilation failed:\n" ?message))
            (flush)
            (System/exit 1)))
        ))))
