(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail*]]
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
                          [lux :as &&lux]
                          [host :as &&host]
                          [case :as &&case]
                          [lambda :as &&lambda]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Utils/Compilers]
(defn ^:private compile-expression [syntax]
  (matchv ::M/objects [syntax]
    [[?form ?type]]
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

      [["lux;Local" ?idx]]
      (&&lux/compile-local compile-expression ?type ?idx)

      [["captured" [?scope ?captured-id ?source]]]
      (&&lux/compile-captured compile-expression ?type ?scope ?captured-id ?source)

      [["lux;Global" [?owner-class ?name]]]
      (&&lux/compile-global compile-expression ?type ?owner-class ?name)

      [["apply" [?fn ?args]]]
      (&&lux/compile-apply compile-expression ?type ?fn ?args)

      [["variant" [?tag ?members]]]
      (&&lux/compile-variant compile-expression ?type ?tag ?members)

      [["case" [?value ?match]]]
      (&&case/compile-case compile-expression ?type ?value ?match)

      [["lambda" [?scope ?env ?body]]]
      (&&lambda/compile-lambda compile-expression ?scope ?env ?body)

      [["ann" [?value-ex ?type-ex]]]
      (&&lux/compile-ann compile-expression ?type ?value-ex ?type-ex)
      
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
      )
    ))

(defn ^:private compile-statement [syntax]
  (matchv ::M/objects [syntax]
    [["def" [?name ?body ?def-data]]]
    (&&lux/compile-def compile-expression ?name ?body ?def-data)

    [["declare-macro" [?module ?name]]]
    (&&lux/compile-declare-macro compile-expression ?module ?name)

    [["jvm-program" ?body]]
    (&&host/compile-jvm-program compile-expression ?body)
    
    [["jvm-interface" [?name ?supers ?methods]]]
    (&&host/compile-jvm-interface compile-expression ?name ?supers ?methods)

    [["jvm-class" [?name ?super-class ?interfaces ?fields ?methods]]]
    (&&host/compile-jvm-class compile-expression ?name ?super-class ?interfaces ?fields ?methods)))

(defn ^:private eval! [expr]
  (&/with-eval
    (|do [module &/get-module-name
          id &/gen-id
          :let [class-name (str module "/" id)
                ;; _ (prn 'eval! id class-name)
                =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 class-name nil "java/lang/Object" nil)
                         (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_eval" "Ljava/lang/Object;" nil nil)
                             (doto (.visitEnd))))]
          _ (&/with-writer (.visitMethod =class Opcodes/ACC_PUBLIC "<clinit>" "()V" nil nil)
              (|do [^MethodVisitor *writer* &/get-writer
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
          _ (&&/save-class! (str id) bytecode)
          loader &/loader]
      (-> (.loadClass ^ClassLoader loader (str module "." id))
          (.getField "_eval")
          (.get nil)
          return))))

(defn ^:private compile-module [name]
  ;; (prn 'compile-module name)
  (if (&&/cached? name)
    (do ;; (println "YOLO")
      (let [file-name (str "input/" name ".lux")
            file-content (slurp file-name)]
        (&&/load-cache name (hash file-content) compile-module)))
    (let [compiler-step (|do [analysis+ (&optimizer/optimize eval! compile-module)]
                          (&/map% compile-statement analysis+))]
      (fn [state]
        (if (->> state (&/get$ &/$MODULES) (&/|contains? name))
          (if (.equals ^Object name "lux")
            (return* state nil)
            (fail* "[Compiler Error] Can't redefine a module!"))
          (let [file-name (str "input/" name ".lux")
                file-content (slurp file-name)
                =class (doto (new ClassWriter ClassWriter/COMPUTE_MAXS)
                         (.visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER)
                                 (str name "/_") nil "java/lang/Object" nil)
                         (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_hash" "I" nil (hash file-content))
                             .visitEnd)
                         (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_compiler" "Ljava/lang/String;" nil &&/version)
                             .visitEnd))]
            (matchv ::M/objects [((&/exhaust% compiler-step)
                                  (->> state
                                       (&/set$ &/$SOURCE (&reader/from file-name file-content))
                                       (&/set$ &/$ENVS (&/|list (&/env name)))
                                       (&/update$ &/$HOST #(&/set$ &/$WRITER (&/V "lux;Some" =class) %))
                                       (&/update$ &/$MODULES #(&/|put name &a-module/init-module %))))]
              [["lux;Right" [?state _]]]
              (&/run-state (|do [defs &a-module/defs
                                 imports &a-module/imports
                                 :let [_ (doto =class
                                           (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_defs" "Ljava/lang/String;" nil
                                                            (->> defs
                                                                 (&/|map (fn [_def]
                                                                           (|let [[?exported ?name ?ann] _def]
                                                                             (str (if ?exported "1" "0") " " ?name " " ?ann))))
                                                                 (&/|interpose "\t")
                                                                 (&/fold str "")))
                                               .visitEnd)
                                           (-> (.visitField (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC) "_imports" "Ljava/lang/String;" nil
                                                            (->> imports (&/|interpose "\t") (&/fold str "")))
                                               .visitEnd)
                                           (.visitEnd))]]
                             (&&/save-class! "_" (.toByteArray =class)))
                           ?state)
              
              [["lux;Left" ?message]]
              (fail* ?message))))))))

(defn ^:private clean-file [^java.io.File file]
  (if (.isDirectory file)
    (do (doseq [f (seq (.listFiles file))]
          (clean-file f))
      (.delete file))
    (.delete file)))

(defn ^:private setup-dirs! []
  (.mkdir (java.io.File. "cache"))
  (.mkdir (java.io.File. "cache/jvm"))
  (.mkdir (java.io.File. "output"))
  (.mkdir (java.io.File. "output/jvm"))
  (doseq [f (seq (.listFiles (java.io.File. "output/jvm")))]
    (clean-file f)))

;; [Resources]
(defn compile-all [modules]
  (setup-dirs!)
  (matchv ::M/objects [((&/map% compile-module (&/|cons "lux" modules)) (&/init-state nil))]
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
