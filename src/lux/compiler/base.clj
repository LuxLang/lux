(ns lux.compiler.base
  (:require [clojure.string :as string]
            [clojure.core.match :refer [match]]
            (lux [base :as & :refer [exec return* return fail fail*
                                     repeat-m exhaust-m try-m try-all-m map-m reduce-m
                                     apply-m
                                     normalize-ident]])
            [lux.analyser.base :as &a])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Exports]
(def local-prefix "l")
(def partial-prefix "p")
(def closure-prefix "c")
(def apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;")

(defn write-file [file data]
  (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
    (.write stream data)))

(defn write-class [name data]
  (write-file (str "output/" name ".class") data))

(defn load-class! [loader name]
  (.loadClass loader name))

(defn save-class! [name bytecode]
  (exec [loader &/loader
         :let [_ (write-class name bytecode)
               _ (load-class! loader (string/replace name #"/" "."))]]
    (return nil)))

(defn total-locals [expr]
  (match expr
    [::&a/case ?variant ?base-register ?num-registers ?branches]
    (+ ?num-registers (reduce max 0 (map (comp total-locals second) ?branches)))
    
    [::&a/tuple ?members]
    (reduce max 0 (map total-locals ?members))

    [::&a/variant ?tag ?members]
    (reduce max 0 (map total-locals ?members))

    [::&a/call ?fn ?args]
    (reduce max 0 (map total-locals (cons ?fn ?args)))
    
    [::&a/jvm-iadd ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-isub ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-imul ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-idiv ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-irem ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-ladd ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-lsub ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-lmul ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-ldiv ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-lrem ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-fadd ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-fsub ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-fmul ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-fdiv ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-frem ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-dadd ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-dsub ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-dmul ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-ddiv ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))
    
    [::&a/jvm-drem ?x ?y]
    (reduce max 0 (map total-locals (list ?x ?y)))

    [::&a/exec ?exprs]
    (reduce max 0 (map total-locals ?exprs))

    [::&a/jvm-new ?class ?classes ?args]
    (reduce max 0 (map total-locals ?args))

    [::&a/jvm-invokestatic ?class ?method ?classes ?args]
    (reduce max 0 (map total-locals ?args))

    [::&a/jvm-invokevirtual ?class ?method ?classes ?object ?args]
    (reduce max 0 (map total-locals ?args))

    [::&a/jvm-aastore ?array ?idx ?elem]
    (reduce max 0 (map total-locals (list ?array ?elem)))

    [::&a/jvm-aaload ?array ?idx]
    (total-locals ?array)
    
    _
    0))
