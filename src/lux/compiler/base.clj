(ns lux.compiler.base
  (:require [clojure.string :as string]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [exec return* return fail fail*]])
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
  (prn 'total-locals1 (aget expr 0))
  (matchv ::M/objects [expr]
    [["Expression" [?struct ?type]]]
    (do (prn 'total-locals2 (aget ?struct 0))
      (matchv ::M/objects [?struct]
        [["case" [?variant ?base-register ?num-registers ?branches]]]
        (+ ?num-registers (&/fold max 0 (&/|map (comp total-locals second) ?branches)))
        
        [["tuple" ?members]]
        (&/fold max 0 (&/|map total-locals ?members))

        [["variant" [?tag ?value]]]
        (total-locals ?value)

        [["call" [?fn ?args]]]
        (&/fold max 0 (&/|map total-locals (&/|cons ?fn ?args)))
        
        [["jvm-iadd" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-isub" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-imul" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-idiv" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-irem" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-ladd" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-lsub" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-lmul" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-ldiv" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-lrem" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-fadd" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-fsub" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-fmul" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-fdiv" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-frem" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-dadd" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-dsub" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-dmul" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-ddiv" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))
        
        [["jvm-drem" [?x ?y]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?x ?y)))

        [["exec" ?exprs]]
        (&/fold max 0 (&/|map total-locals ?exprs))

        [["jvm-new" [?class ?classes ?args]]]
        (&/fold max 0 (&/|map total-locals ?args))

        [["jvm-invokestatic" [?class ?method ?classes ?args]]]
        (&/fold max 0 (&/|map total-locals ?args))

        [["jvm-invokevirtual" [?class ?method ?classes ?object ?args]]]
        (&/fold max 0 (&/|map total-locals ?args))

        [["jvm-aastore" [?array ?idx ?elem]]]
        (&/fold max 0 (&/|map total-locals (&/|list ?array ?elem)))

        [["jvm-aaload" [?array ?idx]]]
        (total-locals ?array)

        [["lambda" _]]
        0
        
        ;; [_]
        ;; 0
        ))))
