(ns lux.compiler.base
  (:require [clojure.string :as string]
            (lux [base :as & :refer [exec return* return fail fail*
                                     repeat-m exhaust-m try-m try-all-m map-m reduce-m
                                     apply-m
                                     normalize-ident]]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)))

;; [Resources]
(def local-prefix "l")
(def partial-prefix "p")
(def closure-prefix "c")
(def tuple-field-prefix "_")
(def apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;")

(defn add-nulls [writer amount]
  (dotimes [_ amount]
    (.visitInsn writer Opcodes/ACONST_NULL)))

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
