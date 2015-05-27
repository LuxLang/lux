(ns lux.compiler.base
  (:require [clojure.string :as string]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail*]])
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

;; (defn write-file [^String file ^bytes data]
;;   (with-open [stream (java.io.BufferedOutputStream. (java.io.FileOutputStream. file))]
;;     (.write stream data)))

;; (defn write-class [name data]
;;   (write-file (str "output/" name ".class") data))

(defn load-class! [^ClassLoader loader name]
  (.loadClass loader name))

;; (defn save-class! [name bytecode]
;;   (|do [loader &/loader
;;          :let [_ (write-class name bytecode)
;;                _ (load-class! loader (string/replace name #"/" "."))]]
;;     (return nil)))

(defn save-class! [name bytecode]
  (let [real-name (string/replace name #"/" ".")]
    (|do [loader &/loader
          !classes &/classes
          :let [_ (swap! !classes assoc real-name bytecode)
                _ (load-class! loader real-name)]]
      (return nil))))
