;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.compiler.base
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            [clojure.java.io :as io]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return fail fail*]]
                 [type :as &type]
                 [host :as &host])
            (lux.analyser [base :as &a]
                          [module :as &a-module]))
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)
           (java.io File
                    BufferedOutputStream
                    FileOutputStream)
           (java.lang.reflect Field)))

;; [Constants]
(def ^String version "0.3")
(def ^String input-dir "source")
(def ^String output-dir "target/jvm")
(def ^String output-package (str output-dir "/program.jar"))
(def ^String function-class "lux/Function")

;; Formats
(def ^String local-prefix "l")
(def ^String partial-prefix "p")
(def ^String closure-prefix "c")
(def ^String apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;")

(def exported-true "1")
(def exported-false "0")
(def exported-separator " ")
(def def-separator "\t")
(def import-separator "\t")
(def tag-separator " ")
(def type-separator "\t")
(def tag-group-separator "\n")

;; [Utils]
(defn ^:private write-file [^String file ^bytes data]
  (with-open [stream (BufferedOutputStream. (FileOutputStream. file))]
    (.write stream data)))

(defn ^:private write-output [module name data]
  (let [module* (&host/->module-class module)
        module-dir (str output-dir "/" module*)]
    (.mkdirs (File. module-dir))
    (write-file (str module-dir "/" name ".class") data)))

;; [Exports]
(defn ^Class load-class! [^ClassLoader loader name]
  ;; (prn 'load-class! name)
  (.loadClass loader name))

(defn save-class! [name bytecode]
  (|do [eval? &/get-eval
        module &/get-module-name
        loader &/loader
        !classes &/classes
        :let [real-name (str (&host/->module-class module) "." name)
              _ (swap! !classes assoc real-name bytecode)
              _ (when (not eval?)
                  (write-output module name bytecode))
              _ (load-class! loader real-name)]]
    (return nil)))

(do-template [<name> <class> <sig> <dup>]
  (defn <name> [^MethodVisitor writer]
    (doto writer
      (.visitMethodInsn Opcodes/INVOKESTATIC <class> "valueOf" (str <sig> (&host/->type-signature <class>))))
    ;; (doto writer
    ;;   ;; X
    ;;   (.visitTypeInsn Opcodes/NEW <class>) ;; XW
    ;;   (.visitInsn <dup>) ;; WXW
    ;;   (.visitInsn <dup>) ;; WWXW
    ;;   (.visitInsn Opcodes/POP) ;; WWX
    ;;   (.visitMethodInsn Opcodes/INVOKESPECIAL <class> "<init>" <sig>) ;; W
    ;;   )
    )

  wrap-boolean "java/lang/Boolean"   "(Z)" Opcodes/DUP_X1
  wrap-byte    "java/lang/Byte"      "(B)" Opcodes/DUP_X1
  wrap-short   "java/lang/Short"     "(S)" Opcodes/DUP_X1
  wrap-int     "java/lang/Integer"   "(I)" Opcodes/DUP_X1
  wrap-long    "java/lang/Long"      "(J)" Opcodes/DUP_X2
  wrap-float   "java/lang/Float"     "(F)" Opcodes/DUP_X1
  wrap-double  "java/lang/Double"    "(D)" Opcodes/DUP_X2
  wrap-char    "java/lang/Character" "(C)" Opcodes/DUP_X1
  )
