;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

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
                          [module :as &a-module])
            [lux.host.generics :as &host-generics])
  (:import (org.objectweb.asm Opcodes
                              Label
                              ClassWriter
                              MethodVisitor)
           (java.io File
                    BufferedOutputStream
                    FileOutputStream)
           (java.lang.reflect Field)))

;; [Constants]
(def ^String version "0.3.1")
(def ^String input-dir "source")
(def ^String output-dir "target/jvm")
(def ^String output-package (str output-dir "/" "program.jar"))
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
(defn ^:private write-file [^String file-name ^bytes data]
  (let [;; file-name (.toLowerCase file-name)
        ]
    (do (assert (not (.exists (File. file-name))) (str "Can't overwrite file: " file-name))
      (with-open [stream (BufferedOutputStream. (FileOutputStream. file-name))]
        (.write stream data)))))

(defn ^:private write-output [module name data]
  (let [module* (&host/->module-class module)
        module-dir (str output-dir "/" module*)]
    (.mkdirs (File. module-dir))
    (write-file (str module-dir "/" name ".class") data)))

(defn class-exists? [^String module ^String class-name]
  "(-> Text Text (IO Bool))"
  (|do [_ (return nil)
        :let [full-path (str output-dir "/" module "/" class-name ".class")
              exists? (.exists (File. full-path))]]
    (return exists?)))

;; [Exports]
(defn ^Class load-class! [^ClassLoader loader name]
  ;; (prn 'load-class! name)
  (.loadClass loader name))

(defn save-class! [name bytecode]
  (|do [eval? &/get-eval
        module &/get-module-name
        loader &/loader
        !classes &/classes
        :let [real-name (str (&host-generics/->class-name module) "." name)
              _ (swap! !classes assoc real-name bytecode)
              _ (when (not eval?)
                  (write-output module name bytecode))
              _ (load-class! loader real-name)]]
    (return nil)))

(do-template [<wrap-name> <unwrap-name> <class> <unwrap-method> <prim> <dup>]
  (do (defn <wrap-name> [^MethodVisitor writer]
        (doto writer
          (.visitMethodInsn Opcodes/INVOKESTATIC <class> "valueOf" (str "(" <prim> ")" (&host-generics/->type-signature <class>)))))
    (defn <unwrap-name> [^MethodVisitor writer]
      (doto writer
        (.visitTypeInsn Opcodes/CHECKCAST <class>)
        (.visitMethodInsn Opcodes/INVOKEVIRTUAL <class> <unwrap-method> (str "()" <prim>)))))

  wrap-boolean unwrap-boolean "java/lang/Boolean"   "booleanValue" "Z" Opcodes/DUP_X1
  wrap-byte    unwrap-byte    "java/lang/Byte"      "byteValue"    "B" Opcodes/DUP_X1
  wrap-short   unwrap-short   "java/lang/Short"     "shortValue"   "S" Opcodes/DUP_X1
  wrap-int     unwrap-int     "java/lang/Integer"   "intValue"     "I" Opcodes/DUP_X1
  wrap-long    unwrap-long    "java/lang/Long"      "longValue"    "J" Opcodes/DUP_X2
  wrap-float   unwrap-float   "java/lang/Float"     "floatValue"   "F" Opcodes/DUP_X1
  wrap-double  unwrap-double  "java/lang/Double"    "doubleValue"  "D" Opcodes/DUP_X2
  wrap-char    unwrap-char    "java/lang/Character" "charValue"    "C" Opcodes/DUP_X1
  )
