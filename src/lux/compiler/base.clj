;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.compiler.base
  (:require [clojure.string :as string]
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
(def ^String version "0.2")
(def ^String input-dir "source")
(def ^String output-dir "target/jvm")
(def ^String function-class "lux/Function")

(def ^String local-prefix "l")
(def ^String partial-prefix "p")
(def ^String closure-prefix "c")
(def ^String apply-signature "(Ljava/lang/Object;)Ljava/lang/Object;")

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
(defn load-class! [^ClassLoader loader name]
  ;; (prn 'load-class! name)
  (.loadClass loader name))

(defn save-class! [name bytecode]
  (|do [eval? &/get-eval
        module &/get-module-name
        loader &/loader
        !classes &/classes
        :let [real-name (str (&host/->module-class module) "." name)
              _ (swap! !classes assoc real-name bytecode)
              _ (load-class! loader real-name)
              _ (when (not eval?)
                  (write-output module name bytecode))]]
    (return nil)))
