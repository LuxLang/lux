(ns lux.compiler.js.cache
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return* return |case |let]]
                 [type :as &type]
                 [host :as &host])
            [lux.host.generics :as &host-generics]
            (lux.analyser [base :as &a]
                          [module :as &a-module]
                          [meta :as &a-meta])
            (lux.compiler [core :as &&core]
                          [io :as &&io])
            (lux.compiler.js [base :as &&]))
  (:import (java.io File)))

;; [Utils]
(defn ^:private read-file [^File file]
  "(-> File (Array Byte))"
  (with-open [reader (io/input-stream file)]
    (let [length (.length file)
          buffer (byte-array length)]
      (.read reader buffer 0 length)
      buffer)))

;; [Resources]
(defn load-def-value [module name]
  (&&/run-js!+ (&&/js-var-name module name)))

(defn install-all-defs-in-module [module-name]
  (|do [:let [module-code-path (str @&&core/!output-dir java.io.File/separator module-name java.io.File/separator &&/module-js-name)
              ^bytes module-code (read-file (new File module-code-path))]
        _ (&&/run-js!+ (new String module-code))]
    (return (&/|list))))

(defn uninstall-all-defs-in-module [module-name]
  (|do []
    (return nil)))
