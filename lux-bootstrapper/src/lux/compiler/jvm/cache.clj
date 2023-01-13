;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns lux.compiler.jvm.cache
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
                          [module :as &a-module])
            (lux.compiler [core :as &&core]
                          [io :as &&io])
            (lux.compiler.jvm [base :as &&]))
  (:import (java.io File)
           (java.lang.reflect Field)
           ))

;; [Utils]
(defn ^:private read-file [^File file]
  "(-> File (Array Byte))"
  (with-open [reader (io/input-stream file)]
    (let [length (.length file)
          buffer (byte-array length)]
      (.read reader buffer 0 length)
      buffer)))

(defn ^:private get-field [^String field-name ^Class class]
  "(-> Text Class Object)"
  (-> class ^Field (.getField field-name) (.get nil)))

;; [Resources]
(defn load-def-value [module name]
  (|do [loader &/loader
        :let [def-class (&&/load-class! loader (str (&host-generics/->class-name module) "." (&host/def-name name)))]]
    (return (get-field &/value-field def-class))))

(defn install-all-defs-in-module [module-name]
  (|do [!classes &/classes
        :let [module-path (str @&&core/!output-dir java.io.File/separator module-name)
              file-name+content (for [^File file (seq (.listFiles (new File module-path)))
                                      :when (not (.isDirectory file))
                                      :let [file-name (.getName file)]]
                                  [(second (re-find #"^(.*)\.class$" file-name))
                                   (read-file file)])
              _ (doseq [[file-name content] file-name+content]
                  (swap! !classes assoc (str (&host-generics/->class-name module-name)
                                             "."
                                             file-name)
                         content))]]
    (return (map first file-name+content))))

(defn uninstall-all-defs-in-module [module-name]
  (|do [!classes &/classes
        :let [module-path (str @&&core/!output-dir java.io.File/separator module-name)
              installed-files (for [^File file (seq (.listFiles (new File module-path)))
                                    :when (not (.isDirectory file))
                                    :let [file-name (.getName file)]]
                                (second (re-find #"^(.*)\.class$" file-name)))
              _ (swap! !classes (fn [_classes-dict]
                                  (reduce dissoc _classes-dict installed-files)))]]
    (return nil)))
