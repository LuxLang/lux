;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.package
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail*]]
                 [host :as &host])
            (lux.compiler [base :as &&]))
  (:import (java.io File
                    FileInputStream
                    FileOutputStream
                    BufferedInputStream)
           (java.util.jar Manifest
                          Attributes$Name
                          JarEntry
                          JarOutputStream
                          )))

;; [Utils]
(def ^:private kilobyte 1024)

(defn ^:private manifest [^String module]
  "(-> Text Manifest)"
  (doto (new Manifest)
    (-> .getMainAttributes (doto (.put Attributes$Name/MAIN_CLASS (str (&host/->module-class module) "._"))
                             (.put Attributes$Name/MANIFEST_VERSION "1.0")))))

(defn ^:private write-class! [^String path ^File file ^JarOutputStream out]
  "(-> Text File JarOutputStream Unit)"
  (with-open [in (new BufferedInputStream (new FileInputStream file))]
    (let [buffer (byte-array (* 10 kilobyte))]
      (doto out
        (.putNextEntry (new JarEntry (str path "/" (.getName file))))
        (-> (.write buffer 0 bytes-read)
            (->> (when (not= -1 bytes-read))
                 (loop [bytes-read (.read in buffer)])))
        (.flush)
        (.closeEntry)
        ))
    ))

(defn ^:private write-module! [^File file ^JarOutputStream out]
  "(-> File JarOutputStream Unit)"
  (let [module-name (.getName file)]
    (doseq [$class (.listFiles file)]
      (write-class! module-name $class out))))

;; [Resources]
(defn package [module]
  "(-> Text (,))"
  ;; (prn 'package module)
  (with-open [out (new JarOutputStream (->> &&/output-package (new File) (new FileOutputStream)) (manifest module))]
    (doseq [$group (.listFiles (new File &&/output-dir))]
      (write-module! $group out))
    ))
