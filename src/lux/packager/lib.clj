;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.packager.lib
  (:require [lux.lib.loader :as &lib])
  (:import (java.io File
                    FileOutputStream)
           java.util.zip.GZIPOutputStream
           (org.apache.commons.compress.archivers.tar TarArchiveEntry
                                                      TarArchiveOutputStream)
           ))

;; [Utils]
(defn ^:private read-file ^objects [^File file]
  (with-open [is (java.io.FileInputStream. file)]
    (let [data (byte-array (.length file))]
      (.read is data)
      data)))

(defn ^:private add-to-tar! [prefix ^File file ^TarArchiveOutputStream os]
  "(-> Text File TarArchiveOutputStream Unit)"
  (let [file-name (str prefix "/" (.getName file))]
    (if (.isDirectory file)
      (doseq [file (seq (.listFiles file))]
        (add-to-tar! file-name file os))
      (let [data (read-file file)]
        (doto os
          (.putArchiveEntry (doto (new TarArchiveEntry file-name)
                              (.setSize (.length file))))
          (.write data 0 (alength data))
          (.closeArchiveEntry))))))

;; [Exports]
(defn package [output-lib-name ^File source-dir]
  "(-> Text File Unit)"
  (with-open [out (->> (str output-lib-name &lib/lib-ext) (new FileOutputStream) (new GZIPOutputStream) (new TarArchiveOutputStream))]
    (doseq [file (seq (.listFiles source-dir))]
      (add-to-tar! "" file out))
    ))
