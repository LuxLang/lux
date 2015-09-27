;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.lib.loader
  (:refer-clojure :exclude [load])
  (:require (lux [base :as & :refer [|let |do return fail return* fail* |case]]))
  (:import (java.io InputStream
                    File
                    FileInputStream
                    ByteArrayInputStream
                    ByteArrayOutputStream)
           java.util.zip.GZIPInputStream
           (org.apache.commons.compress.archivers.tar TarArchiveEntry
                                                      TarArchiveInputStream)))

;; [Utils]
(defn ^:private fetch-libs [from]
  (seq (.listFiles (new File from))))

(let [init-capacity (* 100 1024)
      buffer-size 1024]
  (defn ^:private ^"[B" read-stream [^InputStream is]
    (let [buffer (byte-array buffer-size)]
      (with-open [os (new ByteArrayOutputStream init-capacity)]
        (loop [bytes-read (.read is buffer 0 buffer-size)]
          (when (not= -1 bytes-read)
            (do (.write os buffer 0 bytes-read)
              (recur (.read is buffer 0 buffer-size)))))
        (.toByteArray os)))))

(defn ^:private unpackage [^File lib-file]
  (let [is (->> lib-file
                (new FileInputStream)
                (new GZIPInputStream)
                (new TarArchiveInputStream))]
    (loop [lib-data {}
           entry (.getNextTarEntry is)]
      (if entry
        (recur (assoc lib-data (.getName entry) (new String (read-stream is)))
               (.getNextTarEntry is))
        lib-data))))

;; [Exports]
(def lib-ext ".tar.gz")

(defn load [from]
  (reduce merge {}
          (for [lib (fetch-libs from)]
            (unpackage lib))))

(comment
  (->> &/lib-dir load keys)
  )
