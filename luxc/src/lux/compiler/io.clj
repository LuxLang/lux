;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.io
  (:require (lux [base :as & :refer [|case |let |do return* return fail fail*]])
            (lux.compiler [base :as &&])
            [lux.lib.loader :as &lib]))

;; [Utils]
(def ^:private !libs (atom nil))

(defn ^:private libs-imported? []
  (not (nil? @!libs)))

(defn ^:private init-libs! []
  (reset! !libs (&lib/load)))

;; [Resources]
(defn read-file [source-dirs ^String file-name]
  (|case (&/|some (fn [source-dir]
                    (let [file (new java.io.File (str source-dir "/" file-name))]
                      (if (.exists file)
                        (&/$Some file)
                        &/$None)))
                  source-dirs)
    (&/$Some file)
    (return (slurp file))

    (&/$None)
    (do (when (not (libs-imported?))
          (init-libs!))
      (if-let [code (get @!libs file-name)]
        (return code)
        (fail (str "[I/O Error] File doesn't exist: " file-name))))))
