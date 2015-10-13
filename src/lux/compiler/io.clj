;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.io
  (:require (lux [base :as & :refer [|let |do return* return fail fail*]])
            (lux.compiler [base :as &&])
            [lux.lib.loader :as &lib]))

;; [Utils]
(def ^:private !libs (atom nil))

(defn ^:private libs-imported? []
  (not (nil? @!libs)))

(defn ^:private init-libs! []
  (reset! !libs (&lib/load)))

;; [Resources]
(defn read-file [^String file-name]
  (let [file (new java.io.File (str &&/input-dir  "/" file-name))]
    (if (.exists file)
      (return (slurp file))
      (do (when (not (libs-imported?))
            (init-libs!))
        (if-let [code (get @!libs file-name)]
          (return code)
          (fail (str "[I/O Error] File doesn't exist: " file-name))))
      )))
