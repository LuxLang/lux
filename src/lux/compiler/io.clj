;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.io
  (:require (lux [base :as & :refer [|let |do return* return fail fail*]])
            ))

;; [Resources]
(defn read-file [^String path]
  (let [file (new java.io.File path)]
    (if (.exists file)
      (return (slurp file))
      (fail (str "[I/O Error] File doesn't exist: " path)))))
