;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.compiler.io
  (:require (lux [base :as & :refer [|let |do return* return fail fail*]])
            ))

;; [Resources]
(defn read-file [path]
  (let [file (new java.io.File path)]
    (if (.exists file)
      (return (slurp file))
      (fail (str "[I/O] File doesn't exist: " path)))))
