;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns lux.compiler.io
  (:require (lux [base :as & :refer [|case |let |do return* return fail*]])
            (lux.compiler.jvm [base :as &&])
            [lux.lib.loader :as &lib]))

;; [Utils]
(def ^:private !libs (atom nil))

;; [Resources]
(defn init-libs! [dependencies]
  (reset! !libs (&lib/load dependencies)))

(defn read-file [source-dirs module-name]
  (let [^String host-file-name (str module-name ".old.lux")
        ^String lux-file-name (str module-name ".lux")]
    (|case (&/|some (fn [^String source-dir]
                      (let [host-file (new java.io.File source-dir host-file-name)
                            lux-file (new java.io.File source-dir lux-file-name)]
                        (cond (.exists host-file)
                              (&/$Some (&/T [host-file-name host-file]))

                              (.exists lux-file)
                              (&/$Some (&/T [lux-file-name lux-file]))

                              :else
                              &/$None)))
                    source-dirs)
      (&/$Some [file-name file])
      (return (&/T [file-name (slurp file)]))

      (&/$None)
      (if-let [code (get @!libs host-file-name)]
        (return (&/T [host-file-name code]))
        (if-let [code (get @!libs lux-file-name)]
          (return (&/T [lux-file-name code]))
          (&/fail-with-loc (str "[I/O Error] Module does not exist: " module-name)))))))
