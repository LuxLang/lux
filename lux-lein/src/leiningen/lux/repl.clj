;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns leiningen.lux.repl
  (:require [leiningen.core.classpath :as classpath]
            [leiningen.lux.utils :as &utils])
  (:import (java.io InputStreamReader
                    BufferedReader
                    PrintStream)))

(defn repl [project]
  (println (&utils/repl-path project (:source-paths project)))
  ;; (let [process (.exec (Runtime/getRuntime) (&utils/repl-path project (:source-paths project)))]
  ;;   (with-open [std-in (->> System/in (new InputStreamReader) (new BufferedReader))
  ;;               process-in (->> process .getOutputStream (new PrintStream))
  ;;               process-out (->> process .getInputStream (new InputStreamReader) (new BufferedReader))
  ;;               process-err (->> process .getErrorStream (new InputStreamReader) (new BufferedReader))]
  ;;     (loop []
  ;;       (do (loop []
  ;;             (when (.ready process-out)
  ;;               (println (.readLine process-out))
  ;;               (recur)))
  ;;         (loop [had-error? false]
  ;;           (if (.ready process-out)
  ;;             (do (println (.readLine process-err))
  ;;               (recur true))
  ;;             (when had-error?
  ;;               (System/exit 1))))
  ;;         (when-let [input (.readLine std-in)]
  ;;           (do (.println process-in input)
  ;;             (recur)))))
  ;;     ))
  )
