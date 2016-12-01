;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns leiningen.luxc.compiler
  (:refer-clojure :exclude [compile])
  (:require [leiningen.core.classpath :as classpath]
            (leiningen.luxc [utils :as &utils]
                            [packager :as &packager])))

(defn compile [project]
  (if-let [program-module (get-in project [:lux :program])]
    (do (&utils/run-process (&utils/compile-path project program-module (get project :source-paths (list)))
                            nil
                            "[COMPILATION BEGIN]"
                            "[COMPILATION END]")
      (&packager/package project program-module (get project :resource-paths (list))))
    (println "Please provide a program main module in [:lux :program]")))
