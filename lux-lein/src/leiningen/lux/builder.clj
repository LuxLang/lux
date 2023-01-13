;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns leiningen.lux.builder
  (:require (leiningen.lux
             [utils :as &utils]
             [packager :as &packager])))

(defn build [project]
  (if-let [[program-module program-definition] (get-in project [:lux :program])]
    (let [command (&utils/compile-path project program-module program-definition (get project :source-paths (list)))]
      (when (time (&utils/run-process command
                                      nil
                                      "[COMPILATION BEGAN]"
                                      "[COMPILATION ENDED]"))
        (time (&packager/package project program-module (get project :resource-paths (list))))
        true))
    (println "Please provide a program main module in [:lux :program]")))
