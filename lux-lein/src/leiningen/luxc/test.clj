;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns leiningen.luxc.test
  (:refer-clojure :exclude [test])
  (:require [leiningen.core.classpath :as classpath]
            (leiningen.luxc [utils :as &utils]
                            [packager :as &packager])))

(defn test [project]
  (if-let [tests-module (get-in project [:lux :tests])]
    (do (&utils/run-process (&utils/compile-path project tests-module (concat (:test-paths project) (:source-paths project)))
                            nil
                            "[COMPILATION BEGIN]"
                            "[COMPILATION END]")
      (let [java-cmd (get project :java-cmd "java")
            jvm-opts (->> (get project :jvm-opts) (interpose " ") (reduce str ""))
            output-package (str (get-in project [:lux :target] &utils/output-dir) "/"
                                (get project :jar-name &utils/output-package))]
        (do (&packager/package project tests-module (get project :resource-paths (list)))
          (&utils/run-process (str java-cmd " " jvm-opts " -jar " output-package)
                              nil
                              "[TEST BEGIN]"
                              "[TEST END]"))))
    (println "Please provide a test module in [:lux :tests]")))
