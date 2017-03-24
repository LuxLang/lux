(ns leiningen.lux.builder
  (:require [leiningen.core.classpath :as classpath]
            (leiningen.lux [utils :as &utils]
                           [packager :as &packager])))

(defn build [project]
  (if-let [program-modules (get-in project [:lux :program])]
    (do (when-let [jvm-module (get-in program-modules [:jvm])]
          (when (&utils/run-process (&utils/compile-path project "jvm" jvm-module (get project :source-paths (list)))
                                    nil
                                    "[BUILD BEGIN]"
                                    "[BUILD END]")
            (&packager/package project "jvm" jvm-module (get project :resource-paths (list)))))
      (when-let [js-module (get-in program-modules [:js])]
        (when (&utils/run-process (&utils/compile-path project "js" js-module (get project :source-paths (list)))
                                  nil
                                  "[BUILD BEGIN]"
                                  "[BUILD END]")
          (&packager/package project "js" js-module (get project :resource-paths (list))))))
    (println "Please provide a program main module in [:lux :program]")))
