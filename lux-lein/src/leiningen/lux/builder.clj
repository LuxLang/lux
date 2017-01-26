(ns leiningen.lux.builder
  (:require [leiningen.core.classpath :as classpath]
            (leiningen.lux [utils :as &utils]
                           [packager :as &packager])))

(defn build [project]
  (if-let [program-module (get-in project [:lux :program])]
    (when (&utils/run-process (&utils/compile-path project program-module (get project :source-paths (list)))
                              nil
                              "[BUILD BEGIN]"
                              "[BUILD END]")
      (&packager/package project program-module (get project :resource-paths (list))))
    (println "Please provide a program main module in [:lux :program]")))
