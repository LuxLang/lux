(ns leiningen.lux.builder
  (:require (leiningen.lux [utils :as &utils]
                           [packager :as &packager])))

(def missing-module-error "Please provide a program main module in [:lux :program]")

(defn build [project]
  (if-let [program-module (get-in project [:lux :program])]
    (when (time (&utils/run-process (&utils/compile-path project program-module (get project :source-paths (list)))
                                    nil
                                    "[COMPILATION BEGAN]"
                                    "[COMPILATION ENDED]"))
      (time (&packager/package project program-module (get project :resource-paths (list))))
      true)
    (println missing-module-error)))
