(ns leiningen.lux.builder
  (:require (leiningen.lux
             [utils :as &utils]
             [packager :as &packager])))

(defn build [project]
  (if-let [program-module (get-in project [:lux :program])]
    ;; (if-let [command (&utils/build-jvm project program-module)]
    ;;   (when (time (&utils/run-process command
    ;;                                   nil
    ;;                                   "[COMPILATION BEGAN]"
    ;;                                   "[COMPILATION ENDED]"))
    ;;     true)
    ;;   )
    (let [command (&utils/compile-path project program-module (get project :source-paths (list)))]
      (when (time (&utils/run-process command
                                      nil
                                      "[COMPILATION BEGAN]"
                                      "[COMPILATION ENDED]"))
        (time (&packager/package project program-module (get project :resource-paths (list))))
        true))
    (println "Please provide a program main module in [:lux :program]")))
