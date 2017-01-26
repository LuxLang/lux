(ns leiningen.lux.test
  (:refer-clojure :exclude [test])
  (:require [leiningen.core.classpath :as classpath]
            (leiningen.lux [utils :as &utils]
                           [packager :as &packager])))

(defn test [project]
  (if-let [tests-module (get-in project [:lux :tests])]
    (when (&utils/run-process (&utils/compile-path project tests-module (concat (:test-paths project) (:source-paths project)))
                              nil
                              "[BUILD BEGIN]"
                              "[BUILD END]")
      (let [java-cmd (get project :java-cmd "java")
            jvm-opts (->> (get project :jvm-opts) (interpose " ") (reduce str ""))
            output-package (str (get-in project [:lux :target] &utils/default-output-dir) "/"
                                (get project :jar-name &utils/output-package))]
        (do (&packager/package project tests-module (get project :resource-paths (list)))
          (&utils/run-process (str java-cmd " " jvm-opts " -jar " output-package)
                              nil
                              "[TEST BEGIN]"
                              "[TEST END]"))))
    (println "Please provide a test module in [:lux :tests]")))
