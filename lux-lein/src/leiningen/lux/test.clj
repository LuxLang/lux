(ns leiningen.lux.test
  (:refer-clojure :exclude [test])
  (:require [leiningen.core.classpath :as classpath]
            (leiningen.lux [utils :as &utils]
                           [packager :as &packager])))

(def missing-module-error "Please provide a test module in [:lux :test]")

(defn test [project]
  (if-let [[test-module test-definition] (get-in project [:lux :test])]
    (when (time (&utils/run-process (&utils/compile-path project test-module test-definition (concat (:test-paths project) (:source-paths project)))
                                    nil
                                    "[COMPILATION BEGAN]"
                                    "[COMPILATION ENDED]"))
      (let [java-cmd (get project :java-cmd "java")
            jvm-opts (->> (get project :jvm-opts) (interpose " ") (reduce str ""))
            output-package (str (get project :target-path &utils/default-target-dir)
                                java.io.File/separator
                                (get project :jar-name &utils/output-package))]
        (do (time (&packager/package project test-module (get project :resource-paths (list))))
          (time (&utils/run-process (str java-cmd " " jvm-opts " -jar " output-package)
                                    nil
                                    "[TESTING BEGAN]"
                                    "[TESTING ENDED]"))
          true)))
    (println missing-module-error)))
