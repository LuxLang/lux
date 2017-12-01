(ns leiningen.lux.test
  (:refer-clojure :exclude [test])
  (:require [leiningen.core.classpath :as classpath]
            (leiningen.lux [utils :as &utils]
                           [packager :as &packager])))

(def missing-module-error "Please provide a test module in [:lux :tests]")

(defn test [project]
  (if-let [tests-modules (get-in project [:lux :tests])]
    (do (when-let [jvm-module (get-in tests-modules [:jvm])]
          (when (&utils/run-process (&utils/compile-path project "jvm" jvm-module (concat (:test-paths project) (:source-paths project)))
                                    nil
                                    "[JVM COMPILATION BEGAN]"
                                    "[JVM COMPILATION ENDED]")
            (let [java-cmd (get project :java-cmd "java")
                  jvm-opts (->> (get project :jvm-opts) (interpose " ") (reduce str ""))
                  output-package (str (get-in project [:lux :target] &utils/default-jvm-output-dir)
                                      java.io.File/separator
                                      (get project :jar-name &utils/output-package))]
              (do (&packager/package project "jvm" jvm-module (get project :resource-paths (list)))
                (&utils/run-process (str java-cmd " " jvm-opts " -jar " output-package)
                                    nil
                                    "[JVM TESTING BEGAN]"
                                    "[JVM TESTING ENDED]")
                true))))
      (when-let [js-module (get-in tests-modules [:js])]
        (when (&utils/run-process (&utils/compile-path project "js" js-module (concat (:test-paths project) (:source-paths project)))
                                  nil
                                  "[JS COMPILATION BEGAN]"
                                  "[JS COMPILATION ENDED]")
          (let [output-package (str (get-in project [:lux :target] &utils/default-js-output-dir)
                                    java.io.File/separator
                                    "program.js")]
            (do (&packager/package project "js" js-module (get project :resource-paths (list)))
              (&utils/run-process (str "node " output-package)
                                  nil
                                  "[JS TESTING BEGAN]"
                                  "[JS TESTING ENDED]")
              true))))
      (when (not (or (get-in tests-modules [:jvm])
                     (get-in tests-modules [:js])))
        (println missing-module-error)))
    (println missing-module-error)))
