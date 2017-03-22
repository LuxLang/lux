(ns leiningen.lux.test
  (:refer-clojure :exclude [test])
  (:require [leiningen.core.classpath :as classpath]
            (leiningen.lux [utils :as &utils]
                           [packager :as &packager])))

(defn test [project]
  (if-let [tests-modules (get-in project [:lux :tests])]
    (do (when-let [jvm-module (get-in tests-modules [:jvm])]
          (when (&utils/run-process (&utils/compile-path project "jvm" jvm-module (concat (:test-paths project) (:source-paths project)))
                                    nil
                                    "[BUILD BEGIN]"
                                    "[BUILD END]")
            (let [java-cmd (get project :java-cmd "java")
                  jvm-opts (->> (get project :jvm-opts) (interpose " ") (reduce str ""))
                  output-package (str (get-in project [:lux :target] &utils/default-jvm-output-dir) "/"
                                      (get project :jar-name &utils/output-package))]
              (do (&packager/package project "jvm" jvm-module (get project :resource-paths (list)))
                (&utils/run-process (str java-cmd " " jvm-opts " -jar " output-package)
                                    nil
                                    "[TEST BEGIN]"
                                    "[TEST END]")))))
      (when-let [js-module (get-in tests-modules [:js])]
        (when (&utils/run-process (&utils/compile-path project "js" js-module (concat (:test-paths project) (:source-paths project)))
                                  nil
                                  "[BUILD BEGIN]"
                                  "[BUILD END]")
          (let [output-package (str (get-in project [:lux :target] &utils/default-js-output-dir) "/"
                                    "program.js")]
            (do (&packager/package project "js" js-module (get project :resource-paths (list)))
              (&utils/run-process (str "node " output-package)
                                  nil
                                  "[TEST BEGIN]"
                                  "[TEST END]"))))))
    (println "Please provide a test module in [:lux :tests]")))
