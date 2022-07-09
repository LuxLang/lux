(ns lux
  (:gen-class)
  (:require [lux.base :as & :refer [|let |do return return* |case]]
            [lux.compiler :as &compiler]
            [lux.repl :as &repl]
            [clojure.string :as string]
            :reload-all)
  (:import (java.io File)))

(def unit-separator
  (str (char 31)))

(defn- separate-paths
  "(-> Text (List Text))"
  [paths]
  (-> paths
      (string/replace unit-separator "\n")
      string/split-lines
      rest
      &/->list))

(defn -main [& args]
  (|case (&/->list args)
    (&/$Item "release" (&/$Item program-module (&/$Item program-definition (&/$Item dependencies (&/$Item source-dirs (&/$Item target-dir (&/$End)))))))
    (&compiler/compile-program &/$Build
                               program-module
                               program-definition
                               (separate-paths dependencies)
                               (separate-paths source-dirs)
                               target-dir)

    (&/$Item "repl" (&/$Item dependencies (&/$Item source-dirs (&/$Item target-dir (&/$End)))))
    (&repl/repl (separate-paths dependencies)
                (separate-paths source-dirs)
                target-dir)

    _
    (println "Cannot understand command.")))
