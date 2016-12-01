;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns leiningen.luxc
  (:require [leiningen.pom :as pom]
            [leiningen.core.classpath :as classpath]
            (leiningen.luxc [compiler :as &compiler]
                            [test :as &test]
                            [repl :as &repl])))

;; [Exports]
(defn luxc [project & args]
  (case (first args)
    "compile"
    (&compiler/compile project)

    "test"
    (&test/test project)

    "repl"
    (&repl/repl project)

    ;; default...
    (println "Commands available: compile, test, repl"))
  )
