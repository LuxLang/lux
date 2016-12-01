;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns leiningen.lux
  (:require [leiningen.pom :as pom]
            [leiningen.core.classpath :as classpath]
            (leiningen.lux [builder :as &builder]
                           [test :as &test]
                           [repl :as &repl]
                           [watch :as &watch])))

;; [Exports]
(defn lux [project & args]
  (case (first args)
    "build"
    (&builder/build project)

    "test"
    (&test/test project)

    "repl"
    (&repl/repl project)

    "watch"
    (case (second args)
      "build"
      (&watch/watch #(&builder/build project) project)

      "test"
      (&watch/watch #(&test/test project) project))

    ;; default...
    (println "Commands available: (watch) build, (watch) test, repl"))
  )
