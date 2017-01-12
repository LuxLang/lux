;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux
  (:gen-class)
  (:require [lux.base :as & :refer [|let |do return fail return* fail* |case]]
            [lux.compiler.base :as &compiler-base]
            [lux.compiler :as &compiler]
            [lux.repl :as &repl]
            [clojure.string :as string]
            :reload-all)
  (:import (java.io File)))

(def unit-separator (str (char 31)))

(defn ^:private process-dirs
  "(-> Text (List Text))"
  [resources-dirs]
  (-> resources-dirs
      (string/replace unit-separator "\n")
      string/split-lines
      &/->list))

(defn -main [& args]
  (|case (&/->list args)
    (&/$Cons "release" (&/$Cons program-module (&/$Cons resources-dirs (&/$Cons source-dirs (&/$Cons target-dir (&/$Nil))))))
    (time (&compiler/compile-program &/$Release program-module (process-dirs resources-dirs) (process-dirs source-dirs) target-dir))

    (&/$Cons "debug" (&/$Cons program-module (&/$Cons resources-dirs (&/$Cons source-dirs (&/$Cons target-dir (&/$Nil))))))
    (time (&compiler/compile-program &/$Debug program-module (process-dirs resources-dirs) (process-dirs source-dirs) target-dir))

    (&/$Cons "repl" (&/$Cons resources-dirs (&/$Cons source-dirs (&/$Cons target-dir (&/$Nil)))))
    (&repl/repl (process-dirs resources-dirs)
                (process-dirs source-dirs)
                target-dir)

    _
    (println "Can't understand command.")))
