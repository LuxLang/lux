;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux
  (:gen-class)
  (:require [lux.base :as &]
            [lux.compiler :as &compiler]
            :reload-all))

(defn -main [& [program-module & _]]
  (if program-module
    (time (&compiler/compile-program program-module))
    (println "Please provide a module name to compile."))
  (System/exit 0)
  )

(comment
  (-main "program")
  )
