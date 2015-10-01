;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux
  (:gen-class)
  (:require [lux.base :as & :refer [|let |do return fail return* fail* |case]]
            [lux.compiler.base :as &compiler-base]
            [lux.compiler :as &compiler]
            [lux.packager.lib :as &lib]
            :reload-all)
  (:import (java.io File)))

(defn -main [& args]
  (|case (&/->list args)
    (&/$Cons "compile" (&/$Cons program-module (&/$Nil)))
    (if program-module
      (time (&compiler/compile-program program-module))
      (println "Please provide a module name to compile."))

    (&/$Cons "lib" (&/$Cons lib-module (&/$Nil)))
    (&lib/package lib-module (new File &compiler-base/input-dir))

    _
    (println "Can't understand command."))
  (System/exit 0)
  )

(comment
  (-main "compile" "program")
  (-main "lib" "lux")
  )
