;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux
  (:gen-class)
  (:require [lux.base :as & :refer [|let |do return fail return* fail* |case]]
            [lux.compiler.base :as &compiler-base]
            [lux.compiler :as &compiler]
            :reload-all)
  (:import (java.io File)))

(defn -main [& args]
  (|case (&/->list args)
    (&/$Cons "compile" (&/$Cons program-module (&/$Nil)))
    (time (&compiler/compile-program &/$Release program-module))

    (&/$Cons "compile" (&/$Cons "release" (&/$Cons program-module (&/$Nil))))
    (time (&compiler/compile-program &/$Release program-module))

    (&/$Cons "compile" (&/$Cons "debug" (&/$Cons program-module (&/$Nil))))
    (time (&compiler/compile-program &/$Debug program-module))

    _
    (println "Can't understand command.")))
