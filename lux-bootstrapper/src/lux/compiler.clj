;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case]])
            (lux.compiler [core :as &&core]
                          [io :as &&io]
                          [parallel :as &&parallel]
                          [jvm :as &&jvm])))

(defn init! [dependencies ^String target-dir]
  (do (reset! &&core/!output-dir target-dir)
    (&&parallel/setup!)
    (&&io/init-libs! dependencies)
    (.mkdirs (new java.io.File target-dir))
    (&&jvm/init!)))

(def all-compilers
  &&jvm/all-compilers)

(defn eval! [expr]
  (&&jvm/eval! expr))

(defn compile-module [source-dirs name]
  (&&jvm/compile-module source-dirs name))

(defn compile-program [mode program-module program-definition dependencies source-dirs target-dir]
  (init! dependencies target-dir)
  (&&jvm/compile-program mode program-module program-definition source-dirs))
