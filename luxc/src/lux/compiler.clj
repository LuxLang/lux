(ns lux.compiler
  (:refer-clojure :exclude [compile])
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case]])
            (lux.compiler [core :as &&core]
                          [io :as &&io]
                          [parallel :as &&parallel]
                          [jvm :as &&jvm])))

(defn init! [resources-dirs ^String target-dir]
  (do (reset! &&core/!output-dir target-dir)
    (&&parallel/setup!)
    (&&io/init-libs!)
    (.mkdirs (new java.io.File target-dir))
    (&&jvm/init! resources-dirs target-dir)))

(def all-compilers
  &&jvm/all-compilers)

(defn eval! [expr]
  (&&jvm/eval! expr))

(defn compile-module [source-dirs name]
  (&&jvm/compile-module source-dirs name))

(defn compile-program [mode program-module resources-dir source-dirs target-dir]
  (init! resources-dir target-dir)
  (&&jvm/compile-program mode program-module resources-dir source-dirs target-dir))
