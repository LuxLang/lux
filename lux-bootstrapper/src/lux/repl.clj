;; This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;; If a copy of the MPL was not distributed with this file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns lux.repl
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case]]
                 [type :as &type]
                 [analyser :as &analyser]
                 [optimizer :as &optimizer]
                 [compiler :as &compiler])
            [lux.compiler.cache :as &cache]
            (lux.analyser [base :as &a-base]
                          [lux :as &a-lux]
                          [module :as &module]))
  (:import (java.io InputStreamReader
                    BufferedReader)))

;; [Utils]
(def ^:private repl-module "REPL")

(defn ^:private repl-location [repl-line]
  (&/T [repl-module repl-line 0]))

(defn ^:private init [source-dirs]
  (do (&compiler/init!)
    (|case ((|do [_ (&compiler/compile-module source-dirs &/prelude)
                  _ (&cache/delete repl-module)
                  _ (&module/create-module repl-module 0)
                  _ (fn [?state]
                      (return* (&/set$ &/$source
                                       (&/|list (&/T [(repl-location -1) "(;module: lux)"]))
                                       ?state)
                               nil))
                  analysed-tokens (&analyser/repl-analyse &optimizer/optimize &compiler/eval! (partial &compiler/compile-module source-dirs) &compiler/all-compilers)
                  eval-values (->> analysed-tokens (&/|map &optimizer/optimize) (&/map% &compiler/eval!))]
              (return nil))
            (&/init-state &/$REPL))
      (&/$Right ?state _)
      (do (println)
        (println "Welcome to the REPL!")
        (println "Type \"exit\" to leave.")
        (println)
        ?state)

      (&/$Left ?message)
      (do (println (str "Initialization failed:\n" ?message))
        (flush)
        (System/exit 1)))
    ))

;; [Values]
(defn repl [dependencies source-dirs target-dir]
  (with-open [input (->> System/in (new InputStreamReader) (new BufferedReader))]
    (loop [state (init source-dirs)
           repl-line 0
           multi-line? false]
      (let [_ (if (not multi-line?)
                (.print System/out "> ")
                (.print System/out "  "))
            line (.readLine input)]
        (if (= "exit" line)
          (println "Till next time...")
          (let [line* (&/|list (&/T [(repl-location repl-line) line]))
                state* (&/update$ &/$source
                                  (fn [_source] (&/|++ _source line*))
                                  state)]
            (|case ((|do [analysed-tokens (&analyser/repl-analyse &optimizer/optimize &compiler/eval! (partial &compiler/compile-module source-dirs) &compiler/all-compilers)
                          eval-values (->> analysed-tokens (&/|map &optimizer/optimize) (&/map% &compiler/eval!))
                          :let [outputs (map (fn [analysis value]
                                               (|let [[[_type _location] _term] analysis]
                                                 [_type value]))
                                             (&/->seq analysed-tokens)
                                             (&/->seq eval-values))]]
                      (return outputs))
                    state*)
              (&/$Right state** outputs)
              (do (doseq [[_type _value] outputs]
                    (.println System/out (str ": " (&type/show-type _type) "\n"
                                              "=> " (pr-str _value) "\n")))
                (recur state** (inc repl-line) false))

              (&/$Left ^String ?message)
              (if (or (= "[Reader Error] EOF" ?message)
                      (.contains ?message "[Parser Error] Unbalanced "))
                (recur state* (inc repl-line) true)
                (do (println ?message)
                  (recur state (inc repl-line) false)))
              ))))
      )))
