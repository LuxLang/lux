;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.repl
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail* |case]]
                 [type :as &type]
                 [analyser :as &analyser]
                 [optimizer :as &optimizer]
                 [compiler :as &compiler])
            [lux.analyser.base :as &a-base]
            [lux.analyser.module :as &module])
  (:import (java.io InputStreamReader
                    BufferedReader)))

;; [Utils]
(def ^:private repl-module "REPL")

(defn ^:private init []
  (do (&compiler/init!)
    (|case ((|do [_ (&compiler/compile-module "lux")]
              (&module/enter-module repl-module))
            (&/init-state &/$Debug))
      (&/$Right ?state _)
      (do (println)
        (println "Welcome to the REPL!")
        (println "Type \"exit\" to leave.")
        (println)
        (&/set$ &/$source &/$Nil ?state))

      (&/$Left ?message)
      (assert false ?message))
    ))

(defn ^:private repl-cursor [repl-line]
  (&/T [repl-module repl-line 0]))

;; [Values]
(defn repl [source-dirs]
  (with-open [input (->> System/in (new InputStreamReader) (new BufferedReader))]
    (loop [state (init)
           repl-line 0]
      (let [_ (.print System/out "> ")
            line (.readLine input)]
        (if (= "exit" line)
          (println "Till next time...")
          (let [line* (&/|list (&/T [(repl-cursor repl-line) line]))
                state* (&/update$ &/$source
                                  (fn [_source] (&/|++ _source line*))
                                  state)]
            (|case ((|do [analysed-tokens (&analyser/repl-analyse &compiler/eval! (partial &compiler/compile-module source-dirs) &compiler/all-compilers)
                          optimized-tokens (->> analysed-tokens
                                                (&/|map &a-base/expr-term)
                                                (&/map% &optimizer/optimize))
                          :let [optimized-tokens* (&/->list (map (fn [analysis optim]
                                                                   (|let [[[_type _cursor] _term] analysis]
                                                                     (&a-base/|meta _type _cursor optim)))
                                                                 (&/->seq analysed-tokens)
                                                                 (&/->seq optimized-tokens)))]
                          eval-values (&/map% &compiler/eval! optimized-tokens*)
                          :let [outputs (map (fn [analysis value]
                                               (|let [[[_type _cursor] _term] analysis]
                                                 [_type value]))
                                             (&/->seq analysed-tokens)
                                             (&/->seq eval-values))]]
                      (return outputs))
                    state*)
              (&/$Right state** outputs)
              (do (doseq [[_type _value] outputs]
                    (.println System/out (str "=> " (&type/show-type _type) "\n" (pr-str _value) "\n")))
                (recur state** (inc repl-line)))

              (&/$Left ^String ?message)
              (if (or (= "[Reader Error] EOF" ?message)
                      (.startsWith ?message "[Parser Error] Unbalanced "))
                (recur state* (inc repl-line))
                (do (println ?message)
                  (recur state (inc repl-line))))
              ))))
      )))
