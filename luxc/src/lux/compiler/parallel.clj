;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.parallel
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail* |case]])))

;; [Utils]
(def ^:private !state! (ref nil))

(def ^:private get-compiler
  (fn [compiler]
    (return* compiler compiler)))

;; [Exports]
(defn setup!
  "Must always call this function before using parallel compilation to make sure that the state that is being tracked is in proper shape."
  []
  (dosync (ref-set !state! {:compilations {}
                            :cached-modules {}})))

(defn parallel-compilation [compile-module*]
  (fn [module-name]
    (|do [compiler get-compiler
          :let [[task new?] (dosync (if-let [existing-task (get-in @!state! [:compilations module-name])]
                                      (&/T [existing-task false])
                                      (let [new-task (promise)]
                                        (do (alter !state! assoc-in [:compilations module-name] new-task)
                                          (&/T [new-task true])))))
                _ (when new?
                    (.start (new Thread
                                 (fn []
                                   (let [_ (&/|log! (str "PARALLEL COMPILATION: " module-name "\n"))
                                         out-str (with-out-str
                                                   (|case (&/run-state (compile-module* module-name)
                                                                       compiler)
                                                     (&/$Right post-compiler _)
                                                     (deliver task (&/$Right post-compiler))

                                                     (&/$Left ?error)
                                                     (deliver task (&/$Left ?error))))]
                                     (&/|log! out-str))))))]]
      (return task))))

(defn load-cached-module [module-name]
  (dosync (if-let [existing-cache (get-in @!state! [:cached-modules module-name])]
            (return (&/T [true existing-cache]))
            (let [new-cache (promise)]
              (do (alter !state! assoc-in [:cached-modules module-name] new-cache)
                (return (&/T [false new-cache])))))))
