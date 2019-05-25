(ns lux.compiler.parallel
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case]])))

;; [Utils]
(def ^:private !state! (ref {}))

(def ^:private get-compiler
  (fn [compiler]
    (return* compiler compiler)))

;; [Exports]
(defn setup!
  "Must always call this function before using parallel compilation to make sure that the state that is being tracked is in proper shape."
  []
  (dosync (ref-set !state! {})))

(defn parallel-compilation [compile-module*]
  (fn [module-name]
    (|do [compiler get-compiler
          :let [[task new?] (dosync (if-let [existing-task (get @!state! module-name)]
                                      (&/T [existing-task false])
                                      (let [new-task (promise)]
                                        (do (alter !state! assoc module-name new-task)
                                          (&/T [new-task true])))))
                _ (when new?
                    (.start (new Thread
                                 (fn []
                                   (let [out-str (with-out-str
                                                   (try (|case (&/run-state (compile-module* module-name)
                                                                            compiler)
                                                          (&/$Right post-compiler _)
                                                          (deliver task (&/$Right post-compiler))

                                                          (&/$Left ?error)
                                                          (deliver task (&/$Left ?error)))
                                                     (catch Throwable ex
                                                       (.printStackTrace ex)
                                                       (deliver task (&/$Left "")))))]
                                     (&/|log! out-str))))))]]
      (return task))))
