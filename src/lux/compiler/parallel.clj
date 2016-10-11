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
            (lux [base :as & :refer [|let |do return* return fail fail* |case]])
            (lux.analyser [base :as &a]
                          [module :as &a-module])))

;; [Utils]
(def ^:private !state! (ref {}))

(def ^:private get-compiler
  (fn [compiler]
    (return* compiler compiler)))

(defn ^:private compilation-task [compile-module* module-name]
  (|do [current-module &/get-module-name
        compiler get-compiler
        :let [[task new?] (dosync (if-let [existing-task (get @!state! module-name)]
                                    (&/T [existing-task false])
                                    (let [new-task (promise)]
                                      (do (alter !state! assoc module-name new-task)
                                        (&/T [new-task true])))))
              result (when new?
                       (doto (new Thread
                                  (fn []
                                    (do ;; (&/|log! 'THREAD-START [current-module module-name])
                                      (|case (&/run-state (|do [_ (compile-module* module-name)
                                                                _module (&a-module/find-module module-name)
                                                                ;; :let [_ (&/|log! "Just compiled:" module-name (->> _module
                                                                ;;                                                    (&/get$ &a-module/$defs)
                                                                ;;                                                    &/|length))]
                                                                ]
                                                            (return nil))
                                                          compiler)
                                        (&/$Right post-compiler _)
                                        (do ;; (&/|log! 'FINISHED 'compilation-task [current-module module-name] post-compiler)
                                          (deliver task post-compiler))

                                        (&/$Left ?error)
                                        (do (&/|log! ?error)
                                          ;; (System/exit 1)
                                          )))))
                         (.start)))
              ;; _ (&/|log! 'parallel-compilation [current-module module-name] new? result)
              ]]
    (return task)))

;; [Exports]
(defn setup!
  "Must always call this function before using parallel compilation to make sure that the state that is being tracked is in proper shape."
  []
  (dosync (ref-set !state! {})))

(defn parallel-compilation [compile-module*]
  (fn [module-name]
    (|do [;; pre get-compiler
          ?async (compilation-task compile-module* module-name)
          ;; post get-compiler
          ;; post* (set-compiler (merge-compilers post pre))
          ;; TODO: Some type-vars in the typing environment stay in
          ;; the environment forever, making type-checking slower.
          ;; The merging process for modules more-or-less "fixes" the
          ;; problem by resetting the typing enviroment, but ideally
          ;; those type-vars shouldn't survive in the first place.
          ;; MUST FIX
          ;; :let [_ (prn 'parallel-compilation module-name
          ;;              'PRE (->> pre (&/get$ &/$type-vars) (&/get$ &/$mappings) &/|length)
          ;;              'POST (->> post (&/get$ &/$type-vars) (&/get$ &/$mappings) &/|length)
          ;;              'POST* (->> post* (&/get$ &/$type-vars) (&/get$ &/$mappings) &/|length))]
          ]
      (return ?async)
      )))
