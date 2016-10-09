;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.parallel
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.async
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail* |case]])))

;; [Utils]
(def ^:private !state! (atom {}))

(def ^:private get-compiler
  (fn [compiler]
    (return* compiler compiler)))

(defn ^:private set-compiler [compiler*]
  (fn [_]
    (return* compiler* compiler*)))

(defn ^:private merge-modules
  "(-> Compiler Compiler)"
  [new old]
  (->> old
       (&/set$ &/$source (&/get$ &/$source new))
       (&/set$ &/$modules (&/get$ &/$modules new))
       (&/set$ &/$seed (&/get$ &/$seed new))
       (&/set$ &/$host (&/get$ &/$host new))))

;; [Exports]
(defn setup!
  "Must always call this function before using parallel compilation to make sure that the state that is being tracked is in proper shape."
  []
  (reset! !state! {}))

(defn parallel-compilation [compile-module*]
  (fn [module-name]
    (|do [:let [_ (prn 'parallel-compilation module-name)]
          pre get-compiler
          output (compile-module* module-name)
          post get-compiler
          post* (set-compiler (merge-modules post pre))
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
      (return output)
      )))
