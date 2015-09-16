;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.lambda
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return fail |case]]
                 [host :as &host])
            (lux.analyser [base :as &&]
                          [env :as &env])))

;; [Resource]
(defn with-lambda [self self-type arg arg-type body]
  (&/with-closure
    (|do [scope-name &/get-scope-name]
      (&env/with-local self self-type
        (&env/with-local arg arg-type
          (|do [=return body
                =captured &env/captured-vars]
            (return (&/T scope-name =captured =return))))))))

(defn close-over [scope name register frame]
  (|let [[[register-type register-cursor] _] register
         register* (&&/|meta register-type register-cursor
                             (&/V &&/$captured (&/T scope
                                                    (->> frame (&/get$ &/$closure) (&/get$ &/$counter))
                                                    register)))]
    (&/T register* (&/update$ &/$closure #(->> %
                                               (&/update$ &/$counter inc)
                                               (&/update$ &/$mappings (fn [mps] (&/|put name register* mps))))
                              frame))))
