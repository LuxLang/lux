;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.env
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail |case]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def next-local-idx
  (fn [state]
    (return* state (->> state (&/get$ &/$envs) &/|head (&/get$ &/$locals) (&/get$ &/$counter)))))

(defn with-local [name type body]
  ;; (prn 'with-local name)
  (fn [state]
    ;; (prn 'with-local name)
    (let [old-mappings (->> state (&/get$ &/$envs) &/|head (&/get$ &/$locals) (&/get$ &/$mappings))
          =return (body (&/update$ &/$envs
                                   (fn [stack]
                                     (let [bound-unit (&/V &&/$var (&/V &/$Local (->> (&/|head stack) (&/get$ &/$locals) (&/get$ &/$counter))))]
                                       (&/Cons$ (&/update$ &/$locals #(->> %
                                                                           (&/update$ &/$counter inc)
                                                                           (&/update$ &/$mappings (fn [m] (&/|put name (&/T bound-unit type) m))))
                                                           (&/|head stack))
                                                (&/|tail stack))))
                                   state))]
      (|case =return
        (&/$Right ?state ?value)
        (return* (&/update$ &/$envs (fn [stack*]
                                      (&/Cons$ (&/update$ &/$locals #(->> %
                                                                          (&/update$ &/$counter dec)
                                                                          (&/set$ &/$mappings old-mappings))
                                                          (&/|head stack*))
                                               (&/|tail stack*)))
                            ?state)
                 ?value)
        
        _
        =return))))

(def captured-vars
  (fn [state]
    (return* state (->> state (&/get$ &/$envs) &/|head (&/get$ &/$closure) (&/get$ &/$mappings)))))
