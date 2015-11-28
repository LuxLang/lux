;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.env
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail fail* |case]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def next-local-idx
  (fn [state]
    (return* state (->> state (&/get$ &/$envs) &/|head (&/get$ &/$locals) (&/get$ &/$counter)))))

(defn with-local [name type body]
  (fn [state]
    (let [old-mappings (->> state (&/get$ &/$envs) &/|head (&/get$ &/$locals) (&/get$ &/$mappings))
          =return (body (&/update$ &/$envs
                                   (fn [stack]
                                     (let [var-analysis (&&/|meta type &/empty-cursor (&/V &&/$var (&/V &/$Local (->> (&/|head stack) (&/get$ &/$locals) (&/get$ &/$counter)))))]
                                       (&/Cons$ (&/update$ &/$locals #(->> %
                                                                           (&/update$ &/$counter inc)
                                                                           (&/update$ &/$mappings (fn [m] (&/|put name var-analysis m))))
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

(defn with-alias [name var-analysis body]
  (fn [state]
    (let [old-mappings (->> state (&/get$ &/$envs) &/|head (&/get$ &/$locals) (&/get$ &/$mappings))
          =return (body (&/update$ &/$envs
                                   (fn [stack]
                                     (&/Cons$ (&/update$ &/$locals #(->> %
                                                                         (&/update$ &/$mappings (fn [m] (&/|put name var-analysis m))))
                                                         (&/|head stack))
                                              (&/|tail stack)))
                                   state))]
      (|case =return
        (&/$Right ?state ?value)
        (return* (&/update$ &/$envs (fn [stack*]
                                      (&/Cons$ (&/update$ &/$locals #(->> %
                                                                          (&/set$ &/$mappings old-mappings))
                                                          (&/|head stack*))
                                               (&/|tail stack*)))
                            ?state)
                 ?value)
        
        _
        =return))))

(def captured-vars
  (fn [state]
    (|case (&/get$ &/$envs state)
      (&/$Nil)
      (fail* "[Analyser Error] Can't obtain captured vars without environments.")

      (&/$Cons env _)
      (return* state (->> env (&/get$ &/$closure) (&/get$ &/$mappings))))
    ))
