;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.analyser.env
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail |case]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def next-local-idx
  (fn [state]
    (return* state (->> state (&/get$ &/$ENVS) &/|head (&/get$ &/$LOCALS) (&/get$ &/$COUNTER)))))

(defn with-local [name type body]
  ;; (prn 'with-local name)
  (fn [state]
    ;; (prn 'with-local name)
    (let [old-mappings (->> state (&/get$ &/$ENVS) &/|head (&/get$ &/$LOCALS) (&/get$ &/$MAPPINGS))
          =return (body (&/update$ &/$ENVS
                                   (fn [stack]
                                     (let [bound-unit (&/V "lux;Local" (->> (&/|head stack) (&/get$ &/$LOCALS) (&/get$ &/$COUNTER)))]
                                       (&/|cons (&/update$ &/$LOCALS #(->> %
                                                                           (&/update$ &/$COUNTER inc)
                                                                           (&/update$ &/$MAPPINGS (fn [m] (&/|put name (&/T bound-unit type) m))))
                                                           (&/|head stack))
                                                (&/|tail stack))))
                                   state))]
      (|case =return
        (&/$Right ?state ?value)
        (return* (&/update$ &/$ENVS (fn [stack*]
                                      (&/|cons (&/update$ &/$LOCALS #(->> %
                                                                          (&/update$ &/$COUNTER dec)
                                                                          (&/set$ &/$MAPPINGS old-mappings))
                                                          (&/|head stack*))
                                               (&/|tail stack*)))
                            ?state)
                 ?value)
        
        _
        =return))))

(def captured-vars
  (fn [state]
    (return* state (->> state (&/get$ &/$ENVS) &/|head (&/get$ &/$CLOSURE) (&/get$ &/$MAPPINGS)))))
