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
    (return* state (->> state (&/$get-envs) &/|head (&/$get-locals) (&/$get-counter)))))

(defn with-local [name type body]
  ;; (prn 'with-local name)
  (fn [state]
    ;; (prn 'with-local name)
    (let [old-mappings (->> state (&/$get-envs) &/|head (&/$get-locals) (&/$get-mappings))
          =return (body (&/$update-envs
                         (fn [stack]
                           (let [bound-unit (&/S &&/$var (&/S &/$Local (->> (&/|head stack) (&/$get-locals) (&/$get-counter))))]
                             (&/Cons$ (&/$update-locals #(->> %
                                                              (&/$update-counter inc)
                                                              (&/$update-mappings (fn [m] (&/|put name (&/P bound-unit type) m))))
                                                        (&/|head stack))
                                      (&/|tail stack))))
                         state))]
      (|case =return
        (&/$Right ?state ?value)
        (return* (&/$update-envs (fn [stack*]
                                   (&/Cons$ (&/$update-locals #(->> %
                                                                    (&/$update-counter dec)
                                                                    (&/$set-mappings old-mappings))
                                                              (&/|head stack*))
                                            (&/|tail stack*)))
                                 ?state)
                 ?value)
        
        _
        =return))))

(def captured-vars
  (fn [state]
    (return* state (->> state (&/$get-envs) &/|head (&/$get-closure) (&/$get-mappings)))))
