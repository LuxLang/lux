(ns lux.analyser.env
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* |case |let]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def next-local-idx
  (fn [state]
    (return* state (->> state (&/get$ &/$scopes) &/|head (&/get$ &/$locals) (&/get$ &/$counter)))))

(defn with-local [name type body]
  (fn [state]
    (let [old-mappings (->> state (&/get$ &/$scopes) &/|head (&/get$ &/$locals) (&/get$ &/$mappings))
          =return (body (&/update$ &/$scopes
                                   (fn [stack]
                                     (let [var-analysis (&&/|meta type &/empty-location (&&/$var (&/$Local (->> (&/|head stack) (&/get$ &/$locals) (&/get$ &/$counter)))))]
                                       (&/$Cons (&/update$ &/$locals #(->> %
                                                                           (&/update$ &/$counter inc)
                                                                           (&/update$ &/$mappings (fn [m] (&/|put name (&/T [type var-analysis]) m))))
                                                           (&/|head stack))
                                                (&/|tail stack))))
                                   state))]
      (|case =return
        (&/$Right ?state ?value)
        (return* (&/update$ &/$scopes (fn [stack*]
                                        (&/$Cons (&/update$ &/$locals #(->> %
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
    (let [old-mappings (->> state (&/get$ &/$scopes) &/|head (&/get$ &/$locals) (&/get$ &/$mappings))
          =return (body (&/update$ &/$scopes
                                   (fn [stack]
                                     (&/$Cons (&/update$ &/$locals #(->> %
                                                                         (&/update$ &/$mappings (fn [m] (&/|put name
                                                                                                                (&/T [(&&/expr-type* var-analysis)
                                                                                                                      var-analysis])
                                                                                                                m))))
                                                         (&/|head stack))
                                              (&/|tail stack)))
                                   state))]
      (|case =return
        (&/$Right ?state ?value)
        (return* (&/update$ &/$scopes (fn [stack*]
                                        (&/$Cons (&/update$ &/$locals #(->> %
                                                                            (&/set$ &/$mappings old-mappings))
                                                            (&/|head stack*))
                                                 (&/|tail stack*)))
                            ?state)
                 ?value)
        
        _
        =return))))

(def captured-vars
  (fn [state]
    (|case (&/get$ &/$scopes state)
      (&/$Nil)
      ((&/fail-with-loc "[Analyser Error] Cannot obtain captured vars without environments.")
       state)

      (&/$Cons env _)
      (return* state (->> env
                          (&/get$ &/$captured)
                          (&/get$ &/$mappings)
                          (&/|map (fn [mapping]
                                    (|let [[k v] mapping]
                                      (&/T [k (&/|second v)])))))))
    ))
