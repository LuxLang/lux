(ns lux.analyser.def
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail]])
            [lux.analyser.base :as &&]))

(def $DEFS 0)
(def $MACROS 1)

;; [Exports]
(def init-module
  (&/R ;; "lux;defs"
       (&/|table)
       ;; "lux;macros"
       (&/|table)))

(do-template [<name> <category>]
  (defn <name> [module name]
    (fn [state]
      (return* state
               (->> state (&/get$ &/$MODULES) (&/|get module) (&/get$ <category>) (&/|contains? name)))))

  defined? $DEFS
  macro?   $MACROS
  )

(defn declare-macro [module name]
  (fn [state]
    (return* (&/update$ &/$MODULES (fn [ms] (&/|update module (fn [m] (&/update$ $MACROS #(&/|put name true %) m)) ms)) state)
             nil)))

(defn define [module name type]
  (fn [state]
    (let [full-name (str module &/+name-separator+ name)
          bound (&/V "Expression" (&/T (&/V "global" (&/T module name)) type))]
      (matchv ::M/objects [(&/get$ &/$ENVS state)]
        [["lux;Cons" [?env ["lux;Nil" _]]]]
        (return* (->> state
                      (&/update$ &/$MODULES (fn [ms]
                                                 (&/|update module (fn [m]
                                                                     (&/update$ $DEFS #(&/|put full-name type %)
                                                                                m))
                                                            ms)))
                      (&/set$ &/$ENVS (&/|list (&/update$ &/$LOCALS (fn [locals]
                                                                         (&/update$ &/$MAPPINGS (fn [mappings]
                                                                                                     (&/|put full-name bound mappings))
                                                                                    locals))
                                                          ?env))))
                 nil)
        
        [_]
        (fail "[Analyser Error] Can't create a new global definition outside of a global environment."))
      )))

(defn module-exists? [name]
  (fn [state]
    (return* state
             (->> state (&/get$ &/$MODULES) (&/|contains? name)))))

(defn unalias-module [name]
  (fn [state]
    (if-let [real-name (->> state (&/get$ &/$MODULE-ALIASES) (&/|get name))]
      (return* state real-name)
      (fail "Unknown alias."))))
