(ns lux.analyser.module
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail fail*]]
                 [type :as &type])
            [lux.analyser.base :as &&]))

;; [Exports]
(def init-module
  (&/|table))

(defn define [module name def-data]
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$ENVS state)]
      [["lux;Cons" [?env ["lux;Nil" _]]]]
      (return* (->> state
                    (&/update$ &/$MODULES (fn [ms]
                                            (&/|update module #(&/|put name def-data %)
                                                       ms)))
                    (&/set$ &/$ENVS (&/|list (&/update$ &/$LOCALS (fn [locals]
                                                                    (&/update$ &/$MAPPINGS (fn [mappings]
                                                                                             (&/|put (str "" &/+name-separator+ name)
                                                                                                     (&/V "Expression" (&/T (&/V "global" (&/T module name)) &type/$Void))
                                                                                                     mappings))
                                                                               locals))
                                                        ?env))))
               nil)
      
      [_]
      (fail "[Analyser Error] Can't create a new global definition outside of a global environment."))))

(defn exists? [name]
  (fn [state]
    (return* state
             (->> state (&/get$ &/$MODULES) (&/|contains? name)))))

(defn dealias [name]
  (fn [state]
    (if-let [real-name (->> state (&/get$ &/$MODULE-ALIASES) (&/|get name))]
      (return* state real-name)
      (fail (str "Unknown alias: " name)))))

(defn find-def [module name]
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$MODULES) (&/|get module))]
      (if-let [$def (&/|get name $module)]
        (return* state $def)
        (fail* (str "[Analyser Error] Definition doesn't exist: " (str module &/+name-separator+ name))))
      (fail* (str "[Analyser Error] Module doesn't exist: " module)))))

(defn defined? [module name]
  (&/try-all% (&/|list (|do [_ (find-def module name)]
                         (return true))
                       (return false))))

(defn install-macro [module name macro]
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$MODULES) (&/|get module))]
      (if-let [$def (&/|get name $module)]
        (matchv ::M/objects [$def]
          [["lux;MacroD" ["lux;None" _]]]
          (return* (&/update$ &/$MODULES
                              (fn [$modules]
                                (&/|put module (&/|put name (&/V "lux;MacroD" (&/V "lux;Some" macro)) $module)
                                        $modules))
                              state)
                   nil)

          [["lux;MacroD" ["lux;Some" _]]]
          (fail* (str "[Analyser Error] Can't re-install a macro: " (str module &/+name-separator+ name)))

          [_]
          (fail* (str "[Analyser Error] Can't install a non-macro: " (str module &/+name-separator+ name))))
        (fail* (str "[Analyser Error] Definition doesn't exist: " (str module &/+name-separator+ name))))
      (fail* (str "[Analyser Error] Module doesn't exist: " module)))
    ))
