(ns lux.analyser.module
  (:require [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail fail*]]
                 [type :as &type]
                 [host :as &host])
            [lux.analyser.base :as &&]))

;; [Exports]
(def init-module
  (&/|table))

(defn define [module name def-data type]
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$ENVS state)]
      [["lux;Cons" [?env ["lux;Nil" _]]]]
      (return* (->> state
                    (&/update$ &/$MODULES (fn [ms]
                                            (&/|update module #(&/|put name (&/T false def-data) %)
                                                       ms)))
                    (&/set$ &/$ENVS (&/|list (&/update$ &/$LOCALS (fn [locals]
                                                                    (&/update$ &/$MAPPINGS (fn [mappings]
                                                                                             (&/|put (str "" &/+name-separator+ name)
                                                                                                     (&/T (&/V "lux;Global" (&/T module name)) type)
                                                                                                     mappings))
                                                                               locals))
                                                        ?env))))
               nil)
      
      [_]
      (fail* "[Analyser Error] Can't create a new global definition outside of a global environment."))))

(defn def-alias [a-module a-name r-module r-name type]
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$ENVS state)]
      [["lux;Cons" [?env ["lux;Nil" _]]]]
      (return* (->> state
                    (&/update$ &/$MODULES (fn [ms]
                                            (&/|update a-module #(&/|put a-name (&/T false (&/V "lux;AliasD" (&/T r-module r-name))) %)
                                                       ms)))
                    (&/set$ &/$ENVS (&/|list (&/update$ &/$LOCALS (fn [locals]
                                                                    (&/update$ &/$MAPPINGS (fn [mappings]
                                                                                             (&/|put (str "" &/+name-separator+ a-name)
                                                                                                     (&/T (&/V "lux;Global" (&/T r-module r-name)) type)
                                                                                                     mappings))
                                                                               locals))
                                                        ?env))))
               nil)
      
      [_]
      (fail* "[Analyser Error] Can't alias a global definition outside of a global environment."))))

(defn exists? [name]
  (fn [state]
    (return* state
             (->> state (&/get$ &/$MODULES) (&/|contains? name)))))

(defn dealias [name]
  (fn [state]
    (if-let [real-name (->> state (&/get$ &/$MODULE-ALIASES) (&/|get name))]
      (return* state real-name)
      (fail* (str "Unknown alias: " name)))))

(defn find-def [module name]
  (|do [current-module &/get-module-name]
    (fn [state]
      (if-let [$module (->> state (&/get$ &/$MODULES) (&/|get module))]
        (if-let [$def (&/|get name $module)]
          (matchv ::M/objects [$def]
            [[exported? $$def]]
            (if (or exported? (.equals ^Object current-module module))
              (matchv ::M/objects [$$def]
                [["lux;AliasD" [?r-module ?r-name]]]
                ((find-def ?r-module ?r-name)
                 state)

                [_]
                (return* state (&/T (&/T module name) $$def)))
              (fail* (str "[Analyser Error] Can't use unexported definition: " (str module &/+name-separator+ name)))))
          (fail* (str "[Analyser Error] Definition doesn't exist: " (str module &/+name-separator+ name))))
        (do (prn [module name]
                 (str "[Analyser Error] Module doesn't exist: " module)
                 (->> state (&/get$ &/$MODULES) &/|keys &/->seq))
          (fail* (str "[Analyser Error] Module doesn't exist: " module)))))))

(defn defined? [module name]
  (&/try-all% (&/|list (|do [_ (find-def module name)]
                         (return true))
                       (return false))))

(defn declare-macro [module name]
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$MODULES) (&/|get module))]
      (if-let [$def (&/|get name $module)]
        (matchv ::M/objects [$def]
          [[exported? ["lux;ValueD" ?type]]]
          ((|do [_ (&type/check &type/Macro ?type)
                 ^ClassLoader loader &/loader
                 :let [macro (-> (.loadClass loader (&host/location (&/|list module name)))
                                 (.getField "_datum")
                                 (.get nil))]]
             (fn [state*]
               (return* (&/update$ &/$MODULES
                                   (fn [$modules]
                                     (&/|put module (&/|put name (&/T exported? (&/V "lux;MacroD" macro)) $module)
                                             $modules))
                                   state*)
                        nil)))
           state)
          
          [[_ ["lux;MacroD" _]]]
          (fail* (str "[Analyser Error] Can't re-declare a macro: " (str module &/+name-separator+ name)))

          [[_ ["lux;TypeD" _]]]
          (fail* (str "[Analyser Error] Definition doesn't have macro type: " module ";" name)))
        (fail* (str "[Analyser Error] Definition doesn't exist: " (str module &/+name-separator+ name))))
      (fail* (str "[Analyser Error] Module doesn't exist: " module)))))

(defn export [module name]
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$ENVS state)]
      [["lux;Cons" [?env ["lux;Nil" _]]]]
      (if-let [$def (->> state (&/get$ &/$MODULES) (&/|get module) (&/|get name))]
        (matchv ::M/objects [$def]
          [[true _]]
          (fail* (str "[Analyser Error] Definition has already been exported: " module ";" name))

          [[false ?data]]
          (return* (->> state
                        (&/update$ &/$MODULES (fn [ms]
                                                (&/|update module #(&/|put name (&/T true ?data) %)
                                                           ms))))
                   nil))
        (fail* (str "[Analyser Error] Can't export an inexistent definition: " module ";" name)))
      
      [_]
      (fail* "[Analyser Error] Can't export a global definition outside of a global environment."))))
