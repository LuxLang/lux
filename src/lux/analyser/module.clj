(ns lux.analyser.module
  (:require [clojure.string :as string]
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return return* fail fail*]]
                 [type :as &type]
                 [host :as &host])
            [lux.analyser.base :as &&]))

;; [Utils]
(def ^:private $DEFS 0)
(def ^:private $ALIASES 1)
(def ^:private $IMPORTS 2)

;; [Exports]
(def init-module
  (&/R ;; "lux;defs"
   (&/|table)
   ;; "lux;module-aliases"
   (&/|table)
   ;; "lux;imports"
   (&/|list)
   ))

(defn add-import [module]
  "(-> Text (Lux (,)))"
  (|do [current-module &/get-module-name]
    (fn [state]
      (return* (&/update$ &/$MODULES
                          (fn [ms]
                            (&/|update current-module
                                       (fn [m] (&/update$ $IMPORTS (partial &/|cons module) m))
                                       ms))
                          state)
               nil))))

(defn define [module name def-data type]
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$ENVS state)]
      [["lux;Cons" [?env ["lux;Nil" _]]]]
      (return* (->> state
                    (&/update$ &/$MODULES
                               (fn [ms]
                                 (&/|update module
                                            (fn [m]
                                              (&/update$ $DEFS
                                                         #(&/|put name (&/T false def-data) %)
                                                         m))
                                            ms)))
                    ;; (&/set$ &/$ENVS (&/|list (&/update$ &/$LOCALS (fn [locals]
                    ;;                                                 (&/update$ &/$MAPPINGS (fn [mappings]
                    ;;                                                                          (&/|put (str "" &/+name-separator+ name)
                    ;;                                                                                  (&/T (&/V "lux;Global" (&/T module name)) type)
                    ;;                                                                                  mappings))
                    ;;                                                            locals))
                    ;;                                     ?env)))
                    )
               nil)
      
      [_]
      (fail* (str "[Analyser Error] Can't create a new global definition outside of a global environment: " module ";" name)))))

(defn def-type [module name]
  "(-> Text Text (Lux Type))"
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$MODULES) (&/|get module))]
      (if-let [$def (->> $module (&/get$ $DEFS) (&/|get name))]
        (matchv ::M/objects [$def]
          [["lux;TypeD" _]]
          (return* state &type/Type)

          [["lux;MacroD" _]]
          (return* state &type/Macro)

          [["lux;ValueD" _type]]
          (return* state _type)

          [["lux;AliasD" [?r-module ?r-name]]]
          (&/run-state (def-type ?r-module ?r-name)
                       state))
        (fail* (str "[Analyser Error] Unknown definition: " (str module ";" name))))
      (fail* (str "[Analyser Error] Unknown module: " module)))))

(defn def-alias [a-module a-name r-module r-name type]
  ;; (prn 'def-alias [a-module a-name] [r-module r-name] (&type/show-type type))
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$ENVS state)]
      [["lux;Cons" [?env ["lux;Nil" _]]]]
      (return* (->> state
                    (&/update$ &/$MODULES
                               (fn [ms]
                                 (&/|update a-module
                                            (fn [m]
                                              (&/update$ $DEFS
                                                         #(&/|put a-name (&/T false (&/V "lux;AliasD" (&/T r-module r-name))) %)
                                                         m))
                                            ms)))
                    ;; (&/set$ &/$ENVS (&/|list (&/update$ &/$LOCALS (fn [locals]
                    ;;                                                 (&/update$ &/$MAPPINGS (fn [mappings]
                    ;;                                                                          (&/|put (str "" &/+name-separator+ a-name)
                    ;;                                                                                  (&/T (&/V "lux;Global" (&/T r-module r-name)) type)
                    ;;                                                                                  ;; (aget (->> state (&/get$ &/$MODULES) (&/|get r-module) (&/get$ $DEFS) (&/|get r-name)) 1)
                    ;;                                                                                  mappings))
                    ;;                                                            locals))
                    ;;                                     ?env)))
                    )
               nil)
      
      [_]
      (fail* "[Analyser Error] Can't alias a global definition outside of a global environment."))))

(defn exists? [name]
  "(-> Text (Lux Bool))"
  (fn [state]
    (return* state
             (->> state (&/get$ &/$MODULES) (&/|contains? name)))))

(defn alias [module alias reference]
  (fn [state]
    (return* (->> state
                  (&/update$ &/$MODULES
                             (fn [ms]
                               (&/|update module
                                          #(&/update$ $ALIASES
                                                      (fn [aliases]
                                                        (&/|put alias reference aliases))
                                                      %)
                                          ms))))
             nil)))

(defn dealias [name]
  (|do [current-module &/get-module-name]
    (fn [state]
      (if-let [real-name (->> state (&/get$ &/$MODULES) (&/|get current-module) (&/get$ $ALIASES) (&/|get name))]
        (return* state real-name)
        (fail* (str "Unknown alias: " name))))))

(defn find-def [module name]
  (|do [current-module &/get-module-name]
    (fn [state]
      ;; (prn 'find-def/_0 module name 'current-module current-module)
      (if-let [$module (->> state (&/get$ &/$MODULES) (&/|get module))]
        (do ;; (prn 'find-def/_0.1 module (&/->seq (&/|keys $module)))
            (if-let [$def (->> $module (&/get$ $DEFS) (&/|get name))]
              (matchv ::M/objects [$def]
                [[exported? $$def]]
                (do ;; (prn 'find-def/_1 module name 'exported? exported? (.equals ^Object current-module module))
                    (if (or exported? (.equals ^Object current-module module))
                      (matchv ::M/objects [$$def]
                        [["lux;AliasD" [?r-module ?r-name]]]
                        (do ;; (prn 'find-def/_2 [module name] [?r-module ?r-name])
                            ((find-def ?r-module ?r-name)
                             state))

                        [_]
                        (return* state (&/T (&/T module name) $$def)))
                      (fail* (str "[Analyser Error] Can't use unexported definition: " (str module &/+name-separator+ name))))))
              (fail* (str "[Analyser Error] Definition does not exist: " (str module &/+name-separator+ name)))))
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
    (if-let [$module (->> state (&/get$ &/$MODULES) (&/|get module) (&/get$ $DEFS))]
      (if-let [$def (&/|get name $module)]
        (matchv ::M/objects [$def]
          [[exported? ["lux;ValueD" ?type]]]
          ((|do [_ (&type/check &type/Macro ?type)
                 ^ClassLoader loader &/loader
                 :let [macro (-> (.loadClass loader (str (string/replace module #"/" ".") ".$" (&/normalize-ident name)))
                                 (.getField "_datum")
                                 (.get nil))]]
             (fn [state*]
               (return* (&/update$ &/$MODULES
                                   (fn [$modules]
                                     (&/|update module
                                                (fn [m]
                                                  (&/update$ $DEFS
                                                             #(&/|put name (&/T exported? (&/V "lux;MacroD" macro)) %)
                                                             m))
                                                $modules))
                                   state*)
                        nil)))
           state)
          
          [[_ ["lux;MacroD" _]]]
          (fail* (str "[Analyser Error] Can't re-declare a macro: " (str module &/+name-separator+ name)))

          [[_ ["lux;TypeD" _]]]
          (fail* (str "[Analyser Error] Definition does not have macro type: " (str module &/+name-separator+ name))))
        (fail* (str "[Analyser Error] Definition does not exist: " (str module &/+name-separator+ name))))
      (fail* (str "[Analyser Error] Module does not exist: " module)))))

(defn export [module name]
  (fn [state]
    (matchv ::M/objects [(&/get$ &/$ENVS state)]
      [["lux;Cons" [?env ["lux;Nil" _]]]]
      (if-let [$def (->> state (&/get$ &/$MODULES) (&/|get module) (&/get$ $DEFS) (&/|get name))]
        (matchv ::M/objects [$def]
          [[true _]]
          (fail* (str "[Analyser Error] Definition has already been exported: " module ";" name))

          [[false ?data]]
          (return* (->> state
                        (&/update$ &/$MODULES (fn [ms]
                                                (&/|update module (fn [m]
                                                                    (&/update$ $DEFS
                                                                               #(&/|put name (&/T true ?data) %)
                                                                               m))
                                                           ms))))
                   nil))
        (fail* (str "[Analyser Error] Can't export an inexistent definition: " (str module &/+name-separator+ name))))
      
      [_]
      (fail* "[Analyser Error] Can't export a global definition outside of a global environment."))))

(def defs
  (|do [module &/get-module-name]
    (fn [state]
      (return* state
               (&/|map (fn [kv]
                         (|let [[k v] kv]
                           (matchv ::M/objects [v]
                             [[?exported? ?def]]
                             (do ;; (prn 'defs k ?exported?)
                               (matchv ::M/objects [?def]
                                 [["lux;AliasD" [?r-module ?r-name]]]
                                 (&/T ?exported? k (str "A" ?r-module ";" ?r-name))
                                 
                                 [["lux;MacroD" _]]
                                 (&/T ?exported? k "M")

                                 [["lux;TypeD" _]]
                                 (&/T ?exported? k "T")

                                 [_]
                                 (&/T ?exported? k "V"))))))
                       (->> state (&/get$ &/$MODULES) (&/|get module) (&/get$ $DEFS)))))))

(def imports
  (|do [module &/get-module-name]
    (fn [state]
      (return* state (->> state (&/get$ &/$MODULES) (&/|get module) (&/get$ $IMPORTS))))))
