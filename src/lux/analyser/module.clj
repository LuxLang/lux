;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.analyser.module
  (:refer-clojure :exclude [alias])
  (:require [clojure.string :as string]
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return return* fail fail* |case]]
                 [type :as &type]
                 [host :as &host])
            [lux.analyser.base :as &&]))

;; [Utils]
(def ^:private $DEFS 0)
(def ^:private $ALIASES 1)
(def ^:private $IMPORTS 2)
(def ^:private +init+
  (&/R ;; "lux;defs"
   (&/|table)
   ;; "lux;module-aliases"
   (&/|table)
   ;; "lux;imports"
   (&/|list)
   ))

;; [Exports]
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
    (|case (&/get$ &/$ENVS state)
      (&/$Cons ?env (&/$Nil))
      (return* (->> state
                    (&/update$ &/$MODULES
                               (fn [ms]
                                 (&/|update module
                                            (fn [m]
                                              (&/update$ $DEFS
                                                         #(&/|put name (&/T false def-data) %)
                                                         m))
                                            ms))))
               nil)
      
      _
      (fail* (str "[Analyser Error] Can't create a new global definition outside of a global environment: " module ";" name)))))

(defn def-type [module name]
  "(-> Text Text (Lux Type))"
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$MODULES) (&/|get module))]
      (if-let [$def (->> $module (&/get$ $DEFS) (&/|get name))]
        (|case $def
          [_ ("lux;TypeD" _)]
          (return* state &type/Type)

          [_ ("lux;MacroD" _)]
          (return* state &type/Macro)

          [_ ("lux;ValueD" _type _)]
          (return* state _type)

          [_ ("lux;AliasD" ?r-module ?r-name)]
          (&/run-state (def-type ?r-module ?r-name)
                       state))
        (fail* (str "[Analyser Error] Unknown definition: " (str module ";" name))))
      (fail* (str "[Analyser Error] Unknown module: " module)))))

(defn def-alias [a-module a-name r-module r-name type]
  ;; (prn 'def-alias [a-module a-name] [r-module r-name] (&type/show-type type))
  (fn [state]
    (|case (&/get$ &/$ENVS state)
      (&/$Cons ?env (&/$Nil))
      (return* (->> state
                    (&/update$ &/$MODULES
                               (fn [ms]
                                 (&/|update a-module
                                            (fn [m]
                                              (&/update$ $DEFS
                                                         #(&/|put a-name (&/T false (&/V "lux;AliasD" (&/T r-module r-name))) %)
                                                         m))
                                            ms))))
               nil)
      
      _
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
              (|let [[exported? $$def] $def]
                (do ;; (prn 'find-def/_1 module name 'exported? exported? (.equals ^Object current-module module))
                    (if (or exported? (.equals ^Object current-module module))
                      (|case $$def
                        ("lux;AliasD" ?r-module ?r-name)
                        (do ;; (prn 'find-def/_2 [module name] [?r-module ?r-name])
                            ((find-def ?r-module ?r-name)
                             state))

                        _
                        (return* state (&/T (&/T module name) $$def)))
                      (fail* (str "[Analyser Error] Can't use unexported definition: " (str module &/+name-separator+ name))))))
              (fail* (str "[Analyser Error] Definition does not exist: " (str module &/+name-separator+ name)))))
        (fail* (str "[Analyser Error] Module doesn't exist: " module))))))

(defn defined? [module name]
  (&/try-all% (&/|list (|do [_ (find-def module name)]
                         (return true))
                       (return false))))

(defn declare-macro [module name]
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$MODULES) (&/|get module) (&/get$ $DEFS))]
      (if-let [$def (&/|get name $module)]
        (|case $def
          [exported? ("lux;ValueD" ?type _)]
          ((|do [_ (&type/check &type/Macro ?type)
                 ^ClassLoader loader &/loader
                 :let [macro (-> (.loadClass loader (str (&host/->module-class module) "." (&/normalize-name name)))
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
          
          [_ ("lux;MacroD" _)]
          (fail* (str "[Analyser Error] Can't re-declare a macro: " (str module &/+name-separator+ name)))

          [_ _]
          (fail* (str "[Analyser Error] Definition does not have macro type: " (str module &/+name-separator+ name))))
        (fail* (str "[Analyser Error] Definition does not exist: " (str module &/+name-separator+ name))))
      (fail* (str "[Analyser Error] Module does not exist: " module)))))

(defn export [module name]
  (fn [state]
    (|case (&/get$ &/$ENVS state)
      (&/$Cons ?env (&/$Nil))
      (if-let [$def (->> state (&/get$ &/$MODULES) (&/|get module) (&/get$ $DEFS) (&/|get name))]
        (|case $def
          [true _]
          (fail* (str "[Analyser Error] Definition has already been exported: " module ";" name))

          [false ?data]
          (return* (->> state
                        (&/update$ &/$MODULES (fn [ms]
                                                (&/|update module (fn [m]
                                                                    (&/update$ $DEFS
                                                                               #(&/|put name (&/T true ?data) %)
                                                                               m))
                                                           ms))))
                   nil))
        (fail* (str "[Analyser Error] Can't export an inexistent definition: " (str module &/+name-separator+ name))))
      
      _
      (fail* "[Analyser Error] Can't export a global definition outside of a global environment."))))

(def defs
  (|do [module &/get-module-name]
    (fn [state]
      (return* state
               (&/|map (fn [kv]
                         (|let [[k [?exported? ?def]] kv]
                           (do ;; (prn 'defs k ?exported?)
                               (|case ?def
                                 ("lux;AliasD" ?r-module ?r-name)
                                 (&/T ?exported? k (str "A" ?r-module ";" ?r-name))
                                 
                                 ("lux;MacroD" _)
                                 (&/T ?exported? k "M")

                                 ("lux;TypeD" _)
                                 (&/T ?exported? k "T")

                                 _
                                 (&/T ?exported? k "V")))))
                       (->> state (&/get$ &/$MODULES) (&/|get module) (&/get$ $DEFS)))))))

(def imports
  (|do [module &/get-module-name]
    (fn [state]
      (return* state (->> state (&/get$ &/$MODULES) (&/|get module) (&/get$ $IMPORTS))))))

(defn create-module [name]
  (fn [state]
    (return* (&/update$ &/$MODULES #(&/|put name +init+ %) state) nil)))

(defn enter-module [name]
  (fn [state]
    (return* (->> state
                  (&/update$ &/$MODULES #(&/|put name +init+ %))
                  (&/set$ &/$ENVS (&/|list (&/env name))))
             nil)))
