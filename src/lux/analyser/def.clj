(ns lux.analyser.def
  (:require (clojure [template :refer [do-template]])
            [clojure.core.match :as M :refer [matchv]]
            clojure.core.match.array
            (lux [base :as & :refer [|do return return* fail]])
            [lux.analyser.base :as &&]))

;; [Exports]
(def init-module
  (&/R "lux;defs" (&/|table)
       "lux;macros" (&/|table)))

(do-template [<name> <category>]
  (defn <name> [module name]
    (fn [state]
      (return* state
               (->> state (&/get$ "lux;modules") (&/|get module) (&/get$ <category>) (&/|contains? name)))))

  defined? "lux;defs"
  macro?   "lux;macros"
  )

(defn declare-macro [module name]
  (fn [state]
    (return* (&/update$ "lux;modules" (fn [ms] (&/|update module (fn [m] (&/update$ "lux;macros" #(&/|put name true %) m)) ms)) state)
             nil)))

(defn define [module name type]
  (fn [state]
    (let [full-name (str module &/+name-separator+ name)
          bound (&/V "Expression" (&/T (&/V "global" (&/T module name)) type))]
      (return* (->> state
                    (&/update$ "lux;modules" (fn [ms] (&/|update module (fn [m] (&/update$ "lux;defs" #(&/|put name type %) m)) ms)))
                    (&/update$ "lux;global-env" #(matchv ::M/objects [%]
                                                   [["lux;None" _]]
                                                   (assert false)

                                                   [["lux;Some" table]]
                                                   (&/V "lux;Some" (&/update$ "lux;locals" (fn [locals]
                                                                                             (&/update$ "lux;mappings" (fn [mappings]
                                                                                                                         (&/|merge (&/|table full-name bound, name bound)
                                                                                                                                   mappings))
                                                                                                        locals))
                                                                              table))
                                                   )))
               nil))))

(defn module-exists? [name]
  (fn [state]
    (return* state
             (->> state (&/get$ "lux;modules") (&/|contains? name)))))

(defn unalias-module [name]
  (fn [state]
    (if-let [real-name (->> state (&/get$ "lux;module-aliases") (&/|get name))]
      (return* state real-name)
      (fail "Unknown alias."))))
