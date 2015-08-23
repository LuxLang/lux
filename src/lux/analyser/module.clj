;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.analyser.module
  (:refer-clojure :exclude [alias])
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [defrtags |let |do return return* fail fail* |case $$]]
                 [type :as &type]
                 [host :as &host])))

;; [Utils]
(defrtags
  ["module-aliases"
   "defs"
   "imports"
   "tags"
   "types"])
(def ^:private +init+
  ($$ &/P
      ;; "lux;module-aliases"
      (&/|table)
      ;; "lux;defs"
      (&/|table)
      ;; "lux;imports"
      (&/|list)
      ;; "lux;tags"
      (&/|table)
      ;; "lux;types"
      (&/|table)
      ))

;; [Exports]
(defn add-import [module]
  "(-> Text (Lux (,)))"
  (|do [current-module &/get-module-name]
    (fn [state]
      (return* (&/$update-modules
                (fn [ms]
                  (&/|update current-module
                             (fn [m] ($update-imports (partial &/Cons$ module) m))
                             ms))
                state)
               nil))))

(defn set-imports [imports]
  "(-> (List Text) (Lux (,)))"
  (|do [current-module &/get-module-name]
    (fn [state]
      (return* (&/$update-modules
                (fn [ms]
                  (&/|update current-module
                             (fn [m] ($set-imports imports m))
                             ms))
                state)
               nil))))

(defn define [module name def-data type]
  ;; (prn 'define module name (aget def-data 0) (&type/show-type type))
  (fn [state]
    (|case (&/$get-envs state)
      (&/$Cons ?env (&/$Nil))
      (return* (->> state
                    (&/$update-modules
                     (fn [ms]
                       (&/|update module
                                  (fn [m]
                                    ($update-defs
                                     #(&/|put name (&/P false def-data) %)
                                     m))
                                  ms))))
               nil)
      
      _
      (fail* (str "[Analyser Error] Can't create a new global definition outside of a global environment: " module ";" name)))))

(defn def-type [module name]
  "(-> Text Text (Lux Type))"
  (fn [state]
    (if-let [$module (->> state (&/$get-modules) (&/|get module))]
      (if-let [$def (->> $module ($get-defs) (&/|get name))]
        (|case $def
          [_ (&/$TypeD _)]
          (return* state &type/Type)

          [_ (&/$MacroD _)]
          (return* state &type/Macro)

          [_ (&/$ValueD _type _)]
          (return* state _type)

          [_ (&/$AliasD ?r-module ?r-name)]
          (&/run-state (def-type ?r-module ?r-name)
                       state))
        (fail* (str "[Analyser Error] Unknown definition: " (str module ";" name))))
      (fail* (str "[Analyser Error] Unknown module: " module)))))

(defn type-def [module name]
  "(-> Text Text (Lux Type))"
  (fn [state]
    (if-let [$module (->> state (&/$get-modules) (&/|get module))]
      (if-let [$def (->> $module ($get-defs) (&/|get name))]
        (|case $def
          [_ (&/$TypeD _type)]
          (return* state _type)

          _
          (fail* (str "[Analyser Error] Not a type: " (&/ident->text (&/P module name)))))
        (fail* (str "[Analyser Error] Unknown definition: " (&/ident->text (&/P module name)))))
      (fail* (str "[Analyser Error] Unknown module: " module)))))

(defn def-alias [a-module a-name r-module r-name type]
  ;; (prn 'def-alias [a-module a-name] [r-module r-name] (&type/show-type type))
  (fn [state]
    (|case (&/$get-envs state)
      (&/$Cons ?env (&/$Nil))
      (return* (->> state
                    (&/$update-modules
                     (fn [ms]
                       (&/|update a-module
                                  (fn [m]
                                    ($update-defs
                                     #(&/|put a-name (&/P false (&/S &/$AliasD (&/P r-module r-name))) %)
                                     m))
                                  ms))))
               nil)
      
      _
      (fail* "[Analyser Error] Can't alias a global definition outside of a global environment."))))

(defn exists? [name]
  "(-> Text (Lux Bool))"
  (fn [state]
    ;; (prn 'exists?/_0 &/$modules name)
    ;; (prn 'exists?/_2 (&/adt->text state))
    ;; (prn 'exists?/_3 (&/adt->text (->> state (&/$get-modules))))
    ;; (prn 'exists?/_4 (&/adt->text (->> state (&/$get-modules) (&/|contains? name))))
    (return* state
             (->> state (&/$get-modules) (&/|contains? name)))))

(defn alias [module alias reference]
  (fn [state]
    (return* (->> state
                  (&/$update-modules
                   (fn [ms]
                     (&/|update module
                                #($update-module-aliases
                                  (fn [aliases]
                                    (&/|put alias reference aliases))
                                  %)
                                ms))))
             nil)))

(defn dealias [name]
  (|do [current-module &/get-module-name]
    (fn [state]
      (if-let [real-name (->> state (&/$get-modules) (&/|get current-module) ($get-module-aliases) (&/|get name))]
        (return* state real-name)
        (fail* (str "Unknown alias: " name))))))

(defn find-def [module name]
  (|do [current-module &/get-module-name]
    (fn [state]
      ;; (prn 'find-def/_0 module name 'current-module current-module)
      (if-let [$module (->> state (&/$get-modules) (&/|get module))]
        (do ;; (prn 'find-def/_0.1 module (&/->seq (&/|keys $module)))
            (if-let [$def (->> $module ($get-defs) (&/|get name))]
              (|let [[exported? $$def] $def]
                (do ;; (prn 'find-def/_1 module name 'exported? exported? (.equals ^Object current-module module))
                    (if (or exported? (.equals ^Object current-module module))
                      (|case $$def
                        (&/$AliasD ?r-module ?r-name)
                        (do ;; (prn 'find-def/_2 [module name] [?r-module ?r-name])
                            ((find-def ?r-module ?r-name)
                             state))

                        _
                        (return* state (&/P (&/P module name) $$def)))
                      (fail* (str "[Analyser Error] Can't use unexported definition: " (str module &/+name-separator+ name))))))
              (fail* (str "[Analyser Error] Definition does not exist: " (str module &/+name-separator+ name)))))
        (fail* (str "[Analyser Error] Module doesn't exist: " module))))))

(defn ensure-type-def [def-data]
  "(-> DefData (Lux Type))"
  (|case def-data
    (&/$TypeD type)
    (return type)

    _
    (fail (str "[Analyser Error] Not a type definition: " (&/adt->text def-data)))))

(defn defined? [module name]
  (&/try-all% (&/|list (|do [_ (find-def module name)]
                         (return true))
                       (return false))))

(defn declare-macro [module name]
  (fn [state]
    (if-let [$module (->> state (&/$get-modules) (&/|get module) ($get-defs))]
      (if-let [$def (&/|get name $module)]
        (|case $def
          [exported? (&/$ValueD ?type _)]
          ((|do [_ (&type/check &type/Macro ?type)
                 ^ClassLoader loader &/loader
                 :let [macro (-> (.loadClass loader (str (&host/->module-class module) "." (&/normalize-name name)))
                                 (.getField &/datum-field)
                                 (.get nil))]]
             (fn [state*]
               (return* (&/$update-modules
                         (fn [$modules]
                           (&/|update module
                                      (fn [m]
                                        ($update-defs
                                         #(&/|put name (&/P exported? (&/S &/$MacroD macro)) %)
                                         m))
                                      $modules))
                         state*)
                        nil)))
           state)
          
          [_ (&/$MacroD _)]
          (fail* (str "[Analyser Error] Can't re-declare a macro: " (str module &/+name-separator+ name)))

          [_ _]
          (fail* (str "[Analyser Error] Definition does not have macro type: " (str module &/+name-separator+ name))))
        (fail* (str "[Analyser Error] Definition does not exist: " (str module &/+name-separator+ name))))
      (fail* (str "[Analyser Error] Module does not exist: " module)))))

(defn export [module name]
  (fn [state]
    (|case (&/$get-envs state)
      (&/$Cons ?env (&/$Nil))
      (if-let [$def (->> state (&/$get-modules) (&/|get module) ($get-defs) (&/|get name))]
        (|case $def
          [true _]
          (fail* (str "[Analyser Error] Definition has already been exported: " module ";" name))

          [false ?data]
          (return* (->> state
                        (&/$update-modules (fn [ms]
                                             (&/|update module (fn [m]
                                                                 ($update-defs
                                                                  #(&/|put name (&/P true ?data) %)
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
                                 (&/$AliasD ?r-module ?r-name)
                                 ($$ &/P ?exported? k (str "A" ?r-module ";" ?r-name))
                                 
                                 (&/$MacroD _)
                                 ($$ &/P ?exported? k "M")

                                 (&/$TypeD _)
                                 ($$ &/P ?exported? k "T")

                                 _
                                 ($$ &/P ?exported? k "V")))))
                       (->> state (&/$get-modules) (&/|get module) ($get-defs)))))))

(def imports
  (|do [module &/get-module-name]
    (fn [state]
      (return* state (->> state (&/$get-modules) (&/|get module) ($get-imports))))))

(defn create-module [name]
  "(-> Text (Lux (,)))"
  (fn [state]
    (return* (&/$update-modules #(&/|put name +init+ %) state) nil)))

(defn enter-module [name]
  "(-> Text (Lux (,)))"
  (fn [state]
    (return* (->> state
                  (&/$update-modules #(&/|put name +init+ %))
                  (&/$set-envs (&/|list (&/env name))))
             nil)))

(do-template [<name> <getter> <type>]
  (defn <name> [module]
    <type>
    (fn [state]
      (if-let [=module (->> state (&/$get-modules) (&/|get module))]
        (return* state (<getter> =module))
        (fail* (str "[Lux Error] Unknown module: " module)))
      ))

  tags-by-module  $get-tags  "(-> Text (Lux (List (, Text (, Int (List Text) Type)))))"
  types-by-module $get-types "(-> Text (Lux (List (, Text (, (List Text) Type)))))"
  )

(defn ensure-undeclared-tags [module tags]
  (|do [tags-table (tags-by-module module)
        _ (&/map% (fn [tag]
                    (if (&/|get tag tags-table)
                      (fail (str "[Analyser Error] Can't re-declare tag: " (&/ident->text (&/P module tag))))
                      (return nil)))
                  tags)]
    (return nil)))

(defn ensure-undeclared-type [module name]
  (|do [types-table (types-by-module module)
        _ (&/assert! (nil? (&/|get name types-table)) (str "[Analyser Error] Can't re-declare type: " (&/ident->text (&/P module name))))]
    (return nil)))

(defn declare-tags [module tag-names type]
  "(-> Text (List Text) Type (Lux (,)))"
  (|do [;; :let [_ (prn 'declare-tags module (&/->seq tag-names) (&type/show-type type))]
        _ (ensure-undeclared-tags module tag-names)
        type-name (&type/type-name type)
        :let [[_module _name] type-name]
        _ (&/assert! (= module _module)
                     (str "[Module Error] Can't define tags for a type belonging to a foreign module: " (&/ident->text type-name)))
        _ (ensure-undeclared-type _module _name)]
    (fn [state]
      (if-let [=module (->> state (&/$get-modules) (&/|get module))]
        (let [tags (&/|map (fn [tag-name] (&/P module tag-name)) tag-names)]
          (return* (&/$update-modules
                    (fn [=modules]
                      (&/|update module
                                 #(->> %
                                       ($set-tags (&/fold (fn [table idx+tag-name]
                                                            (|let [[idx tag-name] idx+tag-name]
                                                              (&/|put tag-name ($$ &/P idx tags type) table)))
                                                          ($get-tags %)
                                                          (&/enumerate tag-names)))
                                       ($update-types (partial &/|put _name (&/P tags type))))
                                 =modules))
                    state)
                   nil))
        (fail* (str "[Lux Error] Unknown module: " module))))))

(do-template [<name> <member> <type>]
  (defn <name> [module tag-name]
    <type>
    (fn [state]
      (if-let [=module (->> state (&/$get-modules) (&/|get module))]
        (if-let [^objects idx+tags (&/|get tag-name ($get-tags =module))]
          (|let [[idx tags type] idx+tags]
            (return* state <member>))
          (fail* (str "[Module Error] Unknown tag: " (&/ident->text (&/P module tag-name)))))
        (fail* (str "[Module Error] Unknown module: " module)))))

  tag-index idx  "(-> Text Text (Lux Int))"
  tag-group tags "(-> Text Text (Lux (List Ident)))"
  )
