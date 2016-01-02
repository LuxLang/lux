;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.module
  (:refer-clojure :exclude [alias])
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftags |let |do return return* fail fail* |case]]
                 [type :as &type]
                 [host :as &host])
            [lux.host.generics :as &host-generics]
            (lux.analyser [meta :as &meta])))

;; [Utils]
(deftags
  ["module-aliases"
   "defs"
   "imports"
   "tags"
   "types"])

(def ^:private +init+
  (&/T ;; "lux;module-aliases"
   (&/|table)
   ;; "lux;defs"
   (&/|table)
   ;; "lux;imports"
   &/Nil$
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
      (return* (&/update$ &/$modules
                          (fn [ms]
                            (&/|update current-module
                                       (fn [m] (&/update$ $imports (partial &/Cons$ module) m))
                                       ms))
                          state)
               nil))))

(defn set-imports [imports]
  "(-> (List Text) (Lux (,)))"
  (|do [current-module &/get-module-name]
    (fn [state]
      (return* (&/update$ &/$modules
                          (fn [ms]
                            (&/|update current-module
                                       (fn [m] (&/set$ $imports imports m))
                                       ms))
                          state)
               nil))))

(defn define [module name def-type def-meta def-value]
  (fn [state]
    (when (and (= "Macro" name) (= "lux" module))
      (&type/set-macro-type! def-value))
    (|case (&/get$ &/$envs state)
      (&/$Cons ?env (&/$Nil))
      (return* (->> state
                    (&/update$ &/$modules
                               (fn [ms]
                                 (&/|update module
                                            (fn [m]
                                              (&/update$ $defs
                                                         #(&/|put name (&/T def-type def-meta def-value) %)
                                                         m))
                                            ms))))
               nil)
      
      _
      (fail* (str "[Analyser Error] Can't create a new global definition outside of a global environment: " module ";" name)))))

(defn def-type [module name]
  "(-> Text Text (Lux Type))"
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
      (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
        (|let [[?type ?meta ?value] $def]
          (return* state ?type))
        (fail* (str "[Analyser Error] Unknown definition: " (str module ";" name))))
      (fail* (str "[Analyser Error] Unknown module: " module)))))

(defn type-def [module name]
  "(-> Text Text (Lux Type))"
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
      (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
        (|let [[?type ?meta ?value] $def]
          (|case (&meta/meta-get &meta/type?-tag ?meta)
            (&/$Some _)
            (return* state ?value)

            _
            (fail* (str "[Analyser Error] Not a type: " (&/ident->text (&/T module name))))))
        (fail* (str "[Analyser Error] Unknown definition: " (&/ident->text (&/T module name)))))
      (fail* (str "[Analyser Error] Unknown module: " module)))))

(defn exists? [name]
  "(-> Text (Lux Bool))"
  (fn [state]
    (return* state
             (->> state (&/get$ &/$modules) (&/|contains? name)))))

(defn dealias [name]
  (|do [current-module &/get-module-name]
    (fn [state]
      (if-let [real-name (->> state (&/get$ &/$modules) (&/|get current-module) (&/get$ $module-aliases) (&/|get name))]
        (return* state real-name)
        (fail* (str "Unknown alias: " name))))))

(defn alias [module alias reference]
  (fn [state]
    (if-let [real-name (->> state (&/get$ &/$modules) (&/|get module) (&/get$ $module-aliases) (&/|get alias))]
      (fail* (str "Can't re-use alias \"" alias "\" @ " module))
      (return* (->> state
                    (&/update$ &/$modules
                               (fn [ms]
                                 (&/|update module
                                            #(&/update$ $module-aliases
                                                        (fn [aliases]
                                                          (&/|put alias reference aliases))
                                                        %)
                                            ms))))
               nil))))

(defn find-def [module name]
  (|do [current-module &/get-module-name]
    (fn [state]
      (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
        (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
          (|let [[?type ?meta ?value] $def]
            (if (.equals ^Object current-module module)
              (|case (&meta/meta-get &meta/alias-tag ?meta)
                (&/$Some (&/$IdentM [?r-module ?r-name]))
                ((find-def ?r-module ?r-name)
                 state)

                _
                (return* state (&/T (&/T module name) $def)))
              (|case (&meta/meta-get &meta/export?-tag ?meta)
                (&/$Some (&/$BoolM true))
                (return* state (&/T (&/T module name) $def))

                _
                (fail* (str "[Analyser Error] Can't use unexported definition: " (str module &/+name-separator+ name))))))
          (fail* (str "[Analyser Error] Definition does not exist: " (str module &/+name-separator+ name))))
        (fail* (str "[Analyser Error] Module doesn't exist: " module))))))

(defn ensure-type-def [def-data]
  "(-> DefData (Lux Type))"
  (|let [[?type ?meta ?value] def-data]
    (|case (&meta/meta-get &meta/type?-tag ?meta)
      (&/$Some _)
      (return ?type)

      _
      (fail (str "[Analyser Error] Not a type definition: " (&/adt->text def-data))))))

(defn defined? [module name]
  (&/try-all% (&/|list (|do [_ (find-def module name)]
                         (return true))
                       (return false))))

(def imports
  (|do [module &/get-module-name]
    (fn [state]
      (return* state (->> state (&/get$ &/$modules) (&/|get module) (&/get$ $imports))))))

(defn create-module [name]
  "(-> Text (Lux (,)))"
  (fn [state]
    (return* (&/update$ &/$modules #(&/|put name +init+ %) state) nil)))

(defn enter-module [name]
  "(-> Text (Lux (,)))"
  (fn [state]
    (return* (->> state
                  (&/update$ &/$modules #(&/|put name +init+ %))
                  (&/set$ &/$envs (&/|list (&/env name))))
             nil)))

(do-template [<name> <tag> <type>]
  (defn <name> [module]
    <type>
    (fn [state]
      (if-let [=module (->> state (&/get$ &/$modules) (&/|get module))]
        (return* state (&/get$ <tag> =module))
        (fail* (str "[Lux Error] Unknown module: " module)))
      ))

  tags-by-module  $tags  "(-> Text (Lux (List (, Text (, Int (List Text) Type)))))"
  types-by-module $types "(-> Text (Lux (List (, Text (, (List Text) Type)))))"
  )

(defn ensure-undeclared-tags [module tags]
  (|do [tags-table (tags-by-module module)
        _ (&/map% (fn [tag]
                    (if (&/|get tag tags-table)
                      (fail (str "[Analyser Error] Can't re-declare tag: " (&/ident->text (&/T module tag))))
                      (return nil)))
                  tags)]
    (return nil)))

(defn ensure-undeclared-type [module name]
  (|do [types-table (types-by-module module)
        _ (&/assert! (nil? (&/|get name types-table))
                     (str "[Analyser Error] Can't re-declare type: " (&/ident->text (&/T module name))))]
    (return nil)))

(defn declare-tags [module tag-names type]
  "(-> Text (List Text) Type (Lux (,)))"
  (|do [_ (ensure-undeclared-tags module tag-names)
        type-name (&type/type-name type)
        :let [[_module _name] type-name]
        _ (&/assert! (= module _module)
                     (str "[Module Error] Can't define tags for a type belonging to a foreign module: " (&/ident->text type-name)))
        _ (ensure-undeclared-type _module _name)]
    (fn [state]
      (if-let [=module (->> state (&/get$ &/$modules) (&/|get module))]
        (let [tags (&/|map (fn [tag-name] (&/T module tag-name)) tag-names)]
          (return* (&/update$ &/$modules
                              (fn [=modules]
                                (&/|update module
                                           #(->> %
                                                 (&/set$ $tags (&/fold (fn [table idx+tag-name]
                                                                         (|let [[idx tag-name] idx+tag-name]
                                                                           (&/|put tag-name (&/T idx tags type) table)))
                                                                       (&/get$ $tags %)
                                                                       (&/enumerate tag-names)))
                                                 (&/update$ $types (partial &/|put _name (&/T tags type))))
                                           =modules))
                              state)
                   nil))
        (fail* (str "[Lux Error] Unknown module: " module))))))

(do-template [<name> <idx> <doc>]
  (defn <name> [module tag-name]
    <doc>
    (fn [state]
      (if-let [=module (->> state (&/get$ &/$modules) (&/|get module))]
        (if-let [^objects idx+tags+type (&/|get tag-name (&/get$ $tags =module))]
          (return* state (aget idx+tags+type <idx>))
          (fail* (str "[Module Error] Unknown tag: " (&/ident->text (&/T module tag-name)))))
        (fail* (str "[Module Error] Unknown module: " module)))))

  tag-index 0 "(-> Text Text (Lux Int))"
  tag-group 1 "(-> Text Text (Lux (List Ident)))"
  tag-type  2 "(-> Text Text (Lux Type))"
  )

(def defs
  (|do [module &/get-module-name]
    (fn [state]
      (return* state
               (->> state (&/get$ &/$modules) (&/|get module) (&/get$ $defs)
                    (&/|map (fn [kv]
                              (|let [[k [?def-type ?def-meta ?def-value]] kv]
                                (|case (&meta/meta-get &meta/alias-tag ?def-meta)
                                  (&/$Some (&/$IdentM [?r-module ?r-name]))
                                  (&/T k (str ?r-module ";" ?r-name))
                                  
                                  _
                                  (&/T k "")
                                  )))))))))

(do-template [<name> <type> <tag> <desc>]
  (defn <name> [module name meta type]
    (|case (&meta/meta-get <tag> meta)
      (&/$Some (&/$BoolM true))
      (&/try-all% (&/|list (&type/check <type> type)
                           (fail (str "[Analyser Error] Can't tag as lux;" <desc> "? if it's not a " <desc> ": " (str module ";" name)))))

      _
      (return nil)))

  test-type  &type/Type  &meta/type?-tag  "type"
  test-macro &type/Macro &meta/macro?-tag "macro"
  )
