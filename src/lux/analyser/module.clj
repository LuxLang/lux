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
            (lux [base :as & :refer [deftuple |let |do return return* |case]]
                 [type :as &type]
                 [host :as &host])
            [lux.host.generics :as &host-generics]
            (lux.analyser [meta :as &meta])))

;; [Utils]
(deftuple
  ["module-hash"
   "module-aliases"
   "defs"
   "imports"
   "tags"
   "types"])

(defn ^:private new-module [hash]
  (&/T [;; lux;module-hash
        hash
        ;; "lux;module-aliases"
        (&/|table)
        ;; "lux;defs"
        (&/|table)
        ;; "lux;imports"
        &/$Nil
        ;; "lux;tags"
        (&/|table)
        ;; "lux;types"
        (&/|table)]
       ))

;; [Exports]
(defn add-import [module]
  "(-> Text (Lux Null))"
  (|do [current-module &/get-module-name]
    (fn [state]
      (if (&/|member? module (->> state (&/get$ &/$modules) (&/|get current-module) (&/get$ $imports)))
        ((&/fail-with-loc (str "[Analyser Error] Can't import module " (pr-str module) " twice @ " current-module))
         state)
        (return* (&/update$ &/$modules
                            (fn [ms]
                              (&/|update current-module
                                         (fn [m] (&/update$ $imports (partial &/$Cons module) m))
                                         ms))
                            state)
                 nil)))))

(defn set-imports [imports]
  "(-> (List Text) (Lux Null))"
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
                                                         #(&/|put name (&/T [def-type def-meta def-value]) %)
                                                         m))
                                            ms))))
               nil)
      
      _
      ((&/fail-with-loc (str "[Analyser Error] Can't create a new global definition outside of a global environment: " module ";" name))
       state))))

(defn def-type [module name]
  "(-> Text Text (Lux Type))"
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
      (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
        (|let [[?type ?meta ?value] $def]
          (return* state ?type))
        ((&/fail-with-loc (str "[Analyser Error] Unknown definition: " (str module ";" name)))
         state))
      ((&/fail-with-loc (str "[Analyser Error] Unknown module: " module))
       state))))

(defn type-def [module name]
  "(-> Text Text (Lux [Bool Type]))"
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
      (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
        (|let [[?type ?meta ?value] $def]
          (|case (&meta/meta-get &meta/type?-tag ?meta)
            (&/$Some _)
            (return* state (&/T [(|case (&meta/meta-get &meta/export?-tag ?meta)
                                   (&/$Some _)
                                   true

                                   _
                                   false)
                                 ?value]))

            _
            ((&/fail-with-loc (str "[Analyser Error] Not a type: " (&/ident->text (&/T [module name]))))
             state)))
        ((&/fail-with-loc (str "[Analyser Error] Unknown definition: " (&/ident->text (&/T [module name]))))
         state))
      ((&/fail-with-loc (str "[Analyser Error] Unknown module: " module))
       state))))

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
        ((&/fail-with-loc (str "[Analyser Error] Unknown alias: " name))
         state)))))

(defn alias [module alias reference]
  (fn [state]
    (let [_module_ (->> state (&/get$ &/$modules) (&/|get module))]
      (if (&/|member? module (->> _module_ (&/get$ $imports)))
        ((&/fail-with-loc (str "[Analyser Error] Can't create alias that is the same as a module nameL " (pr-str alias) " for " reference))
         state)
        (if-let [real-name (->> _module_ (&/get$ $module-aliases) (&/|get alias))]
          ((&/fail-with-loc (str "[Analyser Error] Can't re-use alias \"" alias "\" @ " module))
           state)
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
    ))

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
                (return* state (&/T [(&/T [module name]) $def])))
              (|case (&meta/meta-get &meta/export?-tag ?meta)
                (&/$Some (&/$BoolM true))
                (return* state (&/T [(&/T [module name]) $def]))

                _
                ((&/fail-with-loc (str "[Analyser Error] Can't use unexported definition: " (str module &/+name-separator+ name)))
                 state))))
          ((&/fail-with-loc (str "[Analyser Error] Definition does not exist: " (str module &/+name-separator+ name)))
           state))
        ((&/fail-with-loc (str "[Analyser Error] Module doesn't exist: " module))
         state)))))

(defn ensure-type-def [def-data]
  "(-> DefData (Lux Type))"
  (|let [[?type ?meta ?value] def-data]
    (|case (&meta/meta-get &meta/type?-tag ?meta)
      (&/$Some _)
      (return ?type)

      _
      (&/fail-with-loc (str "[Analyser Error] Not a type definition: " (&/adt->text def-data))))))

(defn defined? [module name]
  (&/try-all% (&/|list (|do [_ (find-def module name)]
                         (return true))
                       (return false))))

(defn create-module [name hash]
  "(-> Text Hash-Code (Lux Null))"
  (fn [state]
    (return* (->> state
                  (&/update$ &/$modules #(&/|put name (new-module hash) %))
                  (&/set$ &/$envs (&/|list (&/env name))))
             nil)))

(do-template [<name> <tag> <type>]
  (defn <name> [module]
    <type>
    (fn [state]
      (if-let [=module (->> state (&/get$ &/$modules) (&/|get module))]
        (return* state (&/get$ <tag> =module))
        ((&/fail-with-loc (str "[Lux Error] Unknown module: " module))
         state))
      ))

  tags-by-module  $tags         "(-> Text (Lux (List (, Text (, Int (List Text) Type)))))"
  types-by-module $types       "(-> Text (Lux (List (, Text (, (List Text) Type)))))"
  module-hash     $module-hash "(-> Text (Lux Int))"
  )

(def imports
  (|do [module &/get-module-name
        _imports (fn [state]
                   (return* state (->> state (&/get$ &/$modules) (&/|get module) (&/get$ $imports))))]
    (&/map% (fn [_module]
              (|do [_hash (module-hash _module)]
                (return (&/T [_module _hash]))))
            _imports)))

(defn ensure-undeclared-tags [module tags]
  (|do [tags-table (tags-by-module module)
        _ (&/map% (fn [tag]
                    (if (&/|get tag tags-table)
                      (&/fail-with-loc (str "[Analyser Error] Can't re-declare tag: " (&/ident->text (&/T [module tag]))))
                      (return nil)))
                  tags)]
    (return nil)))

(defn ensure-undeclared-type [module name]
  (|do [types-table (types-by-module module)
        _ (&/assert! (nil? (&/|get name types-table))
                     (str "[Analyser Error] Can't re-declare type: " (&/ident->text (&/T [module name]))))]
    (return nil)))

(defn declare-tags [module tag-names was-exported? type]
  "(-> Text (List Text) Bool Type (Lux Null))"
  (|do [_ (ensure-undeclared-tags module tag-names)
        type-name (&type/type-name type)
        :let [[_module _name] type-name]
        _ (&/assert! (= module _module)
                     (str "[Module Error] Can't define tags for a type belonging to a foreign module: " (&/ident->text type-name)))
        _ (ensure-undeclared-type _module _name)]
    (fn [state]
      (if-let [=module (->> state (&/get$ &/$modules) (&/|get module))]
        (let [tags (&/|map (fn [tag-name] (&/T [module tag-name])) tag-names)]
          (return* (&/update$ &/$modules
                              (fn [=modules]
                                (&/|update module
                                           #(->> %
                                                 (&/set$ $tags (&/fold (fn [table idx+tag-name]
                                                                         (|let [[idx tag-name] idx+tag-name]
                                                                           (&/|put tag-name (&/T [idx tags was-exported? type]) table)))
                                                                       (&/get$ $tags %)
                                                                       (&/enumerate tag-names)))
                                                 (&/update$ $types (partial &/|put _name (&/T [tags was-exported? type]))))
                                           =modules))
                              state)
                   nil))
        ((&/fail-with-loc (str "[Lux Error] Unknown module: " module))
         state)))))

(defn ensure-can-see-tag [module tag-name]
  "(-> Text Text (Lux Unit))"
  (|do [current-module &/get-module-name]
    (fn [state]
      (if-let [=module (->> state (&/get$ &/$modules) (&/|get module))]
        (if-let [^objects idx+tags+exported+type (&/|get tag-name (&/get$ $tags =module))]
          (|let [[?idx ?tags ?exported ?type] idx+tags+exported+type]
            (if (or ?exported
                    (= module current-module))
              (return* state &/unit-tag)
              ((&/fail-with-loc (str "[Analyser Error] Can't access tag #" (&/ident->text (&/T [module tag-name])) " from module " current-module))
               state)))
          ((&/fail-with-loc (str "[Module Error] Unknown tag: " (&/ident->text (&/T [module tag-name]))))
           state))
        ((&/fail-with-loc (str "[Module Error] Unknown module: " module))
         state)))))

(do-template [<name> <part> <doc>]
  (defn <name> [module tag-name]
    <doc>
    (fn [state]
      (if-let [=module (->> state (&/get$ &/$modules) (&/|get module))]
        (if-let [^objects idx+tags+exported+type (&/|get tag-name (&/get$ $tags =module))]
          (|let [[?idx ?tags ?exported ?type] idx+tags+exported+type]
            (return* state <part>))
          ((&/fail-with-loc (str "[Module Error] Unknown tag: " (&/ident->text (&/T [module tag-name]))))
           state))
        ((&/fail-with-loc (str "[Module Error] Unknown module: " module))
         state))))

  tag-index ?idx  "(-> Text Text (Lux Int))"
  tag-group ?tags "(-> Text Text (Lux (List Ident)))"
  tag-type  ?type "(-> Text Text (Lux Type))"
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
                                  (&/T [k (str ?r-module ";" ?r-name)])
                                  
                                  _
                                  (&/T [k ""])
                                  )))))))))

(do-template [<name> <type> <tag> <desc>]
  (defn <name> [module name meta type]
    (|case (&meta/meta-get <tag> meta)
      (&/$Some (&/$BoolM true))
      (&/try-all% (&/|list (&type/check <type> type)
                           (&/fail-with-loc (str "[Analyser Error] Can't tag as lux;" <desc> "? if it's not a " <desc> ": " (str module ";" name)))))

      _
      (return nil)))

  test-type  &type/Type  &meta/type?-tag  "type"
  test-macro &type/Macro &meta/macro?-tag "macro"
  )
