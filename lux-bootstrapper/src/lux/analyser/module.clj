(ns lux.analyser.module
  (:refer-clojure :exclude [alias])
  (:require (clojure [string :as string]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [defvariant deftuple |let |do return return* |case]]
                 [type :as &type]
                 [host :as &host])
            [lux.host.generics :as &host-generics]))

;; [Utils]
;; ModuleState
(defvariant
  ("Active" 0)
  ("Compiled" 0)
  ("Cached" 0))

;; Module
(deftuple
  ["module-hash"
   "module-aliases"
   "defs"
   "imports"
   "module-state"])

(defn ^:private new-module [hash]
  (&/T [;; lux;module-hash
        hash
        ;; "lux;module-aliases"
        (&/|table)
        ;; "lux;defs"
        (&/|table)
        ;; "lux;imports"
        &/$End
        ;; "module-state"
        $Active]
       ))

(do-template [<flagger> <asker> <tag>]
  (do (defn <flagger>
        "(-> Text (Lux Any))"
        [module-name]
        (fn [state]
          (let [state* (&/update$ &/$modules
                                  (fn [modules]
                                    (&/|update module-name
                                               (fn [=module]
                                                 (&/set$ $module-state <tag> =module))
                                               modules))
                                  state)]
            (&/$Right (&/T [state* &/unit-tag])))))
    (defn <asker>
      "(-> Text (Lux Bit))"
      [module-name]
      (fn [state]
        (if-let [=module (->> state (&/get$ &/$modules) (&/|get module-name))]
          (&/$Right (&/T [state (|case (&/get$ $module-state =module)
                                  (<tag>) true
                                  _       false)]))
          (&/$Right (&/T [state false])))
        )))

  flag-active-module   active-module?   $Active
  flag-compiled-module compiled-module? $Compiled
  flag-cached-module   cached-module?   $Cached
  )

;; [Exports]
(defn add-import
  "(-> Text (Lux Null))"
  [module]
  (|do [current-module &/get-module-name]
    (fn [state]
      (if (&/|member? module (->> state (&/get$ &/$modules) (&/|get current-module) (&/get$ $imports)))
        ((&/fail-with-loc (str "[Analyser Error] Cannot import module " (pr-str module) " twice @ " current-module))
         state)
        (return* (&/update$ &/$modules
                            (fn [ms]
                              (&/|update current-module
                                         (fn [m] (&/update$ $imports (partial &/$Item module) m))
                                         ms))
                            state)
                 nil)))))

(defn set-imports
  "(-> (List Text) (Lux Null))"
  [imports]
  (|do [current-module &/get-module-name]
    (fn [state]
      (return* (&/update$ &/$modules
                          (fn [ms]
                            (&/|update current-module
                                       (fn [m] (&/set$ $imports imports m))
                                       ms))
                          state)
               nil))))

(defn type-def
  "(-> Text Text (Lux [Bit Type]))"
  [module name]
  (fn [state]
    (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
      (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
        (|case $def
          (&/$AliasG [o-module o-name])
          ((type-def o-module o-name) state)
          
          (&/$DefinitionG [exported? ?type ?value])
          (if (&type/type= &type/Type ?type)
            (return* state (&/T [exported? ?value]))
            ((&/fail-with-loc (str "[Analyser Error] Not a type: " (&/ident->text (&/T [module name]))))
             state))

          (&/$TypeG [exported? ?value labels])
          (return* state (&/T [exported? ?value]))

          (&/$TagG _)
          ((&/fail-with-loc (str "[Analyser Error] Not a type: " (&/ident->text (&/T [module name]))))
           state)

          (&/$SlotG _)
          ((&/fail-with-loc (str "[Analyser Error] Not a type: " (&/ident->text (&/T [module name]))))
           state))
        ((&/fail-with-loc (str "[Analyser Error] Unknown definition: " (&/ident->text (&/T [module name]))))
         state))
      ((&/fail-with-loc (str "[Analyser Error] Unknown module: " module))
       state))))

(defn exists?
  "(-> Text (Lux Bit))"
  [name]
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
        ((&/fail-with-loc (str "[Analyser Error] Cannot create alias that is the same as a module nameL " (pr-str alias) " for " reference))
         state)
        (if-let [real-name (->> _module_ (&/get$ $module-aliases) (&/|get alias))]
          ((&/fail-with-loc (str "[Analyser Error] Cannot re-use alias \"" alias "\" @ " module))
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

(defn ^:private imports? [state imported-module-name source-module-name]
  (->> state
       (&/get$ &/$modules)
       (&/|get source-module-name)
       (&/get$ $imports)
       (&/|any? (partial = imported-module-name))))

(defn find-def! [module name]
  (|do [current-module &/get-module-name]
    (fn [state]
      (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
        (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
          (|case $def
            (&/$AliasG [?r-module ?r-name])
            ((find-def! ?r-module ?r-name)
             state)

            (&/$DefinitionG $def*)
            (return* state (&/T [(&/T [module name]) $def*]))

            (&/$TypeG [exported? ?value labels])
            (return* state (&/T [(&/T [module name])
                                 (&/T [exported? &type/Type ?value])]))

            (&/$TagG _)
            ((&/fail-with-loc (str "[Analyser Error] Not a definition: " (&/ident->text (&/T [module name]))))
             state)

            (&/$SlotG _)
            ((&/fail-with-loc (str "[Analyser Error] Not a definition: " (&/ident->text (&/T [module name]))))
             state))
          ((&/fail-with-loc (str "[Analyser Error @ find-def!] Definition does not exist: " (str module &/+name-separator+ name)
                                 " at module: " current-module))
           state))
        ((&/fail-with-loc (str "[Analyser Error @ find-def!] Module does not exist: " module
                               " at module: " current-module))
         state)))))

(defn find-def [quoted_module module name]
  (|do [current-module &/get-module-name]
    (fn [state]
      (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
        (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
          (|case $def
            (&/$AliasG [?r-module ?r-name])
            (if (.equals ^Object current-module module)
              ((find-def! ?r-module ?r-name)
               state)
              ((&/fail-with-loc (str "[Analyser Error @ find-def] Cannot use (private) alias: " (str module &/+name-separator+ name)
                                     " at module: " current-module))
               state))
            
            (&/$DefinitionG [exported? ?type ?value])
            (if (or (.equals ^Object current-module module)
                    (and exported?
                         (or (.equals ^Object &/prelude module)
                             (.equals ^Object quoted_module module)
                             (imports? state module current-module))))
              (return* state (&/T [(&/T [module name])
                                   (&/T [exported? ?type ?value])]))
              ((&/fail-with-loc (str "[Analyser Error @ find-def] Cannot use private definition: " (str module &/+name-separator+ name)
                                     " at module: " current-module))
               state))

            (&/$TypeG [exported? ?value labels])
            (if (or (.equals ^Object current-module module)
                    (and exported?
                         (or (.equals ^Object &/prelude module)
                             (.equals ^Object quoted_module module)
                             (imports? state module current-module))))
              (return* state (&/T [(&/T [module name])
                                   (&/T [exported? &type/Type ?value])]))
              ((&/fail-with-loc (str "[Analyser Error @ find-def] Cannot use private definition: " (str module &/+name-separator+ name)
                                     " at module: " current-module))
               state))

            (&/$TagG _)
            ((&/fail-with-loc (str "[Analyser Error] Not a definition: " (&/ident->text (&/T [module name]))))
             state)

            (&/$SlotG _)
            ((&/fail-with-loc (str "[Analyser Error] Not a definition: " (&/ident->text (&/T [module name]))))
             state))
          ((&/fail-with-loc (str "[Analyser Error @ find-def] Definition does not exist: " (str module &/+name-separator+ name)
                                 " at module: " current-module))
           state))
        ((&/fail-with-loc (str "[Analyser Error @ find-def] Module does not exist: " module
                               " at module: " current-module))
         state)))))

(defn find-global [module name]
  (|do [current-module &/get-module-name]
    (fn [state]
      (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
        (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
          (|case $def
            (&/$AliasG [?r-module ?r-name]) (return* state $def)
            (&/$DefinitionG _) (return* state $def)
            (&/$TypeG _) (return* state $def)
            (&/$TagG _) (return* state $def)
            (&/$SlotG _) (return* state $def))
          ((&/fail-with-loc (str "[Analyser Error @ find-def] Global does not exist: " (str module &/+name-separator+ name)
                                 " at module: " current-module))
           state))
        ((&/fail-with-loc (str "[Analyser Error @ find-def] Module does not exist: " module
                               " at module: " current-module))
         state)))))

(do-template [<tag> <find!> <find>]
  (do (defn <find!> [module name]
        (|do [current-module &/get-module-name]
          (fn [state]
            (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
              (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
                (|case $def
                  (&/$AliasG [?r-module ?r-name])
                  ((<find!> ?r-module ?r-name)
                   state)

                  (<tag> ?payload)
                  (return* state ?payload)

                  _
                  ((&/fail-with-loc (str "[Analyser Error] Not a label: " (&/ident->text (&/T [module name]))
                                         " @ " (quote <find!>)))
                   state))
                ((&/fail-with-loc (str "[Analyser Error] Label does not exist: " (str module &/+name-separator+ name)
                                       " at module: " current-module
                                       " @ " (quote <find!>)))
                 state))
              ((&/fail-with-loc (str "[Analyser Error] Module does not exist: " module
                                     " at module: " current-module
                                     " @ " (quote <find!>)))
               state)))))
    (defn <find> [module name]
      (|do [current-module &/get-module-name]
        (fn [state]
          (if-let [$module (->> state (&/get$ &/$modules) (&/|get module))]
            (if-let [$def (->> $module (&/get$ $defs) (&/|get name))]
              (|case $def
                (&/$AliasG [?r-module ?r-name])
                (if (.equals ^Object current-module module)
                  ((<find!> ?r-module ?r-name)
                   state)
                  ((&/fail-with-loc (str "[Analyser Error] Cannot use (private) alias: " (str module &/+name-separator+ name)
                                         " at module: " current-module
                                         " @ " (quote <find>)))
                   state))
                
                (<tag> [exported? type group index])
                (if (or (.equals ^Object current-module module)
                        exported?)
                  (return* state (&/T [exported? type group index]))
                  ((&/fail-with-loc (str "[Analyser Error] Cannot use private label: " (str module &/+name-separator+ name)
                                         " at module: " current-module
                                         " @ " (quote <find>)))
                   state))

                _
                ((&/fail-with-loc (str "[Analyser Error] Not a label: " (&/ident->text (&/T [module name]))
                                       " @ " (quote <find>)))
                 state))
              ((&/fail-with-loc (str "[Analyser Error] Label does not exist: " (str module &/+name-separator+ name)
                                     " at module: " current-module
                                     " @ " (quote <find>)))
               state))
            ((&/fail-with-loc (str "[Analyser Error] Module does not exist: " module
                                   " at module: " current-module
                                   " @ " (quote <find>)))
             state))))))

  &/$TagG find-tag! find-tag
  &/$SlotG find-slot! find-slot
  )

(defn if_not_defined [module name then]
  (|do [exists? (&/try% (find-global module name))]
    (|case exists?
      (&/$Some _)
      (fn [state]
        ((&/fail-with-loc (str "[Analyser Error] Cannot create a new global because the name is already taken."
                               "\n" "Module: " module
                               "\n" "Name: " name))
         state))
      
      (&/$None)
      then)))

(defn defined? [module name]
  (&/try-all% (&/|list (|do [_ (find-def! module name)]
                         (return true))
                       (return false))))

(defn create-module
  "(-> Text Hash-Code (Lux Null))"
  [name hash]
  (fn [state]
    (return* (->> state
                  (&/update$ &/$modules #(&/|put name (new-module hash) %))
                  (&/set$ &/$scopes (&/|list (&/env name &/$End)))
                  (&/set$ &/$current-module (&/$Some name)))
             nil)))

(defn module-hash
  "(-> Text (Lux Int))"
  [module]
  (fn [state]
    (if-let [=module (->> state (&/get$ &/$modules) (&/|get module))]
      (return* state (&/get$ $module-hash =module))
      ((&/fail-with-loc (str "[Lux Error] Unknown module: " module))
       state))))

(def imports
  (|do [module &/get-module-name
        _imports (fn [state]
                   (return* state (->> state (&/get$ &/$modules) (&/|get module) (&/get$ $imports))))]
    (&/map% (fn [_module]
              (|do [_hash (module-hash _module)]
                (return (&/T [_module _hash]))))
            _imports)))

(defn define-alias [module name de-aliased]
  (if_not_defined
      module name
      (fn [state]
        (|case (&/get$ &/$scopes state)
          (&/$Item ?env (&/$End))
          (return* (->> state
                        (&/update$ &/$modules
                                   (fn [ms]
                                     (&/|update module
                                                (fn [m]
                                                  (&/update$ $defs
                                                             #(&/|put name (&/$AliasG de-aliased) %)
                                                             m))
                                                ms))))
                   nil)
          
          _
          ((&/fail-with-loc (str "[Analyser Error] Cannot create a new global definition outside of a global environment: " (str module &/+name-separator+ name)))
           state)))))

(defn define [module name exported? def-type def-value]
  (if_not_defined
      module name
      (fn [state]
        (|case (&/get$ &/$scopes state)
          (&/$Item ?env (&/$End))
          (return* (->> state
                        (&/update$ &/$modules
                                   (fn [ms]
                                     (&/|update module
                                                (fn [m]
                                                  (&/update$ $defs
                                                             #(&/|put name (&/$DefinitionG (&/T [exported? def-type def-value])) %)
                                                             m))
                                                ms))))
                   nil)
          
          _
          ((&/fail-with-loc (str "[Analyser Error] Cannot create a new global definition outside of a global environment: " (str module &/+name-separator+ name)))
           state)))))

(do-template [<name> <tag>]
  (defn <name> [module name exported? type group index]
    (if_not_defined
        module name
        (fn [state]
          (|case (&/get$ &/$scopes state)
            (&/$Item ?env (&/$End))
            (return* (->> state
                          (&/update$ &/$modules
                                     (fn [ms]
                                       (&/|update module
                                                  (fn [m]
                                                    (&/update$ $defs
                                                               #(&/|put name (<tag> (&/T [exported? type group index])) %)
                                                               m))
                                                  ms))))
                     nil)
            
            _
            ((&/fail-with-loc (str "[Analyser Error] Cannot create a new global outside of a global environment: " (str module &/+name-separator+ name)))
             state)))))

  define_tag &/$TagG
  define_slot &/$SlotG
  )

(defn declare-labels
  "(-> Text (List Text) Bit Type (Lux Null))"
  [module record? label-names was-exported? type]
  (|do [type-name (&type/type-name type)
        :let [[_module _name] type-name]
        _ (&/assert! (= module _module)
                     (str "[Module Error] Cannot define labels for a type belonging to a foreign module: " (&/ident->text type-name)))]
    (if (nil? record?)
      (return &/unit-tag)
      (if record?
        (&/map% (fn [idx+label-name]
                  (|let [[index label-name] idx+label-name]
                    (define_slot module label-name was-exported? type label-names index)))
                (&/enumerate label-names))
        (&/map% (fn [idx+label-name]
                  (|let [[index label-name] idx+label-name]
                    (define_tag module label-name was-exported? type label-names index)))
                (&/enumerate label-names))))))

(defn define-type [module name exported? def-value record? labels]
  (if_not_defined
      module name
      (|case labels
        (&/$End)
        (define module name exported? &type/Type def-value)

        (&/$Item labelH labelT)
        (|do [_ (declare-labels module record? labels exported? def-value)]
          (fn [state]
            (|case (&/get$ &/$scopes state)
              (&/$Item ?env (&/$End))
              (return* (->> state
                            (&/update$ &/$modules
                                       (fn [ms]
                                         (&/|update module
                                                    (fn [m]
                                                      (&/update$ $defs
                                                                 #(&/|put name (&/$TypeG (&/T [exported? def-value (if record?
                                                                                                                     (&/$Right (&/T [labelH labelT]))
                                                                                                                     (&/$Left (&/T [labelH labelT])))]))
                                                                          %)
                                                                 m))
                                                    ms))))
                       nil)
              
              _
              ((&/fail-with-loc (str "[Analyser Error] Cannot create a new global definition outside of a global environment: " (str module &/+name-separator+ name)))
               state)))))))

(def defs
  (|do [module &/get-module-name]
    (fn [state]
      (return* state (->> state (&/get$ &/$modules) (&/|get module) (&/get$ $defs))))))

(defn fetch-imports [imports]
  (|case imports
    [_ (&/$Tuple _parts)]
    (&/map% (fn [_part]
              (|case _part
                [_ (&/$Tuple (&/$Item [[_ (&/$Text _module)]
                                       (&/$Item [[_ (&/$Text _alias)]
                                                 (&/$End)])]))]
                (return (&/T [_module _alias]))

                _
                (&/fail-with-loc "[Analyser Error] Incorrect import syntax.")))
            _parts)

    _
    (&/fail-with-loc "[Analyser Error] Incorrect import syntax.")))

(defn find_local [name]
  (fn [state]
    (|let [stack (&/get$ &/$scopes state)
           no-binding? #(and (->> % (&/get$ &/$locals)  (&/get$ &/$mappings) (&/|contains? name) not)
                             (->> % (&/get$ &/$captured) (&/get$ &/$mappings) (&/|contains? name) not))
           [inner outer] (&/|split-with no-binding? stack)]
      (|case outer
        (&/$End)
        (return* state &/$None)

        (&/$Item bottom-outer _)
        (let [local (&/|second (or (->> bottom-outer (&/get$ &/$locals)  (&/get$ &/$mappings) (&/|get name))
                                   (->> bottom-outer (&/get$ &/$captured) (&/get$ &/$mappings) (&/|get name))))]
          (return* state (&/$Some (&/T [local (&/|reverse inner) outer]))))
        ))))
