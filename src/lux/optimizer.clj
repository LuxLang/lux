;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.optimizer
  (:require (lux [base :as & :refer [|let |do return fail return* fail* |case defvariant]]
                 [analyser :as &analyser])
            (lux.analyser [base :as &-base]
                          [case :as &a-case])))

;; [Tags]
(defvariant
  ("bool" 1)
  ("int" 1)
  ("real" 1)
  ("char" 1)
  ("text" 1)
  ("variant" 3)
  ("tuple" 1)
  ("apply" 2)
  ("case" 2)
  ("function" 4)
  ("ann" 3)
  ("var" 1)
  ("captured" 3)
  ("proc" 3)
  )

;; [Utils]
(defn ^:private shift-pattern [pattern]
  (|case pattern
    (&a-case/$StoreTestAC idx)
    (&a-case/$StoreTestAC (inc idx))

    (&a-case/$TupleTestAC sub-tests)
    (&a-case/$TupleTestAC (&/|map shift-pattern sub-tests))

    (&a-case/$VariantTestAC idx num-options sub-test)
    (&a-case/$VariantTestAC (&/T [idx num-options (shift-pattern sub-test)]))

    _
    pattern
    ))

(defn ^:private drop-scope [source]
  (|case source
    [meta ($captured scope idx source*)]
    (&/T [meta ($captured (&/|but-last scope) idx (drop-scope source*))])

    _
    source))

(defn ^:private de-scope [scope]
  "(-> Scope Scope)"
  (|case scope
    (&/$Cons _module (&/$Cons _def (&/$Cons _level-to-remove _levels-to-keep)))
    (&/$Cons _module (&/$Cons _def _levels-to-keep))))

(defn ^:private de-meta [body]
  "(-> Optimized Optimized)"
  (|case body
    [meta ($variant idx is-last? value)]
    ($variant idx is-last? (de-meta value))
    
    [meta ($tuple elems)]
    ($tuple (&/|map de-meta elems))
    
    [meta ($apply func args)]
    ($apply (de-meta func)
            (&/|map de-meta args))
    
    [meta ($case value branches)]
    ($case (de-meta value)
           (&/|map (fn [branch]
                     (|let [[_pattern _body] branch]
                       (&/T [_pattern (de-meta _body)])))
                   branches))
    
    [meta ($function arity scope captured body*)]
    ($function arity
               scope
               (&/|map (fn [capture]
                         (|let [[_name _captured] capture]
                           (&/T [_name (de-meta _captured)]))
                         )
                       captured)
               (de-meta body*))
    
    [meta ($ann value-expr type-expr type-type)]
    ($ann (de-meta value-expr) nil nil)
    
    [meta ($var var-kind)]
    ($var var-kind)
    
    [meta ($captured scope idx source)]
    ($captured scope idx (de-meta source))

    [meta ($proc proc-ident args special-args)]
    (&/T ($proc proc-ident (&/|map de-meta args) special-args))

    [meta not-interesting]
    not-interesting
    ))

(defn ^:private shift-function-body [own-body? body]
  "(-> Optimized Optimized)"
  (|case body
    [meta ($variant idx is-last? value)]
    (&/T [meta ($variant idx is-last? (shift-function-body own-body? value))])
    
    [meta ($tuple elems)]
    (&/T [meta ($tuple (&/|map (partial shift-function-body own-body?) elems))])
    
    [meta ($apply func args)]
    (&/T [meta ($apply (shift-function-body own-body? func)
                       (&/|map (partial shift-function-body own-body?) args))])
    
    [meta ($case value branches)]
    (&/T [meta ($case (shift-function-body own-body? value)
                      (&/|map (fn [branch]
                                (|let [[_pattern _body] branch]
                                  (&/T [(if own-body?
                                          (shift-pattern _pattern)
                                          _pattern)
                                        (shift-function-body own-body? _body)])))
                              branches))])
    
    [meta ($function arity scope captured body*)]
    (&/T [meta ($function arity
                          (de-scope scope)
                          (&/|map (fn [capture]
                                    (|let [[_name [_meta ($captured _scope _idx _source)]] capture]
                                      (&/T [_name (&/T [_meta ($captured (de-scope _scope) _idx (shift-function-body own-body? _source))])])))
                                  captured)
                          (shift-function-body false body*))])
    
    [meta ($ann value-expr type-expr type-type)]
    (&/T [meta ($ann (shift-function-body own-body? value-expr)
                     type-expr
                     type-type)])
    
    [meta ($var var-kind)]
    (if own-body?
      (|case var-kind
        (&/$Local 0)
        (&/T [meta ($apply body
                           (&/|list [meta ($var (&/$Local 1))]))])
        
        (&/$Local idx)
        (&/T [meta ($var (&/$Local (inc idx)))])
        
        (&/$Global ?module ?name)
        body)
      body)
    
    [meta ($captured scope idx source)]
    (if own-body?
      source
      (|case scope
        (&/$Cons _ (&/$Cons _ (&/$Nil)))
        source

        _
        (&/T [meta ($captured (de-scope scope) idx (shift-function-body own-body? source))]))
      )
    
    [meta ($proc proc-ident args special-args)]
    (&/T [meta ($proc proc-ident (&/|map (partial shift-function-body own-body?) args) special-args)])

    not-interesting
    not-interesting
    ))

(defn ^:private optimize-closure [optimize closure]
  (&/|map (fn [capture]
            (|let [[_name _analysis] capture]
              (&/T [_name (optimize _analysis)])))
          closure))

;; [Exports]
(defn optimize [analysis]
  "(-> Analysis Optimized)"
  (|case analysis
    [meta (&-base/$bool value)]
    (&/T [meta ($bool value)])
    
    [meta (&-base/$int value)]
    (&/T [meta ($int value)])
    
    [meta (&-base/$real value)]
    (&/T [meta ($real value)])
    
    [meta (&-base/$char value)]
    (&/T [meta ($char value)])
    
    [meta (&-base/$text value)]
    (&/T [meta ($text value)])
    
    [meta (&-base/$variant idx is-last? value)]
    (&/T [meta ($variant idx is-last? (optimize value))])
    
    [meta (&-base/$tuple elems)]
    (&/T [meta ($tuple (&/|map optimize elems))])
    
    [meta (&-base/$apply func args)]
    (&/T [meta ($apply (optimize func) (&/|map optimize args))])
    
    [meta (&-base/$case value branches)]
    (&/T [meta ($case (optimize value)
                      (&/|map (fn [branch]
                                (|let [[_pattern _body] branch]
                                  (&/T [_pattern (optimize _body)])))
                              branches))])
    
    [meta (&-base/$lambda scope captured body)]
    (|case (optimize body)
      [_ ($function _arity _scope _captured _body)]
      (&/T [meta ($function (inc _arity) scope (optimize-closure optimize captured) (shift-function-body true _body))])

      =body
      (&/T [meta ($function 1 scope (optimize-closure optimize captured) =body)]))
    
    [meta (&-base/$ann value-expr type-expr type-type)]
    (&/T [meta ($ann (optimize value-expr) type-expr type-type)])
    
    [meta (&-base/$var var-kind)]
    (&/T [meta ($var var-kind)])
    
    [meta (&-base/$captured scope idx source)]
    (&/T [meta ($captured scope idx (optimize source))])

    [meta (&-base/$proc proc-ident args special-args)]
    (&/T [meta ($proc proc-ident (&/|map optimize args) special-args)])
    
    _
    (assert false (prn-str 'optimize (&/adt->text analysis)))
    ))
