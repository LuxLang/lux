;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.optimizer
  (:require (lux [base :as & :refer [|let |do return fail return* fail* |case defvariant]]
                 [analyser :as &analyser])
            [lux.analyser.base :as &-base]))

;; [Tags]
(defvariant
  ("bool" 1)
  ("int" 1)
  ("real" 1)
  ("char" 1)
  ("text" 1)
  ("variant" 1)
  ("tuple" 1)
  ("apply" 1)
  ("case" 1)
  ("lambda" 1)
  ("ann" 1)
  ("var" 1)
  ("captured" 1)
  ("proc" 2)
  )

;; [Exports]
(defn optimize-token [analysis]
  "(-> Analysis Optimized)"
  (|case analysis
    [meta (&-base/$bool value)]
    (return (&/T [meta ($bool value)]))
    
    [meta (&-base/$int value)]
    (return (&/T [meta ($int value)]))
    
    [meta (&-base/$real value)]
    (return (&/T [meta ($real value)]))
    
    [meta (&-base/$char value)]
    (return (&/T [meta ($char value)]))
    
    [meta (&-base/$text value)]
    (return (&/T [meta ($text value)]))
    
    [meta (&-base/$variant value)]
    (return (&/T [meta ($variant value)]))
    
    [meta (&-base/$tuple value)]
    (return (&/T [meta ($tuple value)]))
    
    [meta (&-base/$apply value)]
    (return (&/T [meta ($apply value)]))
    
    [meta (&-base/$case value)]
    (return (&/T [meta ($case value)]))
    
    [meta (&-base/$lambda value)]
    (return (&/T [meta ($lambda value)]))
    
    [meta (&-base/$ann value)]
    (return (&/T [meta ($ann value)]))
    
    [meta (&-base/$var value)]
    (return (&/T [meta ($var value)]))
    
    [meta (&-base/$captured value)]
    (return (&/T [meta ($captured value)]))

    [meta (&-base/$proc ?proc-ident ?args)]
    (return (&/T [meta ($proc ?proc-ident ?args)]))
    
    _
    (assert false (prn-str 'optimize-token (&/adt->text analysis)))
    ))

(defn optimize [eval! compile-module compilers]
  (|do [analyses (&analyser/analyse eval! compile-module compilers)]
    (&/map% optimize-token analyses)))
