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

  ("jvm-class" 1)
  ("jvm-interface" 1)
  )

;; [Exports]
(defn optimize-token [analysis]
  "(-> Analysis Optimized)"
  (|case analysis
    (&-base/$bool value)
    (return ($bool value))
    
    (&-base/$int value)
    (return ($int value))
    
    (&-base/$real value)
    (return ($real value))
    
    (&-base/$char value)
    (return ($char value))
    
    (&-base/$text value)
    (return ($text value))
    
    (&-base/$variant value)
    (return ($variant value))
    
    (&-base/$tuple value)
    (return ($tuple value))
    
    (&-base/$apply value)
    (return ($apply value))
    
    (&-base/$case value)
    (return ($case value))
    
    (&-base/$lambda value)
    (return ($lambda value))
    
    (&-base/$ann value)
    (return ($ann value))
    
    (&-base/$var value)
    (return ($var value))
    
    (&-base/$captured value)
    (return ($captured value))

    (&-base/$proc ?proc-ident ?args)
    (return ($proc ?proc-ident ?args))
    
    (&-base/$jvm-class value)
    (return ($jvm-class value))
    
    (&-base/$jvm-interface value)
    (return ($jvm-interface value))
    
    _
    (assert false (prn-str 'optimize-token (&/adt->text analysis)))
    ))

(defn optimize [eval! compile-module compilers]
  (|do [analyses (&analyser/analyse eval! compile-module compilers)]
    (&/map% optimize-token analyses)))
