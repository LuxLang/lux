;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.compiler.module
  (:require (clojure [string :as string]
                     [set :as set]
                     [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return fail fail* |case]]
                 [type :as &type])
            [lux.analyser.module :as &module]))

;; [Exports]
(def tag-groups
  "(Lux (List (, Text (List Text))))"
  (|do [module &/get-current-module]
    (return (&/|map (fn [pair]
                      (|case pair
                        [name [tags _]]
                        (&/T name (&/|map (fn [^objects tag] (aget tag 1)) tags))))
                    (&/get$ &module/$types module)))
    ))
