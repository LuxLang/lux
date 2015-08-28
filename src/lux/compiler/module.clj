;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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
