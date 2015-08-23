;;   Copyright (c) Eduardo Julian. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns lux.analyser.record
  (:require clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftags |let |do return fail |case]])
            (lux.analyser [base :as &&]
                          [module :as &&module])))

;; [Exports]
(defn order-record [pairs]
  "(-> (List (, Syntax Syntax)) (Lux (List Syntax)))"
  (|do [tag-group (|case pairs
                    (&/$Nil)
                    (return (&/|list))
                    
                    (&/$Cons [[_ (&/$TagS tag1)] _] _)
                    (|do [[module name] (&&/resolved-ident tag1)]
                      (&&module/tag-group module name))

                    _
                    (fail "[Analyser Error] Wrong syntax for records. Odd elements must be tags."))
        =pairs (&/map% (fn [kv]
                         (|case kv
                           [[_ (&/$TagS k)] v]
                           (|do [=k (&&/resolved-ident k)]
                             (return (&/P (&/ident->text =k) v)))

                           _
                           (fail "[Analyser Error] Wrong syntax for records. Odd elements must be tags.")))
                       pairs)]
    (&/map% (fn [tag]
              (if-let [member (&/|get tag =pairs)]
                (return member)
                (fail (str "[Analyser Error] Unknown tag: " tag))))
            (&/|map &/ident->text tag-group))))
