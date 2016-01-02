;;  Copyright (c) Eduardo Julian. All rights reserved.
;;  This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
;;  If a copy of the MPL was not distributed with this file,
;;  You can obtain one at http://mozilla.org/MPL/2.0/.

(ns lux.analyser.meta
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [deftags |let |do return return* fail fail* |case]])))

;; [Utils]
(defn ^:private ident= [x y]
  (|let [[px nx] x
         [py ny] y]
    (and (= px py)
         (= nx ny))))

(def ^:private tag-prefix "lux")

;; [Values]
(defn meta-get [ident dict]
  "(-> Ident DefMeta (Maybe DefMetaValue))"
  (|case dict
    (&/$Cons [k v] dict*)
    (if (ident= k ident)
      (&/Some$ v)
      (meta-get ident dict*))

    (&/$Nil)
    &/None$))

(do-template [<name> <tag-name>]
  (def <name> (&/V tag-prefix <tag-name>))

  type?-tag   "type?"
  alias-tag   "alias"
  macro?-tag  "macro?"
  export?-tag "export?"
  tags-tag    "tags"
  )
