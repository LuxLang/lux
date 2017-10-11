(ns lux.analyser.meta
  (:require (clojure [template :refer [do-template]])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return return* |case]])))

;; [Utils]
(defn ^:private ident= [x y]
  (|let [[px nx] x
         [py ny] y]
    (and (= px py)
         (= nx ny))))

(def ^:private tag-prefix "lux")

;; [Values]
(defn meta-get [ident annotations]
  "(-> Ident Code (Maybe Code))"
  (|case annotations
    [_ (&/$Record dict)]
    (loop [dict dict]
      (|case dict
        (&/$Cons [_k v] dict*)
        (|case _k
          [_ (&/$Tag k)]
          (if (ident= k ident)
            (&/$Some v)
            (recur dict*))

          _
          (recur dict*))

        (&/$Nil)
        &/$None))

    _
    &/$None))

(do-template [<name> <tag-name>]
  (def <name> (&/T [tag-prefix <tag-name>]))

  type?-tag   "type?"
  alias-tag   "alias"
  macro?-tag  "macro?"
  export?-tag "export?"
  tags-tag    "tags"
  imports-tag "imports"
  )
