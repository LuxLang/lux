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
(defn meta-get [ident dict]
  "(-> Ident Anns (Maybe Ann-Value))"
  (|case dict
    (&/$Cons [k v] dict*)
    (if (ident= k ident)
      (&/$Some v)
      (meta-get ident dict*))

    (&/$Nil)
    &/$None

    _
    (assert false (println-str (&/adt->text ident)
                               (&/adt->text dict)))))

(do-template [<name> <tag-name>]
  (def <name> (&/T [tag-prefix <tag-name>]))

  type?-tag   "type?"
  alias-tag   "alias"
  macro?-tag  "macro?"
  export?-tag "export?"
  tags-tag    "tags"
  imports-tag "imports"
  )
