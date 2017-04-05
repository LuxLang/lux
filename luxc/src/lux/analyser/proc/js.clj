(ns lux.analyser.proc.js
  (:require (clojure [template :refer [do-template]]
                     [string :as string])
            clojure.core.match
            clojure.core.match.array
            (lux [base :as & :refer [|let |do return* return |case assert!]]
                 [type :as &type])
            (lux.analyser [base :as &&])))

(do-template [<name> <proc>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons ?function ?args) ?values]
          =function (&&/analyse-1 analyse (&/$HostT "function" &/$Nil) ?function)
          =args (&/map% (partial &&/analyse-1+ analyse) ?args)
          _ (&type/check exo-type (&/$HostT "object" &/$Nil))
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["js" <proc>]) (&/$Cons =function =args) (&/|list)))))))

  ^:private analyse-js-new  "new"
  ^:private analyse-js-call "call"
  )

(defn ^:private analyse-js-object-call [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?object (&/$Cons ?field ?args)) ?values]
        =object (&&/analyse-1 analyse (&/$HostT "object" &/$Nil) ?object)
        =field (&&/analyse-1 analyse &type/Text ?field)
        =args (&/map% (partial &&/analyse-1+ analyse) ?args)
        _ (&type/check exo-type (&/$HostT "object" &/$Nil))
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["js" "object-call"]) (&/$Cons =object (&/$Cons =field =args)) (&/|list)))))))

(defn ^:private analyse-js-ref [analyse exo-type ?values]
  (|do [:let [(&/$Cons [_ (&/$TextS ?ref-name)] (&/$Nil)) ?values]
        _ (&type/check exo-type (&/$HostT "object" &/$Nil))
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["js" "ref"]) (&/|list) (&/|list ?ref-name)))))))

(do-template [<name> <proc>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Cons ?object (&/$Cons ?field (&/$Nil))) ?values]
          =object (&&/analyse-1 analyse (&/$HostT "object" &/$Nil) ?object)
          =field (&&/analyse-1 analyse &type/Text ?field)
          _ (&type/check exo-type (&/$HostT "object" &/$Nil))
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["js" <proc>]) (&/|list =object =field) (&/|list)))))))

  ^:private analyse-js-get-field    "get-field"
  ^:private analyse-js-delete-field "delete-field"
  )

(defn ^:private analyse-js-set-field [analyse exo-type ?values]
  (|do [:let [(&/$Cons ?object (&/$Cons ?field (&/$Cons ?value (&/$Nil)))) ?values]
        =object (&&/analyse-1 analyse (&/$HostT "object" &/$Nil) ?object)
        =field (&&/analyse-1 analyse &type/Text ?field)
        =value (&&/analyse-1+ analyse ?value)
        _ (&type/check exo-type (&/$HostT "object" &/$Nil))
        _cursor &/cursor]
    (return (&/|list (&&/|meta exo-type _cursor
                               (&&/$proc (&/T ["js" "set-field"]) (&/|list =object =field =value) (&/|list)))))))

(do-template [<name> <proc> <type>]
  (defn <name> [analyse exo-type ?values]
    (|do [:let [(&/$Nil) ?values]
          :let [output-type (&/$HostT <type> &/$Nil)]
          _ (&type/check exo-type output-type)
          _cursor &/cursor]
      (return (&/|list (&&/|meta exo-type _cursor
                                 (&&/$proc (&/T ["js" <proc>]) (&/|list) (&/|list)))))))

  ^:private analyse-js-object    "object"    "object"
  ^:private analyse-js-null      "null"      "object"
  ^:private analyse-js-undefined "undefined" "undefined"
  )

(defn analyse-host [analyse exo-type proc ?values]
  (case proc
    "new"          (analyse-js-new analyse exo-type ?values)
    "call"         (analyse-js-call analyse exo-type ?values)
    "object-call"  (analyse-js-object-call analyse exo-type ?values)
    "ref"          (analyse-js-ref analyse exo-type ?values)
    "object"       (analyse-js-object analyse exo-type ?values)
    "get-field"    (analyse-js-get-field analyse exo-type ?values)
    "set-field"    (analyse-js-set-field analyse exo-type ?values)
    "delete-field" (analyse-js-delete-field analyse exo-type ?values)
    "null"         (analyse-js-null analyse exo-type ?values)
    "undefined"    (analyse-js-undefined analyse exo-type ?values)
    ;; else
    (&/fail-with-loc (str "[Analyser Error] Unknown host procedure: " ["js" proc])))
  )
